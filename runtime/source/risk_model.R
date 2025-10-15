# ─────────────────────────────────────────────────────────────────────────────
# Drop Risk Model
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(zoo)
library(mgcv)
library(parallel)
library(tidyr)
library(tibble)
library(mvtnorm)
library(ggplot2)

# Extract just the e and v features from sim_data (without tau processing)
extract_risk_features <- function(sim_data) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(data.frame(e = numeric(0), v = numeric(0)))
    }

    # Compute features (same logic as build_hazard_samples)
    q_mag <- abs(sim_data$q)
    q_dir <- ifelse(q_mag > 0, sim_data$q / q_mag, 0)
    v_raw <- sim_data$qd * q_dir
    qlo <- stats::quantile(v_raw, 0.01, na.rm = TRUE)
    qhi <- stats::quantile(v_raw, 0.99, na.rm = TRUE)

    # Return just the features
    data.frame(
        e = pmax(0, pmin(1, sim_data$dist_to_escape_ratio)),
        v = pmin(pmax(v_raw, qlo), qhi)
    )
}

build_hazard_samples <- function(sim_data, tau = 0.2) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(data.frame())
    }

    # Sort (prefer segment+time if available to keep groups contiguous)
    if ("respawn_segment" %in% names(sim_data)) {
        sim_data <- sim_data[order(sim_data$respawn_segment, sim_data$time), ]
    } else {
        sim_data <- sim_data[order(sim_data$time), ]
        sim_data$respawn_segment <- 1L # fallback: single segment
    }

    # Extract risk features using helper function
    features <- extract_risk_features(sim_data)

    # Columns to keep
    base_cols <- c(
        "time", "e", "v", "q", "qd", "q_max", "dist_to_escape_ratio",
        "respawn_segment", "condition", "taskNum", "phase"
    )

    # Build feature frame
    haz <- sim_data
    haz$e <- features$e
    haz$v <- features$v

    # --- positives: last sample of each respawn_segment (+/- tau window if tau > 0) ---
    # Ensure flag exists; if not, infer last by time
    if (!("is_last_sample_of_segment" %in% names(sim_data))) {
        sim_data$is_last_sample_of_segment <- sim_data$time == ave(sim_data$time, sim_data$respawn_segment, FUN = max)
    }

    # Compute last time per segment (prefer flagged; fall back to max time)
    # Vectorized with dplyr .by
    df_pos <- dplyr::as_tibble(sim_data) |>
        dplyr::mutate(
            last_time_flagged = dplyr::if_else(
                any(isTRUE(is_last_sample_of_segment)),
                {
                    flagged_times <- time[is_last_sample_of_segment]
                    if (length(flagged_times) > 0) {
                        max(flagged_times, na.rm = TRUE)
                    } else {
                        as.numeric(NA)
                    }
                },
                as.numeric(NA)
            ),
            .by = respawn_segment
        ) |>
        dplyr::mutate(
            last_time = dplyr::if_else(is.na(last_time_flagged),
                max(time, na.rm = TRUE),
                last_time_flagged
            ),
            .by = respawn_segment
        )

    if (tau > 0) {
        pos <- as.integer(df_pos$time >= (df_pos$last_time - tau) & df_pos$time <= df_pos$last_time)
    } else {
        pos <- as.integer(sim_data$is_last_sample_of_segment %in% TRUE)
    }

    # Attach outcome and trim columns
    haz <- cbind(haz, drop_within_tau = pos)
    haz <- haz[, c(base_cols, "drop_within_tau")]
    haz <- haz[complete.cases(haz[, c("e", "v", "drop_within_tau")]), , drop = FALSE]

    # --- debug output ---
    n_on <- nrow(sim_data)
    n_haz <- nrow(haz)
    positives <- sum(haz$drop_within_tau, na.rm = TRUE)
    dt_med <- if (n_on > 1) stats::median(diff(sim_data$time), na.rm = TRUE) else NA_real_
    hazard_logger <- create_module_logger("HAZARD")
    hazard_logger("DEBUG", sprintf(
        "tau=%.3fs | rows_in=%d | rows_out=%d | dt~%.3fs | positives=%d (%.4f%%)",
        tau, n_on, n_haz, dt_med, positives, ifelse(n_haz > 0, 100 * positives / n_haz, 0)
    ))

    return(haz)
}


resample_simulation_per_episode <- function(sim_data, target_freq = 20, debug = FALSE) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(sim_data)
    }

    req <- c("time", "respawn_segment", "is_last_sample_of_segment")
    miss <- setdiff(req, names(sim_data))
    if (length(miss) > 0) {
        warning("Missing columns for resampling: ", paste(miss, collapse = ", "))
        return(sim_data)
    }

    # --- observed frequency (overall + per segment) ---
    sim_data <- dplyr::arrange(sim_data, respawn_segment, time)
    overall_dt <- suppressWarnings(stats::median(diff(sim_data$time), na.rm = TRUE))
    overall_freq <- if (is.finite(overall_dt)) 1 / overall_dt else NA_real_
    if (debug) {
        message(sprintf("[DEBUG] target_freq = %.3f Hz", target_freq))
        message(sprintf("[DEBUG] overall observed ~ %.3f Hz (median dt = %.6f s)", overall_freq, overall_dt))
    }

    if (debug) {
        per_seg <- sim_data |>
            dplyr::summarise(
                n_raw = dplyr::n(),
                dt_med = stats::median(diff(time), na.rm = TRUE),
                freq_med = 1 / dt_med,
                time_span = max(time) - min(time),
                .by = respawn_segment
            )
        message("[DEBUG] Per-segment sampling summary (first 10 rows):")
        print(utils::head(per_seg, 10))
    }

    # Skip if not actually downsampling
    orig_true <- sum(sim_data$is_last_sample_of_segment, na.rm = TRUE)
    if (is.na(target_freq) || (is.finite(overall_freq) && target_freq >= overall_freq * 0.98)) {
        message(sprintf("[RESAMPLE] Skipped (target >= observed). TRUE flags unchanged: %d", orig_true))
        return(sim_data)
    }

    dt <- 1 / target_freq
    eps <- 1e-9

    # Precompute which segments ended with TRUE originally
    segments_with_true_end <- sim_data |>
        dplyr::summarise(
            has_true_end = dplyr::last(is_last_sample_of_segment, order_by = time),
            .by = respawn_segment
        )

    # --- DIAGNOSTIC: how many unique bins will we get? ---
    bins_diag <- sim_data |>
        dplyr::mutate(t0 = min(time), .by = respawn_segment) |>
        dplyr::mutate(bin = pmax(0L, as.integer(floor((time - t0) * target_freq + eps)))) |>
        dplyr::summarise(
            n_raw = dplyr::n(),
            n_bins = dplyr::n_distinct(bin),
            compress_ratio = n_bins / n_raw,
            min_bin = min(bin), max_bin = max(bin),
            .by = respawn_segment
        )

    if (debug) {
        message("[DEBUG] Bin diagnostics (first 10 rows):")
        print(utils::head(bins_diag, 10))
        message(sprintf(
            "[DEBUG] Overall compression: raw=%d, bins=%d (%.1f%% kept)",
            nrow(sim_data), sum(bins_diag$n_bins),
            100 * sum(bins_diag$n_bins) / nrow(sim_data)
        ))
        if (all(bins_diag$compress_ratio >= 0.99)) {
            message(
                "[DEBUG] WARNING: ~1 bin per raw sample everywhere → effectively no downsampling.\n",
                "        Likely because target_freq ≈ observed or time jitter pushes samples into distinct bins."
            )
        }
        # Fractional part check: are many values near bin edges?
        frac <- ((sim_data$time - ave(sim_data$time, sim_data$respawn_segment, FUN = min)) * target_freq) %% 1
        message("[DEBUG] Fractional part of bin index quantiles (0, .25, .5, .75, 1):")
        print(stats::quantile(frac, c(0, .25, .5, .75, 1), na.rm = TRUE))
    }

    # --- Resample ---
    meta_cols <- intersect(
        c(
            "condition", "phase", "taskNum", "participant", "trialNum", "gender",
            "simulating", "visualizations", "perturbations"
        ),
        names(sim_data)
    )

    out <- sim_data |>
        dplyr::mutate(t0 = min(time), .by = respawn_segment) |>
        dplyr::mutate(bin = pmax(0L, as.integer(floor((time - t0) * target_freq + eps)))) |>
        dplyr::reframe(
            time = dplyr::first(t0) + dplyr::first(bin) / target_freq,
            bin = dplyr::first(bin),
            q = mean(q, na.rm = TRUE),
            qd = mean(qd, na.rm = TRUE),
            q_max = mean(q_max, na.rm = TRUE),
            dist_to_escape_ratio = mean(dist_to_escape_ratio, na.rm = TRUE),
            dplyr::across(dplyr::all_of(meta_cols), dplyr::first),
            .by = c(respawn_segment, bin)
        ) |>
        dplyr::arrange(respawn_segment, time) |>
        dplyr::distinct(respawn_segment, time, .keep_all = TRUE) |>
        dplyr::left_join(segments_with_true_end, by = "respawn_segment") |>
        dplyr::mutate(
            has_true_end = tidyr::replace_na(has_true_end, FALSE),
            row_num = dplyr::row_number(),
            is_last_sample_of_segment = has_true_end & (row_num == max(row_num)),
            .by = respawn_segment
        ) |>
        dplyr::select(-dplyr::any_of(c("bin", "row_num", "has_true_end", "t0")))


    # --- Post-checks ---
    new_true <- sum(out$is_last_sample_of_segment, na.rm = TRUE)
    if (!identical(orig_true, new_true)) {
        warning(sprintf("[RESAMPLE] TRUE-flag count changed: original=%d, resampled=%d", orig_true, new_true))
    }

    if (debug) {
        # Per-segment row counts before/after
        after_seg <- out |>
            dplyr::summarise(n_resampled = dplyr::n(), .by = respawn_segment)
        diag_join <- dplyr::left_join(
            bins_diag |> dplyr::select(respawn_segment, n_raw, n_bins, compress_ratio),
            after_seg,
            by = "respawn_segment"
        )
        message("[DEBUG] Per-segment rows: raw vs. resampled (first 10 rows):")
        print(utils::head(diag_join, 10))

        # Show segments with little/no compression
        stubborn <- diag_join |> dplyr::filter(compress_ratio > 0.95)
        if (nrow(stubborn) > 0) {
            message("[DEBUG] Segments with >95% of rows retained (no real downsampling):")
            print(utils::head(stubborn, 20))
        }
    }

    message(sprintf(
        "[RESAMPLE] Original rows: %d -> Resampled rows: %d (%.1f%%). TRUE flags: %d -> %d",
        nrow(sim_data), nrow(out), 100 * nrow(out) / nrow(sim_data), orig_true, new_true
    ))

    out
}



#' Train risk model from hazard samples
#'
#' @param hazard_samples Data frame from build_hazard_samples()
#' @param use_by_interaction Whether to use factor-by smooths for shape differences (default TRUE)
#' @return Fitted GLM/GLMER model object
#' @export
train_risk_model <- function(hazard_samples, use_by_interaction = FALSE) {
    if (is.null(hazard_samples) || nrow(hazard_samples) == 0) {
        return(NULL)
    }

    # Debug: Check data structure
    model_logger <- create_module_logger("RISK-MODEL")
    model_logger("DEBUG", "Data structure check:")
    model_logger("DEBUG", "  - Rows:", nrow(hazard_samples))
    model_logger("DEBUG", "  - Columns:", paste(names(hazard_samples), collapse = ", "))
    model_logger("DEBUG", "  - Participant column type:", class(hazard_samples$participant))
    model_logger("DEBUG", "  - Unique participants:", length(unique(hazard_samples$participant)))
    model_logger("DEBUG", "  - Participant values:", paste(head(unique(hazard_samples$participant)), collapse = ", "))
    model_logger("DEBUG", "  - Drop_within_tau range:", min(hazard_samples$drop_within_tau, na.rm = TRUE), "to", max(hazard_samples$drop_within_tau, na.rm = TRUE))
    model_logger("DEBUG", "  - E range:", min(hazard_samples$e, na.rm = TRUE), "to", max(hazard_samples$e, na.rm = TRUE))
    model_logger("DEBUG", "  - V range:", min(hazard_samples$v, na.rm = TRUE), "to", max(hazard_samples$v, na.rm = TRUE))

    # Ensure participant is a factor for random effects
    if (!is.factor(hazard_samples$participant)) {
        model_logger("DEBUG", "Converting participant to factor...")
        hazard_samples$participant <- as.factor(hazard_samples$participant)
    }

    # Create condition×phase interaction factor for factor-by smooths (if enabled)
    if (use_by_interaction && "condition" %in% names(hazard_samples) && "phase" %in% names(hazard_samples)) {
        model_logger("DEBUG", "Creating condition×phase interaction factor for shape differences...")
        hazard_samples$cond_phase <- interaction(hazard_samples$condition, hazard_samples$phase, drop = TRUE)
        model_logger("DEBUG", "  - Created", length(levels(hazard_samples$cond_phase)), "unique condition×phase combinations:", paste(levels(hazard_samples$cond_phase), collapse = ", "))
    } else if (!use_by_interaction) {
        model_logger("DEBUG", "Factor-by smooths disabled by use_by_interaction=FALSE. Using simple smooths.")
    } else {
        model_logger("WARN", "condition or phase columns not found. Using simple smooths only.")
    }

    # Remove any rows with missing values
    required_cols <- c("e", "v", "participant", "drop_within_tau")
    if ("cond_phase" %in% names(hazard_samples)) {
        required_cols <- c(required_cols, "cond_phase")
    }
    complete_rows <- complete.cases(hazard_samples[, required_cols])
    if (sum(!complete_rows) > 0) {
        model_logger("DEBUG", "Removing", sum(!complete_rows), "rows with missing values...")
        hazard_samples <- hazard_samples[complete_rows, ]
    }

    # Mixed-effects model with global smooths and parametric condition×phase effects
    if (use_by_interaction && "cond_phase" %in% names(hazard_samples)) {
        model_logger("DEBUG", "Fitting mixed-effects GAM model with global smooths and parametric effects...")
        model_logger("DEBUG", "Model formula: drop_within_tau ~ s(e) + s(v) + condition*phase + s(participant, bs='re')")
        model_logger("DEBUG", "  - Global shapes: s(e) + s(v) capture average state-risk mapping")
        model_logger("DEBUG", "  - Parametric effects: condition*phase captures level differences across groups")
    } else {
        model_logger("DEBUG", "Fitting mixed-effects GAM model with simple smooths...")
        model_logger("DEBUG", "Model formula: drop_within_tau ~ s(e) + s(v) + condition*phase + s(participant, bs='re')")
    }

    tryCatch(
        {
            if (use_by_interaction && "cond_phase" %in% names(hazard_samples)) {
                # Model with global smooths and parametric condition×phase effects (no factor-by smooths)
                model <- bam(
                    drop_within_tau ~
                        s(e, k = 5) + # global shape for e
                        s(v, k = 5) + # global shape for v
                        # removed: s(e, k = 5, by = cond_phase) + # e deviations by condition×phase
                        condition * phase + # parametric intercepts/contrasts
                        s(participant, bs = "re"), # participant random effects
                    family = binomial(link = "cloglog"),
                    data = hazard_samples,
                    method = "fREML",
                    discrete = TRUE, # big speed win
                    select = TRUE, # shrink unnecessary by-smooths toward 0
                    nthreads = parallel::detectCores() # use all cores
                )
            } else {
                # Simple smooths (either disabled or no condition×phase data)
                model <- bam(
                    drop_within_tau ~
                        s(e, k = 5) +
                        s(v, k = 5) +
                        condition * phase + # parametric intercepts/contrasts
                        s(participant, bs = "re"), # participant random effects
                    family = binomial(link = "cloglog"),
                    data = hazard_samples,
                    method = "fREML",
                    discrete = TRUE,
                    select = TRUE, # shrink unnecessary terms toward 0
                    nthreads = parallel::detectCores()
                )
            }
            model_logger("INFO", "Model fitting completed successfully!")
            model_logger("DEBUG", "Model class:", class(model)[1])

            # Log model structure for debugging
            if (use_by_interaction && "cond_phase" %in% names(hazard_samples)) {
                model_logger("DEBUG", "Model includes", length(levels(hazard_samples$cond_phase)), "condition×phase combinations:", paste(levels(hazard_samples$cond_phase), collapse = ", "))
                model_logger("DEBUG", "Model structure: Global shapes + condition×phase deviations")
            } else {
                model_logger("DEBUG", "Model structure: Simple smooths only")
            }

            return(model)
        },
        error = function(e) {
            model_logger("ERROR", "ERROR in model fitting:", e$message)
            return(NULL)
        }
    )
}

#' Score trial with risk model
#'
#' @param sim_data Simulation data from get_simulation_data()
#' @param model Fitted model from train_risk_model()
#' @param tau Time horizon for drop prediction (default 0.2 seconds)
#' @param standardized Whether to standardize predictions to reference condition/phase
#' @param use_factors Whether to include condition/phase factors in prediction (default FALSE)
#' @param preds_path Path to saved predictions file (default uses global path)
#' @param prefer Preferred prediction type: "p_hat_fe" (fixed-effects) or "p_hat_re" (subject-specific)
#' @return Data frame with one row containing risk metrics
#' @export
# ARCHIVED: Score trial with saved predictions (kept for reference)
score_trial_with_saved_predictions <- function(sim_data, model, tau = 0.2, standardized = FALSE, use_factors = FALSE,
                                               preds_path = NULL,
                                               prefer = c("p_hat_fe", "p_hat_re")) {
    if (is.null(model)) {
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    # Use global path if not specified
    if (is.null(preds_path)) {
        ensure_global_data_initialized()
        preds_path <- risk_hazard_samples_preds_path
    }

    prefer <- intersect(prefer, c("p_hat_fe", "p_hat_re"))

    # Try to load saved predictions
    haz_all <- NULL
    if (file.exists(preds_path)) {
        tryCatch(
            {
                haz_all <- readRDS(preds_path)
                score_logger <- create_module_logger("SCORE")
                score_logger("DEBUG", "Loaded saved predictions from:", preds_path)
            },
            error = function(e) {
                score_logger("ERROR", "Failed to load saved predictions:", e$message)
                haz_all <- NULL
            }
        )
    }

    # Identify this participant/trial
    pid <- unique(sim_data$participant)
    if (length(pid) != 1) pid <- pid[1]

    trial <- unique(sim_data$trial)
    if (length(trial) != 1) trial <- trial[1]

    # If we have saved predictions, filter and return
    if (!is.null(haz_all)) {
        sel <- rep(TRUE, nrow(haz_all))
        if ("participant" %in% names(haz_all)) sel <- sel & (as.character(haz_all$participant) == as.character(pid))
        if ("trial" %in% names(haz_all)) sel <- sel & (as.character(haz_all$trial) == as.character(trial))

        haz_trial <- haz_all[sel, , drop = FALSE]
        score_logger("DEBUG", "Filtered to", nrow(haz_trial), "rows for participant", pid, "trial", trial)
        if (nrow(haz_trial) > 0) {
            # Pick preferred prediction column
            pred_col <- prefer[prefer %in% names(haz_trial)][1]
            if (is.na(pred_col)) {
                warning("No predicted columns found in saved file; computing on the fly.")
            } else {
                score_logger("DEBUG", "Using saved predictions (", pred_col, ") for participant", pid, "trial", trial)
                return(list(
                    individual_predictions = haz_trial[[pred_col]],
                    hazard_samples = haz_trial
                ))
            }
        }
    }

    # Fallback: compute predictions on the fly for this trial only
    score_logger("DEBUG", "Computing predictions on-the-fly for participant", pid, "trial", trial)
    return(score_trial_with_model(sim_data, model, tau, standardized = standardized, use_factors = use_factors))
}

#' Score trial with risk model (on-the-fly computation only)
#'
#' @param sim_data Simulation data from get_simulation_data()
#' @param model Fitted model from train_risk_model()
#' @param tau Time horizon for drop prediction (default 0.2 seconds)
#' @param standardized Whether to standardize predictions to reference condition/phase
#' @param use_factors Whether to include condition/phase factors in prediction (default FALSE)
#' @return List with individual_predictions and hazard_samples
#' @export
score_trial_with_model <- function(sim_data, model, tau = 0.2, standardized = FALSE, use_factors = TRUE) {
    if (is.null(model)) {
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    # Identify this participant/trial
    pid <- unique(sim_data$participant)
    if (length(pid) != 1) pid <- pid[1]

    trial <- unique(sim_data$trial)
    if (length(trial) != 1) trial <- trial[1]

    score_logger <- create_module_logger("SCORE")
    score_logger("DEBUG", "Computing predictions on-the-fly for participant", pid, "trial", trial)

    hazard_samples <- build_hazard_samples(sim_data, tau)
    if (nrow(hazard_samples) == 0) {
        score_logger("DEBUG", "score_trial_with_model: returning empty list")
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    # Set participant factor
    hazard_samples$participant <- factor(pid)

    # Handle condition/phase factors only if requested
    if (use_factors) {
        if (standardized) {
            # Use reference levels for standardized predictions
            ref_condition <- "control"
            ref_phase <- "baseline_task"

            if ("condition" %in% names(model$model)) {
                model_cond_levels <- levels(model$model$condition)
                if (ref_condition %in% model_cond_levels) {
                    hazard_samples$condition <- factor(ref_condition, levels = model_cond_levels)
                } else {
                    hazard_samples$condition <- factor(model_cond_levels[1], levels = model_cond_levels)
                }
            }

            if ("phase" %in% names(model$model)) {
                model_phase_levels <- levels(model$model$phase)
                if (ref_phase %in% model_phase_levels) {
                    hazard_samples$phase <- factor(ref_phase, levels = model_phase_levels)
                } else {
                    hazard_samples$phase <- factor(model_phase_levels[1], levels = model_phase_levels)
                }
            }

            # Handle cond_phase interaction
            if ("cond_phase" %in% names(model$model)) {
                expected_cond_phase <- paste(ref_condition, ref_phase, sep = ".")
                model_cond_phase_levels <- levels(model$model$cond_phase)
                if (expected_cond_phase %in% model_cond_phase_levels) {
                    hazard_samples$cond_phase <- factor(expected_cond_phase, levels = model_cond_phase_levels)
                } else {
                    hazard_samples$cond_phase <- factor(model_cond_phase_levels[1], levels = model_cond_phase_levels)
                }
            }
        } else {
            # Use actual condition/phase from data
            if ("condition" %in% names(model$model) && "condition" %in% names(hazard_samples)) {
                hazard_samples$condition <- factor(hazard_samples$condition, levels = levels(model$model$condition))
            }
            if ("phase" %in% names(model$model) && "phase" %in% names(hazard_samples)) {
                hazard_samples$phase <- factor(hazard_samples$phase, levels = levels(model$model$phase))
            }
            if ("cond_phase" %in% names(model$model)) {
                expected_cond_phase <- paste(hazard_samples$condition, hazard_samples$phase, sep = ".")
                model_cond_phase_levels <- levels(model$model$cond_phase)
                hazard_samples$cond_phase <- factor(expected_cond_phase, levels = model_cond_phase_levels)
            }
        }
    }

    # Predict probabilities for each sample
    # Only expect bam models
    if (inherits(model, "bam")) {
        # For mgcv::bam, use exclude to predict without random effects
        p_t <- predict(model, newdata = hazard_samples, type = "response", exclude = "s(participant)")
    } else {
        # Warn for unexpected model types
        warning(sprintf("Unexpected model type: %s. Only mgcv::bam models are supported.", class(model)[1]))
        p_t <- predict(model, newdata = hazard_samples, type = "response")
    }

    # Create a list instead of data.frame to properly store the lists
    result <- list(
        individual_predictions = p_t, # Store individual predictions
        hazard_samples = hazard_samples # Store the hazard samples for mapping
    )

    return(result)
}

#' Save risk model to RDS file
#'
#' @param model Fitted model from train_risk_model()
#' @param file_path Path to save the RDS file
#' @param tau Time horizon used for training (optional, for metadata)
#' @export
save_risk_model <- function(model, file_path, tau = NULL) {
    # Add tau as an attribute if provided
    save_logger <- create_module_logger("SAVE")
    if (!is.null(tau)) {
        attr(model, "tau") <- tau
        save_logger("DEBUG", "Adding tau attribute to model:", tau, "seconds")
    } else {
        save_logger("WARN", "No tau provided, model will be saved without tau attribute")
    }

    # Verify the attribute was added
    if (!is.null(tau)) {
        saved_tau <- attr(model, "tau")
        save_logger("DEBUG", "Verified tau attribute:", saved_tau, "seconds")
    }

    saveRDS(model, file_path)
    save_logger("INFO", "Model saved to:", file_path)
}

#' Load risk model from RDS file
#'
#' @param file_path Path to the RDS file
#' @return Fitted model object
#' @export
load_risk_model <- function(file_path) {
    if (file.exists(file_path)) {
        model <- readRDS(file_path)

        # Check if tau attribute exists
        load_logger <- create_module_logger("LOAD")
        tau <- attr(model, "tau")
        if (!is.null(tau)) {
            load_logger("DEBUG", "Model loaded with tau attribute:", tau, "seconds")
        } else {
            load_logger("WARN", "Model loaded without tau attribute")
        }

        return(model)
    } else {
        load_logger("ERROR", "Model file not found:", file_path)
        return(NULL)
    }
}

#' Set up global risk model variables
#'
#' This function loads the risk model, hazard samples with predictions, and analysis results into global variables.
#' It should be called whenever a new model is trained or when initializing the system.
#'
#' @param model_path Path to the risk model RDS file (optional, uses global path if not provided).
#'   Pass FALSE to skip loading the risk model.
#' @param hazard_samples_path Path to the hazard samples with predictions RDS file (optional, uses global path if not provided).
#'   Pass FALSE to skip loading the hazard samples.
#' @param analysis_results_path Path to the analysis results RDS file (optional, uses global path if not provided).
#'   Pass FALSE to skip loading the analysis results.
#' @export
setup_global_risk_variables <- function(model_path = NULL, hazard_samples_path = NULL, analysis_results_path = NULL) {
    # Ensure global data is initialized to access paths
    ensure_global_data_initialized()

    # Load risk model and extract tau for parallel processing (only if not explicitly skipped)
    if (!identical(model_path, FALSE)) {
        # Use global path if not provided or if NULL
        if (is.null(model_path)) {
            model_path <- risk_model_path
        }

        global_logger <- create_module_logger("GLOBAL")
        if (file.exists(model_path)) {
            GLOBAL_RISK_MODEL <<- load_risk_model(model_path)
            GLOBAL_RISK_TAU <<- attr(GLOBAL_RISK_MODEL, "tau")
            global_logger("INFO", "Risk model loaded from:", model_path)
            if (!is.null(GLOBAL_RISK_TAU)) {
                global_logger("DEBUG", "Risk model tau:", GLOBAL_RISK_TAU, "seconds")
            } else {
                global_logger("WARN", "No tau found in risk model")
            }
        } else {
            global_logger("WARN", "No risk model found at:", model_path, "- use dashboard to load one")
        }
    } else {
        global_logger("DEBUG", "Skipping risk model loading (model_path = FALSE)")
    }

    # Load hazard samples with predictions into global workspace (only if not explicitly skipped)
    if (!identical(hazard_samples_path, FALSE)) {
        # Use global path if not provided or if NULL
        if (is.null(hazard_samples_path)) {
            hazard_samples_path <- risk_hazard_samples_preds_path
        }

        if (file.exists(hazard_samples_path)) {
            GLOBAL_HAZARD_SAMPLES_PREDS <<- readRDS(hazard_samples_path)
            global_logger("INFO", "Hazard samples with predictions loaded from:", hazard_samples_path)
            global_logger("DEBUG", "Loaded", nrow(GLOBAL_HAZARD_SAMPLES_PREDS), "hazard prediction rows")
        } else {
            global_logger("WARN", "No hazard samples with predictions found at:", hazard_samples_path)
        }
    } else {
        global_logger("DEBUG", "Skipping hazard samples loading (hazard_samples_path = FALSE)")
    }

    # Load analysis results into global workspace (only if not explicitly skipped)
    if (!identical(analysis_results_path, FALSE)) {
        # Use global path if not provided or if NULL
        if (is.null(analysis_results_path)) {
            analysis_results_path <- risk_analysis_results_path
        }

        if (file.exists(analysis_results_path)) {
            GLOBAL_HAZARD_ANALYSIS_RESULTS <<- readRDS(analysis_results_path)
            global_logger("INFO", "Analysis results loaded from:", analysis_results_path)
            if (!is.null(GLOBAL_HAZARD_ANALYSIS_RESULTS)) {
                global_logger(
                    "DEBUG", "Loaded analysis results with",
                    ifelse(!is.null(GLOBAL_HAZARD_ANALYSIS_RESULTS$standardized_risks),
                        nrow(GLOBAL_HAZARD_ANALYSIS_RESULTS$standardized_risks), 0
                    ),
                    "standardized risk combinations"
                )
            }
        } else {
            global_logger("WARN", "No analysis results found at:", analysis_results_path)
        }
    } else {
        global_logger("DEBUG", "Skipping analysis results loading (analysis_results_path = FALSE)")
    }
}


#' Train and save risk model using all available trials
#'
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param tau Time horizon for drop prediction (default 0.2 seconds)
#' @param sampling_freq Sampling frequency for resampling (default 90 Hz)
#' @param file_path Path to save the RDS file (default uses global risk_model_path)
#' @param use_by_interaction Whether to use factor-by smooths for shape differences (default TRUE)
#' @param standardized Whether to enable standardized training (default FALSE)
#' @return Fitted model object
#' @export
train_and_save_risk_model <- function(participants, trials, tau = 0.2,
                                      sampling_freq = 90, file_path = NULL, use_by_interaction = FALSE, standardized = FALSE) {
    tryCatch({
        # Use global risk model path if not specified
        if (is.null(file_path)) {
            ensure_global_data_initialized()
            file_path <- risk_model_path
        }
        # Ensure data_extra directory exists
        ensure_global_data_initialized()
        if (!dir.exists(dataExtraFolder)) {
            dir.create(dataExtraFolder, recursive = TRUE)
        }

        # Collect hazard samples from all trials using list for efficiency
        hazard_samples_list <- list()

        # Create all combinations efficiently
        combinations_df <- expand.grid(participant = participants, trial = trials, stringsAsFactors = FALSE)
        combinations_df <- unique(combinations_df) # Remove any duplicates
        total_combinations <- nrow(combinations_df)

        train_logger <- create_module_logger("TRAIN")
        train_logger("INFO", "Processing", total_combinations, "unique participant-trial combinations...")

        # Process each combination
        for (i in 1:nrow(combinations_df)) {
            participant <- combinations_df$participant[i]
            trial <- combinations_df$trial[i]
            train_logger("DEBUG", "Processing participant", participant, "trial", trial, "(combination", i, "/", total_combinations, ")")
            if (i %% 10 == 0 || i == total_combinations) {
                train_logger("DEBUG", "Progress:", i, "/", total_combinations, "combinations processed")
            }

            tryCatch(
                {
                    sim_data <- get_simulation_data(participant, trial, enable_risk = FALSE)
                    if (!is.null(sim_data) && nrow(sim_data) > 0) {
                        # Apply episode-based resampling to simulation data if requested
                        if (sampling_freq < 90) {
                            sim_data <- resample_simulation_per_episode(sim_data, sampling_freq)
                        }

                        hazard_samples <- build_hazard_samples(sim_data, tau)
                        if (nrow(hazard_samples) > 0) {
                            hazard_samples$participant <- participant
                            hazard_samples$trial <- trial
                            hazard_samples_list[[length(hazard_samples_list) + 1]] <- hazard_samples
                        }
                    }
                },
                error = function(e) {
                    train_logger("ERROR", "Error processing participant", participant, "trial", trial, ":", e$message)
                }
            )
        }

        # Combine all hazard samples efficiently
        if (length(hazard_samples_list) > 0) {
            all_hazard_samples <- data.table::rbindlist(hazard_samples_list, fill = TRUE)
            all_hazard_samples <- as.data.frame(all_hazard_samples)
        } else {
            all_hazard_samples <- data.frame()
        }

        if (nrow(all_hazard_samples) == 0) {
            train_logger("ERROR", "No hazard samples found. Cannot train risk model.")
            return(NULL)
        }

        train_logger("INFO", "Training risk model with", nrow(all_hazard_samples), "hazard samples from", length(unique(all_hazard_samples$participant)), "participants,", length(unique(all_hazard_samples$trial)), "trials")

        # Show data summary
        train_logger("DEBUG", "Data summary:")
        train_logger("DEBUG", "  - Total samples:", nrow(all_hazard_samples))
        train_logger("DEBUG", "  - Participants:", length(unique(all_hazard_samples$participant)))
        train_logger("DEBUG", "  - Trials:", length(unique(all_hazard_samples$trial)))
        train_logger("DEBUG", "  - Positive labels (drops):", sum(all_hazard_samples$drop_within_tau), "(", round(100 * mean(all_hazard_samples$drop_within_tau), 1), "%)")

        # Train risk model
        train_logger("INFO", "Step 2/3: Fitting mixed-effects GLM model...")
        model <- train_risk_model(all_hazard_samples, use_by_interaction = use_by_interaction)

        if (is.null(model)) {
            train_logger("ERROR", "Failed to train risk model")
            return(NULL)
        }

        # Save risk model
        train_logger("INFO", "Step 3/3: Saving model and hazard samples to disk...")
        save_risk_model(model, file_path, tau = tau)

        # Save hazard samples alongside the model
        ensure_global_data_initialized()
        saveRDS(all_hazard_samples, risk_hazard_samples_path)

        # Verify the files were created
        if (file.exists(file_path)) {
            file_size <- file.info(file_path)$size
            train_logger("INFO", "Risk model saved successfully to", file_path, "(", round(file_size / 1024^2, 2), "MB)")
        } else {
            train_logger("ERROR", "Model file was not created at", file_path)
        }

        if (file.exists(risk_hazard_samples_path)) {
            file_size <- file.info(risk_hazard_samples_path)$size
            train_logger("INFO", "Hazard samples saved successfully to", risk_hazard_samples_path, "(", round(file_size / 1024^2, 2), "MB)")
        } else {
            train_logger("ERROR", "Hazard samples file was not created at", risk_hazard_samples_path)
        }

        # Stuff for pooled standardized risks analysis, abandoned this as things were getting too complicated

        # ── NEW: Add per-sample predictions to saved hazard samples ──
        train_logger("INFO", "Step 4/5: Adding per-sample predictions to hazard samples...")
        ensure_global_data_initialized()

        # Add fixed-effects predictions (recommended for comparability across participants)
        annotate_hazard_predictions(model, all_hazard_samples,
            include_re = FALSE,
            out_path = risk_hazard_samples_preds_path
        )


        # Set up global variables with the newly trained model
        train_logger("INFO", "Step 5/6: Setting up global risk variables...")
        setup_global_risk_variables(analysis_results_path = FALSE)

        # Conditionally compute standardized risks based on parameter
        if (standardized) {
            # ── NEW: pooled standardized predictions by Condition × Phase ──
            train_logger("INFO", "Step 6/6: Computing pooled standardized risks by Condition × Phase...")
            std_means <- compute_pooled_standardized_risks(
                model,
                all_hazard_samples
            )
            
            # Return both model and standardized means
            return(list(model = model, standardized_means = std_means))
        } else {
            train_logger("INFO", "Step 6/6: Skipping standardized risk computation (disabled)")
            # Return just the model
            return(model)
        }
    })
}

#' Score all trials with saved risk model
#'
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param file_path Path to the saved RDS file (default uses global risk_model_path)
#' @param tau Time horizon for drop prediction (default 0.2 seconds)
#' @return Data frame with risk scores for all trials
#' @export
score_all_trials_with_saved_model <- function(participants, trials,
                                              file_path = NULL,
                                              tau = 0.2) {
    # Use global risk model path if not specified
    if (is.null(file_path)) {
        ensure_global_data_initialized()
        file_path <- risk_model_path
    }
    model <- load_risk_model(file_path)

    if (is.null(model)) {
        score_logger <- create_module_logger("SCORE")
        score_logger("ERROR", "No risk model found at", file_path)
        return(data.frame())
    }

    results_list <- list()

    for (participant in participants) {
        for (trial in trials) {
            tryCatch(
                {
                    sim_data <- get_simulation_data(participant, trial, enable_risk = FALSE)
                    if (!is.null(sim_data) && nrow(sim_data) > 0) {
                        risk_scores <- score_trial_with_model(sim_data, model, tau, use_factors = FALSE)
                        risk_scores$participant <- participant
                        risk_scores$trial <- trial
                        results_list[[length(results_list) + 1]] <- risk_scores
                    }
                },
                error = function(e) {
                    score_logger("ERROR", "Error scoring participant", participant, "trial", trial, ":", e$message)
                }
            )
        }
    }

    # Combine all results efficiently
    if (length(results_list) > 0) {
        results <- data.table::rbindlist(results_list, fill = TRUE)
        results <- as.data.frame(results)
    } else {
        results <- data.frame()
    }

    return(results)
}

# ─────────────────────────────────────────────────────────────────────────────
# Per-sample prediction annotation for saved hazard samples
# ─────────────────────────────────────────────────────────────────────────────

#' Annotate hazard samples with model predictions
#'
#' @param model Fitted model from train_risk_model()
#' @param hazard_samples Data frame from build_hazard_samples()
#' @param include_re Whether to include random effects (subject-specific predictions)
#' @param out_path Path to save the annotated hazard samples (NULL = use global path, FALSE = don't save)
#' @param chunk_by Column names to chunk by for processing large datasets
#' @return Annotated hazard samples data frame
#' @export
annotate_hazard_predictions <- function(model,
                                        hazard_samples,
                                        include_re = FALSE,
                                        out_path = NULL,
                                        chunk_by = c("participant")) {
    # Use global path if not specified, skip saving if FALSE
    if (is.null(out_path)) {
        ensure_global_data_initialized()
        out_path <- risk_hazard_samples_preds_path
    } else if (identical(out_path, FALSE)) {
        # Skip saving if explicitly set to FALSE
        out_path <- NULL
    }

    predict_logger <- create_module_logger("PREDICT")
    predict_logger("INFO", "Annotating hazard samples with", if (include_re) "subject-specific" else "fixed-effects", "predictions...")

    # Ensure factors are properly set - use original values if model levels are empty
    if ("condition" %in% names(model$model)) {
        model_cond_levels <- levels(model$model$condition)

        # If model levels are empty or NULL, use original data levels
        if (is.null(model_cond_levels) || length(model_cond_levels) == 0) {
            orig_cond_values <- unique(as.character(hazard_samples$condition))
            hazard_samples$condition <- factor(hazard_samples$condition, levels = orig_cond_values)
        } else {
            hazard_samples$condition <- factor(as.character(hazard_samples$condition), levels = model_cond_levels)
        }
    } else {
        hazard_samples$condition <- factor(hazard_samples$condition)
    }

    if ("phase" %in% names(model$model)) {
        model_phase_levels <- levels(model$model$phase)

        # If model levels are empty or NULL, use original data levels
        if (is.null(model_phase_levels) || length(model_phase_levels) == 0) {
            orig_phase_values <- unique(as.character(hazard_samples$phase))
            hazard_samples$phase <- factor(hazard_samples$phase, levels = orig_phase_values)
        } else {
            hazard_samples$phase <- factor(as.character(hazard_samples$phase), levels = model_phase_levels)
        }
    } else {
        hazard_samples$phase <- factor(hazard_samples$phase)
    }
    hazard_samples$participant <- factor(hazard_samples$participant)

    # Choose whether to include random effect smooth
    pred_args <- list(type = "response")
    if (!include_re) pred_args$exclude <- "s(participant)" # fixed-effects only

    # Predict in chunks (by participant to keep factor levels stable and memory friendly)
    chunk_by <- intersect(chunk_by, names(hazard_samples))
    if (length(chunk_by) == 0) chunk_by <- "participant"

    split_idx <- split(seq_len(nrow(hazard_samples)), hazard_samples[[chunk_by[1]]])

    p_hat <- numeric(nrow(hazard_samples))
    predict_logger("DEBUG", "Processing", length(split_idx), "participants in chunks...")

    for (i in seq_along(split_idx)) {
        ix <- split_idx[[i]]
        newdf <- hazard_samples[ix, , drop = FALSE]

        tryCatch(
            {
                pred_result <- do.call(predict, c(list(object = model, newdata = newdf), pred_args))
                p_hat[ix] <- pred_result
            },
            error = function(e) {
                predict_logger("ERROR", "ERROR in chunk", i, ":", e$message)
                p_hat[ix] <- NA_real_
            }
        )

        if (i %% 10 == 0 || i == length(split_idx)) {
            predict_logger("DEBUG", "Progress:", i, "/", length(split_idx), "participants (", round(100 * i / length(split_idx), 1), "%)")
        }
    }

    # Attach prediction column (name indicates FE/RE)
    colname <- if (include_re) "p_hat_re" else "p_hat_fe"
    hazard_samples[[colname]] <- p_hat

    # Also add eta (linear predictor) for rate calculations
    eta_colname <- if (include_re) "eta_re" else "eta_fe"
    eta_args <- list(type = "link")
    if (!include_re) eta_args$exclude <- "s(participant)"

    eta_hat <- numeric(nrow(hazard_samples))
    for (i in seq_along(split_idx)) {
        ix <- split_idx[[i]]
        newdf <- hazard_samples[ix, , drop = FALSE]

        tryCatch(
            {
                eta_result <- do.call(predict, c(list(object = model, newdata = newdf), eta_args))
                eta_hat[ix] <- eta_result
            },
            error = function(e) {
                eta_hat[ix] <- NA_real_
            }
        )
    }
    hazard_samples[[eta_colname]] <- eta_hat

    # Add 1s drop risk calculation using eta and tau
    # Get tau from model attributes
    tau <- attr(model, "tau")
    if (is.null(tau) || tau <= 0) {
        # Try to get tau from global variable as fallback
        if (exists("GLOBAL_RISK_TAU") && !is.null(GLOBAL_RISK_TAU)) {
            tau <- GLOBAL_RISK_TAU
            predict_logger("WARN", "Model missing tau attribute, using global tau =", tau, "seconds for 1s drop risk")
        } else {
            # Use default tau as last resort
            tau <- 0.2
            predict_logger("WARN", "Model missing tau attribute and no global tau found, using default tau =", tau, "seconds for 1s drop risk")
        }
    } else {
        predict_logger("DEBUG", "Using model tau =", tau, "seconds for 1s drop risk calculation")
    }

    # Calculate 1s drop risk: lambda = exp(eta) / tau, then p_1s = 1 - exp(-lambda * 1)
    lambda_1s <- exp(eta_hat) / tau # per-second hazard rate
    p_1s <- 1 - exp(-lambda_1s) # 1-second drop risk

    # Add 1s drop risk column (name indicates FE/RE)
    risk_1s_colname <- if (include_re) "risk_1s_re" else "risk_1s_fe"
    hazard_samples[[risk_1s_colname]] <- p_1s

    predict_logger(
        "DEBUG", "Added", risk_1s_colname, "column with 1s drop risk (range:",
        sprintf("%.6f to %.6f", min(p_1s, na.rm = TRUE), max(p_1s, na.rm = TRUE)), ")"
    )

    # Save and return (only if out_path is not NULL)
    if (!is.null(out_path)) {
        saveRDS(hazard_samples, out_path)
        setup_global_risk_variables(hazard_samples_path = out_path, model_path = FALSE, analysis_results_path = FALSE)
        predict_logger("INFO", "Saved hazard samples with predictions to:", out_path)
    } else {
        predict_logger("DEBUG", "Skipped saving hazard samples (out_path = FALSE)")
    }
    invisible(hazard_samples)
}

# ─────────────────────────────────────────────────────────────────────────────
# Pooled, state-conditional standardized mean risks by Condition × Phase
# ─────────────────────────────────────────────────────────────────────────────

#' Compute pooled standardized risks by Condition × Phase
#'
#' @param model Fitted model from train_risk_model()
#' @param hazard_samples Data frame from build_hazard_samples()
#' @param analysis_results_path Path to save analysis results (NULL = use default path, FALSE = skip saving)
#' @return List containing standardized_risks and analysis_results
#' @export
compute_pooled_standardized_risks <- function(model, hazard_samples, analysis_results_path = NULL) {
    std_logger <- create_module_logger("STANDARDIZED")
    std_logger("DEBUG", "compute_pooled_standardized_risks called")

    # sanity
    need <- c("e", "v", "participant", "condition", "phase")
    miss <- setdiff(need, names(hazard_samples))
    if (length(miss) > 0) stop("hazard_samples missing: ", paste(miss, collapse = ", "))

    # Get tau from model attributes (CRITICAL for rate calculations)
    tau <- attr(model, "tau")
    if (is.null(tau) || tau <= 0) {
        # Try to get tau from global variable as fallback
        if (exists("GLOBAL_RISK_TAU") && !is.null(GLOBAL_RISK_TAU)) {
            tau <- GLOBAL_RISK_TAU
            std_logger("WARN", "Model missing tau attribute, using global tau =", tau, "seconds")
        } else {
            # Use default tau as last resort
            tau <- 0.2
            std_logger("WARN", "Model missing tau attribute and no global tau found, using default tau =", tau, "seconds")
        }
    } else {
        std_logger("DEBUG", "Using model tau =", tau, "seconds for rate calculations")
    }

    # factor levels to match the model
    hazard_samples$participant <- factor(hazard_samples$participant)
    if ("condition" %in% names(model$model)) {
        hazard_samples$condition <- factor(hazard_samples$condition,
            levels = levels(model$model$condition)
        )
    } else {
        hazard_samples$condition <- factor(hazard_samples$condition)
    }
    if ("phase" %in% names(model$model)) {
        hazard_samples$phase <- factor(hazard_samples$phase,
            levels = levels(model$model$phase)
        )
    } else {
        hazard_samples$phase <- factor(hazard_samples$phase)
    }

    # reference X: pooled (e, v, participant) rows
    ref_df <- hazard_samples[, c("e", "v", "participant")]
    ref_df$participant <- factor(ref_df$participant)

    # estimate bin width Δt from within-segment time differences (for info only, not used in calculations)
    dt <- NA_real_
    if (all(c("time", "respawn_segment") %in% names(hazard_samples))) {
        dt_vec <- hazard_samples |>
            dplyr::arrange(respawn_segment, time) |>
            dplyr::group_by(respawn_segment) |>
            dplyr::mutate(dt_seg = time - dplyr::lag(time)) |>
            dplyr::summarise(
                dt_med = stats::median(dt_seg[is.finite(dt_seg) & dt_seg > 0], na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::pull(dt_med)
        dt <- stats::median(dt_vec[is.finite(dt_vec) & dt_vec > 0], na.rm = TRUE)
        std_logger("DEBUG", "Estimated dt =", dt, "seconds (for info only)")
    }

    # Condition levels
    if ("condition" %in% names(model$model)) {
        cond_levels <- levels(model$model$condition)
        if (is.null(cond_levels)) cond_levels <- unique(model$model$condition)
    } else {
        cond_levels <- levels(hazard_samples$condition)
        if (is.null(cond_levels)) cond_levels <- unique(hazard_samples$condition)
    }

    # Phase levels
    if ("phase" %in% names(model$model)) {
        phase_levels <- levels(model$model$phase)
        if (is.null(phase_levels)) phase_levels <- unique(model$model$phase)
    } else {
        phase_levels <- levels(hazard_samples$phase)
        if (is.null(phase_levels)) phase_levels <- unique(hazard_samples$phase)
    }

    # Ensure we have valid levels
    if (is.null(cond_levels) || length(cond_levels) == 0) {
        cond_levels <- unique(hazard_samples$condition)
    }
    if (is.null(phase_levels) || length(phase_levels) == 0) {
        phase_levels <- unique(hazard_samples$phase)
    }

    res_list <- list()

    std_logger("INFO", "Computing standardized mapping scores for", length(cond_levels), "condition ×", length(phase_levels), "phase combinations")
    std_logger("DEBUG", "Using fixed reference state distribution (e, v, participant) with standardized condition×phase labels")

    # Ensure we have valid levels to work with
    if (length(cond_levels) == 0) {
        std_logger("ERROR", "No valid condition levels found")
        return(NULL)
    }
    if (length(phase_levels) == 0) {
        std_logger("ERROR", "No valid phase levels found")
        return(NULL)
    }

    # Debug: Show model's factor levels
    if ("condition" %in% names(model$model)) {
        std_logger("DEBUG", "Model condition levels:", paste(levels(model$model$condition), collapse = ", "))
    }
    if ("phase" %in% names(model$model)) {
        std_logger("DEBUG", "Model phase levels:", paste(levels(model$model$phase), collapse = ", "))
    }
    if ("cond_phase" %in% names(model$model)) {
        std_logger("DEBUG", "Model cond_phase levels:", paste(levels(model$model$cond_phase), collapse = ", "))
    }

    # Process all condition × phase combinations
    res_list <- list()

    for (cn in cond_levels) {
        for (ph in phase_levels) {
            std_logger("DEBUG", "Processing: condition='", cn, "', phase='", ph, "'")

            # Create standardized prediction data using fixed reference states
            newdf <- ref_df

            # CRITICAL: Align factor levels with model's training levels
            # This ensures we only use combinations that existed in training
            skip_combination <- FALSE

            if ("condition" %in% names(model$model)) {
                model_cond_levels <- levels(model$model$condition)
                # If model levels are empty, use the levels we determined earlier
                if (is.null(model_cond_levels) || length(model_cond_levels) == 0) {
                    std_logger("WARN", "Model condition levels are empty, using fallback levels")
                    model_cond_levels <- cond_levels
                }

                if (cn %in% model_cond_levels) {
                    newdf$condition <- factor(cn, levels = model_cond_levels)
                } else {
                    std_logger("WARN", "Condition '", cn, "' not in model training levels:", paste(model_cond_levels, collapse = ", "))
                    skip_combination <- TRUE
                }
            } else {
                newdf$condition <- factor(cn, levels = cond_levels)
            }

            if ("phase" %in% names(model$model)) {
                model_phase_levels <- levels(model$model$phase)
                # If model levels are empty, use the levels we determined earlier
                if (is.null(model_phase_levels) || length(model_phase_levels) == 0) {
                    std_logger("WARN", "Model phase levels are empty, using fallback levels")
                    model_phase_levels <- phase_levels
                }

                if (ph %in% model_phase_levels) {
                    newdf$phase <- factor(ph, levels = model_phase_levels)
                } else {
                    std_logger("WARN", "Phase '", ph, "' not in model training levels:", paste(model_phase_levels, collapse = ", "))
                    skip_combination <- TRUE
                }
            } else {
                newdf$phase <- factor(ph, levels = phase_levels)
            }

            # Skip prediction if factor levels don't match
            if (skip_combination) {
                std_logger("WARN", "Skipping prediction due to factor level mismatch")
                next
            }

            # Create cond_phase interaction factor for factor-by smooths
            if ("cond_phase" %in% names(model$model)) {
                # Create the interaction factor using the same method as training
                newdf$cond_phase <- interaction(newdf$condition, newdf$phase, drop = TRUE)

                # Get the model's cond_phase levels
                model_cond_phase_levels <- levels(model$model$cond_phase)

                # Debug: Show what we're trying to create vs what the model expects
                std_logger("DEBUG", "Created cond_phase:", paste(levels(newdf$cond_phase), collapse = ", "))
                std_logger("DEBUG", "Model expects cond_phase:", paste(model_cond_phase_levels, collapse = ", "))

                # Check if our created levels match the model's levels
                if (!all(levels(newdf$cond_phase) %in% model_cond_phase_levels)) {
                    std_logger("WARN", "Some cond_phase levels not in model training levels")
                    # Try to align the levels
                    newdf$cond_phase <- factor(newdf$cond_phase, levels = model_cond_phase_levels)
                }
            }

            # Skip prediction if cond_phase levels don't match
            if (skip_combination) {
                std_logger("WARN", "Skipping prediction due to cond_phase level mismatch")
                next
            }

            std_logger("DEBUG", "Computing standardized risk for condition='", cn, "', phase='", ph, "'")

            # Predict fixed-effects hazard; exclude random effect smooth for population-level mapping
            # Use parallel processing for faster prediction (same as training)
            n_cores <- parallel::detectCores()
            std_logger("DEBUG", "Using", n_cores, "cores for parallel prediction")
            std_logger("DEBUG", "Predicting on", nrow(newdf), "rows...")

            # Add timing
            start_time <- Sys.time()
            eta <- predict(model, newdata = newdf, type = "link", exclude = "s(participant)", nthreads = n_cores)
            end_time <- Sys.time()
            std_logger("DEBUG", "Prediction completed in", round(as.numeric(end_time - start_time, units = "secs"), 2), "seconds")

            # per-τ drop probability (for backward compat with 'mean_hazard'):
            p_tau <- 1 - exp(-exp(eta))
            mean_h <- mean(tapply(p_tau, newdf$participant, mean, na.rm = TRUE) |> unlist(), na.rm = TRUE)

            # sampling-invariant rate and 1-second risk using model's tau:
            lambda <- exp(eta) / tau # per-second hazard rate
            risk1s <- 1 - exp(-lambda) # 1-second risk
            exp_dpm <- 60 * mean(tapply(lambda, newdf$participant, mean, na.rm = TRUE) |> unlist(), na.rm = TRUE)
            risk_1s <- mean(tapply(risk1s, newdf$participant, mean, na.rm = TRUE) |> unlist(), na.rm = TRUE)

            res_list[[paste(cn, ph, sep = "|")]] <- data.frame(
                condition = cn,
                phase = ph,
                n_participants = length(unique(newdf$participant)),
                mean_hazard = mean_h,
                dt_estimate = dt,
                exp_drops_per_min = exp_dpm,
                risk_1s = risk_1s
            )
        }
    }

    out <- do.call(rbind, res_list)
    rownames(out) <- NULL

    std_logger("INFO", "Successfully computed standardized risks for", nrow(out), "condition×phase combinations")
    if (nrow(out) > 0) {
        std_logger("DEBUG", "Computed combinations:")
        for (i in seq_len(nrow(out))) {
            std_logger("DEBUG", "  -", out$condition[i], "×", out$phase[i], ": risk_1s =", round(out$risk_1s[i], 6), ", mean_hazard =", round(out$mean_hazard[i], 6))
        }
    }

    # Perform comprehensive statistical analysis
    analysis_results <- perform_risk_analysis(model, hazard_samples, out, analysis_results_path)

    # Always print standardized risks to console
    std_logger("INFO", "Pooled standardized risks computed:")
    print(out)


    return(list(
        standardized_risks = out,
        analysis_results   = analysis_results
    ))
}


# ─────────────────────────────────────────────────────────────────────────────
# Comprehensive Risk Analysis: Statistical Tests, DiD, Bootstrap CIs
# ─────────────────────────────────────────────────────────────────────────────

#' Perform comprehensive risk analysis with statistical tests
#'
#' @param model Fitted model from train_risk_model()
#' @param hazard_samples Data frame from build_hazard_samples()
#' @param std_means Standardized means data frame
#' @param analysis_results_path Path to save analysis results (NULL = use default path, FALSE = skip saving)
#' @return Analysis results list
#' @export
perform_risk_analysis <- function(model, hazard_samples, std_means, analysis_results_path = NULL) {
    analysis_logger <- create_module_logger("ANALYSIS")
    analysis_logger("INFO", "Performing comprehensive risk analysis...")

    # 0) Sanity: std_means must be non-empty
    if (is.null(std_means) || !nrow(std_means)) {
        stop("[ANALYSIS] standardized means are empty; cannot proceed.")
    }

    # Standardize factor levels to match model
    hazard_samples$participant <- factor(hazard_samples$participant)
    if ("condition" %in% names(model$model)) {
        hazard_samples$condition <- factor(hazard_samples$condition, levels = levels(model$model$condition))
    } else {
        hazard_samples$condition <- factor(hazard_samples$condition)
    }
    if ("phase" %in% names(model$model)) {
        hazard_samples$phase <- factor(hazard_samples$phase, levels = levels(model$model$phase))
    } else {
        hazard_samples$phase <- factor(hazard_samples$phase)
    }

    # Use levels from std_means (robust)
    cond_levels <- unique(as.character(std_means$condition))
    phase_levels <- unique(as.character(std_means$phase))
    analysis_logger("DEBUG", "cond_levels =", cond_levels)
    analysis_logger("DEBUG", "phase_levels =", phase_levels)

    # 1) Condition × Phase interaction (hazard model) - try ML LRT, else Wald
    analysis_logger("DEBUG", "Testing condition × phase interaction...")
    m_full_ml <- tryCatch(update(model, method = "ML"), error = function(e) NULL)
    m_noint_ml <- tryCatch(update(model, . ~ . - condition:phase, method = "ML"), error = function(e) NULL)

    interaction_test <- if (!is.null(m_full_ml) && !is.null(m_noint_ml)) {
        analysis_logger("DEBUG", "Using LRT (ML) for interaction test...")
        anova(m_noint_ml, m_full_ml, test = "Chisq")
    } else {
        warning("[ANALYSIS] Could not refit models with ML for LRT. Will use Wald test.")
        NULL
    }

    # Fallback: Wald test
    wald_interaction <- NULL
    if (is.null(interaction_test)) {
        analysis_logger("DEBUG", "Using Wald test for interaction...")
        cf <- coef(model)
        V <- model$Vp
        idx <- grep("condition.*:phase", names(cf))
        idx <- idx[is.finite(cf[idx])]
        if (length(idx) > 0) {
            bI <- cf[idx]
            VI <- V[idx, idx, drop = FALSE]
            ok <- is.finite(bI) & is.finite(diag(VI))
            bI <- bI[ok]
            VI <- VI[ok, ok, drop = FALSE]
            if (length(bI) > 0 && all(is.finite(VI))) {
                WI <- try(solve(VI), silent = TRUE)
                if (!inherits(WI, "try-error")) {
                    W <- as.numeric(t(bI) %*% WI %*% bI)
                    df <- length(bI)
                    p <- stats::pchisq(W, df = df, lower.tail = FALSE)
                    wald_interaction <- data.frame(statistic = W, df = df, p.value = p)
                }
            }
        }
    }

    # 2) Within-condition deltas and DiD (from std_means)
    analysis_logger("DEBUG", "Computing within-condition changes and DiD...")
    mm <- std_means |>
        dplyr::mutate(
            condition = as.character(condition),
            phase = as.character(phase)
        )

    needed_phases <- c("baseline_task", "retention", "transfer")
    if (!all(needed_phases %in% unique(mm$phase))) {
        warning(
            "[ANALYSIS] Expected phases missing in standardized means: ",
            paste(setdiff(needed_phases, unique(mm$phase)), collapse = ", ")
        )
    }

    delta <- std_means |>
        dplyr::select(condition, phase, mean_hazard) |>
        tidyr::pivot_wider(id_cols = condition, names_from = phase, values_from = mean_hazard) |>
        dplyr::mutate(
            d_retention = retention - baseline_task,
            d_transfer  = transfer - baseline_task
        )

    get_mh <- function(cond, ph) {
        v <- mm$mean_hazard[mm$condition == cond & mm$phase == ph]
        if (length(v) == 0) NA_real_ else v[1]
    }

    DiD <- tibble::tibble(
        contrast = c(
            "PV vs Control (Retention)", "PV vs Control (Transfer)",
            "P  vs Control (Retention)", "P  vs Control (Transfer)",
            "P  vs PV (Retention)", "P  vs PV (Transfer)"
        ),
        DiD = c(
            (get_mh("perturbation_visualization", "retention") - get_mh("perturbation_visualization", "baseline_task")) -
                (get_mh("control", "retention") - get_mh("control", "baseline_task")),
            (get_mh("perturbation_visualization", "transfer") - get_mh("perturbation_visualization", "baseline_task")) -
                (get_mh("control", "transfer") - get_mh("control", "baseline_task")),
            (get_mh("perturbation", "retention") - get_mh("perturbation", "baseline_task")) -
                (get_mh("control", "retention") - get_mh("control", "baseline_task")),
            (get_mh("perturbation", "transfer") - get_mh("perturbation", "baseline_task")) -
                (get_mh("control", "transfer") - get_mh("control", "baseline_task")),
            (get_mh("perturbation", "retention") - get_mh("perturbation", "baseline_task")) -
                (get_mh("perturbation_visualization", "retention") - get_mh("perturbation_visualization", "baseline_task")),
            (get_mh("perturbation", "transfer") - get_mh("perturbation", "baseline_task")) -
                (get_mh("perturbation_visualization", "transfer") - get_mh("perturbation_visualization", "baseline_task"))
        )
    )

    # 3) Parametric bootstrap CIs (guarded)
    analysis_logger("DEBUG", "Computing parametric bootstrap CIs...")
    means_CI <- NULL
    DiD_boot <- NULL
    within_ci <- NULL

    if (!all(c("e", "v", "participant") %in% names(hazard_samples))) {
        warning("[ANALYSIS] Missing e/v/participant in hazard_samples; skipping bootstrap.")
    } else {
        ref_df <- hazard_samples[, c("e", "v", "participant")]
        ref_df$participant <- factor(ref_df$participant)

        cond_levels_b <- unique(mm$condition)
        phase_levels_b <- unique(mm$phase)

        beta_hat <- coef(model)
        V <- model$Vp

        if (is.null(V) || any(!is.finite(V))) {
            warning("[ANALYSIS] No valid covariance matrix; skipping bootstrap.")
        } else {
            B <- 100L # increase to 1000 for final runs
            analysis_logger("INFO", "Starting optimized parametric bootstrap with", B, "iterations...")
            analysis_logger("DEBUG", "OPTIMIZATIONS: Vectorized bootstrap + rowsum aggregation + outer combo loop")
            analysis_logger("DEBUG", "METRIC: Computing 1-second risk only (risk_1s)")
            analysis_logger("DEBUG", "EXPECTED BENEFITS: Memory efficient + 10-50x faster + full condition×phase coverage")
            flush.console()

            # Get tau from model attributes for rate calculations
            tau_model <- attr(model, "tau")
            if (is.null(tau_model)) {
                tau_model <- 0.2 # fallback
                analysis_logger("WARN", "No tau found in model, using fallback: 0.2s")
            }

            # 1) BEFORE THE LOOP: Cache reference frame and participant indices
            analysis_logger("DEBUG", "Caching reference frame and participant indices...")
            ref_df <- hazard_samples[, c("e", "v", "participant")]
            ref_df$participant <- factor(ref_df$participant)

            # Create participant index and counts (reused for every combo)
            pid <- as.integer(factor(ref_df$participant))
            n_g <- tabulate(pid) # length = #participants
            G <- length(n_g) # number of participants

            analysis_logger("DEBUG", "Reference frame:", nrow(ref_df), "rows,", G, "participants")

            # Precompute Cholesky decomposition for coefficient sampling
            set.seed(123)
            p <- length(beta_hat)
            L <- chol(V)

            # Draw all coefficients at once (p × B matrix)
            Z_matrix <- matrix(rnorm(p * B), p, B)
            Beta_matrix <- beta_hat + L %*% Z_matrix # p × B

            # Storage for final results (B-length vectors per combo)
            combo_results <- list()

            # 2) OUTER LOOP: Process each condition×phase combination
            for (cond in cond_levels_b) {
                for (ph in phase_levels_b) {
                    key <- paste(cond, ph, sep = "|")
                    analysis_logger("DEBUG", "Processing combo:", key)

                    # Create newdf by copying ref_df and setting condition/phase
                    newdf <- ref_df
                    newdf$condition <- factor(cond, levels = cond_levels_b)
                    newdf$phase <- factor(ph, levels = phase_levels_b)

                    # Add cond_phase factor for factor-by smooths
                    if ("cond_phase" %in% names(model$model)) {
                        expected_cond_phase <- paste(cond, ph, sep = ".")
                        model_cond_phase_levels <- levels(model$model$cond_phase)
                        if (expected_cond_phase %in% model_cond_phase_levels) {
                            newdf$cond_phase <- factor(expected_cond_phase, levels = model_cond_phase_levels)
                        } else {
                            analysis_logger("WARN", "Skipping combo '", key, "' - cond_phase '", expected_cond_phase, "' not in model levels")
                            next
                        }
                    }

                    # 3) VECTORIZED BOOTSTRAP: Precompute design matrix once per combo
                    X <- predict(model, newdata = newdf, type = "lpmatrix", exclude = "s(participant)")

                    # Compute all linear predictors at once (n × B)
                    Eta <- X %*% Beta_matrix # n × B

                    # Convert to 1-second risk only (the metric we care about)
                    Lambda <- exp(Eta) / tau_model # per-second hazard rate (n × B)
                    Risk1s <- 1 - exp(-Lambda) # 1-second risk (n × B)

                    # 4) AGGREGATE BY PARTICIPANT WITH ROWSUM (the fast bit)
                    # Sum by participant across ALL bootstrap columns in one shot
                    S_1s <- rowsum(Risk1s, pid) # (G × B) sums for 1-second risk

                    # Turn sums into participant means
                    M_1s <- sweep(S_1s, 1, n_g, "/") # divide each row g by its n_g

                    # Equal-weight participants to get the combo's pooled mean per bootstrap
                    overall_1s <- colMeans(M_1s) # length B

                    # 5) STORE RESULTS (only 1-second risk)
                    combo_results[[key]] <- overall_1s

                    analysis_logger("DEBUG", "Completed combo", key, ":", B, "bootstrap samples")
                }
            }

            # OLD CHUNKED PROCESSING REMOVED - REPLACED WITH VECTORIZED APPROACH ABOVE

            # Convert to expected format for downstream analysis
            analysis_logger("DEBUG", "Converting results to expected format...")
            boot_list <- vector("list", B)
            for (b in seq_len(B)) {
                if (b %% 20 == 0 || b == B) {
                    analysis_logger("DEBUG", "Converting bootstrap iteration:", b, "/", B, "(", round(100 * b / B, 1), "%)")
                    flush.console()
                }

                # Extract results for this bootstrap iteration
                grid <- expand.grid(
                    condition = cond_levels_b,
                    phase = phase_levels_b,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
                )

                grid$mean_hazard <- sapply(seq_len(nrow(grid)), function(i) {
                    key <- paste(grid$condition[i], grid$phase[i], sep = "|")
                    if (key %in% names(combo_results)) {
                        combo_results[[key]][b] # Direct access to 1-second risk vector
                    } else {
                        NA_real_
                    }
                })

                boot_list[[b]] <- grid
            }

            # OLD CODE REMOVED - END OF REPLACEMENT
            if (FALSE) { # This block is disabled - old chunked processing
                for (chunk_idx in seq_len(n_chunks)) {
                    start_row <- (chunk_idx - 1) * chunk_n + 1
                    end_row <- min(chunk_idx * chunk_n, n_rows)
                    chunk_rows <- start_row:end_row

                    # Build prediction data for this chunk
                    newdf_chunk <- ref_df[chunk_rows, , drop = FALSE]
                    newdf_chunk$condition <- factor(cond, levels = cond_levels_b)
                    newdf_chunk$phase <- factor(ph, levels = phase_levels_b)

                    # Add cond_phase factor for factor-by smooths
                    if ("cond_phase" %in% names(model$model)) {
                        expected_cond_phase <- paste(cond, ph, sep = ".")
                        model_cond_phase_levels <- levels(model$model$cond_phase)
                        if (expected_cond_phase %in% model_cond_phase_levels) {
                            newdf_chunk$cond_phase <- factor(expected_cond_phase, levels = model_cond_phase_levels)
                        } else {
                            analysis_logger("WARN", "Skipping chunk for combo '", key, "' - cond_phase '", expected_cond_phase, "' not in model levels")
                            next
                        }
                    }

                    # Get design matrix for this chunk
                    X_chunk <- predict(model,
                        newdata = newdf_chunk, type = "lpmatrix",
                        exclude = "s(participant)", newdata.guaranteed = TRUE
                    )

                    # Process beta draws in batches
                    for (batch_start in seq(1, B, by = b_batch)) {
                        batch_end <- min(batch_start + b_batch - 1, B)
                        batch_size <- batch_end - batch_start + 1
                        batch_cols <- batch_start:batch_end

                        # Draw coefficients for this batch
                        Z_batch <- matrix(rnorm(p * batch_size), p, batch_size)
                        Beta_batch <- beta_hat + L %*% Z_batch

                        # Compute linear predictors for this chunk×batch
                        Eta_chunk <- X_chunk %*% Beta_batch # chunk_n × batch_size

                        # Compute risk_1s directly (the metric we care about)
                        lambda_chunk <- exp(Eta_chunk) / tau_model
                        p1_chunk <- 1 - exp(-lambda_chunk) # chunk_n × batch_size

                        # Efficient aggregation using rowsum (fast C code)
                        pid_idx_chunk <- as.numeric(newdf_chunk$participant)

                        # Sum p1_chunk by participant
                        S <- rowsum(p1_chunk, group = pid_idx_chunk) # n_participants × batch_size

                        # Count samples per participant (only need to do this once per chunk)
                        if (batch_start == 1) {
                            C <- rowsum(matrix(1, nrow = nrow(p1_chunk), ncol = 1), group = pid_idx_chunk)
                            cnt_P[participant_map %in% rownames(S)] <- cnt_P[participant_map %in% rownames(S)] + as.numeric(C)
                        }

                        # Accumulate sums
                        participant_indices <- match(rownames(S), participant_map)
                        sum_BxP[batch_cols, participant_indices] <- sum_BxP[batch_cols, participant_indices] + t(S)
                    }

                    if (chunk_idx %% 5 == 0 || chunk_idx == n_chunks) {
                        analysis_logger("DEBUG", "Completed chunk", chunk_idx, "/", n_chunks, "for combo", key)
                    }
                }

                # Final aggregation: divide by counts and take mean across participants
                participant_means <- sum_BxP / cnt_P # B × n_participants
                overall_means <- rowMeans(participant_means, na.rm = TRUE) # B-length vector

                # Store only the B-length result vector
                combo_results[[key]] <- overall_means

                analysis_logger("DEBUG", "Completed combo", key, ":", length(overall_means), "bootstrap samples")
            } # End of disabled old code block

            analysis_logger("INFO", "Bootstrap completed, computing confidence intervals...")
            flush.console()
            boot_arr <- dplyr::bind_rows(boot_list, .id = "boot_id")

            if (!nrow(boot_arr)) {
                warning("[ANALYSIS] Bootstrap produced no rows; skipping CIs.")
            } else {
                # Means + CIs
                means_CI <- boot_arr |>
                    dplyr::group_by(condition, phase) |>
                    dplyr::summarise(
                        mean = mean(mean_hazard),
                        lo = stats::quantile(mean_hazard, 0.025, na.rm = TRUE),
                        hi = stats::quantile(mean_hazard, 0.975, na.rm = TRUE),
                        .groups = "drop"
                    )

                # DiD bootstrap
                did_from_df <- function(df, condA, condB, phase) {
                    wide <- df |>
                        tidyr::pivot_wider(id_cols = condition, names_from = phase, values_from = mean_hazard)
                    if (!all(c("baseline_task", phase) %in% names(wide))) {
                        return(NA_real_)
                    }
                    as.numeric((wide[wide$condition == condA, phase] - wide[wide$condition == condA, "baseline_task"]) -
                        (wide[wide$condition == condB, phase] - wide[wide$condition == condB, "baseline_task"]))
                }

                did_names <- rbind(
                    c("perturbation_visualization", "control", "retention"),
                    c("perturbation_visualization", "control", "transfer"),
                    c("perturbation", "control", "retention"),
                    c("perturbation", "control", "transfer"),
                    c("perturbation", "perturbation_visualization", "retention"),
                    c("perturbation", "perturbation_visualization", "transfer")
                )
                pts <- apply(did_names, 1, \(r) did_from_df(std_means, r[1], r[2], r[3]))

                analysis_logger("DEBUG", "Computing DiD bootstrap samples...")
                flush.console()
                did_boot_mat <- sapply(seq_len(B), function(b) {
                    if (b %% 20 == 0 || b == B) {
                        analysis_logger("DEBUG", "DiD bootstrap progress:", b, "/", B, "(", round(100 * b / B, 1), "%)")
                        flush.console()
                    }
                    dfb <- boot_list[[b]]
                    apply(did_names, 1, \(r) did_from_df(dfb, r[1], r[2], r[3]))
                })

                # Bootstrap p-values for DiD (two-sided) + Holm
                did_pvals <- apply(did_boot_mat, 1, function(x) {
                    x <- x[is.finite(x)]
                    if (!length(x)) {
                        return(NA_real_)
                    }
                    2 * min(mean(x >= 0), mean(x <= 0))
                })
                did_pvals_holm <- p.adjust(did_pvals, method = "holm")

                DiD_boot <- tibble::tibble(
                    contrast = c(
                        "PV vs Control (Retention)", "PV vs Control (Transfer)",
                        "P vs Control (Retention)", "P vs Control (Transfer)",
                        "P vs PV (Retention)", "P vs PV (Transfer)"
                    ),
                    point = as.numeric(pts),
                    lo = apply(did_boot_mat, 1, \(x) stats::quantile(x, .025, na.rm = TRUE)),
                    med = apply(did_boot_mat, 1, \(x) stats::quantile(x, .5, na.rm = TRUE)),
                    hi = apply(did_boot_mat, 1, \(x) stats::quantile(x, .975, na.rm = TRUE)),
                    p_boot = as.numeric(did_pvals),
                    p_boot_holm = as.numeric(did_pvals_holm)
                )

                # Within-condition deltas: CIs + p-values
                analysis_logger("DEBUG", "Computing bootstrap p-values for within-condition changes...")
                flush.console()

                within_boot_list <- lapply(boot_list, function(dfb) {
                    wide <- dfb |>
                        tidyr::pivot_wider(id_cols = condition, names_from = phase, values_from = mean_hazard)
                    tibble::tibble(
                        condition   = wide$condition,
                        d_retention = wide$retention - wide$baseline_task,
                        d_transfer  = wide$transfer - wide$baseline_task
                    )
                })
                within_boot <- dplyr::bind_rows(within_boot_list, .id = "boot_id")

                delta_point <- std_means |>
                    dplyr::select(condition, phase, mean_hazard) |>
                    tidyr::pivot_wider(id_cols = condition, names_from = phase, values_from = mean_hazard) |>
                    dplyr::transmute(
                        condition,
                        d_retention = retention - baseline_task,
                        d_transfer  = transfer - baseline_task
                    )

                p_within <- within_boot |>
                    tidyr::pivot_longer(cols = c(d_retention, d_transfer), names_to = "delta", values_to = "val") |>
                    dplyr::group_by(condition, delta) |>
                    dplyr::summarise(
                        p_boot = {
                            v <- val[is.finite(val)]
                            if (!length(v)) NA_real_ else 2 * min(mean(v >= 0), mean(v <= 0))
                        },
                        .groups = "drop"
                    ) |>
                    dplyr::group_by(delta) |>
                    dplyr::mutate(p_boot_holm = p.adjust(p_boot, method = "holm")) |>
                    dplyr::ungroup()

                within_ci <- within_boot |>
                    tidyr::pivot_longer(cols = c(d_retention, d_transfer), names_to = "delta", values_to = "val") |>
                    dplyr::group_by(condition, delta) |>
                    dplyr::summarise(
                        lo = stats::quantile(val, .025, na.rm = TRUE),
                        med = stats::quantile(val, .5, na.rm = TRUE),
                        hi = stats::quantile(val, .975, na.rm = TRUE),
                        .groups = "drop"
                    ) |>
                    dplyr::left_join(
                        delta_point |>
                            tidyr::pivot_longer(cols = c(d_retention, d_transfer), names_to = "delta", values_to = "point"),
                        by = c("condition", "delta")
                    ) |>
                    dplyr::left_join(p_within, by = c("condition", "delta"))
            } # end non-empty boot_arr
        } # end valid V
    } # end bootstrap branch

    # 4) Optional plot (only if means_CI exists and has data)
    if (!is.null(means_CI) && nrow(means_CI) > 0) {
        analysis_logger("DEBUG", "Creating visualization...")
        p <- ggplot2::ggplot(
            means_CI,
            ggplot2::aes(phase, mean,
                ymin = lo, ymax = hi,
                group = condition, color = condition
            )
        ) +
            ggplot2::geom_line() +
            ggplot2::geom_point() +
            ggplot2::geom_errorbar(width = .1) +
            ggplot2::labs(y = "Standardized mean hazard (per Δt)", x = NULL) +
            ggplot2::theme_minimal()
    } else {
        p <- NULL
    }

    results <- list(
        std_means = std_means,
        interaction_test = interaction_test,
        wald_interaction = wald_interaction,
        within_condition_changes = delta,
        within_condition_CI = within_ci,
        difference_in_differences = DiD,
        bootstrap_means_CI = means_CI,
        bootstrap_DiD_CI = DiD_boot,
        plot = p
    )

    # Determine final path for saving
    final_path <- analysis_results_path
    if (is.null(final_path)) {
        # NULL -> use default path
        ensure_global_data_initialized()
        final_path <- risk_analysis_results_path
    } else if (identical(final_path, FALSE)) {
        # FALSE -> set to NULL (skip saving)
        final_path <- NULL
    }

    # Save files only if we have a valid path
    if (!is.null(final_path)) {
        # Save analysis to text file using the same path but with .txt extension
        txt_path <- gsub("\\.rds$", ".txt", final_path)
        save_analysis_to_txt(results, std_means, txt_path)

        # Save analysis results to RDS file
        analysis_results_to_save <- list(
            standardized_risks = std_means,
            analysis_results = results
        )
        saveRDS(analysis_results_to_save, final_path)
        analysis_logger("INFO", "Analysis results saved to:", final_path)

        # Set up global variables with the analysis results
        setup_global_risk_variables(model_path = FALSE, hazard_samples_path = FALSE, analysis_results_path = final_path)
    } else {
        analysis_logger("DEBUG", "Skipped saving analysis results (analysis_results_path = FALSE)")
    }

    print_analysis_summary(results)

    analysis_logger("INFO", "Analysis complete!")
    return(results)
}


# Helper function to save analysis to text file
#' @param results Analysis results from perform_risk_analysis
#' @param std_means Standardized means data frame
#' @param txt_path Path to save the text file
save_analysis_to_txt <- function(results, std_means, txt_path) {
    # Check if file already exists and create unique filename if needed
    if (file.exists(txt_path)) {
        # Extract directory, base name, and extension
        dir_path <- dirname(txt_path)
        base_name <- tools::file_path_sans_ext(basename(txt_path))
        ext <- tools::file_ext(txt_path)

        # Create timestamp for unique filename
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        new_filename <- paste0(base_name, "_", timestamp, ".", ext)
        txt_path <- file.path(dir_path, new_filename)

        analysis_logger("WARN", "File already exists, saving to:", txt_path)
    }

    sink(txt_path)

    # Executive summary at the very top
    render_exec_summary(results, stdout())

    cat("=== POOLED STANDARDIZED RISKS ===\n\n")
    cat("Standardized risk estimates by condition and phase:\n")
    print(std_means)
    cat("\n")

    cat("=== RISK MODEL STATISTICAL ANALYSIS ===\n\n")

    cat("1. CONDITION × PHASE INTERACTION TEST\n")
    cat("=====================================\n")
    if (!is.null(results$interaction_test)) {
        cat("Likelihood Ratio Test (ML):\n")
        print(results$interaction_test)
    } else if (!is.null(results$wald_interaction)) {
        cat("Wald Test (LRT failed - ML refit unsuccessful):\n")
        print(results$wald_interaction)
    } else {
        cat("No interaction test available\n")
    }
    cat("\n")

    cat("2. WITHIN-CONDITION CHANGES\n")
    cat("===========================\n")
    print(results$within_condition_changes)
    cat("\n")

    cat("2b. WITHIN-CONDITION CHANGES WITH BOOTSTRAP CIs AND P-VALUES\n")
    cat("============================================================\n")
    if (!is.null(results$within_condition_CI) && nrow(results$within_condition_CI) > 0) {
        print(results$within_condition_CI)
    } else {
        cat("No within-condition CIs available (skipped or failed).\n")
    }
    cat("\n")

    cat("3. DIFFERENCE-IN-DIFFERENCES\n")
    cat("============================\n")
    print(results$difference_in_differences)
    cat("\n")

    cat("4. BOOTSTRAP CONFIDENCE INTERVALS - STANDARDIZED MEANS\n")
    cat("======================================================\n")
    if (!is.null(results$bootstrap_means_CI) && nrow(results$bootstrap_means_CI) > 0) {
        print(results$bootstrap_means_CI)
    } else {
        cat("No bootstrap CIs available (skipped or failed).\n")
    }
    cat("\n")

    cat("5. BOOTSTRAP CONFIDENCE INTERVALS - DIFFERENCE-IN-DIFFERENCES\n")
    cat("=============================================================\n")
    if (!is.null(results$bootstrap_DiD_CI) && nrow(results$bootstrap_DiD_CI) > 0) {
        print(results$bootstrap_DiD_CI)
    } else {
        cat("No bootstrap DiD CIs available (skipped or failed).\n")
    }
    cat("\n")

    sink()
    message(sprintf("[ANALYSIS] Results saved to: %s", txt_path))
}

#' Render executive summary tables
#'
#' @param results List containing analysis results
#' @param file_con Connection to write to (e.g., from sink())
#' @export
render_exec_summary <- function(results, file_con) {
    # Helper formatters
    fmt_num <- function(x) sprintf("%.6f", x)
    fmt_ci <- function(lo, hi) sprintf("[%.6f, %.6f]", lo, hi)
    fmt_p <- function(p) {
        if (is.na(p)) {
            return("NA")
        }
        if (p < 0.001) {
            return("<0.001")
        }
        sprintf("%.3f", p)
    }

    # Determine metric label
    metric_label <- if (!is.null(results$std_means) && "risk_1s" %in% names(results$std_means)) {
        "standardized 1-second risk (population-level; RE excluded)"
    } else {
        "per-bin hazard (cloglog; population-level; RE excluded)"
    }

    cat("=== EXECUTIVE SUMMARY ===\n\n", file = file_con)
    cat(sprintf("Metric: %s\n\n", metric_label), file = file_con)

    # A) Within-condition learning deltas
    cat("A) Within-condition learning (Δ vs baseline)\n", file = file_con)
    if (!is.null(results$within_condition_CI) && nrow(results$within_condition_CI) > 0) {
        within_df <- results$within_condition_CI

        # Standardize ordering: conditions first, then deltas
        condition_order <- c("control", "perturbation", "perturbation_visualization")
        condition_order <- condition_order[condition_order %in% within_df$condition]
        if (length(condition_order) == 0) condition_order <- unique(within_df$condition)

        delta_order <- c("d_retention", "d_transfer")
        delta_order <- delta_order[delta_order %in% within_df$delta]
        if (length(delta_order) == 0) delta_order <- unique(within_df$delta)

        # Pretty delta labels
        delta_labels <- c(
            "d_retention" = "Retention − Baseline",
            "d_transfer" = "Transfer − Baseline"
        )

        # Create ordered table
        within_ordered <- within_df[order(
            match(within_df$condition, condition_order),
            match(within_df$delta, delta_order)
        ), ]

        # Print table
        cat("Condition           | Delta              | Effect    | 95% CI              | p_boot  | p_Holm\n", file = file_con)
        cat("--------------------|--------------------|-----------|---------------------|---------|--------\n", file = file_con)

        for (i in seq_len(nrow(within_ordered))) {
            row <- within_ordered[i, ]
            cond <- row$condition
            delta <- delta_labels[row$delta]
            effect <- fmt_num(row$point)
            ci <- fmt_ci(row$lo, row$hi)
            p_boot <- fmt_p(row$p_boot)
            p_holm <- fmt_p(row$p_boot_holm)

            cat(sprintf(
                "%-19s | %-18s | %-9s | %-19s | %-7s | %s\n",
                cond, delta, effect, ci, p_boot, p_holm
            ), file = file_con)
        }
    } else {
        cat("(not available)\n", file = file_con)
    }

    cat("\n", file = file_con)

    # B) Difference-in-Differences
    cat("B) Difference-in-Differences\n", file = file_con)
    if (!is.null(results$bootstrap_DiD_CI) && nrow(results$bootstrap_DiD_CI) > 0) {
        did_df <- results$bootstrap_DiD_CI

        # Standardize ordering: exact six rows if present
        contrast_order <- c(
            "PV vs Control (Retention)",
            "PV vs Control (Transfer)",
            "P vs Control (Retention)",
            "P vs Control (Transfer)",
            "P vs PV (Retention)",
            "P vs PV (Transfer)"
        )

        # Filter to only existing contrasts
        did_ordered <- did_df[did_df$contrast %in% contrast_order, ]
        did_ordered <- did_ordered[order(match(did_ordered$contrast, contrast_order)), ]

        if (nrow(did_ordered) > 0) {
            # Print table
            cat("Contrast                    | Effect    | 95% CI              | p_boot  | p_Holm\n", file = file_con)
            cat("----------------------------|-----------|---------------------|---------|--------\n", file = file_con)

            for (i in seq_len(nrow(did_ordered))) {
                row <- did_ordered[i, ]
                contrast <- row$contrast
                effect <- fmt_num(row$point)
                ci <- fmt_ci(row$lo, row$hi)
                p_boot <- fmt_p(row$p_boot)
                p_holm <- fmt_p(row$p_boot_holm)

                cat(sprintf(
                    "%-27s | %-9s | %-19s | %-7s | %s\n",
                    contrast, effect, ci, p_boot, p_holm
                ), file = file_con)
            }
        } else {
            cat("(not available)\n", file = file_con)
        }
    } else {
        cat("(not available)\n", file = file_con)
    }

    cat("\n----------------------------------------\n\n", file = file_con)
}

# Helper function to print analysis summary to console
print_analysis_summary <- function(results) {
    cat("\n=== ANALYSIS SUMMARY ===\n")

    # Concise executive summary for console
    cat("\n--- EXECUTIVE SUMMARY ---\n")

    # Determine metric label
    metric_label <- if (!is.null(results$std_means) && "risk_1s" %in% names(results$std_means)) {
        "standardized 1-second risk (population-level; RE excluded)"
    } else {
        "per-bin hazard (cloglog; population-level; RE excluded)"
    }
    cat(sprintf("Metric: %s\n\n", metric_label))

    # A) Within-condition learning deltas (concise)
    cat("A) Within-condition learning (Δ vs baseline)\n")
    if (!is.null(results$within_condition_CI) && nrow(results$within_condition_CI) > 0) {
        within_df <- results$within_condition_CI

        # Standardize ordering
        condition_order <- c("control", "perturbation", "perturbation_visualization")
        condition_order <- condition_order[condition_order %in% within_df$condition]
        if (length(condition_order) == 0) condition_order <- unique(within_df$condition)

        delta_order <- c("d_retention", "d_transfer")
        delta_order <- delta_order[delta_order %in% within_df$delta]
        if (length(delta_order) == 0) delta_order <- unique(within_df$delta)

        # Pretty delta labels
        delta_labels <- c(
            "d_retention" = "Retention − Baseline",
            "d_transfer" = "Transfer − Baseline"
        )

        # Create ordered table
        within_ordered <- within_df[order(
            match(within_df$condition, condition_order),
            match(within_df$delta, delta_order)
        ), ]

        # Print concise table
        cat("Condition           | Delta              | Effect    | 95% CI              | p_boot  | p_Holm\n")
        cat("--------------------|--------------------|-----------|---------------------|---------|--------\n")

        for (i in seq_len(nrow(within_ordered))) {
            row <- within_ordered[i, ]
            cond <- row$condition
            delta <- delta_labels[row$delta]
            effect <- sprintf("%.6f", row$point)
            ci <- sprintf("[%.6f, %.6f]", row$lo, row$hi)
            p_boot <- if (is.na(row$p_boot)) "NA" else if (row$p_boot < 0.001) "<0.001" else sprintf("%.3f", row$p_boot)
            p_holm <- if (is.na(row$p_boot_holm)) "NA" else if (row$p_boot_holm < 0.001) "<0.001" else sprintf("%.3f", row$p_boot_holm)

            cat(sprintf(
                "%-19s | %-18s | %-9s | %-19s | %-7s | %s\n",
                cond, delta, effect, ci, p_boot, p_holm
            ))
        }
    } else {
        cat("(not available)\n")
    }

    cat("\n")

    # B) Difference-in-Differences (concise)
    cat("B) Difference-in-Differences\n")
    if (!is.null(results$bootstrap_DiD_CI) && nrow(results$bootstrap_DiD_CI) > 0) {
        did_df <- results$bootstrap_DiD_CI

        # Standardize ordering
        contrast_order <- c(
            "PV vs Control (Retention)",
            "PV vs Control (Transfer)",
            "P vs Control (Retention)",
            "P vs Control (Transfer)",
            "P vs PV (Retention)",
            "P vs PV (Transfer)"
        )

        # Filter to only existing contrasts
        did_ordered <- did_df[did_df$contrast %in% contrast_order, ]
        did_ordered <- did_ordered[order(match(did_ordered$contrast, contrast_order)), ]

        if (nrow(did_ordered) > 0) {
            # Print concise table
            cat("Contrast                    | Effect    | 95% CI              | p_boot  | p_Holm\n")
            cat("----------------------------|-----------|---------------------|---------|--------\n")

            for (i in seq_len(nrow(did_ordered))) {
                row <- did_ordered[i, ]
                contrast <- row$contrast
                effect <- sprintf("%.6f", row$point)
                ci <- sprintf("[%.6f, %.6f]", row$lo, row$hi)
                p_boot <- if (is.na(row$p_boot)) "NA" else if (row$p_boot < 0.001) "<0.001" else sprintf("%.3f", row$p_boot)
                p_holm <- if (is.na(row$p_boot_holm)) "NA" else if (row$p_boot_holm < 0.001) "<0.001" else sprintf("%.3f", row$p_boot_holm)

                cat(sprintf(
                    "%-27s | %-9s | %-19s | %-7s | %s\n",
                    contrast, effect, ci, p_boot, p_holm
                ))
            }
        } else {
            cat("(not available)\n")
        }
    } else {
        cat("(not available)\n")
    }

    cat("\n--- DETAILED ANALYSIS ---\n")

    # Interaction test
    if (!is.null(results$interaction_test)) {
        p_val <- results$interaction_test$`Pr(>Chi)`[2]
        cat(sprintf("Condition × Phase (LRT-ML): p = %.4f\n", p_val))
    } else if (!is.null(results$wald_interaction)) {
        wi <- results$wald_interaction
        cat(sprintf("Condition × Phase (Wald):  χ²=%.2f, df=%d, p=%.4f\n", wi$statistic, wi$df, wi$p.value))
    } else {
        cat("Condition × Phase: test unavailable\n")
    }

    # Key DiD results
    cat("\nKey Difference-in-Differences:\n")
    if (!is.null(results$bootstrap_DiD_CI) && nrow(results$bootstrap_DiD_CI) > 0) {
        for (i in 1:nrow(results$bootstrap_DiD_CI)) {
            contrast <- results$bootstrap_DiD_CI$contrast[i]
            point <- results$bootstrap_DiD_CI$point[i]
            lo <- results$bootstrap_DiD_CI$lo[i]
            hi <- results$bootstrap_DiD_CI$hi[i]
            p_boot <- results$bootstrap_DiD_CI$p_boot[i]
            p_holm <- results$bootstrap_DiD_CI$p_boot_holm[i]
            cat(sprintf(
                "  %s: %.4f [95%% CI: %.4f, %.4f], p=%.4f (Holm: %.4f)\n",
                contrast, point, lo, hi, p_boot, p_holm
            ))
        }
    } else {
        cat("No bootstrap DiD CIs available (skipped or failed).\n")
    }

    # Within-condition changes with p-values
    cat("\nWithin-Condition Changes:\n")
    if (!is.null(results$within_condition_CI) && nrow(results$within_condition_CI) > 0) {
        for (i in 1:nrow(results$within_condition_CI)) {
            condition <- results$within_condition_CI$condition[i]
            delta <- results$within_condition_CI$delta[i]
            point <- results$within_condition_CI$point[i]
            lo <- results$within_condition_CI$lo[i]
            hi <- results$within_condition_CI$hi[i]
            p_boot <- results$within_condition_CI$p_boot[i]
            p_holm <- results$within_condition_CI$p_boot_holm[i]
            cat(sprintf(
                "  %s %s: %.4f [95%% CI: %.4f, %.4f], p=%.4f (Holm: %.4f)\n",
                condition, delta, point, lo, hi, p_boot, p_holm
            ))
        }
    } else {
        cat("No within-condition CIs available (skipped or failed).\n")
    }

    cat("\n")
}
