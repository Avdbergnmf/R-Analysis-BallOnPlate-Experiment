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

    # --- features (compute winsorization limits once) ---
    q_mag <- abs(sim_data$q)
    q_dir <- ifelse(q_mag > 0, sim_data$q / q_mag, 0)
    v_raw <- sim_data$qd * q_dir
    qlo <- stats::quantile(v_raw, 0.01, na.rm = TRUE)
    qhi <- stats::quantile(v_raw, 0.99, na.rm = TRUE)

    # Columns to keep
    base_cols <- c(
        "time", "e", "v", "q", "qd", "q_max", "dist_to_escape_ratio",
        "respawn_segment", "condition", "taskNum", "phase"
    )

    # Build feature frame
    haz <- transform(
        sim_data,
        e = pmax(0, pmin(1, dist_to_escape_ratio)),
        v = pmin(pmax(v_raw, qlo), qhi)
    )

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
    cat(sprintf(
        "[hazard] tau=%.3fs | rows_in=%d | rows_out=%d | dt~%.3fs | positives=%d (%.4f%%)\n",
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
#' @return Fitted GLM/GLMER model object
#' @export
train_risk_model <- function(hazard_samples) {
    if (is.null(hazard_samples) || nrow(hazard_samples) == 0) {
        return(NULL)
    }

    # Debug: Check data structure
    cat(sprintf("Data structure check:\n"))
    cat(sprintf("  - Rows: %d\n", nrow(hazard_samples)))
    cat(sprintf("  - Columns: %s\n", paste(names(hazard_samples), collapse = ", ")))
    cat(sprintf("  - Participant column type: %s\n", class(hazard_samples$participant)))
    cat(sprintf("  - Unique participants: %d\n", length(unique(hazard_samples$participant))))
    cat(sprintf("  - Participant values: %s\n", paste(head(unique(hazard_samples$participant)), collapse = ", ")))
    cat(sprintf("  - Drop_within_tau range: %.0f to %.0f\n", min(hazard_samples$drop_within_tau, na.rm = TRUE), max(hazard_samples$drop_within_tau, na.rm = TRUE)))
    cat(sprintf("  - E range: %.3f to %.3f\n", min(hazard_samples$e, na.rm = TRUE), max(hazard_samples$e, na.rm = TRUE)))
    cat(sprintf("  - V range: %.3f to %.3f\n", min(hazard_samples$v, na.rm = TRUE), max(hazard_samples$v, na.rm = TRUE)))

    # Ensure participant is a factor for random effects
    if (!is.factor(hazard_samples$participant)) {
        cat("Converting participant to factor...\n")
        hazard_samples$participant <- as.factor(hazard_samples$participant)
    }

    # Remove any rows with missing values
    complete_rows <- complete.cases(hazard_samples[, c("e", "v", "participant", "drop_within_tau")])
    if (sum(!complete_rows) > 0) {
        cat(sprintf("Removing %d rows with missing values...\n", sum(!complete_rows)))
        hazard_samples <- hazard_samples[complete_rows, ]
    }

    # Mixed-effects model with participant random effects using mgcv::bam (multithreaded)
    cat("Fitting mixed-effects GAM model with mgcv::bam (multithreaded)...\n")
    # n_cores <- parallel::detectCores()
    # cat(sprintf("Using %d CPU cores for parallel processing\n", n_cores))

    tryCatch(
        {
            model <- bam(
                drop_within_tau ~ s(e, k = 5) + s(v, k = 5) + condition * phase + s(participant, bs = "re"),
                family = binomial(link = "cloglog"),
                data = hazard_samples,
                method = "fREML",
                discrete = TRUE, # big speed win
                nthreads = parallel::detectCores() # use all cores
            )
            cat("Model fitting completed successfully!\n")
            cat(sprintf("Model class: %s\n", class(model)[1]))
            return(model)
        },
        error = function(e) {
            cat(sprintf("ERROR in model fitting: %s\n", e$message))
            return(NULL)
        }
    )
}

#' Score trial with risk model
#'
#' @param sim_data Simulation data from get_simulation_data()
#' @param model Fitted model from train_risk_model()
#' @param tau Time horizon for drop prediction (default 0.2 seconds)
#' @param use_saved Whether to try using saved predictions first (default TRUE)
#' @param preds_path Path to saved predictions file (default uses global path)
#' @param prefer Preferred prediction type: "p_hat_fe" (fixed-effects) or "p_hat_re" (subject-specific)
#' @return Data frame with one row containing risk metrics
#' @export
score_trial_with_model <- function(sim_data, model, tau = 0.2,
                                   use_saved = TRUE,
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
    if (use_saved && file.exists(preds_path)) {
        tryCatch(
            {
                haz_all <- readRDS(preds_path)
                cat(sprintf("[SCORE] Loaded saved predictions from: %s\n", preds_path))
            },
            error = function(e) {
                cat(sprintf("[SCORE] Failed to load saved predictions: %s\n", e$message))
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
        if ("participant" %in% names(haz_all)) sel <- sel & (haz_all$participant == pid)
        if ("trial" %in% names(haz_all)) sel <- sel & (haz_all$trial == trial)

        haz_trial <- haz_all[sel, , drop = FALSE]
        if (nrow(haz_trial) > 0) {
            # Pick preferred prediction column
            pred_col <- prefer[prefer %in% names(haz_trial)][1]
            if (is.na(pred_col)) {
                warning("No predicted columns found in saved file; computing on the fly.")
            } else {
                cat(sprintf(
                    "[SCORE] Using saved predictions (%s) for participant %s, trial %s\n",
                    pred_col, pid, trial
                ))
                return(list(
                    individual_predictions = haz_trial[[pred_col]],
                    hazard_samples = haz_trial
                ))
            }
        }
    }

    # Fallback: compute predictions on the fly for this trial only
    cat(sprintf("[SCORE] Computing predictions on-the-fly for participant %s, trial %s\n", pid, trial))

    hazard_samples <- build_hazard_samples(sim_data, tau)
    if (nrow(hazard_samples) == 0) {
        cat("[DEBUG] score_trial_with_model: returning empty list\n")
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    # Minimal factor alignment
    hazard_samples$participant <- factor(hazard_samples$participant <- pid)
    if ("condition" %in% names(model$model) && "condition" %in% names(hazard_samples)) {
        hazard_samples$condition <- factor(hazard_samples$condition, levels = levels(model$model$condition))
    }
    if ("phase" %in% names(model$model) && "phase" %in% names(hazard_samples)) {
        hazard_samples$phase <- factor(hazard_samples$phase, levels = levels(model$model$phase))
    }

    # Predict probabilities for each sample
    # Handle different model types
    if (inherits(model, "bam") || inherits(model, "gam")) {
        # For mgcv::bam/gam, use exclude to predict without random effects
        p_t <- predict(model, newdata = hazard_samples, type = "response", exclude = "s(participant)")
    } else if (inherits(model, "glmmTMB")) {
        # For glmmTMB, use re.form = NA to predict without random effects
        p_t <- predict(model, newdata = hazard_samples, type = "response", re.form = NA)
    } else {
        # Fallback for other model types
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
#' @export
save_risk_model <- function(model, file_path) {
    saveRDS(model, file_path)
}

#' Load risk model from RDS file
#'
#' @param file_path Path to the RDS file
#' @return Fitted model object
#' @export
load_risk_model <- function(file_path) {
    if (file.exists(file_path)) {
        return(readRDS(file_path))
    } else {
        return(NULL)
    }
}


#' Train and save risk model using all available trials
#'
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param tau Time horizon for drop prediction (default 0.2 seconds)
#' @param file_path Path to save the RDS file (default uses global risk_model_path)
#' @return Fitted model object
#' @export
train_and_save_risk_model <- function(participants, trials, tau = 0.2,
                                      sampling_freq = 90, file_path = NULL) {
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

        # Collect hazard samples from all trials
        all_hazard_samples <- data.frame()

        # Create all combinations efficiently
        combinations_df <- expand.grid(participant = participants, trial = trials, stringsAsFactors = FALSE)
        combinations_df <- unique(combinations_df) # Remove any duplicates
        total_combinations <- nrow(combinations_df)

        cat(sprintf("Processing %d unique participant-trial combinations...\n", total_combinations))

        # Process each combination
        for (i in 1:nrow(combinations_df)) {
            participant <- combinations_df$participant[i]
            trial <- combinations_df$trial[i]
            cat(sprintf("[DEBUG] Processing participant %s, trial %s (combination %d/%d)\n", participant, trial, i, total_combinations))
            if (i %% 10 == 0 || i == total_combinations) {
                cat(sprintf("Progress: %d/%d combinations processed\n", i, total_combinations))
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
                            all_hazard_samples <- rbind(all_hazard_samples, hazard_samples)
                        }
                    }
                },
                error = function(e) {
                    cat(sprintf(
                        "Error processing participant %s, trial %s: %s\n",
                        participant, trial, e$message
                    ))
                }
            )
        }

        if (nrow(all_hazard_samples) == 0) {
            cat("No hazard samples found. Cannot train risk model.\n")
            return(NULL)
        }

        cat(sprintf(
            "Training risk model with %d hazard samples from %d participants, %d trials\n",
            nrow(all_hazard_samples), length(unique(all_hazard_samples$participant)), length(unique(all_hazard_samples$trial))
        ))

        # Show data summary
        cat(sprintf("Data summary:\n"))
        cat(sprintf("  - Total samples: %d\n", nrow(all_hazard_samples)))
        cat(sprintf("  - Participants: %d\n", length(unique(all_hazard_samples$participant))))
        cat(sprintf("  - Trials: %d\n", length(unique(all_hazard_samples$trial))))
        cat(sprintf(
            "  - Positive labels (drops): %d (%.1f%%)\n",
            sum(all_hazard_samples$drop_within_tau),
            100 * mean(all_hazard_samples$drop_within_tau)
        ))

        # Train risk model
        cat("Step 2/3: Fitting mixed-effects GLM model...\n")
        model <- train_risk_model(all_hazard_samples)

        if (is.null(model)) {
            cat("Failed to train risk model\n")
            return(NULL)
        }

        # Save risk model
        cat("Step 3/3: Saving model and hazard samples to disk...\n")
        save_risk_model(model, file_path)

        # Save hazard samples alongside the model
        ensure_global_data_initialized()
        saveRDS(all_hazard_samples, risk_hazard_samples_path)

        # Verify the files were created
        if (file.exists(file_path)) {
            file_size <- file.info(file_path)$size
            cat(sprintf("Risk model saved successfully to %s (%.2f MB)\n", file_path, file_size / 1024^2))
        } else {
            cat(sprintf("ERROR: Model file was not created at %s\n", file_path))
        }

        if (file.exists(risk_hazard_samples_path)) {
            file_size <- file.info(risk_hazard_samples_path)$size
            cat(sprintf("Hazard samples saved successfully to %s (%.2f MB)\n", risk_hazard_samples_path, file_size / 1024^2))
        } else {
            cat(sprintf("ERROR: Hazard samples file was not created at %s\n", risk_hazard_samples_path))
        }

        # ── NEW: Add per-sample predictions to saved hazard samples ──
        cat("Step 4/5: Adding per-sample predictions to hazard samples...\n")
        ensure_global_data_initialized()

        # Add fixed-effects predictions (recommended for comparability across participants)
        annotate_hazard_predictions(model, all_hazard_samples,
            include_re = FALSE,
            out_path = risk_hazard_samples_preds_path
        )

        # (Optionally) also add subject-specific predictions in a separate file
        # annotate_hazard_predictions(model, all_hazard_samples, include_re = TRUE,
        #                             out_path = risk_hazard_samples_preds_re_path)

        # ── NEW: pooled standardized predictions by Condition × Phase ──
        cat("Step 5/5: Computing pooled standardized risks by Condition × Phase...\n")
        std_means <- compute_pooled_standardized_risks(
            model,
            all_hazard_samples,
            tau = tau,
            save_csv_path = risk_standardized_path
        )

        # Return both model and standardized means
        return(list(model = model, standardized_means = std_means))
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
        cat(sprintf("No risk model found at %s\n", file_path))
        return(data.frame())
    }

    results <- data.frame()

    for (participant in participants) {
        for (trial in trials) {
            tryCatch(
                {
                    sim_data <- get_simulation_data(participant, trial, enable_risk = FALSE)
                    if (!is.null(sim_data) && nrow(sim_data) > 0) {
                        risk_scores <- score_trial_with_model(sim_data, model, tau)
                        risk_scores$participant <- participant
                        risk_scores$trial <- trial
                        results <- rbind(results, risk_scores)
                    }
                },
                error = function(e) {
                    cat(sprintf(
                        "Error scoring participant %s, trial %s: %s\n",
                        participant, trial, e$message
                    ))
                }
            )
        }
    }

    return(results)
}

# ─────────────────────────────────────────────────────────────────────────────
# Per-sample prediction annotation for saved hazard samples
# ─────────────────────────────────────────────────────────────────────────────
annotate_hazard_predictions <- function(model,
                                        hazard_samples,
                                        include_re = FALSE,
                                        out_path = NULL,
                                        chunk_by = c("participant")) {
    # Use global path if not specified
    if (is.null(out_path)) {
        ensure_global_data_initialized()
        if (include_re) {
            out_path <- risk_hazard_samples_preds_re_path
        } else {
            out_path <- risk_hazard_samples_preds_path
        }
    }

    cat(sprintf(
        "[PREDICT] Annotating hazard samples with %s predictions...\n",
        if (include_re) "subject-specific" else "fixed-effects"
    ))

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
    cat(sprintf("[PREDICT] Processing %d participants in chunks...\n", length(split_idx)))

    for (i in seq_along(split_idx)) {
        ix <- split_idx[[i]]
        newdf <- hazard_samples[ix, , drop = FALSE]

        tryCatch(
            {
                pred_result <- do.call(predict, c(list(object = model, newdata = newdf), pred_args))
                p_hat[ix] <- pred_result
            },
            error = function(e) {
                cat(sprintf("[PREDICT] ERROR in chunk %d: %s\n", i, e$message))
                p_hat[ix] <- NA_real_
            }
        )

        if (i %% 10 == 0 || i == length(split_idx)) {
            cat(sprintf(
                "[PREDICT] Progress: %d/%d participants (%.1f%%)\n",
                i, length(split_idx), 100 * i / length(split_idx)
            ))
        }
    }

    # Attach prediction column (name indicates FE/RE)
    colname <- if (include_re) "p_hat_re" else "p_hat_fe"
    hazard_samples[[colname]] <- p_hat


    # Save and return
    saveRDS(hazard_samples, out_path)
    message(sprintf("[PREDICT] Saved hazard samples with predictions to: %s", out_path))
    invisible(hazard_samples)
}

# ─────────────────────────────────────────────────────────────────────────────
# Pooled, state-conditional standardized mean risks by Condition × Phase
# ─────────────────────────────────────────────────────────────────────────────
# ─────────────────────────────────────────────────────────────────────────────
# Pooled, state-conditional standardized mean risks by Condition × Phase
# ─────────────────────────────────────────────────────────────────────────────
compute_pooled_standardized_risks <- function(model, hazard_samples, tau = 0,
                                              save_csv_path = NULL) {
    cat("[DEBUG] compute_pooled_standardized_risks called with tau =", tau, "\n")

    # sanity
    need <- c("e", "v", "participant", "condition", "phase")
    miss <- setdiff(need, names(hazard_samples))
    if (length(miss) > 0) stop("hazard_samples missing: ", paste(miss, collapse = ", "))

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

    # estimate bin width Δt from within-segment time differences (if available)
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

    for (cn in cond_levels) {
        for (ph in phase_levels) {
            newdf <- ref_df
            newdf$condition <- factor(cn, levels = cond_levels)
            newdf$phase <- factor(ph, levels = phase_levels)

            # predict fixed-effects hazard; exclude random effect smooth
            eta <- predict(model, newdata = newdf, type = "link", exclude = "s(participant)")
            # per-bin prob (for backward compat with 'mean_hazard'):
            p <- 1 - exp(-exp(eta))
            mean_h <- mean(tapply(p, newdf$participant, mean, na.rm = TRUE) |> unlist(), na.rm = TRUE)
            # sampling-invariant rate and 1-second risk:
            if (is.finite(dt) && dt > 0) {
                lambda <- exp(eta) / dt
                risk1s <- 1 - exp(-lambda)
                exp_dpm <- 60 * mean(tapply(lambda, newdf$participant, mean, na.rm = TRUE) |> unlist(), na.rm = TRUE)
                risk_1s <- mean(tapply(risk1s, newdf$participant, mean, na.rm = TRUE) |> unlist(), na.rm = TRUE)
            } else {
                exp_dpm <- NA_real_
                risk_1s <- NA_real_
            }

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

    if (!is.null(save_csv_path)) {
        utils::write.csv(out, save_csv_path, row.names = FALSE)
        message(sprintf("[STANDARDIZED] Saved pooled standardized risks to: %s", save_csv_path))
    }

    # Perform comprehensive statistical analysis
    analysis_results <- perform_risk_analysis(model, hazard_samples, out, save_csv_path)

    # console summary (only when not called from UI)
    if (is.null(save_csv_path)) {
        cat("[STANDARDIZED] Pooled standardized risks computed:\n")
        print(out)
    }

    return(list(
        standardized_risks = out,
        analysis_results   = analysis_results
    ))
}


# ─────────────────────────────────────────────────────────────────────────────
# Comprehensive Risk Analysis: Statistical Tests, DiD, Bootstrap CIs
# ─────────────────────────────────────────────────────────────────────────────
perform_risk_analysis <- function(model, hazard_samples, std_means, save_csv_path = NULL) {
    cat("[ANALYSIS] Performing comprehensive risk analysis...\n")

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
    cat("[ANALYSIS] cond_levels =", cond_levels, "\n")
    cat("[ANALYSIS] phase_levels =", phase_levels, "\n")

    # 1) Condition × Phase interaction (hazard model) - try ML LRT, else Wald
    cat("[ANALYSIS] Testing condition × phase interaction...\n")
    m_full_ml <- tryCatch(update(model, method = "ML"), error = function(e) NULL)
    m_noint_ml <- tryCatch(update(model, . ~ . - condition:phase, method = "ML"), error = function(e) NULL)

    interaction_test <- if (!is.null(m_full_ml) && !is.null(m_noint_ml)) {
        cat("[ANALYSIS] Using LRT (ML) for interaction test...\n")
        anova(m_noint_ml, m_full_ml, test = "Chisq")
    } else {
        warning("[ANALYSIS] Could not refit models with ML for LRT. Will use Wald test.")
        NULL
    }

    # Fallback: Wald test
    wald_interaction <- NULL
    if (is.null(interaction_test)) {
        cat("[ANALYSIS] Using Wald test for interaction...\n")
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
    cat("[ANALYSIS] Computing within-condition changes and DiD...\n")
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
            "P  vs Control (Retention)", "P  vs Control (Transfer)"
        ),
        DiD = c(
            (get_mh("perturbation_visualization", "retention") - get_mh("perturbation_visualization", "baseline_task")) -
                (get_mh("control", "retention") - get_mh("control", "baseline_task")),
            (get_mh("perturbation_visualization", "transfer") - get_mh("perturbation_visualization", "baseline_task")) -
                (get_mh("control", "transfer") - get_mh("control", "baseline_task")),
            (get_mh("perturbation", "retention") - get_mh("perturbation", "baseline_task")) -
                (get_mh("control", "retention") - get_mh("control", "baseline_task")),
            (get_mh("perturbation", "transfer") - get_mh("perturbation", "baseline_task")) -
                (get_mh("control", "transfer") - get_mh("control", "baseline_task"))
        )
    )

    # 3) Parametric bootstrap CIs (guarded)
    cat("[ANALYSIS] Computing parametric bootstrap CIs...\n")
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

        make_eta <- function(beta_vec, cond, ph) {
            newdf <- ref_df
            newdf$condition <- factor(cond, levels = cond_levels_b)
            newdf$phase <- factor(ph, levels = phase_levels_b)
            X <- predict(model, newdata = newdf, type = "lpmatrix", exclude = "s(participant)")
            as.vector(X %*% beta_vec)
        }
        std_mean_for <- function(beta_vec, cond, ph) {
            eta <- make_eta(beta_vec, cond, ph)
            p <- 1 - exp(-exp(eta)) # inverse cloglog
            by_pid <- tapply(p, ref_df$participant, mean, na.rm = TRUE)
            mean(unlist(by_pid), na.rm = TRUE)
        }

        beta_hat <- coef(model)
        V <- model$Vp

        if (is.null(V) || any(!is.finite(V))) {
            warning("[ANALYSIS] No valid covariance matrix; skipping bootstrap.")
        } else {
            B <- 100L # increase to 1000 for final runs
            cat(sprintf("[ANALYSIS] Starting parametric bootstrap with %d iterations...\n", B))
            flush.console()

            get_all_means <- function(beta_vec) {
                grid <- expand.grid(
                    condition = cond_levels_b,
                    phase = phase_levels_b,
                    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
                )
                grid$mean_hazard <- mapply(
                    function(cn, ph) std_mean_for(beta_vec, cn, ph),
                    grid$condition, grid$phase
                )
                grid
            }

            set.seed(123)
            beta_draw <- function() {
                out <- try(mvtnorm::rmvnorm(1, mean = beta_hat, sigma = V), silent = TRUE)
                if (inherits(out, "try-error")) beta_hat else as.numeric(out)
            }

            boot_list <- vector("list", B)
            cat(sprintf("[ANALYSIS] Running %d bootstrap iterations...\n", B))
            flush.console()

            for (b in seq_len(B)) {
                if (b %% 10 == 0 || b == B) {
                    cat(sprintf("[ANALYSIS] Bootstrap progress: %d/%d (%.1f%%)\n", b, B, 100 * b / B))
                    flush.console()
                }
                boot_list[[b]] <- get_all_means(beta_draw())
            }

            cat("[ANALYSIS] Bootstrap completed, computing confidence intervals...\n")
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
                    c("perturbation", "control", "transfer")
                )
                pts <- apply(did_names, 1, \(r) did_from_df(std_means, r[1], r[2], r[3]))

                cat("[ANALYSIS] Computing DiD bootstrap samples...\n")
                flush.console()
                did_boot_mat <- sapply(seq_len(B), function(b) {
                    if (b %% 20 == 0 || b == B) {
                        cat(sprintf("[ANALYSIS] DiD bootstrap progress: %d/%d (%.1f%%)\n", b, B, 100 * b / B))
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
                        "P vs Control (Retention)",  "P vs Control (Transfer)"
                    ),
                    point = as.numeric(pts),
                    lo = apply(did_boot_mat, 1, \(x) stats::quantile(x, .025, na.rm = TRUE)),
                    med = apply(did_boot_mat, 1, \(x) stats::quantile(x, .5, na.rm = TRUE)),
                    hi = apply(did_boot_mat, 1, \(x) stats::quantile(x, .975, na.rm = TRUE)),
                    p_boot = as.numeric(did_pvals),
                    p_boot_holm = as.numeric(did_pvals_holm)
                )

                # Within-condition deltas: CIs + p-values
                cat("[ANALYSIS] Computing bootstrap p-values for within-condition changes...\n")
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

    # 4) Optional plot (only if means_CI exists)
    if (!is.null(means_CI)) {
        cat("[ANALYSIS] Creating visualization...\n")
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
        interaction_test = interaction_test,
        wald_interaction = wald_interaction,
        within_condition_changes = delta,
        within_condition_CI = within_ci,
        difference_in_differences = DiD,
        bootstrap_means_CI = means_CI,
        bootstrap_DiD_CI = DiD_boot,
        plot = p
    )

    if (!is.null(save_csv_path)) {
        txt_path <- gsub("\\.csv$", "_analysis.txt", save_csv_path)
        save_analysis_to_txt(results, txt_path)
    }
    print_analysis_summary(results)

    cat("[ANALYSIS] Analysis complete!\n")
    return(results)
}


# Helper function to save analysis to text file
save_analysis_to_txt <- function(results, txt_path) {
    sink(txt_path)
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

# Helper function to print analysis summary to console
print_analysis_summary <- function(results) {
    cat("\n=== ANALYSIS SUMMARY ===\n")

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
