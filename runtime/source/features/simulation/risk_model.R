# ─────────────────────────────────────────────────────────────────────────────
# Drop Risk Model
# ─────────────────────────────────────────────────────────────────────────────

get_mgcv_thread_count <- function(default = 1L) {
    default <- max(1L, as.integer(default))
    if (exists("get_configured_core_count", mode = "function")) {
        allow_detect <- TRUE
        if (base::exists("MAX_CORES", envir = .GlobalEnv, inherits = FALSE)) {
            max_cfg <- base::get("MAX_CORES", envir = .GlobalEnv, inherits = FALSE)
            allow_detect <- max_cfg <= 0
        }
        cores <- get_configured_core_count(default = default, allow_autodetect = allow_detect)
        cores <- as.integer(cores)
        if (is.na(cores) || cores < 1L) {
            cores <- default
        }
        # Always leave one core free for the system when possible
        if (cores > 1L) {
            cores <- cores - 1L
        }
        return(max(1L, cores))
    }
    default
}

normalize_factor_labels <- function(x) {
    if (is.null(x)) {
        return(x)
    }
    if (is.factor(x)) {
        x <- as.character(x)
    }
    x <- as.character(x)
    if (!length(x)) {
        return(x)
    }
    x <- trimws(x)
    x[!nzchar(x)] <- NA_character_
    x
}

unique_nonempty <- function(x) {
    vals <- normalize_factor_labels(x)
    vals <- vals[!is.na(vals)]
    unique(vals)
}

get_model_levels <- function(model, key, fallback_values = NULL) {
    attr_name <- paste0("levels_", key)
    lvls <- attr(model, attr_name)
    lvls <- normalize_factor_labels(lvls)
    lvls <- lvls[!is.na(lvls)]
    if ((is.null(lvls) || length(lvls) == 0) && !is.null(fallback_values)) {
        fallback <- unique_nonempty(fallback_values)
        if (length(fallback) > 0) {
            lvls <- fallback
        }
    }
    lvls
}

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

    # Normalize categorical labels to avoid whitespace-related mismatches
    dropped_rows <- rep(FALSE, nrow(haz))
    if ("condition" %in% names(haz)) {
        haz$condition <- normalize_factor_labels(haz$condition)
        dropped_rows <- dropped_rows | is.na(haz$condition)
    }
    if ("phase" %in% names(haz)) {
        haz$phase <- normalize_factor_labels(haz$phase)
        dropped_rows <- dropped_rows | is.na(haz$phase)
    }
    if (any(dropped_rows)) {
        hazard_logger(
            "WARN",
            sprintf(
                "Dropping %d hazard rows lacking normalized condition/phase labels",
                sum(dropped_rows)
            )
        )
        haz <- haz[!dropped_rows, , drop = FALSE]
    }

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

    # Normalize condition and phase labels before modeling
    condition_levels <- NULL
    phase_levels <- NULL
    cond_phase_levels <- NULL

    if ("condition" %in% names(hazard_samples)) {
        condition_values <- normalize_factor_labels(hazard_samples$condition)
        missing_condition <- is.na(condition_values)
        if (any(missing_condition)) {
            model_logger("WARN", sprintf("Removing %d hazard rows with missing condition labels after normalization", sum(missing_condition)))
            hazard_samples <- hazard_samples[!missing_condition, , drop = FALSE]
            condition_values <- condition_values[!missing_condition]
        }
        condition_levels <- unique(condition_values)
        hazard_samples$condition <- factor(condition_values, levels = condition_levels)
        model_logger("DEBUG", "Condition levels after normalization:", paste(condition_levels, collapse = ", "))
    }

    if ("phase" %in% names(hazard_samples)) {
        phase_values <- normalize_factor_labels(hazard_samples$phase)
        missing_phase <- is.na(phase_values)
        if (any(missing_phase)) {
            model_logger("WARN", sprintf("Removing %d hazard rows with missing phase labels after normalization", sum(missing_phase)))
            hazard_samples <- hazard_samples[!missing_phase, , drop = FALSE]
            if (!is.null(condition_levels)) {
                condition_values <- normalize_factor_labels(hazard_samples$condition)
                condition_levels <- unique(condition_values)
                hazard_samples$condition <- factor(condition_values, levels = condition_levels)
            }
            phase_values <- phase_values[!missing_phase]
        }
        phase_levels <- unique(phase_values)
        hazard_samples$phase <- factor(phase_values, levels = phase_levels)
        model_logger("DEBUG", "Phase levels after normalization:", paste(phase_levels, collapse = ", "))
    }

    # Create condition x phase interaction factor for factor-by smooths (if enabled)
    if (use_by_interaction && "condition" %in% names(hazard_samples) && "phase" %in% names(hazard_samples)) {
        model_logger("DEBUG", "Creating condition x phase interaction factor for shape differences...")
        hazard_samples$cond_phase <- interaction(hazard_samples$condition, hazard_samples$phase, drop = TRUE)
        cond_phase_levels <- levels(hazard_samples$cond_phase)
        model_logger("DEBUG", "  - Created", length(cond_phase_levels), "unique condition x phase combinations:", paste(cond_phase_levels, collapse = ", "))
    } else if (!use_by_interaction) {
        model_logger("DEBUG", "Factor-by smooths disabled by use_by_interaction=FALSE. Using simple smooths.")
    } else {
        model_logger("WARN", "condition or phase columns not found. Using simple smooths only.")
    }

    # Remove any rows with missing values
    required_cols <- c("e", "v", "participant", "drop_within_tau")
    if ("condition" %in% names(hazard_samples)) {
        required_cols <- c(required_cols, "condition")
    }
    if ("phase" %in% names(hazard_samples)) {
        required_cols <- c(required_cols, "phase")
    }
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

    bam_threads <- get_mgcv_thread_count(default = 1L)
    model_logger("DEBUG", sprintf("Using %d thread(s) for bam() fit", bam_threads))

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
                    nthreads = bam_threads # controlled thread count
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
                    nthreads = bam_threads
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

            if (!is.null(condition_levels) && length(condition_levels) > 0) {
                attr(model, "levels_condition") <- condition_levels
            }
            if (!is.null(phase_levels) && length(phase_levels) > 0) {
                attr(model, "levels_phase") <- phase_levels
            }
            if (!is.null(cond_phase_levels) && length(cond_phase_levels) > 0) {
                attr(model, "levels_cond_phase") <- cond_phase_levels
            }

            return(model)
        },
        error = function(e) {
            model_logger("ERROR", "ERROR in model fitting:", e$message)
            return(NULL)
        }
    )
}

#' Apply standardized condition and phase to hazard samples
#'
#' @param hazard_samples Data frame with hazard samples
#' @param model Fitted model from train_risk_model()
#' @param standardized_condition Optional condition to use for standardized predictions (overrides data condition)
#' @param standardized_phase Optional phase to use for standardized predictions (overrides data phase)
#' @return Modified hazard_samples with standardized condition/phase factors
#' @export
apply_model_factors_to_hazard_samples <- function(hazard_samples, model, standardized_condition = NULL, standardized_phase = NULL) {
    factor_logger <- create_module_logger("APPLY_FACTORS")
    factor_logger("DEBUG", "Applying model factors to", nrow(hazard_samples), "hazard samples...")

    # Set participant factor
    hazard_samples$participant <- factor(hazard_samples$participant)
    factor_logger("DEBUG", "Set participant factor with", nlevels(hazard_samples$participant), "levels")

    n_rows <- nrow(hazard_samples)
    clean_input <- function(x, fallback, label) {
        if (is.null(x) || length(x) == 0) {
            return(fallback)
        }
        if (length(x) == 1 && n_rows > 1) {
            return(rep(x, length.out = n_rows))
        }
        if (length(x) != n_rows) {
            factor_logger(
                "WARN",
                sprintf(
                    "Length of standardized %s (%d) does not match hazard samples (%d); recycling to match.",
                    label, length(x), n_rows
                )
            )
            return(rep(x, length.out = n_rows))
        }
        x
    }

    ref_condition <- normalize_factor_labels(clean_input(standardized_condition, hazard_samples$condition, "condition"))
    ref_phase <- normalize_factor_labels(clean_input(standardized_phase, hazard_samples$phase, "phase"))

    summarize_values <- function(values) {
        values <- normalize_factor_labels(values)
        values <- unique(values[!is.na(values)])
        if (length(values) == 0) {
            return("<none>")
        }
        display <- values[seq_len(min(length(values), 5))]
        collapsed <- paste(display, collapse = ", ")
        if (length(values) > 5) {
            collapsed <- paste0(collapsed, ", ...")
        }
        collapsed
    }

    factor_logger(
        "DEBUG",
        sprintf(
            "Using reference condition(s): %s | phase(s): %s",
            summarize_values(ref_condition),
            summarize_values(ref_phase)
        )
    )

    model_cond_levels <- get_model_levels(model, "condition", ref_condition)
    model_phase_levels <- get_model_levels(model, "phase", ref_phase)
    model_cond_phase_levels <- get_model_levels(
        model,
        "cond_phase",
        paste(ref_condition, ref_phase, sep = ".")
    )

    apply_factor <- function(values, levels, label) {
        if (length(levels) == 0) {
            factor_logger(
                "WARN",
                sprintf("Model has no stored %s levels; using raw values", label)
            )
            return(factor(values))
        }
        unmatched <- setdiff(unique(values), c(levels, NA_character_))
        mapped <- values
        if (length(unmatched) > 0) {
            factor_logger(
                "WARN",
                sprintf(
                    "Standardized %s value(s) %s not seen during training; mapping to baseline level %s",
                    label,
                    summarize_values(unmatched),
                    levels[1]
                )
            )
            mapped[mapped %in% unmatched] <- levels[1]
        }
        factor(mapped, levels = levels)
    }

    hazard_samples$condition <- apply_factor(ref_condition, model_cond_levels, "condition")
    factor_logger("DEBUG", "Applied condition factor:", summarize_values(hazard_samples$condition))

    hazard_samples$phase <- apply_factor(ref_phase, model_phase_levels, "phase")
    factor_logger("DEBUG", "Applied phase factor:", summarize_values(hazard_samples$phase))

    if (length(model_cond_phase_levels) > 0) {
        combined_ref <- paste(as.character(hazard_samples$condition), as.character(hazard_samples$phase), sep = ".")
        unmatched_cp <- setdiff(unique(combined_ref), c(model_cond_phase_levels, NA_character_))
        mapped_cp <- combined_ref
        if (length(unmatched_cp) > 0) {
            factor_logger(
                "WARN",
                sprintf(
                    "Standardized condition x phase combination(s) %s not in training; mapping to %s",
                    summarize_values(unmatched_cp),
                    model_cond_phase_levels[1]
                )
            )
            mapped_cp[mapped_cp %in% unmatched_cp] <- model_cond_phase_levels[1]
        }
        hazard_samples$cond_phase <- factor(mapped_cp, levels = model_cond_phase_levels)
        factor_logger("DEBUG", "Applied cond_phase factor:", summarize_values(hazard_samples$cond_phase))
    }

    factor_logger("DEBUG", "Factor application completed successfully")
    return(hazard_samples)
}



#' Score trial with risk model (on-the-fly computation only)
#'
#' @param sim_data Simulation data from get_simulation_data()
#' @param model Fitted model from train_risk_model()
#' @param tau Time horizon for drop prediction (default 0.15 seconds)
#' @param standardized_condition Optional condition to use for standardized predictions (overrides data condition)
#' @param standardized_phase Optional phase to use for standardized predictions (overrides data phase)
#' @return List with individual_predictions and hazard_samples
#' @export
score_trial_with_model <- function(sim_data, model, tau = 0.15, standardized_condition = NULL, standardized_phase = NULL) {
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

    # Handle condition/phase factors (if they are supplied)
    hazard_samples <- apply_model_factors_to_hazard_samples(hazard_samples, model, standardized_condition, standardized_phase)

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

    level_map <- list(
        condition = attr(model, "levels_condition"),
        phase = attr(model, "levels_phase"),
        cond_phase = attr(model, "levels_cond_phase")
    )
    for (key in names(level_map)) {
        lvls <- level_map[[key]]
        if (!is.null(lvls) && length(lvls) > 0) {
            preview <- paste(head(lvls, 5), collapse = ", ")
            if (length(lvls) > 5) {
                preview <- paste0(preview, ", ...")
            }
            save_logger("DEBUG", sprintf("Persisting %s levels (%d): %s", key, length(lvls), preview))
        }
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

        level_map <- list(
            condition = attr(model, "levels_condition"),
            phase = attr(model, "levels_phase"),
            cond_phase = attr(model, "levels_cond_phase")
        )
        for (key in names(level_map)) {
            lvls <- level_map[[key]]
            if (!is.null(lvls) && length(lvls) > 0) {
                preview <- paste(head(lvls, 5), collapse = ", ")
                if (length(lvls) > 5) {
                    preview <- paste0(preview, ", ...")
                }
                load_logger("DEBUG", sprintf("Model %s levels (%d): %s", key, length(lvls), preview))
            }
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
#' Ensure global hazard samples with predictions are available
#'
#' This function checks if global hazard samples with predictions exist, and if not,
#' loads them from the default path. It's a lightweight version of setup_global_risk_variables
#' that only handles hazard samples.
#'
#' @return TRUE if hazard samples are available, FALSE otherwise
#' @export
ensure_global_hazard_samples_available <- function() {
    # Check if global hazard samples already exist
    if (exists("GLOBAL_HAZARD_SAMPLES_PREDS") && !is.null(GLOBAL_HAZARD_SAMPLES_PREDS)) {
        global_logger <- create_module_logger("GLOBAL")
        global_logger("DEBUG", "Global hazard samples already available with", nrow(GLOBAL_HAZARD_SAMPLES_PREDS), "rows")
        return(TRUE)
    }

    # Ensure global data is initialized to access paths
    ensure_global_data_initialized()

    global_logger <- create_module_logger("GLOBAL")

    # Try to load hazard samples from default path
    if (file.exists(risk_hazard_samples_preds_path)) {
        GLOBAL_HAZARD_SAMPLES_PREDS <<- readRDS(risk_hazard_samples_preds_path)
        global_logger("INFO", "Loaded hazard samples with predictions from:", risk_hazard_samples_preds_path)
        global_logger("DEBUG", "Loaded", nrow(GLOBAL_HAZARD_SAMPLES_PREDS), "hazard prediction rows")

        # Ensure efficient structure for lookups
        if (!data.table::is.data.table(GLOBAL_HAZARD_SAMPLES_PREDS)) {
            GLOBAL_HAZARD_SAMPLES_PREDS <<- data.table::as.data.table(GLOBAL_HAZARD_SAMPLES_PREDS)
        }
        if ("participant" %in% names(GLOBAL_HAZARD_SAMPLES_PREDS)) {
            GLOBAL_HAZARD_SAMPLES_PREDS[, participant := as.character(participant)]
        }
        if ("trial" %in% names(GLOBAL_HAZARD_SAMPLES_PREDS)) {
            GLOBAL_HAZARD_SAMPLES_PREDS[, trial := as.character(trial)]
        }
        if (all(c("participant", "trial") %in% names(GLOBAL_HAZARD_SAMPLES_PREDS))) {
            data.table::setkey(GLOBAL_HAZARD_SAMPLES_PREDS, participant, trial)
        }

        # Ensure tau attribute is available for downstream consumers
        tau_attr <- attr(GLOBAL_HAZARD_SAMPLES_PREDS, "tau")
        if (is.null(tau_attr) && "tau" %in% names(GLOBAL_HAZARD_SAMPLES_PREDS)) {
            tau_candidates <- unique(na.omit(as.numeric(GLOBAL_HAZARD_SAMPLES_PREDS$tau)))
            if (length(tau_candidates) > 0) {
                tau_attr <- tau_candidates[1]
            }
        }
        if (!is.null(tau_attr) && is.finite(tau_attr) && tau_attr > 0) {
            attr(GLOBAL_HAZARD_SAMPLES_PREDS, "tau") <<- tau_attr
            global_logger("DEBUG", "Hazard predictions tau attribute set to", tau_attr, "seconds")
        }
        return(TRUE)
    } else {
        global_logger("WARN", "No hazard samples with predictions found at:", risk_hazard_samples_preds_path)
        return(FALSE)
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
#' @return Fitted model object
#' @export
train_and_save_risk_model <- function(participants, trials, tau = 0.15,
                                      sampling_freq = 90, file_path = NULL, use_by_interaction = FALSE,
                                      standardized_condition = NULL, standardized_phase = NULL,
                                      annotate_predictions = TRUE, compute_standardized = TRUE) {
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
                    sim_data <- get_simulation_data(participant, trial)
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

        std_means <- NULL

        if (annotate_predictions) {
            # ── NEW: Add per-sample predictions to saved hazard samples ──
            train_logger("INFO", "Step 4/5: Adding per-sample predictions to hazard samples...")
            ensure_global_data_initialized()

            # Add fixed-effects predictions (recommended for comparability across participants)
            all_hazard_samples <- apply_model_factors_to_hazard_samples(all_hazard_samples, model, standardized_condition, standardized_phase)
            annotate_hazard_predictions(model, all_hazard_samples,
                include_re = FALSE,
                out_path = risk_hazard_samples_preds_path
            )
        } else {
            train_logger("INFO", "Skipping prediction annotation (annotate_predictions = FALSE)")
        }

        if (compute_standardized) {
            if (!annotate_predictions) {
                train_logger("WARN", "Standardized risk computation requested but predictions were not generated; skipping standardized computation.")
            } else if (nrow(all_hazard_samples) > 0) {
                # ── NEW: pooled standardized predictions by Condition × Phase ──
                train_logger("INFO", "Step 5/5: Computing pooled standardized risks by Condition × Phase...")
                std_means <- compute_pooled_standardized_risks(
                    model,
                    all_hazard_samples
                )
            } else {
                train_logger("INFO", "Step 5/5: Skipping standardized risk computation (no samples found)")
            }
        }

        if (!is.null(std_means)) {
            # Return both model and standardized means
            return(list(model = model, standardized_means = std_means))
        }

        # Return just the model
        return(model)
    })
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
                                        out_path = NULL) {
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

    # Choose whether to include random effect smooth
    pred_args <- list(type = "response")
    if (!include_re) pred_args$exclude <- "s(participant)" # fixed-effects only

    # Predict all at once
    predict_logger("DEBUG", "Processing", nrow(hazard_samples), "samples in prediction...")

    tryCatch(
        {
            p_hat <- do.call(predict, c(list(object = model, newdata = hazard_samples), pred_args))
        },
        error = function(e) {
            predict_logger("ERROR", "ERROR in prediction:", e$message)
            p_hat <- rep(NA_real_, nrow(hazard_samples))
        }
    )

    # Attach prediction column (name indicates FE/RE)
    colname <- if (include_re) "p_hat_re" else "p_hat_fe"
    hazard_samples[[colname]] <- p_hat

    # Also add eta (linear predictor) for rate calculations
    eta_colname <- if (include_re) "eta_re" else "eta_fe"
    eta_args <- list(type = "link")
    if (!include_re) eta_args$exclude <- "s(participant)"

    tryCatch(
        {
            eta_hat <- do.call(predict, c(list(object = model, newdata = hazard_samples), eta_args))
        },
        error = function(e) {
            predict_logger("ERROR", "ERROR in eta prediction:", e$message)
            eta_hat <- rep(NA_real_, nrow(hazard_samples))
        }
    )
    hazard_samples[[eta_colname]] <- eta_hat

    # Add 1s drop risk calculation using eta and tau
    # Get tau from model attributes
    tau <- attr(model, "tau")
    if (is.null(tau) || !is.finite(tau) || tau <= 0) {
        tau <- attr(hazard_samples, "tau")
    }
    if ((is.null(tau) || !is.finite(tau) || tau <= 0) && "tau" %in% names(hazard_samples)) {
        tau_candidates <- unique(na.omit(as.numeric(hazard_samples$tau)))
        if (length(tau_candidates) > 0) {
            tau <- tau_candidates[1]
        }
    }
    if (is.null(tau) || !is.finite(tau) || tau <= 0) {
        tau <- 0.2
        predict_logger("WARN", "Tau not provided; using default tau =", tau, "seconds for 1s drop risk")
    } else {
        predict_logger("DEBUG", "Using tau =", tau, "seconds for 1s drop risk calculation")
    }

    # Store tau attribute for downstream consumers
    attr(hazard_samples, "tau") <- tau

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
        predict_logger("INFO", "Saved hazard samples with predictions to:", out_path)
    } else {
        predict_logger("DEBUG", "Skipped saving hazard samples (out_path = FALSE)")
    }
    invisible(hazard_samples)
}
