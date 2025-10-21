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

    need <- c("e", "v", "participant", "condition", "phase")
    miss <- setdiff(need, names(hazard_samples))
    if (length(miss) > 0) stop("hazard_samples missing: ", paste(miss, collapse = ", "))

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
        std_logger("WARN", "Tau not provided; using default tau =", tau, "seconds")
    } else {
        std_logger("DEBUG", "Using tau =", tau, "seconds for rate calculations")
    }
    attr(hazard_samples, "tau") <- tau

    hazard_samples$participant <- factor(hazard_samples$participant)

    raw_condition <- normalize_factor_labels(hazard_samples$condition)
    raw_phase <- normalize_factor_labels(hazard_samples$phase)

    condition_levels <- get_model_levels(model, "condition", raw_condition)
    phase_levels <- get_model_levels(model, "phase", raw_phase)
    cond_phase_levels <- get_model_levels(
        model,
        "cond_phase",
        paste(raw_condition, raw_phase, sep = ".")
    )

    if (length(condition_levels) == 0) {
        std_logger("ERROR", "No valid condition levels available for standardization")
        return(NULL)
    }
    if (length(phase_levels) == 0) {
        std_logger("ERROR", "No valid phase levels available for standardization")
        return(NULL)
    }

    hazard_samples$condition <- factor(raw_condition, levels = condition_levels)
    hazard_samples$phase <- factor(raw_phase, levels = phase_levels)
    if (length(cond_phase_levels) > 0) {
        hazard_samples$cond_phase <- factor(
            paste(hazard_samples$condition, hazard_samples$phase, sep = "."),
            levels = cond_phase_levels
        )
    }

    std_logger("INFO", sprintf(
        "Computing standardized mapping scores for %d condition x %d phase combinations",
        length(condition_levels),
        length(phase_levels)
    ))
    std_logger("DEBUG", "Condition levels:", paste(condition_levels, collapse = ", "))
    std_logger("DEBUG", "Phase levels:", paste(phase_levels, collapse = ", "))
    if (length(cond_phase_levels) > 0) {
        std_logger("DEBUG", "condition x phase levels:", paste(cond_phase_levels, collapse = ", "))
    }

    ref_df <- hazard_samples[, c("e", "v", "participant")]
    ref_df$participant <- factor(ref_df$participant)

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

    total_targets <- length(condition_levels) * length(phase_levels)
    res_list <- list()

    n_cores_default <- get_mgcv_thread_count(default = 1L)

    for (cn in condition_levels) {
        for (ph in phase_levels) {
            combo_label <- paste(cn, ph, sep = " x ")

            if (length(cond_phase_levels) > 0) {
                expected_cond_phase <- paste(cn, ph, sep = ".")
                if (!(expected_cond_phase %in% cond_phase_levels)) {
                    std_logger("DEBUG", "Skipping", combo_label, "because it was not present during model training")
                    next
                }
            }

            std_logger("DEBUG", "Processing standardized combination:", combo_label)

            newdf <- ref_df
            newdf$condition <- factor(rep(cn, nrow(newdf)), levels = condition_levels)
            newdf$phase <- factor(rep(ph, nrow(newdf)), levels = phase_levels)
            if (length(cond_phase_levels) > 0) {
                newdf$cond_phase <- factor(
                    paste(newdf$condition, newdf$phase, sep = "."),
                    levels = cond_phase_levels
                )
            }

            std_logger("DEBUG", "Using", n_cores_default, "cores for parallel prediction")
            std_logger("DEBUG", "Predicting on", nrow(newdf), "rows for", combo_label, "...")

            start_time <- Sys.time()
            eta <- predict(model, newdata = newdf, type = "link", exclude = "s(participant)", nthreads = n_cores_default)
            end_time <- Sys.time()
            std_logger("DEBUG", "Prediction for", combo_label, "completed in", round(as.numeric(end_time - start_time, units = "secs"), 2), "seconds")

            p_tau <- 1 - exp(-exp(eta))
            mean_h <- mean(tapply(p_tau, newdf$participant, mean, na.rm = TRUE) |>
                unlist(), na.rm = TRUE)

            lambda <- exp(eta) / tau
            risk1s <- 1 - exp(-lambda)
            exp_dpm <- 60 * mean(tapply(lambda, newdf$participant, mean, na.rm = TRUE) |>
                unlist(), na.rm = TRUE)
            risk_1s <- mean(tapply(risk1s, newdf$participant, mean, na.rm = TRUE) |>
                unlist(), na.rm = TRUE)

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

    if (length(res_list) == 0) {
        std_logger("ERROR", "No standardized risks could be computed; aborting analysis")
        return(NULL)
    }

    out <- do.call(rbind, res_list)
    rownames(out) <- NULL

    std_logger("INFO", sprintf(
        "Successfully computed standardized risks for %d/%d target combinations",
        nrow(out),
        total_targets
    ))
    if (nrow(out) > 0) {
        std_logger("DEBUG", "Computed combinations:")
        for (i in seq_len(nrow(out))) {
            std_logger(
                "DEBUG",
                sprintf(
                    "  - %s x %s : risk_1s = %.6f , mean_hazard = %.6f",
                    out$condition[i], out$phase[i], out$risk_1s[i], out$mean_hazard[i]
                )
            )
        }
    }

    analysis_results <- perform_risk_analysis(model, hazard_samples, out, analysis_results_path)

    std_logger("INFO", "Pooled standardized risks computed:")
    print(out)

    return(list(
        standardized_risks = out,
        analysis_results = analysis_results
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

    raw_condition <- normalize_factor_labels(hazard_samples$condition)
    raw_phase <- normalize_factor_labels(hazard_samples$phase)

    condition_levels <- get_model_levels(model, "condition", raw_condition)
    phase_levels <- get_model_levels(model, "phase", raw_phase)

    if (length(condition_levels) == 0) {
        condition_levels <- unique(raw_condition[!is.na(raw_condition)])
        analysis_logger("WARN", "Model missing stored condition levels; using observed hazard sample levels")
    }
    if (length(phase_levels) == 0) {
        phase_levels <- unique(raw_phase[!is.na(raw_phase)])
        analysis_logger("WARN", "Model missing stored phase levels; using observed hazard sample levels")
    }

    hazard_samples$condition <- factor(raw_condition, levels = condition_levels)
    hazard_samples$phase <- factor(raw_phase, levels = phase_levels)

    analysis_logger("DEBUG", "Condition levels for analysis:", paste(condition_levels, collapse = ", "))
    analysis_logger("DEBUG", "Phase levels for analysis:", paste(phase_levels, collapse = ", "))

    # Use levels from std_means (robust)
    cond_levels <- unique(normalize_factor_labels(std_means$condition))
    phase_levels <- unique(normalize_factor_labels(std_means$phase))
    analysis_logger("DEBUG", "cond_levels =", cond_levels)
    analysis_logger("DEBUG", "phase_levels =", phase_levels)

    # 1) Condition x Phase interaction (hazard model) - Wald test only
    # ML-based likelihood ratio tests were repeatedly unstable/failing, so we skip them and rely on the Wald test.
    analysis_logger("DEBUG", "Computing condition x phase interaction via Wald test")
    interaction_test <- NULL

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
        dplyr::select(condition, phase, risk_1s) |>
        tidyr::pivot_wider(id_cols = condition, names_from = phase, values_from = risk_1s) |>
        dplyr::mutate(
            d_retention = retention - baseline_task,
            d_transfer  = transfer - baseline_task
        )

    extract_mean <- function(data, cond, ph) {
        v <- data |>
            dplyr::filter(condition == cond, phase == ph) |>
            dplyr::pull(risk_1s)
        if (length(v) == 0 || all(is.na(v))) {
            return(NA_real_)
        }
        v[[1]]
    }

    get_mh <- function(cond, ph) {
        extract_mean(mm, cond, ph)
    }

    missing_notified <- new.env(parent = emptyenv())

    warn_missing_mean <- function(cond, ph) {
        key <- paste(cond, ph, sep = "::")
        if (!exists(key, envir = missing_notified, inherits = FALSE)) {
            analysis_logger("WARN", sprintf(
                "Missing standardized means for %s (%s vs baseline); returning NA",
                cond, ph
            ))
            assign(key, TRUE, envir = missing_notified)
        }
    }

    delta_for <- function(cond, phase) {
        base_val <- get_mh(cond, "baseline_task")
        phase_val <- get_mh(cond, phase)
        if (is.na(base_val)) warn_missing_mean(cond, "baseline_task")
        if (is.na(phase_val)) warn_missing_mean(cond, phase)
        if (is.na(base_val) || is.na(phase_val)) {
            return(NA_real_)
        }
        phase_val - base_val
    }

    calc_did <- function(condA, condB, phase) {
        deltaA <- delta_for(condA, phase)
        deltaB <- delta_for(condB, phase)
        if (is.na(deltaA) || is.na(deltaB)) {
            return(NA_real_)
        }
        deltaA - deltaB
    }

    contrast_defs <- tibble::tribble(
        ~contrast, ~condA, ~condB, ~phase,
        "PV vs Control (Retention)", "perturbation_visualization", "control", "retention",
        "PV vs Control (Transfer)", "perturbation_visualization", "control", "transfer",
        "P vs Control (Retention)", "perturbation", "control", "retention",
        "P vs Control (Transfer)", "perturbation", "control", "transfer",
        "P vs PV (Retention)", "perturbation", "perturbation_visualization", "retention",
        "P vs PV (Transfer)", "perturbation", "perturbation_visualization", "transfer"
    )

    available_contrasts <- contrast_defs |>
        dplyr::filter(.data$condA %in% cond_levels, .data$condB %in% cond_levels, .data$phase %in% needed_phases)

    if (nrow(available_contrasts) > 0) {
        DiD <- available_contrasts
        DiD$DiD <- mapply(calc_did, DiD$condA, DiD$condB, DiD$phase)
        DiD <- dplyr::select(DiD, contrast, DiD)
    } else {
        DiD <- tibble::tibble(contrast = character(), DiD = numeric())
    }

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

            # Log initial memory usage
            if (exists("gc")) {
                mem_before <- gc(verbose = FALSE)
                analysis_logger("DEBUG", "Memory before bootstrap:", round(sum(mem_before[, 2]), 1), "MB")
            }
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

                    # Add error handling for each combo
                    tryCatch(
                        {
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

                            # Check memory usage before processing
                            n_samples <- nrow(newdf)
                            estimated_memory_mb <- (n_samples * B * 8) / (1024^2) # 8 bytes per double
                            analysis_logger("DEBUG", "Memory estimate for combo", key, ":", round(estimated_memory_mb, 1), "MB")

                            chunk_budget_mb <- getOption("risk.analysis.chunk_memory_mb", 512)
                            if (estimated_memory_mb > chunk_budget_mb) {
                                analysis_logger("WARN", "Large memory requirement detected, using chunked processing for", key)

                                # Chunked processing to avoid memory blow-up
                                chunk_size <- max(1L, floor((chunk_budget_mb * 1024^2) / (B * 8)))
                                chunk_size <- min(chunk_size, n_samples)
                                if (chunk_size < 1L) {
                                    chunk_size <- max(1L, floor(n_samples / 2))
                                }
                                n_chunks <- ceiling(n_samples / chunk_size)
                                analysis_logger("DEBUG", "Chunk size set to", chunk_size, "rows across", n_chunks, "chunks (budget:", chunk_budget_mb, "MB)")

                                # Accumulators per participant across chunks
                                S_1s_accum <- matrix(0, nrow = G, ncol = B)

                                for (chunk_idx in seq_len(n_chunks)) {
                                    start_idx <- (chunk_idx - 1L) * chunk_size + 1L
                                    end_idx <- min(chunk_idx * chunk_size, n_samples)
                                    chunk_indices <- start_idx:end_idx

                                    newdf_chunk <- newdf[chunk_indices, , drop = FALSE]
                                    pid_chunk <- pid[chunk_indices]
                                    pid_chunk_factor <- factor(pid_chunk, levels = seq_len(G))

                                    X_chunk <- predict(model, newdata = newdf_chunk, type = "lpmatrix", exclude = "s(participant)")
                                    Eta_chunk <- X_chunk %*% Beta_matrix
                                    Lambda_chunk <- exp(Eta_chunk) / tau_model
                                    Risk1s_chunk <- 1 - exp(-Lambda_chunk)

                                    S_chunk <- rowsum(Risk1s_chunk, pid_chunk_factor, reorder = FALSE)
                                    S_chunk <- as.matrix(S_chunk)
                                    if (nrow(S_chunk) != G) {
                                        S_full <- matrix(0, nrow = G, ncol = ncol(S_chunk))
                                        idx <- as.integer(rownames(S_chunk))
                                        S_full[idx, ] <- S_chunk
                                        S_chunk <- S_full
                                    }
                                    S_1s_accum <- S_1s_accum + S_chunk

                                    rm(X_chunk, Eta_chunk, Lambda_chunk, Risk1s_chunk, S_chunk)
                                    gc()

                                    analysis_logger("DEBUG", "Completed chunk", chunk_idx, "/", n_chunks, "for combo", key)
                                }

                                S_1s <- S_1s_accum
                                rm(S_1s_accum)
                                M_1s <- sweep(S_1s, 1, n_g, "/")
                                overall_1s <- colMeans(M_1s)
                                rm(S_1s, M_1s)
                                gc()
                            } else {
                                # Standard vectorized processing for smaller datasets
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

                                # Clean up large matrices immediately
                                rm(X, Eta, Lambda, Risk1s, S_1s, M_1s)
                                gc()
                            }

                            # 5) STORE RESULTS (only 1-second risk)
                            combo_results[[key]] <- overall_1s

                            analysis_logger("DEBUG", "Completed combo", key, ":", B, "bootstrap samples")
                        },
                        error = function(e) {
                            analysis_logger("ERROR", "Failed to process combo", key, ":", e$message)
                            # Store NA results for failed combo
                            combo_results[[key]] <<- rep(NA_real_, B)
                        }
                    )
                }
            }

            # OLD CHUNKED PROCESSING REMOVED - REPLACED WITH VECTORIZED APPROACH ABOVE

            # Log memory usage after bootstrap processing
            if (exists("gc")) {
                mem_after <- gc(verbose = FALSE)
                analysis_logger("DEBUG", "Memory after bootstrap:", round(sum(mem_after[, 2]), 1), "MB")
            }

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

                grid$risk_1s <- sapply(seq_len(nrow(grid)), function(i) {
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
                        mean = mean(risk_1s),
                        lo = stats::quantile(risk_1s, 0.025, na.rm = TRUE),
                        hi = stats::quantile(risk_1s, 0.975, na.rm = TRUE),
                        .groups = "drop"
                    )

                # DiD bootstrap
                delta_value <- function(df, cond, phase) {
                    base_val <- extract_mean(df, cond, "baseline_task")
                    targ_val <- extract_mean(df, cond, phase)
                    if (is.na(base_val)) {
                        warn_missing_mean(cond, "baseline_task")
                    }
                    if (is.na(targ_val)) {
                        warn_missing_mean(cond, phase)
                    }
                    if (is.na(base_val) || is.na(targ_val)) {
                        return(NA_real_)
                    }
                    targ_val - base_val
                }

                did_from_df <- function(df, condA, condB, phase) {
                    deltaA <- delta_value(df, condA, phase)
                    deltaB <- delta_value(df, condB, phase)
                    if (is.na(deltaA) || is.na(deltaB)) {
                        return(NA_real_)
                    }
                    deltaA - deltaB
                }

                if (nrow(available_contrasts) > 0) {
                    did_names <- as.matrix(dplyr::select(available_contrasts, condA, condB, phase))
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

                    if (is.null(dim(did_boot_mat))) {
                        did_boot_mat <- matrix(did_boot_mat, nrow = nrow(available_contrasts))
                    }

                    did_pvals <- apply(did_boot_mat, 1, function(x) {
                        x <- x[is.finite(x)]
                        if (!length(x)) {
                            return(NA_real_)
                        }
                        2 * min(mean(x >= 0), mean(x <= 0))
                    })
                    did_pvals_holm <- if (length(did_pvals) > 0 && any(is.finite(did_pvals))) p.adjust(did_pvals, method = "holm") else rep(NA_real_, length(did_pvals))

                    DiD_boot <- tibble::tibble(
                        contrast = available_contrasts$contrast,
                        point = as.numeric(pts),
                        lo = apply(did_boot_mat, 1, \(x) stats::quantile(x, .025, na.rm = TRUE)),
                        med = apply(did_boot_mat, 1, \(x) stats::quantile(x, .5, na.rm = TRUE)),
                        hi = apply(did_boot_mat, 1, \(x) stats::quantile(x, .975, na.rm = TRUE)),
                        p_boot = as.numeric(did_pvals),
                        p_boot_holm = as.numeric(did_pvals_holm)
                    )
                } else {
                    analysis_logger("INFO", "No valid condition contrasts available for DiD bootstrap; skipping DiD summary.")
                    DiD_boot <- tibble::tibble()
                }

                # Within-condition deltas: CIs + p-values
                analysis_logger("DEBUG", "Computing bootstrap p-values for within-condition changes...")
                flush.console()

                within_boot_list <- lapply(boot_list, function(dfb) {
                    wide <- dfb |>
                        tidyr::pivot_wider(id_cols = condition, names_from = phase, values_from = risk_1s)
                    tibble::tibble(
                        condition   = wide$condition,
                        d_retention = wide$retention - wide$baseline_task,
                        d_transfer  = wide$transfer - wide$baseline_task
                    )
                })
                within_boot <- dplyr::bind_rows(within_boot_list, .id = "boot_id")

                delta_point <- std_means |>
                    dplyr::select(condition, phase, risk_1s) |>
                    tidyr::pivot_wider(id_cols = condition, names_from = phase, values_from = risk_1s) |>
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

                # Clean up within-condition bootstrap objects
                rm(within_boot_list, within_boot)
                gc() # Force garbage collection
            } # end non-empty boot_arr

            # Clean up bootstrap list after all DiD and within-condition processing
            rm(boot_list)
            gc() # Force garbage collection

            # Clean up bootstrap data after all processing is complete
            rm(boot_arr)
            gc() # Force garbage collection
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

        # Save analysis results to RDS file (now with proper memory cleanup)
        analysis_results_to_save <- list(
            standardized_risks = std_means,
            analysis_results = results
        )
        saveRDS(analysis_results_to_save, final_path)
        analysis_logger("INFO", "Analysis results saved to:", final_path)
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
    writer_logger <- create_module_logger("ANALYSIS")

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

        writer_logger("WARN", "File already exists, saving to:", txt_path)
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
    used_bootstrap_for_did <- !is.null(results$bootstrap_DiD_CI) && nrow(results$bootstrap_DiD_CI) > 0
    did_table <- if (used_bootstrap_for_did) {
        results$bootstrap_DiD_CI
    } else {
        results$difference_in_differences
    }
    if (!is.null(did_table) && nrow(did_table) > 0) {
        print(did_table)
    } else {
        cat("No difference-in-differences results available (skipped or failed).\n")
    }
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
    if (!isTRUE(used_bootstrap_for_did)) {
        if (!is.null(results$bootstrap_DiD_CI) && nrow(results$bootstrap_DiD_CI) > 0) {
            print(results$bootstrap_DiD_CI)
        } else {
            cat("No bootstrap DiD CIs available (skipped or failed).\n")
        }
    } else {
        cat("Bootstrap difference-in-differences reported above.\n")
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



