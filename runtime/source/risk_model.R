# ─────────────────────────────────────────────────────────────────────────────
# Drop Risk Model
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(zoo)
library(mgcv)

build_hazard_samples <- function(sim_data, tau = 0.2) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(data.frame())
    }

    # Sort once
    sim_data <- sim_data[order(sim_data$time), ]


    if (nrow(sim_data) == 0) {
        cat("[DEBUG] No data found!\n")
        return(data.frame())
    }

    # --- features (compute winsorization limits once) ---
    # Calculate velocity towards closest edge
    # The closest edge is at distance q_max from center, so direction is q/|q| * q_max
    # Velocity towards edge = qd * (direction to edge)
    # Since we want velocity TOWARDS edge, we use the component of qd in the direction of q
    q_magnitude <- abs(sim_data$q)
    q_direction <- ifelse(q_magnitude > 0, sim_data$q / q_magnitude, 0)
    v_towards_edge <- sim_data$qd * q_direction
    # v_raw <- abs(on_platform_data$qd) - OLD CODE
    # Winsorize the velocity towards edge
    v_raw <- v_towards_edge
    qlo <- quantile(v_raw, 0.01, na.rm = TRUE)
    qhi <- quantile(v_raw, 0.99, na.rm = TRUE)

    # Create episode_id from respawn_segment if available, otherwise use trial
    if ("respawn_segment" %in% names(sim_data)) {
        episode_id <- sim_data$respawn_segment
    } else {
        # Fallback: use trial number as episode (assuming one episode per trial)
        episode_id <- rep(1, nrow(sim_data))
    }

    # Select columns to include in hazard samples
    base_cols <- c("time", "e", "v", "q", "qd", "q_max", "dist_to_escape_ratio", "episode_id")

    haz <- transform(
        sim_data,
        e = pmax(0, pmin(1, dist_to_escape_ratio)),
        v = pmin(pmax(v_raw, qlo), qhi),
        episode_id = episode_id
    )[, base_cols]

    # --- label: within tau of NEXT drop (use times of end-of-segment) ---
    # Accept either a provided flag or fall back to detecting on->off transitions
    last_samples <- numeric(0) # Initialize

    if ("is_last_sample_of_segment" %in% names(sim_data)) {
        # Debug: Check the is_last_sample_of_segment column
        n_last_segment_flags <- sum(sim_data$is_last_sample_of_segment, na.rm = TRUE)
        cat(sprintf("[DEBUG] Found %d TRUE values in is_last_sample_of_segment column\n", n_last_segment_flags))

        if (n_last_segment_flags > 0) {
            last_segment_times <- sim_data$time[sim_data$is_last_sample_of_segment == TRUE]
        }

        last_samples <- sim_data$time[sim_data$is_last_sample_of_segment == TRUE]
    } else {
        cat("[DEBUG] is_last_sample_of_segment column not found, using fallback detection\n")
        # derive last on-platform sample times as those where simulating goes TRUE->FALSE next
        sim_on <- sim_data$simulating == TRUE
        last_idx <- which(sim_on & !c(sim_on[-1], FALSE))
        last_samples <- sim_data$time[last_idx]
        cat(sprintf("[DEBUG] Fallback detection found %d last samples\n", length(last_samples)))
    }
    last_samples <- sort(unique(last_samples))

    haz$drop_within_tau <- 0L
    positives <- 0L

    if (length(last_samples) > 0) {
        if (tau == 0) {
            # Special case: tau=0 means only the last sample of each segment gets label 1
            haz$drop_within_tau <- as.integer(haz$time %in% last_samples)
            positives <- sum(haz$drop_within_tau, na.rm = TRUE)
        } else {
            # Normal case: for each hazard sample, find first last_sample strictly after it
            idx <- findInterval(haz$time, last_samples, rightmost.closed = FALSE)
            next_idx <- idx + 1L
            next_idx[next_idx > length(last_samples)] <- NA_integer_
            next_drop_time <- ifelse(is.na(next_idx), NA_real_, last_samples[next_idx])

            time_to_next_drop <- next_drop_time - haz$time
            haz$drop_within_tau <- as.integer(!is.na(time_to_next_drop) & time_to_next_drop <= tau & time_to_next_drop >= 0)
            positives <- sum(haz$drop_within_tau, na.rm = TRUE)
        }
    }

    # Remove NA features last
    haz <- haz[!is.na(haz$e) & !is.na(haz$v), , drop = FALSE]

    # --- critical debug output ---
    n_on <- nrow(sim_data)
    n_haz <- nrow(haz)
    n_last <- length(last_samples)
    dt_med <- if (n_on > 1) stats::median(diff(sim_data$time), na.rm = TRUE) else NA_real_
    cat(sprintf(
        "[hazard] tau=%.3fs | on-samples=%d | hazard-rows=%d | last_samples=%d | dt~%.3fs | positives=%d (%.4f%%)\n",
        tau, n_on, n_haz, n_last, dt_med, positives, ifelse(n_haz > 0, 100 * positives / n_haz, 0)
    ))

    return(haz)
}


#' Resample simulation data per episode to target frequency
#'
#' @param sim_data Raw simulation data frame with respawn_segment, time, and other columns
#' @param target_freq Target sampling frequency in Hz
#' @return Resampled simulation data frame
#' @export
resample_simulation_per_episode <- function(sim_data, target_freq = 20) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(sim_data)
    }

    # If target frequency is 90 Hz or higher, don't resample
    if (target_freq >= 90) {
        return(sim_data)
    }

    # Check required columns
    required_cols <- c("time", "respawn_segment")
    missing_cols <- setdiff(required_cols, names(sim_data))
    if (length(missing_cols) > 0) {
        cat(sprintf("[WARNING] Missing columns for episode resampling: %s\n", paste(missing_cols, collapse = ", ")))
        return(sim_data)
    }

    dt <- 1 / target_freq

    resampled_data <- sim_data |>
        dplyr::group_by(respawn_segment) |>
        # episode start and end times
        dplyr::mutate(
            t0 = min(time),
            t_end = {
                drop_times <- time[is_last_sample_of_segment == TRUE]
                if (length(drop_times) > 0) {
                    max(drop_times)
                } else {
                    max(time) # Use episode end if no drops
                }
            }
        ) |>
        # bin index: floor((t - t0)/dt)
        dplyr::mutate(bin = floor((time - t0) / dt)) |>
        # aggregate to one row per bin using reframe
        dplyr::reframe(
            time = first(t0) + bin * dt,
            # average numeric columns
            q = mean(q, na.rm = TRUE),
            qd = mean(qd, na.rm = TRUE),
            q_max = mean(q_max, na.rm = TRUE),
            dist_to_escape_ratio = mean(dist_to_escape_ratio, na.rm = TRUE),
            is_last_sample_of_segment = max(is_last_sample_of_segment, na.rm = TRUE),
            # take first value for non-numeric columns (should be constant within episode)
            dplyr::across(where(~ !is.numeric(.)), first),
            .by = respawn_segment
        )

    # Time is already correctly named in reframe output

    cat(sprintf(
        "[RESAMPLE] Original: %d samples -> Resampled: %d samples [%.1f%% retained]\n",
        nrow(sim_data), nrow(resampled_data), 100 * nrow(resampled_data) / nrow(sim_data)
    ))

    return(resampled_data)
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
    n_cores <- parallel::detectCores()
    cat(sprintf("Using %d CPU cores for parallel processing\n", n_cores))

    tryCatch(
        {
            model <- bam(
                drop_within_tau ~ s(e, k = 5) + s(v, k = 5) + s(participant, bs = "re"),
                family = binomial(link = "cloglog"),
                data = hazard_samples,
                method = "fREML",
                discrete = TRUE, # big speed win
                nthreads = n_cores # use all cores
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
#' @return Data frame with one row containing risk metrics
#' @export
score_trial_with_model <- function(sim_data, model, tau = 0.2) {
    if (is.null(model)) {
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    # Build hazard samples
    hazard_samples <- build_hazard_samples(sim_data, tau)

    if (nrow(hazard_samples) == 0) {
        cat("[DEBUG] score_trial_with_model: returning empty list\n")
        return(list(
            individual_predictions = numeric(0), # Store empty predictions
            hazard_samples = data.frame() # Store empty hazard samples
        ))
    }

    # Add participant information for prediction
    # Extract participant from sim_data (should be the same for all rows)
    participant_value <- unique(sim_data$participant)

    # Handle missing or empty participant information
    if (length(participant_value) == 0 || all(is.na(participant_value))) {
        cat("[ERROR] No valid participant information found in sim_data\n")
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    if (length(participant_value) != 1) {
        cat(sprintf("[WARNING] Multiple participants found in sim_data: %s\n", paste(participant_value, collapse = ", ")))
        participant_value <- participant_value[1] # Use first one
    }

    # Add participant column to hazard samples
    hazard_samples$participant <- as.factor(participant_value)

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
        cat("Step 3/3: Saving model to disk...\n")
        save_risk_model(model, file_path)

        # Verify the file was created
        if (file.exists(file_path)) {
            file_size <- file.info(file_path)$size
            cat(sprintf("Risk model saved successfully to %s (%.2f MB)\n", file_path, file_size / 1024^2))
        } else {
            cat(sprintf("ERROR: Model file was not created at %s\n", file_path))
        }

        return(model)
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
