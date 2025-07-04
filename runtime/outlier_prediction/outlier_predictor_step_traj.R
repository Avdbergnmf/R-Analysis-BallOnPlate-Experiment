## ========================================================================== ##
## train_step_trajectory_model.R - Trajectory-based false heel strike and   ##
## step outlier detection                                                    ##
## ========================================================================== ##
## Based on foot trajectory patterns for false heel strikes and step        ##
## outliers. Follows the same flow as heelstrike_outlier_predictor.R but     ##
## uses trajectory features instead of statistical features                  ##
## -------------------------------------------------------------------------- ##

# ============================================================================ #
#                           CONFIGURATION SETTINGS                            #
# ============================================================================ #

# EXECUTION CONTROL
FORCE_RETRAIN <- TRUE # Suggested: FALSE for faster execution, TRUE for fresh models
DO_HEELSTRIKES <- TRUE # Set to FALSE to skip false heel strike detection
DO_OUTLIERS <- TRUE # Set to FALSE to skip outlier detection

# TRAJECTORY PARAMETERS
TRAJECTORY_FEATURES <- c("pos_x", "pos_z") # Which trajectory features to extract
USE_PCA <- FALSE # Use PCA for dimensionality reduction (less needed with engineered features)
PCA_COMPONENTS <- 10 # Number of PCA components to keep (if USE_PCA = TRUE)

# PREDICTION THRESHOLD - Probability threshold for classifying outliers
PRED_THRESHOLD <- 0.5 # Suggested: 0.5 (lower = more sensitive, higher = more conservative)

# SEPARATE THRESHOLDS for different models due to class imbalance differences
HEELSTRIKE_THRESHOLD <- 0.5 # False heel strikes: 2.6% minority class
STEP_THRESHOLD <- 0.5 # Step outliers: 0.6% minority class - very low threshold needed

# TRAINING SPLIT - Proportion of data used for training (rest for testing)
TRAIN_SPLIT <- 1.0 # Use all data for training

# RANDOM FOREST PARAMETERS - Conservative to prevent overfitting
RF_NTREE <- 300
RF_NODESIZE <- 10
RF_MAXNODES <- 50
RF_MTRY_FRACTION <- 0.33

# ONE-CLASS SVM PARAMETERS (for step outlier pattern matching)
# These parameters are only used for the step outlier model which is now trained
# as a novelty detector on the *outlier* trajectories themselves.
SVM_NU <- 0.001 # Very restrictive: expect 0.1% outliers in scan set (much more conservative)
SVM_KERNEL <- "rbfdot"
SVM_SIGMA <- 0.01 # Tighter RBF kernel for more precise matching

# OUTLIER DETECTION TUNING - Target similar ratio to original data
# Original: 154 outliers, want at most 50% increase = ~230 total, so ~76 new max
TARGET_OUTLIER_RATIO <- 0.01 # Target 0.6% of scanned trajectories as outliers (back to original)

# PARALLEL PROCESSING
ENABLE_PARALLEL <- TRUE
PARALLEL_CORES <- max(1, parallel::detectCores() - 1)

# TRIAL EXCLUSIONS
EXCLUDE_TRIALS <- c(1, 4, 6) # ignore warm-up / familiarisation trials

# ========================== ADVANCED TRAJECTORY ANALYSIS ==================== #

# NOISE FILTERING: Simple smoothing function for noisy trajectory data
apply_trajectory_smoothing <- function(data, window_size = 3) {
    # Apply simple moving average smoothing
    if (length(data) < window_size) {
        return(data)
    }

    smoothed <- data
    half_window <- floor(window_size / 2)

    for (i in (half_window + 1):(length(data) - half_window)) {
        start_idx <- i - half_window
        end_idx <- i + half_window
        smoothed[i] <- mean(data[start_idx:end_idx], na.rm = TRUE)
    }

    return(smoothed)
}

# ALTERNATIVE: Exponential smoothing for trajectory data
apply_exponential_smoothing <- function(data, alpha = 0.3) {
    # Simple exponential smoothing to reduce noise
    if (length(data) < 2) {
        return(data)
    }

    smoothed <- numeric(length(data))
    smoothed[1] <- data[1]

    for (i in 2:length(data)) {
        smoothed[i] <- alpha * data[i] + (1 - alpha) * smoothed[i - 1]
    }

    return(smoothed)
}

# OPTIMIZED: Calculate kinematic features with noise filtering
calculate_kinematic_features <- function(time, position, feature_prefix = "") {
    if (length(time) < 3 || length(unique(position)) == 1) {
        return(setNames(rep(NA_real_, 8), paste0(feature_prefix, c(
            "max_velocity", "mean_velocity", "velocity_peaks",
            "max_acceleration", "acceleration_range", "mean_jerk",
            "jerk_peaks", "trajectory_smoothness"
        ))))
    }

    # NOISE FILTERING: Apply smoothing to position data before differentiation
    # Use trajectory smoothing to reduce noise before calculating derivatives
    position_smooth <- apply_trajectory_smoothing(position, window_size = 3)

    # VECTORIZED: Calculate derivatives from smoothed data
    dt <- diff(time)
    velocity <- diff(position_smooth) / dt

    # ADDITIONAL FILTERING: Apply smoothing to velocity (first derivative amplifies noise)
    velocity_smooth <- apply_trajectory_smoothing(velocity, window_size = 3)

    if (length(velocity_smooth) < 2) {
        return(setNames(rep(NA_real_, 8), paste0(feature_prefix, c(
            "max_velocity", "mean_velocity", "velocity_peaks",
            "max_acceleration", "acceleration_range", "mean_jerk",
            "jerk_peaks", "trajectory_smoothness"
        ))))
    }

    # Calculate acceleration from smoothed velocity
    acceleration <- diff(velocity_smooth) / dt[-1]

    # CRITICAL FILTERING: Apply stronger smoothing to acceleration (double differentiation = very noisy)
    acceleration_smooth <- apply_trajectory_smoothing(acceleration, window_size = 5)

    if (length(acceleration_smooth) < 2) {
        jerk <- NA_real_
        trajectory_smoothness <- NA_real_
    } else {
        # Calculate jerk from smoothed acceleration
        jerk <- diff(acceleration_smooth) / dt[-c(1, 2)]

        # MAXIMUM FILTERING: Apply strongest smoothing to jerk (triple differentiation = extremely noisy)
        jerk_smooth <- apply_trajectory_smoothing(jerk, window_size = 5)

        # VECTORIZED: Trajectory smoothness calculation
        mean_dt <- mean(dt, na.rm = TRUE)
        trajectory_smoothness <- if (length(jerk_smooth) > 0) -sum(jerk_smooth^2, na.rm = TRUE) * mean_dt^5 else NA_real_
    }

    # OPTIMIZED: Vectorized peak counting function (with smoothed data)
    count_peaks <- function(x) {
        if (length(x) < 3) {
            return(0)
        }
        x_abs <- abs(x)
        # Vectorized peak detection: find local maxima
        diff_signs <- diff(sign(diff(x_abs)))
        sum(diff_signs == -2, na.rm = TRUE)
    }

    # VECTORIZED: Feature calculations using smoothed derivatives
    abs_velocity <- abs(velocity_smooth)
    abs_acceleration <- abs(acceleration_smooth)

    features <- c(
        max_velocity = max(abs_velocity, na.rm = TRUE),
        mean_velocity = mean(abs_velocity, na.rm = TRUE),
        velocity_peaks = count_peaks(velocity),
        max_acceleration = max(abs_acceleration, na.rm = TRUE),
        acceleration_range = diff(range(acceleration, na.rm = TRUE)),
        mean_jerk = if (length(jerk_smooth) > 0) mean(abs(jerk_smooth), na.rm = TRUE) else NA_real_,
        jerk_peaks = if (length(jerk_smooth) > 0) count_peaks(jerk_smooth) else 0,
        trajectory_smoothness = trajectory_smoothness
    )

    setNames(features, paste0(feature_prefix, names(features)))
}

# OPTIMIZED: Calculate frequency domain features (vectorized)
calculate_frequency_features <- function(time, position, feature_prefix = "") {
    if (length(time) < 8 || length(unique(position)) == 1) {
        return(setNames(rep(NA_real_, 5), paste0(feature_prefix, c(
            "dominant_frequency", "spectral_centroid", "spectral_rolloff",
            "low_freq_power", "high_freq_power"
        ))))
    }

    # OPTIMIZED: More efficient power-of-2 calculation
    n_points <- min(256, 2^floor(log2(length(time))))
    if (n_points < 8) n_points <- 8

    # VECTORIZED: Direct interpolation
    time_range <- range(time)
    time_regular <- seq(time_range[1], time_range[2], length.out = n_points)
    position_regular <- approx(time, position, xout = time_regular, rule = 2)$y

    # NOISE FILTERING: Apply light smoothing to interpolated data for better FFT
    position_smooth <- apply_trajectory_smoothing(position_regular, window_size = 3)

    # Apply FFT (centered data for better frequency analysis)
    position_centered <- position_smooth - mean(position_smooth)
    fft_result <- fft(position_centered)
    n_freq <- length(fft_result)

    # VECTORIZED: Power spectrum calculation (single-sided)
    half_n <- n_freq %/% 2
    power_spectrum <- Mod(fft_result[1:half_n])^2
    freq_bins <- seq(0, 0.5, length.out = half_n)

    if (length(power_spectrum) < 2) {
        return(setNames(rep(NA_real_, 5), paste0(feature_prefix, c(
            "dominant_frequency", "spectral_centroid", "spectral_rolloff",
            "low_freq_power", "high_freq_power"
        ))))
    }

    # VECTORIZED: Dominant frequency (excluding DC component)
    power_no_dc <- power_spectrum[-1]
    dominant_idx <- which.max(power_no_dc) + 1
    dominant_frequency <- freq_bins[dominant_idx]

    # VECTORIZED: Spectral centroid
    total_power <- sum(power_spectrum)
    spectral_centroid <- if (total_power > 0) sum(freq_bins * power_spectrum) / total_power else 0

    # VECTORIZED: Spectral rolloff (95% of energy)
    cumulative_power <- cumsum(power_spectrum)
    rolloff_threshold <- 0.95 * total_power
    rolloff_idx <- which(cumulative_power >= rolloff_threshold)[1]
    spectral_rolloff <- if (!is.na(rolloff_idx)) freq_bins[rolloff_idx] else freq_bins[length(freq_bins)]

    # VECTORIZED: Power in frequency bands
    mid_freq <- 0.25
    low_freq_mask <- freq_bins <= mid_freq
    high_freq_mask <- freq_bins > mid_freq
    low_freq_power <- sum(power_spectrum[low_freq_mask])
    high_freq_power <- sum(power_spectrum[high_freq_mask])

    features <- c(
        dominant_frequency = dominant_frequency,
        spectral_centroid = spectral_centroid,
        spectral_rolloff = spectral_rolloff,
        low_freq_power = low_freq_power,
        high_freq_power = high_freq_power
    )

    setNames(features, paste0(feature_prefix, names(features)))
}

# OPTIMIZED: Calculate shape and geometric features (vectorized)
calculate_shape_features <- function(time, pos_x, pos_z) {
    if (length(time) < 3 || length(unique(pos_x)) == 1 || length(unique(pos_z)) == 1) {
        return(setNames(rep(NA_real_, 6), c(
            "path_length", "displacement", "tortuosity", "aspect_ratio",
            "trajectory_area", "convex_hull_area"
        )))
    }

    # VECTORIZED: Path length calculation
    dx <- diff(pos_x)
    dz <- diff(pos_z)
    segment_lengths <- sqrt(dx^2 + dz^2)
    path_length <- sum(segment_lengths, na.rm = TRUE)

    # VECTORIZED: Displacement (straight-line distance)
    n <- length(pos_x)
    displacement <- sqrt((pos_x[n] - pos_x[1])^2 + (pos_z[n] - pos_z[1])^2)

    # Tortuosity (path length / displacement)
    tortuosity <- if (displacement > 0) path_length / displacement else NA_real_

    # VECTORIZED: Range calculations
    pos_x_range <- range(pos_x, na.rm = TRUE)
    pos_z_range <- range(pos_z, na.rm = TRUE)
    x_range <- diff(pos_x_range)
    z_range <- diff(pos_z_range)
    aspect_ratio <- if (z_range > 0) x_range / z_range else NA_real_

    # VECTORIZED: Trajectory area (trapezoidal rule)
    trajectory_area <- if (n > 2) {
        # More efficient vectorized trapezoidal calculation
        pos_z_sum <- pos_z[-n] + pos_z[-1] # pos_z[i] + pos_z[i+1]
        abs(sum(dx * pos_z_sum) / 2)
    } else {
        NA_real_
    }

    # Convex hull area approximation (bounding box)
    convex_hull_area <- x_range * z_range

    c(
        path_length = path_length,
        displacement = displacement,
        tortuosity = tortuosity,
        aspect_ratio = aspect_ratio,
        trajectory_area = trajectory_area,
        convex_hull_area = convex_hull_area
    )
}

# Comprehensive trajectory feature extraction
extract_comprehensive_trajectory_features <- function(time, pos_x, pos_z) {
    if (length(time) < 3) {
        message("[DEBUG] extract_comprehensive_trajectory_features: insufficient data (", length(time), " points)")
        return(data.frame())
    }

    # Normalize time for consistency
    time_norm <- (time - min(time)) / (max(time) - min(time))

    features <- list()

    tryCatch(
        {
            # 1. Kinematic features for x and z
            features <- c(features, calculate_kinematic_features(time, pos_x, "x_"))
            features <- c(features, calculate_kinematic_features(time, pos_z, "z_"))

            # 2. Frequency features for x and z
            features <- c(features, calculate_frequency_features(time_norm, pos_x, "x_"))
            features <- c(features, calculate_frequency_features(time_norm, pos_z, "z_"))

            # 3. Shape and geometric features
            features <- c(features, calculate_shape_features(time, pos_x, pos_z))

            # 4. Statistical features
            statistical_features <- c(
                duration = max(time) - min(time),
                sampling_rate = length(time) / (max(time) - min(time)),
                x_range = diff(range(pos_x, na.rm = TRUE)),
                z_range = diff(range(pos_z, na.rm = TRUE)),
                x_mean = mean(pos_x, na.rm = TRUE),
                z_mean = mean(pos_z, na.rm = TRUE),
                x_std = sd(pos_x, na.rm = TRUE),
                z_std = sd(pos_z, na.rm = TRUE)
            )

            features <- c(features, statistical_features)

            # Convert to data frame - ensure numeric columns
            if (length(features) > 0) {
                # Create a single-row data frame with proper column names
                result <- data.frame(as.list(features), stringsAsFactors = FALSE)
                # Ensure all columns are numeric
                result[] <- lapply(result, function(x) as.numeric(as.character(x)))
                message("[DEBUG] extract_comprehensive_trajectory_features: extracted ", ncol(result), " features")
                return(result)
            } else {
                message("[DEBUG] extract_comprehensive_trajectory_features: no features extracted")
                return(data.frame())
            }
        },
        error = function(e) {
            message("[ERROR] extract_comprehensive_trajectory_features failed: ", e$message)
            return(data.frame())
        }
    )
}

# Extract heel strike trajectory vector (segment from prev to next heel strike)
extract_hs_trajectory <- function(participant, trial, foot, idx, heel_data, foot_data) {
    if (idx <= 1 || idx >= nrow(heel_data)) {
        return(NULL)
    }

    t_prev <- heel_data$time[idx - 1]
    t_curr <- heel_data$time[idx]
    t_next <- heel_data$time[idx + 1]

    # Get trajectory segment from previous to next heel strike
    seg <- foot_data[foot_data$time >= t_prev & foot_data$time <= t_next, ]
    if (nrow(seg) < 5) {
        return(NULL)
    }

    # Extract comprehensive trajectory features
    if (!all(TRAJECTORY_FEATURES %in% names(seg))) {
        return(NULL)
    }

    trajectory_features <- extract_comprehensive_trajectory_features(
        seg$time, seg[[TRAJECTORY_FEATURES[1]]], seg[[TRAJECTORY_FEATURES[2]]]
    )

    if (nrow(trajectory_features) == 0) {
        return(NULL)
    }

    # Combine metadata with features
    result_data <- data.frame(
        participant = participant,
        trialNum = trial,
        foot = foot,
        time = t_curr,
        stringsAsFactors = FALSE
    )

    # Combine with trajectory features
    cbind(result_data, trajectory_features)
}

# Extract step trajectory vector (segment between consecutive heel strikes of same foot)
extract_step_trajectory <- function(participant, trial, foot, start_idx, heel_data, foot_data) {
    if (start_idx >= nrow(heel_data)) {
        return(NULL)
    }

    t_start <- heel_data$time[start_idx]
    t_end <- heel_data$time[start_idx + 1]

    # Get trajectory segment for this step
    seg <- foot_data[foot_data$time >= t_start & foot_data$time <= t_end, ]
    if (nrow(seg) < 5) {
        return(NULL)
    }

    # Extract comprehensive trajectory features
    if (!all(TRAJECTORY_FEATURES %in% names(seg))) {
        message("[DEBUG] extract_step_trajectory: missing trajectory features in seg data")
        return(NULL)
    }

    trajectory_features <- extract_comprehensive_trajectory_features(
        seg$time, seg[[TRAJECTORY_FEATURES[1]]], seg[[TRAJECTORY_FEATURES[2]]]
    )

    if (nrow(trajectory_features) == 0) {
        message("[DEBUG] extract_step_trajectory: comprehensive feature extraction returned empty result")
        return(NULL)
    }

    # Combine metadata with features
    result_data <- data.frame(
        participant = participant,
        trialNum = trial,
        foot = foot,
        time = t_start, # Use step start time for matching
        step_duration = t_end - t_start,
        stringsAsFactors = FALSE
    )

    # Combine with trajectory features
    result <- cbind(result_data, trajectory_features)
    message("[DEBUG] extract_step_trajectory: returning ", nrow(result), " rows, ", ncol(result), " columns")
    return(result)
}

# ========================== PERFORMANCE OPTIMIZATIONS ===================== #

# Pre-allocate and vectorize trajectory extraction
build_trial_hs_trajectories <- function(participant, trial, removed_hs_times = NULL) {
    message("[DEBUG] build_trial_hs_trajectories called for participant: ", participant, ", trial: ", trial)
    message("[DEBUG] removed_hs_times provided: ", if (is.null(removed_hs_times)) "NULL" else paste(nrow(removed_hs_times), "false heel strikes"))

    pre <- get_preprocessed_data(participant, trial, c("leftfoot", "rightfoot"))
    if (any(vapply(pre, nrow, integer(1)) == 0)) {
        message("[DEBUG] No preprocessed data available for participant: ", participant, ", trial: ", trial)
        return(data.frame())
    }

    events <- find_foot_events(participant, trial)
    hs <- events$heelStrikes
    original_hs_count <- nrow(hs)
    message("[DEBUG] Original heel strikes found: ", original_hs_count)

    # Filter out removed heel strikes if provided - VECTORIZED
    if (!is.null(removed_hs_times) && nrow(removed_hs_times) > 0) {
        message("[DEBUG] Filtering out false heel strikes...")
        removal_key <- removed_hs_times %>%
            dplyr::filter(participant == !!participant, trialNum == !!trial)

        message("[DEBUG] False heel strikes to remove for this trial: ", nrow(removal_key))

        if (nrow(removal_key) > 0) {
            hs_before_filter <- nrow(hs)
            # Vectorized time matching using %in% instead of join
            hs_times_round <- round(as.numeric(hs$time), 2)
            removal_times_round <- round(as.numeric(removal_key$time), 2)
            keep_mask <- !hs_times_round %in% removal_times_round
            hs <- hs[keep_mask, ]
            hs_after_filter <- nrow(hs)
            message("[DEBUG] Heel strikes after filtering: ", hs_before_filter, " -> ", hs_after_filter, " (removed: ", hs_before_filter - hs_after_filter, ")")
        }
    } else {
        message("[DEBUG] No false heel strikes to filter out")
    }

    if (nrow(hs) == 0) {
        return(data.frame())
    }

    # Pre-allocate results list with known size
    out <- vector("list", 2)
    names(out) <- c("Left", "Right")

    for (ft in c("Left", "Right")) {
        ft_rows <- hs[hs$foot == ft, ]
        if (nrow(ft_rows) < 3) next # Need at least 3 heel strikes for trajectory

        ftDataName <- if (ft == "Left") "leftfoot" else "rightfoot"
        ftData <- pre[[ftDataName]]

        # Pre-allocate trajectories list
        valid_indices <- 2:(nrow(ft_rows) - 1)
        n_trajectories <- length(valid_indices)
        trajectories <- vector("list", n_trajectories)

        # Vectorized trajectory extraction
        for (i in seq_along(valid_indices)) {
            idx <- valid_indices[i]
            traj <- extract_hs_trajectory(participant, trial, ft, idx, ft_rows, ftData)
            if (!is.null(traj)) {
                trajectories[[i]] <- traj
            }
        }

        # Remove NULL entries efficiently
        trajectories <- trajectories[!vapply(trajectories, is.null, logical(1))]

        if (length(trajectories) > 0) {
            out[[ft]] <- dplyr::bind_rows(trajectories)
        }
    }

    dplyr::bind_rows(out)
}

# Build step trajectory segments for a trial - OPTIMIZED
build_trial_step_trajectories <- function(participant, trial, removed_hs_times = NULL) {
    message("[DEBUG] build_trial_step_trajectories called for participant: ", participant, ", trial: ", trial)
    message("[DEBUG] removed_hs_times provided: ", if (is.null(removed_hs_times)) "NULL" else paste(nrow(removed_hs_times), "false heel strikes"))

    pre <- get_preprocessed_data(participant, trial, c("leftfoot", "rightfoot"))
    if (any(vapply(pre, nrow, integer(1)) == 0)) {
        message("[DEBUG] No preprocessed data available for participant: ", participant, ", trial: ", trial)
        return(data.frame())
    }

    events <- find_foot_events(participant, trial)
    hs <- events$heelStrikes
    original_hs_count <- nrow(hs)
    message("[DEBUG] Original heel strikes found: ", original_hs_count)

    # Filter out removed heel strikes if provided - VECTORIZED
    if (!is.null(removed_hs_times) && nrow(removed_hs_times) > 0) {
        message("[DEBUG] Filtering out false heel strikes...")
        removal_key <- removed_hs_times %>%
            dplyr::filter(participant == !!participant, trialNum == !!trial)

        message("[DEBUG] False heel strikes to remove for this trial: ", nrow(removal_key))

        if (nrow(removal_key) > 0) {
            hs_before_filter <- nrow(hs)
            # Vectorized time matching using %in% instead of join
            hs_times_round <- round(as.numeric(hs$time), 2)
            removal_times_round <- round(as.numeric(removal_key$time), 2)
            keep_mask <- !hs_times_round %in% removal_times_round
            hs <- hs[keep_mask, ]
            hs_after_filter <- nrow(hs)
            message("[DEBUG] Heel strikes after filtering: ", hs_before_filter, " -> ", hs_after_filter, " (removed: ", hs_before_filter - hs_after_filter, ")")
        }
    } else {
        message("[DEBUG] No false heel strikes to filter out")
    }

    if (nrow(hs) == 0) {
        return(data.frame())
    }

    # Pre-allocate results list
    out <- vector("list", 2)
    names(out) <- c("Left", "Right")

    for (ft in c("Left", "Right")) {
        ft_rows <- hs[hs$foot == ft, ]
        if (nrow(ft_rows) < 2) next # Need at least 2 heel strikes for step

        ftDataName <- if (ft == "Left") "leftfoot" else "rightfoot"
        ftData <- pre[[ftDataName]]

        # Pre-allocate trajectories list
        n_steps <- nrow(ft_rows) - 1
        trajectories <- vector("list", n_steps)

        # Vectorized step trajectory extraction
        for (i in seq_len(n_steps)) {
            traj <- extract_step_trajectory(participant, trial, ft, i, ft_rows, ftData)
            if (!is.null(traj)) {
                trajectories[[i]] <- traj
            }
        }

        # Remove NULL entries efficiently
        trajectories <- trajectories[!vapply(trajectories, is.null, logical(1))]

        if (length(trajectories) > 0) {
            out[[ft]] <- dplyr::bind_rows(trajectories)
            message("[DEBUG] Extracted ", nrow(out[[ft]]), " step trajectories for ", ft, " foot")
        } else {
            message("[DEBUG] No step trajectories extracted for ", ft, " foot")
        }
    }

    final_result <- dplyr::bind_rows(out)
    message("[DEBUG] build_trial_step_trajectories final result: ", nrow(final_result), " trajectories")
    return(final_result)
}

# ========================== MODEL CONFIGURATION ============================ #

# Generic model configuration
create_trajectory_model_config <- function(model_type) {
    if (model_type == "false_heelstrike") {
        list(
            extract_func = function(participant, trial, removed_times = NULL) {
                build_trial_hs_trajectories(participant, trial, removed_times)
            },
            time_column = "time",
            exclude_cols = c("participant", "trialNum", "time", "foot", "is_outlier", "step_duration"),
            model_name = "false heel strike trajectory",
            data_type = "false_heelstrikes",
            model_file = file.path(models_dir, "false_heelstrike_trajectory_model.rds"),
            requires_context = FALSE
        )
    } else if (model_type == "outlier") {
        list(
            extract_func = function(participant, trial, removed_times = NULL) {
                build_trial_step_trajectories(participant, trial, removed_times)
            },
            time_column = "time",
            exclude_cols = c("participant", "trialNum", "time", "foot", "is_outlier", "step_duration"),
            model_name = "step outlier trajectory",
            data_type = "outliers",
            model_file = file.path(models_dir, "step_outlier_trajectory_model.rds"),
            requires_context = TRUE
        )
    } else {
        stop("Unknown model type: ", model_type)
    }
}

# ========================== GENERIC TRAINING FUNCTIONS ===================== #

# Generic function to get trials with specific data type
get_trials_with_data <- function(data, data_type) {
    message("[DEBUG] get_trials_with_data called for data_type: ", data_type)
    target_data <- data[[data_type]]

    if (is.null(target_data)) {
        message("[DEBUG] target_data is NULL for data_type: ", data_type)
        return(data.frame(
            participant = character(0),
            trialNum = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    if (nrow(target_data) == 0) {
        message("[DEBUG] target_data has 0 rows for data_type: ", data_type)
        return(data.frame(
            participant = character(0),
            trialNum = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    message("[DEBUG] target_data has ", nrow(target_data), " rows for data_type: ", data_type)
    message("[DEBUG] target_data columns: ", paste(names(target_data), collapse = ", "))

    result <- target_data %>%
        select(participant, trialNum) %>%
        distinct() %>%
        mutate(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum)
        )

    message("[DEBUG] get_trials_with_data returning ", nrow(result), " trials for data_type: ", data_type)
    return(result)
}

# Generic function to standardize data types for joining
standardize_join_columns <- function(df) {
    df %>%
        mutate(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum)
        )
}

# Generic function to create labels using time tolerance
add_labels <- function(df, data, data_type, time_column = "time") {
    target_data <- data[[data_type]]

    if (is.null(target_data) || nrow(target_data) == 0) {
        df$is_outlier <- FALSE
        return(df)
    }

    label_key <- target_data %>%
        mutate(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum),
            time_key = round(as.numeric(time), 2),
            is_outlier = TRUE
        )

    df %>%
        mutate(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum),
            time_key = round(as.numeric(.data[[time_column]]), 2)
        ) %>%
        left_join(
            label_key %>%
                select(participant, trialNum, time_key, is_outlier) %>%
                distinct(), # Remove duplicates to avoid many-to-many
            by = c("participant", "trialNum", "time_key")
        ) %>%
        mutate(is_outlier = ifelse(is.na(is_outlier), FALSE, is_outlier)) %>%
        select(-time_key)
}

# Generic function to exclude specific trials
exclude_trials <- function(df, trials_to_exclude = EXCLUDE_TRIALS) {
    df %>% dplyr::filter(!(trialNum %in% trials_to_exclude))
}

# ========================== INITIALIZATION ================================== #

cat("========================================\n")
cat("TRAJECTORY-BASED FALSE HEEL STRIKE\n")
cat("AND OUTLIER PREDICTOR\n")
cat("========================================\n")
cat("Configuration Settings:\n")
cat("- Force Retrain:", FORCE_RETRAIN, "\n")
cat("- Do Heel Strikes:", DO_HEELSTRIKES, "\n")
cat("- Do Outliers:", DO_OUTLIERS, "\n")
cat("- Trajectory Features:", paste(TRAJECTORY_FEATURES, collapse = ", "), "\n")
cat("- Feature Types: Kinematic, Frequency, Shape, Statistical\n")
cat("- Use PCA:", USE_PCA, "\n")
if (USE_PCA) cat("- PCA Components:", PCA_COMPONENTS, "\n")
cat("- Heel Strike Threshold:", HEELSTRIKE_THRESHOLD, "\n")
cat("- Step Outlier Threshold:", STEP_THRESHOLD, "(lower due to severe class imbalance)\n")
cat("- Training Split:", TRAIN_SPLIT, "\n")
cat("- RF Trees:", RF_NTREE, "\n")
cat("- RF Node Size:", RF_NODESIZE, "\n")
cat("- RF Max Nodes:", RF_MAXNODES, "\n")
cat("- Exclude Trials:", paste(EXCLUDE_TRIALS, collapse = ", "), "\n")
cat("- Parallel Processing:", if (ENABLE_PARALLEL) paste("Enabled (", PARALLEL_CORES, " cores)") else "Disabled", "\n")
cat("========================================\n\n")

# Detect the correct paths based on directory structure
setup_start <- Sys.time()
if (file.exists("data_extra") && file.exists("outlier_prediction")) {
    # We're in the workspace/runtime directory
    runtime_dir <- "."
    data_extra_dir <- "data_extra"
    outlier_prediction_dir <- "outlier_prediction"
} else if (file.exists("runtime") && file.exists(file.path("runtime", "data_extra"))) {
    # We're in parent directory, runtime is a subdirectory
    runtime_dir <- "runtime"
    data_extra_dir <- file.path("runtime", "data_extra")
    outlier_prediction_dir <- file.path("runtime", "outlier_prediction")
} else if (file.exists(file.path("..", "data_extra")) && file.exists(file.path("..", "outlier_prediction"))) {
    # We're in a subdirectory of runtime
    runtime_dir <- ".."
    data_extra_dir <- file.path("..", "data_extra")
    outlier_prediction_dir <- file.path("..", "outlier_prediction")
} else {
    stop("Could not find data_extra and outlier_prediction directories. Please ensure you're running from the correct location.")
}

# Set up file paths
false_heelstrikes_path <- file.path(data_extra_dir, falseHeelStrikesFile)
outliers_path <- file.path(data_extra_dir, outliersFile)

# Create models directory if it doesn't exist
models_dir <- file.path(outlier_prediction_dir, "models")
if (!dir.exists(models_dir)) {
    dir.create(models_dir, recursive = TRUE)
    message("[INFO] Created models directory: ", models_dir)
}

# Training data file (RDS file with all gait parameters) - from results folder
train_files <- c(file.path(runtime_dir, "results", "allGaitParams.rds"))

# Verify the training data file exists to prevent expensive recalculation
if (!file.exists(train_files[1])) {
    stop(
        "ERROR: allGaitParams.rds not found at: ", train_files[1],
        "\nThis file contains pre-computed gait parameters and is required to avoid expensive recalculation.",
        "\nPlease ensure this file exists before running the trajectory model."
    )
}

message("[INFO] ✓ allGaitParams.rds found at: ", train_files[1])
file_size <- file.size(train_files[1]) / (1024^2) # Convert to MB
message("[INFO] ✓ File size: ", round(file_size, 2), " MB")

# Source project code
SCRIPT_DIR <- dirname(normalizePath(sys.frame(1)$ofile %||% "runtime/outlier_prediction"))
RUNTIME_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)

message("[DEBUG] RUNTIME_DIR: ", RUNTIME_DIR)
message("[DEBUG] Sourcing required files...")

# Source all required files
source_files <- c(
    "initialization.R",
    "data_loading.R",
    "pre_processing.R", # This contains get_preprocessed_data!
    "find_foot_events.R",
    "get_data_from_loop.R"
)

for (source_file in source_files) {
    source_path <- file.path(RUNTIME_DIR, "source", source_file)
    message("[DEBUG] Sourcing: ", source_path)

    if (file.exists(source_path)) {
        tryCatch(
            {
                source(source_path)
                message("[DEBUG] ✓ Successfully sourced: ", source_file)
            },
            error = function(e) {
                message("[ERROR] Failed to source ", source_file, ": ", e$message)
            }
        )
    } else {
        message("[ERROR] File not found: ", source_path)
    }
}

# Verify critical functions are available
critical_functions <- c("get_preprocessed_data", "find_foot_events", "get_data_from_loop_parallel")
missing_functions <- c()

for (func_name in critical_functions) {
    if (!exists(func_name)) {
        missing_functions <- c(missing_functions, func_name)
        message("[ERROR] Missing function: ", func_name)
    } else {
        message("[DEBUG] ✓ Function available: ", func_name)
    }
}

if (length(missing_functions) > 0) {
    stop("Critical functions missing: ", paste(missing_functions, collapse = ", "))
}

# Initialize global data environment (required by many source functions)
message("[DEBUG] Initializing global data environment...")

# Set a flag to prevent expensive calculations during initialization
.TRAJECTORY_MODEL_LOADING <<- TRUE

tryCatch(
    {
        if (exists("ensure_global_data_initialized")) {
            ensure_global_data_initialized()
            message("[DEBUG] ✓ Global data environment initialized")
        } else {
            message("[WARN] ensure_global_data_initialized function not found - some functions may fail")
        }
    },
    error = function(e) {
        message("[WARN] Failed to initialize global data environment: ", e$message)
        message("[WARN] Some functions may not work properly")
    }
)

# Clear the flag after initialization
if (exists(".TRAJECTORY_MODEL_LOADING", envir = .GlobalEnv)) {
    rm(.TRAJECTORY_MODEL_LOADING, envir = .GlobalEnv)
}

# Explicitly check that we're not accidentally triggering expensive calculations
message("[DEBUG] Checking for signs of expensive calculation triggers...")
if (exists("calc_all_gait_params")) {
    message("[WARN] calc_all_gait_params function is available - ensuring it's not called accidentally")
}
if (exists("load_or_calculate")) {
    message("[WARN] load_or_calculate function is available - will only use pre-computed allGaitParams.rds")
}

# Required packages
required <- c("dplyr", "tidyr", "randomForest", "data.table", "parallel", "signal", "pracma", "kernlab")
new_pkgs <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) {
    message("[DEBUG] Installing missing packages: ", paste(new_pkgs, collapse = ", "))
    install.packages(new_pkgs, quiet = TRUE)
}
message("[DEBUG] Loading required packages...")
lapply(required, function(pkg) {
    library(pkg, character.only = TRUE)
    message("[DEBUG] ✓ Loaded package: ", pkg)
})

setup_time <- Sys.time()
message("[TIMING] Initialization and setup completed in: ", round(difftime(setup_time, setup_start, units = "secs"), 2), " seconds")

# ========================== DATA LOADING FUNCTIONS ========================= #

# Load false heel strikes and outlier data from CSV files
load_outliers <- function() {
    message("[DEBUG] Looking for data files in: ", normalizePath(data_extra_dir, mustWork = FALSE))
    message("[DEBUG] False heel strikes file: ", normalizePath(false_heelstrikes_path, mustWork = FALSE))
    message("[DEBUG] Outliers file: ", normalizePath(outliers_path, mustWork = FALSE))

    false_heelstrikes <- NULL
    outliers <- NULL

    if (file.exists(false_heelstrikes_path)) {
        message("[INFO] Loading false heel strikes from: ", false_heelstrikes_path)
        false_heelstrikes <- read.csv(false_heelstrikes_path, stringsAsFactors = FALSE)
        message("[DEBUG] False heel strikes: ", nrow(false_heelstrikes), " records")
        false_heelstrikes$participant <- as.character(false_heelstrikes$participant)
        false_heelstrikes$trialNum <- as.numeric(false_heelstrikes$trialNum)
        false_heelstrikes$time <- as.numeric(false_heelstrikes$time)
    } else {
        message("[WARN] False heel strikes file not found: ", false_heelstrikes_path)
    }

    if (file.exists(outliers_path)) {
        message("[INFO] Loading outliers from: ", outliers_path)
        outliers <- read.csv(outliers_path, stringsAsFactors = FALSE)
        message("[DEBUG] Outliers: ", nrow(outliers), " records")
        outliers$participant <- as.character(outliers$participant)
        outliers$trialNum <- as.numeric(outliers$trialNum)
        outliers$time <- as.numeric(outliers$time)
    } else {
        message("[WARN] Outliers file not found: ", outliers_path)
    }

    list(false_heelstrikes = false_heelstrikes, outliers = outliers)
}



# ========================== PARALLEL PROCESSING HELPERS ==================== #

# Helper function to setup parallel workers (eliminates code duplication)
setup_parallel_workers <- function(cl) {
    # Export the entire global environment to ensure all objects are available
    message("[DEBUG] Exporting entire global environment to parallel workers...")
    global_vars <- ls(envir = .GlobalEnv)
    message("[DEBUG] Exporting ", length(global_vars), " global variables")
    parallel::clusterExport(cl, global_vars, envir = .GlobalEnv)

    # Load required packages on cluster workers
    message("[DEBUG] Loading packages on parallel workers...")
    parallel::clusterEvalQ(cl, {
        # Load all required packages
        required_packages <- c("dplyr", "tidyr", "randomForest", "data.table", "signal", "pracma", "kernlab")
        for (pkg in required_packages) {
            tryCatch(
                {
                    library(pkg, character.only = TRUE)
                },
                error = function(e) {
                    message("[WARN] Worker failed to load package ", pkg, ": ", e$message)
                }
            )
        }
    })

    # Source required files on each worker
    message("[DEBUG] Sourcing files on parallel workers...")
    parallel::clusterEvalQ(cl, {
        # Source all required files on each worker
        source_files <- c(
            "initialization.R",
            "data_loading.R",
            "pre_processing.R",
            "find_foot_events.R",
            "get_data_from_loop.R"
        )

        for (source_file in source_files) {
            source_path <- file.path(RUNTIME_DIR, "source", source_file)
            if (file.exists(source_path)) {
                tryCatch(
                    {
                        source(source_path)
                    },
                    error = function(e) {
                        message("[WARN] Worker failed to source ", source_file, ": ", e$message)
                    }
                )
            }
        }

        # Initialize global data environment on each worker
        tryCatch(
            {
                if (exists("ensure_global_data_initialized")) {
                    ensure_global_data_initialized()
                }
            },
            error = function(e) {
                message("[WARN] Worker failed to initialize global data: ", e$message)
            }
        )
    })
}

# Helper function to create extraction wrapper (eliminates code duplication)
create_extraction_wrapper <- function(config, context_data, operation_type = "training") {
    function(trial_row) {
        participant <- trial_row$participant
        trial <- trial_row$trialNum

        message(
            "[DEBUG] ", if (operation_type == "training") "Processing" else "Predicting",
            " trial ", participant, " trial ", trial, " (parallel)"
        )
        message("[DEBUG] config$requires_context: ", config$requires_context)
        message("[DEBUG] context_data available: ", if (is.null(context_data)) "NULL" else paste(nrow(context_data), "entries"))

        tryCatch(
            {
                if (config$requires_context) {
                    result <- config$extract_func(participant, trial, context_data)
                } else {
                    result <- config$extract_func(participant, trial)
                }

                if (!is.null(result) && nrow(result) > 0) {
                    return(result)
                } else {
                    return(NULL)
                }
            },
            error = function(e) {
                message(
                    "[WARN] Error ", if (operation_type == "training") "processing" else "extracting prediction data for",
                    " ", participant, " trial ", trial, ": ", e$message
                )
                return(NULL)
            }
        )
    }
}

# ========================== TRAINING DATA CREATION ========================= #

# Create trajectory training data
create_trajectory_training_data <- function(config, data, context_data = NULL) {
    training_data_start <- Sys.time()
    message("[DEBUG] Creating ", config$model_name, " training data...")

    # Get trials with data
    relevant_trials <- get_trials_with_data(data, config$data_type)

    if (nrow(relevant_trials) == 0) {
        message("[WARN] No trials found with ", config$data_type, " data")
        return(NULL)
    }

    message("[INFO] Found ", nrow(relevant_trials), " trials with ", config$data_type, " data")

    # Create extraction wrapper
    extract_wrapper <- create_extraction_wrapper(config, context_data, "training")

    # Start trajectory extraction timing
    trajectory_extraction_start <- Sys.time()

    if (ENABLE_PARALLEL && nrow(relevant_trials) > 1) {
        message("[INFO] Processing ", nrow(relevant_trials), " trials in parallel using ", PARALLEL_CORES, " cores")

        # Create cluster
        cl <- parallel::makeCluster(PARALLEL_CORES)

        # Setup parallel workers
        setup_parallel_workers(cl)

        # Create trial list for parallel processing
        trial_list <- split(relevant_trials, seq_len(nrow(relevant_trials)))

        # Apply extraction function to each trial in parallel
        message("[DEBUG] Starting parallel trajectory extraction...")
        trajectory_dfs <- parallel::parLapply(cl, trial_list, extract_wrapper)

        # Stop cluster
        parallel::stopCluster(cl)

        # OPTIMIZED: Pre-allocate and filter NULL results
        valid_results <- !vapply(trajectory_dfs, is.null, logical(1))
        trajectory_dfs <- trajectory_dfs[valid_results]

        train_df <- dplyr::bind_rows(trajectory_dfs)

        message("[INFO] ✓ Parallel processing completed")
    } else {
        # Sequential processing fallback
        message("[INFO] Processing ", nrow(relevant_trials), " trials sequentially")

        # Pre-allocate trajectory list
        n_trials <- nrow(relevant_trials)
        trajectory_dfs <- vector("list", n_trials)

        # OPTIMIZED: Vectorized trial processing setup
        participants <- relevant_trials$participant
        trial_nums <- relevant_trials$trialNum

        for (i in seq_len(n_trials)) {
            participant <- participants[i]
            trial <- trial_nums[i]

            message("[DEBUG] Processing trial ", i, "/", n_trials, ": ", participant, " trial ", trial)
            message("[DEBUG] config$requires_context: ", config$requires_context)
            message("[DEBUG] context_data available: ", if (is.null(context_data)) "NULL" else paste(nrow(context_data), "entries"))

            tryCatch(
                {
                    if (config$requires_context) {
                        result <- config$extract_func(participant, trial, context_data)
                    } else {
                        result <- config$extract_func(participant, trial)
                    }

                    if (!is.null(result) && nrow(result) > 0) {
                        trajectory_dfs[[i]] <- result
                    }
                },
                error = function(e) {
                    message("[WARN] Error processing ", participant, " trial ", trial, ": ", e$message)
                }
            )
        }

        # OPTIMIZED: Remove NULL entries efficiently
        valid_results <- !vapply(trajectory_dfs, is.null, logical(1))
        trajectory_dfs <- trajectory_dfs[valid_results]

        # Combine results
        train_df <- dplyr::bind_rows(trajectory_dfs)
    }

    trajectory_extraction_time <- Sys.time()
    message("[TIMING] Trajectory extraction completed in: ", round(difftime(trajectory_extraction_time, trajectory_extraction_start, units = "secs"), 2), " seconds")

    if (is.null(train_df) || nrow(train_df) == 0) {
        message("[WARN] No ", config$model_name, " trajectory data extracted")
        return(NULL)
    }

    message("[DEBUG] Extracted ", nrow(train_df), " ", config$model_name, " trajectories")

    # Add labels
    labeling_start <- Sys.time()
    train_df <- add_labels(train_df, data, config$data_type, config$time_column)
    labeling_time <- Sys.time()
    message("[TIMING] Labeling completed in: ", round(difftime(labeling_time, labeling_start, units = "secs"), 2), " seconds")

    # Exclude specific trials
    exclude_start <- Sys.time()
    train_df <- exclude_trials(train_df)
    exclude_time <- Sys.time()
    message("[TIMING] Trial exclusion completed in: ", round(difftime(exclude_time, exclude_start, units = "secs"), 2), " seconds")

    message("[DEBUG] Final ", config$model_name, " training data: ", nrow(train_df), " trajectories")
    message("[DEBUG] Label distribution: ", paste(names(table(train_df$is_outlier)), "=", table(train_df$is_outlier), collapse = ", "))

    training_data_time <- Sys.time()
    message("[TIMING] Total ", config$model_name, " training data creation completed in: ", round(difftime(training_data_time, training_data_start, units = "secs"), 2), " seconds")

    train_df
}

# ========================== MODEL TRAINING ================================== #

# ======================== HELPER: ONE-CLASS SVM TRAINING =================== #
train_oneclass_svm_model <- function(train_df, config) {
    model_training_start <- Sys.time()
    message("[DEBUG] Starting one-class SVM training for ", config$model_name, " ...")

    if (is.null(train_df) || nrow(train_df) == 0) {
        message("[ERROR] Empty training data for one-class SVM")
        return(NULL)
    }

    # Keep only the positive (is_outlier == TRUE) trajectories
    pos_df <- train_df %>% dplyr::filter(is_outlier)
    if (nrow(pos_df) < 10) {
        message("[ERROR] Too few positive outlier trajectories (", nrow(pos_df), ") for one-class SVM")
        return(NULL)
    }

    # Feature columns – numeric only, excluding meta columns
    feature_cols <- setdiff(names(pos_df), config$exclude_cols)
    numeric_features <- feature_cols[sapply(pos_df[feature_cols], is.numeric)]
    if (length(numeric_features) == 0) {
        message("[ERROR] No numeric features for one-class SVM training")
        return(NULL)
    }

    x_mat <- as.matrix(pos_df[, numeric_features, drop = FALSE])

    # Train one-class SVM (kernlab)
    svm_kernel <- if (SVM_KERNEL == "rbfdot") {
        if (is.null(SVM_SIGMA)) kernlab::rbfdot() else kernlab::rbfdot(sigma = SVM_SIGMA)
    } else {
        SVM_KERNEL
    }

    svm_model <- kernlab::ksvm(x = x_mat, type = "one-svc", kernel = svm_kernel, nu = SVM_NU)

    # Calculate decision scores on training data to establish thresholds
    training_scores <- kernlab::predict(svm_model, x_mat, type = "decision")
    score_quantiles <- quantile(training_scores, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

    # Use a conservative threshold - only accept very high scoring trajectories
    # Use 75th percentile of training outlier scores as minimum threshold
    conservative_threshold <- score_quantiles[["75%"]]

    message("[DEBUG] Training outlier decision score distribution:")
    message("[DEBUG]   10%: ", round(score_quantiles[["10%"]], 4))
    message("[DEBUG]   25%: ", round(score_quantiles[["25%"]], 4))
    message("[DEBUG]   50%: ", round(score_quantiles[["50%"]], 4))
    message("[DEBUG]   75%: ", round(score_quantiles[["75%"]], 4))
    message("[DEBUG]   90%: ", round(score_quantiles[["90%"]], 4))
    message("[DEBUG] Conservative threshold (75th percentile): ", round(conservative_threshold, 4))

    model_training_time <- Sys.time()
    message("[INFO] ✓ one-class SVM model trained (", nrow(pos_df), " outlier trajectories)")
    message("[TIMING] One-class SVM training completed in: ", round(difftime(model_training_time, model_training_start, units = "secs"), 2), " seconds")

    list(
        model = svm_model,
        feature_cols = numeric_features,
        raw_trajectory_cols = numeric_features,
        use_oneclass = TRUE,
        config = config,
        training_size = nrow(pos_df),
        conservative_threshold = conservative_threshold,
        training_score_quantiles = score_quantiles
    )
}

# Train trajectory-based outlier model
train_trajectory_model <- function(train_df, config) {
    # If config$data_type == "outliers" we now use one-class SVM
    if (config$data_type == "outliers") {
        return(train_oneclass_svm_model(train_df, config))
    }

    # ------------------------------------ existing RF training path for heelstrike
    model_training_start <- Sys.time()
    message("[DEBUG] Starting ", config$model_name, " model training...")

    if (is.null(train_df) || nrow(train_df) == 0) {
        message("[ERROR] No training data provided for ", config$model_name, " model")
        return(NULL)
    }

    # Remove rows with missing values
    data_cleaning_start <- Sys.time()
    original_rows <- nrow(train_df)
    train_df <- train_df %>% drop_na()
    data_cleaning_time <- Sys.time()
    message("[DEBUG] Data cleaning: ", original_rows, " -> ", nrow(train_df), " rows")
    message("[TIMING] Data cleaning completed in: ", round(difftime(data_cleaning_time, data_cleaning_start, units = "secs"), 2), " seconds")

    if (nrow(train_df) == 0) {
        message("[WARN] No ", config$model_name, " training data after removing NAs")
        return(NULL)
    }

    # Convert outcome to factor
    train_df$is_outlier <- factor(train_df$is_outlier)

    # Check if we have both classes
    class_levels <- levels(train_df$is_outlier)
    message("[DEBUG] Class levels: ", paste(class_levels, collapse = ", "))

    if (length(class_levels) < 2) {
        message("[WARN] Need both positive and negative examples for ", config$model_name, " training")
        return(NULL)
    }

    message("[INFO] ", config$model_name, " training data: ", nrow(train_df), " observations")
    message(
        "[INFO] ", config$model_name, " positive cases: ", sum(train_df$is_outlier == "TRUE"),
        " (", round(100 * mean(train_df$is_outlier == "TRUE"), 1), "%)"
    )

    # Get feature columns - all numeric columns except those in exclude list
    feature_cols <- setdiff(names(train_df), config$exclude_cols)

    message("[DEBUG] Total columns in training data: ", length(names(train_df)))
    message("[DEBUG] Excluded columns: ", paste(config$exclude_cols, collapse = ", "))
    message("[DEBUG] Potential feature columns: ", length(feature_cols))
    message("[DEBUG] First 10 potential features: ", paste(head(feature_cols, 10), collapse = ", "))

    # OPTIMIZED: Vectorized numeric feature detection
    numeric_features <- feature_cols[vapply(train_df[feature_cols], is.numeric, logical(1))]

    message("[DEBUG] Numeric features found: ", length(numeric_features))
    if (length(numeric_features) > 0) {
        message("[DEBUG] First 10 numeric features: ", paste(head(numeric_features, 10), collapse = ", "))
        message("[DEBUG] Column types sample: ")
        # OPTIMIZED: Vectorized column type checking for sample
        sample_cols <- head(feature_cols, 5)
        col_types <- vapply(train_df[sample_cols], function(x) class(x)[1], character(1))
        col_samples <- vapply(train_df[sample_cols], function(x) {
            if (is.numeric(x)) as.character(round(mean(x, na.rm = TRUE), 3)) else as.character(x[1])
        }, character(1))

        for (i in seq_along(sample_cols)) {
            message("[DEBUG]   ", sample_cols[i], ": ", col_types[i], " (sample: ", col_samples[i], ")")
        }
    }

    if (length(numeric_features) == 0) {
        message("[ERROR] No numeric features found for ", config$model_name, " training")
        message("[ERROR] Available column types:")
        # OPTIMIZED: Vectorized error reporting
        error_cols <- head(feature_cols, 10)
        for (col in error_cols) {
            col_type <- class(train_df[[col]])[1]
            message("[ERROR]   ", col, ": ", col_type)
        }
        return(NULL)
    }

    message("[INFO] Found ", length(numeric_features), " comprehensive trajectory features for ", config$model_name, " model")
    message("[DEBUG] Feature categories: kinematic, frequency, shape, statistical")

    # Apply PCA transformation if enabled
    pca_start <- Sys.time()
    pca_model <- NULL
    final_features <- numeric_features
    training_data <- train_df[, numeric_features, drop = FALSE]

    if (USE_PCA) {
        message("[INFO] Applying PCA transformation...")

        # OPTIMIZED: Vectorized complete cases check
        complete_rows <- complete.cases(training_data)
        if (sum(complete_rows) < nrow(training_data)) {
            message("[DEBUG] Removing ", nrow(training_data) - sum(complete_rows), " rows with NAs for PCA")
            training_data <- training_data[complete_rows, ]
            train_df <- train_df[complete_rows, ]
        }

        if (nrow(training_data) < 10) {
            message("[WARN] Too few complete observations for PCA, using raw features")
        } else {
            # Apply PCA
            pca_model <- prcomp(training_data, center = TRUE, scale. = TRUE)

            # Determine number of components to keep
            n_components <- min(PCA_COMPONENTS, ncol(pca_model$x), nrow(training_data) - 1)

            # Transform data
            pca_data <- pca_model$x[, 1:n_components, drop = FALSE]

            # OPTIMIZED: Vectorized variance calculation
            var_explained <- cumsum(pca_model$sdev^2) / sum(pca_model$sdev^2)

            message("[INFO] PCA: ", length(numeric_features), " features → ", n_components, " components")
            message("[INFO] PCA: ", round(var_explained[n_components] * 100, 1), "% variance explained")

            # Update training data and feature names
            training_data <- as.data.frame(pca_data)
            final_features <- colnames(pca_data)
        }
    }
    pca_time <- Sys.time()
    message("[TIMING] PCA transformation completed in: ", round(difftime(pca_time, pca_start, units = "secs"), 2), " seconds")

    message("[INFO] Using ", length(final_features), " final features for ", config$model_name, " model")

    # Calculate mtry
    mtry_val <- max(1, floor(sqrt(length(final_features)) * RF_MTRY_FRACTION))

    # Train Random Forest
    rf_training_start <- Sys.time()
    message("[DEBUG] Training Random Forest with ", RF_NTREE, " trees, mtry=", mtry_val)

    rf <- randomForest::randomForest(
        x = training_data,
        y = train_df$is_outlier,
        ntree = RF_NTREE,
        importance = TRUE,
        mtry = mtry_val,
        nodesize = RF_NODESIZE,
        maxnodes = RF_MAXNODES,
        do.trace = FALSE
    )
    rf_training_time <- Sys.time()
    message("[TIMING] Random Forest training completed in: ", round(difftime(rf_training_time, rf_training_start, units = "secs"), 2), " seconds")

    # ----------------- NEW: Determine optimal probability threshold ---------
    threshold_start <- Sys.time()
    optimal_threshold <- PRED_THRESHOLD # fallback
    tryCatch(
        {
            oob_probs <- if (!is.null(rf$votes)) rf$votes[, "TRUE"] else NULL
            if (!is.null(oob_probs)) {
                truth <- train_df$is_outlier
                roc_obj <- pROC::roc(truth, oob_probs, quiet = TRUE)
                coords_res <- pROC::coords(roc_obj, "best", best.method = "youden")
                # coords returns a vector; first element is threshold
                optimal_threshold <- as.numeric(coords_res["threshold"] %||% coords_res[1])
                message("[INFO] Optimal threshold (Youden J): ", round(optimal_threshold, 3))
            }
        },
        error = function(e) {
            message("[WARN] Could not compute optimal threshold: ", e$message)
        }
    )
    threshold_time <- Sys.time()
    message("[TIMING] Threshold optimisation completed in: ", round(difftime(threshold_time, threshold_start, units = "secs"), 2), " seconds")
    # -----------------------------------------------------------------------

    model_result <- list(
        model = rf,
        feature_cols = final_features,
        raw_trajectory_cols = numeric_features,
        pca_model = pca_model,
        use_pca = USE_PCA,
        config = config,
        training_size = nrow(train_df),
        class_distribution = table(train_df$is_outlier),
        optimal_threshold = optimal_threshold
    )

    model_training_time <- Sys.time()
    message("[INFO] ✓ ", config$model_name, " model training completed")
    message("[TIMING] Total ", config$model_name, " model training completed in: ", round(difftime(model_training_time, model_training_start, units = "secs"), 2), " seconds")

    model_result
}

# Train or load model
train_or_load_trajectory_model <- function(config, train_df = NULL) {
    # Check if model exists and we don't want to force retrain
    model_file_normalized <- normalizePath(config$model_file, mustWork = FALSE)
    message("[DEBUG] Checking for existing model at: ", model_file_normalized)
    message("[DEBUG] FORCE_RETRAIN: ", FORCE_RETRAIN)
    message("[DEBUG] File exists: ", file.exists(config$model_file))

    if (!FORCE_RETRAIN && file.exists(config$model_file)) {
        message("[INFO] Loading existing ", config$model_name, " model from: ", config$model_file)
        model_result <- readRDS(config$model_file)
        message("[INFO] ✓ ", config$model_name, " model loaded successfully")
        return(model_result)
    }

    message("[INFO] Training new ", config$model_name, " model...")

    # Train new model
    model_result <- train_trajectory_model(train_df, config)

    if (!is.null(model_result)) {
        # Ensure the directory exists before saving
        model_dir <- dirname(config$model_file)
        if (!dir.exists(model_dir)) {
            dir.create(model_dir, recursive = TRUE)
            message("[INFO] Created model directory: ", model_dir)
        }

        saveRDS(model_result, config$model_file)
        message("[INFO] ", config$model_name, " model saved to: ", config$model_file)

        # Training summary
        message("[SUMMARY] ", toupper(gsub("_", " ", model_type)), " MODEL TRAINING:")
        message("  • Training trajectories: ", model_result$training_size)
        message("  • Features used: ", length(model_result$feature_cols))
        if (!is.null(model_result$class_distribution)) {
            pos_cases <- model_result$class_distribution["TRUE"]
            neg_cases <- model_result$class_distribution["FALSE"]
            pos_pct <- round(100 * pos_cases / (pos_cases + neg_cases), 1)
            message("  • Positive cases: ", pos_cases, " (", pos_pct, "%)")
            message("  • Negative cases: ", neg_cases, " (", 100 - pos_pct, "%)")
        }
    } else {
        message("[ERROR] ", config$model_name, " model training failed")
    }

    model_result
}

# ========================== PREDICTION FUNCTIONS =========================== #

# Predict using trajectory model
predict_trajectories <- function(model_result, config, data, context_data = NULL, threshold = NULL) {
    prediction_start_time <- Sys.time()

    if (is.null(model_result)) {
        message("[WARN] No ", config$model_name, " model available for prediction")
        return(data.frame())
    }

    # Use model's optimal threshold if none supplied
    if (is.null(threshold)) {
        if (!is.null(model_result$optimal_threshold)) {
            threshold <- model_result$optimal_threshold
        } else if (config$data_type == "outliers") {
            threshold <- STEP_THRESHOLD
        } else {
            threshold <- HEELSTRIKE_THRESHOLD
        }
        message("[DEBUG] Using threshold: ", round(threshold, 3))
    }

    message("[INFO] Predicting ", config$model_name, "...")

    # Get all available trials (excluding those with existing data)
    trial_selection_start <- Sys.time()
    raw_data <- readRDS(train_files[1])
    existing_trials <- get_trials_with_data(data, config$data_type)

    all_trials <- raw_data %>%
        select(participant, trialNum) %>%
        distinct() %>%
        standardize_join_columns() %>%
        anti_join(existing_trials, by = c("participant", "trialNum")) %>%
        exclude_trials()
    trial_selection_time <- Sys.time()
    message("[TIMING] Trial selection completed in: ", round(difftime(trial_selection_time, trial_selection_start, units = "secs"), 2), " seconds")

    if (nrow(all_trials) == 0) {
        message("[INFO] No new trials to predict for ", config$model_name)
        return(data.frame())
    }

    message("[DEBUG] Predicting on ", nrow(all_trials), " trials")

    # Extract trajectories for prediction with parallel processing if enabled
    if (ENABLE_PARALLEL && nrow(all_trials) > 1) {
        message("[INFO] Extracting prediction data from ", nrow(all_trials), " trials in parallel using ", PARALLEL_CORES, " cores")

        cl <- parallel::makeCluster(PARALLEL_CORES)
        on.exit(parallel::stopCluster(cl))

        # Setup parallel workers using helper function
        setup_parallel_workers(cl)

        # Create wrapper function for parallel execution
        predict_extract_wrapper <- create_extraction_wrapper(config, context_data, "prediction")

        # Apply extraction function to each trial in parallel
        message("[DEBUG] Running parallel prediction trajectory extraction...")
        trial_list <- split(all_trials, seq(nrow(all_trials)))
        trajectory_dfs <- parallel::parLapply(cl, trial_list, predict_extract_wrapper)

        predict_df <- dplyr::bind_rows(trajectory_dfs)

        message("[INFO] ✓ Parallel prediction extraction completed")
    } else {
        # Sequential processing fallback
        message("[INFO] Extracting prediction data from ", nrow(all_trials), " trials sequentially")

        trajectory_dfs <- list()
        for (i in seq_len(nrow(all_trials))) {
            trial_row <- all_trials[i, ]
            participant <- trial_row$participant
            trial <- trial_row$trialNum

            message("[DEBUG] Extracting prediction data for trial ", i, "/", nrow(all_trials), ": ", participant, " trial ", trial)
            message("[DEBUG] config$requires_context: ", config$requires_context)
            message("[DEBUG] context_data available: ", if (is.null(context_data)) "NULL" else paste(nrow(context_data), "entries"))

            tryCatch(
                {
                    if (config$requires_context) {
                        result <- config$extract_func(participant, trial, context_data)
                    } else {
                        result <- config$extract_func(participant, trial)
                    }

                    if (!is.null(result) && nrow(result) > 0) {
                        trajectory_dfs[[i]] <- result
                    }
                },
                error = function(e) {
                    message("[WARN] Error extracting prediction data for ", participant, " trial ", trial, ": ", e$message)
                }
            )
        }

        predict_df <- dplyr::bind_rows(trajectory_dfs)
    }

    if (is.null(predict_df) || nrow(predict_df) == 0) {
        message("[WARN] No trajectory data for ", config$model_name, " prediction")
        return(data.frame())
    }

    message("[DEBUG] Raw prediction data extracted: ", nrow(predict_df), " rows, ", ncol(predict_df), " columns")
    message("[DEBUG] First few column names: ", paste(head(names(predict_df), 15), collapse = ", "), if (ncol(predict_df) > 15) "..." else "")

    # Clean prediction data
    predict_df <- predict_df %>% drop_na()

    if (nrow(predict_df) == 0) {
        message("[WARN] No complete trajectory data for ", config$model_name, " prediction")
        return(data.frame())
    }

    message("[DEBUG] After cleaning: ", nrow(predict_df), " rows remain")

    # Prepare features for prediction
    raw_trajectory_cols <- model_result$raw_trajectory_cols
    available_raw_features <- intersect(raw_trajectory_cols, names(predict_df))

    message("[DEBUG] Model expects ", length(raw_trajectory_cols), " features")
    message("[DEBUG] Prediction data has ", ncol(predict_df), " columns: ", paste(head(names(predict_df), 10), collapse = ", "), if (ncol(predict_df) > 10) "..." else "")
    message("[DEBUG] Available raw features: ", length(available_raw_features))
    message("[DEBUG] Missing features: ", length(raw_trajectory_cols) - length(available_raw_features))

    if (length(available_raw_features) == 0) {
        message("[DEBUG] Expected features: ", paste(head(raw_trajectory_cols, 10), collapse = ", "), if (length(raw_trajectory_cols) > 10) "..." else "")
        message("[ERROR] No raw trajectory features available for ", config$model_name, " prediction")
        return(data.frame())
    }

    if (length(available_raw_features) < length(raw_trajectory_cols)) {
        message(
            "[WARN] Missing some trajectory features: expected ", length(raw_trajectory_cols),
            ", found ", length(available_raw_features)
        )
    }

    # Extract raw trajectory data for prediction
    prediction_data <- predict_df[, available_raw_features, drop = FALSE]

    # Apply PCA transformation if model used PCA
    if (!is.null(model_result$use_pca) && model_result$use_pca && !is.null(model_result$pca_model)) {
        message("[DEBUG] Applying PCA transformation to prediction data...")

        # Ensure we have complete cases for PCA
        complete_rows <- complete.cases(prediction_data)
        if (sum(complete_rows) < nrow(prediction_data)) {
            message("[DEBUG] Removing ", nrow(prediction_data) - sum(complete_rows), " incomplete rows for PCA prediction")
            prediction_data <- prediction_data[complete_rows, ]
            predict_df <- predict_df[complete_rows, ]
        }

        if (nrow(prediction_data) == 0) {
            message("[WARN] No complete cases for PCA prediction")
            return(data.frame())
        }

        # Apply PCA transformation
        pca_pred <- predict(model_result$pca_model, prediction_data)
        n_components <- ncol(model_result$pca_model$x)
        pca_components <- min(n_components, ncol(pca_pred))

        prediction_data <- as.data.frame(pca_pred[, 1:pca_components, drop = FALSE])

        # Ensure column names match training features
        final_features <- intersect(model_result$feature_cols, names(prediction_data))
        if (length(final_features) == 0) {
            message("[ERROR] No PCA features match training features for ", config$model_name, " prediction")
            return(data.frame())
        }

        prediction_data <- prediction_data[, final_features, drop = FALSE]
    } else {
        # Use raw features directly
        final_features <- intersect(model_result$feature_cols, names(prediction_data))
        if (length(final_features) == 0) {
            message("[ERROR] No features match training features for ", config$model_name, " prediction")
            return(data.frame())
        }

        prediction_data <- prediction_data[, final_features, drop = FALSE]
    }

    message("[DEBUG] Using ", ncol(prediction_data), " features for ", config$model_name, " prediction")

    # Make predictions
    if (!is.null(model_result$use_oneclass) && model_result$use_oneclass) {
        # One-class SVM: Use decision scores for more precise control
        svm_pred <- predict(model_result$model, as.matrix(prediction_data), type = "response")
        svm_scores <- predict(model_result$model, as.matrix(prediction_data), type = "decision")

        # DEBUG: Check prediction types and values
        message("[DEBUG] SVM prediction type: ", class(svm_pred))
        message("[DEBUG] SVM prediction unique values: ", paste(unique(svm_pred), collapse = ", "))
        message("[DEBUG] SVM scores type: ", class(svm_scores))
        message("[DEBUG] SVM scores length: ", length(svm_scores))

        # Apply multiple constraints for conservative outlier detection:
        # 1. SVM must classify as in-class (+1)
        # 2. Decision score must exceed conservative threshold (75th percentile of training outliers)
        # 3. Apply target ratio constraint to prevent too many predictions

        conservative_threshold <- model_result$conservative_threshold

        # Safety check for threshold
        if (is.null(conservative_threshold) || !is.numeric(conservative_threshold)) {
            message("[WARN] Invalid conservative threshold: ", conservative_threshold, " - using fallback")
            conservative_threshold <- quantile(svm_scores, 0.75, na.rm = TRUE)
            message("[DEBUG] Fallback threshold (75th percentile): ", round(conservative_threshold, 4))
        }

        basic_outliers <- (svm_pred == 1) & (svm_scores >= conservative_threshold)
        n_basic_outliers <- sum(basic_outliers, na.rm = TRUE)

        # Calculate target number based on scan size and target ratio
        target_max_outliers <- ceiling(nrow(prediction_data) * TARGET_OUTLIER_RATIO)

        message("[DEBUG] ", config$model_name, " SVM decision analysis:")
        message("[DEBUG]   Total trajectories scanned: ", nrow(prediction_data))
        message("[DEBUG]   SVM in-class (+1): ", sum(svm_pred == 1, na.rm = TRUE))
        message("[DEBUG]   SVM out-of-class (-1): ", sum(svm_pred == -1, na.rm = TRUE))
        message("[DEBUG]   Decision score range: ", round(min(svm_scores, na.rm = TRUE), 4), " to ", round(max(svm_scores, na.rm = TRUE), 4))
        message("[DEBUG]   Conservative threshold: ", round(conservative_threshold, 4))
        message("[DEBUG]   Decision scores above threshold: ", sum(svm_scores >= conservative_threshold, na.rm = TRUE))
        message("[DEBUG]   Basic outliers (in-class + high score): ", n_basic_outliers)
        message("[DEBUG]   Target max outliers (", TARGET_OUTLIER_RATIO * 100, "%): ", target_max_outliers)

        # If no outliers meet the conservative threshold, use adaptive approach
        # For one-class SVM, we expect all predictions to be either +1 (similar) or -1 (different)
        # But if we get unexpected values, fall back to score-based selection
        if (n_basic_outliers == 0) {
            message("[DEBUG]   No outliers found with conservative approach, using adaptive threshold...")

            # Check if we have valid SVM predictions
            in_class_count <- sum(svm_pred == 1, na.rm = TRUE)
            if (in_class_count > 0) {
                message("[DEBUG]   Using in-class (+1) predictions for adaptive threshold")
                in_class_indices <- which(svm_pred == 1)
                in_class_scores <- svm_scores[in_class_indices]
            } else {
                message("[DEBUG]   No in-class predictions, using all scores for adaptive threshold")
                in_class_indices <- seq_len(length(svm_scores))
                in_class_scores <- svm_scores
            }

            # Use top scoring trajectories up to target ratio
            n_adaptive_outliers <- min(target_max_outliers, length(in_class_scores))
            if (n_adaptive_outliers > 0) {
                adaptive_threshold <- sort(in_class_scores, decreasing = TRUE)[n_adaptive_outliers]

                # Apply adaptive threshold
                if (in_class_count > 0) {
                    basic_outliers <- (svm_pred == 1) & (svm_scores >= adaptive_threshold)
                } else {
                    basic_outliers <- svm_scores >= adaptive_threshold
                }

                n_basic_outliers <- sum(basic_outliers, na.rm = TRUE)
                message("[DEBUG]   Adaptive threshold: ", round(adaptive_threshold, 4))
                message("[DEBUG]   Adaptive basic outliers: ", n_basic_outliers)
            }
        }

        if (n_basic_outliers <= target_max_outliers) {
            # We're within target, use all basic outliers
            if (n_basic_outliers > 0) {
                predict_df$pred_outlier <- basic_outliers
                message("[DEBUG]   Using all ", n_basic_outliers, " basic outliers (within target)")
            } else {
                predict_df$pred_outlier <- FALSE
                message("[DEBUG]   No basic outliers found (within target)")
            }
        } else {
            # Too many basic outliers, select top scoring ones
            if (n_basic_outliers > 0) {
                # Get scores for basic outliers and select top ones
                basic_outlier_indices <- which(basic_outliers)
                basic_outlier_scores <- svm_scores[basic_outlier_indices]

                # Select top scoring outliers up to target limit
                top_indices <- basic_outlier_indices[order(basic_outlier_scores, decreasing = TRUE)[1:target_max_outliers]]

                predict_df$pred_outlier <- FALSE
                predict_df$pred_outlier[top_indices] <- TRUE

                message("[DEBUG]   Selected top ", target_max_outliers, " outliers from ", n_basic_outliers, " candidates")
                message("[DEBUG]   Score range of selected: ", round(min(svm_scores[top_indices]), 4), " to ", round(max(svm_scores[top_indices]), 4))
            } else {
                predict_df$pred_outlier <- FALSE
                message("[DEBUG]   No outliers selected (no basic outliers found)")
            }
        }

        # Store decision scores for analysis
        predict_df$pred_outlier_prob <- svm_scores # Store decision scores instead of probabilities

        message("[DEBUG] ", config$model_name, " final outlier predictions: ", sum(predict_df$pred_outlier))
    } else {
        predictions <- predict(model_result$model, prediction_data, type = "prob")
        predict_df$pred_outlier_prob <- predictions[, "TRUE"]
        predict_df$pred_outlier <- predict_df$pred_outlier_prob > threshold

        # DEBUG: Show probability distribution
        prob_summary <- summary(predict_df$pred_outlier_prob)
        message(
            "[DEBUG] ", config$model_name, " prediction probabilities - Min: ", round(prob_summary["Min."], 4),
            ", Median: ", round(prob_summary["Median"], 4),
            ", Mean: ", round(mean(predict_df$pred_outlier_prob), 4),
            ", Max: ", round(prob_summary["Max."], 4)
        )
        message("[DEBUG] ", config$model_name, " predictions above threshold (", threshold, "): ", sum(predict_df$pred_outlier))
    }

    # Return predicted outliers
    predicted_outliers <- predict_df %>%
        dplyr::filter(pred_outlier) %>%
        select(participant, trialNum, time = all_of(config$time_column), pred_outlier_prob)

    message("[INFO] Predicted ", nrow(predicted_outliers), " ", config$model_name, " cases")

    prediction_end_time <- Sys.time()
    total_prediction_time <- difftime(prediction_end_time, prediction_start_time, units = "secs")
    message("[TIMING] Total ", config$model_name, " prediction completed in: ", round(total_prediction_time, 2), " seconds")

    predicted_outliers
}

# ========================== FILE OUTPUT FUNCTIONS ========================== #

# Create updated files
create_updated_files <- function(existing_data, new_false_heelstrike_predictions, new_outlier_predictions = NULL) {
    # Update false heel strikes
    updated_false_heelstrikes <- existing_data$false_heelstrikes

    if (nrow(new_false_heelstrike_predictions) > 0) {
        new_false_heelstrikes <- new_false_heelstrike_predictions %>%
            select(participant, trialNum, time) %>%
            mutate(
                participant = as.character(participant),
                trialNum = as.numeric(trialNum),
                time = as.numeric(time)
            )

        updated_false_heelstrikes <- bind_rows(updated_false_heelstrikes, new_false_heelstrikes) %>%
            distinct() %>%
            arrange(participant, trialNum, time)
    }

    # Update outliers
    updated_outliers <- existing_data$outliers

    if (!is.null(new_outlier_predictions) && nrow(new_outlier_predictions) > 0) {
        new_outliers <- new_outlier_predictions %>%
            select(participant, trialNum, time) %>%
            mutate(
                participant = as.character(participant),
                trialNum = as.numeric(trialNum),
                time = as.numeric(time)
            )

        if (is.null(updated_outliers)) {
            updated_outliers <- new_outliers
        } else {
            updated_outliers <- bind_rows(updated_outliers, new_outliers) %>%
                distinct() %>%
                arrange(participant, trialNum, time)
        }
    }

    # Save files with consistent names (overwrite previous results)
    false_heelstrike_file <- file.path(data_extra_dir, falseHeelStrikesTrajFile)
    write.csv(updated_false_heelstrikes, false_heelstrike_file, row.names = FALSE)
    message("[INFO] ✓ Updated false heel strikes saved to: ", false_heelstrike_file)

    outliers_file <- file.path(data_extra_dir, outliersTrajFile)
    if (!is.null(updated_outliers)) {
        write.csv(updated_outliers, outliers_file, row.names = FALSE)
        message("[INFO] ✓ Updated outliers saved to: ", outliers_file)
    } else {
        empty_outliers <- data.frame(participant = character(0), trialNum = numeric(0), time = numeric(0))
        write.csv(empty_outliers, outliers_file, row.names = FALSE)
        message("[INFO] ✓ Empty outliers file created: ", outliers_file)
    }

    # Create summary
    summary_file <- file.path(data_extra_dir, trajectoryPredictionSummaryFile)
    cat("Trajectory-Based Prediction Summary\n", file = summary_file)
    cat("==================================\n", file = summary_file, append = TRUE)
    cat("Timestamp: ", format(Sys.time()), "\n", file = summary_file, append = TRUE)
    cat("Method: Comprehensive trajectory feature extraction", if (USE_PCA) " with PCA" else "", "\n", file = summary_file, append = TRUE)
    cat("Trajectory features: ", paste(TRAJECTORY_FEATURES, collapse = ", "), "\n", file = summary_file, append = TRUE)
    cat("Feature types: Kinematic, Frequency, Shape, Statistical\n", file = summary_file, append = TRUE)
    if (USE_PCA) cat("PCA components: ", PCA_COMPONENTS, "\n", file = summary_file, append = TRUE)
    cat("\nFALSE HEEL STRIKES:\n", file = summary_file, append = TRUE)
    cat("Original: ", nrow(existing_data$false_heelstrikes), "\n", file = summary_file, append = TRUE)
    cat("New predicted: ", nrow(new_false_heelstrike_predictions), "\n", file = summary_file, append = TRUE)
    cat("Total: ", nrow(updated_false_heelstrikes), "\n", file = summary_file, append = TRUE)
    cat("\nOUTLIERS:\n", file = summary_file, append = TRUE)
    original_outliers <- if (is.null(existing_data$outliers)) 0 else nrow(existing_data$outliers)
    new_outliers <- if (is.null(new_outlier_predictions)) 0 else nrow(new_outlier_predictions)
    total_outliers <- if (is.null(updated_outliers)) 0 else nrow(updated_outliers)
    cat("Original: ", original_outliers, "\n", file = summary_file, append = TRUE)
    cat("New predicted: ", new_outliers, "\n", file = summary_file, append = TRUE)
    cat("Total: ", total_outliers, "\n", file = summary_file, append = TRUE)

    message("[INFO] ✓ Summary saved to: ", summary_file)

    list(
        false_heelstrike_file = false_heelstrike_file,
        outliers_file = outliers_file,
        summary_file = summary_file,
        original_false_heelstrike_count = nrow(existing_data$false_heelstrikes),
        predicted_false_heelstrike_count = nrow(new_false_heelstrike_predictions),
        total_false_heelstrike_count = nrow(updated_false_heelstrikes),
        original_outlier_count = original_outliers,
        predicted_outlier_count = new_outliers,
        total_outlier_count = total_outliers
    )
}

# ========================== MAIN EXECUTION FUNCTIONS ====================== #

# Load and validate input data
load_and_validate_data <- function() {
    data_load_start <- Sys.time()
    data <- load_outliers()
    data_load_time <- Sys.time()
    message("[TIMING] Data loaded in: ", round(difftime(data_load_time, data_load_start, units = "secs"), 2), " seconds")

    if (is.null(data$false_heelstrikes)) {
        stop(paste("No false heel strikes found. Please ensure", falseHeelStrikesFile, "exists."))
    }

    message("[INFO] Loaded ", nrow(data$false_heelstrikes), " false heel strikes")
    if (!is.null(data$outliers)) {
        message("[INFO] Loaded ", nrow(data$outliers), " existing outliers")
    }

    return(data)
}

# Generic detection execution function
execute_detection <- function(data, model_type, context_data = NULL) {
    config <- create_trajectory_model_config(model_type)

    # Check if this detection type should be run
    should_run <- switch(model_type,
        "false_heelstrike" = DO_HEELSTRIKES,
        "outlier" = DO_OUTLIERS,
        TRUE
    )

    if (!should_run) {
        message("\n[INFO] SKIPPING ", config$model_name, " detection (disabled in configuration)")
        return(list(
            model = NULL,
            predictions = data.frame(),
            updated_context = if (model_type == "false_heelstrike") data$false_heelstrikes else NULL
        ))
    }

    step_num <- switch(model_type,
        "false_heelstrike" = "1 & 2",
        "outlier" = "3 & 4"
    )
    message("\n[INFO] STEP ", step_num, ": Executing ", config$model_name, " detection...")

    # Check if model exists first to avoid expensive training data creation
    model_file_normalized <- normalizePath(config$model_file, mustWork = FALSE)
    message("[DEBUG] Checking for existing model at: ", model_file_normalized)
    message("[DEBUG] FORCE_RETRAIN: ", FORCE_RETRAIN)
    message("[DEBUG] File exists: ", file.exists(config$model_file))

    model_result <- NULL

    if (!FORCE_RETRAIN && file.exists(config$model_file)) {
        # Load existing model without creating training data
        message("[INFO] Loading existing ", config$model_name, " model from: ", config$model_file)
        model_result <- readRDS(config$model_file)
        message("[INFO] ✓ ", config$model_name, " model loaded successfully")
    } else {
        # Need to train - create training data
        training_start <- Sys.time()
        message("[INFO] Training ", config$model_name, " model...")

        train_df <- create_trajectory_training_data(config, data, context_data)
        model_result <- train_trajectory_model(train_df, config)

        if (!is.null(model_result)) {
            # Ensure the directory exists before saving
            model_dir <- dirname(config$model_file)
            if (!dir.exists(model_dir)) {
                dir.create(model_dir, recursive = TRUE)
                message("[INFO] Created model directory: ", model_dir)
            }

            saveRDS(model_result, config$model_file)
            message("[INFO] ", config$model_name, " model saved to: ", config$model_file)

            # Training summary
            message("[SUMMARY] ", toupper(gsub("_", " ", model_type)), " MODEL TRAINING:")
            message("  • Training trajectories: ", model_result$training_size)
            message("  • Features used: ", length(model_result$feature_cols))
            if (!is.null(model_result$class_distribution)) {
                pos_cases <- model_result$class_distribution["TRUE"]
                neg_cases <- model_result$class_distribution["FALSE"]
                pos_pct <- round(100 * pos_cases / (pos_cases + neg_cases), 1)
                message("  • Positive cases: ", pos_cases, " (", pos_pct, "%)")
                message("  • Negative cases: ", neg_cases, " (", 100 - pos_pct, "%)")
            }
        }

        training_time <- Sys.time()
        message(
            "[TIMING] ", config$model_name, " model training completed in: ",
            round(difftime(training_time, training_start, units = "secs"), 2), " seconds"
        )
    }

    if (is.null(model_result)) {
        message("[ERROR] ", config$model_name, " model loading/training failed")
        if (model_type == "false_heelstrike") {
            stop(config$model_name, " model loading/training failed")
        } else {
            return(list(
                model = NULL,
                predictions = data.frame(),
                updated_context = NULL
            ))
        }
    }

    # Prediction phase
    prediction_start <- Sys.time()
    message("[INFO] Predicting ", config$model_name, "...")

    # Use appropriate threshold based on model type
    prediction_threshold <- switch(model_type,
        "false_heelstrike" = HEELSTRIKE_THRESHOLD,
        "outlier" = STEP_THRESHOLD,
        PRED_THRESHOLD
    )

    # message("[DEBUG] Using prediction threshold: ", prediction_threshold, " for ", config$model_name)
    new_predictions <- predict_trajectories(model_result, config, data, context_data)

    prediction_time <- Sys.time()
    message(
        "[TIMING] ", config$model_name, " prediction completed in: ",
        round(difftime(prediction_time, prediction_start, units = "secs"), 2), " seconds"
    )

    # Summary of prediction results
    training_size <- if (!is.null(model_result$training_size)) model_result$training_size else "unknown"

    # Get total source data count for context
    total_source_count <- tryCatch(
        {
            if (file.exists(train_files[1])) {
                raw_data <- readRDS(train_files[1])
                total_heel_strikes <- raw_data %>%
                    dplyr::select(participant, trialNum) %>%
                    dplyr::distinct() %>%
                    nrow()
                paste(total_heel_strikes, "trials")
            } else {
                "unknown"
            }
        },
        error = function(e) "unknown"
    )

    message("[SUMMARY] ", toupper(gsub("_", " ", model_type)), " PREDICTION RESULTS:")
    message("  • Training data size: ", training_size, " trajectories")
    message("  • New predictions: ", nrow(new_predictions))
    message("  • Source data: ", total_source_count, " in allGaitParams")

    if (nrow(new_predictions) > 0) {
        # Show breakdown by participant if reasonable number
        pred_summary <- new_predictions %>%
            dplyr::group_by(participant) %>%
            dplyr::summarise(count = n(), .groups = "drop") %>%
            dplyr::arrange(desc(count))

        if (nrow(pred_summary) <= 10) {
            message("  • Breakdown by participant:")
            for (i in 1:min(5, nrow(pred_summary))) {
                message("    - ", pred_summary$participant[i], ": ", pred_summary$count[i])
            }
            if (nrow(pred_summary) > 5) {
                message("    - ... and ", nrow(pred_summary) - 5, " more")
            }
        } else {
            message("  • Participants affected: ", nrow(pred_summary))
        }
    }

    # Update context for false heel strikes
    updated_context <- NULL
    if (model_type == "false_heelstrike") {
        context_update_start <- Sys.time()
        updated_context <- data$false_heelstrikes

        if (nrow(new_predictions) > 0) {
            new_entries <- new_predictions %>%
                select(participant, trialNum, time) %>%
                mutate(
                    participant = as.character(participant),
                    trialNum = as.numeric(trialNum),
                    time = as.numeric(time)
                )
            updated_context <- bind_rows(updated_context, new_entries) %>%
                distinct() %>%
                arrange(participant, trialNum, time)
        }

        context_update_time <- Sys.time()
        message(
            "[TIMING] Context update completed in: ",
            round(difftime(context_update_time, context_update_start, units = "secs"), 2), " seconds"
        )
    }

    return(list(
        model = model_result,
        predictions = new_predictions,
        updated_context = updated_context
    ))
}

# Create output files and generate summary report
create_files_and_summary <- function(data, heelstrike_results, outlier_results) {
    # Create updated files (only if at least one process was run)
    result <- NULL

    if (DO_HEELSTRIKES || DO_OUTLIERS) {
        file_creation_start <- Sys.time()
        message("\n[INFO] Creating updated files...")
        result <- create_updated_files(data, heelstrike_results$predictions, outlier_results$predictions)
        file_creation_time <- Sys.time()
        message("[TIMING] Files created in: ", round(difftime(file_creation_time, file_creation_start, units = "secs"), 2), " seconds")
    } else {
        message("\n[INFO] No processes were run - skipping file creation")
    }

    # Summary
    message("\n==== TRAJECTORY-BASED PREDICTION SUMMARY ====")
    message("Configuration:")
    message("- Do Heel Strikes: ", DO_HEELSTRIKES)
    message("- Do Outliers: ", DO_OUTLIERS)

    if (!is.null(result)) {
        message("FALSE HEEL STRIKES:")
        message("- Original: ", result$original_false_heelstrike_count)
        message("- New predicted: ", result$predicted_false_heelstrike_count)
        message("- Total: ", result$total_false_heelstrike_count)
        message("OUTLIERS:")
        message("- Original: ", result$original_outlier_count)
        message("- New predicted: ", result$predicted_outlier_count)
        message("- Total: ", result$total_outlier_count)
        message("Files created:")
        message("- ", basename(result$false_heelstrike_file))
        message("- ", basename(result$outliers_file))
        message("- ", basename(result$summary_file))
    } else {
        message("No files created (both DO_HEELSTRIKES and DO_OUTLIERS were FALSE)")
    }

    return(result)
}

# Clean main execution function
main <- function() {
    script_start_time <- Sys.time()
    message("[TIMING] Trajectory-based false heel strike and outlier detection started at: ", format(script_start_time))

    # Step 1: Load and validate data
    data <- load_and_validate_data()

    # Step 2: Execute false heel strike detection
    heelstrike_results <- execute_detection(data, "false_heelstrike")

    # Step 3: Execute outlier detection
    outlier_results <- execute_detection(data, "outlier", heelstrike_results$updated_context)

    # Step 4: Create files and generate summary report
    result <- create_files_and_summary(data, heelstrike_results, outlier_results)

    # Final timing and completion message
    script_end_time <- Sys.time()
    total_time <- difftime(script_end_time, script_start_time, units = "secs")
    message("\n[TIMING] ===== TOTAL EXECUTION TIME SUMMARY =====")
    message("[TIMING] Script started at: ", format(script_start_time))
    message("[TIMING] Script completed at: ", format(script_end_time))
    message("[TIMING] Total execution time: ", round(total_time, 2), " seconds (", round(total_time / 60, 2), " minutes)")

    message("\n[INFO] Trajectory-based false heel strike and outlier detection complete!")

    invisible(list(
        false_heelstrike_model = heelstrike_results$model,
        outlier_model = outlier_results$model,
        results = result,
        executed_heelstrikes = DO_HEELSTRIKES,
        executed_outliers = DO_OUTLIERS
    ))
}

# Run the main function
if (interactive() || !exists("sourced_script")) {
    main()
}
