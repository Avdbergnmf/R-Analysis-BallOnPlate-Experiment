#' Complexity Calculation Functions
#'
#' Main functions for calculating complexity metrics for participants and trials

#' Calculate complexity metrics for a single participant/trial combination
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param allGaitParams Full gait parameter dataset
#' @param outlier_col_names Vector of potential outlier column names to check
#' @param include_continuous Logical, whether to include continuous simulation data complexity
#' @param continuous_vars Vector of continuous variable names to analyze (e.g., "p", "hipPos", "pelvisPos")
#' @param debug Logical, whether to collect debug information (works with parallel processing)
#' @return Single row data frame with complexity metrics (and debug_messages column if debug=TRUE)
calculate_complexity_single <- function(participant, trial, allGaitParams,
                                        outlier_col_names = c("outlierSteps"),
                                        include_continuous = TRUE,
                                        continuous_vars = c("p", "hipPos", "pelvisPos"),
                                        debug = TRUE) {
  # Initialize debug messages collection for parallel processing compatibility
  debug_messages <- character(0)

  # Create complexity logger with access to debug_messages
  log_msg <- create_module_logger("COMPLEXITY", enabled = debug, messages = debug_messages)

  log_msg("DEBUG", "Processing participant:", participant, "trial:", trial)

  # Filter data for this specific participant/trial
  trial_data <- allGaitParams %>%
    dplyr::filter(participant == !!participant, trialNum == !!trial)

  # Check if gait parameters are missing
  has_gait_data <- nrow(trial_data) > 0

  if (!has_gait_data) {
    log_msg("WARN", "  No gait parameters found for this combination (missing foot tracker data)")
    condition <- NA_character_
  } else {
    # Get condition from trial data
    condition <- unique(trial_data$condition)[1]
  }

  # Initialize result with identifiers
  # Convert trialNum to character to avoid ordered factor issues when combining across participants
  result <- data.frame(
    participant = participant,
    condition = condition,
    trialNum = as.character(trial),
    stringsAsFactors = FALSE
  )

  # Calculate step frequency for adaptive frequency bands
  # Note: stepTimes represent time between consecutive heel strikes (any foot)
  # This gives double-stepping frequency, so we divide by 2 to get proper step frequency
  # If gait data is missing for training trials (7 or 8), try to use the other training trial
  step_freq <- NULL
  step_freq_data <- NULL

  if (has_gait_data && "stepTimes" %in% colnames(trial_data)) {
    step_freq_data <- trial_data
  } else if (!has_gait_data && (trial == "7" || trial == "8")) {
    # For training trials, try fallback to the other training trial for step frequency only
    fallback_trial <- if (trial == "7") "8" else "7"
    fallback_data <- allGaitParams %>%
      dplyr::filter(participant == !!participant, trialNum == !!fallback_trial)

    if (nrow(fallback_data) > 0 && "stepTimes" %in% colnames(fallback_data)) {
      log_msg("WARN", "  Using step frequency from training trial", fallback_trial, "for adaptive frequency bands")
      step_freq_data <- fallback_data
    }
  }

  # Calculate step frequency if we have data
  if (!is.null(step_freq_data)) {
    # Apply outlier filtering to step times
    step_times <- apply_outlier_filtering(step_freq_data$stepTimes, step_freq_data, outlier_col_names)

    # Calculate step frequency (Hz) from median step time (seconds)
    # stepTimes represent time between consecutive heel strikes (any foot), so this gives double-stepping frequency
    # Divide by 2 to get proper step frequency (step cycles)
    step_times <- step_times[is.finite(step_times)]
    if (length(step_times) >= 5) {
      median_step_time <- median(step_times)
      if (is.finite(median_step_time) && median_step_time > 0) {
        raw_step_freq <- 1 / median_step_time
        step_freq <- raw_step_freq / 2 # Divide by 2 to get proper step frequency
        log_msg("INFO", "  Calculated step frequency:", round(step_freq, 3), "Hz (from raw frequency:", round(raw_step_freq, 3), "Hz, median step time:", round(median_step_time, 3), "s)")
      }
    }
  }

  # Store step frequency in results for use by other analyses
  result$step_freq <- if (!is.null(step_freq)) step_freq else NA_real_

  # Process discrete gait complexity types (only if we have gait data)
  if (has_gait_data) {
    complexity_types <- intersect(c("stepTimes", "stepWidths"), colnames(trial_data))

    for (dataType in complexity_types) {
      log_msg("DEBUG", "  Processing discrete gait data:", dataType)

      values <- trial_data[[dataType]]
      original_length <- length(values)
      log_msg("DEBUG", "    Original length:", original_length)

      # Apply outlier filtering
      values <- apply_outlier_filtering(values, trial_data, outlier_col_names, log_msg)

      # Calculate complexity metrics using helper function
      result <- add_discrete_complexity(result, dataType, values, log_msg)
    }
  } else {
    log_msg("INFO", "  Skipping discrete gait metrics (no gait data available)")
  }

  # Compute per-foot foot-placement control residuals (if we have gait data)
  if (has_gait_data) {
    log_msg("DEBUG", "  Computing per-foot foot-placement control residuals")

    # Check if heel-strike data has hip position annotations
    if ("hip_pos_x" %in% colnames(trial_data)) {
      # Compute per-foot position-only metrics
      fp_metrics <- compute_fp_control_metrics_per_foot(trial_data)

      # Add metrics to result
      for (name in names(fp_metrics)) {
        result[[name]] <- fp_metrics[[name]]
      }
    } else {
      log_msg("DEBUG", "  No hip position data available for foot-placement control")
      # Add NA columns for per-foot metrics using centralized function
      na_metrics <- get_na_results_fp_control("both")
      for (name in names(na_metrics)) {
        result[[name]] <- na_metrics[[name]]
      }
    }
  } else {
    # No gait data, add NA columns for per-foot metrics using centralized function
    na_metrics <- get_na_results_fp_control("both")
    for (name in names(na_metrics)) {
      result[[name]] <- na_metrics[[name]]
    }
  }

  # Process continuous data if requested
  if (include_continuous) {
    log_msg("DEBUG", "  Processing continuous data")

    # Define signal configurations: var_name -> (tracker_type, column_name, description)
    signal_configs <- list(
      p = list(tracker = "sim", column = "p", description = "Plate position from simulation"),
      hipPos = list(tracker = "hip", column = "pos_x", description = "Hip position"),
      pelvisPos = list(tracker = "udp", column = "PelvisPos", description = "Pelvis position")
    )

    # Process each requested continuous variable
    for (var_name in continuous_vars) {
      if (var_name %in% names(signal_configs)) {
        config <- signal_configs[[var_name]]
        log_msg("DEBUG", "  Processing", config$description)
        result <- process_continuous_signal(
          participant, trial,
          config$tracker, config$column,
          var_name, config$description,
          result, log_msg, step_freq
        )
      } else {
        log_msg("WARN", "  Unknown continuous variable:", var_name)
      }
    }
  }

  # Add debug messages to result if debug is enabled
  if (debug && length(debug_messages) > 0) {
    result$debug_messages <- list(debug_messages)
  }

  return(result)
}

#' Complexity calculation function designed for get_data_from_loop framework
#' This function signature matches what get_data_from_loop expects
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param ... Additional arguments passed from get_data_from_loop
#' @return Single row data frame with complexity metrics
calc_complexity_for_loop <- function(participant, trial, ...) {
  # Get additional arguments
  args <- list(...)

  # Extract parameters, with defaults
  allGaitParams <- args$allGaitParams
  outlier_col_names <- if (is.null(args$outlier_col_names)) c("outlierSteps") else args$outlier_col_names
  include_continuous <- if (is.null(args$include_continuous)) TRUE else args$include_continuous
  continuous_vars <- if (is.null(args$continuous_vars)) c("p", "hipPos", "pelvisPos") else args$continuous_vars
  debug <- if (is.null(args$debug)) FALSE else args$debug

  if (is.null(allGaitParams)) {
    stop("allGaitParams must be provided in the ... arguments")
  }

  # Use the existing single calculation function
  result <- calculate_complexity_single(
    participant, trial, allGaitParams, outlier_col_names,
    include_continuous, continuous_vars, debug
  )

  return(result)
}

#' Calculate complexity metrics for all participants and trials using get_data_from_loop framework
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @param include_continuous Logical, whether to include continuous simulation data complexity
#' @param continuous_vars Vector of continuous variable names to analyze (e.g., "p", "hipPos", "pelvisPos")
#' @return Data frame with complexity metrics for all valid combinations
#' @note This function requires allGaitParams to be available in the global environment
get_all_complexity_metrics <- function(loop_function, include_continuous = TRUE,
                                       continuous_vars = c("p", "hipPos", "pelvisPos")) {
  # Create complexity logger for this function
  complexity_logger <- create_module_logger("COMPLEXITY")
  
  log_operation_start(complexity_logger, "get_all_complexity_metrics")
  
  # Ensure global parameters and data are initialized
  ensure_global_data_initialized()

  # Check if allGaitParams exists in the global environment
  if (!exists("allGaitParams", envir = .GlobalEnv)) {
    complexity_logger("ERROR", "allGaitParams not found in global environment")
    stop("allGaitParams not found in global environment. Please ensure gait parameters are calculated first.")
  }

  # Get allGaitParams from global environment
  allGaitParams <- allGaitParams
  complexity_logger("DEBUG", "Retrieved allGaitParams from global environment")

  # Use centralized parameters - debug is now supported in parallel mode
  debug <- TRUE # Always enable debug now that parallel debugging is supported
  complexity_logger("DEBUG", "Debug logging enabled for parallel processing")

  # Create a wrapper function that matches get_data_from_loop expectations
  complexity_function <- function(participant, trial) {
    outlier_cols <- c("outlierSteps")
    return(calculate_complexity_single(
      participant, trial, allGaitParams, outlier_cols,
      include_continuous, continuous_vars, debug
    ))
  }

  # Verify only udp data so non-task trials (e.g., 3, 9) are included.
  # Continuous metrics will be attempted opportunistically inside the worker
  # if simulation/task data exist, but are not required to run.
  datasets_to_verify <- c("udp") # , "hip", "sim")
  complexity_logger("DEBUG", "Datasets to verify:", paste(datasets_to_verify, collapse = ", "))

  # Use the provided loop function
  complexity_logger("INFO", "Starting complexity calculation using loop function")
  result <- loop_function(complexity_function, datasets_to_verify = datasets_to_verify)
  complexity_logger("INFO", "Completed complexity calculation, got", nrow(result), "results")

  # Remove debug messages from the final result (they were already output during execution)
  if ("debug_messages" %in% colnames(result)) {
    result$debug_messages <- NULL
    complexity_logger("DEBUG", "Removed debug_messages column from final result")
  }

  log_operation_end(complexity_logger, "get_all_complexity_metrics", success = TRUE)
  return(result)
}
