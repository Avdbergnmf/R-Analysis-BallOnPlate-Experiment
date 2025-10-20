# =============================================================================
# CALCULATION WRAPPER FUNCTIONS
# =============================================================================
# These functions provide wrapper interfaces for the load_or_calculate() system
# They bridge between the global data loading system and self-contained feature modules

#' Calculate gait parameters for a single participant/trial combination
#' @param participant Participant ID
#' @param trial Trial number
#' @return Tibble with gait parameters
calculate_gait_parameters <- function(participant, trialNum) {
  # Get preprocessed data using utils function (depends on global cache environments)
  preprocessedData <- get_preprocessed_data(participant, trialNum, c("leftfoot", "rightfoot", "hip"))
  
  # Use gait feature module for foot event detection with preprocessed data
  gaitData <- gait$detect_foot_events(participant, trialNum, preprocessedData)

  heelStrikesData <- gaitData$heelStrikes # should already be sorted based on time
  toeOffsData <- gaitData$toeOffs

  relHeelStrikesData <- heelStrikesData %>%
    dplyr::arrange(time) %>% # Ensure data is ordered by time across all heel strikes
    dplyr::mutate(across(where(is.numeric), ~ c(0, diff(.x)))) %>%
    dplyr::ungroup()

  diffData <- gait$add_step_differences(relHeelStrikesData)

  # time-based
  stepTimes <- relHeelStrikesData$time # Calculate step times  >>> NOTE: The first heelstrike is only used as a starting point to the second
  swingTimes <- heelStrikesData$time - toeOffsData$time # Calculate swing times <<< L = N   (not N-1)
  stanceTimes <- stepTimes - swingTimes # Calculate stance times

  # position-based
  stepWidths <- relHeelStrikesData$pos_x # Calculate step width
  stepWidths <- ifelse(relHeelStrikesData$foot == "Left", stepWidths * -1, stepWidths) # Adjust sign based on which foot is stepping
  stepLengths <- relHeelStrikesData$actual_pos_z # Calculate step lengths
  speed <- stepLengths / stepTimes # Calculate speed

  # Note: outlierSteps column should already be populated by gait$detect_foot_events() from CSV data
  # If outlierSteps column doesn't exist, initialize as FALSE
  if (!"outlierSteps" %in% colnames(heelStrikesData)) {
    warning("outlierSteps column not found in heel strikes data. Initializing as FALSE.")
    heelStrikesData$outlierSteps <- FALSE
  }

  # Report outlier statistics for this participant/trial
  num_outliers <- sum(heelStrikesData$outlierSteps, na.rm = TRUE)
  total_steps <- nrow(heelStrikesData)
  if (num_outliers > 0) {
    cat(sprintf(
      "DEBUG: Participant %s Trial %s - %d/%d steps marked as outliers (%.1f%%)\n",
      participant, trialNum, num_outliers, total_steps,
      100 * num_outliers / total_steps
    ))
    flush.console()
  }

  # Remove the first step from all parameters (first step is always wrong)
  stepTimes <- stepTimes[-1]
  stepLengths <- stepLengths[-1]
  stepWidths <- stepWidths[-1]
  speed <- speed[-1]
  heelStrikesData <- heelStrikesData[-1, ]

  # Create the main data frame based on heelStrikes data (which contains suspect column)
  gaitParams <- heelStrikesData

  # Add calculated gait parameters as columns
  gaitParams$stepTimes <- stepTimes
  gaitParams$stepLengths <- stepLengths
  gaitParams$stepWidths <- stepWidths
  gaitParams$centered_stepLengths <- stepLengths - mean(stepLengths, na.rm = TRUE)
  gaitParams$centered_stepWidths <- stepWidths - mean(stepWidths, na.rm = TRUE)
  gaitParams$speed <- speed

  return(gaitParams)
}

#' Calculate gait parameters for all participants and trials
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @return Tibble with gait parameters for all combinations
calc_all_gait_params <- function(loop_function) {
  # Ensure heavy data is initialized in main session
  return(loop_function(calculate_gait_parameters))
}
attr(calc_all_gait_params, "log_label") <- "gait_parameters"

#' Get all questionnaire results using the questionnaire feature module
#' @param loop_function Function to use for processing (not used for questionnaires)
#' @return Combined questionnaire results data frame
get_all_questionnaire_results <- function(loop_function = NULL) {
  # Note: This function doesn't use loop_function as it processes questionnaire data directly
  # The parameter is kept for consistency with other calculation functions
  
  # Use the questionnaire feature module to get results
  questionnaire$get_all_questionnaire_results(
    all_questionnaires = c("IMI", "UserExperience"),
    participants = participants,
    get_q_data_func = get_q_data,
    get_question_info_func = get_question_info,
    get_question_weights_func = get_question_weights,
    qAnswers = qAnswers,
    condition_number_func = condition_number
  )
}
attr(get_all_questionnaire_results, "log_label") <- "questionnaire_results"

#' Calculate task metrics for all participants and trials
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @return Tibble with task metrics for all combinations
get_all_task_metrics <- function(loop_function) {
  simulation_api <- simulation$get_all_task_metrics
  if (!is.function(simulation_api)) {
    stop("Simulation module did not provide get_all_task_metrics(). Ensure the feature loaded correctly.")
  }
  simulation_api(loop_function)
}
attr(get_all_task_metrics, "log_label") <- "task_metrics"
attr(get_all_task_metrics, "preload") <- function() {
  ensure_global_data_initialized()
  simulation$ensure_global_hazard_samples_available()
  invisible(NULL)
}
attr(get_all_task_metrics, "extra_globals") <- c("GLOBAL_HAZARD_SAMPLES_PREDS")

#' Calculate complexity metrics for all participants and trials
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @param include_continuous Logical, whether to include continuous simulation data complexity
#' @param continuous_vars Vector of continuous variable names to analyze (e.g., "p", "hipPos", "pelvisPos")
#' @return Data frame with complexity metrics for all valid combinations
get_all_complexity_metrics <- function(loop_function, include_continuous = TRUE,
                                       continuous_vars = c("p", "hipPos", "pelvisPos")) {
  # Use the complexity feature module
  complexity_api <- complexity$get_all_complexity_metrics
  if (!is.function(complexity_api)) {
    stop("Complexity module did not provide get_all_complexity_metrics(). Ensure the feature loaded correctly.")
  }
  complexity_api(loop_function, include_continuous, continuous_vars)
}
attr(get_all_complexity_metrics, "log_label") <- "complexity_metrics"
attr(get_all_complexity_metrics, "preload") <- function() {
  combinations_df <- NULL
  include_continuous <- TRUE
  continuous_vars <- c("p", "hipPos", "pelvisPos")

  if (exists(".LOAD_OR_CALC_CONTEXT", envir = .GlobalEnv, inherits = FALSE)) {
    ctx <- get(".LOAD_OR_CALC_CONTEXT", envir = .GlobalEnv)
    if (!is.null(ctx$combinations_df)) {
      combinations_df <- ctx$combinations_df
    }
    if (!is.null(ctx$args)) {
      args <- ctx$args
      if (!is.null(args$include_continuous)) {
        include_continuous <- args$include_continuous
      }
      if (!is.null(args$continuous_vars)) {
        continuous_vars <- args$continuous_vars
      }
    }
  }

  preload_complexity_data(combinations_df, include_continuous, continuous_vars)
  invisible(NULL)
}
attr(get_all_complexity_metrics, "extra_globals") <- c(".GLOBAL_TRACKER_CACHE", ".GLOBAL_SIMULATION_CACHE")
#' Load or create UDP trim information for all participant/trial combinations
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @return Data frame with UDP trim metadata
load_or_create_udp_time_trim_info <- function(loop_function) {
  ensure_global_data_initialized()

  if (missing(loop_function) || !is.function(loop_function)) {
    stop("load_or_create_udp_time_trim_info: valid 'loop_function' must be supplied")
  }

  build_row <- function(p, tr, ...) {
    rng <- get_udp_time_ranges(p, tr)
    data.frame(
      participant = p,
      trial = as.numeric(tr),
      has_udp_data = rng$has_udp_data,
      trial_duration = rng$trial_duration,
      total_valid_duration = rng$total_valid_duration,
      num_segments = rng$num_segments,
      valid_ranges = I(list(rng$valid_ranges)),
      stringsAsFactors = FALSE
    )
  }

  loop_function(build_row, datasets_to_verify = c("udp"))
}
attr(load_or_create_udp_time_trim_info, "log_label") <- "udp_trim_info"
