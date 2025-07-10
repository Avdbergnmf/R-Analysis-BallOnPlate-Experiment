####### PARAMETER INITIALIZATION
# All parameters are now defined in initialization.R for centralized configuration
# This includes: xOptions, categories, columns_to_not_summarize, categoriesExtra, categoriesExtraInputs, xOptions2D

add_identifiers_and_categories <- function(data, participant, trial) {
  data <- add_identifiers(data, participant, trial)
  data <- add_category_columns(data)
  return(data)
}

add_identifiers <- function(data, participant, trial) {
  data$participant <- as.factor(participant)
  data$trialNum <- as.ordered(trial)
  return(data)
}

add_category_columns <- function(data) {
  # participant and trial constant per data frame
  participant <- as.character(data$participant[1])
  trial <- as.numeric(as.character(data$trialNum[1]))

  perturb_val <- has_perturbations(participant, trial)
  vis_val <- has_visualizations(participant, trial)
  task_val <- has_task(participant, trial)
  tread_val <- get_move_speed(participant, trial)
  cond_val <- condition_number(participant)

  # ---------------------------------------------------------------------
  # Demographic / questionnaire details fetched from p_details.csv
  # ---------------------------------------------------------------------
  gender_val <- get_p_detail(participant, "gender")
  motion_val <- get_p_detail(participant, "motion")
  age_val <- as.numeric(get_p_detail(participant, "age"))
  weight_val <- as.numeric(get_p_detail(participant, "weight"))
  education_val <- get_p_detail(participant, "education")
  vr_exp_val <- get_p_detail(participant, "vr_experience")
  height_scale_val <- as.numeric(get_p_detail(participant, "height_scale"))

  n <- nrow(data)
  data$perturbations <- as.factor(rep(perturb_val, n))
  data$visualizations <- as.factor(rep(vis_val, n))
  data$task <- as.factor(rep(task_val, n))
  data$treadmillSpeed <- as.numeric(rep(tread_val, n))
  data$condition <- as.factor(rep(cond_val, n))

  # Demographic columns (repeat per row)
  data$gender <- as.factor(rep(gender_val, n))
  data$motion <- as.factor(rep(motion_val, n))
  data$age <- rep(age_val, n)
  data$weight <- rep(weight_val, n)
  data$education <- as.factor(rep(education_val, n))
  data$vr_experience <- as.factor(rep(vr_exp_val, n))
  data$height_scale <- rep(height_scale_val, n)

  return(data)
}

#' Calculate gait parameters for a single participant/trial combination
#' @param participant Participant ID
#' @param trial Trial number
#' @param slice_length Optional length of time slices
#' @return Tibble with gait parameters
calculate_gait_parameters <- function(participant, trialNum) {
  gaitData <- find_foot_events(participant, trialNum)

  heelStrikesData <- gaitData$heelStrikes # should already be sorted based on time
  toeOffsData <- gaitData$toeOffs

  relHeelStrikesData <- gaitData$heelStrikes %>%
    dplyr::group_by(foot) %>%
    dplyr::mutate(across(where(is.numeric), ~ c(0, diff(.x)))) %>%
    dplyr::ungroup()

  diffData <- add_diff_per_foot(relHeelStrikesData)

  # time-based
  stepTimes <- relHeelStrikesData$time # Calculate step times  >>> NOTE: The first heelstrike is only used as a starting point to the second
  swingTimes <- heelStrikesData$time - toeOffsData$time # Calculate swing times <<< L = N   (not N-1)
  stanceTimes <- stepTimes - swingTimes # Calculate stance times

  # position-based
  stepWidths <- relHeelStrikesData$pos_x # Calculate step width
  stepWidths <- ifelse(relHeelStrikesData$foot == "Left", stepWidths * -1, stepWidths) # Adjust sign based on which foot is stepping
  stepLengths <- relHeelStrikesData$actual_pos_z # Calculate step lengths
  speed <- stepLengths / stepTimes # Calculate speed

  # Note: outlierSteps column should already be populated by find_foot_events() from CSV data
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
