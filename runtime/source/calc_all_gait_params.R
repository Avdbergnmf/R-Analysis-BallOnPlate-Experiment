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

  n <- nrow(data)
  data$perturbations <- as.factor(rep(perturb_val, n))
  data$visualizations <- as.factor(rep(vis_val, n))
  data$task <- as.factor(rep(task_val, n))
  data$treadmillSpeed <- as.numeric(rep(tread_val, n))
  data$condition <- as.factor(rep(cond_val, n))

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

  # Initial outlier detection that doesn't work so well.
  heelStrikesData$outlierSteps <- detect_outliers_modified_z_scores(stepTimes, threshold = 6) # another option: stepTimes > median(stepTimes) * 2 | stepTimes < median(stepTimes) * 0.5
  currentlyIgnoredSteps <- heelStrikesData$outlierSteps
  heelStrikesData$outlierSteps <- heelStrikesData$outlierSteps | detect_outliers_modified_z_scores(speed, currentlyIgnoredSteps, 6)
  currentlyIgnoredSteps <- currentlyIgnoredSteps | heelStrikesData$outlierSteps
  heelStrikesData$outlierSteps <- heelStrikesData$outlierSteps | detect_outliers_modified_z_scores(stepLengths, currentlyIgnoredSteps, 10)

  # Make a list of all the gait parameters
  gaitParams <- list(
    stepTimes = stepTimes,
    # stanceTimes = stanceTimes, ####### These are not working well atm. Toe-offs are not filtered properly
    # swingStanceRatio = swingTimes / stanceTimes,
    # swingTimes = swingTimes,
    # finalStepWidths = finalStepWidths,
    stepLengths = stepLengths,
    # finalStepLengths = finalStepLengths,
    stepWidths = stepWidths,
    centered_stepLengths = stepLengths - mean(stepLengths),
    centered_stepWidths = stepWidths - mean(stepWidths),
    speed = speed,
    heelStrikes = heelStrikesData,
    toeOffs = toeOffsData,
    relHeelStrikes = relHeelStrikesData,
    diffData = diffData
  )

  # Remove all the first steps, because they are always wrong.
  gaitParams <- lapply(gaitParams, function(x) {
    # Check if the element is a vector or data frame
    if (is.vector(x)) {
      # Remove the first element for vectors
      x[-1]
    } else if (is.data.frame(x)) {
      # Remove the first row for data frames
      x[-1, ]
    } else {
      x # Return the element unchanged if it is not a vector or data frame
    }
  })

  return(gaitParams)
}

#' Calculate gait parameters for all participants and trials
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @return Tibble with gait parameters for all combinations
calc_all_gait_params <- function(loop_function) {
  # Ensure heavy data is initialized in main session
  return(loop_function(calculate_gait_parameters))
}
