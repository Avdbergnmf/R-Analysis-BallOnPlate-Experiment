####### DEFINITIONS
# Getting types for later use
xOptions <- c("time", "pos_x", "pos_y", "pos_z", "actual_pos_z")
# xOptions2D is now loaded in initialization.R to avoid heavy operations during sourcing
# xOptions2D <- colnames(get_t_data(participants[1], "leftfoot", 1)) # options for pos rot trackers
categories <- c("participant", "condition", "trialNum") #  "heelStrikes.foot"
# categoriesInputs <- append(categories, "None")
columns_to_not_summarize <- c("visualizations", "perturbations") # these are categorical columns we may want to use for our statistics but we dont want to summarize in our mu table
categoriesExtra <- c(categories, columns_to_not_summarize)
categoriesExtraInputs <- append(categoriesExtra, c("heelStrikes.foot", "slice_index", "None"))

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
  trial <- as.numeric(as.character(data$trialNum))
  participant <- as.character(data$participant)

  # Add categorical columns
  data$perturbations <- as.factor(mapply(has_perturbations, participant, trial))
  data$visualizations <- as.factor(mapply(has_visualizations, participant, trial))
  data$task <- as.factor(mapply(has_task, participant, trial))
  data$treadmillSpeed <- as.numeric(mapply(get_move_speed, participant, trial))

  data$condition <- as.factor(sapply(participant, condition_number))

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

  relHeelStrikesData <- gaitData$heelStrikes
  # Apply diff and padding to each numeric column
  relHeelStrikesData[] <- lapply(relHeelStrikesData, function(column) {
    if (is.numeric(column)) {
      # Calculate differences and pad with a leading zero
      c(0, diff(column))
    } else {
      column # Return non-numeric columns unchanged
    }
  })

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
#' @param parallel Whether to use parallel processing (default TRUE)
#' @return Tibble with gait parameters for all combinations
calc_all_gait_params <- function(parallel = TRUE) {
  # Ensure heavy data is initialized in main session
  ensure_global_data_initialized()
  if (parallel) {
    return(get_data_from_loop_parallel(calculate_gait_parameters))
  } else {
    return(get_data_from_loop(calculate_gait_parameters))
  }
}
