#' Main Gait Event Detection Function
#'
#' Primary function for detecting foot events with outlier processing

#' Find foot events for a participant and trial
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param preprocessedData List containing preprocessed data (leftfoot, rightfoot, hip)
#' @return List with heelStrikes and toeOffs dataframes
find_foot_events <- function(participant, trialNum, preprocessedData) {
  gait_main_logger <- create_module_logger("GAIT-MAIN")
  gait_main_logger("DEBUG", "Starting find_foot_events for participant:", participant, "trial:", trialNum)
  
  leftfoot <- preprocessedData$leftfoot
  rightfoot <- preprocessedData$rightfoot
  hip <- preprocessedData$hip

  # Detect toe-off and heelstrikes
  gait_main_logger("DEBUG", "Starting left foot detection")
  footEventsLeft <- detect_foot_events_coordinates(leftfoot, hip, rightfoot)
  gait_main_logger("DEBUG", "Left foot detection completed, starting right foot detection")
  footEventsRight <- detect_foot_events_coordinates(rightfoot, hip, leftfoot)
  gait_main_logger("DEBUG", "Right foot detection completed")

  # Add a 'foot' column to each event dataframe
  footEventsLeft$heelStrikes$foot <- "Left"
  footEventsLeft$toeOffs$foot <- "Left"
  footEventsRight$heelStrikes$foot <- "Right"
  footEventsRight$toeOffs$foot <- "Right"

  # Combine heel strikes and foot lifts from both feet
  gait_main_logger("DEBUG", "Combining heel strikes and toe-offs from both feet")
  combinedHeelStrikes <- rbind(footEventsLeft$heelStrikes, footEventsRight$heelStrikes)
  combinedToeOffs <- rbind(footEventsLeft$toeOffs, footEventsRight$toeOffs)

  # Order the events by time
  gait_main_logger("DEBUG", "Ordering events by time")
  combinedHeelStrikes <- combinedHeelStrikes[order(combinedHeelStrikes$time), ]
  combinedToeOffs <- combinedToeOffs[order(combinedToeOffs$time), ]

  # ---------------------------------------------------------------------------
  # Ensure participant / trialNum columns exist for downstream outlier handling
  # The detection routines often return heel-strike data without these columns
  # because they are known implicitly from the function arguments.  The unified
  # `apply_outliers()` helper, however, requires them for reliable joins.
  # ---------------------------------------------------------------------------
  if (!"participant" %in% names(combinedHeelStrikes)) {
    combinedHeelStrikes$participant <- as.character(participant)
  }
  if (!"trialNum" %in% names(combinedHeelStrikes)) {
    combinedHeelStrikes$trialNum <- as.numeric(trialNum)
  }

  # propagate suspect flags; if missing columns, set FALSE
  if (!"suspect" %in% colnames(combinedHeelStrikes)) combinedHeelStrikes$suspect <- FALSE
  if ("suspect_alt" %in% colnames(combinedHeelStrikes)) {
    combinedHeelStrikes$suspect <- combinedHeelStrikes$suspect | combinedHeelStrikes$suspect_alt
    combinedHeelStrikes$suspect_alt <- NULL
  }

  # Apply outlier processing if outlier data is provided
  if (exists("outliers_heel_data", envir = .GlobalEnv) && exists("outliers_steps_data", envir = .GlobalEnv)) {
    gait_main_logger("DEBUG", "Starting outlier processing")
    combinedHeelStrikes <- apply_outlier_processing(
      combinedHeelStrikes,
      heel_outliers = outliers_heel_data,
      step_outliers = outliers_steps_data,
      tolerance = 0.03
    )
    gait_main_logger("DEBUG", "Outlier processing completed")
  } else {
    gait_main_logger("DEBUG", "No outlier data available, initializing outlierSteps column")
    # Initialize outlierSteps column if no outlier data available
    if (!"outlierSteps" %in% colnames(combinedHeelStrikes)) {
      combinedHeelStrikes$outlierSteps <- FALSE
    }
  }

  ensure_alternation <- function(data1, data2) {
    # mark suspect for same-foot consecutive strikes instead of removing
    # BUT exclude outliers from alternation analysis since they're already marked
    data1$suspect_alt <- FALSE

    # Filter out outliers for alternation analysis
    non_outlier_data <- data1[!data1$outlierSteps, ]

    if (nrow(non_outlier_data) > 1) {
      # Check alternation only on non-outlier data
      incorrect_seq <- which(diff(as.numeric(non_outlier_data$foot == "Left")) == 0)
      if (length(incorrect_seq) > 0) {
        # Map back to original indices
        non_outlier_indices <- which(!data1$outlierSteps)
        original_indices <- non_outlier_indices[incorrect_seq + 1]
        data1$suspect_alt[original_indices] <- TRUE # mark the second of the two as suspect
        gait_main_logger <- create_module_logger("GAIT-MAIN")
        gait_main_logger("INFO", "Marked", length(incorrect_seq), "steps as suspect due to alternation violation (excluding outliers).")
      }
    }
    return(list(data1 = data1, data2 = data2))
  }

  detect_long_step_times <- function(data1, data2) {
    # mark suspect for steps with unusually long intervals (more than 2x median)
    # exclude outliers from analysis since they're already marked
    data1$suspect_long_time <- FALSE

    # Filter out outliers for analysis
    non_outlier_data <- data1[!data1$outlierSteps, ]

    if (nrow(non_outlier_data) > 2) {
      # Calculate time differences between consecutive steps
      step_times <- diff(non_outlier_data$time)
      median_step_time <- median(step_times, na.rm = TRUE)
      threshold <- 2 * median_step_time

      # Find steps with unusually long intervals
      long_interval_indices <- which(step_times > threshold)
      if (length(long_interval_indices) > 0) {
        # Map back to original indices (mark the second step of each long interval)
        non_outlier_indices <- which(!data1$outlierSteps)
        original_indices <- non_outlier_indices[long_interval_indices + 1]
        data1$suspect_long_time[original_indices] <- TRUE
        gait_main_logger <- create_module_logger("GAIT-MAIN")
        gait_main_logger("INFO", "Marked", length(long_interval_indices), "steps as suspect due to unusually long step time (>2x median of", round(median_step_time, 3), "s, threshold:", round(threshold, 3), "s) (excluding outliers).")
      }
    }
    return(list(data1 = data1, data2 = data2))
  }

  # Apply alternation marking (no removal) AFTER outlier marking
  gait_main_logger("DEBUG", "Starting alternation checking")
  results <- ensure_alternation(combinedHeelStrikes, combinedToeOffs)
  combinedHeelStrikes <- results$data1
  combinedToeOffs <- results$data2
  gait_main_logger("DEBUG", "Alternation checking completed")

  # Apply long step time detection
  gait_main_logger("DEBUG", "Starting long step time checking")
  results <- detect_long_step_times(combinedHeelStrikes, combinedToeOffs)
  combinedHeelStrikes <- results$data1
  combinedToeOffs <- results$data2
  gait_main_logger("DEBUG", "Long step time checking completed")

  # Merge all suspect flags into suspect column
  if ("suspect_alt" %in% colnames(combinedHeelStrikes)) {
    combinedHeelStrikes$suspect <- combinedHeelStrikes$suspect | combinedHeelStrikes$suspect_alt
    combinedHeelStrikes$suspect_alt <- NULL
  }
  if ("suspect_long_time" %in% colnames(combinedHeelStrikes)) {
    combinedHeelStrikes$suspect <- combinedHeelStrikes$suspect | combinedHeelStrikes$suspect_long_time
    combinedHeelStrikes$suspect_long_time <- NULL
  }

  # Label step numbers. Assuming each heel strike represents a new step
  gait_main_logger("DEBUG", "Labeling step numbers")
  combinedHeelStrikes$step <- seq_len(nrow(combinedHeelStrikes))
  combinedToeOffs$step <- seq_len(nrow(combinedToeOffs))

  gait_main_logger("DEBUG", "find_foot_events completed for participant:", participant, "trial:", trialNum)
  return(list(heelStrikes = combinedHeelStrikes, toeOffs = combinedToeOffs))
}
