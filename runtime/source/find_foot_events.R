################ Data Manipulation ################

# Helper function to check if hip data is stationary
is_hip_stationary <- function(hipData, threshold_cm = 0.01) {
  first_5_seconds <- hipData$time <= (hipData$time[1] + 5)
  hip_range_x <- max(hipData$pos_x[first_5_seconds]) - min(hipData$pos_x[first_5_seconds])
  hip_range_y <- max(hipData$pos_y[first_5_seconds]) - min(hipData$pos_y[first_5_seconds])
  hip_range_z <- max(hipData$pos_z[first_5_seconds]) - min(hipData$pos_z[first_5_seconds])

  return(hip_range_x < threshold_cm && hip_range_y < threshold_cm && hip_range_z < threshold_cm)
}

# Helper function to detect local extremes # Detect local extremes of relative foot pos - Based on https://c-motion.com/v3dwiki/index.php/Tutorial:_Gait_Events#Method_1._Coordinate_Based_Algorithm
detect_local_extremes <- function(relFootPos_filtered) {
  local_maxima <- which(diff(sign(diff(relFootPos_filtered))) == -2) + 1
  local_minima <- which(diff(sign(diff(relFootPos_filtered))) == 2) + 1

  # Always start with a minimum (toe-off)
  if (local_minima[1] > local_maxima[1]) {
    local_maxima <- local_maxima[-1]
  }

  return(list(maxima = local_maxima, minima = local_minima))
}

# Helper function to filter extremes based on time and position differences
filter_extremes <- function(local_maxima, local_minima, time_vector, pos_vector, min_time_diff = 0.1, min_pos_diff = 0.0) {
  i <- 1
  N_removed_time <- 0
  N_removed_pos <- 0

  while (i < length(local_maxima) && i < length(local_minima)) {
    # Calculate differences
    time_diff_max_to_min <- abs(time_vector[local_maxima[i]] - time_vector[local_minima[i + 1]])
    time_diff_min_to_max <- abs(time_vector[local_minima[i]] - time_vector[local_maxima[i]])
    pos_diff_max_to_min <- abs(pos_vector[local_maxima[i]] - pos_vector[local_minima[i + 1]])
    pos_diff_min_to_max <- abs(pos_vector[local_minima[i]] - pos_vector[local_maxima[i]])

    # Remove pairs that don't meet criteria
    if (time_diff_max_to_min < min_time_diff) {
      local_minima <- local_minima[-(i + 1)]
      local_maxima <- local_maxima[-i]
      N_removed_time <- N_removed_time + 1
    } else if (time_diff_min_to_max < min_time_diff) {
      local_maxima <- local_maxima[-i]
      local_minima <- local_minima[-i]
      N_removed_time <- N_removed_time + 1
    } else if (pos_diff_max_to_min < min_pos_diff) {
      local_minima <- local_minima[-(i + 1)]
      local_maxima <- local_maxima[-i]
      N_removed_pos <- N_removed_pos + 1
    } else if (pos_diff_min_to_max < min_pos_diff) {
      local_maxima <- local_maxima[-i]
      local_minima <- local_minima[-i]
      N_removed_pos <- N_removed_pos + 1
    } else {
      i <- i + 1
    }
  }

  return(list(
    maxima = local_maxima, minima = local_minima,
    N_removed_time = N_removed_time, N_removed_pos = N_removed_pos
  ))
}

# Helper function to filter extremes based on hip position
filter_by_hip_position <- function(local_maxima, local_minima, relFootPos) {
  count_wrong_side_of_hip <- length(relFootPos[local_maxima] > 0) + length(relFootPos[local_maxima] > 0)
  local_maxima <- local_maxima[relFootPos[local_maxima] > 0]
  local_minima <- local_minima[relFootPos[local_minima] < 0]

  return(list(
    maxima = local_maxima, minima = local_minima,
    count_wrong_side_of_hip = count_wrong_side_of_hip
  ))
}

# Helper function to check and fix alternation
check_alternation <- function(local_maxima, local_minima) {
  N_removed_min <- 0
  N_removed_max <- 0

  if (length(local_minima) == 0 || length(local_maxima) == 0) {
    warning("No local minima or maxima found for alternation checking. Skipping alternation step.")
    return(list(
      maxima = local_maxima, minima = local_minima,
      N_removed_min = 0, N_removed_max = 0
    ))
  }

  i <- 1
  while (i <= length(local_minima) && i <= length(local_maxima)) {
    if (i > length(local_minima) || i > length(local_maxima)) break

    # Remove maxima that come before their corresponding minima
    while (i <= length(local_minima) && i <= length(local_maxima) && local_maxima[i] < local_minima[i]) {
      if (length(local_maxima) < i) break
      local_maxima <- local_maxima[-i]
      N_removed_min <- N_removed_min + 1
      if (length(local_maxima) < i) break
    }

    # Remove minima that come after the next maxima
    while ((i + 1) <= length(local_minima) && i <= length(local_maxima) && local_maxima[i] > local_minima[i + 1]) {
      if (length(local_minima) < (i + 1)) break
      local_minima <- local_minima[-(i + 1)]
      N_removed_max <- N_removed_max + 1
      if (length(local_minima) < (i + 1)) break
    }

    i <- i + 1
  }

  return(list(
    maxima = local_maxima, minima = local_minima,
    N_removed_min = N_removed_min, N_removed_max = N_removed_max
  ))
}

# Helper function to create output dataframes
create_output_dataframes <- function(footData, local_maxima, local_minima) {
  heelStrikes <- data.frame(footData[local_maxima, ])
  toeOffs <- data.frame(footData[local_minima, ])

  # Center heelstrike locations
  heelStrikes$centered_pos_x <- heelStrikes$pos_x - mean(heelStrikes$pos_x, na.rm = TRUE)
  heelStrikes$centered_pos_z <- heelStrikes$pos_z - mean(heelStrikes$pos_z, na.rm = TRUE)

  return(list(heelStrikes = heelStrikes, toeOffs = toeOffs))
}

detect_foot_events_coordinates <- function(footData, hipData) {
  # Check if hip data is stationary
  useHip <- is_hip_stationary(hipData)

  if (useHip) {
    print("Hip data appears stationary (moves less than 1cm). Using absolute foot position instead of relative position.")
    relFootPos <- footData$pos_z
  } else {
    relFootPos <- footData$pos_z - hipData$pos_z
  }

  # Filter and detect extremes
  relFootPos_filtered <- apply_padding_and_filter(relFootPos, 4, 90, 5)
  extremes <- detect_local_extremes(relFootPos_filtered)
  local_maxima <- extremes$maxima
  local_minima <- extremes$minima

  # Filter extremes based on time and position
  filtered <- filter_extremes(local_maxima, local_minima, footData$time, relFootPos_filtered)
  local_maxima <- filtered$maxima
  local_minima <- filtered$minima

  # Filter by hip position
  if (useHip) {
    hip_filtered <- filter_by_hip_position(local_maxima, local_minima, relFootPos)
    local_maxima <- hip_filtered$maxima
    local_minima <- hip_filtered$minima
  }

  # Check alternation
  alternation <- check_alternation(local_maxima, local_minima)
  local_maxima <- alternation$maxima
  local_minima <- alternation$minima

  # Refine heelstrike
  local_maxima <- refine_heelstrike(footData, local_maxima, local_minima)

  # Ensure lengths match
  lMax <- length(local_maxima)
  lMin <- length(local_minima)
  if (lMax != lMin) {
    if (lMax > lMin) {
      print("Something REALLY WRONG... check detect_foot_events_coordinates()")
    }
    trimLength <- min(lMax, lMin)
    local_maxima <- local_maxima[1:trimLength]
    local_minima <- local_minima[1:trimLength]
  }

  # Logging
  if (filtered$N_removed_time > 0) {
    print(paste("removed", filtered$N_removed_time, "max+min due to time constraint."))
  }
  if (filtered$N_removed_pos > 0) {
    print(paste("removed", filtered$N_removed_pos, "max+min due to pos difference constraint."))
  }
  if (hip_filtered$count_wrong_side_of_hip > 0) {
    print(paste("removed", hip_filtered$count_wrong_side_of_hip, " extrema due to wrong side of hip"))
  }
  if (alternation$N_removed_max + alternation$N_removed_min > 0) {
    print(paste("removed", alternation$N_removed_max, "maxima, and", alternation$N_removed_min, "minima due to wrong alternation."))
  }

  if (length(local_maxima) != length(local_minima)) {
    print(paste("WARNING: Length maxima:", length(local_maxima), "Length minima:", length(local_minima)))
  }

  # Create output dataframes
  output <- create_output_dataframes(footData, local_maxima, local_minima)
  print(paste("--- ---totalsteps: ", length(output$heelStrikes$time)))

  return(output)
}

find_foot_events <- function(participant, trialNum) {
  preprocessedData <- get_preprocessed_data(participant, trialNum, c("leftfoot", "rightfoot", "hip"))

  leftfoot <- preprocessedData$leftfoot
  rightfoot <- preprocessedData$rightfoot
  hip <- preprocessedData$hip

  # Detect toe-off and heelstrikes
  footEventsLeft <- detect_foot_events_coordinates(leftfoot, hip)
  footEventsRight <- detect_foot_events_coordinates(rightfoot, hip)

  # Add a 'foot' column to each event dataframe
  footEventsLeft$heelStrikes$foot <- "Left"
  footEventsLeft$toeOffs$foot <- "Left"
  footEventsRight$heelStrikes$foot <- "Right"
  footEventsRight$toeOffs$foot <- "Right"

  # Combine heel strikes and foot lifts from both feet
  combinedHeelStrikes <- rbind(footEventsLeft$heelStrikes, footEventsRight$heelStrikes)
  combinedToeOffs <- rbind(footEventsLeft$toeOffs, footEventsRight$toeOffs)

  # Order the events by time
  combinedHeelStrikes <- combinedHeelStrikes[order(combinedHeelStrikes$time), ]
  combinedToeOffs <- combinedToeOffs[order(combinedToeOffs$time), ]

  ensure_alternation <- function(data1, data2) {
    incorrect_seq <- which(diff(as.numeric(data1$foot == "Left")) == 0)

    if (length(incorrect_seq > 0)) {
      # print(data1[c(incorrect_seq,incorrect_seq+1),])
      data1 <- data1[-(incorrect_seq + 1), ] # remove the second (later) value
      data2 <- data2[-(incorrect_seq + 1), ] # remove the second (later) value
      print(paste("--- ---removed", length(incorrect_seq), "steps total (wrong alternation). Removing at place:", incorrect_seq))
      # print("----------------------")
    }
    return(list(data1 = data1, data2 = data2))
  }

  # Apply alternation check on heelStrikes and remove index columns
  results <- ensure_alternation(combinedHeelStrikes, combinedToeOffs)
  combinedHeelStrikes <- results$data1
  combinedToeOffs <- results$data2

  # Label step numbers. Assuming each heel strike represents a new step
  combinedHeelStrikes$step <- seq_len(nrow(combinedHeelStrikes))
  combinedToeOffs$step <- seq_len(nrow(combinedToeOffs))

  return(list(heelStrikes = combinedHeelStrikes, toeOffs = combinedToeOffs))
}

add_diff_per_foot <- function(relHeelStrikesData) {
  # Group by foot, calculate diffs, and create a new dataframe with just the diffs
  diffData <- relHeelStrikesData %>%
    arrange(time) %>% # Ensure data is ordered by time within each foot
    group_by(foot) %>%
    mutate(
      stepWidth = c(NA, diff(pos_x)),
      stepLength = c(NA, diff(actual_pos_z))
    ) %>%
    ungroup() %>% # Remove grouping
    select(time, foot, stepWidth, stepLength) # Select only relevant columns for the new dataframe

  return(diffData)
}

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

refine_heelstrike <- function(footData, local_maxima, local_minima, smoothing_window = 5, change_threshold = 0.05) {
  # Refine heelstrike times
  refined_local_maxima <- c()
  for (i in seq_along(local_maxima)) {
    heelstrike_time <- footData$time[local_maxima[i]]
    toeoff_time <- footData$time[local_minima[i + 1]]

    # Extract the segment from heelstrike to the next toe-off
    segment <- footData %>%
      filter(time >= heelstrike_time & time <= toeoff_time)

    # Smooth the x and y positions
    segment$smoothed_x <- zoo::rollmean(segment$pos_x, smoothing_window, fill = NA)
    segment$smoothed_y <- zoo::rollmean(segment$pos_y, smoothing_window, fill = NA)

    # Calculate the rate of change
    dx <- diff(segment$smoothed_x) / diff(segment$time)
    dy <- diff(segment$smoothed_y) / diff(segment$time)

    # Find stable points
    stable_points <- which(abs(dx) < change_threshold & abs(dy) < change_threshold)

    if (length(stable_points) > 0) {
      # Take the first stable point
      stable_point <- stable_points[1]

      # Update the heelstrike time to the stable point
      refined_local_maxima <- c(refined_local_maxima, local_maxima[i] + stable_point)
      # print(paste("Stable point found for heelstrike at time:", heelstrike_time,". Shifted by:", segment$time[stable_point] - heelstrike_time,"s"))
    } else {
      if (!is.na(min(dx))) { # this happens if there is no more data after the heelstrike, this almost always results in a wrong step, so we remove it
        refined_local_maxima <- c(refined_local_maxima, local_maxima[i])
      }

      print(paste("No stable point found for heelstrike at time:", heelstrike_time, ". Minimum change x:", min(dx), ". Minimum change y:", min(dy)))
    }
  }
  return(refined_local_maxima)
}
