################ Data Manipulation ################

# Helper function to check if hip data is stationary
is_hip_stationary <- function(hipData, threshold_cm = 0.01, time_window = 30) {
  first_seconds <- hipData$time <= (hipData$time[1] + time_window)
  hip_range_x <- max(hipData$pos_x[first_seconds]) - min(hipData$pos_x[first_seconds])
  hip_range_y <- max(hipData$pos_y[first_seconds]) - min(hipData$pos_y[first_seconds])
  hip_range_z <- max(hipData$pos_z[first_seconds]) - min(hipData$pos_z[first_seconds])
  # print(paste("Hip range x:", hip_range_x, "cm, y:", hip_range_y, "cm, z:", hip_range_z, "cm"))
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
  useHip <- !is_hip_stationary(hipData)
  checkAlternation <- TRUE

  if (useHip) {
    relFootPos <- footData$pos_z - hipData$pos_z
  } else {
    print("Hip data appears stationary (moves less than the threshold). Using absolute foot position instead of relative position.")
    relFootPos <- footData$pos_z
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
  if (useHip) { # but only if we have the hip data.
    hip_filtered <- filter_by_hip_position(local_maxima, local_minima, relFootPos)
    local_maxima <- hip_filtered$maxima
    local_minima <- hip_filtered$minima
  }

  # Check alternation
  if (checkAlternation) {
    alternation <- check_alternation(local_maxima, local_minima)
    local_maxima <- alternation$maxima
    local_minima <- alternation$minima
  }

  # Refine heelstrike and capture unstable labels
  refine_res <- refine_heelstrike(footData, local_maxima, local_minima)
  local_maxima <- refine_res$maxima
  suspect_unstable_idx <- refine_res$suspect_unstable

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
    # keep only suspect indices within range after trimming
    if (exists("suspect_unstable_idx")) {
      suspect_unstable_idx <- suspect_unstable_idx[suspect_unstable_idx <= trimLength]
    }
  }

  # Logging
  if (filtered$N_removed_time > 0) {
    print(paste("removed", filtered$N_removed_time, "max+min due to time constraint."))
  }
  if (filtered$N_removed_pos > 0) {
    print(paste("removed", filtered$N_removed_pos, "max+min due to pos difference constraint."))
  }
  if (useHip && hip_filtered$count_wrong_side_of_hip > 0) {
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
  # Add 'suspect' column default FALSE
  output$heelStrikes$suspect <- FALSE
  # flag unstable
  if (length(suspect_unstable_idx) > 0) {
    output$heelStrikes$suspect[suspect_unstable_idx] <- TRUE
  }

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

  # propagate suspect flags; if missing columns, set FALSE
  if (!"suspect" %in% colnames(combinedHeelStrikes)) combinedHeelStrikes$suspect <- FALSE
  if ("suspect_alt" %in% colnames(combinedHeelStrikes)) {
    combinedHeelStrikes$suspect <- combinedHeelStrikes$suspect | combinedHeelStrikes$suspect_alt
    combinedHeelStrikes$suspect_alt <- NULL
  }

  ensure_alternation <- function(data1, data2) {
    # mark suspect for same-foot consecutive strikes instead of removing
    data1$suspect_alt <- FALSE
    incorrect_seq <- which(diff(as.numeric(data1$foot == "Left")) == 0)
    if (length(incorrect_seq) > 0) {
      data1$suspect_alt[incorrect_seq + 1] <- TRUE # mark the second of the two as suspect
      message("Marked ", length(incorrect_seq), " steps as suspect due to alternation violation.")
    }
    return(list(data1 = data1, data2 = data2))
  }

  # Apply alternation marking (no removal)
  results <- ensure_alternation(combinedHeelStrikes, combinedToeOffs)
  combinedHeelStrikes <- results$data1
  combinedToeOffs <- results$data2

  # Merge suspect_alt into suspect column (again, after alternation check)
  if ("suspect_alt" %in% colnames(combinedHeelStrikes)) {
    combinedHeelStrikes$suspect <- combinedHeelStrikes$suspect | combinedHeelStrikes$suspect_alt
    combinedHeelStrikes$suspect_alt <- NULL
  }

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

refine_heelstrike <- function(footData, local_maxima, local_minima,
                              smoothing_window = 5, change_threshold = 0.05) {
  # Vectorized heel-strike refinement - much faster than loop-based approach

  # Only process heel-strikes that have a corresponding next toe-off
  max_iterations <- min(length(local_maxima), length(local_minima) - 1)

  if (max_iterations == 0) {
    # No valid pairs to process - mark all as suspect
    return(list(maxima = local_maxima, suspect_unstable = seq_along(local_maxima)))
  }

  # Pre-smooth the entire dataset once (vectorized operation)
  footData$smoothed_x <- zoo::rollmean(footData$pos_x, smoothing_window, fill = NA)
  footData$smoothed_y <- zoo::rollmean(footData$pos_y, smoothing_window, fill = NA)

  # Calculate rate of change for entire dataset (vectorized)
  footData$dx <- c(NA, diff(footData$smoothed_x) / diff(footData$time))
  footData$dy <- c(NA, diff(footData$smoothed_y) / diff(footData$time))

  # Vectorized processing of heel-strike pairs
  heelstrike_indices <- local_maxima[1:max_iterations]
  toeoff_indices <- local_minima[2:(max_iterations + 1)]

  # Vectorized function to find first stable point in each segment
  find_stable_point <- function(start_idx, end_idx) {
    if (start_idx >= end_idx) {
      return(start_idx)
    } # Edge case

    segment_indices <- start_idx:end_idx
    dx_segment <- footData$dx[segment_indices]
    dy_segment <- footData$dy[segment_indices]

    # Find stable points (vectorized logical operations)
    stable_mask <- abs(dx_segment) < change_threshold & abs(dy_segment) < change_threshold
    stable_mask[is.na(stable_mask)] <- FALSE

    stable_points <- which(stable_mask)

    if (length(stable_points) > 0) {
      return(start_idx + stable_points[1] - 1) # Convert back to global index
    } else {
      # Check if we have valid data (any non-NA values)
      if (any(!is.na(dx_segment))) {
        return(start_idx) # Keep original if no stable point but valid data
      } else {
        return(NA) # Mark as invalid
      }
    }
  }

  # Apply vectorized refinement using mapply
  refined_indices <- mapply(find_stable_point, heelstrike_indices, toeoff_indices, SIMPLIFY = TRUE)

  # Identify unstable (suspect) heel-strikes
  unstable_mask <- is.na(refined_indices)
  unstable_indices <- which(unstable_mask)

  # Remove NA values and keep valid refined indices
  valid_mask <- !unstable_mask
  refined_local_maxima <- refined_indices[valid_mask]

  # Report unstable heel-strikes (vectorized message creation)
  if (sum(unstable_mask) > 0) {
    heelstrike_times <- footData$time[heelstrike_indices[unstable_mask]]
    message(
      "No stable point found for ", sum(unstable_mask), " heel-strikes at times: ",
      paste(round(heelstrike_times, 3), collapse = ", "), "; labelled as SUSPECT_UNSTABLE."
    )
  }

  # Handle any remaining heel-strikes that don't have a next toe-off
  if (length(local_maxima) > max_iterations) {
    remaining_heelstrikes <- local_maxima[(max_iterations + 1):length(local_maxima)]
    refined_local_maxima <- c(refined_local_maxima, remaining_heelstrikes)
    # Mark these as suspect since we couldn't refine them
    remaining_indices <- (length(refined_local_maxima) - length(remaining_heelstrikes) + 1):length(refined_local_maxima)
    unstable_indices <- c(unstable_indices, remaining_indices)
    message("Marked ", length(remaining_heelstrikes), " heel-strikes as suspect (no corresponding toe-off for refinement)")
  }

  return(list(maxima = refined_local_maxima, suspect_unstable = unstable_indices))
}
