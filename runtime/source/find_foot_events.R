################ Data Manipulation ################

# Simplified function to check if a heel strike is a step outlier
check_outlier_heel_strike <- function(participant, trialNum, time, tolerance = 0.01) {
  if (!exists("outliers_steps_data", envir = .GlobalEnv)) {
    return(FALSE)
  }

  participant_char <- as.character(participant)
  trialNum_num <- as.numeric(trialNum)
  participant_outliers <- outliers_steps_data[
    outliers_steps_data$participant == participant_char &
      outliers_steps_data$trialNum == trialNum_num,
  ]

  if (nrow(participant_outliers) == 0) {
    return(FALSE)
  }

  diffs <- abs(participant_outliers$time - time)
  return(min(diffs) <= tolerance)
}

# Backward compatibility function
is_outlier_heel_strike <- function(participant, trialNum, time, tolerance = 0.01) {
  check_outlier_heel_strike(participant, trialNum, time, tolerance)
}

# Helper function to mark outlier steps in heel strike data
mark_outlier_steps <- function(heel_strikes_data, participant, trialNum, tolerance = 0.01) {
  # Initialize columns if they don't exist
  if (!"outlierSteps" %in% colnames(heel_strikes_data)) {
    heel_strikes_data$outlierSteps <- FALSE
  }

  # Check each heel strike against outlier data
  num_step_outliers <- 0
  for (i in seq_len(nrow(heel_strikes_data))) {
    if (check_outlier_heel_strike(participant, trialNum, heel_strikes_data$time[i], tolerance)) {
      heel_strikes_data$outlierSteps[i] <- TRUE
      num_step_outliers <- num_step_outliers + 1
    }
  }

  # Report step outlier statistics
  if (num_step_outliers > 0) {
    cat(sprintf(
      "DEBUG: Marked %d heel strikes as step outliers from CSV data\n",
      num_step_outliers
    ))
  }

  return(heel_strikes_data)
}



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
  # Identify steps that are on the wrong side of hip (for suspect marking)
  suspect_maxima_mask <- relFootPos[local_maxima] <= 0
  suspect_minima_mask <- relFootPos[local_minima] >= 0

  suspect_maxima_indices <- which(suspect_maxima_mask)
  suspect_minima_indices <- which(suspect_minima_mask)

  # For fallback removal (when no other foot data), still filter out wrong side
  filtered_maxima <- local_maxima[relFootPos[local_maxima] > 0]
  filtered_minima <- local_minima[relFootPos[local_minima] < 0]

  count_wrong_side_of_hip <- sum(suspect_maxima_mask) + sum(suspect_minima_mask)

  # Add logging
  if (count_wrong_side_of_hip > 0) {
    print(paste("found", count_wrong_side_of_hip, "max+min on wrong side of hip (marked as suspect)."))
  }

  return(list(
    maxima = filtered_maxima, minima = filtered_minima,
    suspect_maxima_indices = suspect_maxima_indices,
    suspect_minima_indices = suspect_minima_indices,
    count_wrong_side_of_hip = count_wrong_side_of_hip
  ))
}

# Helper function to filter extremes based on other foot position
filter_by_other_foot_position <- function(local_maxima, local_minima, relFootPos, threshold = 0.05) {
  # Same logic as hip filtering but with different threshold and sign expectations
  # relFootPos should be current_foot_pos_x - other_foot_pos_x

  # For heel strikes (maxima): expect some separation, keep if abs(relFootPos) > threshold
  valid_maxima_mask <- abs(relFootPos[local_maxima]) > threshold
  valid_maxima <- local_maxima[valid_maxima_mask]

  # For toe-offs (minima): more lenient, keep if abs(relFootPos) > threshold/2
  valid_minima_mask <- abs(relFootPos[local_minima]) > (threshold / 2)
  valid_minima <- local_minima[valid_minima_mask]

  # Count removed extrema
  count_wrong_side_of_foot <- sum(!valid_maxima_mask) + sum(!valid_minima_mask)

  # Add logging
  if (count_wrong_side_of_foot > 0) {
    print(paste("removed", count_wrong_side_of_foot, "max+min due to insufficient foot separation."))
  }

  return(list(
    maxima = valid_maxima, minima = valid_minima,
    count_wrong_side_of_foot = count_wrong_side_of_foot
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

  # Debug logging
  cat(sprintf(
    "DEBUG: Alternation check - Input: %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))

  i <- 1
  while (i <= length(local_minima) && i <= length(local_maxima)) {
    if (i > length(local_minima) || i > length(local_maxima)) break

    # Debug current state
    if (i <= length(local_maxima) && i <= length(local_minima)) {
      cat(sprintf(
        "DEBUG: Iteration %d - maxima[%d]=%d, minima[%d]=%d\n",
        i, i, local_maxima[i], i, local_minima[i]
      ))
    }

    # Remove maxima that come before their corresponding minima
    while (i <= length(local_minima) && i <= length(local_maxima) && local_maxima[i] < local_minima[i]) {
      if (length(local_maxima) < i) break
      cat(sprintf(
        "DEBUG: Removing maxima[%d]=%d (comes before minima[%d]=%d)\n",
        i, local_maxima[i], i, local_minima[i]
      ))
      local_maxima <- local_maxima[-i]
      N_removed_min <- N_removed_min + 1
      if (length(local_maxima) < i) break
    }

    # Remove minima that come after the next maxima
    while ((i + 1) <= length(local_minima) && i <= length(local_maxima) && local_maxima[i] > local_minima[i + 1]) {
      if (length(local_minima) < (i + 1)) break
      cat(sprintf(
        "DEBUG: Removing minima[%d]=%d (comes after maxima[%d]=%d)\n",
        i + 1, local_minima[i + 1], i, local_maxima[i]
      ))
      local_minima <- local_minima[-(i + 1)]
      N_removed_max <- N_removed_max + 1
      if (length(local_minima) < (i + 1)) break
    }

    i <- i + 1
  }

  cat(sprintf(
    "DEBUG: Alternation check - Output: %d maxima, %d minima (removed %d max, %d min)\n",
    length(local_maxima), length(local_minima), N_removed_min, N_removed_max
  ))

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

detect_foot_events_coordinates <- function(footData, hipData, otherFootData = NULL) {
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

  cat(sprintf(
    "DEBUG: Initial detection - %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))

  # Filter extremes based on time and position
  filtered <- filter_extremes(local_maxima, local_minima, footData$time, relFootPos_filtered)
  local_maxima <- filtered$maxima
  local_minima <- filtered$minima

  cat(sprintf(
    "DEBUG: After time/position filtering - %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))

  # Filter by other foot position, with hip as fallback
  position_filtered <- NULL
  hip_filtered <- NULL

  # Always try to filter by other foot position first
  skipOtherFootCheck <- TRUE
  if (!is.null(otherFootData) && nrow(otherFootData) > 0 && !skipOtherFootCheck) {
    # Calculate relative foot position: current_foot_pos_x - other_foot_pos_x
    other_foot_pos_x <- approx(otherFootData$time, otherFootData$pos_x,
      xout = footData$time, rule = 1
    )$y
    relFootPos_x <- footData$pos_x - other_foot_pos_x

    position_filtered <- filter_by_other_foot_position(local_maxima, local_minima, relFootPos_x)
    local_maxima <- position_filtered$maxima
    local_minima <- position_filtered$minima

    cat(sprintf(
      "DEBUG: After other foot filtering - %d maxima, %d minima\n",
      length(local_maxima), length(local_minima)
    ))
  }

  # Always check hip position for suspect marking (if hip data available)
  if (!is.null(hipData) && nrow(hipData) > 0) {
    hip_pos_x <- safe_approx(hipData$time, hipData$pos_x,
      xout = footData$time, rule = 1
    )

    relFootPos_x_hip <- footData$pos_x - hip_pos_x

    # If hip_pos_x is all NA (insufficient points), skip hip-based suspect marking
    if (all(is.na(hip_pos_x))) {
      hip_filtered <- NULL
    } else {
      hip_filtered <- filter_by_hip_position(local_maxima, local_minima, relFootPos_x_hip)
      # Don't update local_maxima/local_minima here - hip filtering is only for suspect marking
    }
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

  cat(sprintf(
    "DEBUG: After refinement - %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))

  # Ensure lengths match
  lMax <- length(local_maxima)
  lMin <- length(local_minima)
  if (lMax != lMin) {
    # if (lMax > lMin) {
    #  print("Something REALLY WRONG... check detect_foot_events_coordinates()")
    # }
    trimLength <- min(lMax, lMin)
    local_maxima <- local_maxima[1:trimLength]
    local_minima <- local_minima[1:trimLength]
    # keep only suspect indices within range after trimming
    if (exists("suspect_unstable_idx")) {
      suspect_unstable_idx <- suspect_unstable_idx[suspect_unstable_idx <= trimLength]
    }

    cat(sprintf(
      "DEBUG: After length matching - %d maxima, %d minima\n",
      length(local_maxima), length(local_minima)
    ))
  }

  # Logging
  if (filtered$N_removed_time > 0) {
    print(paste("removed", filtered$N_removed_time, "max+min due to time constraint."))
  }
  if (filtered$N_removed_pos > 0) {
    print(paste("removed", filtered$N_removed_pos, "max+min due to pos difference constraint."))
  }
  # Position-based filtering logging is now handled within the filtering logic above
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

  # Flag unstable steps from refinement
  if (length(suspect_unstable_idx) > 0) {
    output$heelStrikes$suspect[suspect_unstable_idx] <- TRUE
  }

  # Flag suspect steps based on hip position (only if hip filtering was performed and other foot data was available)
  if (!is.null(hip_filtered) && !is.null(position_filtered)) {
    # Map original indices to final indices after all filtering
    original_maxima_before_filtering <- extremes$maxima

    # Find which of the original suspect indices are still in our final maxima
    for (suspect_idx in hip_filtered$suspect_maxima_indices) {
      original_suspect_index <- original_maxima_before_filtering[suspect_idx]
      final_position <- which(local_maxima == original_suspect_index)
      if (length(final_position) > 0) {
        output$heelStrikes$suspect[final_position] <- TRUE
      }
    }
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
  footEventsLeft <- detect_foot_events_coordinates(leftfoot, hip, rightfoot)
  footEventsRight <- detect_foot_events_coordinates(rightfoot, hip, leftfoot)

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

  # Note: Heel strike removal is now handled in the UI (page16_manualOutlierFiltering.Rmd)

  # Mark outlier steps based on CSV data
  combinedHeelStrikes <- mark_outlier_steps(combinedHeelStrikes, participant, trialNum)

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

  if (max_iterations <= 0) {
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
  heelstrike_indices <- local_maxima[seq_len(max_iterations)]
  toeoff_indices <- local_minima[seq_len(max_iterations) + 1]

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

# -----------------------------------------------------------------------------
# Utility: safe_approx ---------------------------------------------------------
# Returns interpolated vector (same length as xout) or all-NA when the source
# series does not contain at least two finite, unique sample points.  Prevents
# approx() from erroring inside parallel workers.

safe_approx <- function(x, y, xout, rule = 1) {
  valid <- which(is.finite(x) & is.finite(y))
  if (length(unique(x[valid])) < 2) {
    return(rep(NA_real_, length(xout)))
  }
  return(approx(x[valid], y[valid], xout = xout, rule = rule)$y)
}
