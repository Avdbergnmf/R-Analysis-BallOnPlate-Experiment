#' Gait Event Filtering Functions
#'
#' Functions for filtering and cleaning detected gait events

#' Filter extremes based on time and position differences
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param time_vector Time vector
#' @param pos_vector Position vector
#' @param min_time_diff Minimum time difference
#' @param min_pos_diff Minimum position difference
#' @return List with filtered maxima and minima
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

#' Filter extremes based on hip position
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param relFootPos Relative foot position
#' @return List with filtered maxima and minima
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
    gait_filtering_logger <- create_module_logger("GAIT-FILTERING")
    gait_filtering_logger("INFO", "Found", count_wrong_side_of_hip, "max+min on wrong side of hip (marked as suspect).")
  }

  return(list(
    maxima = filtered_maxima, minima = filtered_minima,
    suspect_maxima_indices = suspect_maxima_indices,
    suspect_minima_indices = suspect_minima_indices,
    count_wrong_side_of_hip = count_wrong_side_of_hip
  ))
}

#' Filter extremes based on other foot position
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param relFootPos Relative foot position
#' @param threshold Threshold for foot separation
#' @return List with filtered maxima and minima
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
    gait_filtering_logger <- create_module_logger("GAIT-FILTERING")
    gait_filtering_logger("INFO", "Removed", count_wrong_side_of_foot, "max+min due to insufficient foot separation.")
  }

  return(list(
    maxima = valid_maxima, minima = valid_minima,
    count_wrong_side_of_foot = count_wrong_side_of_foot
  ))
}
