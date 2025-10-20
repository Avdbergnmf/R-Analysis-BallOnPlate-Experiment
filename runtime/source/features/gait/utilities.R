#' Gait Analysis Utility Functions
#'
#' Helper functions for gait event detection and processing


#' Check if hip data is stationary
#' @param hipData Hip position data
#' @param threshold_cm Threshold for movement detection in cm
#' @param time_window Time window to check in seconds
#' @return TRUE if hip is stationary
is_hip_stationary <- function(hipData, threshold_cm = 0.01, time_window = 30) {
  first_seconds <- hipData$time <= (hipData$time[1] + time_window)
  hip_range_x <- max(hipData$pos_x[first_seconds]) - min(hipData$pos_x[first_seconds])
  hip_range_y <- max(hipData$pos_y[first_seconds]) - min(hipData$pos_y[first_seconds])
  hip_range_z <- max(hipData$pos_z[first_seconds]) - min(hipData$pos_z[first_seconds])
  return(hip_range_x < threshold_cm && hip_range_y < threshold_cm && hip_range_z < threshold_cm)
}

#' Create output dataframes for heel strikes and toe-offs
#' @param footData Foot position data
#' @param local_maxima Indices of local maxima (heel strikes)
#' @param local_minima Indices of local minima (toe-offs)
#' @return List with heelStrikes and toeOffs dataframes
create_output_dataframes <- function(footData, local_maxima, local_minima) {
  heelStrikes <- data.frame(footData[local_maxima, ])
  toeOffs <- data.frame(footData[local_minima, ])

  # Center heelstrike locations
  heelStrikes$centered_pos_x <- heelStrikes$pos_x - mean(heelStrikes$pos_x, na.rm = TRUE)
  heelStrikes$centered_pos_z <- heelStrikes$pos_z - mean(heelStrikes$pos_z, na.rm = TRUE)

  return(list(heelStrikes = heelStrikes, toeOffs = toeOffs))
}

#' Add step differences per foot
#' @param relHeelStrikesData Relative heel strikes data
#' @return Dataframe with step width and length differences
add_diff_per_foot <- function(relHeelStrikesData) {
  gait_util_logger <- create_module_logger("GAIT-UTIL")
  gait_util_logger("DEBUG", "add_diff_per_foot called")
  gait_util_logger("DEBUG", "Input data dimensions:", nrow(relHeelStrikesData), "x", ncol(relHeelStrikesData))
  
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

  gait_util_logger("DEBUG", "add_diff_per_foot completed, result dimensions:", nrow(diffData), "x", ncol(diffData))
  return(diffData)
}

#' Safe approximation function that prevents errors in parallel workers
#' @param x Input x values
#' @param y Input y values
#' @param xout Output x values
#' @param rule Rule for extrapolation
#' @return Interpolated values or NA if insufficient data
safe_approx <- function(x, y, xout, rule = 1) {
  valid <- which(is.finite(x) & is.finite(y))
  if (length(unique(x[valid])) < 2) {
    return(rep(NA_real_, length(xout)))
  }
  return(approx(x[valid], y[valid], xout = xout, rule = rule)$y)
}

#' Sample a numeric time series using nearest-neighbour lookups
#' @param times Numeric vector of timestamps for the source signal
#' @param values Numeric vector of samples corresponding to `times`
#' @param query_times Numeric vector of timestamps to sample
#' @return Numeric vector with the same length as `query_times`
sample_time_series_nearest <- function(times, values, query_times) {
  if (length(times) == 0 || length(values) == 0 || length(query_times) == 0) {
    return(rep(NA_real_, length(query_times)))
  }

  vapply(query_times, function(target_time) {
    if (!is.finite(target_time)) {
      return(NA_real_)
    }

    idx <- which.min(abs(times - target_time))
    if (length(idx) == 0 || !is.finite(idx)) {
      return(NA_real_)
    }

    values[idx]
  }, numeric(1))
}
