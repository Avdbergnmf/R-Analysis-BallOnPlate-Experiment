#' Signal Processing Functions
#'
#' Functions for extracting and processing continuous signals from various data sources

#' Sample a time series at specific query times using nearest-neighbor
#' @param times Numeric vector of time points in the series
#' @param values Numeric vector of values at those times
#' @param query_times Numeric vector of times to sample at
#' @param method Sampling method (only "nearest" implemented)
#' @return Numeric vector of sampled values (same length as query_times)
sample_time_series <- function(times, values, query_times, method = "nearest") {
  if (length(times) == 0 || length(values) == 0 || length(query_times) == 0) {
    return(rep(NA_real_, length(query_times)))
  }

  # Nearest-neighbor: for each query time, find closest time point
  sampled <- vapply(query_times, function(qt) {
    if (!is.finite(qt)) {
      return(NA_real_)
    }
    idx <- which.min(abs(times - qt))
    if (length(idx) == 0) {
      return(NA_real_)
    }
    values[idx]
  }, FUN.VALUE = numeric(1))

  return(sampled)
}

#' Extract signal from simulation data
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param column_name Name of the column to extract from simulation data
#' @return List with values, time, fs, and n_valid
get_simulation_signal <- function(participant, trial, column_name) {
  sim_data <- get_simulation_data(participant, trial)

  if (is.null(sim_data) || nrow(sim_data) == 0) {
    return(list(values = numeric(0), time = numeric(0), fs = NA_real_, n_valid = 0L))
  }

  # Filter to only real simulation data (when ball is on plate)
  sim_data_filtered <- sim_data %>% dplyr::filter(simulating == TRUE)

  if (!column_name %in% colnames(sim_data_filtered)) {
    return(list(values = numeric(0), time = numeric(0), fs = NA_real_, n_valid = 0L))
  }

  # Extract values and times
  values <- sim_data_filtered[[column_name]]
  times <- sim_data_filtered$time

  # Calculate sampling frequency
  fs <- if (!is.null(times) && length(times) > 1) {
    dt <- median(diff(times), na.rm = TRUE)
    if (is.finite(dt) && dt > 0) 1 / dt else NA_real_
  } else {
    NA_real_
  }

  # Return in standard format
  list(
    values = values[is.finite(values)],
    time = if (!is.null(times)) times[is.finite(values)] else numeric(0),
    fs = fs,
    n_valid = sum(is.finite(values))
  )
}

#' Extract signal from tracker data (hip, udp, etc.)
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param tracker_type Tracker type (e.g., "hip", "udp")
#' @param column_name Name of the column to extract from tracker data
#' @param description Human-readable description for logging
#' @return List with values, time, fs, and n_valid
get_tracker_signal <- function(participant, trial, tracker_type, column_name, description) {
  tracker_data <- get_t_data(participant, tracker_type, trial)

  if (is.null(tracker_data) || nrow(tracker_data) == 0) {
    return(list(values = numeric(0), time = numeric(0), fs = NA_real_, n_valid = 0L))
  }

  if (!column_name %in% colnames(tracker_data)) {
    return(list(values = numeric(0), time = numeric(0), fs = NA_real_, n_valid = 0L))
  }

  # Extract values and times
  values <- tracker_data[[column_name]]
  times <- tracker_data$time

  # Calculate sampling frequency
  fs <- if (!is.null(times) && length(times) > 1) {
    dt <- median(diff(times), na.rm = TRUE)
    if (is.finite(dt) && dt > 0) 1 / dt else NA_real_
  } else {
    NA_real_
  }

  # Return in standard format
  valid_mask <- is.finite(values) & is.finite(times)
  list(
    values = values[valid_mask],
    time = times[valid_mask],
    fs = fs,
    n_valid = sum(valid_mask)
  )
}

#' Apply 6 Hz low-pass Butterworth filter and downsample to 30 Hz
#' @param values_raw Raw signal values
#' @param fs Sampling frequency
#' @param log_msg Logging function
#' @return Filtered and downsampled values
apply_signal_filtering <- function(values_raw, fs, log_msg) {
  # Apply 6 Hz low-pass Butterworth filter (4th-order, zero-lag)
  # Retains the 0.05-0.5 Hz perturbation band while removing higher-frequency noise
  cutoff <- 6 / (fs / 2)
  if (cutoff >= 1) {
    values_filt <- values_raw
  } else {
    bf <- signal::butter(4, cutoff, type = "low")
    values_filt <- as.numeric(signal::filtfilt(bf, values_raw))
  }

  # Down-sample to 30 Hz to avoid oversampling bias in complexity stats
  if (fs > 30) {
    dec_factor <- floor(fs / 30)
    if (dec_factor > 1) {
      values_filt <- values_filt[seq(1, length(values_filt), by = dec_factor)]
      log_msg("DEBUG", "      Down-sampled with factor", dec_factor, "→", length(values_filt), "samples (@30 Hz approx.)")
    }
  }

  return(values_filt)
}
