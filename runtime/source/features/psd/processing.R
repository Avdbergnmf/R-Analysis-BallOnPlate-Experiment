#' Power Spectrum Data Processing Functions
#'
#' Functions for processing and aggregating power spectrum data across groups

#' Compute power spectrum data for all groups
#' @param cached_tracker_data Cached tracker data frame
#' @param var_name Variable name to analyze (e.g., "pos_x", "vel_y")
#' @param group_by Grouping variable
#' @param split_by Split variable (NULL for no split)
#' @param max_freq Maximum frequency
#' @param smooth_bandwidth Smoothing bandwidth
#' @param normalize Normalize power
#' @return Data frame with power spectrum for each group
compute_power_spectrum_data <- function(cached_tracker_data, var_name, group_by, split_by = NULL,
                                        max_freq = 2.5, smooth_bandwidth = 0.1, 
                                        normalize = TRUE) {
  
  if (is.null(cached_tracker_data) || nrow(cached_tracker_data) == 0) {
    warning("No cached tracker data provided")
    return(NULL)
  }
  
  # Get grouping information from get_mu_dyn_long
  mu_data <- get_mu_dyn_long()
  if (is.null(mu_data) || nrow(mu_data) == 0) {
    warning("No mu data available for grouping")
    return(NULL)
  }
  
  # Group data
  group_vars <- c("participant", "trialNum")
  if (!is.null(group_by) && group_by != "None") {
    if (!group_by %in% group_vars) {
      group_vars <- c(group_vars, group_by)
    }
  }
  if (!is.null(split_by) && split_by != "None") {
    if (!split_by %in% group_vars) {
      group_vars <- c(group_vars, split_by)
    }
  }
  
  # Ensure we have the necessary grouping variables
  group_vars <- unique(group_vars[group_vars %in% names(mu_data)])
  
  if (length(group_vars) == 0) {
    warning("No valid grouping variables found")
    return(NULL)
  }
  
  # Get unique combinations from mu data
  data_grouped <- mu_data %>%
    dplyr::select(dplyr::all_of(group_vars)) %>%
    dplyr::distinct()
  
  # Compute PSD for each group
  psd_list <- list()
  
  for (i in seq_len(nrow(data_grouped))) {
    row <- data_grouped[i, ]
    
    # Get continuous data from cached tracker data
    trial_data <- cached_tracker_data %>%
      dplyr::filter(
        participant == as.character(row$participant),
        trialNum == as.character(row$trialNum)
      )
    
    if (nrow(trial_data) == 0 || !var_name %in% names(trial_data)) {
      next
    }
    
    # Clean data
    valid_mask <- is.finite(trial_data[[var_name]]) & is.finite(trial_data$time)
    values <- trial_data[[var_name]][valid_mask]
    times <- trial_data$time[valid_mask]
    
    if (length(values) < 50) {
      next
    }
    
    # Calculate sampling frequency
    if (length(times) > 1) {
      dt <- median(diff(times), na.rm = TRUE)
      fs <- if (is.finite(dt) && dt > 0) 1 / dt else 120
    } else {
      fs <- 120  # Default
    }
    
    # Compute PSD
    psd_df <- compute_single_psd(
      values,
      fs = fs,
      max_freq = max_freq,
      normalize = normalize
    )
    
    if (is.null(psd_df)) {
      next
    }
    
    # Smooth PSD
    psd_df <- smooth_psd(psd_df, smooth_bandwidth)
    
    # Add grouping information
    for (var in names(row)) {
      psd_df[[var]] <- row[[var]]
    }
    
    psd_list[[length(psd_list) + 1]] <- psd_df
  }
  
  if (length(psd_list) == 0) {
    return(NULL)
  }
  
  # Combine all PSDs
  psd_combined <- dplyr::bind_rows(psd_list)
  
  # Average across participant/trial to get group-level spectra
  avg_vars <- c("frequency")
  if (!is.null(group_by) && group_by != "None" && group_by %in% names(psd_combined)) {
    avg_vars <- c(avg_vars, group_by)
  }
  if (!is.null(split_by) && split_by != "None" && split_by %in% names(psd_combined)) {
    avg_vars <- c(avg_vars, split_by)
  }
  
  psd_combined <- psd_combined %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(avg_vars))) %>%
    dplyr::summarize(
      power = mean(power, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(psd_combined)
}

#' Extract time series data for a specific trial
#' @param cached_tracker_data Cached tracker data frame
#' @param participant Participant ID
#' @param trial_num Trial number
#' @param var_name Variable name to extract
#' @return List with values, times, and sampling frequency
extract_trial_data <- function(cached_tracker_data, participant, trial_num, var_name) {
  trial_data <- cached_tracker_data %>%
    dplyr::filter(
      participant == as.character(participant),
      trialNum == as.character(trial_num)
    )
  
  if (nrow(trial_data) == 0 || !var_name %in% names(trial_data)) {
    return(NULL)
  }
  
  # Clean data
  valid_mask <- is.finite(trial_data[[var_name]]) & is.finite(trial_data$time)
  values <- trial_data[[var_name]][valid_mask]
  times <- trial_data$time[valid_mask]
  
  if (length(values) < 50) {
    return(NULL)
  }
  
  # Calculate sampling frequency
  if (length(times) > 1) {
    dt <- median(diff(times), na.rm = TRUE)
    fs <- if (is.finite(dt) && dt > 0) 1 / dt else 120
  } else {
    fs <- 120  # Default
  }
  
  list(values = values, times = times, fs = fs)
}

#' Aggregate power spectra across groups
#' @param psd_list List of power spectrum data frames
#' @param group_by Grouping variable
#' @param split_by Split variable
#' @return Aggregated power spectrum data frame
aggregate_power_spectra <- function(psd_list, group_by = NULL, split_by = NULL) {
  if (length(psd_list) == 0) {
    return(NULL)
  }
  
  # Combine all PSDs
  psd_combined <- dplyr::bind_rows(psd_list)
  
  # Average across participant/trial to get group-level spectra
  avg_vars <- c("frequency")
  if (!is.null(group_by) && group_by != "None" && group_by %in% names(psd_combined)) {
    avg_vars <- c(avg_vars, group_by)
  }
  if (!is.null(split_by) && split_by != "None" && split_by %in% names(psd_combined)) {
    avg_vars <- c(avg_vars, split_by)
  }
  
  psd_combined %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(avg_vars))) %>%
    dplyr::summarize(
      power = mean(power, na.rm = TRUE),
      .groups = "drop"
    )
}
