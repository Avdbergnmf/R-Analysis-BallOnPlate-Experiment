#' Power Spectrum Utilities for Visualization
#'
#' This file contains functions for computing and plotting power spectra
#' across grouped and split data, similar to histogram functionality.

#' Compute power spectrum for a single time series
#' @param x Numeric vector of time series data
#' @param fs Sampling frequency in Hz
#' @param max_freq Maximum frequency to return
#' @param normalize Whether to normalize power to sum to 1
#' @return Data frame with frequency and power columns
compute_single_psd <- function(x, fs = 30, max_freq = 2.5, normalize = TRUE) {
  if (length(x) < 50 || !is.numeric(x)) {
    return(NULL)
  }
  
  # Remove NA values
  x <- x[is.finite(x)]
  if (length(x) < 50) {
    return(NULL)
  }
  
  tryCatch({
    # Remove linear trend
    x_detrended <- x - mean(x)
    
    # Apply Hamming window
    window_length <- length(x_detrended)
    window <- 0.54 - 0.46 * cos(2 * pi * (0:(window_length - 1)) / (window_length - 1))
    x_windowed <- x_detrended * window
    
    # Compute FFT
    fft_result <- fft(x_windowed)
    power_spectrum <- abs(fft_result)^2
    
    # Frequency vector (only positive frequencies)
    n_half <- ceiling(length(power_spectrum) / 2)
    freqs <- seq(0, fs / 2, length.out = n_half)
    power_spectrum <- power_spectrum[1:n_half]
    
    # Filter to max frequency
    freq_mask <- freqs <= max_freq
    freqs <- freqs[freq_mask]
    power_spectrum <- power_spectrum[freq_mask]
    
    # Normalize if requested
    if (normalize) {
      power_sum <- sum(power_spectrum)
      if (power_sum > 0) {
        power_spectrum <- power_spectrum / power_sum
      }
    }
    
    data.frame(
      frequency = freqs,
      power = power_spectrum
    )
  }, error = function(e) {
    NULL
  })
}

#' Smooth power spectrum using kernel smoothing
#' @param psd_df Data frame with frequency and power columns
#' @param bandwidth Smoothing bandwidth in Hz
#' @return Data frame with smoothed power
smooth_psd <- function(psd_df, bandwidth = 0.1) {
  if (is.null(psd_df) || nrow(psd_df) < 10) {
    return(psd_df)
  }
  
  tryCatch({
    # Use loess for smoothing
    span <- bandwidth / (max(psd_df$frequency) - min(psd_df$frequency))
    span <- max(0.01, min(0.99, span))  # Constrain span to valid range
    
    smooth_fit <- loess(power ~ frequency, data = psd_df, span = span)
    psd_df$power <- predict(smooth_fit, psd_df$frequency)
    
    # Ensure no negative power values
    psd_df$power[psd_df$power < 0] <- 0
    
    psd_df
  }, error = function(e) {
    # Return original if smoothing fails
    psd_df
  })
}


#' Load raw continuous data for a participant/trial from tracker
#' @param participant_id Participant ID
#' @param trial_num Trial number
#' @param tracker Tracker name (e.g., "hip", "pelvis", "udp")
#' @param var_name Variable name (e.g., "pos_x", "vel_y", etc.)
#' @param cached_tracker_data Optional cached tracker data frame (for efficiency)
#' @return List with values and time vectors, or NULL if not found
load_continuous_data <- function(participant_id, trial_num, tracker, var_name, cached_tracker_data = NULL) {
  tryCatch({
    # Use cached tracker data if available
    if (!is.null(cached_tracker_data) && nrow(cached_tracker_data) > 0) {
      # Filter cached data for this participant-trial combination
      data <- cached_tracker_data %>% 
        dplyr::filter(
          participant == participant_id, 
          trialNum == trial_num
        )
    } else {
      # Fall back to loading fresh data (old behavior)
      data <- get_t_data(participant_id, tracker, trial_num)
    }
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    if (!var_name %in% names(data)) {
      return(NULL)
    }
    return(list(
      values = data[[var_name]],
      time = data$time
    ))
  }, error = function(e) {
    warning(paste("Failed to load continuous data:", e$message))
    return(NULL)
  })
}

#' Get step frequency from get_mu_dyn_long data
#' @param participant_id Participant ID
#' @param trial_num Trial number
#' @return Step frequency in Hz, or NULL if unavailable
get_step_frequency_from_complexity <- function(participant_id, trial_num) {
  # Get step frequency from get_mu_dyn_long (column: complexity.step_freq)
  mu_data <- get_mu_dyn_long()
  
  if (is.null(mu_data) || nrow(mu_data) == 0) {
    return(NULL)
  }
  
  # Check if complexity.step_freq column exists
  if (!"complexity.step_freq" %in% names(mu_data)) {
    return(NULL)
  }
  
  # Get step frequency for this participant/trial
  trial_data <- mu_data %>%
    dplyr::filter(participant == participant_id, trialNum == as.character(trial_num))
  
  if (nrow(trial_data) == 0) {
    return(NULL)
  }
  
  step_freq <- trial_data$complexity.step_freq[1]
  
  # Validate
  if (!is.finite(step_freq) || step_freq < 0.05 || step_freq > 15) {
    return(NULL)
  }
  
  return(step_freq)
}

#' Compute power spectrum data for all groups
#' @param cached_tracker_data Cached tracker data frame
#' @param tracker Tracker name (e.g., "hip", "pelvis", "udp")
#' @param var_name Variable name to analyze (e.g., "pos_x", "vel_y")
#' @param group_by Grouping variable
#' @param split_by Split variable (NULL for no split)
#' @param max_freq Maximum frequency
#' @param smooth_bandwidth Smoothing bandwidth
#' @param normalize Normalize power
#' @return Data frame with power spectrum for each group
compute_power_spectrum_data <- function(cached_tracker_data, tracker, var_name, group_by, split_by = NULL,
                                        max_freq = 2.5, smooth_bandwidth = 0.1, 
                                        normalize = TRUE) {
  
  if (is.null(cached_tracker_data) || nrow(cached_tracker_data) == 0) {
    warning("No cached tracker data provided")
    return(NULL)
  }
  
  # Get grouping information from get_mu_dyn_long (has all the category columns)
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
  
  # Get unique combinations from mu data (for grouping/splitting)
  data_grouped <- mu_data %>%
    dplyr::select(dplyr::all_of(group_vars)) %>%
    dplyr::distinct()
  
  # Compute PSD for each group
  psd_list <- list()
  
  for (i in seq_len(nrow(data_grouped))) {
    row <- data_grouped[i, ]
    
    # Get step frequency from complexity metrics dataset
    step_freq <- get_step_frequency_from_complexity(
      as.character(row$participant),
      as.numeric(row$trialNum)
    )
    
    # Get continuous data directly from cached tracker data
    trial_data <- cached_tracker_data %>%
      dplyr::filter(
        participant == as.character(row$participant),
        trialNum == as.character(row$trialNum)
      )
    
    if (nrow(trial_data) == 0 || !var_name %in% names(trial_data)) {
      next
    }
    
    continuous_data <- list(
      values = trial_data[[var_name]],
      time = trial_data$time
    )
    
    if (is.null(continuous_data) || length(continuous_data$values) < 50) {
      next
    }
    
    # Clean data
    valid_mask <- is.finite(continuous_data$values) & is.finite(continuous_data$time)
    values <- continuous_data$values[valid_mask]
    times <- continuous_data$time[valid_mask]
    
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
    
    # Add step frequency for adaptive band calculation
    psd_df$step_freq <- if (!is.null(step_freq)) step_freq else NA_real_
    
    psd_list[[length(psd_list) + 1]] <- psd_df
  }
  
  if (length(psd_list) == 0) {
    return(NULL)
  }
  
  # Combine all PSDs
  psd_combined <- dplyr::bind_rows(psd_list)
  
  # Average across participant/trial to get group-level spectra
  # This is essential for proper stacked area charts
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
      step_freq = mean(step_freq, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(psd_combined)
}

#' Plot power spectrum with adaptive frequency bands
#' @param ps_data Power spectrum data frame
#' @param stack Whether to stack the spectra
#' @param show_individual Whether to show individual spectra
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_power_spectrum <- function(ps_data, stack = TRUE, show_individual = FALSE,
                               split_by = NULL, base_size = 10) {
  
  if (is.null(ps_data) || nrow(ps_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Determine grouping variable (the one that's not frequency, power, step_freq, or split_by)
  group_var <- setdiff(names(ps_data), c("frequency", "power", "step_freq", split_by))
  if (length(group_var) == 0) {
    group_var <- NULL
  } else {
    group_var <- group_var[1]
  }
  
  # Create base plot
  p <- ggplot(ps_data, aes(x = frequency, y = power))
  
  if (stack && !is.null(group_var)) {
    # Stacked area chart - each group is a colored area
    p <- p + geom_area(aes_string(fill = group_var), position = "stack", alpha = 0.8)
  } else if (!is.null(group_var)) {
    # Line plot with different colors for each group
    p <- p + geom_line(aes_string(color = group_var), size = 1.2, alpha = 0.8)
  } else {
    # Simple line plot (only one group)
    p <- p + geom_line(size = 1.2, color = "steelblue")
  }
  
  # Add adaptive frequency band boundaries (matching complexity.R)
  if ("step_freq" %in% names(ps_data)) {
    # Get average step frequency
    mean_step_freq <- mean(ps_data$step_freq, na.rm = TRUE)
    
    if (is.finite(mean_step_freq) && mean_step_freq > 0.05 && mean_step_freq < 15) {
      # Adaptive bands based on step frequency (matching complexity.R lines 336-338)
      low_high <- mean_step_freq * 0.8
      mid_high <- mean_step_freq * 1.2
      
      p <- p +
        geom_vline(xintercept = low_high, linetype = "dashed", color = "darkgray", alpha = 0.7) +
        geom_vline(xintercept = mid_high, linetype = "dashed", color = "darkgray", alpha = 0.7) +
        annotate("text", x = low_high / 2, y = Inf, label = "Low", 
                vjust = 1.5, size = 3, color = "darkgray") +
        annotate("text", x = (low_high + mid_high) / 2, y = Inf, label = "Mid (Gait)", 
                vjust = 1.5, size = 3, color = "darkgray") +
        annotate("text", x = mid_high + (max(ps_data$frequency) - mid_high) / 2, y = Inf, label = "High", 
                vjust = 1.5, size = 3, color = "darkgray")
    } else {
      # Fixed bands fallback (matching complexity.R lines 347-349)
      p <- p +
        geom_vline(xintercept = 0.5, linetype = "dashed", color = "darkgray", alpha = 0.7) +
        geom_vline(xintercept = 2, linetype = "dashed", color = "darkgray", alpha = 0.7) +
        annotate("text", x = 0.25, y = Inf, label = "Low", 
                vjust = 1.5, size = 3, color = "darkgray") +
        annotate("text", x = 1.25, y = Inf, label = "Mid (Gait)", 
                vjust = 1.5, size = 3, color = "darkgray") +
        annotate("text", x = (2 + max(ps_data$frequency)) / 2, y = Inf, label = "High", 
                vjust = 1.5, size = 3, color = "darkgray")
    }
  }
  
  # Add faceting if split_by is specified
  if (!is.null(split_by) && split_by %in% names(ps_data)) {
    p <- p + facet_wrap(as.formula(paste("~", split_by)), ncol = 1, scales = "free_y")
  }
  
  # Styling
  p <- p +
    labs(
      x = "Frequency (Hz)",
      y = if (stack && !is.null(group_var)) "Stacked Power" else "Power",
      title = "Power Spectral Density"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      legend.title = element_blank()
    )
  
  return(p)
}

#' Compute summary statistics for power spectrum with adaptive bands
#' @param ps_data Power spectrum data
#' @param group_by Grouping variable
#' @param split_by Split variable
#' @return Data frame with summary statistics
compute_power_stats <- function(ps_data, group_by, split_by = NULL) {
  if (is.null(ps_data) || nrow(ps_data) == 0) {
    return(NULL)
  }
  
  # Compute peak frequency and total power for each group
  stats_vars <- c("frequency")
  if (!is.null(group_by) && group_by != "None" && group_by %in% names(ps_data)) {
    stats_vars <- c(stats_vars, group_by)
  }
  if (!is.null(split_by) && split_by != "None" && split_by %in% names(ps_data)) {
    stats_vars <- c(stats_vars, split_by)
  }
  
  # Calculate step frequency for adaptive bands
  mean_step_freq <- if ("step_freq" %in% names(ps_data)) {
    mean(ps_data$step_freq, na.rm = TRUE)
  } else {
    NA_real_
  }
  
  # Define adaptive frequency bands (matching complexity.R lines 331-350)
  if (is.finite(mean_step_freq) && mean_step_freq > 0.05 && mean_step_freq < 15) {
    # Adaptive bands based on step frequency
    low_high <- mean_step_freq * 0.8
    mid_high <- mean_step_freq * 1.2
    
    ps_data <- ps_data %>%
      dplyr::mutate(
        freq_band = dplyr::case_when(
          frequency >= 0.05 & frequency <= low_high ~ sprintf("Low (0.05-%.2f Hz)", low_high),
          frequency > low_high & frequency <= mid_high ~ sprintf("Mid (%.2f-%.2f Hz)", low_high, mid_high),
          frequency > mid_high ~ sprintf("High (>%.2f Hz)", mid_high),
          TRUE ~ "Other"
        )
      )
  } else {
    # Fixed bands fallback (matching complexity.R lines 347-349)
    ps_data <- ps_data %>%
      dplyr::mutate(
        freq_band = dplyr::case_when(
          frequency >= 0.05 & frequency <= 0.5 ~ "Low (0.05-0.5 Hz)",
          frequency > 0.5 & frequency <= 2 ~ "Mid (0.5-2 Hz)",
          frequency > 2 ~ "High (>2 Hz)",
          TRUE ~ "Other"
        )
      )
  }
  
  # Compute statistics
  stats_df <- ps_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(stats_vars, "frequency")))) %>%
    dplyr::summarize(
      step_freq_hz = if ("step_freq" %in% names(ps_data)) mean(step_freq, na.rm = TRUE) else NA_real_,
      peak_frequency = frequency[which.max(power)],
      total_power = sum(power, na.rm = TRUE),
      mean_frequency = sum(frequency * power, na.rm = TRUE) / sum(power, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Add power by frequency band
  band_stats <- ps_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(
      setdiff(stats_vars, "frequency"), "freq_band"
    )))) %>%
    dplyr::summarize(
      band_power = sum(power, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = freq_band,
      values_from = band_power,
      values_fill = 0
    )
  
  # Join statistics
  group_vars <- setdiff(stats_vars, "frequency")
  if (length(group_vars) > 0) {
    stats_df <- stats_df %>%
      dplyr::left_join(band_stats, by = group_vars)
  } else {
    stats_df <- dplyr::bind_cols(stats_df, band_stats)
  }
  
  # Round values
  stats_df <- stats_df %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
  
  return(stats_df)
}

