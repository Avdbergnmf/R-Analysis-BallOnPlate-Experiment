#' Power Spectrum Utilities for Visualization
#'
#' Simplified functions for computing and plotting power spectra.

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
    # Apply same preprocessing as complexity analysis:
    # 1. 6 Hz low-pass Butterworth filter (4th-order, zero-lag)
    cutoff <- 6 / (fs / 2)
    if (cutoff >= 1) {
      x_filtered <- x
    } else {
      bf <- signal::butter(4, cutoff, type = "low")
      x_filtered <- as.numeric(signal::filtfilt(bf, x))
    }
    
    # 2. Down-sample to 30 Hz to avoid oversampling bias
    if (fs > 30) {
      dec_factor <- floor(fs / 30)
      if (dec_factor > 1) {
        x_filtered <- x_filtered[seq(1, length(x_filtered), by = dec_factor)]
        fs <- 30  # Update sampling frequency
      }
    }
    
    # Remove linear trend
    x_detrended <- x_filtered - mean(x_filtered)
    
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
    
    # Filter to max frequency and exclude 0 Hz (DC component)
    freq_mask <- freqs > 0 & freqs <= max_freq
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

#' Plot power spectrum
#' @param ps_data Power spectrum data frame
#' @param stack Whether to stack the spectra
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_power_spectrum <- function(ps_data, stack = TRUE, split_by = NULL, base_size = 10) {
  
  if (is.null(ps_data) || nrow(ps_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Determine grouping variable (the one that's not frequency, power, or split_by)
  group_var <- setdiff(names(ps_data), c("frequency", "power", split_by))
  if (length(group_var) == 0) {
    group_var <- NULL
  } else {
    group_var <- group_var[1]
  }
  
  # Create base plot
  p <- ggplot(ps_data, aes(x = frequency, y = power))
  
  if (stack && !is.null(group_var)) {
    # Stacked area chart
    p <- p + geom_area(aes_string(fill = group_var), position = "stack", alpha = 0.7)
  } else if (!is.null(group_var)) {
    # Line plot with different colors for each group
    p <- p + geom_line(aes_string(color = group_var), size = 1.2)
  } else {
    # Simple line plot
    p <- p + geom_line(size = 1.2, color = "steelblue")
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
