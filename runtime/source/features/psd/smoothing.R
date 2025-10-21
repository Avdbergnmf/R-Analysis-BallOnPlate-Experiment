#' Power Spectrum Smoothing Functions
#'
#' Functions for smoothing and post-processing power spectral density data

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

#' Apply moving average smoothing to power spectrum
#' @param psd_df Data frame with frequency and power columns
#' @param window_size Size of moving average window
#' @return Data frame with smoothed power
smooth_psd_moving_average <- function(psd_df, window_size = 5) {
  if (is.null(psd_df) || nrow(psd_df) < window_size) {
    return(psd_df)
  }
  
  tryCatch({
    # Apply moving average
    psd_df$power <- stats::filter(psd_df$power, rep(1/window_size, window_size), sides = 2)
    psd_df$power[is.na(psd_df$power)] <- 0  # Handle edge effects
    
    psd_df
  }, error = function(e) {
    psd_df
  })
}

#' Apply Gaussian kernel smoothing to power spectrum
#' @param psd_df Data frame with frequency and power columns
#' @param sigma Standard deviation of Gaussian kernel
#' @return Data frame with smoothed power
smooth_psd_gaussian <- function(psd_df, sigma = 0.05) {
  if (is.null(psd_df) || nrow(psd_df) < 10) {
    return(psd_df)
  }
  
  tryCatch({
    # Create Gaussian kernel
    freq_range <- max(psd_df$frequency) - min(psd_df$frequency)
    kernel_width <- sigma * freq_range
    
    # Apply kernel smoothing
    smoothed_power <- numeric(nrow(psd_df))
    for (i in seq_len(nrow(psd_df))) {
      weights <- dnorm(psd_df$frequency, psd_df$frequency[i], kernel_width)
      weights <- weights / sum(weights)  # Normalize weights
      smoothed_power[i] <- sum(psd_df$power * weights)
    }
    
    psd_df$power <- smoothed_power
    psd_df
  }, error = function(e) {
    psd_df
  })
}

#' Normalize power spectrum
#' @param psd_df Data frame with frequency and power columns
#' @param method Normalization method: "sum", "max", or "area"
#' @return Data frame with normalized power
normalize_psd <- function(psd_df, method = "sum") {
  if (is.null(psd_df) || nrow(psd_df) == 0) {
    return(psd_df)
  }
  
  tryCatch({
    if (method == "sum") {
      power_sum <- sum(psd_df$power)
      if (power_sum > 0) {
        psd_df$power <- psd_df$power / power_sum
      }
    } else if (method == "max") {
      power_max <- max(psd_df$power)
      if (power_max > 0) {
        psd_df$power <- psd_df$power / power_max
      }
    } else if (method == "area") {
      # Normalize by area under the curve
      freq_diff <- diff(psd_df$frequency)
      if (length(freq_diff) > 0) {
        area <- sum(psd_df$power[-1] * freq_diff)
        if (area > 0) {
          psd_df$power <- psd_df$power / area
        }
      }
    }
    
    psd_df
  }, error = function(e) {
    psd_df
  })
}
