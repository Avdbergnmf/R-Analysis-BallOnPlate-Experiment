#' Power Spectrum Computation Functions
#'
#' Core functions for computing power spectral density from time series data

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

#' Apply Butterworth low-pass filter
#' @param x Input signal
#' @param fs Sampling frequency
#' @param cutoff_freq Cutoff frequency in Hz
#' @param order Filter order
#' @return Filtered signal
apply_lowpass_filter <- function(x, fs, cutoff_freq = 6, order = 4) {
  cutoff <- cutoff_freq / (fs / 2)
  if (cutoff >= 1) {
    return(x)
  }
  
  tryCatch({
    bf <- signal::butter(order, cutoff, type = "low")
    as.numeric(signal::filtfilt(bf, x))
  }, error = function(e) {
    x  # Return original if filtering fails
  })
}

#' Downsample signal to target frequency
#' @param x Input signal
#' @param fs_original Original sampling frequency
#' @param fs_target Target sampling frequency
#' @return List with downsampled signal and new sampling frequency
downsample_signal <- function(x, fs_original, fs_target = 30) {
  if (fs_original <= fs_target) {
    return(list(signal = x, fs = fs_original))
  }
  
  dec_factor <- floor(fs_original / fs_target)
  if (dec_factor > 1) {
    downsampled <- x[seq(1, length(x), by = dec_factor)]
    return(list(signal = downsampled, fs = fs_target))
  }
  
  return(list(signal = x, fs = fs_original))
}

#' Apply Hamming window to signal
#' @param x Input signal
#' @return Windowed signal
apply_hamming_window <- function(x) {
  window_length <- length(x)
  window <- 0.54 - 0.46 * cos(2 * pi * (0:(window_length - 1)) / (window_length - 1))
  x * window
}

#' Compute FFT and extract power spectrum
#' @param x Windowed signal
#' @param fs Sampling frequency
#' @param max_freq Maximum frequency to return
#' @return List with frequency and power vectors
compute_fft_psd <- function(x, fs, max_freq = 2.5) {
  # Compute FFT
  fft_result <- fft(x)
  power_spectrum <- abs(fft_result)^2
  
  # Frequency vector (only positive frequencies)
  n_half <- ceiling(length(power_spectrum) / 2)
  freqs <- seq(0, fs / 2, length.out = n_half)
  power_spectrum <- power_spectrum[1:n_half]
  
  # Filter to max frequency and exclude 0 Hz (DC component)
  freq_mask <- freqs > 0 & freqs <= max_freq
  freqs <- freqs[freq_mask]
  power_spectrum <- power_spectrum[freq_mask]
  
  list(frequency = freqs, power = power_spectrum)
}
