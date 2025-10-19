#' Core Complexity Metrics
#'
#' Optimized implementations of nonlinear dynamics complexity metrics
#' All functions are fully vectorized for maximum performance

#' Sample Entropy (Richman & Moorman 2000)
#' @param x numeric vector of time series data
#' @param m pattern length (default 2)
#' @param r tolerance for matching (default 0.2 * sd(x))
#' @return Sample entropy value
sampen <- function(x, m = 2, r = 0.2 * sd(x)) {
  if (length(x) < 10) {
    return(NA)
  }

  # Create embedded matrices
  patterns_m <- embed(x, m)
  patterns_m1 <- embed(x, m + 1)

  n_m <- nrow(patterns_m)
  n_m1 <- nrow(patterns_m1)

  # Fully vectorized distance calculations
  # Compute all pairwise distances at once using outer operations
  dist_matrix_m <- as.matrix(dist(patterns_m, method = "maximum"))
  dist_matrix_m1 <- as.matrix(dist(patterns_m1[1:n_m1, ], method = "maximum"))

  # Count matches (excluding diagonal)
  upper_tri_m <- upper.tri(dist_matrix_m)
  upper_tri_m1 <- upper.tri(dist_matrix_m1)

  matches_m <- sum(dist_matrix_m[upper_tri_m] <= r)
  matches_m1 <- sum(dist_matrix_m1[upper_tri_m1] <= r)

  if (matches_m == 0 || matches_m1 == 0) {
    return(NA)
  }

  total_pairs <- sum(upper_tri_m)
  phi_m <- matches_m / total_pairs
  phi_m1 <- matches_m1 / total_pairs

  return(-log(phi_m1 / phi_m))
}

#' Multiscale Entropy (Costa et al. 2002)
#' @param x numeric vector of time series data
#' @param max.scale maximum scale to compute (default 20)
#' @param m pattern length (default 2)
#' @param r tolerance for matching (default 0.2 * sd(x))
#' @return List with mse vector and complexity index
mse <- function(x, max.scale = 20, m = 2, r = 0.2 * sd(x)) {
  if (length(x) < 30) {
    return(list(mse = NA, complexity = NA))
  }

  N <- length(x)
  max.scale <- min(max.scale, floor(N / 10))

  # Vectorized coarse-graining and entropy calculation
  mse_values <- vapply(1:max.scale, function(scale) {
    if (scale == 1) {
      coarse_grained <- x
    } else {
      # Vectorized coarse-graining
      n_points <- floor(N / scale)
      indices <- rep(1:n_points, each = scale)[1:N]
      coarse_grained <- vapply(1:n_points, function(i) {
        start_idx <- (i - 1) * scale + 1
        end_idx <- min(i * scale, N)
        mean(x[start_idx:end_idx])
      }, FUN.VALUE = numeric(1))
    }
    sampen(coarse_grained, m, r)
  }, FUN.VALUE = numeric(1))

  complexity <- sum(mse_values[!is.na(mse_values)])
  return(list(mse = mse_values, complexity = complexity))
}

#' Detrended Fluctuation Analysis Alpha (Peng et al. 1994)
#' @param x numeric vector of time series data
#' @param min.box minimum box size (default 4)
#' @param max.frac maximum box size as fraction of series length (default 0.25)
#' @param n.points number of box sizes to use (default 15)
#' @return DFA alpha scaling exponent
dfa_alpha <- function(x, min.box = 4, max.frac = 0.25, n.points = 15) {
  if (length(x) < 20) {
    return(NA)
  }

  N <- length(x)
  y <- cumsum(x - mean(x))

  max.box <- floor(N * max.frac)
  box_sizes <- unique(floor(exp(seq(log(min.box), log(max.box), length.out = n.points))))

  # Fully vectorized fluctuation calculation
  fluctuations <- vapply(box_sizes, function(n) {
    n_boxes <- floor(N / n)
    if (n_boxes < 2) {
      return(NA_real_)
    }

    # Vectorized segmentation and detrending
    total_points <- n_boxes * n
    y_trimmed <- y[1:total_points]

    # Create design matrix for all segments at once
    time_vec <- rep(1:n, n_boxes)
    segment_vec <- rep(1:n_boxes, each = n)

    # Fit all trends simultaneously using grouped regression
    segments <- split(data.frame(y = y_trimmed, t = time_vec), segment_vec)

    F_n <- sum(vapply(segments, function(seg) {
      if (nrow(seg) == n) {
        fit <- lm.fit(cbind(1, seg$t), seg$y)
        sum((seg$y - fit$fitted.values)^2)
      } else {
        0
      }
    }, FUN.VALUE = numeric(1)))

    sqrt(F_n / total_points)
  }, FUN.VALUE = numeric(1))

  # Vectorized log-log fit
  valid <- !is.na(fluctuations) & fluctuations > 0
  if (sum(valid) < 3) {
    return(NA)
  }

  alpha <- lm.fit(cbind(1, log(box_sizes[valid])), log(fluctuations[valid]))$coefficients[2]
  return(alpha)
}

#' Largest Lyapunov Exponent (Rosenstein et al. 1993)
#' @param x numeric vector of time series data
#' @param emb.dim embedding dimension (default 3)
#' @param delay time delay (default 1)
#' @param fit.range range for linear fit as fraction of series (default c(0, 0.8))
#' @return Largest Lyapunov exponent
lyapunov <- function(x, emb.dim = 3, delay = 1, fit.range = c(0, 0.8)) {
  if (length(x) < 30) {
    return(NA)
  }

  # Vectorized embedding
  embedded <- embed(x, emb.dim * delay)[, seq(1, emb.dim * delay, by = delay)]
  n_points <- nrow(embedded)

  if (n_points < 10) {
    return(NA)
  }

  # Fully vectorized nearest neighbor search and divergence calculation
  # Compute all pairwise distances at once
  dist_matrix <- as.matrix(dist(embedded))

  # Exclude temporally close points
  temporal_mask <- abs(outer(1:n_points, 1:n_points, "-")) > max(3, n_points * 0.1)
  dist_matrix[!temporal_mask] <- Inf

  # Find nearest neighbors vectorized
  nn_indices <- apply(dist_matrix, 1, which.min)

  # Calculate divergences vectorized (exclude last point)
  valid_indices <- 1:(n_points - 1)
  current_indices <- valid_indices
  next_indices <- current_indices + 1
  nn_current <- nn_indices[current_indices]
  nn_next <- pmin(nn_current + 1, n_points)

  # Vectorized divergence calculation
  divergences <- sqrt(rowSums((embedded[next_indices, ] - embedded[nn_next, ])^2))

  # Remove invalid values
  valid_divergences <- divergences[is.finite(divergences) & divergences > 0]

  if (length(valid_divergences) < 5) {
    return(NA)
  }

  # Vectorized exponential fit
  log_div <- log(valid_divergences)
  time_steps <- seq_along(log_div)

  start_idx <- max(1, floor(length(log_div) * fit.range[1]))
  end_idx <- min(length(log_div), floor(length(log_div) * fit.range[2]))

  if (end_idx - start_idx < 3) {
    return(NA)
  }

  fit_indices <- start_idx:end_idx
  lambda <- lm.fit(cbind(1, time_steps[fit_indices]), log_div[fit_indices])$coefficients[2]

  return(lambda)
}

#' Power Spectral Density Analysis
#' @param x numeric vector of time series data
#' @param fs sampling frequency (default 30 Hz)
#' @param step_freq optional step frequency in Hz to define adaptive frequency bands
#' @param log_msg optional logging function for warnings
#' @return List with frequency metrics
psd_analysis <- function(x, fs = 30, step_freq = NULL, log_msg = NULL) {
  if (length(x) < 50) {
    return(list(
      peak_freq = NA,
      mean_freq = NA,
      spectral_entropy = NA,
      spectral_centroid = NA,
      spectral_bandwidth = NA,
      power_low = NA,
      power_mid = NA,
      power_high = NA
    ))
  }

  tryCatch(
    {
      # Remove linear trend
      x_detrended <- x - mean(x)

      # Apply window to reduce spectral leakage
      window_length <- length(x_detrended)
      window <- 0.54 - 0.46 * cos(2 * pi * (0:(window_length - 1)) / (window_length - 1)) # Hamming window
      x_windowed <- x_detrended * window

      # Compute FFT
      fft_result <- fft(x_windowed)
      power_spectrum <- abs(fft_result)^2

      # Frequency vector
      freqs <- seq(0, fs / 2, length.out = length(power_spectrum))

      # Only use positive frequencies (first half)
      n_half <- ceiling(length(power_spectrum) / 2)
      freqs <- freqs[1:n_half]
      power_spectrum <- power_spectrum[1:n_half]

      # Normalize power spectrum
      power_spectrum <- power_spectrum / sum(power_spectrum)

      # Peak frequency (frequency with maximum power)
      peak_idx <- which.max(power_spectrum)
      peak_freq <- freqs[peak_idx]

      # Mean frequency (weighted average)
      mean_freq <- sum(freqs * power_spectrum)

      # Spectral entropy (measure of spectral complexity)
      spectral_entropy <- -sum(power_spectrum * log(power_spectrum + .Machine$double.eps))

      # Spectral centroid (center of mass of the spectrum)
      spectral_centroid <- sum(freqs * power_spectrum) / sum(power_spectrum)

      # Spectral bandwidth (spread around centroid)
      spectral_bandwidth <- sqrt(sum(((freqs - spectral_centroid)^2) * power_spectrum))

      # Power in different frequency bands
      # If step frequency is provided, use adaptive bands based on gait characteristics
      # Otherwise, use fixed bands
      if (!is.null(step_freq) && is.finite(step_freq) && step_freq > 0.05 && step_freq < 15) {
        # Adaptive frequency bands based on step frequency:
        # Low: below step frequency (perturbation-related movements)
        # Mid: around step frequency (gait-related movements)
        # High: above 1.2x step frequency (noise and fast corrections)
        low_mask <- freqs >= 0.05 & freqs <= 0.8 * step_freq
        mid_mask <- freqs > 0.8 * step_freq & freqs <= 1.2 * step_freq
        high_mask <- freqs > 1.2 * step_freq & freqs <= 15
      } else {
        if (!is.null(log_msg)) {
          log_msg("WARN", "Using fixed frequency bands (step_freq invalid or out of range)")
        }
        # Fixed frequency bands (fallback):
        # Low frequency: 0.05-0.5 Hz (typical perturbation range)
        # Mid frequency: 0.5-2 Hz (walking-related)
        # High frequency: 2-15 Hz (noise and fast movements)
        low_mask <- freqs >= 0.05 & freqs <= 0.5
        mid_mask <- freqs > 0.5 & freqs <= 2
        high_mask <- freqs > 2 & freqs <= 15
      }

      power_low <- sum(power_spectrum[low_mask])
      power_mid <- sum(power_spectrum[mid_mask])
      power_high <- sum(power_spectrum[high_mask])

      return(list(
        peak_freq = peak_freq,
        mean_freq = mean_freq,
        spectral_entropy = spectral_entropy,
        spectral_centroid = spectral_centroid,
        spectral_bandwidth = spectral_bandwidth,
        power_low = power_low,
        power_mid = power_mid,
        power_high = power_high
      ))
    },
    error = function(e) {
      return(list(
        peak_freq = NA,
        mean_freq = NA,
        spectral_entropy = NA,
        spectral_centroid = NA,
        spectral_bandwidth = NA,
        power_low = NA,
        power_mid = NA,
        power_high = NA
      ))
    }
  )
}
