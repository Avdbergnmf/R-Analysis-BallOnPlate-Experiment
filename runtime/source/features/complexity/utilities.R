#' Complexity Utilities
#'
#' Helper functions for complexity metric calculations

#' Helper function to create metric column names
#' @param variable Variable name (e.g., "stepTimes")
#' @param metric Metric name (e.g., "sampen")
#' @return Column name as character, e.g., "stepTimes_sampen"
get_metric_name <- function(variable, metric) {
  paste0(variable, "_", metric)
}

#' Calculate basic summary statistics for a vector
#' @param values Numeric vector of data values
#' @param prefix Column name prefix (e.g., "p_raw", "stepTimes")
#' @return Named list with formatted column names
calculate_summary_stats <- function(values, prefix) {
  values <- values[is.finite(values)]
  n <- length(values)

  if (n == 0) {
    stats <- list(NA_real_, NA_real_, NA_real_, 0L)
  } else {
    mean_val <- mean(values)
    sd_val <- sd(values)
    cv_val <- sd_val / abs(mean_val)
    stats <- list(mean_val, sd_val, cv_val, n)
  }

  # Return with formatted names
  names(stats) <- paste0(prefix, c("_mean", "_sd", "_cv", "_n"))
  stats
}

#' Apply outlier filtering to values
#' @param values Numeric vector to filter
#' @param trial_data Trial data frame containing outlier columns
#' @param outlier_col_names Vector of potential outlier column names to check
#' @param log_msg Logging function (optional)
#' @return Filtered values vector
apply_outlier_filtering <- function(values, trial_data, outlier_col_names, log_msg = NULL) {
  # Find outlier column
  outlier_col_found <- NULL
  for (col_name in outlier_col_names) {
    if (col_name %in% colnames(trial_data)) {
      outlier_col_found <- col_name
      break
    }
  }

  if (!is.null(outlier_col_found)) {
    if (!is.null(log_msg)) {
      log_msg("DEBUG", "    Using outlier column:", outlier_col_found)
    }

    outlier_mask <- trial_data[[outlier_col_found]]
    if (is.logical(outlier_mask)) {
      values <- values[!outlier_mask]
    } else {
      values <- values[outlier_mask == FALSE]
    }

    if (!is.null(log_msg)) {
      log_msg("DEBUG", "    After filtering:", length(values), "values")
    }
  }

  return(values)
}

#' Helper function to add discrete complexity metrics to result
#' @param result Result data frame to modify
#' @param dataType Type of data (e.g., "stepTimes", "stepWidths")
#' @param values Numeric vector of data values
#' @param log_msg Logging function
#' @return Modified result data frame
add_discrete_complexity <- function(result, dataType, values, log_msg) {
  # Clean data first - remove NA and infinite values
  values <- values[is.finite(values)]

  # Add basic summary statistics
  stats <- calculate_summary_stats(values, dataType)
  for (name in names(stats)) {
    result[[name]] <- stats[[name]]
  }

  # Check if we have sufficient valid data for complexity metrics
  if (length(values) >= 10) {
    values_sd <- sd(values)
    if (is.finite(values_sd) && values_sd > .Machine$double.eps * 10) {
      tryCatch(
        {
          # Adaptive MSE scales based on data length
          max_scale <- if (length(values) > 1000) 10 else 20
          mse_result <- mse(values, max.scale = max_scale)

          # Calculate metrics
          metrics <- list(
            sampen = sampen(values),
            mse_index = mse_result$complexity,
            dfa_alpha = dfa_alpha(values),
            lyapunov = lyapunov(values)
          )

          # Add to result with proper naming
          for (metric in names(metrics)) {
            result[[get_metric_name(dataType, metric)]] <- metrics[[metric]]
          }

          log_msg("DEBUG", "    Computed", length(metrics), "discrete complexity metrics")
        },
        error = function(e) {
          warning(paste("Error in", dataType, "complexity calculation:", e$message))
          # Add NAs for failed metrics
          na_metrics <- c("sampen", "mse_index", "dfa_alpha", "lyapunov")
          for (metric in na_metrics) {
            result[[get_metric_name(dataType, metric)]] <- NA
          }
        }
      )
    } else {
      # Insufficient variability - add NAs
      na_metrics <- c("sampen", "mse_index", "dfa_alpha", "lyapunov")
      for (metric in na_metrics) {
        result[[get_metric_name(dataType, metric)]] <- NA
      }
      log_msg("DEBUG", "    Insufficient variability for complexity calculation")
    }
  } else {
    # Insufficient data - add NAs
    na_metrics <- c("sampen", "mse_index", "dfa_alpha", "lyapunov")
    for (metric in na_metrics) {
      result[[get_metric_name(dataType, metric)]] <- NA
    }
    log_msg("DEBUG", "    Insufficient data for complexity calculation (", length(values), "valid values)")
  }

  return(result)
}

#' Helper function to add continuous complexity metrics to result
#' @param result Result data frame to modify
#' @param var_name Variable name (e.g., "p")
#' @param values Numeric vector of filtered data values
#' @param log_msg Logging function
#' @param step_freq Optional step frequency in Hz for adaptive frequency bands
#' @param values_raw Optional raw unfiltered data values for basic statistics
#' @return Modified result data frame
add_continuous_complexity <- function(result, var_name, values, log_msg, step_freq = NULL, values_raw = NULL) {
  # Add basic summary statistics on raw data (if provided)
  if (!is.null(values_raw)) {
    stats_raw <- calculate_summary_stats(values_raw, paste0(var_name, "_raw"))
    for (name in names(stats_raw)) {
      result[[name]] <- stats_raw[[name]]
    }
  }

  # Clean filtered data - remove NA and infinite values
  values <- values[is.finite(values)]

  # Add basic summary statistics on filtered/downsampled data
  stats_filt <- calculate_summary_stats(values, paste0(var_name, "_filt"))
  for (name in names(stats_filt)) {
    result[[name]] <- stats_filt[[name]]
  }

  # Check if we have sufficient valid data for complexity metrics
  if (length(values) >= 50) {
    values_sd <- sd(values)
    if (is.finite(values_sd) && values_sd > .Machine$double.eps * 10) {
      tryCatch(
        {
          # Smart downsampling for very long series
          if (length(values) > 2000) {
            downsample_factor <- ceiling(length(values) / 1500)
            values <- values[seq(1, length(values), by = downsample_factor)]
          }

          # Compute multiscale entropy index
          max_scale <- if (length(values) > 1000) 10 else 20
          mse_result <- mse(values, max.scale = max_scale)

          # Optimized metrics for continuous data (now including MSE index)
          metrics <- list(
            sampen = sampen(values, m = 2, r = 0.15 * sd(values)),
            mse_index = mse_result$complexity,
            dfa_alpha = dfa_alpha(values, n.points = 8),
            lyapunov = lyapunov(values),
            sd_ratio = {
              diff_var <- var(diff(values))
              total_var <- var(values)
              if (total_var > 0) sqrt(diff_var / total_var) else NA
            }
          )

          # Add frequency domain analysis with adaptive frequency bands
          psd_metrics <- psd_analysis(values, fs = 30, step_freq = step_freq, log_msg = log_msg)
          metrics <- c(metrics, psd_metrics)

          # Add to result with proper naming
          for (metric in names(metrics)) {
            result[[get_metric_name(var_name, metric)]] <- metrics[[metric]]
          }

          log_msg("DEBUG", "      Computed", length(metrics), "continuous complexity metrics")
        },
        error = function(e) {
          warning(paste("Error in continuous", var_name, "complexity calculation:", e$message))
          # Add NAs for failed metrics
          na_metrics <- c(
            "sampen", "mse_index", "dfa_alpha", "lyapunov", "sd_ratio",
            "peak_freq", "mean_freq", "spectral_entropy", "spectral_centroid",
            "spectral_bandwidth", "power_low", "power_mid", "power_high"
          )
          for (metric in na_metrics) {
            result[[get_metric_name(var_name, metric)]] <- NA
          }
        }
      )
    } else {
      # Insufficient variability - add NAs
      na_metrics <- c(
        "sampen", "mse_index", "dfa_alpha", "lyapunov", "sd_ratio",
        "peak_freq", "mean_freq", "spectral_entropy", "spectral_centroid",
        "spectral_bandwidth", "power_low", "power_mid", "power_high"
      )
      for (metric in na_metrics) {
        result[[get_metric_name(var_name, metric)]] <- NA
      }
      log_msg("DEBUG", "      Insufficient variability for continuous complexity calculation")
    }
  } else if (length(values) >= 10) {
    values_sd <- sd(values)
    if (is.finite(values_sd) && values_sd > .Machine$double.eps * 10) {
      # Fall back to standard metrics for shorter series
      tryCatch(
        {
          max_scale <- if (length(values) > 1000) 10 else 20
          mse_result <- mse(values, max.scale = max_scale)

          metrics <- list(
            sampen = sampen(values),
            mse_index = mse_result$complexity,
            dfa_alpha = dfa_alpha(values),
            lyapunov = lyapunov(values)
          )

          # Add to result with proper naming
          for (metric in names(metrics)) {
            result[[get_metric_name(var_name, metric)]] <- metrics[[metric]]
          }

          log_msg("DEBUG", "      Computed", length(metrics), "standard complexity metrics")
        },
        error = function(e) {
          warning(paste("Error in continuous", var_name, "complexity calculation:", e$message))
          # Add NAs for failed metrics
          na_metrics <- c("sampen", "mse_index", "dfa_alpha", "lyapunov")
          for (metric in na_metrics) {
            result[[get_metric_name(var_name, metric)]] <- NA
          }
        }
      )
    } else {
      # Insufficient variability - add NAs
      na_metrics <- c("sampen", "mse_index", "dfa_alpha", "lyapunov")
      for (metric in na_metrics) {
        result[[get_metric_name(var_name, metric)]] <- NA
      }
      log_msg("DEBUG", "      Insufficient variability for standard complexity calculation")
    }
  } else {
    # Insufficient data - add NAs for continuous metrics
    na_metrics <- c(
      "sampen", "mse_index", "dfa_alpha", "lyapunov", "sd_ratio",
      "peak_freq", "mean_freq", "spectral_entropy", "spectral_centroid",
      "spectral_bandwidth", "power_low", "power_mid", "power_high"
    )
    for (metric in na_metrics) {
      result[[get_metric_name(var_name, metric)]] <- NA
    }
    log_msg("DEBUG", "      Insufficient data for complexity calculation (", length(values), "valid values)")
  }

  return(result)
}

#' Helper function to process a continuous signal with filtering and downsampling
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param tracker_type Type of tracker (e.g., "udp", "hip", "sim")
#' @param column_name Name of the column to extract (e.g., "PelvisPos", "pos_x")
#' @param var_name Variable name for output columns (e.g., "hipPos", "pelvisPos")
#' @param description Human-readable description for logging
#' @param result Result data frame to add metrics to
#' @param log_msg Logging function
#' @param step_freq Optional step frequency in Hz for adaptive frequency bands
#' @return Modified result data frame with complexity metrics added
process_continuous_signal <- function(participant, trial, tracker_type, column_name,
                                      var_name, description, result, log_msg, step_freq = NULL) {
  tryCatch(
    {
      # Get signal data from appropriate source
      signal_data <- if (tracker_type == "sim") {
        get_simulation_signal(participant, trial, column_name)
      } else {
        get_tracker_signal(participant, trial, tracker_type, column_name, description)
      }

      if (length(signal_data$values) > 0) {
        log_msg("DEBUG", "    ", description, "available:", signal_data$n_valid, "valid samples")

        # Store raw values for statistics
        values_raw <- signal_data$values

        # Get sampling frequency
        fs <- if (is.finite(signal_data$fs)) signal_data$fs else 120

        # Apply filtering and downsampling
        values_filt <- apply_signal_filtering(values_raw, fs, log_msg)
        log_msg("DEBUG", "      Processed data length:", length(values_filt), "values")

        # Calculate complexity metrics with adaptive frequency bands (pass raw values too)
        result <- add_continuous_complexity(result, var_name, values_filt, log_msg, step_freq, values_raw)
      } else {
        log_msg("DEBUG", "    No", description, "available")
      }
    },
    error = function(e) {
      log_msg("ERROR", "    Error getting", description, ":", e$message)
    }
  )

  return(result)
}
