#' Complexity Metrics for Gait Analysis
#'
#' Functions to calculate various complexity metrics for heel-strike interval data
#' Based on established algorithms from nonlinear dynamics literature

#' Sample Entropy (Richman & Moorman 2000)
#' @param x numeric vector of time series data
#' @param m pattern length (default 2)
#' @param r tolerance for matching (default 0.2 * sd(x))
#' @return Sample entropy value
sampen <- function(x, m = 2, r = 0.2 * sd(x)) {
  if (length(x) < 10) {
    return(NA)
  }

  N <- length(x)
  patterns_m <- matrix(0, N - m + 1, m)
  patterns_m1 <- matrix(0, N - m, m + 1)

  # Create pattern matrices
  for (i in 1:(N - m + 1)) {
    patterns_m[i, ] <- x[i:(i + m - 1)]
  }

  for (i in 1:(N - m)) {
    patterns_m1[i, ] <- x[i:(i + m)]
  }

  # Count matches
  matches_m <- 0
  matches_m1 <- 0

  for (i in 1:(N - m)) {
    for (j in (i + 1):(N - m + 1)) {
      if (j <= (N - m)) {
        if (max(abs(patterns_m[i, ] - patterns_m[j, ])) <= r) {
          matches_m <- matches_m + 1
        }
        if (max(abs(patterns_m1[i, ] - patterns_m1[j, ])) <= r) {
          matches_m1 <- matches_m1 + 1
        }
      } else {
        if (max(abs(patterns_m[i, ] - patterns_m[j, ])) <= r) {
          matches_m <- matches_m + 1
        }
      }
    }
  }

  if (matches_m == 0 || matches_m1 == 0) {
    return(NA)
  }

  phi_m <- matches_m / ((N - m) * (N - m - 1) / 2)
  phi_m1 <- matches_m1 / ((N - m) * (N - m - 1) / 2)

  return(-log(phi_m1 / phi_m))
}

#' Approximate Entropy (Pincus 1991)
#' @param x numeric vector of time series data
#' @param m pattern length (default 2)
#' @param r tolerance for matching (default 0.2 * sd(x))
#' @return Approximate entropy value
apen <- function(x, m = 2, r = 0.2 * sd(x)) {
  if (length(x) < 10) {
    return(NA)
  }

  N <- length(x)

  # Function to calculate phi
  phi <- function(m) {
    patterns <- matrix(0, N - m + 1, m)
    for (i in 1:(N - m + 1)) {
      patterns[i, ] <- x[i:(i + m - 1)]
    }

    C <- numeric(N - m + 1)
    for (i in 1:(N - m + 1)) {
      matches <- 0
      for (j in 1:(N - m + 1)) {
        if (max(abs(patterns[i, ] - patterns[j, ])) <= r) {
          matches <- matches + 1
        }
      }
      C[i] <- matches / (N - m + 1)
    }

    return(mean(log(C)))
  }

  return(phi(m) - phi(m + 1))
}

#' Multiscale Entropy + Complexity Index (Costa 2002)
#' @param x numeric vector of time series data
#' @param max.scale maximum scale to compute (default 30)
#' @param m pattern length (default 2)
#' @param r tolerance for matching (default 0.2 * sd(x))
#' @return List with mse vector and complexity index
mse <- function(x, max.scale = 30, m = 2, r = 0.2 * sd(x)) {
  if (length(x) < 30) {
    return(list(mse = NA, complexity = NA))
  }

  N <- length(x)
  max.scale <- min(max.scale, floor(N / 10))
  mse_values <- numeric(max.scale)

  for (scale in 1:max.scale) {
    if (scale == 1) {
      coarse_grained <- x
    } else {
      # Coarse-graining
      n_points <- floor(N / scale)
      coarse_grained <- numeric(n_points)
      for (i in 1:n_points) {
        start_idx <- (i - 1) * scale + 1
        end_idx <- i * scale
        coarse_grained[i] <- mean(x[start_idx:end_idx])
      }
    }

    mse_values[scale] <- sampen(coarse_grained, m, r)
  }

  # Complexity index (sum of all valid MSE values)
  complexity <- sum(mse_values[!is.na(mse_values)])

  return(list(mse = mse_values, complexity = complexity))
}

#' Detrended Fluctuation Analysis α (Peng 1995)
#' @param x numeric vector of time series data
#' @param min.box minimum box size (default 4)
#' @param max.frac maximum box size as fraction of series length (default 0.25)
#' @param n.points number of box sizes to use (default 20)
#' @return DFA alpha scaling exponent
dfa_alpha <- function(x, min.box = 4, max.frac = 0.25, n.points = 20) {
  if (length(x) < 20) {
    return(NA)
  }

  N <- length(x)

  # Integrate the profile
  y <- cumsum(x - mean(x))

  # Box sizes
  max.box <- floor(N * max.frac)
  box_sizes <- unique(floor(exp(seq(log(min.box), log(max.box), length.out = n.points))))

  fluctuations <- numeric(length(box_sizes))

  for (i in seq_along(box_sizes)) {
    n <- box_sizes[i]
    n_boxes <- floor(N / n)

    if (n_boxes < 2) {
      fluctuations[i] <- NA
      next
    }

    F_n <- 0
    for (j in 1:n_boxes) {
      start_idx <- (j - 1) * n + 1
      end_idx <- j * n

      # Fit linear trend
      t <- 1:n
      y_segment <- y[start_idx:end_idx]
      trend <- lm(y_segment ~ t)$fitted.values

      # Calculate fluctuation
      F_n <- F_n + sum((y_segment - trend)^2)
    }

    fluctuations[i] <- sqrt(F_n / (n_boxes * n))
  }

  # Fit log-log relationship
  valid_indices <- !is.na(fluctuations) & fluctuations > 0
  if (sum(valid_indices) < 3) {
    return(NA)
  }

  log_n <- log(box_sizes[valid_indices])
  log_F <- log(fluctuations[valid_indices])

  alpha <- lm(log_F ~ log_n)$coefficients[2]

  return(alpha)
}

#' Largest Lyapunov Exponent (Rosenstein 1993)
#' @param x numeric vector of time series data
#' @param emb.dim embedding dimension (default 5)
#' @param delay time delay (default 1)
#' @param fit.range range for linear fit as fraction of series (default c(0, 1))
#' @return Largest Lyapunov exponent
lyapunov <- function(x, emb.dim = 5, delay = 1, fit.range = c(0, 1)) {
  if (length(x) < 50) {
    return(NA)
  }

  N <- length(x)
  M <- N - (emb.dim - 1) * delay

  if (M < 10) {
    return(NA)
  }

  # Embed the time series
  embedded <- matrix(0, M, emb.dim)
  for (i in 1:M) {
    for (j in 1:emb.dim) {
      embedded[i, j] <- x[i + (j - 1) * delay]
    }
  }

  # Find nearest neighbors
  divergences <- numeric(M - 1)

  for (i in 1:(M - 1)) {
    distances <- numeric(M)
    for (j in 1:M) {
      distances[j] <- sqrt(sum((embedded[i, ] - embedded[j, ])^2))
    }

    # Find nearest neighbor (excluding self and temporally correlated points)
    valid_indices <- which(abs(1:M - i) > mean(abs(1:M - i)) * 0.1)
    if (length(valid_indices) < 2) {
      divergences[i] <- NA
      next
    }

    nn_idx <- valid_indices[which.min(distances[valid_indices])]

    # Calculate divergence
    if (i + 1 <= M && nn_idx + 1 <= M) {
      divergences[i] <- sqrt(sum((embedded[i + 1, ] - embedded[nn_idx + 1, ])^2))
    } else {
      divergences[i] <- NA
    }
  }

  # Remove invalid divergences
  valid_divergences <- divergences[!is.na(divergences) & divergences > 0]

  if (length(valid_divergences) < 10) {
    return(NA)
  }

  # Fit exponential growth
  log_div <- log(valid_divergences)
  time_steps <- 1:length(log_div)

  # Use specified range for fitting
  start_idx <- max(1, floor(length(log_div) * fit.range[1]))
  end_idx <- min(length(log_div), floor(length(log_div) * fit.range[2]))

  if (end_idx - start_idx < 3) {
    return(NA)
  }

  fit_range_idx <- start_idx:end_idx
  lambda <- lm(log_div[fit_range_idx] ~ time_steps[fit_range_idx])$coefficients[2]

  return(lambda)
}

#' Poincaré SD1 / SD2
#' @param x numeric vector of time series data
#' @return List with sd1, sd2, and sd1/sd2 ratio
poincare_sd <- function(x) {
  if (length(x) < 3) {
    return(list(sd1 = NA, sd2 = NA, sd1_sd2 = NA))
  }

  diff_x <- diff(x)
  sd1 <- sqrt(var(diff_x) / 2)
  sd2 <- sqrt(2 * var(x) - 0.5 * var(diff_x))

  if (sd2 == 0) {
    sd1_sd2 <- NA
  } else {
    sd1_sd2 <- sd1 / sd2
  }

  return(list(sd1 = sd1, sd2 = sd2, sd1_sd2 = sd1_sd2))
}

#' Wrapper returning a named list ready to c() onto gaitParams
#' @param stride_int numeric vector of stride intervals (outliers should be pre-filtered)
#' @return Named list of complexity metrics
compute_complexity <- function(stride_int) {
  # Initialize return list with NAs
  na_result <- list(
    sampen = NA,
    apen = NA,
    mse_index = NA,
    dfa_alpha = NA,
    lyapunov = NA,
    sd1 = NA,
    sd2 = NA,
    sd1_sd2 = NA
  )

  # Basic length check
  if (length(stride_int) < 10) {
    return(na_result)
  }

  # Handle any remaining NA or infinite values
  stride_int <- stride_int[is.finite(stride_int)]

  # Recheck length after filtering
  if (length(stride_int) < 10) {
    return(na_result)
  }

  # Additional robustness: check for constant or near-constant series
  if (sd(stride_int) < .Machine$double.eps * 10) {
    return(na_result)
  }

  # Calculate complexity metrics with error handling
  tryCatch(
    {
      pc <- poincare_sd(stride_int)
      mse_result <- mse(stride_int)

      return(list(
        sampen = sampen(stride_int),
        apen = apen(stride_int),
        mse_index = mse_result$complexity,
        dfa_alpha = dfa_alpha(stride_int),
        lyapunov = lyapunov(stride_int),
        sd1 = pc$sd1,
        sd2 = pc$sd2,
        sd1_sd2 = pc$sd1_sd2
      ))
    },
    error = function(e) {
      # If any error occurs during calculation, return NAs
      warning(paste("Error in complexity calculation:", e$message))
      return(na_result)
    }
  )
}

#' Compute complexity metrics for all participants and trials (Enhanced Version)
#' @param allGaitParams Full gait parameter dataset
#' @param categories Grouping categories (default c("participant","condition","trialNum"))
#' @param outlier_col_names Vector of potential outlier column names to check (default c("outlierSteps", "heelStrikes.outlierSteps"))
#' @param debug Logical, whether to print debug information during processing (default FALSE)
#' @return Data frame with complexity metrics for step times and widths
get_all_complexity_metrics <- function(allGaitParams,
                                       categories = c("participant", "condition", "trialNum"),
                                       outlier_col_names = c("outlierSteps", "heelStrikes.outlierSteps"),
                                       debug = FALSE) {
  # Debug helper function
  debug_log <- function(...) {
    if (debug) {
      cat("[DEBUG]", ..., "\n")
      flush.console()
    }
  }

  # Remove foot-related categories as they're not relevant for complexity metrics
  categories_no_foot <- setdiff(categories, "heelStrikes.foot")

  debug_log("Starting complexity analysis with categories:", paste(categories_no_foot, collapse = ", "))

  # Check required columns
  if (!all(categories_no_foot %in% colnames(allGaitParams))) {
    stop("allGaitParams must contain all specified category columns")
  }

  # Dynamically detect available complexity types (best of function 1)
  complexity_types <- intersect(c("stepTimes", "stepWidths"), colnames(allGaitParams))

  if (length(complexity_types) == 0) {
    stop("No valid complexity data types (stepTimes, stepWidths) found in allGaitParams")
  }

  debug_log("Found complexity types:", paste(complexity_types, collapse = ", "))
  debug_log("Looking for outlier columns:", paste(outlier_col_names, collapse = ", "))

  # Process each complexity type
  results <- lapply(complexity_types, function(dataType) {
    debug_log("Processing complexity type:", dataType)

    allGaitParams %>%
      group_by(across(all_of(categories_no_foot))) %>%
      group_modify(~ {
        # Get group identifiers for debug logging
        group_info <- .y[1, ]
        group_str <- paste(names(group_info), group_info, sep = "=", collapse = ", ")
        debug_log("  Processing group:", group_str)

        values <- .x[[dataType]]
        original_length <- length(values)
        debug_log("    Original", dataType, "length:", original_length)

        # Robust outlier filtering (best of function 2, but parameterized)
        outlier_col_found <- NULL
        for (col_name in outlier_col_names) {
          if (col_name %in% colnames(.x)) {
            outlier_col_found <- col_name
            break
          }
        }

        if (!is.null(outlier_col_found)) {
          debug_log("    Found outlier column:", outlier_col_found)
          # Filter out outliers (keep only non-outliers)
          outlier_mask <- .x[[outlier_col_found]]
          if (is.logical(outlier_mask)) {
            values <- values[!outlier_mask]
          } else {
            # Handle case where outlier column might be non-logical
            values <- values[outlier_mask == FALSE]
          }
          filtered_length <- length(values)
          outliers_removed <- original_length - filtered_length
          debug_log("    After outlier filtering:", filtered_length, "values (removed", outliers_removed, "outliers)")
        } else {
          debug_log("    No outlier column found - using all values")
        }

        # Check for sufficient data
        if (length(values) < 10) {
          debug_log("    WARNING: Insufficient data for complexity calculation (", length(values), "values)")
        }

        # Calculate complexity metrics
        debug_log("    Computing complexity metrics...")
        complexity_result <- compute_complexity(values)

        # Check for NA results
        na_count <- sum(sapply(complexity_result, is.na))
        if (na_count > 0) {
          debug_log("    WARNING:", na_count, "out of", length(complexity_result), "complexity metrics returned NA")
        } else {
          debug_log("    All complexity metrics computed successfully")
        }

        as.data.frame(complexity_result)
      }) %>%
      ungroup() %>%
      rename_with(~ paste0(dataType, "_complexity.", .x), -all_of(categories_no_foot))
  })

  names(results) <- complexity_types

  # Combine all results
  debug_log("Combining results from", length(complexity_types), "complexity types...")
  final_result <- reduce(results, left_join, by = categories_no_foot)

  debug_log("Complexity analysis complete!")
  debug_log("Final result dimensions:", nrow(final_result), "rows x", ncol(final_result), "columns")

  return(final_result)
}

#' Merge summarized gait data with complexity metrics
#' @param mu_gait Summarized gait data
#' @param complexity_data Precomputed complexity metrics
#' @return Combined data frame
merge_mu_with_complexity <- function(mu_gait, complexity_data) {
  if (is.null(complexity_data) || nrow(complexity_data) == 0) {
    message("No complexity data available for merging")
    return(mu_gait)
  }
  if (is.null(mu_gait) || nrow(mu_gait) == 0) {
    message("No gait data available for merging")
    return(complexity_data)
  }
  mu_gait_copy <- mu_gait
  complexity_copy <- complexity_data
  mu_gait_copy$trialNum <- as.numeric(as.character(mu_gait_copy$trialNum))
  complexity_copy$trialNum <- as.numeric(as.character(complexity_copy$trialNum))
  full_join(mu_gait_copy, complexity_copy, by = c("participant", "trialNum"))
}
