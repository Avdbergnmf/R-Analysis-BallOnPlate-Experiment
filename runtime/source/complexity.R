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

#' Calculate complexity metrics for a single participant/trial combination
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param allGaitParams Full gait parameter dataset
#' @param outlier_col_names Vector of potential outlier column names to check
#' @param debug Logical, whether to print debug information
#' @return Single row data frame with all complexity metrics
calculate_complexity_single <- function(participant, trial, allGaitParams,
                                        outlier_col_names = c("outlierSteps", "heelStrikes.outlierSteps"),
                                        debug = FALSE) {
  # Debug helper function
  debug_log <- function(...) {
    if (debug) {
      cat("[DEBUG]", ..., "\n")
      flush.console()
    }
  }

  debug_log("Processing participant:", participant, "trial:", trial)

  # Filter data for this specific participant/trial
  trial_data <- allGaitParams %>%
    filter(participant == !!participant, trialNum == !!trial)

  if (nrow(trial_data) == 0) {
    debug_log("  No data found for this combination")
    return(data.frame(participant = participant, trialNum = trial))
  }

  # Get condition (assuming it's consistent within a trial)
  condition <- unique(trial_data$condition)[1]

  # Initialize result with identifiers
  result <- data.frame(
    participant = participant,
    condition = condition,
    trialNum = trial
  )

  # Process each complexity type
  complexity_types <- intersect(c("stepTimes", "stepWidths"), colnames(trial_data))

  for (dataType in complexity_types) {
    debug_log("  Processing", dataType)

    values <- trial_data[[dataType]]
    original_length <- length(values)
    debug_log("    Original length:", original_length)

    # Apply outlier filtering
    outlier_col_found <- NULL
    for (col_name in outlier_col_names) {
      if (col_name %in% colnames(trial_data)) {
        outlier_col_found <- col_name
        break
      }
    }

    if (!is.null(outlier_col_found)) {
      debug_log("    Using outlier column:", outlier_col_found)
      outlier_mask <- trial_data[[outlier_col_found]]
      if (is.logical(outlier_mask)) {
        values <- values[!outlier_mask]
      } else {
        values <- values[outlier_mask == FALSE]
      }
      debug_log("    After filtering:", length(values), "values")
    }

    # Calculate complexity metrics
    complexity_result <- compute_complexity(values)

    # Add to result with proper naming
    for (metric_name in names(complexity_result)) {
      result[[paste0(dataType, "_complexity.", metric_name)]] <- complexity_result[[metric_name]]
    }

    debug_log("    Computed", length(complexity_result), "complexity metrics")
  }

  return(result)
}



#' Complexity calculation function designed for get_data_from_loop framework
#' This function signature matches what get_data_from_loop expects
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param ... Additional arguments passed from get_data_from_loop
#' @return Single row data frame with complexity metrics
calc_complexity_for_loop <- function(participant, trial, ...) {
  # Get additional arguments
  args <- list(...)

  # Extract parameters, with defaults
  allGaitParams <- args$allGaitParams
  outlier_col_names <- if (is.null(args$outlier_col_names)) c("outlierSteps", "heelStrikes.outlierSteps") else args$outlier_col_names
  debug <- if (is.null(args$debug)) FALSE else args$debug

  if (is.null(allGaitParams)) {
    stop("allGaitParams must be provided in the ... arguments")
  }

  # Use the existing single calculation function
  result <- calculate_complexity_single(participant, trial, allGaitParams, outlier_col_names, debug)

  return(result)
}

#' Main complexity calculation function using get_data_from_loop framework
#' @param allGaitParams Full gait parameter dataset
#' @param outlier_col_names Vector of potential outlier column names to check
#' @param debug Logical, whether to print debug information
#' @param use_parallel Logical, whether to use parallel processing (default TRUE)
#' @return Data frame with complexity metrics for all valid combinations
get_all_complexity_metrics <- function(allGaitParams,
                                       outlier_col_names = c("outlierSteps", "heelStrikes.outlierSteps"),
                                       debug = FALSE,
                                       use_parallel = TRUE) {
  # Debug helper function
  debug_log <- function(...) {
    if (debug) {
      cat("[DEBUG]", ..., "\n")
      flush.console()
    }
  }

  debug_log("Starting complexity analysis using get_data_from_loop framework")
  debug_log("Parallel processing:", use_parallel)

  # Store original participants and allTrials if they exist, or get from data
  if (!exists("participants", envir = .GlobalEnv)) {
    participants <<- unique(allGaitParams$participant)
    debug_log("Set participants from data:", paste(participants, collapse = ", "))
  }

  if (!exists("allTrials", envir = .GlobalEnv)) {
    allTrials <<- unique(allGaitParams$trialNum)
    debug_log("Set allTrials from data:", paste(allTrials, collapse = ", "))
  }

  # Use the appropriate loop function from your framework
  if (use_parallel) {
    debug_log("Using get_data_from_loop_parallel")

    # For parallel processing, we need to bypass the file verification
    # since we're working with in-memory data
    valid_combinations <- allGaitParams %>%
      select(participant, trialNum) %>%
      distinct() %>%
      arrange(participant, trialNum)

    debug_log("Found", nrow(valid_combinations), "valid combinations")

    # Set up parallel processing using your framework's approach
    numCores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(numCores)
    doParallel::registerDoParallel(cl)

    # Export required variables and functions to cluster
    parallel::clusterExport(cl, c(
      "allGaitParams", "outlier_col_names", "debug",
      "calc_complexity_for_loop", "calculate_complexity_single", "compute_complexity",
      "sampen", "apen", "mse", "dfa_alpha", "lyapunov", "poincare_sd"
    ), envir = environment())

    # Load required packages on each worker
    parallel::clusterEvalQ(cl, {
      library(dplyr)
    })

    # Run the parallel loop using your framework pattern
    results_list <- foreach::foreach(
      i = 1:nrow(valid_combinations),
      .combine = rbind,
      .packages = c("dplyr")
    ) %dopar% {
      participant <- valid_combinations$participant[i]
      trial <- valid_combinations$trialNum[i]

      calc_complexity_for_loop(
        participant, trial,
        allGaitParams = allGaitParams,
        outlier_col_names = outlier_col_names,
        debug = FALSE # Turn off debug in parallel workers
      )
    }

    # Clean up cluster
    parallel::stopCluster(cl)
    result <- as.data.frame(results_list)
  } else {
    debug_log("Using get_data_from_loop (sequential)")

    # For sequential, we can use a modified approach that bypasses file verification
    # Get valid combinations from the data itself
    valid_combinations <- allGaitParams %>%
      select(participant, trialNum) %>%
      distinct() %>%
      arrange(participant, trialNum)

    debug_log("Found", nrow(valid_combinations), "valid combinations")

    # Optimized: pre-allocate list to avoid repeated rbind operations
    total_combinations <- nrow(valid_combinations)
    results_list <- vector("list", total_combinations)

    # Process each combination with progress bar (like your framework)
    for (i in 1:total_combinations) {
      participant <- valid_combinations$participant[i]
      trial <- valid_combinations$trialNum[i]

      # Progress bar (matching your framework style)
      progress_percent <- (i / total_combinations) * 100
      progress_bar_length <- 50
      num_hashes <- floor(progress_bar_length * progress_percent / 100)
      num_dashes <- progress_bar_length - num_hashes
      progress_bar <- paste0(
        "[", paste(rep("#", num_hashes), collapse = ""),
        paste(rep("-", num_dashes), collapse = ""), "]"
      )

      if (!debug) { # Show progress bar unless in debug mode
        cat(sprintf(
          "\rProgress: %s %.2f%% On Participant: %s, Trial: %s",
          progress_bar, progress_percent, participant, trial
        ))
        flush.console()
      }

      # Calculate complexity for this combination
      single_result <- calc_complexity_for_loop(
        participant, trial,
        allGaitParams = allGaitParams,
        outlier_col_names = outlier_col_names,
        debug = debug
      )

      # Store in pre-allocated list instead of rbind for better performance
      results_list[[i]] <- single_result
    }

    # Combine all results efficiently using rbindlist
    result <- data.table::rbindlist(results_list, fill = TRUE)
    result <- as.data.frame(result) # Convert back for compatibility

    if (!debug) cat("\n") # New line after progress bar
  }

  debug_log("Complexity analysis complete!")
  debug_log("Final result dimensions:", nrow(result), "rows x", ncol(result), "columns")

  return(result)
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

  # Filter complexity data using the common helper function
  complexity_filtered <- filter_by_gait_combinations(mu_gait_copy, complexity_copy, "complexity") # defined in summarize_simulation.R

  # Merge based on participant and trialNum (using left_join to preserve all gait data)
  left_join(mu_gait_copy, complexity_filtered, by = c("participant", "trialNum"))
}
