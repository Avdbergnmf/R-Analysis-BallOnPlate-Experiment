#' Foot Placement Control Metrics
#'
#' Functions for calculating foot placement control residuals and related metrics

#' Centralized NA column definitions for foot-placement control metrics
#' @param foot Foot side ("left", "right", "both", or NULL for both)
#' @return List with NA values for specified foot(s)
get_na_results_fp_control <- function(foot = "both") {
  # Base metric names and values
  base_metrics <- list(
    step_pos_x_residual_rmse = NA_real_,
    step_pos_x_beta0 = NA_real_,
    step_pos_x_beta1 = NA_real_,
    step_pos_x_r2 = NA_real_,
    step_pos_x_n = 0L
  )

  if (is.null(foot) || foot == "both") {
    # Return both left and right with mean metrics
    result <- c(
      # Left foot
      setNames(base_metrics, paste0(names(base_metrics), "_left")),
      # Right foot
      setNames(base_metrics, paste0(names(base_metrics), "_right")),
      # Mean metrics (subset)
      step_pos_x_residual_rmse_mean = NA_real_,
      step_pos_x_beta1_mean = NA_real_,
      step_pos_x_r2_mean = NA_real_,
      step_pos_x_n_total = 0L
    )
  } else if (foot == "left") {
    # Return only left foot
    result <- setNames(base_metrics, paste0(names(base_metrics), "_left"))
  } else if (foot == "right") {
    # Return only right foot
    result <- setNames(base_metrics, paste0(names(base_metrics), "_right"))
  } else {
    stop("Invalid foot argument. Must be 'left', 'right', 'both', or NULL")
  }

  return(result)
}

#' Build per-foot FP datasets with contralateral check
#' @param trial_hs_df Heel-strike data frame for one participant & trial
#' @param foot Foot side ("Left" or "Right")
#' @return Data frame with columns FP (response) and P (predictor)
build_fp_rows_per_foot <- function(trial_hs_df, foot) {
  # Filter to heel strikes for the specified foot
  foot_strikes <- trial_hs_df[trial_hs_df$foot == foot, ]

  if (nrow(foot_strikes) == 0) {
    return(data.frame(FP = numeric(0), P = numeric(0)))
  }

  # Sort by time to ensure proper ordering
  foot_strikes <- foot_strikes[order(foot_strikes$time), ]

  FP_vec <- numeric(0)
  P_vec <- numeric(0)

  # Sign convention: +1 for Right, -1 for Left
  s <- if (foot == "Right") 1 else -1

  # Iterate through heel strikes for this foot
  for (i in seq_len(nrow(foot_strikes))) {
    current_time <- foot_strikes$time[i]

    # Find immediate previous heel-strike row by time
    prev_strikes <- trial_hs_df[trial_hs_df$time < current_time, ]
    if (nrow(prev_strikes) == 0) next

    # Get the most recent previous heel strike
    prev_i <- which.max(prev_strikes$time)
    prev_strike <- prev_strikes[prev_i, ]

    # Check if previous heel strike is contralateral
    is_contralateral <- if (foot == "Right") prev_strike$foot == "Left" else prev_strike$foot == "Right"
    if (!is_contralateral) next

    # Check that neither current nor previous heel strike is flagged
    current_outlier <- if ("outlierSteps" %in% colnames(foot_strikes)) foot_strikes$outlierSteps[i] else FALSE
    current_suspect <- if ("suspect" %in% colnames(foot_strikes)) foot_strikes$suspect[i] else FALSE
    prev_outlier <- if ("outlierSteps" %in% colnames(prev_strike)) prev_strike$outlierSteps else FALSE
    prev_suspect <- if ("suspect" %in% colnames(prev_strike)) prev_strike$suspect else FALSE

    if (current_outlier || current_suspect || prev_outlier || prev_suspect) next

    # Check that we have valid position data
    if (!is.finite(foot_strikes$pos_x[i]) || !is.finite(prev_strike$pos_x) ||
      !is.finite(foot_strikes$hip_pos_x[i]) || !is.finite(prev_strike$hip_pos_x)) {
      next
    }

    # Compute response and predictor
    FP <- s * (foot_strikes$pos_x[i] - prev_strike$pos_x)
    P <- s * (prev_strike$hip_pos_x - prev_strike$pos_x)

    FP_vec <- c(FP_vec, FP)
    P_vec <- c(P_vec, P)
  }

  return(data.frame(FP = FP_vec, P = P_vec))
}

#' Fit position-only FP control model
#' @param FP Foot placement response vector
#' @param P Position predictor vector
#' @return Named list with rmse, beta0, beta1, r2, and n
fit_fp_pos_only <- function(FP, P) {
  n <- length(FP)
  if (n < 20) {
    return(list(
      rmse = NA_real_,
      beta0 = NA_real_,
      beta1 = NA_real_,
      r2 = NA_real_,
      n = n
    ))
  }

  tryCatch(
    {
      # Fit FP ~ 1 + P via lm.fit()
      X <- cbind(1, P)
      fit <- lm.fit(X, FP)

      beta0 <- fit$coefficients[1]
      beta1 <- fit$coefficients[2]

      # Check for singular fit
      if (!is.finite(beta0) || !is.finite(beta1)) {
        return(list(
          rmse = NA_real_,
          beta0 = NA_real_,
          beta1 = NA_real_,
          r2 = NA_real_,
          n = n
        ))
      }

      # Compute residuals and RMSE (with df correction)
      predicted <- beta0 + beta1 * P
      residuals <- FP - predicted

      # Model RMSE: sqrt(sum(residuals^2) / (n - p)) where p = 2 (intercept + slope)
      rmse <- sqrt(sum(residuals^2) / (n - 2))

      # Compute R²
      ss_res <- sum(residuals^2)
      ss_tot <- sum((FP - mean(FP))^2)
      r2 <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0

      return(list(
        rmse = rmse,
        beta0 = beta0,
        beta1 = beta1,
        r2 = r2,
        n = n
      ))
    },
    error = function(e) {
      return(list(
        rmse = NA_real_,
        beta0 = NA_real_,
        beta1 = NA_real_,
        r2 = NA_real_,
        n = n
      ))
    }
  )
}

#' Compute per-foot FP control metrics
#' @param trial_hs_df Heel-strike data frame for one participant & trial
#' @return Named list with per-foot and mean metrics
compute_fp_control_metrics_per_foot <- function(trial_hs_df) {
  # Build FP datasets for both feet
  left_data <- build_fp_rows_per_foot(trial_hs_df, "Left")
  right_data <- build_fp_rows_per_foot(trial_hs_df, "Right")

  # Fit position-only models for each foot
  left_results <- fit_fp_pos_only(left_data$FP, left_data$P)
  right_results <- fit_fp_pos_only(right_data$FP, right_data$P)

  # Create named outputs for left foot
  left_metrics <- list(
    step_pos_x_residual_rmse_left = left_results$rmse,
    step_pos_x_beta0_left = left_results$beta0,
    step_pos_x_beta1_left = left_results$beta1,
    step_pos_x_r2_left = left_results$r2,
    step_pos_x_n_left = left_results$n
  )

  # Create named outputs for right foot
  right_metrics <- list(
    step_pos_x_residual_rmse_right = right_results$rmse,
    step_pos_x_beta0_right = right_results$beta0,
    step_pos_x_beta1_right = right_results$beta1,
    step_pos_x_r2_right = right_results$r2,
    step_pos_x_n_right = right_results$n
  )

  # Compute mean metrics (ignore NAs)
  mean_metrics <- list(
    step_pos_x_residual_rmse_mean = mean(c(left_results$rmse, right_results$rmse), na.rm = TRUE),
    step_pos_x_beta1_mean = mean(c(left_results$beta1, right_results$beta1), na.rm = TRUE),
    step_pos_x_r2_mean = mean(c(left_results$r2, right_results$r2), na.rm = TRUE),
    step_pos_x_n_total = left_results$n + right_results$n
  )

  # Return combined results
  c(left_metrics, right_metrics, mean_metrics)
}
