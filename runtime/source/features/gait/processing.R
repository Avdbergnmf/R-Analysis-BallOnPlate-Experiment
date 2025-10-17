#' Gait Event Processing Functions
#'
#' Functions for post-processing and refinement of gait events

#' Check and fix alternation between left and right foot
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @return List with corrected maxima and minima
check_alternation <- function(local_maxima, local_minima) {
  N_removed_min <- 0
  N_removed_max <- 0

  if (length(local_minima) == 0 || length(local_maxima) == 0) {
    warning("No local minima or maxima found for alternation checking. Skipping alternation step.")
    return(list(
      maxima = local_maxima, minima = local_minima,
      N_removed_min = 0, N_removed_max = 0
    ))
  }

  # Debug logging
  cat(sprintf(
    "DEBUG: Alternation check - Input: %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))
  flush.console()

  original_maxima_count <- length(local_maxima)
  original_minima_count <- length(local_minima)

  # Vectorized approach: identify which elements to keep
  min_len <- min(length(local_maxima), length(local_minima))

  if (min_len == 0) {
    return(list(
      maxima = local_maxima, minima = local_minima,
      N_removed_min = 0, N_removed_max = 0
    ))
  }

  # Create logical vectors for which elements to keep
  keep_maxima <- rep(TRUE, length(local_maxima))
  keep_minima <- rep(TRUE, length(local_minima))

  # Vectorized: Remove maxima that come before their corresponding minima
  if (min_len > 0) {
    indices_to_check <- seq_len(min_len)
    keep_maxima[indices_to_check] <- local_maxima[indices_to_check] >= local_minima[indices_to_check]
  }

  # Vectorized: Remove minima that come after the next maxima
  if (length(local_maxima) > 0 && length(local_minima) > 1) {
    max_indices <- seq_len(min(length(local_maxima), length(local_minima) - 1))
    min_indices <- max_indices + 1
    keep_minima[min_indices] <- local_minima[min_indices] >= local_maxima[max_indices]
  }

  # Apply filtering in one operation
  new_maxima <- local_maxima[keep_maxima]
  new_minima <- local_minima[keep_minima]

  # Calculate removed counts
  N_removed_min <- original_maxima_count - length(new_maxima)
  N_removed_max <- original_minima_count - length(new_minima)

  # Debug logging for removals
  if (N_removed_min > 0) {
    cat(sprintf("DEBUG: Removed %d maxima\n", N_removed_min))
    flush.console()
  }
  if (N_removed_max > 0) {
    cat(sprintf("DEBUG: Removed %d minima\n", N_removed_max))
    flush.console()
  }

  cat(sprintf(
    "DEBUG: Alternation check - Output: %d maxima, %d minima (removed %d max, %d min)\n",
    length(new_maxima), length(new_minima), N_removed_min, N_removed_max
  ))
  flush.console()

  return(list(
    maxima = new_maxima, minima = new_minima,
    N_removed_min = N_removed_min, N_removed_max = N_removed_max
  ))
}

#' Refine heel strike detection using stability analysis
#' @param footData Foot position data
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param smoothing_window Window size for smoothing
#' @param change_threshold Threshold for stability detection
#' @return List with refined maxima and suspect indices
refine_heelstrike <- function(footData, local_maxima, local_minima,
                              smoothing_window = 5, change_threshold = 0.05) {
  # Vectorized heel-strike refinement - much faster than loop-based approach

  # Only process heel-strikes that have a corresponding next toe-off
  max_iterations <- min(length(local_maxima), length(local_minima) - 1)

  if (max_iterations <= 0) {
    # No valid pairs to process - mark all as suspect
    return(list(maxima = local_maxima, suspect_unstable = seq_along(local_maxima)))
  }

  # Pre-smooth the entire dataset once (vectorized operation)
  footData$smoothed_x <- zoo::rollmean(footData$pos_x, smoothing_window, fill = NA)
  footData$smoothed_y <- zoo::rollmean(footData$pos_y, smoothing_window, fill = NA)

  # Calculate rate of change for entire dataset (vectorized)
  footData$dx <- c(NA, diff(footData$smoothed_x) / diff(footData$time))
  footData$dy <- c(NA, diff(footData$smoothed_y) / diff(footData$time))

  # Vectorized processing of heel-strike pairs
  heelstrike_indices <- local_maxima[seq_len(max_iterations)]
  toeoff_indices <- local_minima[seq_len(max_iterations) + 1]

  # Vectorized function to find first stable point in each segment
  find_stable_point <- function(start_idx, end_idx) {
    if (start_idx >= end_idx) {
      return(start_idx)
    } # Edge case

    segment_indices <- start_idx:end_idx
    dx_segment <- footData$dx[segment_indices]
    dy_segment <- footData$dy[segment_indices]

    # Find stable points (vectorized logical operations)
    stable_mask <- abs(dx_segment) < change_threshold & abs(dy_segment) < change_threshold
    stable_mask[is.na(stable_mask)] <- FALSE

    stable_points <- which(stable_mask)

    if (length(stable_points) > 0) {
      return(start_idx + stable_points[1] - 1) # Convert back to global index
    } else {
      # Check if we have valid data (any non-NA values)
      if (any(!is.na(dx_segment))) {
        return(start_idx) # Keep original if no stable point but valid data
      } else {
        return(NA) # Mark as invalid
      }
    }
  }

  # Apply vectorized refinement using mapply
  refined_indices <- mapply(find_stable_point, heelstrike_indices, toeoff_indices, SIMPLIFY = TRUE)

  # Identify unstable (suspect) heel-strikes
  unstable_mask <- is.na(refined_indices)
  unstable_indices <- which(unstable_mask)

  # Remove NA values and keep valid refined indices
  valid_mask <- !unstable_mask
  refined_local_maxima <- refined_indices[valid_mask]

  # Report unstable heel-strikes (vectorized message creation)
  if (sum(unstable_mask) > 0) {
    heelstrike_times <- footData$time[heelstrike_indices[unstable_mask]]
    message(
      "No stable point found for ", sum(unstable_mask), " heel-strikes at times: ",
      paste(round(heelstrike_times, 3), collapse = ", "), "; labelled as SUSPECT_UNSTABLE."
    )
  }

  # Handle any remaining heel-strikes that don't have a next toe-off
  if (length(local_maxima) > max_iterations) {
    remaining_heelstrikes <- local_maxima[(max_iterations + 1):length(local_maxima)]
    refined_local_maxima <- c(refined_local_maxima, remaining_heelstrikes)
    # Mark these as suspect since we couldn't refine them
    remaining_indices <- (length(refined_local_maxima) - length(remaining_heelstrikes) + 1):length(refined_local_maxima)
    unstable_indices <- c(unstable_indices, remaining_indices)
    message("Marked ", length(remaining_heelstrikes), " heel-strikes as suspect (no corresponding toe-off for refinement)")
  }

  return(list(maxima = refined_local_maxima, suspect_unstable = unstable_indices))
}