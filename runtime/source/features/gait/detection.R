#' Gait Event Detection Functions
#'
#' Core functions for detecting heel strikes and toe-offs

#' Detect local extremes of relative foot position
#' Based on https://c-motion.com/v3dwiki/index.php/Tutorial:_Gait_Events#Method_1._Coordinate_Based_Algorithm
#' @param relFootPos_filtered Filtered relative foot position data
#' @return List with maxima and minima indices
detect_local_extremes <- function(relFootPos_filtered) {
  local_maxima <- which(diff(sign(diff(relFootPos_filtered))) == -2) + 1
  local_minima <- which(diff(sign(diff(relFootPos_filtered))) == 2) + 1

  # Always start with a minimum (toe-off)
  if (local_minima[1] > local_maxima[1]) {
    local_maxima <- local_maxima[-1]
  }

  return(list(maxima = local_maxima, minima = local_minima))
}

#' Detect foot events using coordinate-based algorithm
#' @param footData Foot position data
#' @param hipData Hip position data
#' @param otherFootData Other foot position data (optional)
#' @return List with heelStrikes and toeOffs dataframes
detect_foot_events_coordinates <- function(footData, hipData, otherFootData = NULL) {
  # Check if hip data is stationary
  useHip <- !is_hip_stationary(hipData)
  checkAlternation <- TRUE

  if (useHip) {
    relFootPos <- footData$pos_z - hipData$pos_z
  } else {
    print("Hip data appears stationary (moves less than the threshold). Using absolute foot position instead of relative position.")
    relFootPos <- footData$pos_z
  }

  # Filter and detect extremes
  relFootPos_filtered <- apply_padding_and_filter(relFootPos, 4, 90, 5)
  extremes <- detect_local_extremes(relFootPos_filtered)
  local_maxima <- extremes$maxima
  local_minima <- extremes$minima

  cat(sprintf(
    "DEBUG: Initial detection - %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))
  flush.console()

  # Filter extremes based on time and position
  filtered <- filter_extremes(local_maxima, local_minima, footData$time, relFootPos_filtered)
  local_maxima <- filtered$maxima
  local_minima <- filtered$minima

  cat(sprintf(
    "DEBUG: After time/position filtering - %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))
  flush.console()

  # Filter by other foot position, with hip as fallback
  position_filtered <- NULL
  hip_filtered <- NULL

  # Always try to filter by other foot position first
  skipOtherFootCheck <- TRUE
  if (!is.null(otherFootData) && nrow(otherFootData) > 0 && !skipOtherFootCheck) {
    # Calculate relative foot position: current_foot_pos_x - other_foot_pos_x
    other_foot_pos_x <- approx(otherFootData$time, otherFootData$pos_x,
      xout = footData$time, rule = 1
    )$y
    relFootPos_x <- footData$pos_x - other_foot_pos_x

    position_filtered <- filter_by_other_foot_position(local_maxima, local_minima, relFootPos_x)
    local_maxima <- position_filtered$maxima
    local_minima <- position_filtered$minima

    cat(sprintf(
      "DEBUG: After other foot filtering - %d maxima, %d minima\n",
      length(local_maxima), length(local_minima)
    ))
    flush.console()
  }

  # Always check hip position for suspect marking (if hip data available)
  if (!is.null(hipData) && nrow(hipData) > 0) {
    hip_pos_x <- safe_approx(hipData$time, hipData$pos_x,
      xout = footData$time, rule = 1
    )

    relFootPos_x_hip <- footData$pos_x - hip_pos_x

    # If hip_pos_x is all NA (insufficient points), skip hip-based suspect marking
    if (all(is.na(hip_pos_x))) {
      hip_filtered <- NULL
    } else {
      hip_filtered <- filter_by_hip_position(local_maxima, local_minima, relFootPos_x_hip)
      # Don't update local_maxima/local_minima here - hip filtering is only for suspect marking
    }
  }

  # Check alternation
  if (checkAlternation) {
    alternation <- check_alternation(local_maxima, local_minima)
    local_maxima <- alternation$maxima
    local_minima <- alternation$minima
  }

  # Refine heelstrike and capture unstable labels
  refine_res <- refine_heelstrike(footData, local_maxima, local_minima)
  local_maxima <- refine_res$maxima
  suspect_unstable_idx <- refine_res$suspect_unstable

  cat(sprintf(
    "DEBUG: After refinement - %d maxima, %d minima\n",
    length(local_maxima), length(local_minima)
  ))
  flush.console()

  # Ensure lengths match
  lMax <- length(local_maxima)
  lMin <- length(local_minima)
  if (lMax != lMin) {
    trimLength <- min(lMax, lMin)
    local_maxima <- local_maxima[1:trimLength]
    local_minima <- local_minima[1:trimLength]
    # keep only suspect indices within range after trimming
    if (exists("suspect_unstable_idx")) {
      suspect_unstable_idx <- suspect_unstable_idx[suspect_unstable_idx <= trimLength]
    }

    cat(sprintf(
      "DEBUG: After length matching - %d maxima, %d minima\n",
      length(local_maxima), length(local_minima)
    ))
    flush.console()
  }

  # Logging
  if (filtered$N_removed_time > 0) {
    print(paste("removed", filtered$N_removed_time, "max+min due to time constraint."))
  }
  if (filtered$N_removed_pos > 0) {
    print(paste("removed", filtered$N_removed_pos, "max+min due to pos difference constraint."))
  }
  if (alternation$N_removed_max + alternation$N_removed_min > 0) {
    print(paste("removed", alternation$N_removed_max, "maxima, and", alternation$N_removed_min, "minima due to wrong alternation."))
  }

  if (length(local_maxima) != length(local_minima)) {
    print(paste("WARNING: Length maxima:", length(local_maxima), "Length minima:", length(local_minima)))
  }

  # Create output dataframes
  output <- create_output_dataframes(footData, local_maxima, local_minima)
  # Add 'suspect' column default FALSE
  output$heelStrikes$suspect <- FALSE

  # Add hip position annotations at heel-strike times
  if (!is.null(hipData) && nrow(hipData) > 0) {
    # Sample hip positions at heel-strike times using nearest-neighbor
    output$heelStrikes$hip_pos_x <- sample_time_series(hipData$time, hipData$pos_x, output$heelStrikes$time)
    output$heelStrikes$hip_pos_y <- sample_time_series(hipData$time, hipData$pos_y, output$heelStrikes$time)
    output$heelStrikes$hip_pos_z <- sample_time_series(hipData$time, hipData$pos_z, output$heelStrikes$time)
  } else {
    # No hip data available, add NA columns
    output$heelStrikes$hip_pos_x <- NA_real_
    output$heelStrikes$hip_pos_y <- NA_real_
    output$heelStrikes$hip_pos_z <- NA_real_
  }

  # Flag unstable steps from refinement
  if (length(suspect_unstable_idx) > 0) {
    output$heelStrikes$suspect[suspect_unstable_idx] <- TRUE
  }

  # Flag suspect steps based on hip position (only if hip filtering was performed and other foot data was available)
  if (!is.null(hip_filtered) && !is.null(position_filtered)) {
    # Map original indices to final indices after all filtering
    original_maxima_before_filtering <- extremes$maxima

    # Find which of the original suspect indices are still in our final maxima
    for (suspect_idx in hip_filtered$suspect_maxima_indices) {
      original_suspect_index <- original_maxima_before_filtering[suspect_idx]
      final_position <- which(local_maxima == original_suspect_index)
      if (length(final_position) > 0) {
        output$heelStrikes$suspect[final_position] <- TRUE
      }
    }
  }

  print(paste("--- ---totalsteps: ", length(output$heelStrikes$time)))

  return(output)
}
