#' Gait Feature Module API
#'
#' Public interface for gait event detection and analysis functions.
#' This module provides comprehensive foot event detection for gait analysis.

# =============================================================================
# MAIN GAIT EVENT DETECTION
# =============================================================================

#' Find foot events for a participant and trial
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @return List with heelStrikes and toeOffs dataframes
detect_foot_events <- function(participant, trialNum) {
  find_foot_events(participant, trialNum)
}

# =============================================================================
# INDIVIDUAL FOOT EVENT DETECTION
# =============================================================================

#' Detect foot events using coordinate-based algorithm for a single foot
#' @param footData Foot position data
#' @param hipData Hip position data
#' @param otherFootData Other foot position data (optional)
#' @return List with heelStrikes and toeOffs dataframes
detect_single_foot_events <- function(footData, hipData, otherFootData = NULL) {
  detect_foot_events_coordinates(footData, hipData, otherFootData)
}

# =============================================================================
# FILTERING AND PROCESSING FUNCTIONS
# =============================================================================

#' Filter extremes based on time and position differences
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param time_vector Time vector
#' @param pos_vector Position vector
#' @param min_time_diff Minimum time difference
#' @param min_pos_diff Minimum position difference
#' @return List with filtered maxima and minima
filter_gait_extremes <- function(local_maxima, local_minima, time_vector, pos_vector, min_time_diff = 0.1, min_pos_diff = 0.0) {
  filter_extremes(local_maxima, local_minima, time_vector, pos_vector, min_time_diff, min_pos_diff)
}

#' Filter extremes based on hip position
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param relFootPos Relative foot position
#' @return List with filtered maxima and minima
filter_by_hip_position <- function(local_maxima, local_minima, relFootPos) {
  filter_by_hip_position(local_maxima, local_minima, relFootPos)
}

#' Filter extremes based on other foot position
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param relFootPos Relative foot position
#' @param threshold Threshold for foot separation
#' @return List with filtered maxima and minima
filter_by_other_foot_position <- function(local_maxima, local_minima, relFootPos, threshold = 0.05) {
  filter_by_other_foot_position(local_maxima, local_minima, relFootPos, threshold)
}

#' Check and fix alternation between left and right foot
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @return List with corrected maxima and minima
check_foot_alternation <- function(local_maxima, local_minima) {
  check_alternation(local_maxima, local_minima)
}

#' Refine heel strike detection using stability analysis
#' @param footData Foot position data
#' @param local_maxima Indices of local maxima
#' @param local_minima Indices of local minima
#' @param smoothing_window Window size for smoothing
#' @param change_threshold Threshold for stability detection
#' @return List with refined maxima and suspect indices
refine_heel_strikes <- function(footData, local_maxima, local_minima, smoothing_window = 5, change_threshold = 0.05) {
  refine_heelstrike(footData, local_maxima, local_minima, smoothing_window, change_threshold)
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Check if hip data is stationary
#' @param hipData Hip position data
#' @param threshold_cm Threshold for movement detection in cm
#' @param time_window Time window to check in seconds
#' @return TRUE if hip is stationary
is_hip_stationary <- function(hipData, threshold_cm = 0.01, time_window = 30) {
  is_hip_stationary(hipData, threshold_cm, time_window)
}

#' Create output dataframes for heel strikes and toe-offs
#' @param footData Foot position data
#' @param local_maxima Indices of local maxima (heel strikes)
#' @param local_minima Indices of local minima (toe-offs)
#' @return List with heelStrikes and toeOffs dataframes
create_gait_output_dataframes <- function(footData, local_maxima, local_minima) {
  create_output_dataframes(footData, local_maxima, local_minima)
}

#' Add step differences per foot
#' @param relHeelStrikesData Relative heel strikes data
#' @return Dataframe with step width and length differences
add_step_differences <- function(relHeelStrikesData) {
  add_diff_per_foot(relHeelStrikesData)
}

#' Safe approximation function that prevents errors in parallel workers
#' @param x Input x values
#' @param y Input y values
#' @param xout Output x values
#' @param rule Rule for extrapolation
#' @return Interpolated values or NA if insufficient data
safe_approximation <- function(x, y, xout, rule = 1) {
  safe_approx(x, y, xout, rule)
}

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Note: The following functions are available directly from the module:
# - get_rotations_data()
# - load_rotations(participant, trialNum)
# - preprocess_data(participant, trialNum, dataName)
# - get_preprocessed_data(participant, trialNum, dataList)

# =============================================================================
# DATA TRANSFORMATIONS
# =============================================================================

# Note: The following functions are available directly from the module:
# - is_kinematic_data(data)
# - rotate_y(data, theta_deg)
# - apply_padding_and_filter(column, poly_order, fs, cutoff_freq)

# =============================================================================
# STATISTICS AND ANALYSIS
# =============================================================================

# Note: The following functions are available directly from the module:
# - calculate_step_statistics(data, group_vars)

#' Get gait event statistics
#' @param gait_data Gait data from detect_foot_events
#' @return List with gait statistics
get_gait_statistics <- function(gait_data) {
  heel_strikes <- gait_data$heelStrikes
  toe_offs <- gait_data$toeOffs
  
  list(
    total_steps = nrow(heel_strikes),
    left_steps = sum(heel_strikes$foot == "Left", na.rm = TRUE),
    right_steps = sum(heel_strikes$foot == "Right", na.rm = TRUE),
    suspect_steps = sum(heel_strikes$suspect, na.rm = TRUE),
    outlier_steps = sum(heel_strikes$outlierSteps, na.rm = TRUE),
    time_range = if (nrow(heel_strikes) > 0) range(heel_strikes$time, na.rm = TRUE) else c(NA, NA)
  )
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Complete gait analysis pipeline
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @return List with heelStrikes and toeOffs dataframes
analyze_gait <- function(participant, trialNum) {
  detect_foot_events(participant, trialNum)
}
