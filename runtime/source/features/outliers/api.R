#' Outliers Feature Module API
#'
#' Public interface for outlier detection, checking, marking, and application functions.
#' This module provides comprehensive outlier handling for heel strike data.

# =============================================================================
# DETECTION FUNCTIONS
# =============================================================================

#' Find heel strikes with two-stage tolerance system
#'
#' @param outliers Data frame with outlier information
#' @param data_source Data frame containing heel strike data
#' @param search_tolerance Maximum time difference to search for potential matches (default 0.03s)
#' @param grouping_tolerance Maximum time difference to group steps together (default 0.001s)
#' @return Data table with all matched rows (including grouped steps)
find_heel_strikes <- function(outliers, data_source, search_tolerance = 0.03, grouping_tolerance = 0.001) {
    find_closest_heel_strikes(outliers, data_source, search_tolerance, grouping_tolerance)
}

#' Find the closest heel strike within a time threshold
#'
#' @param clicked_time Numeric time value from the plot interaction
#' @param participant  Participant identifier
#' @param trial        Trial number
#' @param trial_data   Data frame containing heel strike data for plotting
#' @param threshold    Maximum absolute time difference allowed (seconds)
#' @return Single-row data.frame of the closest heel strike, or NULL
find_closest_strike <- function(clicked_time, participant, trial, trial_data, threshold = 0.03) {
    find_closest_heel_strike(clicked_time, participant, trial, trial_data, threshold)
}

# =============================================================================
# CHECKING FUNCTIONS
# =============================================================================

#' Check if a heel strike is marked as an outlier
#'
#' @param participant Participant identifier
#' @param trialNum   Trial number
#' @param time       Time of the heel strike
#' @param search_tolerance Maximum time difference to search for matches (default 0.03s)
#' @param grouping_tolerance Maximum time difference to group steps together (default 0.001s)
#' @return TRUE if the heel strike is marked as an outlier, FALSE otherwise
is_outlier_strike <- function(participant, trialNum, time, search_tolerance = 0.03, grouping_tolerance = 0.001) {
    check_outlier_heel_strike(participant, trialNum, time, search_tolerance, grouping_tolerance)
}

# =============================================================================
# MARKING FUNCTIONS
# =============================================================================

#' Mark outlier steps in heel strike data
#'
#' @param heel_strikes_data Data frame containing heel strike data
#' @param participant       Participant identifier
#' @param trialNum         Trial number
#' @param search_tolerance Maximum time difference to search for matches (default 0.03s)
#' @param grouping_tolerance Maximum time difference to group steps together (default 0.001s)
#' @return Modified heel_strikes_data with outlierSteps column updated
mark_steps_as_outliers <- function(heel_strikes_data, participant, trialNum, search_tolerance = 0.03, grouping_tolerance = 0.001) {
    mark_outlier_steps(heel_strikes_data, participant, trialNum, search_tolerance, grouping_tolerance)
}

# =============================================================================
# APPLICATION FUNCTIONS
# =============================================================================

#' Apply heel-strike removals and step outlier markings to any data frame
#'
#' This is the single, central routine that both the interactive UI (page16) and the
#' pre-processing pipeline (find_foot_events) should call so we never duplicate
#' logic.  It assumes the input data frame contains – at minimum – the columns
#' `participant`, `trialNum`, and `time`.  If an `outlierSteps` column is present
#' it will be updated; otherwise the column will be created.
#'
#' @param data           A data.frame or data.table with heel-strike level rows
#' @param heel_outliers  Data frame with heel-strike outliers (to REMOVE)
#' @param step_outliers  Data frame with step outliers      (to MARK)
#' @param tolerance      Maximum absolute time difference (seconds) to search for heel strike removal matches
#' @param step_tolerance Maximum absolute time difference (seconds) to search for step outlier marking matches
#' @param grouping_tolerance Maximum absolute time difference (seconds) to group steps together (default 0.001s)
#' @return A **data.frame** with the requested removals / markings applied
apply_outlier_processing <- function(data,
                                     heel_outliers = NULL,
                                     step_outliers = NULL,
                                     tolerance = 0.03,
                                     step_tolerance = 0.03,
                                     grouping_tolerance = 0.001) {
    apply_outliers(data, heel_outliers, step_outliers, tolerance, step_tolerance, grouping_tolerance)
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Complete outlier processing pipeline
#'
#' @param data           A data.frame or data.table with heel-strike level rows
#' @param heel_outliers  Data frame with heel-strike outliers (to REMOVE)
#' @param step_outliers  Data frame with step outliers      (to MARK)
#' @param tolerance      Maximum absolute time difference (seconds) to search for heel strike removal matches
#' @param step_tolerance Maximum absolute time difference (seconds) to search for step outlier marking matches
#' @param grouping_tolerance Maximum absolute time difference (seconds) to group steps together (default 0.001s)
#' @return A **data.frame** with the requested removals / markings applied
process_outliers <- function(data,
                            heel_outliers = NULL,
                            step_outliers = NULL,
                            tolerance = 0.03,
                            step_tolerance = 0.03,
                            grouping_tolerance = 0.001) {
    apply_outlier_processing(data, heel_outliers, step_outliers, tolerance, step_tolerance, grouping_tolerance)
}

#' Get outlier statistics for a dataset
#'
#' @param data Data frame with outlierSteps column
#' @return List with outlier statistics
get_outlier_stats <- function(data) {
    if (!"outlierSteps" %in% names(data)) {
        return(list(
            total_steps = nrow(data),
            outlier_steps = 0,
            outlier_percentage = 0
        ))
    }
    
    total_steps <- nrow(data)
    outlier_steps <- sum(data$outlierSteps, na.rm = TRUE)
    outlier_percentage <- if (total_steps > 0) (outlier_steps / total_steps) * 100 else 0
    
    list(
        total_steps = total_steps,
        outlier_steps = outlier_steps,
        outlier_percentage = outlier_percentage
    )
}
