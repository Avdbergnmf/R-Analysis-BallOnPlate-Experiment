#' Outlier Application Functions
#'
#' Functions for applying outlier removals and markings to datasets

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
apply_outliers <- function(data,
                           heel_outliers = NULL,
                           step_outliers = NULL,
                           tolerance = 0.03,
                           step_tolerance = 0.03,
                           grouping_tolerance = 0.001) {
    dt <- data.table::as.data.table(data)

    # Ensure consistent types for reliable joins
    dt[, `:=`(
        participant = as.character(participant),
        trialNum    = as.numeric(as.character(trialNum))
    )]

    # Guarantee the logical column exists so downstream code is safe
    if (!"outlierSteps" %in% names(dt)) {
        dt[, outlierSteps := FALSE]
    }

    # --------------------------------------------------
    # 1) Permanently DELETE heel strikes flagged as false
    # --------------------------------------------------
    if (!is.null(heel_outliers) && nrow(heel_outliers) > 0) {
        matches <- find_closest_heel_strikes(heel_outliers, dt, tolerance, grouping_tolerance)
        if (!is.null(matches) && nrow(matches) > 0) {
            matches <- data.table::as.data.table(matches)
            rows_to_remove <- dt[matches, on = .(participant, trialNum, time), nomatch = 0, which = TRUE]
            if (length(rows_to_remove)) {
                dt <- dt[-rows_to_remove]
            }
        }
    }

    # --------------------------------------------------
    # 2) MARK step outliers for exclusion in analysis
    # --------------------------------------------------
    if (!is.null(step_outliers) && nrow(step_outliers) > 0) {
        matches <- find_closest_heel_strikes(step_outliers, dt, step_tolerance, grouping_tolerance)
        if (!is.null(matches) && nrow(matches) > 0) {
            rows_to_mark <- dt[matches, on = .(participant, trialNum, time), nomatch = 0, which = TRUE]
            if (length(rows_to_mark)) {
                dt[rows_to_mark, outlierSteps := TRUE]
            }
        }
    }

    return(as.data.frame(dt))
}
