# Utility functions for handling outliers

#' Find the closest heel strikes in a dataset efficiently using data.table
#'
#' @param outliers Data frame with outlier information
#' @param data_source Data frame containing heel strike data
#' @param max_distance Maximum time difference allowed
#' @return Data table with matched rows
find_closest_heel_strikes <- function(outliers, data_source, max_distance = 0.03) {
    if (is.null(outliers) || nrow(outliers) == 0) {
        return(NULL)
    }

    # Convert to data.table and ensure consistent types
    dt <- data.table::as.data.table(data_source)
    outliers_dt <- data.table::as.data.table(outliers)

    # Ensure consistent types
    dt[, `:=`(
        participant = as.character(participant),
        trialNum = as.numeric(as.character(trialNum))
    )]
    outliers_dt[, `:=`(
        participant = as.character(participant),
        trialNum = as.numeric(as.character(trialNum))
    )]

    # Find matches within time window
    matches <- data.table::rbindlist(lapply(seq_len(nrow(outliers_dt)), function(i) {
        outlier <- outliers_dt[i]

        # Filter to current participant/trial
        trial_data <- dt[participant == outlier$participant &
            trialNum == outlier$trialNum]

        if (nrow(trial_data) == 0) {
            return(NULL)
        }

        # Find closest time within tolerance
        time_diffs <- abs(trial_data$time - outlier$time)
        closest_idx <- which.min(time_diffs)

        if (time_diffs[closest_idx] > max_distance) {
            return(NULL)
        }

        trial_data[closest_idx]
    }))

    matches
}

#' Check if a heel strike is marked as an outlier
#'
#' @param participant Participant identifier
#' @param trialNum   Trial number
#' @param time       Time of the heel strike
#' @param tolerance  Maximum time difference to consider a match
#' @return TRUE if the heel strike is marked as an outlier, FALSE otherwise
check_outlier_heel_strike <- function(participant, trialNum, time, tolerance = 0.03) {
    if (!exists("outliers_steps_data", envir = .GlobalEnv)) {
        return(FALSE)
    }

    participant_char <- as.character(participant)
    trialNum_num <- as.numeric(trialNum)
    participant_outliers <- outliers_steps_data[
        outliers_steps_data$participant == participant_char &
            outliers_steps_data$trialNum == trialNum_num,
    ]

    if (nrow(participant_outliers) == 0) {
        return(FALSE)
    }

    diffs <- abs(participant_outliers$time - time)
    return(min(diffs) <= tolerance)
}

#' Mark outlier steps in heel strike data
#'
#' @param heel_strikes_data Data frame containing heel strike data
#' @param participant       Participant identifier
#' @param trialNum         Trial number
#' @param tolerance        Maximum time difference to consider a match
#' @return Modified heel_strikes_data with outlierSteps column updated
mark_outlier_steps <- function(heel_strikes_data, participant, trialNum, tolerance = 0.03) {
    # Initialize columns if they don't exist
    if (!"outlierSteps" %in% colnames(heel_strikes_data)) {
        heel_strikes_data$outlierSteps <- FALSE
    }

    # Check each heel strike against outlier data
    num_step_outliers <- 0
    for (i in seq_len(nrow(heel_strikes_data))) {
        if (check_outlier_heel_strike(participant, trialNum, heel_strikes_data$time[i], tolerance)) {
            heel_strikes_data$outlierSteps[i] <- TRUE
            num_step_outliers <- num_step_outliers + 1
        }
    }

    # Report step outlier statistics
    if (num_step_outliers > 0) {
        cat(sprintf(
            "DEBUG: Marked %d heel strikes as step outliers from CSV data\n",
            num_step_outliers
        ))
    }

    return(heel_strikes_data)
}

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
#' @param tolerance      Maximum absolute time difference (seconds) to consider a match
#' @return A **data.frame** with the requested removals / markings applied
apply_outliers <- function(data,
                           heel_outliers = NULL,
                           step_outliers = NULL,
                           tolerance = 0.03) {
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
        matches <- find_closest_heel_strikes(heel_outliers, dt, tolerance)
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
        matches <- find_closest_heel_strikes(step_outliers, dt, tolerance)
        if (!is.null(matches) && nrow(matches) > 0) {
            rows_to_mark <- dt[matches, on = .(participant, trialNum, time), nomatch = 0, which = TRUE]
            if (length(rows_to_mark)) {
                dt[rows_to_mark, outlierSteps := TRUE]
            }
        }
    }

    return(as.data.frame(dt))
}
