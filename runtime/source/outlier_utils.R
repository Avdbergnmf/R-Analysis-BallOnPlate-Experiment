# Utility functions for handling outliers

#' Find heel strikes with two-stage tolerance system
#'
#' @param outliers Data frame with outlier information
#' @param data_source Data frame containing heel strike data
#' @param search_tolerance Maximum time difference to search for potential matches (default 0.03s)
#' @param grouping_tolerance Maximum time difference to group steps together (default 0.001s)
#' @return Data table with all matched rows (including grouped steps)
find_closest_heel_strikes <- function(outliers, data_source, search_tolerance = 0.03, grouping_tolerance = 0.001) {
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

    # Find matches using two-stage tolerance system
    matches <- data.table::rbindlist(lapply(seq_len(nrow(outliers_dt)), function(i) {
        outlier <- outliers_dt[i]

        # Filter to current participant/trial
        trial_data <- dt[participant == outlier$participant &
            trialNum == outlier$trialNum]

        if (nrow(trial_data) == 0) {
            return(NULL)
        }

        # STAGE 1: Find ALL potential matches within search tolerance
        time_diffs <- abs(trial_data$time - outlier$time)
        within_search_idx <- which(time_diffs <= search_tolerance)

        if (length(within_search_idx) == 0) {
            return(NULL)
        }

        # Get the potential matches
        potential_matches <- trial_data[within_search_idx]

        # STAGE 2: Group steps that are very close together
        if (nrow(potential_matches) > 1) {
            # Sort by time to ensure proper grouping
            potential_matches <- potential_matches[order(time)]

            # Find groups of steps that are within grouping_tolerance of each other
            groups <- list()
            current_group <- 1
            groups[[current_group]] <- 1 # Start with first step

            for (j in 2:nrow(potential_matches)) {
                time_diff <- potential_matches$time[j] - potential_matches$time[j - 1]
                if (time_diff <= grouping_tolerance) {
                    # Add to current group
                    groups[[current_group]] <- c(groups[[current_group]], j)
                } else {
                    # Start new group
                    current_group <- current_group + 1
                    groups[[current_group]] <- j
                }
            }

            # Return ALL steps from ALL groups (since they're all close to the outlier)
            all_grouped_indices <- unlist(groups)
            return(potential_matches[all_grouped_indices])
        } else {
            # Only one match found, return it
            return(potential_matches)
        }
    }))

    matches
}

#' Find the closest heel strike within a time threshold
#'
#' Simple utility used by the manual outlier UI to locate the heel strike nearest
#' to a clicked time for a specific participant and trial. Returns NULL if no
#' heel strike is found within the provided threshold.
#'
#' @param clicked_time Numeric time value from the plot interaction
#' @param participant  Participant identifier
#' @param trial        Trial number
#' @param trial_data   Data frame containing heel strike data for plotting
#' @param threshold    Maximum absolute time difference allowed (seconds)
#' @return Single-row data.frame of the closest heel strike, or NULL
find_closest_heel_strike <- function(clicked_time, participant, trial, trial_data, threshold = 0.03) {
    if (is.null(trial_data) || nrow(trial_data) == 0) {
        return(NULL)
    }

    # If caller passed full dataset, filter to current participant/trial
    if ("participant" %in% names(trial_data) && "trialNum" %in% names(trial_data)) {
        # Attempt to narrow to the specific trial; if this yields rows, use it
        narrowed <- tryCatch(
            {
                trial_data[trial_data$participant == participant & trial_data$trialNum == as.numeric(trial), ]
            },
            error = function(e) NULL
        )
        if (!is.null(narrowed) && is.data.frame(narrowed) && nrow(narrowed) > 0) {
            trial_data <- narrowed
        }
    }

    time_diffs <- abs(trial_data$time - clicked_time)
    closest_idx <- which.min(time_diffs)

    if (length(closest_idx) == 0 || time_diffs[closest_idx] > threshold) {
        return(NULL)
    }

    return(trial_data[closest_idx, , drop = FALSE])
}

#' Check if a heel strike is marked as an outlier
#'
#' @param participant Participant identifier
#' @param trialNum   Trial number
#' @param time       Time of the heel strike
#' @param search_tolerance Maximum time difference to search for matches (default 0.03s)
#' @param grouping_tolerance Maximum time difference to group steps together (default 0.001s)
#' @return TRUE if the heel strike is marked as an outlier, FALSE otherwise
check_outlier_heel_strike <- function(participant, trialNum, time, search_tolerance = 0.03, grouping_tolerance = 0.001) {
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

    # Use the two-stage tolerance system
    diffs <- abs(participant_outliers$time - time)
    within_search <- diffs <= search_tolerance

    if (!any(within_search)) {
        return(FALSE)
    }

    # If multiple matches found, check if they're within grouping tolerance
    search_matches <- participant_outliers[within_search, ]
    if (nrow(search_matches) > 1) {
        # Sort by time and check if any are within grouping tolerance
        search_matches <- search_matches[order(time)]
        for (i in 1:(nrow(search_matches) - 1)) {
            if (abs(search_matches$time[i + 1] - search_matches$time[i]) <= grouping_tolerance) {
                return(TRUE) # Found grouped steps
            }
        }
    }

    # Single match or no grouping found
    return(min(diffs) <= search_tolerance)
}

#' Mark outlier steps in heel strike data
#'
#' @param heel_strikes_data Data frame containing heel strike data
#' @param participant       Participant identifier
#' @param trialNum         Trial number
#' @param search_tolerance Maximum time difference to search for matches (default 0.03s)
#' @param grouping_tolerance Maximum time difference to group steps together (default 0.001s)
#' @return Modified heel_strikes_data with outlierSteps column updated
mark_outlier_steps <- function(heel_strikes_data, participant, trialNum, search_tolerance = 0.03, grouping_tolerance = 0.001) {
    # Initialize columns if they don't exist
    if (!"outlierSteps" %in% colnames(heel_strikes_data)) {
        heel_strikes_data$outlierSteps <- FALSE
    }

    # Check each heel strike against outlier data
    num_step_outliers <- 0
    for (i in seq_len(nrow(heel_strikes_data))) {
        if (check_outlier_heel_strike(participant, trialNum, heel_strikes_data$time[i], search_tolerance, grouping_tolerance)) {
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
        flush.console()
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
