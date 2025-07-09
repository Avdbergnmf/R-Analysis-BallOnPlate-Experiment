# Utility functions for handling outliers

#' Find the closest heel strike in a dataset
#'
#' @param outlier_time Time of the outlier
#' @param participant  Participant identifier
#' @param trial_num    Trial number
#' @param data_source  Data frame containing heel strike data
#' @param max_distance Maximum time difference allowed
#' @return The closest row from \code{data_source} within \code{max_distance} or NULL
find_closest_heel_strike <- function(outlier_time, participant, trial_num,
                                     data_source = NULL, max_distance = 0.03) {
    if (is.null(data_source)) {
        data_source <- allGaitParams
    }

    trial_data <- data_source %>%
        dplyr::filter(participant == !!participant & trialNum == !!trial_num) %>%
        dplyr::arrange(time)

    if (nrow(trial_data) == 0) {
        return(NULL)
    }

    closest_step <- trial_data %>%
        dplyr::mutate(time_diff = abs(time - !!outlier_time)) %>%
        dplyr::arrange(time_diff) %>%
        dplyr::slice(1)

    if (nrow(closest_step) > 0 && closest_step$time_diff <= max_distance) {
        return(closest_step)
    }
    NULL
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

#' Backward compatibility function for outlier checking
#'
#' @inheritParams check_outlier_heel_strike
#' @return TRUE if the heel strike is marked as an outlier, FALSE otherwise
is_outlier_heel_strike <- function(participant, trialNum, time, tolerance = 0.03) {
    check_outlier_heel_strike(participant, trialNum, time, tolerance)
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

#' Apply manual outliers to a gait parameter table
#'
#' Removes heel strikes specified in \code{heel_outliers} and marks step outliers
#' in \code{step_outliers}. Utilises \code{mark_outlier_steps()} from
#' \code{find_foot_events.R}.
#'
#' @param data            allGaitParams data frame
#' @param heel_outliers   data frame with columns participant, trialNum, time
#' @param step_outliers   data frame with columns participant, trialNum, time
#' @param tolerance       matching tolerance in seconds
#' @return Modified \code{data}
apply_outliers_to_gaitparams <- function(data, heel_outliers, step_outliers,
                                         tolerance = 0.03) {
    rows_to_remove <- integer(0)

    if (!is.null(heel_outliers) && nrow(heel_outliers) > 0) {
        for (i in seq_len(nrow(heel_outliers))) {
            hs <- heel_outliers[i, ]
            closest <- find_closest_heel_strike(hs$time, hs$participant,
                as.numeric(hs$trialNum),
                data_source = data,
                max_distance = tolerance
            )
            if (!is.null(closest)) {
                idx <- which(data$time == closest$time &
                    data$participant == closest$participant &
                    data$trialNum == closest$trialNum)
                rows_to_remove <- c(rows_to_remove, idx)
            }
        }
        rows_to_remove <- unique(rows_to_remove)
        if (length(rows_to_remove) > 0) {
            data <- data[-rows_to_remove, ]
            rownames(data) <- NULL
        }
    }

    # Reset outlier related columns
    data$outlierSteps <- FALSE
    if ("heelStrikeBad" %in% colnames(data)) data$heelStrikeBad <- NULL
    if ("suspect" %in% colnames(data)) data$suspect <- NULL

    if (!is.null(step_outliers) && nrow(step_outliers) > 0) {
        for (i in seq_len(nrow(step_outliers))) {
            so <- step_outliers[i, ]
            closest <- find_closest_heel_strike(so$time, so$participant,
                as.numeric(so$trialNum),
                data_source = data,
                max_distance = tolerance
            )
            if (!is.null(closest)) {
                idx <- which(data$time == closest$time &
                    data$participant == closest$participant &
                    data$trialNum == closest$trialNum)
                if (length(idx) > 0) {
                    data$outlierSteps[idx] <- TRUE
                }
            }
        }
    }

    data
}
