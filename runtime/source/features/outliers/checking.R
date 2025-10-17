#' Outlier Checking Functions
#'
#' Functions for checking if heel strikes are marked as outliers

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
