#' Outlier Marking Functions
#'
#' Functions for marking heel strikes as outliers

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
