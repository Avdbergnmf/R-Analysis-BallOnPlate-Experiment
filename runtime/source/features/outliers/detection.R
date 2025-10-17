#' Outlier Detection Functions
#'
#' Functions for finding and matching heel strikes with outlier data

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
