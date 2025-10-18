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
    outlier_logger <- create_module_logger("OUTLIER-DETECTION")
    
    if (is.null(outliers) || nrow(outliers) == 0) {
        return(NULL)
    }

    outlier_logger("DEBUG", "Starting find_closest_heel_strikes with", nrow(outliers), "outliers and", nrow(data_source), "heel strikes")

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

    # Find matches using vectorized approach
    outlier_logger("DEBUG", "Starting vectorized matching approach")
    start_time <- Sys.time()
    
    # Create a cross join to find all potential matches
    # This is much more efficient than the lapply loop
    dt[, row_id := .I]  # Add row ID for tracking
    outliers_dt[, outlier_id := .I]  # Add outlier ID for tracking
    
    # Cross join on participant and trialNum to get all potential matches
    potential_matches <- dt[outliers_dt, on = .(participant, trialNum), nomatch = 0, allow.cartesian = TRUE]
    
    if (nrow(potential_matches) == 0) {
        outlier_logger("DEBUG", "No potential matches found")
        return(NULL)
    }
    
    # Calculate time differences for all potential matches at once
    potential_matches[, time_diff := abs(time - i.time)]
    
    # Filter to matches within search tolerance
    within_tolerance <- potential_matches[time_diff <= search_tolerance]
    
    if (nrow(within_tolerance) == 0) {
        outlier_logger("DEBUG", "No matches within search tolerance")
        return(NULL)
    }
    
    # Group by outlier_id and apply grouping logic
    matches_list <- list()
    unique_outliers <- unique(within_tolerance$outlier_id)
    
    for (outlier_id in unique_outliers) {
        outlier_matches <- within_tolerance[outlier_id == outlier_id]
        
        if (nrow(outlier_matches) > 1) {
            # Sort by time and apply grouping logic
            outlier_matches <- outlier_matches[order(time)]
            
            # Find groups of steps that are within grouping_tolerance of each other
            groups <- list()
            current_group <- 1
            groups[[current_group]] <- 1
            
            for (j in 2:nrow(outlier_matches)) {
                time_diff <- outlier_matches$time[j] - outlier_matches$time[j - 1]
                if (time_diff <= grouping_tolerance) {
                    groups[[current_group]] <- c(groups[[current_group]], j)
                } else {
                    current_group <- current_group + 1
                    groups[[current_group]] <- j
                }
            }
            
            # Return ALL steps from ALL groups
            all_grouped_indices <- unlist(groups)
            matches_list[[length(matches_list) + 1]] <- outlier_matches[all_grouped_indices]
        } else {
            matches_list[[length(matches_list) + 1]] <- outlier_matches
        }
    }
    
    matches <- data.table::rbindlist(matches_list, fill = TRUE)
    end_time <- Sys.time()
    
    # Log statistics
    n_outliers_processed <- length(unique_outliers)
    n_matches_found <- if (is.null(matches)) 0 else nrow(matches)
    outlier_logger("INFO", "Outlier matching results:", n_outliers_processed, "outliers processed,", n_matches_found, "matches found")
    outlier_logger("DEBUG", "Vectorized matching completed in", round(as.numeric(end_time - start_time, units = "secs"), 2), "seconds")

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
