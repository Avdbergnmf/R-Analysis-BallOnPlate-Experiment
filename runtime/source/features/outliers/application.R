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
    outlier_app_logger <- create_module_logger("OUTLIER-APPLICATION")
    
    # Log input statistics
    n_heel_outliers <- if (is.null(heel_outliers)) 0 else nrow(heel_outliers)
    n_step_outliers <- if (is.null(step_outliers)) 0 else nrow(step_outliers)
    n_input_rows <- nrow(data)
    
    outlier_app_logger("INFO", "Starting outlier application - Input:", n_input_rows, "rows,", n_heel_outliers, "heel outliers,", n_step_outliers, "step outliers")
    
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
        outlier_app_logger("DEBUG", "Processing heel strike removals")
        matches <- find_closest_heel_strikes(heel_outliers, dt, tolerance, grouping_tolerance)
        if (!is.null(matches) && nrow(matches) > 0) {
            matches <- data.table::as.data.table(matches)
            rows_to_remove <- dt[matches, on = .(participant, trialNum, time), nomatch = 0, which = TRUE]
            if (length(rows_to_remove)) {
                n_removed <- length(rows_to_remove)
                dt <- dt[-rows_to_remove]
                outlier_app_logger("INFO", "Removed", n_removed, "heel strikes from dataset")
            } else {
                outlier_app_logger("WARN", "Found heel strike matches but could not remove any rows")
            }
        } else {
            outlier_app_logger("WARN", "No heel strike matches found for removal")
        }
    } else {
        outlier_app_logger("DEBUG", "No heel outliers to process")
    }

    # --------------------------------------------------
    # 2) MARK step outliers for exclusion in analysis
    # --------------------------------------------------
    if (!is.null(step_outliers) && nrow(step_outliers) > 0) {
        outlier_app_logger("DEBUG", "Processing step outlier markings")
        matches <- find_closest_heel_strikes(step_outliers, dt, step_tolerance, grouping_tolerance)
        if (!is.null(matches) && nrow(matches) > 0) {
            rows_to_mark <- dt[matches, on = .(participant, trialNum, time), nomatch = 0, which = TRUE]
            if (length(rows_to_mark)) {
                n_marked <- length(rows_to_mark)
                dt[rows_to_mark, outlierSteps := TRUE]
                outlier_app_logger("INFO", "Marked", n_marked, "steps as outliers")
            } else {
                outlier_app_logger("WARN", "Found step outlier matches but could not mark any rows")
            }
        } else {
            outlier_app_logger("WARN", "No step outlier matches found for marking")
        }
    } else {
        outlier_app_logger("DEBUG", "No step outliers to process")
    }

    # Log final statistics
    n_output_rows <- nrow(dt)
    n_outlier_steps <- sum(dt$outlierSteps, na.rm = TRUE)
    outlier_app_logger("INFO", "Outlier application completed - Output:", n_output_rows, "rows,", n_outlier_steps, "marked as outliers")
    
    return(as.data.frame(dt))
}
