#' Gait Outlier Processing
#'
#' Helper functions for applying manual and automatic outlier decisions to gait
#' heel-strike data. These routines were migrated from the standalone outliers
#' feature so that gait owns the entire foot-event pipeline.

# =============================================================================
# MATCHING UTILITIES
# =============================================================================

#' Find heel strikes matching provided outlier annotations
#'
#' Uses a two-stage tolerance system: first a broad search tolerance to locate
#' candidate heel strikes, followed by an optional grouping tolerance that keeps
#' closely spaced steps together.
find_closest_heel_strikes <- function(outliers,
                                      data_source,
                                      search_tolerance = 0.03,
                                      grouping_tolerance = 0.001) {
  logger <- create_module_logger("GAIT-OUTLIER-DETECTION")

  if (is.null(outliers) || nrow(outliers) == 0) {
    return(NULL)
  }

  logger("DEBUG", "Matching", nrow(outliers), "outlier records against", nrow(data_source), "heel strikes")

  dt <- data.table::as.data.table(data_source)
  outliers_dt <- data.table::as.data.table(outliers)

  dt[, `:=`(
    participant = as.character(participant),
    trialNum = as.numeric(as.character(trialNum))
  )]
  outliers_dt[, `:=`(
    participant = as.character(participant),
    trialNum = as.numeric(as.character(trialNum))
  )]

  heel_summary <- dt[, .N, by = .(participant, trialNum)]
  outlier_summary <- outliers_dt[
    participant %in% heel_summary$participant &
      trialNum %in% heel_summary$trialNum,
    .N, by = .(participant, trialNum)
  ]

  summarise_for_log <- function(summary_dt, label) {
    if (is.null(summary_dt) || nrow(summary_dt) == 0) {
      return(paste0(label, ": none"))
    }
    paste0(
      label, ": ",
      paste(sprintf("P%s_T%s=%d", summary_dt$participant, summary_dt$trialNum, summary_dt$N), collapse = ", ")
    )
  }

  logger("DEBUG",
         summarise_for_log(heel_summary, "Heel strikes"),
         "|",
         summarise_for_log(outlier_summary, "Outlier annotations"))

  dt[, row_id := .I]
  outliers_dt[, outlier_id := .I]

  potential_matches <- dt[outliers_dt, on = .(participant, trialNum), nomatch = 0, allow.cartesian = TRUE]
  if (nrow(potential_matches) == 0) {
    logger("DEBUG", "No potential matches for supplied outliers")
    return(NULL)
  }

  potential_matches[, time_diff := abs(time - i.time)]
  within_tolerance <- potential_matches[time_diff <= search_tolerance]
  if (nrow(within_tolerance) == 0) {
    logger("DEBUG", "No heel strikes within search tolerance")
    return(NULL)
  }

  matches_list <- list()
  for (outlier_id in unique(within_tolerance$outlier_id)) {
    outlier_matches <- within_tolerance[outlier_id == outlier_id]

    if (nrow(outlier_matches) > 1) {
      outlier_matches <- outlier_matches[order(time)]
      groups <- list()
      current_group <- 1
      groups[[current_group]] <- 1

      for (idx in 2:nrow(outlier_matches)) {
        time_diff <- outlier_matches$time[idx] - outlier_matches$time[idx - 1]
        if (time_diff <= grouping_tolerance) {
          groups[[current_group]] <- c(groups[[current_group]], idx)
        } else {
          current_group <- current_group + 1
          groups[[current_group]] <- idx
        }
      }

      matches_list[[length(matches_list) + 1]] <- outlier_matches[unlist(groups)]
    } else {
      matches_list[[length(matches_list) + 1]] <- outlier_matches
    }
  }

  matches <- data.table::rbindlist(matches_list, fill = TRUE)
  if ("row_id" %in% names(matches)) {
    matches <- matches[!duplicated(row_id)]
  }
  logger("INFO",
         "Processed", length(unique(within_tolerance$outlier_id)), "outliers and found",
         if (is.null(matches)) 0 else nrow(matches), "matches")

  matches
}

#' Find the heel strike closest to a clicked timestamp
find_closest_heel_strike <- function(clicked_time,
                                     participant,
                                     trial,
                                     trial_data,
                                     threshold = 0.03) {
  if (is.null(trial_data) || nrow(trial_data) == 0) {
    return(NULL)
  }

  if ("participant" %in% names(trial_data) && "trialNum" %in% names(trial_data)) {
    narrowed <- tryCatch(
      {
        trial_data[trial_data$participant == participant &
                     trial_data$trialNum == as.numeric(trial), ]
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

  trial_data[closest_idx, , drop = FALSE]
}

# =============================================================================
# CHECKING AND MARKING
# =============================================================================

#' Check whether a heel strike is marked as an outlier
check_outlier_heel_strike <- function(participant,
                                      trialNum,
                                      time,
                                      search_tolerance = 0.03,
                                      grouping_tolerance = 0.001) {
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
  within_search <- diffs <= search_tolerance
  if (!any(within_search)) {
    return(FALSE)
  }

  search_matches <- participant_outliers[within_search, ]
  if (nrow(search_matches) > 1) {
    search_matches <- search_matches[order(time)]
    for (idx in 1:(nrow(search_matches) - 1)) {
      if (abs(search_matches$time[idx + 1] - search_matches$time[idx]) <= grouping_tolerance) {
        return(TRUE)
      }
    }
  }

  min(diffs) <= search_tolerance
}

#' Mark outlier steps within a heel-strike dataset
mark_outlier_steps <- function(heel_strikes_data,
                               participant,
                               trialNum,
                               search_tolerance = 0.03,
                               grouping_tolerance = 0.001) {
  if (!"outlierSteps" %in% colnames(heel_strikes_data)) {
    heel_strikes_data$outlierSteps <- FALSE
  }

  num_step_outliers <- 0
  for (idx in seq_len(nrow(heel_strikes_data))) {
    if (check_outlier_heel_strike(participant, trialNum, heel_strikes_data$time[idx],
                                  search_tolerance, grouping_tolerance)) {
      heel_strikes_data$outlierSteps[idx] <- TRUE
      num_step_outliers <- num_step_outliers + 1
    }
  }

  if (num_step_outliers > 0) {
    cat(sprintf("DEBUG: Marked %d heel strikes as step outliers from CSV data\n", num_step_outliers))
    flush.console()
  }

  heel_strikes_data
}

# =============================================================================
# APPLICATION
# =============================================================================

#' Apply outlier removals and markings
apply_outliers <- function(data,
                           heel_outliers = NULL,
                           step_outliers = NULL,
                           tolerance = 0.03,
                           step_tolerance = 0.03,
                           grouping_tolerance = 0.001) {
  logger <- create_module_logger("GAIT-OUTLIER-APPLICATION")

  n_input_rows <- nrow(data)
  n_step_outliers <- if (is.null(step_outliers)) 0 else nrow(step_outliers)
  n_heel_outliers <- if (is.null(heel_outliers)) 0 else nrow(heel_outliers)

  logger("INFO", "Applying outliers to", n_input_rows, "rows (",
         n_step_outliers, "step outliers,", n_heel_outliers, "heel outliers )")

  dt <- data.table::as.data.table(data)
  dt[, `:=`(
    participant = as.character(participant),
    trialNum = as.numeric(as.character(trialNum))
  )]

  if (!"outlierSteps" %in% names(dt)) {
    dt[, outlierSteps := FALSE]
  }

  dt_summary <- dt[, .N, by = .(participant, trialNum)]

  summarise_for_log <- function(summary_dt, label) {
    if (is.null(summary_dt) || nrow(summary_dt) == 0) {
      return(paste0(label, ": none"))
    }
    paste0(
      label, ": ",
      paste(sprintf("P%s_T%s=%d", summary_dt$participant, summary_dt$trialNum, summary_dt$N), collapse = ", ")
    )
  }

  summarise_outliers <- function(outlier_data) {
    if (is.null(outlier_data) || nrow(outlier_data) == 0) {
      return(NULL)
    }
    tmp <- data.table::as.data.table(outlier_data)
    tmp[, `:=`(
      participant = as.character(participant),
      trialNum = as.numeric(as.character(trialNum))
    )]
    tmp[
      participant %in% dt_summary$participant &
        trialNum %in% dt_summary$trialNum,
      .N, by = .(participant, trialNum)
    ]
  }

  heel_outlier_summary <- summarise_outliers(heel_outliers)
  step_outlier_summary <- summarise_outliers(step_outliers)

  logger("DEBUG",
         summarise_for_log(dt_summary, "Heel strikes detected"),
         "|",
         summarise_for_log(heel_outlier_summary, "Heel-outlier rows"),
         "|",
         summarise_for_log(step_outlier_summary, "Step-outlier rows"))

  if (!is.null(heel_outliers) && nrow(heel_outliers) > 0) {
    logger("DEBUG", "Processing heel strike removals")
    matches <- find_closest_heel_strikes(heel_outliers, dt, tolerance, grouping_tolerance)
    if (!is.null(matches) && nrow(matches) > 0) {
      matches <- data.table::as.data.table(matches)
      rows_to_remove <- dt[matches, on = .(participant, trialNum, time), nomatch = 0, which = TRUE]
      rows_to_remove <- unique(rows_to_remove)
      if (length(rows_to_remove)) {
        dt <- dt[-rows_to_remove]
        logger("INFO", "Removed", length(rows_to_remove), "heel strikes")
      } else {
        logger("WARN", "Heel strike matches found but no rows removed")
      }
    } else {
      logger("WARN", "No heel strike matches found for removal")
    }
  }

  if (!is.null(step_outliers) && nrow(step_outliers) > 0) {
    logger("DEBUG", "Processing step outlier markings")
    matches <- find_closest_heel_strikes(step_outliers, dt, step_tolerance, grouping_tolerance)
    if (!is.null(matches) && nrow(matches) > 0) {
      rows_to_mark <- dt[matches, on = .(participant, trialNum, time), nomatch = 0, which = TRUE]
      rows_to_mark <- unique(rows_to_mark)
      if (length(rows_to_mark)) {
        dt[rows_to_mark, outlierSteps := TRUE]
        logger("INFO", "Marked", length(rows_to_mark), "steps as outliers")
      } else {
        logger("WARN", "Step outlier matches found but no rows marked")
      }
    } else {
      logger("WARN", "No step outlier matches found for marking")
    }
  }

  logger("INFO", "Outlier application finished on", nrow(dt), "rows with",
         sum(dt$outlierSteps, na.rm = TRUE), "marked outliers")

  as.data.frame(dt)
}

# =============================================================================
# SUMMARY
# =============================================================================

#' Compute simple statistics about marked outliers
outlier_stats <- function(data) {
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
