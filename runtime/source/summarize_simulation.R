#' Calculate total score from simulation data
#'
#' @param sim_data Enhanced simulation data from get_simulation_data()
#' @return List with final_score, time_in_bowl_bonus, and total_score
#' @export
calculate_total_score <- function(sim_data) {
    # Initialize default values
    final_score <- NA_real_
    time_in_bowl_bonus <- 0

    # Extract participant and trial from sim_data for debugging
    participant_id <- if ("participant" %in% colnames(sim_data)) as.character(sim_data$participant[1]) else "Unknown"
    trial_id <- if ("trialNum" %in% colnames(sim_data)) as.character(sim_data$trialNum[1]) else "Unknown"

    # Extract participant and trial from sim_data for debugging
    participant_id <- if ("participant" %in% colnames(sim_data)) as.character(sim_data$participant[1]) else "Unknown"
    trial_id <- if ("trialNum" %in% colnames(sim_data)) as.character(sim_data$trialNum[1]) else "Unknown"

    # Get final score from simulation data
    if ("score" %in% colnames(sim_data)) {
        # Get the final (last) non-NA score value instead of maximum
        score_values <- sim_data$score[!is.na(sim_data$score)]
        if (length(score_values) > 0) {
            final_score <- tail(score_values, 1) # Take the last value instead of max
        }
    }

    # Add time_in_bowl bonus points if the column exists
    if ("time_in_bowl" %in% colnames(sim_data)) {
        time_in_bowl_values <- sim_data$time_in_bowl[!is.na(sim_data$time_in_bowl)]
        if (length(time_in_bowl_values) > 0) {
            # Convert time_in_bowl to points (currently just taking the raw value)
            time_in_bowl_bonus <- tail(time_in_bowl_values, 1)
        }
    }

    # Calculate total score
    total_score <- if (is.na(final_score)) time_in_bowl_bonus else final_score + time_in_bowl_bonus

    # Return calculated scores
    return(list(
        final_score = final_score,
        time_in_bowl_bonus = time_in_bowl_bonus,
        total_score = total_score
    ))
}

#' Compute task metrics for a single simulation dataset
#'
#' @param sim_data Enhanced simulation data from get_simulation_data()
#' @param min_attempt_duration Minimum duration (s) for an attempt to be counted
#' @return Tibble with task metrics
#' @export
compute_task_metrics <- function(sim_data, min_attempt_duration = 0) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(tibble())
    }

    # Filter to only use real simulation data (when ball is on plate)
    analysis_data <- sim_data %>%
        dplyr::filter(simulating == TRUE)

    if (nrow(analysis_data) == 0) {
        return(tibble())
    }

    # Get parameters to summarize
    params <- get_simulation_parameters()
    param_names <- names(params)

    # Filter to only include parameters that actually exist in the data
    param_names <- intersect(param_names, colnames(analysis_data))

    # Debug: Check risk columns
    risk_cols <- c("drop_risk_bin", "drop_lambda", "drop_risk_1s", "velocity_towards_edge")
    existing_risk_cols <- intersect(risk_cols, colnames(analysis_data))
    if (length(existing_risk_cols) > 0) {
        cat(sprintf("[TASK_METRICS] Found risk columns: %s\n", paste(existing_risk_cols, collapse = ", ")))
        for (col in existing_risk_cols) {
            valid_count <- sum(!is.na(analysis_data[[col]]))
            cat(sprintf("[TASK_METRICS] %s: %d/%d valid values\n", col, valid_count, nrow(analysis_data)))
        }
    } else {
        cat("[TASK_METRICS] No risk columns found in analysis_data\n")
    }

    # Get velocity/acceleration parameters that need absolute values
    abs_params <- c("vx", "vy", "vx_world", "ax", "ay", "ax_world")

    # Count ball falls (respawns)
    n_ball_falls <- length(unique(analysis_data$respawn_segment)) - 1

    # Compute basic simulation info
    sim_info <- analysis_data %>%
        summarise(
            sim_duration = max(simulation_time, na.rm = TRUE) - min(simulation_time, na.rm = TRUE),
            n_samples = n(),
            mean_arcDeg = mean(arcDeg, na.rm = TRUE),
            n_arcDeg_changes = length(unique(arcDeg)),
            final_work = if ("work" %in% colnames(analysis_data)) max(work) else NA_real_,
            final_work_plate = if ("work_plate" %in% colnames(analysis_data)) max(work_plate) else NA_real_,
            final_work_world = if ("work_world" %in% colnames(analysis_data)) max(work_world) else NA_real_
        ) %>%
        mutate(
            n_ball_falls = n_ball_falls
        )

    # Compute metrics for all parameters in a single operation
    # Only include data when ball is on the plate (using simulating column)
    param_metrics <- analysis_data %>%
        summarise(across(
            all_of(param_names),
            list(
                mean = ~ if (cur_column() %in% abs_params) mean(abs(.), na.rm = TRUE) else mean(., na.rm = TRUE),
                sd = ~ sd(., na.rm = TRUE),
                cv = ~ {
                    m <- if (cur_column() %in% abs_params) mean(abs(.), na.rm = TRUE) else mean(., na.rm = TRUE)
                    s <- sd(., na.rm = TRUE)
                    if (is.finite(m) && is.finite(s) && m != 0) s / m else NA_real_
                }
            )
        ))

    # Combine all metrics
    result <- bind_cols(sim_info, param_metrics)

    # Debug: Check if risk metrics made it into the result
    risk_metric_cols <- grep("drop_risk|velocity_towards_edge", colnames(result), value = TRUE)
    if (length(risk_metric_cols) > 0) {
        cat(sprintf("[TASK_METRICS] Risk metrics in result: %s\n", paste(risk_metric_cols, collapse = ", ")))
        for (col in risk_metric_cols) {
            value <- result[[col]]
            cat(sprintf("[TASK_METRICS] %s: %s\n", col, ifelse(is.na(value), "NA", as.character(value))))
        }
    } else {
        cat("[TASK_METRICS] No risk metrics found in result\n")
    }

    # Add corrected dist_to_escape_ratio_mean (includes non-simulating samples as 0)
    if ("dist_to_escape_ratio" %in% colnames(sim_data)) {
        # Create corrected version: set non-simulating samples to 0, keep simulating samples as-is
        corrected_escape_ratio <- ifelse(sim_data$simulating, sim_data$dist_to_escape_ratio, 0)
        result$dist_to_escape_ratio_mean_corrected <- mean(corrected_escape_ratio, na.rm = TRUE)
    } else {
        result$dist_to_escape_ratio_mean_corrected <- NA_real_
    }

    # Add post-log metrics (log1p of means to handle zeros properly)
    # post_log_v_to_edge: log1p of velocity_towards_edge_mean
    if ("velocity_towards_edge_mean" %in% colnames(result)) {
        v_to_edge_mean <- result$velocity_towards_edge_mean
        if (!is.na(v_to_edge_mean)) {
            result$post_log_v_to_edge <- log1p(v_to_edge_mean)
        } else {
            result$post_log_v_to_edge <- NA_real_
        }
    } else {
        result$post_log_v_to_edge <- NA_real_
    }

    # post_log_edge_pressure: log1p of edge_pressure_mean
    if ("edge_pressure_mean" %in% colnames(result)) {
        edge_pressure_mean <- result$edge_pressure_mean
        if (!is.na(edge_pressure_mean)) {
            result$post_log_edge_pressure <- log1p(edge_pressure_mean)
        } else {
            result$post_log_edge_pressure <- NA_real_
        }
    } else {
        result$post_log_edge_pressure <- NA_real_
    }


    # add total score
    scores <- calculate_total_score(sim_data)
    result <- result %>%
        mutate(
            total_score = scores$total_score
        )

    return(result)
}

# Helper for get_all_task_metrics
get_metrics <- function(p, t) {
    sim_data <- get_simulation_data(p, t)
    return(compute_task_metrics(sim_data))
}

#' Get task metrics for all participants and trials
#'
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @return Tibble with task metrics for all participant/trial combinations
#' @export
get_all_task_metrics <- function(loop_function) {
    return(loop_function(get_metrics, datasets_to_verify = c("task", "level")))
}


#' Process task metrics data with time slicing
#'
#' @param task_data The task metrics data frame
#' @param slice_length Length of each time slice in seconds
#' @param remove_middle_slices Whether to keep only first and last slices
#' @return Processed task metrics data with slice indices
#' @export
process_task_metrics_sliced <- function(task_data, slice_length = 180, remove_middle_slices = FALSE) {
    # Add slice index based on simulation time
    task_data <- task_data %>%
        group_by(participant, trialNum) %>%
        mutate(
            slice_index = floor((simulation_time - min(simulation_time, na.rm = TRUE)) / slice_length) + 1
        ) %>%
        ungroup()

    # If remove_middle_slices is TRUE, keep only first and last slice
    if (remove_middle_slices) {
        task_data <- task_data %>%
            group_by(trialNum) %>%
            dplyr::filter(slice_index == min(slice_index, na.rm = TRUE) | slice_index == max(slice_index, na.rm = TRUE)) %>%
            ungroup()
    }

    return(task_data)
}

#' Get filtered and sliced task metrics
#'
#' @param allTaskMetrics The complete task metrics data frame
#' @param selected_participants Vector of participant IDs to include
#' @param selected_trials Vector of trial numbers to include
#' @param slice_length Length of each time slice in seconds
#' @param remove_middle_slices Whether to keep only first and last slices
#' @return Filtered and sliced task metrics data
#' @export
get_task_metrics_sliced <- function(allTaskMetrics, selected_participants, selected_trials,
                                    slice_length = 180, remove_middle_slices = FALSE) {
    # Filter by selected participants and trials
    task_data <- allTaskMetrics %>%
        dplyr::filter(
            participant %in% selected_participants,
            trialNum %in% selected_trials
        )

    # Process with time slicing
    task_data <- process_task_metrics_sliced(task_data, slice_length, remove_middle_slices)

    return(task_data)
}
