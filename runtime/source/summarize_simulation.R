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

    # Get velocity/acceleration parameters that need absolute values
    abs_params <- c("vx", "vy", "vx_world", "ax", "ay", "ax_world")

    # Compute attempt-level metrics and summary statistics
    attempts <- compute_attempts_data(sim_data, min_attempt_duration)
    avg_attempt_duration <- if (nrow(attempts) > 0) mean(attempts$duration, na.rm = TRUE) else NA_real_
    n_attempts <- nrow(attempts)

    # Compute basic simulation info
    sim_info <- analysis_data %>%
        summarise(
            sim_duration = max(simulation_time, na.rm = TRUE) - min(simulation_time, na.rm = TRUE),
            n_samples = n(),
            n_respawns = length(unique(respawn_segment)) - 1,
            mean_arcDeg = mean(arcDeg, na.rm = TRUE),
            n_arcDeg_changes = length(unique(arcDeg)),
            final_work = if ("work" %in% colnames(analysis_data)) last(work, default = NA_real_) else NA_real_,
            final_work_world = if ("work_world" %in% colnames(analysis_data)) last(work_world, default = NA_real_) else NA_real_
        ) %>%
        mutate(
            n_ball_falls = n_respawns,
            n_attempts = n_attempts,
            avg_time_on_plate = avg_attempt_duration
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

#' Retrieve attempt data for a participant and trial
#'
#' @param p Participant identifier
#' @param t Trial number
#' @param min_attempt_duration Minimum duration (s) for an attempt to be counted
#' @return Tibble of attempt metrics
#' @export
get_attempts <- function(p, t, min_attempt_duration = 0) {
    sim_data <- get_simulation_data(p, t)
    attempts <- compute_attempts_data(sim_data, min_attempt_duration)
    return(attempts)
}

#' Compute attempt-level metrics for a simulation
#'
#' @param sim_data Enhanced simulation data from get_simulation_data()
#' @param min_attempt_duration Minimum duration (s) for an attempt to be counted
#' @return Tibble with metrics for each attempt
#' @export
compute_attempts_data <- function(sim_data, min_attempt_duration = 0) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(tibble())
    }

    analysis_data <- sim_data %>%
        dplyr::filter(simulating == TRUE)

    if (nrow(analysis_data) == 0) {
        return(tibble())
    }

    params <- get_simulation_parameters()
    param_names <- names(params)

    # Filter to only include parameters that actually exist in the data
    param_names <- intersect(param_names, colnames(analysis_data))

    abs_params <- c("vx", "vy", "vx_world", "ax", "ay", "ax_world")

    attempts <- analysis_data %>%
        group_by(respawn_segment) %>%
        summarise(
            start_time = min(simulation_time, na.rm = TRUE),
            end_time = max(simulation_time, na.rm = TRUE),
            duration = end_time - start_time,
            # Compute metrics for all data within each attempt (already filtered for simulating == TRUE)
            across(
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
            ),
            .groups = "drop"
        ) %>%
        dplyr::filter(duration >= min_attempt_duration) %>%
        arrange(start_time) %>%
        mutate(attempt = row_number(), .before = 1)

    return(attempts)
}

#' Get task metrics for all participants and trials
#'
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @return Tibble with task metrics for all participant/trial combinations
#' @export
get_all_task_metrics <- function(loop_function) {
    return(loop_function(get_metrics, datasets_to_verify = c("task", "level")))
}

#' Get attempt metrics for all participants and trials
#'
#' @param loop_function Function to use for processing (get_data_from_loop or get_data_from_loop_parallel)
#' @param min_attempt_duration Minimum duration (s) for an attempt to be counted
#' @return Tibble with attempt metrics for all participant/trial combinations
#' @export
get_all_attempt_metrics <- function(loop_function, min_attempt_duration = 0) {
    attempt_fun <- function(p, t) {
        get_attempts(p, t, min_attempt_duration)
    }

    return(loop_function(attempt_fun, datasets_to_verify = c("task", "level")))
}

#' Merge mu gait data with mu_task simulation data
#'
#' @param mu_gait Existing mu gait data
#' @param mu_task Task simulation metrics data
#' @return Combined data with both gait and task metrics
#' @export
merge_mu_with_task <- function(mu_gait, mu_task) {
    if (is.null(mu_task) || nrow(mu_task) == 0) {
        message("No task data available for merging")
        return(mu_gait)
    }

    if (is.null(mu_gait) || nrow(mu_gait) == 0) {
        message("No gait data available for merging")
        return(mu_task)
    }

    # Create copies of the data frames to avoid modifying the originals
    mu_gait_copy <- mu_gait
    mu_task_copy <- mu_task

    # Convert trialNum to numeric in both datasets
    mu_gait_copy$trialNum <- as.numeric(as.character(mu_gait_copy$trialNum))
    mu_task_copy$trialNum <- as.numeric(as.character(mu_task_copy$trialNum))

    # Filter task data using the common helper function
    mu_task_filtered <- filter_by_gait_combinations(mu_gait_copy, mu_task_copy, "task")

    # Add task. prefix to all task metric columns (except participant and trialNum)
    task_metrics_cols <- setdiff(colnames(mu_task_filtered), c("participant", "trialNum"))
    mu_task_renamed <- mu_task_filtered %>%
        rename_with(~ paste0("task.", .x), all_of(task_metrics_cols))

    # Merge based on participant and trialNum (using left_join to preserve all gait data)
    merged_data <- left_join(
        mu_gait_copy,
        mu_task_renamed,
        by = c("participant", "trialNum")
    )

    return(merged_data)
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
