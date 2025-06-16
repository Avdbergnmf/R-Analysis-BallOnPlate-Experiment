#' Compute task metrics for a single simulation dataset
#'
#' @param sim_data Enhanced simulation data from get_simulation_data()
#' @return Tibble with task metrics
#' @export
compute_task_metrics <- function(sim_data) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(tibble())
    }

    # Filter to only use real simulation data (not artificial points)
    analysis_data <- sim_data %>%
        filter(simulating == TRUE)

    if (nrow(analysis_data) == 0) {
        return(tibble())
    }

    # Get parameters to summarize
    params <- get_simulation_parameters()
    param_names <- names(params)

    # Get velocity/acceleration parameters that need absolute values
    abs_params <- c("vx", "vy", "vx_world", "ax", "ay", "ax_world")

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
        )

    # Compute metrics for parameters that need absolute values
    abs_metrics <- analysis_data %>%
        summarise(across(
            all_of(intersect(param_names, abs_params)),
            list(
                max = ~ max(abs(.), na.rm = TRUE),
                sd = ~ sd(., na.rm = TRUE),
                mean = ~ mean(abs(.), na.rm = TRUE)
            )
        ))

    # Compute metrics for regular parameters
    reg_metrics <- analysis_data %>%
        summarise(across(
            all_of(setdiff(param_names, abs_params)),
            list(
                max = ~ max(., na.rm = TRUE),
                sd = ~ sd(., na.rm = TRUE),
                mean = ~ mean(., na.rm = TRUE)
            )
        ))

    # Combine all metrics
    result <- bind_cols(sim_info, abs_metrics, reg_metrics)

    return(result)
}

get_metrics <- function(p, t) {
    sim_data <- get_simulation_data(p, t)
    return(compute_task_metrics(sim_data))
}

#' Get task metrics for all participants and trials
#'
#' @param participants List of participant identifiers
#' @param trials List of trial numbers
#' @param slice_length Optional length of time slices in seconds
#' @param remove_middle_slices Optional flag to remove middle slices
#' @return Tibble with task metrics for all participant/trial combinations
#' @export
get_all_task_metrics <- function() {
    return(get_data_from_loop_parallel(get_metrics, datasets_to_verify = c("sim", "level")))
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

    # Add task. prefix to all task metric columns (except participant and trialNum)
    task_metrics_cols <- setdiff(colnames(mu_task_copy), c("participant", "trialNum"))
    mu_task_renamed <- mu_task_copy %>%
        rename_with(~ paste0("task.", .x), all_of(task_metrics_cols))


    # Merge based on participant and trialNum
    merged_data <- full_join(
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
            filter(slice_index == min(slice_index, na.rm = TRUE) | slice_index == max(slice_index, na.rm = TRUE)) %>%
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
        filter(
            participant %in% selected_participants,
            trialNum %in% selected_trials
        )

    # Process with time slicing
    task_data <- process_task_metrics_sliced(task_data, slice_length, remove_middle_slices)

    return(task_data)
}
