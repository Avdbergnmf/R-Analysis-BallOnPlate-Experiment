# ─────────────────────────────────────────────────────────────────────────────
# Simulation Data Processing
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(tibble)

#' Get processed simulation data for a participant and trial
#'
#' This function loads the raw simulation tracker data and task level data,
#' creates the appropriate CircularProfileShape, and enhances the dataset with:
#' - World coordinates (x, y) using getXY()
#' - Velocity and acceleration using post_process_df()
#' - Energy metrics using add_energy_cols()
#' - Safety metrics using add_safety_cols()
#' - Power metrics using add_power_cols()
#' - Respawn event handling with artificial datapoints and spike elimination
#'
#' @param participant Character string identifying the participant
#' @param trial Integer trial number
#' @return A tibble with enhanced simulation data, or empty tibble if no data available
#' @export
get_simulation_data <- function(participant, trial) {
    tryCatch(
        {
            # Load and validate raw simulation data
            sim_data <- get_t_data(participant, "sim", trial)
            if (is.null(sim_data) || nrow(sim_data) == 0) {
                message(sprintf("No simulation data found for participant %s, trial %s", participant, trial))
                return(tibble())
            }

            # Remove non-simulation steps and validate
            sim_data <- sim_data %>% filter(dt != 0)
            if (nrow(sim_data) == 0) {
                message(sprintf("No simulation steps (dt > 0) found for participant %s, trial %s", participant, trial))
                return(tibble())
            }

            # Step 1: Detect respawn events and mark simulation data
            sim_data <- detect_respawn_events(sim_data)

            # Step 2: Add artificial datapoints at gap boundaries
            sim_data <- add_artificial_gap_points(sim_data)

            # Step 3: Assign arc degrees from task level data
            level_data <- get_t_data(participant, "level", trial)
            if (is.null(level_data) || nrow(level_data) == 0) {
                message(sprintf("No task level data found for participant %s, trial %s. Using default arcDeg=180", participant, trial))
            }
            sim_data <- assign_arc_degrees(sim_data, level_data)

            # Step 4: Calculate world coordinates
            sim_data_enhanced <- calculate_coordinates(sim_data)

            # Step 5: Add simulation time column
            sim_data_enhanced <- add_simulation_time(sim_data_enhanced)

            # Step 6: Apply post-processing for velocities and accelerations
            sim_data_enhanced <- post_process_df(sim_data_enhanced)

            # Step 7: Ensure qdd column exists
            sim_data_enhanced <- ensure_qdd_column(sim_data_enhanced)

            # Step 8: Add energy, power, and safety metrics
            representative_arcDeg <- tail(sim_data_enhanced$arcDeg, 1)
            representative_shape <- CircularProfileShape$new(
                R = 5.0, arcDeg = representative_arcDeg, L = 0,
                mu_d = 0.03, mu_s = 0.05, damping = 0, g = 9.81
            )

            sim_data_enhanced <- add_energy_cols(sim_data_enhanced, g = representative_shape$g)
            sim_data_enhanced <- add_power_cols(sim_data_enhanced)
            sim_data_enhanced <- add_safety_cols(sim_data_enhanced, representative_shape)

            # Step 9: Zero out derived variables at gap boundaries and adjacent samples
            indices_to_zero <- get_indices_to_zero(sim_data_enhanced)
            sim_data_enhanced <- zero_variables_at_indices(sim_data_enhanced, indices_to_zero)

            # Step 10: Add participant and trial identifiers
            sim_data_enhanced <- sim_data_enhanced %>%
                mutate(
                    participant = as.factor(participant),
                    trialNum = as.ordered(trial)
                )

            return(sim_data_enhanced)
        },
        error = function(e) {
            message(sprintf("Error processing simulation data for participant %s, trial %s: %s", participant, trial, e$message))
            return(tibble())
        }
    )
}

#' Get simulation data for multiple participants and trials
#'
#' @param participants Vector of participant identifiers
#' @param trials Vector of trial numbers
#' @param parallel Logical, whether to use parallel processing (default TRUE)
#' @return Combined tibble with simulation data for all participant/trial combinations
#' @export
get_all_simulation_data <- function(participants = NULL, trials = NULL, parallel = TRUE) {
    # Use global participants and allTrials if not specified
    if (is.null(participants)) {
        if (exists("participants")) {
            participants <- participants
        } else {
            stop("No participants specified and global 'participants' variable not found")
        }
    }

    if (is.null(trials)) {
        if (exists("allTrials")) {
            trials <- allTrials
        } else {
            stop("No trials specified and global 'allTrials' variable not found")
        }
    }

    # Create combinations of participants and trials
    combinations <- expand.grid(participant = participants, trial = trials, stringsAsFactors = FALSE)

    if (parallel && requireNamespace("foreach", quietly = TRUE) && requireNamespace("doParallel", quietly = TRUE)) {
        # Parallel processing
        numCores <- parallel::detectCores() - 1
        cl <- parallel::makeCluster(numCores)
        doParallel::registerDoParallel(cl)

        # Export necessary variables and functions to the cluster
        parallel::clusterExport(cl, c(
            "get_simulation_data", "get_t_data", "CircularProfileShape",
            "post_process_df", "add_energy_cols", "add_power_cols", "add_safety_cols",
            "participants", "allTrials"
        ), envir = environment())

        tryCatch({
            result <- foreach::foreach(
                i = 1:nrow(combinations),
                .combine = bind_rows,
                .packages = c("dplyr", "tibble", "R6")
            ) %dopar% {
                # Source required files in each worker
                source("source/setup.R", local = FALSE)
                source("source/data_loading.R", local = FALSE)
                source("source/profile_shapes.R", local = FALSE)
                source("source/simulation_core.R", local = FALSE)
                source("source/get_simulation_data.R", local = FALSE)

                # Get data for this combination
                participant <- combinations$participant[i]
                trial <- combinations$trial[i]

                # Get simulation data and compute metrics
                sim_data <- get_simulation_data(participant, trial)
                if (is.null(sim_data) || nrow(sim_data) == 0) {
                    # Create dummy row with NA values for missing data
                    dummy_metrics <- tibble(
                        participant = as.factor(participant),
                        trialNum = ordered(trial, levels = trials), # Use consistent levels
                        max_q = NA_real_, sd_q = NA_real_, mean_q = NA_real_,
                        max_p = NA_real_, sd_p = NA_real_, mean_p = NA_real_,
                        max_vx = NA_real_, sd_vx = NA_real_, mean_vx = NA_real_,
                        max_vy = NA_real_, sd_vy = NA_real_, mean_vy = NA_real_,
                        max_vx_world = NA_real_, sd_vx_world = NA_real_, mean_vx_world = NA_real_,
                        max_ax = NA_real_, sd_ax = NA_real_, mean_ax = NA_real_,
                        max_ay = NA_real_, sd_ay = NA_real_, mean_ay = NA_real_,
                        max_ax_world = NA_real_, sd_ax_world = NA_real_, mean_ax_world = NA_real_,
                        max_qd = NA_real_, sd_qd = NA_real_, mean_qd = NA_real_,
                        max_e = NA_real_, sd_e = NA_real_, mean_e = NA_real_,
                        max_e_world = NA_real_, sd_e_world = NA_real_, mean_e_world = NA_real_,
                        max_ke = NA_real_, sd_ke = NA_real_, mean_ke = NA_real_,
                        max_ke_world = NA_real_, sd_ke_world = NA_real_, mean_ke_world = NA_real_,
                        max_pe = NA_real_, sd_pe = NA_real_, mean_pe = NA_real_,
                        max_power = NA_real_, sd_power = NA_real_, mean_power = NA_real_,
                        max_power_world = NA_real_, sd_power_world = NA_real_, mean_power_world = NA_real_,
                        max_margin = NA_real_, mean_margin = NA_real_, sd_margin = NA_real_,
                        max_danger = NA_real_, mean_danger = NA_real_, sd_danger = NA_real_,
                        final_work = NA_real_, final_work_world = NA_real_,
                        max_x = NA_real_, sd_x = NA_real_, mean_x = NA_real_,
                        max_x_world = NA_real_, sd_x_world = NA_real_, mean_x_world = NA_real_,
                        max_y = NA_real_, sd_y = NA_real_, mean_y = NA_real_,
                        sim_duration = NA_real_, n_samples = NA_integer_, n_respawns = NA_integer_,
                        mean_arcDeg = NA_real_, n_arcDeg_changes = NA_integer_
                    )
                    return(dummy_metrics)
                }

                metrics <- compute_simulation_metrics(sim_data)
                if (nrow(metrics) == 0) {
                    # Create dummy row with NA values if metrics computation fails
                    dummy_metrics <- tibble(
                        participant = as.factor(participant),
                        trialNum = ordered(trial, levels = trials), # Use consistent levels
                        max_q = NA_real_, sd_q = NA_real_, mean_q = NA_real_,
                        max_p = NA_real_, sd_p = NA_real_, mean_p = NA_real_,
                        max_vx = NA_real_, sd_vx = NA_real_, mean_vx = NA_real_,
                        max_vy = NA_real_, sd_vy = NA_real_, mean_vy = NA_real_,
                        max_vx_world = NA_real_, sd_vx_world = NA_real_, mean_vx_world = NA_real_,
                        max_ax = NA_real_, sd_ax = NA_real_, mean_ax = NA_real_,
                        max_ay = NA_real_, sd_ay = NA_real_, mean_ay = NA_real_,
                        max_ax_world = NA_real_, sd_ax_world = NA_real_, mean_ax_world = NA_real_,
                        max_qd = NA_real_, sd_qd = NA_real_, mean_qd = NA_real_,
                        max_e = NA_real_, sd_e = NA_real_, mean_e = NA_real_,
                        max_e_world = NA_real_, sd_e_world = NA_real_, mean_e_world = NA_real_,
                        max_ke = NA_real_, sd_ke = NA_real_, mean_ke = NA_real_,
                        max_ke_world = NA_real_, sd_ke_world = NA_real_, mean_ke_world = NA_real_,
                        max_pe = NA_real_, sd_pe = NA_real_, mean_pe = NA_real_,
                        max_power = NA_real_, sd_power = NA_real_, mean_power = NA_real_,
                        max_power_world = NA_real_, sd_power_world = NA_real_, mean_power_world = NA_real_,
                        max_margin = NA_real_, mean_margin = NA_real_, sd_margin = NA_real_,
                        max_danger = NA_real_, mean_danger = NA_real_, sd_danger = NA_real_,
                        final_work = NA_real_, final_work_world = NA_real_,
                        max_x = NA_real_, sd_x = NA_real_, mean_x = NA_real_,
                        max_x_world = NA_real_, sd_x_world = NA_real_, mean_x_world = NA_real_,
                        max_y = NA_real_, sd_y = NA_real_, mean_y = NA_real_,
                        sim_duration = NA_real_, n_samples = NA_integer_, n_respawns = NA_integer_,
                        mean_arcDeg = NA_real_, n_arcDeg_changes = NA_integer_
                    )
                    return(dummy_metrics)
                }

                # Add participant and trial identifiers with consistent factor levels
                metrics %>%
                    mutate(
                        participant = as.factor(participant),
                        trialNum = ordered(trial, levels = trials), # Use consistent levels
                        .before = 1
                    )
            }
        }, finally = {
            parallel::stopCluster(cl)
        })
    } else {
        # Sequential processing
        result_list <- vector("list", nrow(combinations))

        for (i in 1:nrow(combinations)) {
            participant <- combinations$participant[i]
            trial <- combinations$trial[i]
            result_list[[i]] <- get_simulation_data(participant, trial)
        }

        result <- bind_rows(result_list)
    }

    return(result)
}

#' Check if simulation data is available for a participant and trial
#'
#' @param participant Character string identifying the participant
#' @param trial Integer trial number
#' @return Logical indicating whether simulation data exists
#' @export
has_simulation_data <- function(participant, trial) {
    sim_data <- get_t_data(participant, "sim", trial)
    return(!is.null(sim_data) && nrow(sim_data) > 0)
}

#' Helper function to detect and mark respawn events
#' @param data Raw simulation data
#' @return Data with respawn detection columns added
detect_respawn_events <- function(data) {
    data %>%
        arrange(time) %>%
        mutate(
            time_diff = c(0, diff(time)),
            is_respawn_gap = time_diff > 1.0,
            respawn_segment = cumsum(is_respawn_gap) + 1,
            is_post_respawn = is_respawn_gap,
            simulating = TRUE
        )
}

#' Helper function to create artificial datapoints at gap boundaries
#' @param data Data with respawn events detected
#' @return Data with artificial datapoints added
add_artificial_gap_points <- function(data) {
    if (!any(data$is_respawn_gap)) {
        return(data)
    }

    gap_indices <- which(data$is_respawn_gap)
    artificial_rows <- list()

    for (gap_idx in gap_indices) {
        time_before_gap <- data$time[gap_idx - 1]
        time_after_gap <- data$time[gap_idx]

        # Artificial point at end of previous segment
        end_point <- data[gap_idx - 1, ] %>%
            mutate(
                time = time_before_gap + 0.001,
                p = 0, q = 0, qd = 0,
                dt = 0.001,
                time_diff = 0.001,
                is_respawn_gap = FALSE,
                is_post_respawn = FALSE,
                simulating = FALSE
            )

        # Artificial point at start of next segment
        start_point <- data[gap_idx, ] %>%
            mutate(
                time = time_after_gap - 0.001,
                p = 0, q = 0, qd = 0,
                dt = 0.001,
                time_diff = time_after_gap - time_before_gap - 0.002,
                is_respawn_gap = FALSE,
                is_post_respawn = FALSE,
                simulating = FALSE
            )

        artificial_rows <- c(artificial_rows, list(end_point, start_point))
    }

    # Combine and re-sort data
    bind_rows(data, artificial_rows) %>%
        arrange(time) %>%
        mutate(
            time_diff = c(0, diff(time)),
            is_respawn_gap = time_diff > 1.0,
            respawn_segment = cumsum(is_respawn_gap) + 1,
            is_post_respawn = is_respawn_gap
        )
}

#' Helper function to assign arcDeg values based on task level data
#' @param sim_data Simulation data
#' @param level_data Task level data
#' @return Simulation data with arcDeg column
assign_arc_degrees <- function(sim_data, level_data) {
    if (is.null(level_data) || nrow(level_data) == 0) {
        sim_data$arcDeg <- 180
        return(sim_data)
    }

    level_data <- level_data[order(level_data$time), ]
    level_indices <- findInterval(sim_data$time, level_data$time, rightmost.closed = TRUE)
    level_indices[level_indices == 0] <- 1

    sim_data$arcDeg <- level_data$deg[level_indices]
    sim_data$arcDeg[is.na(sim_data$arcDeg)] <- 180

    return(sim_data)
}

#' Helper function to calculate world coordinates
#' @param data Simulation data with arcDeg
#' @return Data with local and world coordinates added
calculate_coordinates <- function(data) {
    unique_arcDegs <- unique(data$arcDeg)
    data$x <- NA
    data$y <- NA
    data$q_max <- NA # Add q_max column

    # Calculate coordinates for real simulation data
    for (arcDeg in unique_arcDegs) {
        shape <- CircularProfileShape$new(
            R = 5.0, arcDeg = arcDeg, L = 0,
            mu_d = 0.03, mu_s = 0.05, damping = 0, g = 9.81
        )

        rows_with_arcDeg <- which(data$arcDeg == arcDeg & data$simulating)

        if (length(rows_with_arcDeg) > 0) {
            q_values <- data$q[rows_with_arcDeg]
            local_coords_matrix <- t(sapply(q_values, function(q_val) shape$getXY(q_val)))
            data$x[rows_with_arcDeg] <- local_coords_matrix[, 1]
            data$y[rows_with_arcDeg] <- local_coords_matrix[, 2]

            # Store q_max for this arcDeg
            data$q_max[rows_with_arcDeg] <- shape$getMaxQ()
        }
    }

    # Set artificial points to origin
    artificial_indices <- which(!data$simulating)
    if (length(artificial_indices) > 0) {
        data$x[artificial_indices] <- 0
        data$y[artificial_indices] <- 0
        data$q_max[artificial_indices] <- 0 # 180 * pi / 180 * 6.0 / 2 # Default q_max for artificial points
    }

    # Add world coordinates in same step
    data %>%
        mutate(
            x_world = p + x # World x position
            # y is the same in both frames (plate doesn't move vertically)
        )
}

#' Helper function to add simulation time column
#' @param data Enhanced simulation data
#' @return Data with simulation_time column
add_simulation_time <- function(data) {
    data %>%
        mutate(
            cumulative_gap_time = cumsum(ifelse(is_respawn_gap, time_diff, 0)),
            simulation_time = cumsum(dt) + cumulative_gap_time
        )
}

#' Helper function to compute missing qdd if needed
#' @param data Data after post-processing
#' @return Data with qdd column ensured
ensure_qdd_column <- function(data) {
    if (!"qdd" %in% colnames(data) && "qd" %in% colnames(data)) {
        data %>%
            arrange(time) %>%
            mutate(qdd = c(0, diff(qd) / diff(time)))
    } else {
        data
    }
}

#' Helper function to zero out all variable types at specified indices
#' @param data Enhanced simulation data
#' @param indices_to_zero Vector of row indices to zero out
#' @return Data with variables zeroed at specified indices
zero_variables_at_indices <- function(data, indices_to_zero) {
    if (length(indices_to_zero) == 0) {
        return(data)
    }

    # Define variable groups
    variable_groups <- list(
        kinematic_local = c("vx", "vy", "ax", "ay", "qd", "qdd"),
        kinematic_world = c("vx_world", "ax_world"),
        energy_local = c("ke", "e"),
        energy_world = c("ke_world", "e_world"),
        power_local = c("power", "work"),
        power_world = c("power_world", "work_world"),
        safety = c("margin_E", "danger")
    )

    # Zero out each group
    for (group_vars in variable_groups) {
        for (var in group_vars) {
            if (var %in% colnames(data)) {
                data[[var]][indices_to_zero] <- 0
            }
        }
    }

    return(data)
}

#' Helper function to identify indices that need variable zeroing
#' @param data Enhanced simulation data
#' @return Vector of indices to zero out
get_indices_to_zero <- function(data) {
    if (!"simulating" %in% colnames(data)) {
        return(integer(0))
    }

    artificial_indices <- which(!data$simulating)
    adjacent_indices <- c()

    # Find real samples adjacent to artificial points (they create jumps to/from zero)
    for (idx in artificial_indices) {
        # Sample before artificial point
        if (idx > 1 && data$simulating[idx - 1]) {
            adjacent_indices <- c(adjacent_indices, idx - 1)
        }
        # Sample after artificial point
        if (idx < nrow(data) && data$simulating[idx + 1]) {
            adjacent_indices <- c(adjacent_indices, idx + 1)
        }
    }

    unique(c(artificial_indices, adjacent_indices))
}

#' Compute summary metrics from simulation data
#'
#' @param sim_data Enhanced simulation data from get_simulation_data()
#' @return Tibble with summary metrics
#' @export
compute_simulation_metrics <- function(sim_data) {
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(tibble())
    }

    # Filter to only use real simulation data (not artificial points)
    analysis_data <- sim_data %>%
        filter(simulating == TRUE) # Only analyze real simulation data, not artificial points

    if (nrow(analysis_data) == 0) {
        return(tibble())
    }

    # Compute summary metrics (following the sweep code pattern)
    metrics <- analysis_data %>%
        summarise(
            # Position metrics
            max_q = max(q, na.rm = TRUE),
            sd_q = sd(q, na.rm = TRUE),
            mean_q = mean(q, na.rm = TRUE),

            # Plate position metrics
            max_p = max(p, na.rm = TRUE),
            sd_p = sd(p, na.rm = TRUE),
            mean_p = mean(p, na.rm = TRUE),

            # Velocity metrics (local - more relevant for escape)
            max_vx = max(abs(vx), na.rm = TRUE),
            sd_vx = sd(vx, na.rm = TRUE),
            mean_vx = mean(abs(vx), na.rm = TRUE),
            max_vy = max(abs(vy), na.rm = TRUE),
            sd_vy = sd(vy, na.rm = TRUE),
            mean_vy = mean(abs(vy), na.rm = TRUE),

            # World velocity metrics
            max_vx_world = max(abs(vx_world), na.rm = TRUE),
            sd_vx_world = sd(vx_world, na.rm = TRUE),
            mean_vx_world = mean(abs(vx_world), na.rm = TRUE),

            # Acceleration metrics
            max_ax = max(abs(ax), na.rm = TRUE),
            sd_ax = sd(ax, na.rm = TRUE),
            mean_ax = mean(abs(ax), na.rm = TRUE),
            max_ay = max(abs(ay), na.rm = TRUE),
            sd_ay = sd(ay, na.rm = TRUE),
            mean_ay = mean(abs(ay), na.rm = TRUE),

            # World acceleration metrics
            max_ax_world = max(abs(ax_world), na.rm = TRUE),
            sd_ax_world = sd(ax_world, na.rm = TRUE),
            mean_ax_world = mean(abs(ax_world), na.rm = TRUE),

            # Arc-length velocity metrics
            max_qd = max(abs(qd), na.rm = TRUE),
            sd_qd = sd(qd, na.rm = TRUE),
            mean_qd = mean(abs(qd), na.rm = TRUE),

            # Energy metrics
            max_e = max(e, na.rm = TRUE),
            sd_e = sd(e, na.rm = TRUE),
            mean_e = mean(e, na.rm = TRUE),

            # World energy metrics
            max_e_world = max(e_world, na.rm = TRUE),
            sd_e_world = sd(e_world, na.rm = TRUE),
            mean_e_world = mean(e_world, na.rm = TRUE),

            # Kinetic energy metrics (local)
            max_ke = max(ke, na.rm = TRUE),
            sd_ke = sd(ke, na.rm = TRUE),
            mean_ke = mean(ke, na.rm = TRUE),

            # World kinetic energy metrics
            max_ke_world = max(ke_world, na.rm = TRUE),
            sd_ke_world = sd(ke_world, na.rm = TRUE),
            mean_ke_world = mean(ke_world, na.rm = TRUE),

            # Potential energy metrics
            max_pe = max(pe, na.rm = TRUE),
            sd_pe = sd(pe, na.rm = TRUE),
            mean_pe = mean(pe, na.rm = TRUE),

            # Power metrics (local - if available)
            max_power = if ("power" %in% colnames(analysis_data)) max(power, na.rm = TRUE) else NA_real_,
            sd_power = if ("power" %in% colnames(analysis_data)) sd(power, na.rm = TRUE) else NA_real_,
            mean_power = if ("power" %in% colnames(analysis_data)) mean(power, na.rm = TRUE) else NA_real_,

            # World power metrics (if available)
            max_power_world = if ("power_world" %in% colnames(analysis_data)) max(power_world, na.rm = TRUE) else NA_real_,
            sd_power_world = if ("power_world" %in% colnames(analysis_data)) sd(power_world, na.rm = TRUE) else NA_real_,
            mean_power_world = if ("power_world" %in% colnames(analysis_data)) mean(power_world, na.rm = TRUE) else NA_real_,

            # Safety margin metrics
            max_margin = max(margin_E, na.rm = TRUE),
            mean_margin = mean(margin_E, na.rm = TRUE),
            sd_margin = sd(margin_E, na.rm = TRUE),

            # Danger metrics
            max_danger = max(danger, na.rm = TRUE),
            mean_danger = mean(danger, na.rm = TRUE),
            sd_danger = sd(danger, na.rm = TRUE),

            # Work metrics (local - if available)
            final_work = if ("work" %in% colnames(analysis_data)) last(work, default = NA_real_) else NA_real_,

            # World work metrics (if available)
            final_work_world = if ("work_world" %in% colnames(analysis_data)) last(work_world, default = NA_real_) else NA_real_,

            # Local coordinate metrics (plate-relative)
            max_x = max(abs(x), na.rm = TRUE),
            sd_x = sd(x, na.rm = TRUE),
            mean_x = mean(x, na.rm = TRUE),

            # World coordinate metrics
            max_x_world = max(x_world, na.rm = TRUE),
            sd_x_world = sd(x_world, na.rm = TRUE),
            mean_x_world = mean(x_world, na.rm = TRUE),
            max_y = max(y, na.rm = TRUE),
            sd_y = sd(y, na.rm = TRUE),
            mean_y = mean(y, na.rm = TRUE),

            # Simulation duration and data quality
            sim_duration = max(simulation_time, na.rm = TRUE) - min(simulation_time, na.rm = TRUE),
            n_samples = n(),
            n_respawns = length(unique(respawn_segment)) - 1,

            # Arc degree info
            mean_arcDeg = mean(arcDeg, na.rm = TRUE),
            n_arcDeg_changes = length(unique(arcDeg))
        )

    return(metrics)
}

#' Get simulation task metrics for multiple participants and trials
#'
#' @param participants Vector of participant identifiers (default: global participants)
#' @param trials Vector of trial numbers (default: global allTrials)
#' @param parallel Whether to use parallel processing (default TRUE)
#' @return Tibble with task metrics for all participant/trial combinations
#' @export
get_mu_task <- function(participants = NULL, trials = NULL, parallel = TRUE) {
    # Use global participants and allTrials if not specified
    if (is.null(participants)) {
        if (exists("participants", envir = .GlobalEnv)) {
            participants <- get("participants", envir = .GlobalEnv)
        } else {
            stop("No participants specified and global 'participants' variable not found")
        }
    }

    if (is.null(trials)) {
        if (exists("allTrials", envir = .GlobalEnv)) {
            trials <- get("allTrials", envir = .GlobalEnv)
        } else {
            stop("No trials specified and global 'allTrials' variable not found")
        }
    }

    # Create combinations of participants and trials
    combinations <- expand.grid(participant = participants, trial = trials, stringsAsFactors = FALSE)

    if (parallel && requireNamespace("foreach", quietly = TRUE) && requireNamespace("doParallel", quietly = TRUE)) {
        # Parallel processing
        numCores <- parallel::detectCores() - 1
        cl <- parallel::makeCluster(numCores)
        doParallel::registerDoParallel(cl)

        # Export necessary variables and functions to the cluster
        parallel::clusterExport(cl, c(
            "get_simulation_data", "compute_simulation_metrics",
            "participants", "allTrials"
        ), envir = environment())

        tryCatch({
            result <- foreach::foreach(
                i = 1:nrow(combinations),
                .combine = bind_rows,
                .packages = c("dplyr", "tibble", "R6")
            ) %dopar% {
                # Source required files in each worker
                source("source/setup.R", local = FALSE)
                source("source/data_loading.R", local = FALSE)
                source("source/profile_shapes.R", local = FALSE)
                source("source/simulation_core.R", local = FALSE)
                source("source/get_simulation_data.R", local = FALSE)

                # Get data for this combination
                participant <- combinations$participant[i]
                trial <- combinations$trial[i]

                # Get simulation data and compute metrics
                sim_data <- get_simulation_data(participant, trial)
                if (is.null(sim_data) || nrow(sim_data) == 0) {
                    # Create dummy row with consistent structure
                    return(create_dummy_task_metrics_with_levels(participant, trial, allTrials))
                }

                metrics <- compute_simulation_metrics(sim_data)
                if (nrow(metrics) == 0) {
                    # Create dummy row with consistent structure
                    return(create_dummy_task_metrics_with_levels(participant, trial, allTrials))
                }

                # Add participant and trial identifiers with consistent factor levels
                metrics %>%
                    mutate(
                        participant = as.factor(participant),
                        trialNum = ordered(trial, levels = allTrials), # Use consistent levels
                        .before = 1
                    )
            }
        }, finally = {
            parallel::stopCluster(cl)
        })
    } else {
        # Sequential processing
        result_list <- vector("list", nrow(combinations))

        for (i in 1:nrow(combinations)) {
            participant <- combinations$participant[i]
            trial <- combinations$trial[i]

            # Get simulation data and compute metrics
            sim_data <- get_simulation_data(participant, trial)
            if (is.null(sim_data) || nrow(sim_data) == 0) {
                # Create dummy row with consistent structure
                result_list[[i]] <- create_dummy_task_metrics_with_levels(participant, trial, trials)
                next
            }

            metrics <- compute_simulation_metrics(sim_data)
            if (nrow(metrics) == 0) {
                # Create dummy row with consistent structure
                result_list[[i]] <- create_dummy_task_metrics_with_levels(participant, trial, trials)
                next
            }

            # Add participant and trial identifiers with consistent factor levels
            result_list[[i]] <- metrics %>%
                mutate(
                    participant = as.factor(participant),
                    trialNum = ordered(trial, levels = trials), # Use consistent levels
                    .before = 1
                )
        }

        result <- bind_rows(result_list)
    }

    # Filter out empty results - no need for factor level conversion anymore
    result <- result %>%
        filter(!is.na(participant))

    return(result)
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

    # Add task_ prefix to all task metric columns (except participant and trialNum)
    task_metrics_cols <- setdiff(colnames(mu_task), c("participant", "trialNum"))
    mu_task_renamed <- mu_task %>%
        rename_with(~ paste0("task_", .x), all_of(task_metrics_cols))

    # Merge based on participant and trialNum
    # Use full_join to keep all data from both sources
    merged_data <- full_join(
        mu_gait,
        mu_task_renamed,
        by = c("participant", "trialNum")
    )

    return(merged_data)
}

# ─────────────────────────────────────────────────────────────────────────────
# Simulation Data Helper Functions
# ─────────────────────────────────────────────────────────────────────────────

#' Get simulation variable choices with pretty names
#'
#' @param data Simulation data
#' @return Named vector of available variables
#' @export
get_simulation_variable_choices <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return(NULL)
    }

    # Get pretty names mapping
    pretty_names <- get_simulation_variable_names()

    # Create named vector: names are pretty names, values are actual column names
    # This is the format expected by selectizeInput choices
    available_choices <- list()
    for (col_name in names(pretty_names)) { # names(pretty_names) are the column names
        pretty_name <- pretty_names[col_name] # get the pretty name for this column
        if (col_name %in% colnames(data)) {
            available_choices[[pretty_name]] <- col_name # pretty name as key, column name as value
        }
    }

    return(available_choices)
}

#' Get mapping of variable names to pretty display names
#'
#' @return Named vector mapping actual column names to pretty names
#' @export
get_simulation_variable_names <- function() {
    c(
        `time` = "Unity Time",
        `simulation_time` = "Simulation Time",
        `p` = "Plate Position",
        `q` = "Arc-length Position",
        `qd` = "Arc-length Velocity",
        `qdd` = "Arc-length Acceleration",
        `q_max` = "Max Arc-length (Escape)",
        `x` = "X Position (plate-relative)",
        `x_world` = "X Position (world)",
        `y` = "Y Position (height)",
        `y_escape` = "Escape Height",
        `dist_to_escape` = "Distance to Escape",
        `vx` = "X Velocity (plate-relative)",
        `vx_world` = "X Velocity (world)",
        `vy` = "Y Velocity",
        `ax` = "X Acceleration (plate-relative)",
        `ax_world` = "X Acceleration (world)",
        `ay` = "Y Acceleration",
        `ke` = "Kinetic Energy (plate-relative)",
        `ke_world` = "Kinetic Energy (world)",
        `pe` = "Potential Energy",
        `e` = "Total Energy (plate-relative)",
        `e_world` = "Total Energy (world)",
        `e_potential_needed` = "Potential Energy Needed",
        `e_loss_friction` = "Energy Loss (Friction)",
        `e_loss_damping` = "Energy Loss (Damping)",
        `e_total_needed` = "Total Energy Needed to Escape",
        `power` = "Power (plate-relative)",
        `power_world` = "Power (world)",
        `work` = "Work (plate-relative)",
        `work_world` = "Work (world)",
        `margin_E` = "Energy Margin",
        `danger` = "Danger Level",
        `arcDeg` = "Arc Degrees",
        `dt` = "Time Step",
        `time_diff` = "Time Gap",
        `is_respawn_gap` = "Is Respawn Event",
        `respawn_segment` = "Respawn Segment",
        `is_post_respawn` = "Post-Respawn Flag",
        `cumulative_gap_time` = "Cumulative Gap Time",
        `simulating` = "Real/Artificial Data"
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# Simulation Data Caching
# ─────────────────────────────────────────────────────────────────────────────

#' Get all enhanced simulation data with caching
#'
#' @param participants Vector of participant identifiers (default: global participants)
#' @param trials Vector of trial numbers (default: global allTrials)
#' @param parallel Whether to use parallel processing (default TRUE)
#' @param force_regenerate Whether to force regeneration even if cached data exists
#' @return Combined tibble with enhanced simulation data for all participant/trial combinations
#' @export
get_all_simulation_data_cached <- function(participants = NULL, trials = NULL, parallel = TRUE, force_regenerate = FALSE) {
    # Use global participants and allTrials if not specified
    if (is.null(participants)) {
        if (exists("participants", envir = .GlobalEnv)) {
            participants <- get("participants", envir = .GlobalEnv)
        } else {
            stop("No participants specified and global 'participants' variable not found")
        }
    }

    if (is.null(trials)) {
        if (exists("allTrials", envir = .GlobalEnv)) {
            trials <- get("allTrials", envir = .GlobalEnv)
        } else {
            stop("No trials specified and global 'allTrials' variable not found")
        }
    }

    # Create results directory if it doesn't exist
    resultsFolder <- "results"
    if (!dir.exists(resultsFolder)) {
        dir.create(resultsFolder, recursive = TRUE)
    }

    # File path for cached simulation data
    simDataFile <- file.path(resultsFolder, "allSimulationData.rds")

    # Load or calculate the data
    if (force_regenerate || !file.exists(simDataFile)) {
        message("Generating enhanced simulation data for all participants and trials...")
        allSimData <- get_all_simulation_data(participants, trials, parallel)

        # Save the data
        message(sprintf("Saving enhanced simulation data to %s", simDataFile))
        saveRDS(allSimData, simDataFile)
    } else {
        message(sprintf("Loading cached simulation data from %s", simDataFile))
        allSimData <- readRDS(simDataFile)
    }

    return(allSimData)
}

#' Calculate task metrics from cached simulation data with time slicing support
#'
#' @param allSimData Enhanced simulation data (from get_all_simulation_data_cached)
#' @param participants Vector of participants to include (default: all in data)
#' @param trials Vector of trials to include (default: all in data)
#' @param slice_length Length of time slices in seconds (default: NULL for no slicing)
#' @param time_col Column to use for time slicing (default: "simulation_time")
#' @return Tibble with task metrics for all participant/trial combinations
#' @export
compute_all_task_metrics <- function(allSimData, participants = NULL, trials = NULL, slice_length = NULL, time_col = "simulation_time") {
    if (is.null(allSimData) || nrow(allSimData) == 0) {
        return(tibble())
    }

    # Get all possible participants and trials from global variables if not specified
    if (is.null(participants)) {
        if (exists("participants", envir = .GlobalEnv)) {
            participants <- get("participants", envir = .GlobalEnv)
        } else {
            participants <- unique(allSimData$participant)
        }
    }

    if (is.null(trials)) {
        if (exists("allTrials", envir = .GlobalEnv)) {
            trials <- get("allTrials", envir = .GlobalEnv)
        } else {
            trials <- unique(allSimData$trialNum)
        }
    }

    # Create all combinations of participants and trials
    all_combinations <- expand.grid(
        participant = participants,
        trial = trials,
        stringsAsFactors = FALSE
    )

    # Add slice index if slicing is requested
    if (!is.null(slice_length)) {
        # Apply time slicing to the simulation data
        allSimData <- allSimData %>%
            group_by(participant, trialNum) %>%
            mutate(
                slice_index = floor((.data[[time_col]] - min(.data[[time_col]], na.rm = TRUE)) / slice_length) + 1
            ) %>%
            ungroup()

        # Get unique slice indices for each trial
        slice_info <- allSimData %>%
            filter(participant %in% participants, trialNum %in% trials) %>%
            group_by(participant, trialNum) %>%
            summarise(max_slice = max(slice_index, na.rm = TRUE), .groups = "drop") %>%
            filter(!is.na(max_slice))

        # Create combinations with slices
        all_combinations <- slice_info %>%
            rowwise() %>%
            do({
                participant_val <- .$participant
                trial_val <- .$trialNum
                max_slice_val <- .$max_slice
                data.frame(
                    participant = participant_val,
                    trial = trial_val,
                    slice_index = 1:max_slice_val
                )
            }) %>%
            ungroup()

        # Add missing combinations (trials without data) with slice_index = 1
        missing_combinations <- anti_join(
            expand.grid(participant = participants, trial = trials, stringsAsFactors = FALSE),
            slice_info,
            by = c("participant", "trial" = "trialNum")
        ) %>%
            mutate(slice_index = 1)

        all_combinations <- bind_rows(all_combinations, missing_combinations)
    }

    # Compute metrics for each combination
    result_list <- vector("list", nrow(all_combinations))

    for (i in 1:nrow(all_combinations)) {
        participant <- all_combinations$participant[i]
        trial <- all_combinations$trial[i]
        slice_idx <- if (!is.null(slice_length)) all_combinations$slice_index[i] else NULL

        # Filter data for this participant/trial/slice
        if (!is.null(slice_length)) {
            sim_data <- allSimData %>%
                filter(
                    participant == !!participant,
                    trialNum == !!trial,
                    slice_index == !!slice_idx
                )
        } else {
            sim_data <- allSimData %>%
                filter(participant == !!participant, trialNum == !!trial)
        }

        if (nrow(sim_data) == 0) {
            # Create dummy metrics for missing data
            dummy_metrics <- create_dummy_task_metrics(participant, trial)
            if (!is.null(slice_length)) {
                dummy_metrics$slice_index <- slice_idx
            }
            result_list[[i]] <- dummy_metrics
        } else {
            # Compute metrics
            metrics <- compute_simulation_metrics(sim_data)
            if (nrow(metrics) == 0) {
                dummy_metrics <- create_dummy_task_metrics(participant, trial)
                if (!is.null(slice_length)) {
                    dummy_metrics$slice_index <- slice_idx
                }
                result_list[[i]] <- dummy_metrics
            } else {
                # Add identifiers
                result_list[[i]] <- metrics %>%
                    mutate(
                        participant = participant,
                        trialNum = trial,
                        .before = 1
                    )

                # Add slice index if slicing is used
                if (!is.null(slice_length)) {
                    result_list[[i]]$slice_index <- slice_idx
                }
            }
        }
    }

    # Combine all results
    result <- bind_rows(result_list)
    return(result)
}

#' Optimized get_mu_task using cached simulation data with time slicing support
#'
#' @param allSimData Enhanced simulation data (from get_all_simulation_data_cached), if NULL will load from cache
#' @param participants Vector of participants to include (default: from filtered data)
#' @param trials Vector of trials to include (default: from filtered data)
#' @param slice_length Length of time slices in seconds (default: NULL for no slicing)
#' @param time_col Column to use for time slicing (default: "simulation_time")
#' @return Tibble with task metrics for all participant/trial combinations
#' @export
get_mu_task_cached <- function(allSimData = NULL, participants = NULL, trials = NULL, slice_length = NULL, time_col = "simulation_time") {
    if (is.null(allSimData)) {
        # Load from cache if not provided
        allSimData <- get_all_simulation_data_cached()
    }

    return(compute_all_task_metrics(allSimData, participants, trials, slice_length, time_col))
}

#' Regenerate cached simulation data
#'
#' @param participants Vector of participant identifiers (default: global participants)
#' @param trials Vector of trial numbers (default: global allTrials)
#' @param parallel Whether to use parallel processing (default TRUE)
#' @return Combined tibble with enhanced simulation data for all participant/trial combinations
#' @export
regenerate_simulation_cache <- function(participants = NULL, trials = NULL, parallel = TRUE) {
    message("Forcing regeneration of simulation data cache...")
    return(get_all_simulation_data_cached(participants, trials, parallel, force_regenerate = TRUE))
}

#' Check if simulation cache file exists
#'
#' @return Logical indicating whether the cache file exists
#' @export
simulation_cache_exists <- function() {
    resultsFolder <- "results"
    simDataFile <- file.path(resultsFolder, "allSimulationData.rds")
    return(file.exists(simDataFile))
}

#' Get information about the simulation cache
#'
#' @return List with cache information (exists, file size, modification time)
#' @export
simulation_cache_info <- function() {
    resultsFolder <- "results"
    simDataFile <- file.path(resultsFolder, "allSimulationData.rds")

    if (file.exists(simDataFile)) {
        file_info <- file.info(simDataFile)
        return(list(
            exists = TRUE,
            file_path = simDataFile,
            size_mb = round(file_info$size / 1024^2, 2),
            modified = file_info$mtime
        ))
    } else {
        return(list(
            exists = FALSE,
            file_path = simDataFile,
            size_mb = NA,
            modified = NA
        ))
    }
}

#' Get template structure for task metrics
#'
#' @return Tibble with the correct structure and column types for task metrics
#' @export
get_task_metrics_template <- function() {
    # Create a minimal dummy simulation data to get the structure
    dummy_sim_data <- tibble(
        time = c(0, 1),
        simulation_time = c(0, 1),
        p = c(0, 0),
        q = c(0, 0),
        qd = c(0, 0),
        qdd = c(0, 0),
        q_max = c(1, 1),
        x = c(0, 0),
        y = c(0, 0),
        y_escape = c(1, 1),
        dist_to_escape = c(1, 1),
        vx = c(0, 0),
        vy = c(0, 0),
        ax = c(0, 0),
        ay = c(0, 0),
        x_world = c(0, 0),
        vx_world = c(0, 0),
        ax_world = c(0, 0),
        ke = c(0, 0),
        pe = c(0, 0),
        e = c(0, 0),
        ke_world = c(0, 0),
        e_world = c(0, 0),
        e_potential_needed = c(0, 0),
        e_loss_friction = c(0, 0),
        e_loss_damping = c(0, 0),
        e_total_needed = c(0, 0),
        power = c(0, 0),
        work = c(0, 0),
        power_world = c(0, 0),
        work_world = c(0, 0),
        margin_E = c(0, 0),
        danger = c(0, 0),
        arcDeg = c(180, 180),
        dt = c(1, 1),
        time_diff = c(0, 1),
        is_respawn_gap = c(FALSE, FALSE),
        respawn_segment = c(1, 1),
        is_post_respawn = c(FALSE, FALSE),
        cumulative_gap_time = c(0, 0),
        simulating = c(TRUE, TRUE),
        participant = factor(c("TEMPLATE", "TEMPLATE")),
        trialNum = ordered(c(1, 1))
    )

    # Compute metrics to get the correct structure
    template_metrics <- compute_simulation_metrics(dummy_sim_data)

    return(template_metrics)
}

#' Create dummy task metrics using template
#'
#' @param participant Participant identifier
#' @param trial Trial number
#' @param template Template metrics structure (if NULL, will generate)
#' @return Tibble with dummy metrics (all NA values)
#' @export
create_dummy_task_metrics <- function(participant, trial, template = NULL) {
    if (is.null(template)) {
        template <- get_task_metrics_template()
    }

    # Create dummy row with same structure but NA values
    dummy_metrics <- template[1, ] # Take first row as template

    # Set all numeric/integer columns to NA
    for (col in names(dummy_metrics)) {
        if (is.numeric(dummy_metrics[[col]]) || is.integer(dummy_metrics[[col]])) {
            dummy_metrics[[col]] <- NA_real_
        }
    }

    # Set the identifiers
    dummy_metrics$participant <- as.factor(participant)
    dummy_metrics$trialNum <- trial

    return(dummy_metrics)
}

#' Create dummy task metrics for consistent factor levels
#'
#' @param participant Participant identifier
#' @param trial Trial number
#' @param trials Vector of all trial levels for consistent factor creation
#' @param template Template metrics structure (if NULL, will generate)
#' @return Tibble with dummy metrics with consistent factor levels
#' @export
create_dummy_task_metrics_with_levels <- function(participant, trial, trials, template = NULL) {
    dummy_metrics <- create_dummy_task_metrics(participant, trial, template)

    # Ensure consistent factor levels for trialNum
    dummy_metrics$trialNum <- ordered(trial, levels = trials)

    return(dummy_metrics)
}
