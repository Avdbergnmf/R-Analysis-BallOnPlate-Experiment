# ─────────────────────────────────────────────────────────────────────────────
# Simulation Data Processing
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(tibble)

# Source the simulation core functions
# source("runtime/source/simulation_core.R")

# Source the risk model functions
source("source/risk_model.R")

#' Get list of base parameters to summarize
#'
#' @return List of parameter names to summarize
#' @export
get_simulation_parameters <- function() {
    list(
        # Position parameters
        q = "Arc-length Position",
        p = "Plate Position",
        pInput = "Input Plate Position",
        pVel = "Plate Velocity",
        pVelAbsolute = "Plate Velocity Absolute",
        pAcc = "Plate Acceleration",
        pAccAbsolute = "Plate Acceleration Absolute",

        # Velocity parameters
        vx = "X Velocity (plate-relative)",
        vy = "Y Velocity",
        vx_world = "X Velocity (world)",

        # Acceleration parameters
        ax = "X Acceleration (plate-relative)",
        ay = "Y Acceleration",
        ax_world = "X Acceleration (world)",

        # Arc-length velocity
        qd = "Arc-length Velocity",

        # Energy parameters
        e = "Total Energy (plate-relative)",
        e_world = "Total Energy (world)",
        ke = "Kinetic Energy (plate-relative)",
        ke_world = "Kinetic Energy (world)",
        pe = "Potential Energy",

        # Power parameters
        power = "Power (plate-relative)",
        power_world = "Power (world)",
        power_plate = "Plate Power",

        # Work parameters
        work = "Work (plate-relative)",
        work_world = "Work (world)",
        work_plate = "Plate Work",

        # Safety parameters
        margin_E = "Energy Margin",
        danger = "Danger Level",
        dist_to_escape = "Distance to Escape",
        dist_to_escape_ratio = "Distance to Escape Ratio",
        dist_to_escape_ratio_mean_corrected = "Distance to Escape Ratio (Corrected)",
        dist_to_escape_arcdeg6 = "Distance to Escape (arcDeg=6)",
        e_total_needed = "Total Energy Needed to Escape",
        time_to_escape = "Time to Escape",

        # Coordinate parameters
        x = "X Position (plate-relative)",
        x_world = "X Position (world)",
        y = "Y Position (height)",

        # Performance parameters
        score = "Running Score",
        total_score = "Total Score",

        # Status parameters
        simulating = "Ball on Plate Status",

        # Risk prediction parameters
        drop_risk_bin = "Drop Risk (Per-Bin/Tau)",
        drop_lambda = "Drop Rate (Per-Second/Lambda)",
        drop_risk_1s = "Drop Risk (1-Second)",
        velocity_towards_edge = "Velocity Towards Edge",
        approach_pressure = "Approach Pressure",
        retreat_pressure = "Retreat Pressure",
        retreat_share = "Retreat Share",
        retreat_speed_cond = "Retreat Speed (Conditional)",
        approach_speed_cond = "Approach Speed (Conditional)",
        absqd = "Absolute Arc-length Velocity",
        v_e = "Escape Distance Velocity",
        edge_pressure = "Edge Pressure",
        edge_pressure_clamped = "Edge Pressure (Clamped)",
        log_v_to_edge = "Log1p(Velocity Towards Edge)",
        post_log_v_to_edge = "Log1p(Velocity Towards Edge Mean)",
        post_log_edge_pressure = "Log1p(Edge Pressure Mean)"
    )
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
        `pInput` = "Input Plate Position",
        `pVel` = "Plate Velocity",
        `pVelAbsolute` = "Plate Velocity Absolute",
        `pAcc` = "Plate Acceleration",
        `pAccAbsolute` = "Plate Acceleration Absolute",
        `time_in_bowl` = "Time in Bowl",
        `ball_in_bowl` = "Ball In Bowl Status",
        `q` = "Arc-length Position",
        `qd` = "Arc-length Velocity",
        `qdd` = "Arc-length Acceleration",
        `q_max` = "Max Arc-length (Escape)",
        `x` = "X Position (plate-relative)",
        `x_world` = "X Position (world)",
        `y` = "Y Position (height)",
        `y_escape` = "Escape Height",
        `dist_to_escape` = "Distance to Escape",
        `dist_to_escape_ratio` = "Distance to Escape Ratio",
        `dist_to_escape_ratio_mean_corrected` = "Distance to Escape Ratio (Corrected)",
        `dist_to_escape_arcdeg6` = "Distance to Escape (arcDeg=6)",
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
        `power_plate` = "Plate Power",
        `work` = "Work (plate-relative)",
        `work_world` = "Work (world)",
        `work_plate` = "Plate Work",
        `margin_E` = "Energy Margin",
        `danger` = "Danger Level",
        `time_to_escape` = "Time to Escape",
        `score` = "Score",
        `total_score` = "Total Score",
        `arcDeg` = "Arc Degrees",
        `dt` = "Time Step",
        `time_diff` = "Time Gap",
        `is_respawn_gap` = "Is Respawn Event",
        `respawn_segment` = "Respawn Segment",
        `is_last_sample_of_segment` = "Is Last Sample of Segment",
        `simulating` = "Ball on Plate Status",
        `drop_risk_bin` = "Drop Risk (Per-Bin/Tau)",
        `drop_lambda` = "Drop Rate (Per-Second/Lambda)",
        `drop_risk_1s` = "Drop Risk (1-Second)",
        `velocity_towards_edge` = "Velocity Towards Edge",
        `approach_pressure` = "Approach Pressure",
        `retreat_pressure` = "Retreat Pressure",
        `retreat_share` = "Retreat Share",
        `retreat_speed_cond` = "Retreat Speed (Conditional)",
        `approach_speed_cond` = "Approach Speed (Conditional)",
        `absqd` = "Absolute Arc-length Velocity",
        `v_e` = "Escape Distance Velocity",
        `edge_pressure` = "Edge Pressure",
        `edge_pressure_clamped` = "Edge Pressure (Clamped)",
        `log_v_to_edge` = "Log1p(Velocity Towards Edge)",
        `post_log_v_to_edge` = "Log1p(Velocity Towards Edge Mean)",
        `post_log_edge_pressure` = "Log1p(Edge Pressure Mean)"
    )
}

#' Get simulation data for a specific participant and trial
#'
#' @param participant Participant identifier
#' @param trial Trial number
#' @return Tibble with simulation data or empty tibble if no data found
#' @export
get_simulation_data <- function(participant, trial, enable_risk = TRUE, tau = 0.2, allow_direct_model = FALSE) {
    # cat(sprintf("[INFO] get_simulation_data: loading simulation data for participant %s, trial %s.\n", participant, trial))
    # Load raw simulation data and apply trial duration capping
    sim_data <- get_t_data(participant, "sim", trial)

    # Filter out non-simulation steps
    sim_data <- sim_data %>%
        dplyr::filter(dt > 0)

    if (nrow(sim_data) == 0) {
        return(tibble())
    }

    # Get level data for arcDeg and apply trial duration capping
    level_data <- get_t_data(participant, "level", trial, apply_udp_trimming = FALSE)
    level_data <- shift_trial_time_lookup(level_data, participant, trial)

    # If level data is missing or empty, log an error but continue.
    # Using cat() ensures it is captured by captureOutput in parallel logs.
    if (is.null(level_data) || nrow(level_data) == 0) {
        cat(sprintf(
            "[ERROR] Level data not found or empty for participant %s, trial %s (tracker: level). Proceeding with default arcDeg=180.\n",
            as.character(participant), as.character(trial)
        ))
        # Allow downstream code to handle missing level data (assign_arc_degrees will default to 180)
    }

    # Load task data and merge columns
    task_data <- get_t_data(participant, "task", trial)
    if (!is.null(task_data) && nrow(task_data) > 0) {
        sim_data <- left_join(
            sim_data,
            task_data %>% select(time, pInput, pVel, pAcc, score, time_in_bowl, ball_in_bowl),
            by = "time"
        )
        # Remove samples where pVel == 0 and time_in_bowl < 0.001 (near-zero dwell in bowl with no plate motion)
        remove_mask <- is.finite(sim_data$pVel) & sim_data$pVel == 0 &
            is.finite(sim_data$time_in_bowl) & sim_data$time_in_bowl < 1e-3
        removed_n <- sum(remove_mask, na.rm = TRUE)
        if (removed_n > 0) {
            # cat(sprintf("[INFO] Removed %d samples with pVel == 0 and time_in_bowl < 0.001.\n", removed_n))
            sim_data <- sim_data[!remove_mask, , drop = FALSE]
        } else {
            cat("[INFO] No samples removed with pVel == 0 and time_in_bowl < 0.001.\n")
        }
    }

    # Add identifiers BEFORE processing (needed for risk predictions)
    # Extract unique participant value for condition and phase functions
    participant_value <- unique(participant)
    if (length(participant_value) != 1) {
        stop(sprintf("Expected single participant, got %d: %s", length(participant_value), paste(participant_value, collapse = ", ")))
    }

    sim_data <- sim_data %>%
        mutate(
            participant = as.factor(participant),
            trialNum = as.ordered(trial),
            condition = condition_number(participant_value),
            phase = get_trial_phase(participant_value, trial),
            taskNum = task_num_lookup(trial)
        )

    # Process the simulation data (now with task columns and identifiers available)
    sim_data <- process_simulation_data(sim_data, level_data, enable_risk, tau, allow_direct_model)

    # DEBUG: Check if time_in_bowl column successfully merged and resolve duplicates if needed
    time_in_bowl_like <- grep("^time_in_bowl", colnames(sim_data), value = TRUE)
    if (length(time_in_bowl_like) > 1 && !("time_in_bowl" %in% colnames(sim_data))) {
        # Prefer unsuffixed column if exists, otherwise take the first match
        sim_data$time_in_bowl <- sim_data[[time_in_bowl_like[1]]]
    }

    if ("time_in_bowl" %in% colnames(sim_data)) {
        non_na_idx <- which(!is.na(sim_data$time_in_bowl))
        first_val <- if (length(non_na_idx) > 0) sim_data$time_in_bowl[non_na_idx[1]] else NA_real_
    }

    return(sim_data)
}

#' Process simulation data with enhanced metrics
#'
#' @param sim_data Raw simulation data
#' @param level_data Level data for arcDeg assignment
#' @return Processed simulation data with enhanced metrics
#' @export
process_simulation_data <- function(sim_data, level_data, enable_risk = FALSE, tau = 0.2, allow_direct_model = FALSE) {
    # Step 1: Detect respawn events for ball fall counting
    sim_data <- detect_respawn_events(sim_data)


    # Step 2: Assign arc degrees from task level data
    sim_data <- assign_arc_degrees(sim_data, level_data)

    # Step 3: Calculate world coordinates using CircularProfileShape
    sim_data <- calculate_coordinates(sim_data)

    # Step 4: Add simulation time column
    sim_data <- add_simulation_time(sim_data)

    # Step 5: Apply post-processing for velocities and accelerations
    sim_data <- post_process_df(sim_data)

    # Step 6: Ensure qdd column exists
    sim_data <- ensure_qdd_column(sim_data)

    # Step 7: Add energy, power, and safety metrics
    representative_arcDeg <- tail(sim_data$arcDeg, 1)
    representative_shape <- CircularProfileShape$new(
        R = 5.0, arcDeg = representative_arcDeg, L = 0,
        mu_d = 0.03, mu_s = 0.05, damping = 0, g = 9.81
    )

    sim_data <- add_energy_cols(sim_data, g = representative_shape$g)
    sim_data <- add_safety_cols(sim_data, representative_shape)

    # Step 7.5: Add escape distance velocity metric (depends on dist_to_escape_ratio from safety cols)
    sim_data <- add_escape_distance_velocity(sim_data)

    # Step 8: Set first sample after each gap to NA to prevent velocity/acceleration spikes
    sim_data <- zero_first_sample_after_gaps(sim_data)

    # Step 8.5: Add power and work calculations AFTER zeroing gaps to prevent NA propagation
    sim_data <- add_power_cols(sim_data)

    # Step 9: Add risk predictions if model exists and enabled
    sim_data <- add_risk_predictions(sim_data, enable_risk, tau, allow_direct_model)

    return(sim_data)
}

#' Calculate coordinates from simulation data using CircularProfileShape
#'
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
    }

    # Add world coordinates in same step
    data %>%
        mutate(
            x_world = p + x # World x position
            # y is the same in both frames (plate doesn't move vertically)
        )
}

#' Helper function to detect and mark respawn events
#' @param data Raw simulation data
#' @return Data with respawn detection columns added
detect_respawn_events <- function(data) {
    result <- data %>%
        arrange(time) %>%
        mutate(
            time_diff = c(0, diff(time)),
            is_respawn_gap = time_diff > 1.0,
            respawn_segment = cumsum(is_respawn_gap) + 1,
            simulating = TRUE,
            # Mark the sample before each gap as "ball dropped after this sample"
            is_last_sample_of_segment = lead(is_respawn_gap, default = FALSE)
        )

    # Debug: Check what we found
    n_gaps <- sum(result$is_respawn_gap, na.rm = TRUE)
    n_last_samples <- sum(result$is_last_sample_of_segment, na.rm = TRUE)
    cat(sprintf(
        "[DEBUG] detect_respawn_events: found %d gaps, %d last samples of segments\n",
        n_gaps, n_last_samples
    ))

    return(result)
}


#' Helper function to assign arcDeg values based on task level data
#' Uses global `arcdeg_lookup_table` (defined in initialization.R) to map
#' recorded `level` → corrected arcDeg, ignoring the mis-recorded `deg` column.
#' @param sim_data Simulation data
#' @param level_data Task level data
#' @return Simulation data with arcDeg column
assign_arc_degrees <- function(sim_data, level_data) {
    # Ensure global parameters (including arcdeg_lookup_table) are available
    if (exists("ensure_global_data_initialized", envir = .GlobalEnv)) {
        get("ensure_global_data_initialized", envir = .GlobalEnv)()
    }

    if (is.null(level_data) || nrow(level_data) == 0) {
        sim_data$arcDeg <- 180
        return(sim_data)
    }

    level_data <- level_data[order(level_data$time), ]
    level_indices <- findInterval(sim_data$time, level_data$time)
    level_indices[level_indices == 0] <- 1 # Fix for initial 0-level timestamp duplication

    # Select the level at each sim time
    levels_at_time <- level_data$level[level_indices]

    # Map level → arcDeg using lookup; coerce to character for names indexing
    if (exists("arcdeg_lookup_table", envir = .GlobalEnv)) {
        lookup <- get("arcdeg_lookup_table", envir = .GlobalEnv)
        sim_data$arcDeg <- as.numeric(lookup[as.character(levels_at_time)])
    } else {
        # Fallback: compute same mapping on the fly (8 - level/3)
        sim_data$arcDeg <- as.numeric(8 - as.numeric(levels_at_time) / 3)
    }

    # Any NA (e.g., out-of-range/missing levels) default to 180 for safety
    sim_data$arcDeg[!is.finite(sim_data$arcDeg)] <- 180

    return(sim_data)
}

#' Helper function to add simulation time column
#' @param data Enhanced simulation data
#' @return Data with simulation_time column
add_simulation_time <- function(data) {
    data %>%
        mutate(
            simulation_time = cumsum(dt)
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

#' Helper function to add velocity towards edge metrics
#' @param data Simulation data with q and qd columns
#' @return Data with velocity_towards_edge, approach_pressure, retreat_pressure, retreat_share, retreat_speed_cond, approach_speed_cond, and log_v_to_edge columns added
add_velocity_towards_edge_metrics <- function(data) {
    if (sum(data$simulating) > 0) {
        # Calculate velocity towards edge: qd * sign(q)
        q_magnitude <- abs(data$q)
        q_direction <- ifelse(q_magnitude > 0, data$q / q_magnitude, 0)
        v_towards_edge <- data$qd * q_direction

        # Add velocity_towards_edge column
        data$velocity_towards_edge <- NA_real_
        data$velocity_towards_edge[data$simulating] <- v_towards_edge[data$simulating]

        # Add approach_pressure column: pmax(velocity_towards_edge, 0) - captures how aggressively participants drive toward edges
        approach_pressure <- pmax(v_towards_edge, 0)
        data$approach_pressure <- NA_real_
        data$approach_pressure[data$simulating] <- approach_pressure[data$simulating]

        # Add retreat_pressure column: -pmin(velocity_towards_edge, 0) - captures retreat behavior
        retreat_pressure <- -pmin(v_towards_edge, 0)
        data$retreat_pressure <- NA_real_
        data$retreat_pressure[data$simulating] <- retreat_pressure[data$simulating]

        # Add retreat_share column: mean(v_towards_edge < 0) - frequency of retreat behavior
        retreat_share <- as.numeric(v_towards_edge < 0)
        data$retreat_share <- NA_real_
        data$retreat_share[data$simulating] <- retreat_share[data$simulating]

        # Add retreat_speed_cond column: -v_towards_edge[v_towards_edge < 0] - conditional retreat speed
        retreat_speed_cond <- ifelse(v_towards_edge < 0, -v_towards_edge, NA_real_)
        data$retreat_speed_cond <- NA_real_
        data$retreat_speed_cond[data$simulating] <- retreat_speed_cond[data$simulating]

        # Add approach_speed_cond column: v_towards_edge[v_towards_edge > 0] - conditional approach speed
        approach_speed_cond <- ifelse(v_towards_edge > 0, v_towards_edge, NA_real_)
        data$approach_speed_cond <- NA_real_
        data$approach_speed_cond[data$simulating] <- approach_speed_cond[data$simulating]

        # Add log(v_to_edge) column: log1p(approach_pressure) to handle zeros properly
        data$log_v_to_edge <- NA_real_
        data$log_v_to_edge[data$simulating] <- log1p(approach_pressure[data$simulating])

        # Add absqd column: absolute value of qd (velocity magnitude)
        data$absqd <- NA_real_
        data$absqd[data$simulating] <- abs(data$qd[data$simulating])
        
    } else {
        data$velocity_towards_edge <- NA_real_
        data$approach_pressure <- NA_real_
        data$retreat_pressure <- NA_real_
        data$retreat_share <- NA_real_
        data$retreat_speed_cond <- NA_real_
        data$approach_speed_cond <- NA_real_
        data$log_v_to_edge <- NA_real_
        data$absqd <- NA_real_
        cat("[RISK] No on-platform samples found, all velocity metrics set to NA\n")
    }
    return(data)
}

#' Helper function to add edge-pressure metric
#' @param data Simulation data with velocity_towards_edge and dist_to_escape_ratio
#' @return Data with edge_pressure, edge_pressure_clamped, and edge_pressure_unclamped columns added
add_edge_pressure_metric <- function(data) {
    if (sum(data$simulating) > 0 && "dist_to_escape_ratio" %in% colnames(data)) {
        # Calculate edge pressure: velocity towards edge * (1 - distance ratio)
        # Higher values indicate more pressure (closer to edge + moving towards edge)
        v_towards_edge <- data$velocity_towards_edge

        # Main edge_pressure: includes negative velocities (no pmax)
        edge_pressure <- v_towards_edge * (1 - data$dist_to_escape_ratio)

        # Clamped version: only positive velocities (pmax(0, v_towards_edge))
        edge_pressure_clamped <- pmax(0, v_towards_edge) * (1 - data$dist_to_escape_ratio)

        data$edge_pressure <- NA_real_
        data$edge_pressure[data$simulating] <- edge_pressure[data$simulating]

        data$edge_pressure_clamped <- NA_real_
        data$edge_pressure_clamped[data$simulating] <- edge_pressure_clamped[data$simulating]

        cat(sprintf(
            "[RISK] Added edge_pressure for %d on-platform samples (range: %.4f to %.4f)\n",
            sum(data$simulating), min(edge_pressure[data$simulating], na.rm = TRUE), max(edge_pressure[data$simulating], na.rm = TRUE)
        ))
        cat(sprintf(
            "[RISK] Added edge_pressure_clamped for %d on-platform samples (range: %.4f to %.4f)\n",
            sum(data$simulating), min(edge_pressure_clamped[data$simulating], na.rm = TRUE), max(edge_pressure_clamped[data$simulating], na.rm = TRUE)
        ))
    } else {
        data$edge_pressure <- NA_real_
        data$edge_pressure_clamped <- NA_real_
        if (sum(data$simulating) == 0) {
            cat("[RISK] No on-platform samples found, edge pressure metrics set to NA\n")
        } else {
            cat("[RISK] dist_to_escape_ratio not available, edge pressure metrics set to NA\n")
        }
    }
    return(data)
}

#' Helper function to add escape distance velocity metric
#' @param data Simulation data with dist_to_escape_ratio and dt columns
#' @return Data with v_e column added
add_escape_distance_velocity <- function(data) {
    if (sum(data$simulating) > 0 && "dist_to_escape_ratio" %in% colnames(data) && "dt" %in% colnames(data)) {
        # Calculate v_e = d(dist_to_escape_ratio)/dt using finite differences
        # Forward difference: (f(t+dt) - f(t)) / dt

        # Initialize v_e column
        data$v_e <- NA_real_

        # Only calculate for on-platform samples
        simulating_mask <- data$simulating
        n_simulating <- sum(simulating_mask)

        if (n_simulating > 1) {
            # Get indices of simulating samples
            simulating_indices <- which(simulating_mask)

            # Create masks for current and next samples
            curr_mask <- simulating_indices[1:(n_simulating - 1)]
            next_mask <- simulating_indices[2:n_simulating]

            # Check if samples are consecutive (no gaps between them)
            consecutive_mask <- (next_mask - curr_mask) == 1

            # Calculate finite differences vectorized
            ratio_curr <- data$dist_to_escape_ratio[curr_mask]
            ratio_next <- data$dist_to_escape_ratio[next_mask]
            dt_vals <- data$dt[curr_mask]

            # Only calculate where we have consecutive samples and valid data
            valid_mask <- consecutive_mask &
                !is.na(ratio_curr) & !is.na(ratio_next) &
                !is.na(dt_vals) & dt_vals > 0

            if (sum(valid_mask) > 0) {
                data$v_e[curr_mask[valid_mask]] <- (ratio_next[valid_mask] - ratio_curr[valid_mask]) / dt_vals[valid_mask]
            }
        }

        cat(sprintf(
            "[RISK] Added v_e (escape distance velocity) for %d on-platform samples (range: %.4f to %.4f)\n",
            sum(!is.na(data$v_e)), min(data$v_e, na.rm = TRUE), max(data$v_e, na.rm = TRUE)
        ))
    } else {
        data$v_e <- NA_real_
        if (sum(data$simulating) == 0) {
            cat("[RISK] No on-platform samples found, v_e set to NA\n")
        } else {
            cat("[RISK] dist_to_escape_ratio or dt not available, v_e set to NA\n")
        }
    }
    return(data)
}

#' Helper function to set samples around gaps to NA to prevent velocity/acceleration spikes
#' @param data Enhanced simulation data
#' @return Data with samples around gaps set to NA
zero_first_sample_after_gaps <- function(data) {
    if (!"is_respawn_gap" %in% colnames(data)) {
        return(data)
    }

    # Find indices of gaps
    gap_indices <- which(data$is_respawn_gap)

    if (length(gap_indices) == 0) {
        return(data)
    }

    # Find indices of last sample before each gap and first sample after each gap
    # last_sample_before_gap_indices <- gap_indices - 1
    first_sample_after_gap_indices <- gap_indices

    # Remove any invalid indices (e.g., if gap is at first sample)
    # last_sample_before_gap_indices <- last_sample_before_gap_indices[last_sample_before_gap_indices > 0]

    # Combine all indices that need to be set to NA
    indices_to_zero <- unique(c(first_sample_after_gap_indices))

    # Define variables that should be set to NA around gaps
    # These are variables that would have artificial jumps due to respawn
    variables_to_zero <- c(
        # Position variables (keep as they represent actual respawn position)
        # "q", "x", "y", "x_world", "p", "pInput",

        # Velocity variables (these would have huge spikes)
        "qd", "vx", "vy", "vx_world", "pVel", "pVelAbsolute",

        # Acceleration variables (these would have huge spikes)
        "qdd", "ax", "ay", "ax_world", "pAcc", "pAccAbsolute",

        # Energy variables (these would have jumps)
        "ke", "ke_world", "pe", "e", "e_world",

        # Power variables (these would have jumps)
        "power", "power_world", "power_plate",

        # Work variables are now calculated AFTER this function, so don't zero them here
        # "work", "work_world", "work_plate",

        # Safety variables (these would have jumps)
        "margin_E", "danger", "time_to_escape", "dist_to_escape",
        "dist_to_escape_ratio", "dist_to_escape_arcdeg6", "e_total_needed"
    )

    # Set specified variables to NA at samples around gaps
    for (var in variables_to_zero) {
        if (var %in% colnames(data)) {
            data[[var]][indices_to_zero] <- NA_real_
        }
    }

    return(data)
}

#' Helper function to add risk predictions if model exists
#' @param data Enhanced simulation data
#' @return Data with risk prediction columns added
# Utility: Compute hazard-bin width per row
compute_dt_haz <- function(hazard_data) {
    hazard_data <- hazard_data |>
        dplyr::arrange(respawn_segment, time) |>
        dplyr::group_by(respawn_segment) |>
        dplyr::mutate(
            dt_haz = dplyr::lead(time) - time,
            dt_haz = ifelse(is.na(dt_haz), dplyr::lag(dt_haz), dt_haz), # carry forward for last row
            dt_haz = ifelse(is.na(dt_haz), stats::median(dt_haz, na.rm = TRUE), dt_haz) # fallback to median
        ) |>
        dplyr::ungroup()
    return(hazard_data)
}

# Utility: Assign risk predictions to data, mapping only to on-platform samples
assign_risk_predictions <- function(data, p_bin, lambda, p_1s, method_name = "unknown") {
    # Create mask for on-platform samples that are NOT last samples of segments
    # This matches what the global hazard samples contain
    on_platform_mask <- data$simulating
    if ("is_last_sample_of_segment" %in% names(data)) {
        on_platform_mask <- on_platform_mask & !data$is_last_sample_of_segment
    }

    on_platform_indices <- which(on_platform_mask)

    # Check if we have the right number of predictions
    n_predictions <- length(p_bin)
    n_target_samples <- length(on_platform_indices)

    if (n_predictions != n_target_samples) {
        cat(sprintf(
            "[RISK] WARNING: Prediction count mismatch (%s): got %d predictions for %d target samples\n",
            method_name, n_predictions, n_target_samples
        ))

        # Use minimum length to avoid errors
        n_to_assign <- min(n_predictions, n_target_samples)
        cat(sprintf("[RISK] Using first %d predictions for first %d target samples\n", n_to_assign, n_to_assign))

        # Truncate arrays to minimum length
        p_bin <- p_bin[1:n_to_assign]
        lambda <- lambda[1:n_to_assign]
        p_1s <- p_1s[1:n_to_assign]
        on_platform_indices <- on_platform_indices[1:n_to_assign]
    }

    # Initialize all risk columns to NA
    data$drop_risk_bin <- NA_real_
    data$drop_lambda <- NA_real_
    data$drop_risk_1s <- NA_real_

    # Assign predictions to the target samples
    data$drop_risk_bin[on_platform_indices] <- p_bin
    data$drop_lambda[on_platform_indices] <- lambda
    data$drop_risk_1s[on_platform_indices] <- p_1s

    # Debug: Check assignment immediately after
    cat(sprintf(
        "[RISK] After assignment (%s) - drop_risk_bin: %d/%d valid, drop_lambda: %d/%d valid, drop_risk_1s: %d/%d valid\n",
        method_name, sum(!is.na(data$drop_risk_bin)), nrow(data),
        sum(!is.na(data$drop_lambda)), nrow(data),
        sum(!is.na(data$drop_risk_1s)), nrow(data)
    ))

    return(data)
}


add_risk_predictions <- function(data, enable_risk = FALSE, tau = 0.2, allow_direct_model = FALSE) {
    cat(sprintf(
        "[RISK] Starting add_risk_predictions for %d samples, enable_risk=%s\n",
        nrow(data), enable_risk
    ))

    # Add velocity towards edge metrics using helper function
    data <- add_velocity_towards_edge_metrics(data)

    # Add edge-pressure metric using helper function
    data <- add_edge_pressure_metric(data)

    # Initialize all risk columns
    data$drop_risk_bin <- NA_real_
    data$drop_lambda <- NA_real_
    data$drop_risk_dt <- NA_real_
    data$drop_risk_1s <- NA_real_

    # Check if risk predictions are enabled
    if (!enable_risk) {
        cat("[RISK] Risk predictions disabled, returning with NA risk columns\n")
        return(data)
    }

    # Try global hazard samples first (most efficient), fallback to direct model if needed
    risk_scores <- NULL
    prediction_method <- "none"

    # First attempt: Use global hazard samples with predictions (most efficient)
    cat("[RISK] Attempting to use global hazard samples with predictions...\n")
    tryCatch(
        {
            # Check if global hazard samples exist
            if (!exists("GLOBAL_HAZARD_SAMPLES_PREDS") || is.null(GLOBAL_HAZARD_SAMPLES_PREDS)) {
                stop("No global hazard samples with predictions found")
            }

            # Get tau from global variable if available
            if (exists("GLOBAL_RISK_TAU") && !is.null(GLOBAL_RISK_TAU)) {
                cat(sprintf("[RISK] Using tau from global variable: %.3f seconds\n", GLOBAL_RISK_TAU))
                tau <- GLOBAL_RISK_TAU
            } else {
                cat(sprintf("[RISK] No global tau found, using parameter: %.3f seconds\n", tau))
            }

            # Try to use global hazard samples
            risk_scores <- score_trial_with_global_predictions(data, tau)

            # Check if we got valid predictions
            if (is.null(risk_scores) || length(risk_scores$individual_predictions) == 0) {
                stop("No valid predictions returned from global hazard samples")
            }

            # Check for all NA predictions (indicates a problem)
            if (all(is.na(risk_scores$individual_predictions))) {
                stop("All predictions are NA from global hazard samples")
            }

            # Check for mostly NA predictions (indicates a serious problem)
            na_ratio <- sum(is.na(risk_scores$individual_predictions)) / length(risk_scores$individual_predictions)
            if (na_ratio > 0.9) {
                stop(sprintf("Too many NA predictions from global hazard samples (%.1f%% NA)", na_ratio * 100))
            }

            prediction_method <- "global_hazard_samples"
            cat("[RISK] SUCCESS: Using global hazard samples with predictions\n")
        },
        error = function(e) {
            cat(sprintf("[RISK] Global hazard samples failed: %s\n", e$message))
            risk_scores <<- NULL
        }
    )

    # Fallback: Use direct model prediction if global hazard samples failed and allowed
    if (is.null(risk_scores) && allow_direct_model) {
        cat("[RISK] Falling back to direct model prediction (allow_direct_model=TRUE)\n")
        tryCatch(
            {
                # Check if global risk model exists
                if (!exists("GLOBAL_RISK_MODEL") || is.null(GLOBAL_RISK_MODEL)) {
                    stop("No global risk model found - use dashboard to load one")
                }

                model <- GLOBAL_RISK_MODEL
                cat(sprintf("[RISK] Using global risk model, class: %s\n", class(model)[1]))

                # Check if this is an old glmmTMB model
                if (inherits(model, "glmmTMB")) {
                    stop("Found old glmmTMB model. Please retrain the model to use the new mgcv::bam approach.")
                }

                # Get tau from global variable if available
                if (exists("GLOBAL_RISK_TAU") && !is.null(GLOBAL_RISK_TAU)) {
                    cat(sprintf("[RISK] Using tau from global variable: %.3f seconds\n", GLOBAL_RISK_TAU))
                    tau <- GLOBAL_RISK_TAU
                } else {
                    cat(sprintf("[RISK] No global tau found, using parameter: %.3f seconds\n", tau))
                }

                # Use direct model prediction (standardized by default)
                risk_scores <- score_trial_with_model(data, model, tau, standardized = FALSE, use_factors = FALSE)

                # Check if we got valid predictions
                if (is.null(risk_scores) || length(risk_scores$individual_predictions) == 0) {
                    stop("No valid predictions returned from direct model")
                }

                # Check for all NA predictions (indicates a problem)
                if (all(is.na(risk_scores$individual_predictions))) {
                    stop("All predictions are NA from direct model")
                }

                # Check for mostly NA predictions (indicates a serious problem)
                na_ratio <- sum(is.na(risk_scores$individual_predictions)) / length(risk_scores$individual_predictions)
                if (na_ratio > 0.9) {
                    stop(sprintf("Too many NA predictions from direct model (%.1f%% NA)", na_ratio * 100))
                }

                prediction_method <- "direct_model"
                cat("[RISK] SUCCESS: Using direct model prediction as fallback\n")
            },
            error = function(e) {
                cat(sprintf("[RISK] Direct model prediction also failed: %s\n", e$message))
                risk_scores <<- NULL
            }
        )
    }

    # If both methods failed, return early
    if (is.null(risk_scores)) {
        if (allow_direct_model) {
            cat("[RISK] Both global hazard samples and direct model prediction failed\n")
        } else {
            cat("[RISK] Global hazard samples failed and direct model prediction is disabled\n")
        }
        return(data)
    }

    # Process the results (same for both approaches)
    tryCatch(
        {
            if (length(risk_scores$individual_predictions) == 0) {
                cat("[RISK] No predictions returned from global hazard samples\n")
                return(data)
            }

            # Get the individual predictions (these are per-bin probabilities)
            p_bin <- risk_scores$individual_predictions

            # Calculate risk metrics using tau (not dt_sample)
            # lambda: instantaneous hazard rate per second
            lambda <- exp(log(-log(1 - p_bin))) / tau

            # p_1s: 1-second risk (comparable across sampling rates)
            p_1s <- 1 - exp(-lambda * 1)

            # Assign results using helper function
            data <- assign_risk_predictions(data, p_bin, lambda, p_1s, "global_hazard_samples")

            # Provide proof of successful assignment
            n_valid_bin <- sum(!is.na(p_bin))
            n_valid_lambda <- sum(!is.na(lambda))
            n_valid_1s <- sum(!is.na(p_1s))

            approach_name <- if (prediction_method == "direct_model") "Direct model predictions" else "Global hazard predictions"
            cat(sprintf("[RISK] SUCCESS: %s computed for all samples:\n", approach_name))
            cat(sprintf(
                "  - drop_risk_bin: %d/%d valid values (%.1f%%)\n",
                n_valid_bin, nrow(data), 100 * n_valid_bin / nrow(data)
            ))
            cat(sprintf(
                "  - drop_lambda: %d/%d valid values (%.1f%%)\n",
                n_valid_lambda, nrow(data), 100 * n_valid_lambda / nrow(data)
            ))
            cat(sprintf(
                "  - drop_risk_1s: %d/%d valid values (%.1f%%)\n",
                n_valid_1s, nrow(data), 100 * n_valid_1s / nrow(data)
            ))

            if (n_valid_lambda > 0) {
                cat(sprintf(
                    "  - drop_lambda range: %.6f to %.6f per second\n",
                    min(lambda, na.rm = TRUE), max(lambda, na.rm = TRUE)
                ))
            }
            if (n_valid_1s > 0) {
                cat(sprintf(
                    "  - drop_risk_1s range: %.6f to %.6f\n",
                    min(p_1s, na.rm = TRUE), max(p_1s, na.rm = TRUE)
                ))
            }
        },
        error = function(e) {
            cat(sprintf("[RISK] ERROR during global hazard predictions: %s\n", e$message))
        }
    )

    # Debug: Check final state before returning
    cat(sprintf(
        "[RISK] Final check - drop_risk_bin: %d/%d valid, drop_lambda: %d/%d valid, drop_risk_1s: %d/%d valid\n",
        sum(!is.na(data$drop_risk_bin)), nrow(data),
        sum(!is.na(data$drop_lambda)), nrow(data),
        sum(!is.na(data$drop_risk_1s)), nrow(data)
    ))

    cat("[RISK] add_risk_predictions completed\n")
    return(data)
}

#' Score trial with global hazard samples predictions (efficient approach)
#'
#' @param sim_data Simulation data from get_simulation_data()
#' @param tau Time horizon for drop prediction (default 0.2 seconds)
#' @return List with individual_predictions and hazard_samples
#' @export

score_trial_with_global_predictions <- function(sim_data, tau = 0.2) {
    # Check if global hazard samples exist
    if (!exists("GLOBAL_HAZARD_SAMPLES_PREDS") || is.null(GLOBAL_HAZARD_SAMPLES_PREDS)) {
        cat("[SCORE] No global hazard samples with predictions found\n")
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    # Identify this participant/trial
    pid <- unique(sim_data$participant)
    if (length(pid) != 1) pid <- pid[1]

    trial <- unique(sim_data$trial)
    if (length(trial) != 1) trial <- trial[1]

    # Filter global hazard samples by participant and trial
    haz_all <- GLOBAL_HAZARD_SAMPLES_PREDS
    sel <- rep(TRUE, nrow(haz_all))
    if ("participant" %in% names(haz_all)) sel <- sel & (as.character(haz_all$participant) == as.character(pid))
    if ("trial" %in% names(haz_all)) sel <- sel & (as.character(haz_all$trial) == as.character(trial))

    haz_trial <- haz_all[sel, , drop = FALSE]
    cat(sprintf("[SCORE] Filtered to %d rows for participant %s, trial %s\n", nrow(haz_trial), pid, trial))

    if (nrow(haz_trial) == 0) {
        cat("[SCORE] No hazard samples found for this participant/trial\n")
        return(list(
            individual_predictions = numeric(0),
            hazard_samples = data.frame()
        ))
    }

    # Pick preferred prediction column (p_hat_fe for fixed effects)
    pred_col <- "p_hat_fe"
    if (!pred_col %in% names(haz_trial)) {
        # Fallback to other prediction columns
        available_cols <- intersect(c("p_hat_fe", "p_hat_re", "p_hat"), names(haz_trial))
        if (length(available_cols) > 0) {
            pred_col <- available_cols[1]
            cat(sprintf("[SCORE] Using fallback prediction column: %s\n", pred_col))
        } else {
            cat("[SCORE] No prediction columns found in global hazard samples\n")
            return(list(
                individual_predictions = numeric(0),
                hazard_samples = data.frame()
            ))
        }
    }

    cat(sprintf(
        "[SCORE] Using global predictions (%s) for participant %s, trial %s\n",
        pred_col, pid, trial
    ))

    return(list(
        individual_predictions = haz_trial[[pred_col]],
        hazard_samples = haz_trial
    ))
}

# ARCHIVED: Direct model prediction approach (kept for reference)
# This was the previous approach that used direct model prediction on all samples
# It was replaced with the saved predictions approach for better reliability
add_risk_predictions_direct_model <- function(data, enable_risk = FALSE, tau = 0.2) {
    cat(sprintf(
        "[RISK] Starting add_risk_predictions_direct_model for %d samples, enable_risk=%s\n",
        nrow(data), enable_risk
    ))

    # Add velocity towards edge metrics using helper function
    data <- add_velocity_towards_edge_metrics(data)

    # Add edge-pressure metric using helper function
    data <- add_edge_pressure_metric(data)

    # Initialize all risk columns
    data$drop_risk_bin <- NA_real_
    data$drop_lambda <- NA_real_
    data$drop_risk_dt <- NA_real_
    data$drop_risk_1s <- NA_real_

    # Check if risk predictions are enabled
    if (!enable_risk) {
        cat("[RISK] Risk predictions disabled, returning with NA risk columns\n")
        return(data)
    }

    # Check if global risk model exists
    if (!exists("GLOBAL_RISK_MODEL") || is.null(GLOBAL_RISK_MODEL)) {
        cat("[RISK] No global risk model found - use dashboard to load one\n")
        return(data)
    }

    # Use global risk model for direct prediction
    tryCatch(
        {
            model <- GLOBAL_RISK_MODEL
            cat(sprintf("[RISK] Using global risk model, class: %s\n", class(model)[1]))

            # Check if this is an old glmmTMB model
            if (inherits(model, "glmmTMB")) {
                cat("[RISK] WARNING: Found old glmmTMB model. Please retrain the model to use the new mgcv::bam approach.\n")
                return(data)
            }

            # Get tau from model attribute if available, otherwise use parameter
            model_tau <- attr(model, "tau")
            if (!is.null(model_tau)) {
                cat(sprintf("[RISK] Using tau from model: %.3f seconds\n", model_tau))
                tau <- model_tau
            } else {
                cat(sprintf("[RISK] No tau found in model, using parameter: %.3f seconds\n", tau))
            }

            # Debug: Check model structure
            cat(sprintf("[RISK] Model model data columns: %s\n", paste(names(model$model), collapse = ", ")))
            cat(sprintf("[RISK] Model formula: %s\n", deparse(model$formula)))
            if ("condition" %in% names(model$model)) {
                cat(sprintf("[RISK] Model condition levels: '%s'\n", paste(levels(model$model$condition), collapse = "', '")))
            }
            if ("phase" %in% names(model$model)) {
                cat(sprintf("[RISK] Model phase levels: '%s'\n", paste(levels(model$model$phase), collapse = "', '")))
            }

            # Prepare data for prediction using direct model approach
            cat("[RISK] Preparing data for direct model prediction...\n")

            # Extract risk features directly (no filtering like build_hazard_samples)
            cat(sprintf("[RISK] Extracting risk features for all %d samples...\n", nrow(data)))
            features <- extract_risk_features(data)

            # Debug: Check feature ranges
            cat(sprintf(
                "[RISK] Feature ranges - e: %.4f to %.4f, v: %.4f to %.4f\n",
                min(features$e, na.rm = TRUE), max(features$e, na.rm = TRUE),
                min(features$v, na.rm = TRUE), max(features$v, na.rm = TRUE)
            ))

            # Create prediction data frame with required columns
            pred_data <- data.frame(
                e = features$e,
                v = features$v,
                participant = data$participant,
                condition = data$condition,
                phase = data$phase
            )

            # Debug: Check input data
            cat(sprintf(
                "[RISK] Input data - participants: %s, conditions: %s, phases: %s\n",
                paste(unique(pred_data$participant), collapse = ", "),
                paste(unique(pred_data$condition), collapse = ", "),
                paste(unique(pred_data$phase), collapse = ", ")
            ))

            # Ensure factors match model levels
            if ("condition" %in% names(model$model)) {
                model_cond_levels <- levels(model$model$condition)
                cat(sprintf("[RISK] Model condition levels: %s\n", paste(model_cond_levels, collapse = ", ")))
                if (length(model_cond_levels) > 0) {
                    pred_data$condition <- factor(pred_data$condition, levels = model_cond_levels)
                } else {
                    cat("[RISK] WARNING: Model condition levels are empty, using data levels\n")
                    pred_data$condition <- factor(pred_data$condition)
                }
            }
            if ("phase" %in% names(model$model)) {
                model_phase_levels <- levels(model$model$phase)
                cat(sprintf("[RISK] Model phase levels: %s\n", paste(model_phase_levels, collapse = ", ")))
                if (length(model_phase_levels) > 0) {
                    # Handle phases not found in model by setting them to baseline_task
                    pred_phases <- as.character(pred_data$phase)
                    unknown_phases <- !pred_phases %in% model_phase_levels
                    if (any(unknown_phases)) {
                        cat(sprintf("[RISK] Found %d samples with unknown phases, setting to 'baseline_task': %s\n", 
                            sum(unknown_phases), paste(unique(pred_phases[unknown_phases]), collapse = ", ")))
                        pred_phases[unknown_phases] <- "baseline_task"
                    }
                    pred_data$phase <- factor(pred_phases, levels = model_phase_levels)
                } else {
                    cat("[RISK] WARNING: Model phase levels are empty, using data levels\n")
                    pred_data$phase <- factor(pred_data$phase)
                }
            }
            pred_data$participant <- factor(pred_data$participant)

            # Get predictions (eta and p_hat)
            cat(sprintf("[RISK] Computing predictions for all %d samples...\n", nrow(pred_data)))

            # Try prediction with full data first
            eta <- tryCatch(
                {
                    predict(model, newdata = pred_data, type = "link", exclude = "s(participant)")
                },
                error = function(e) {
                    cat(sprintf("[RISK] Full prediction failed: %s\n", e$message))
                    cat("[RISK] Trying to reconstruct factor levels from model training data...\n")

                    # Try to get factor levels from the model's training data
                    model_data <- model$model
                    if ("condition" %in% names(model_data) && "phase" %in% names(model_data)) {
                        # Get unique values from training data
                        train_conditions <- unique(as.character(model_data$condition))
                        train_phases <- unique(as.character(model_data$phase))

                        cat(sprintf("[RISK] Training data conditions: %s\n", paste(train_conditions, collapse = ", ")))
                        cat(sprintf("[RISK] Training data phases: %s\n", paste(train_phases, collapse = ", ")))

                        # Handle phases not found in training data by setting them to baseline_task
                        pred_phases_fixed <- as.character(pred_data$phase)
                        unknown_phases <- !pred_phases_fixed %in% train_phases
                        if (any(unknown_phases)) {
                            cat(sprintf("[RISK] Found %d samples with unknown phases, setting to 'baseline_task': %s\n", 
                                sum(unknown_phases), paste(unique(pred_phases_fixed[unknown_phases]), collapse = ", ")))
                            pred_phases_fixed[unknown_phases] <- "baseline_task"
                        }
                        
                        # Create new prediction data with reconstructed levels
                        pred_data_fixed <- data.frame(
                            e = pred_data$e,
                            v = pred_data$v,
                            participant = pred_data$participant,
                            condition = factor(pred_data$condition, levels = train_conditions),
                            phase = factor(pred_phases_fixed, levels = train_phases)
                        )

                        # Try prediction with reconstructed levels
                        tryCatch(
                            {
                                predict(model, newdata = pred_data_fixed, type = "link", exclude = "s(participant)")
                            },
                            error = function(e3) {
                                cat(sprintf("[RISK] Reconstructed levels prediction failed: %s\n", e3$message))
                                rep(NA_real_, nrow(pred_data))
                            }
                        )
                    } else {
                        cat("[RISK] Cannot reconstruct factor levels, returning NA predictions\n")
                        rep(NA_real_, nrow(pred_data))
                    }
                }
            )

            # Debug: Check eta range
            cat(sprintf(
                "[RISK] Eta range: %.4f to %.4f (valid: %d/%d)\n",
                min(eta, na.rm = TRUE), max(eta, na.rm = TRUE),
                sum(!is.na(eta)), length(eta)
            ))

            # Calculate risk metrics using tau (not dt_sample)
            # p_bin: probability of drop within tau
            p_bin <- 1 - exp(-exp(eta))

            # Debug: Check p_bin range
            cat(sprintf(
                "[RISK] P_bin range: %.6f to %.6f (valid: %d/%d)\n",
                min(p_bin, na.rm = TRUE), max(p_bin, na.rm = TRUE),
                sum(!is.na(p_bin)), length(p_bin)
            ))

            # lambda: instantaneous hazard rate per second
            lambda <- exp(eta) / tau

            # Debug: Check lambda range
            cat(sprintf(
                "[RISK] Lambda range: %.6f to %.6f (valid: %d/%d)\n",
                min(lambda, na.rm = TRUE), max(lambda, na.rm = TRUE),
                sum(!is.na(lambda)), length(lambda)
            ))

            # p_1s: 1-second risk (comparable across sampling rates)
            p_1s <- 1 - exp(-lambda * 1)

            # Debug: Check p_1s range
            cat(sprintf(
                "[RISK] P_1s range: %.6f to %.6f (valid: %d/%d)\n",
                min(p_1s, na.rm = TRUE), max(p_1s, na.rm = TRUE),
                sum(!is.na(p_1s)), length(p_1s)
            ))

            # Assign results using helper function
            data <- assign_risk_predictions(data, p_bin, lambda, p_1s, "direct_model")

            # Provide proof of successful assignment
            n_valid_bin <- sum(!is.na(p_bin))
            n_valid_lambda <- sum(!is.na(lambda))
            n_valid_1s <- sum(!is.na(p_1s))

            cat(sprintf("[RISK] SUCCESS: Direct model predictions computed for all samples:\n"))
            cat(sprintf(
                "  - drop_risk_bin: %d/%d valid values (%.1f%%)\n",
                n_valid_bin, nrow(data), 100 * n_valid_bin / nrow(data)
            ))
            cat(sprintf(
                "  - drop_lambda: %d/%d valid values (%.1f%%)\n",
                n_valid_lambda, nrow(data), 100 * n_valid_lambda / nrow(data)
            ))
            cat(sprintf(
                "  - drop_risk_1s: %d/%d valid values (%.1f%%)\n",
                n_valid_1s, nrow(data), 100 * n_valid_1s / nrow(data)
            ))

            if (n_valid_lambda > 0) {
                cat(sprintf(
                    "  - drop_lambda range: %.6f to %.6f per second\n",
                    min(lambda, na.rm = TRUE), max(lambda, na.rm = TRUE)
                ))
            }
            if (n_valid_1s > 0) {
                cat(sprintf(
                    "  - drop_risk_1s range: %.6f to %.6f\n",
                    min(p_1s, na.rm = TRUE), max(p_1s, na.rm = TRUE)
                ))
            }
        },
        error = function(e) {
            cat(sprintf("[RISK] ERROR during direct model prediction: %s\n", e$message))
        }
    )

    # Debug: Check final state before returning
    cat(sprintf(
        "[RISK] Final check - drop_risk_bin: %d/%d valid, drop_lambda: %d/%d valid, drop_risk_1s: %d/%d valid\n",
        sum(!is.na(data$drop_risk_bin)), nrow(data),
        sum(!is.na(data$drop_lambda)), nrow(data),
        sum(!is.na(data$drop_risk_1s)), nrow(data)
    ))

    cat("[RISK] add_risk_predictions_direct_model completed\n")
    return(data)
}


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
