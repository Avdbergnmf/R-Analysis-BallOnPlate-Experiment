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
        drop_risk = "Drop Risk",
        velocity_towards_edge = "Velocity Towards Edge"
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
        `drop_risk` = "Drop Risk",
        `velocity_towards_edge` = "Velocity Towards Edge"
    )
}

#' Get simulation data for a specific participant and trial
#'
#' @param participant Participant identifier
#' @param trial Trial number
#' @return Tibble with simulation data or empty tibble if no data found
#' @export
get_simulation_data <- function(participant, trial, enable_risk = TRUE, tau = 0.2) {
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
    sim_data <- process_simulation_data(sim_data, level_data, enable_risk, tau)

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
process_simulation_data <- function(sim_data, level_data, enable_risk = FALSE, tau = 0.2) {
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
    sim_data <- add_power_cols(sim_data)
    sim_data <- add_safety_cols(sim_data, representative_shape)

    # Step 8: Set first sample after each gap to NA to prevent velocity/acceleration spikes
    sim_data <- zero_first_sample_after_gaps(sim_data)

    # Step 9: Add risk predictions if model exists and enabled
    sim_data <- add_risk_predictions(sim_data, enable_risk, tau)

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

        # Work variables (these would have jumps)
        "work", "work_world", "work_plate",

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
add_risk_predictions <- function(data, enable_risk = FALSE, tau = 0.2) {
    # Check if risk predictions are enabled
    if (!enable_risk) {
        # Risk predictions disabled, add NA columns
        data$drop_risk <- NA_real_

        # Still add velocity towards edge column for plotting
        # Calculate velocity towards edge for on-platform samples
        on_platform_mask <- data$simulating == TRUE
        if (sum(on_platform_mask) > 0) {
            # Calculate velocity towards edge
            q_magnitude <- abs(data$q[on_platform_mask])
            q_direction <- ifelse(q_magnitude > 0, data$q[on_platform_mask] / q_magnitude, 0)
            v_towards_edge <- data$qd[on_platform_mask] * q_direction

            # Initialize column with NA
            data$velocity_towards_edge <- NA_real_
            # Assign values to on-platform samples
            data$velocity_towards_edge[on_platform_mask] <- v_towards_edge
        } else {
            data$velocity_towards_edge <- NA_real_
        }

        return(data)
    }

    # Check if risk model exists
    ensure_global_data_initialized()

    if (!file.exists(risk_model_path)) {
        # No risk model available, add NA columns
        data$drop_risk <- NA_real_
        data$velocity_towards_edge <- NA_real_

        return(data)
    }

    # Use global risk model if available, otherwise skip risk predictions
    if (exists("GLOBAL_RISK_MODEL", envir = .GlobalEnv) && !is.null(get("GLOBAL_RISK_MODEL", envir = .GlobalEnv))) {
        model <- get("GLOBAL_RISK_MODEL", envir = .GlobalEnv)
    } else {
        # No global model available - skip risk predictions
        data$drop_risk <- NA_real_
        data$velocity_towards_edge <- NA_real_
        return(data)
    }

    if (is.null(model)) {
        # Failed to load risk model, add NA columns
        data$drop_risk <- NA_real_
        data$velocity_towards_edge <- NA_real_

        return(data)
    }

    # Debug: Check model class
    cat(sprintf("[DEBUG] Loaded model class: %s\n", class(model)[1]))

    # Check if this is an old glmmTMB model
    if (inherits(model, "glmmTMB")) {
        cat("[WARNING] Found old glmmTMB model. Please retrain the model to use the new mgcv::bam approach.\n")
        data$drop_risk <- NA_real_
        data$velocity_towards_edge <- NA_real_

        return(data)
    }

    # Score the trial with the risk model
    risk_scores <- score_trial_with_model(data, model, tau)

    # Initialize risk column with NA
    data$drop_risk <- NA_real_

    # Add velocity towards edge column to the data
    # Initialize column with NA
    data$velocity_towards_edge <- NA_real_

    # Extract velocity from hazard samples if available
    if (length(risk_scores$hazard_samples) > 0 && !is.null(risk_scores$hazard_samples) && nrow(risk_scores$hazard_samples) > 0) {
        hazard_samples <- risk_scores$hazard_samples

        # Since hazard samples come from the same data, times should match exactly
        # Find matching indices using match() for exact time matches
        time_matches <- match(hazard_samples$time, data$time)
        data$velocity_towards_edge[time_matches] <- hazard_samples$v
    }

    # Map individual predictions back to the original data
    # Debug: Check the structure of risk_scores
    cat(sprintf("[DEBUG] Risk scores structure:\n"))
    cat(sprintf("  - risk_scores type: %s\n", class(risk_scores)))
    cat(sprintf("  - hazard_samples length: %d\n", length(risk_scores$hazard_samples)))
    cat(sprintf("  - individual_predictions length: %d\n", length(risk_scores$individual_predictions)))

    if (length(risk_scores$hazard_samples) > 0 && !is.null(risk_scores$hazard_samples) && nrow(risk_scores$hazard_samples) > 0) {
        hazard_samples <- risk_scores$hazard_samples
        individual_predictions <- risk_scores$individual_predictions

        cat(sprintf("  - hazard_samples rows: %d\n", nrow(hazard_samples)))
        cat(sprintf("  - individual_predictions length: %d\n", length(individual_predictions)))

        # Since hazard samples come from the same data, times should match exactly
        # Find matching indices using match() for exact time matches
        time_matches <- match(hazard_samples$time, data$time)
        data$drop_risk[time_matches] <- individual_predictions
    } else {
        cat("[DEBUG] No hazard samples available for mapping predictions\n")
    }

    cat(sprintf(
        "[DEBUG] add_risk_predictions: completed for %d samples\n",
        nrow(data)
    ))
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
