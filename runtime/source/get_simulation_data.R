# ─────────────────────────────────────────────────────────────────────────────
# Simulation Data Processing
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(tibble)

# Source the simulation core functions
# source("runtime/source/simulation_core.R")

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

        # Safety parameters
        margin_E = "Energy Margin",
        danger = "Danger Level",
        e_total_needed = "Total Energy Needed to Escape",

        # Coordinate parameters
        x = "X Position (plate-relative)",
        x_world = "X Position (world)",
        y = "Y Position (height)",

        # Performance parameters
        score = "Running Score",
        total_score = "Total Score",

        # Status parameters
        simulating = "Ball on Plate Status"
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
        `score` = "Score",
        `total_score` = "Total Score",
        `arcDeg` = "Arc Degrees",
        `dt` = "Time Step",
        `time_diff` = "Time Gap",
        `is_respawn_gap` = "Is Respawn Event",
        `respawn_segment` = "Respawn Segment",
        `is_post_respawn` = "Post-Respawn Flag",
        `cumulative_gap_time` = "Cumulative Gap Time",
        `simulating` = "Ball on Plate Status"
    )
}

#' Get simulation data for a specific participant and trial
#'
#' @param participant Participant identifier
#' @param trial Trial number
#' @return Tibble with simulation data or empty tibble if no data found
#' @export
get_simulation_data <- function(participant, trial) {
    # Load raw simulation data and apply trial duration capping
    sim_data <- get_t_data(participant, "sim", trial)
    # Data already trimmed in get_t_data(); no extra capping needed

    # Filter out non-simulation steps
    sim_data <- sim_data %>%
        dplyr::filter(dt > 0)

    if (nrow(sim_data) == 0) {
        return(tibble())
    }

    # Get level data for arcDeg and apply trial duration capping
    level_data <- get_t_data(participant, "level", trial)
    # Data already trimmed in get_t_data(); no extra capping needed

    # Process the simulation data
    sim_data <- process_simulation_data(sim_data, level_data)

    # Load task data and apply trial duration capping
    task_data <- get_t_data(participant, "task", trial)
    # Data already trimmed in get_t_data(); no extra capping needed

    if (!is.null(task_data) && nrow(task_data) > 0) {
        # Add pInput if available
        if (all(c("time", "pInput") %in% colnames(task_data))) {
            sim_data <- left_join(sim_data, task_data %>% select(time, pInput), by = "time")
        }

        # Add running score if available
        if ("score" %in% colnames(task_data)) {
            sim_data <- left_join(sim_data, task_data %>% select(time, score), by = "time")
        }
    }

    # Add identifiers
    sim_data <- sim_data %>%
        mutate(
            participant = as.factor(participant),
            trialNum = as.ordered(trial)
        )

    return(sim_data)
}

#' Process simulation data with enhanced metrics
#'
#' @param sim_data Raw simulation data
#' @param level_data Level data for arcDeg assignment
#' @return Processed simulation data with enhanced metrics
#' @export
process_simulation_data <- function(sim_data, level_data) {
    # Step 1: Detect respawn events and mark simulation data
    sim_data <- detect_respawn_events(sim_data)

    # Step 2: Add artificial datapoints at gap boundaries
    sim_data <- add_artificial_gap_points(sim_data)

    # Step 3: Assign arc degrees from task level data
    sim_data <- assign_arc_degrees(sim_data, level_data)

    # Step 4: Calculate world coordinates using CircularProfileShape
    sim_data <- calculate_coordinates(sim_data)

    # Step 5: Add simulation time column
    sim_data <- add_simulation_time(sim_data)

    # Step 6: Apply post-processing for velocities and accelerations
    sim_data <- post_process_df(sim_data)

    # Step 7: Ensure qdd column exists
    sim_data <- ensure_qdd_column(sim_data)

    # Step 8: Add energy, power, and safety metrics
    representative_arcDeg <- tail(sim_data$arcDeg, 1)
    representative_shape <- CircularProfileShape$new(
        R = 5.0, arcDeg = representative_arcDeg, L = 0,
        mu_d = 0.03, mu_s = 0.05, damping = 0, g = 9.81
    )

    sim_data <- add_energy_cols(sim_data, g = representative_shape$g)
    sim_data <- add_power_cols(sim_data)
    sim_data <- add_safety_cols(sim_data, representative_shape)

    # Step 9: Zero out derived variables at gap boundaries and adjacent samples
    indices_to_zero <- get_indices_to_zero(sim_data)
    sim_data <- zero_variables_at_indices(sim_data, indices_to_zero)

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
        data$q_max[artificial_indices] <- 0
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
