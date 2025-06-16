# ─────────────────────────────────────────────────────────────────────────────
# Simulation Data Processing
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(tibble)


#' Get list of base parameters to summarize
#'
#' @return List of parameter names to summarize
#' @export
get_simulation_parameters <- function() {
    list(
        # Position parameters
        q = "Arc-length Position",
        p = "Plate Position",

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

        # Coordinate parameters
        x = "X Position (plate-relative)",
        x_world = "X Position (world)",
        y = "Y Position (height)"
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

#' Get simulation data for a specific participant and trial
#'
#' @param participant Participant identifier
#' @param trial Trial number
#' @param trials Optional list of trials for consistent factor creation
#' @return Tibble with simulation data or empty tibble if no data found
#' @export
get_simulation_data <- function(participant, trial, trials = NULL) {
    # Load raw simulation data
    sim_data <- load_simulation_data(participant, trial)
    if (is.null(sim_data) || nrow(sim_data) == 0) {
        return(tibble())
    }

    # Filter out non-simulation steps
    sim_data <- sim_data %>%
        filter(dt > 0)

    if (nrow(sim_data) == 0) {
        return(tibble())
    }

    # Get level data for arcDeg
    level_data <- get_level_data(participant, trial)
    if (is.null(level_data) || nrow(level_data) == 0) {
        warning(sprintf("No level data found for participant %s, trial %d", participant, trial))
        return(tibble())
    }

    # Process the simulation data
    sim_data <- process_simulation_data(sim_data, level_data)

    # Add identifiers
    sim_data <- sim_data %>%
        mutate(
            participant = factor(participant, levels = if (!is.null(trials)) unique(trials$participant) else NULL),
            trialNum = factor(trial, levels = if (!is.null(trials)) unique(trials$trialNum) else NULL)
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
    # Calculate coordinates
    sim_data <- calculate_coordinates(sim_data)

    # Add simulation time
    sim_data <- add_simulation_time(sim_data)

    # Ensure qdd column exists
    sim_data <- ensure_qdd_column(sim_data)

    # Get indices to zero
    indices_to_zero <- get_indices_to_zero(sim_data)

    # Zero variables at respawn points
    sim_data <- zero_variables_at_indices(sim_data, indices_to_zero)

    # Assign arc degrees
    sim_data <- assign_arc_degrees(sim_data, level_data)

    # Add energy metrics
    sim_data <- add_energy_metrics(sim_data)

    # Add safety metrics
    sim_data <- add_safety_metrics(sim_data)

    # Add power metrics
    sim_data <- add_power_metrics(sim_data)

    # Add work metrics
    sim_data <- add_work_metrics(sim_data)

    return(sim_data)
}

#' Calculate coordinates from simulation data
#'
#' @param sim_data Simulation data
#' @return Simulation data with calculated coordinates
#' @export
calculate_coordinates <- function(sim_data) {
    sim_data %>%
        mutate(
            x = R * sin(q),
            y = R * (1 - cos(q)),
            x_world = x + p
        )
}

#' Add simulation time to data
#'
#' @param sim_data Simulation data
#' @return Simulation data with added simulation time
#' @export
add_simulation_time <- function(sim_data) {
    sim_data %>%
        mutate(simulation_time = cumsum(dt))
}

#' Ensure qdd column exists
#'
#' @param sim_data Simulation data
#' @return Simulation data with qdd column
#' @export
ensure_qdd_column <- function(sim_data) {
    if (!"qdd" %in% colnames(sim_data)) {
        sim_data$qdd <- 0
    }
    return(sim_data)
}

#' Get indices to zero at respawn points
#'
#' @param sim_data Simulation data
#' @return Vector of indices to zero
#' @export
get_indices_to_zero <- function(sim_data) {
    # Find respawn points (where y_escape equals R)
    respawn_indices <- which(sim_data$y_escape == sim_data$R)

    # Add one to each index to get the point after respawn
    respawn_indices + 1
}

#' Zero variables at specified indices
#'
#' @param sim_data Simulation data
#' @param indices Indices to zero
#' @return Simulation data with zeroed variables
#' @export
zero_variables_at_indices <- function(sim_data, indices) {
    # Zero velocity and acceleration at respawn points
    sim_data$qd[indices] <- 0
    sim_data$qdd[indices] <- 0

    return(sim_data)
}

#' Assign arc degrees to simulation data
#'
#' @param sim_data Simulation data
#' @param level_data Level data
#' @return Simulation data with assigned arc degrees
#' @export
assign_arc_degrees <- function(sim_data, level_data) {
    # Find level indices based on simulation time
    level_indices <- findInterval(sim_data$time, level_data$time, rightmost.closed = TRUE)

    # Assign arc degrees
    sim_data$arcDeg <- level_data$deg[level_indices]

    return(sim_data)
}

#' Add energy metrics to simulation data
#'
#' @param sim_data Simulation data
#' @return Simulation data with added energy metrics
#' @export
add_energy_metrics <- function(sim_data) {
    sim_data %>%
        mutate(
            # Kinetic energy (plate-relative)
            ke = 0.5 * m * qd^2,

            # Kinetic energy (world)
            ke_world = 0.5 * m * (qd^2 + pd^2),

            # Potential energy
            pe = m * g * y,

            # Total energy (plate-relative)
            e = ke + pe,

            # Total energy (world)
            e_world = ke_world + pe
        )
}

#' Add safety metrics to simulation data
#'
#' @param sim_data Simulation data
#' @return Simulation data with added safety metrics
#' @export
add_safety_metrics <- function(sim_data) {
    sim_data %>%
        mutate(
            # Energy margin (difference between current energy and escape energy)
            margin_E = e_world - m * g * R,

            # Danger level (normalized energy margin)
            danger = margin_E / (m * g * R)
        )
}

#' Add power metrics to simulation data
#'
#' @param sim_data Simulation data
#' @return Simulation data with added power metrics
#' @export
add_power_metrics <- function(sim_data) {
    sim_data %>%
        mutate(
            # Power (plate-relative)
            power = m * g * qd * sin(q),

            # Power (world)
            power_world = m * g * (qd * sin(q) + pd * cos(q))
        )
}

#' Add work metrics to simulation data
#'
#' @param sim_data Simulation data
#' @return Simulation data with added work metrics
#' @export
add_work_metrics <- function(sim_data) {
    sim_data %>%
        mutate(
            # Work (plate-relative)
            work = cumsum(power * dt),

            # Work (world)
            work_world = cumsum(power_world * dt)
        )
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
