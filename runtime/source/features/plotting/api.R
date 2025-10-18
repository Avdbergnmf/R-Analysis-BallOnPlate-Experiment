#' Plotting Feature Module API
#'
#' Self-contained plotting functions for the BallOnPlate experiment.
#' This module provides plotting capabilities without depending on other feature modules.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

# =============================================================================
# DATA ACCESS FUNCTIONS
# =============================================================================

# Note: get_tracker_data function removed - use get_t_data from data_loading.R instead

#' Preprocess tracker data (apply rotations if kinematic data)
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param dataName Data type name (e.g., "leftfoot", "rightfoot", "hip")
#' @return Preprocessed dataframe
preprocess_tracker_data <- function(participant, trialNum, dataName) {
  plotting_logger("DEBUG", sprintf("Preprocessing data for participant %s, trial %s, dataName %s", participant, trialNum, dataName))
  
  data <- get_t_data(participant, dataName, trialNum)

  # Check if data is empty
  if (is.null(data) || nrow(data) == 0) {
    plotting_logger("WARN", sprintf("No data found for participant %s, trial %s, dataName %s", participant, trialNum, dataName))
    return(data.frame())
  }

  # Check if data contains kinematic columns that can be rotated
  if (is_kinematic_data(data)) {
    # Get rotations data from global variable (loaded in initialization.R)
    if (exists("rotations_data") && !is.null(rotations_data)) {
      rotation_info <- rotations_data[[participant]][[as.character(trialNum)]]
      if (!is.null(rotation_info) && !is.null(rotation_info[[dataName]])) {
        theta_deg <- rotation_info[[dataName]]
        data <- rotate_y(data, theta_deg)
        plotting_logger("DEBUG", sprintf("Applied rotation of %f degrees to %s data", theta_deg, dataName))
      }
    }
  }

  return(data)
}

#' Check if data contains kinematic columns that can be rotated
#' @param data Dataframe to check
#' @return TRUE if data has kinematic columns
is_kinematic_data <- function(data) {
  # Check if the data has the required position columns for rotation
  required_pos_cols <- c("pos_x", "pos_y", "pos_z")
  has_position_data <- all(required_pos_cols %in% colnames(data))
  
  return(has_position_data)
}

#' Rotate data around Y-axis
#' @param data Dataframe with position and rotation columns
#' @param theta_deg Rotation angle in degrees
#' @return Dataframe with rotated positions and orientations
rotate_y <- function(data, theta_deg) {
  theta <- theta_deg * pi / 180 # convert to radians
  # Create the rotation matrix
  rotation_matrix <- matrix(
    c(
      cos(theta), 0, sin(theta),
      0, 1, 0,
      -sin(theta), 0, cos(theta)
    ),
    nrow = 3, byrow = TRUE
  )

  # Apply the rotation to position columns
  positions <- as.matrix(data[, c("pos_x", "pos_y", "pos_z")])
  rotated_positions <- positions %*% t(rotation_matrix)

  # Update the dataframe with rotated positions
  data$pos_x <- rotated_positions[, 1]
  data$pos_y <- rotated_positions[, 2]
  data$pos_z <- rotated_positions[, 3]

  # Optionally, if you need to rotate orientation vectors (rot_x, rot_y, rot_z)
  if (all(c("rot_x", "rot_y", "rot_z") %in% colnames(data))) {
    rotations <- as.matrix(data[, c("rot_x", "rot_y", "rot_z")])
    rotated_rotations <- rotations %*% t(rotation_matrix)

    data$rot_x <- rotated_rotations[, 1]
    data$rot_y <- rotated_rotations[, 2]
    data$rot_z <- rotated_rotations[, 3]
  }

  return(data)
}

# =============================================================================
# PLOTTING FUNCTIONS
# =============================================================================

#' Create error plot with message
#' @param message Error message to display
#' @param baseSize Base font size
#' @return ggplot object with error message
create_error_plot <- function(message, baseSize = 10) {
  plotting_logger("WARN", sprintf("Creating error plot: %s", message))
  
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = message, size = baseSize, hjust = 0.5, vjust = 0.5) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "lightgray", color = "red", size = 2))
  
  return(p)
}

#' Validate data and columns for plotting
#' @param xData X-axis data
#' @param yData Y-axis data
#' @param xtracker X tracker name
#' @param ytracker Y tracker name
#' @param x_axis X-axis column name
#' @param y_axis Y-axis column name
#' @param participant Participant ID
#' @param trialNum Trial number
#' @param baseSize Base font size
#' @return Error plot if validation fails, NULL if validation passes
validate_data_and_columns <- function(xData, yData, xtracker, ytracker, x_axis, y_axis, participant, trialNum, baseSize) {
  # Check if data is empty
  if (is.null(xData) || nrow(xData) == 0) {
    return(create_error_plot(paste("No data available for", xtracker, "in participant", participant, "trial", trialNum), baseSize))
  }

  if (is.null(yData) || nrow(yData) == 0) {
    return(create_error_plot(paste("No data available for", ytracker, "in participant", participant, "trial", trialNum), baseSize))
  }

  # Check if requested columns exist
  if (!x_axis %in% colnames(xData)) {
    return(create_error_plot(paste("Column", x_axis, "not found in", xtracker, "data"), baseSize))
  }

  if (!y_axis %in% colnames(yData)) {
    return(create_error_plot(paste("Column", y_axis, "not found in", ytracker, "data"), baseSize))
  }

  return(NULL) # No errors
}

#' Create 2D plot of tracker data
#' @param xtracker X-axis tracker name
#' @param ytracker Y-axis tracker name
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param x_axis X-axis column name
#' @param y_axis Y-axis column name
#' @param plotlines Whether to plot lines
#' @param plotpoints Whether to plot points
#' @param extraTitle Additional title text
#' @param baseSize Base font size
#' @return ggplot object
plot_2d <- function(xtracker, ytracker, participant, trialNum, x_axis = "time", y_axis = "pos_z", plotlines = TRUE, plotpoints = FALSE, extraTitle = "", baseSize = 10) {
  plotting_logger("DEBUG", "Starting plot_2d function")
  plotting_logger("DEBUG", sprintf("Input parameters - xtracker: %s ytracker: %s participant: %s trialNum: %s x_axis: %s y_axis: %s", xtracker, ytracker, participant, trialNum, x_axis, y_axis))

  # Load data for both trackers
  xData <- preprocess_tracker_data(participant, trialNum, xtracker)
  yData <- preprocess_tracker_data(participant, trialNum, ytracker)

  # Validate data and columns - return error plot if validation fails
  error_plot <- validate_data_and_columns(xData, yData, xtracker, ytracker, x_axis, y_axis, participant, trialNum, baseSize)
  if (!is.null(error_plot)) {
    return(error_plot)
  }

  # Create the plot
  p <- ggplot(xData, aes(.data[[x_axis]], .data[[y_axis]])) +
    labs(x = x_axis, y = y_axis) +
    theme_minimal(base_size = baseSize) +
    ggtitle(paste(extraTitle, ",", x_axis, "vs.", y_axis))

  # Add geometries based on options
  if (plotlines) {
    p <- p + geom_path()
  }

  if (plotpoints) {
    p <- p + geom_point()
  }

  # Set equal coordinates for position plots
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }

  plotting_logger("DEBUG", "plot_2d function completed successfully")
  return(p)
}

plotting_logger("INFO", "Plotting feature module API loaded successfully")
