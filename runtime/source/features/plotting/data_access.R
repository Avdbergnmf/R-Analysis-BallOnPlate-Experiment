#' Plotting Data Access Functions
#'
#' Functions for accessing and preprocessing tracker data for plotting.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

#' Preprocess tracker data (apply rotations if kinematic data)
#' @param data Raw tracker data
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param dataName Data type name (e.g., "leftfoot", "rightfoot", "hip")
#' @return Preprocessed dataframe
preprocess_tracker_data <- function(data, participant, trialNum, dataName) {
  plotting_logger("DEBUG", sprintf("Preprocessing data for participant %s, trial %s, dataName %s", participant, trialNum, dataName))
  
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
