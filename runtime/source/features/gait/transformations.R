#' Gait Data Transformation Functions
#'
#' Functions for transforming and filtering gait data

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
rotate_y <- function(data, theta_deg) { # THETA IN DEGREES!
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
  rotations <- as.matrix(data[, c("rot_x", "rot_y", "rot_z")])
  rotated_rotations <- rotations %*% t(rotation_matrix)

  data$rot_x <- rotated_rotations[, 1]
  data$rot_y <- rotated_rotations[, 2]
  data$rot_z <- rotated_rotations[, 3]

  return(data)
}

#' Apply padding and Butterworth filter to a data column
#' @param column Data column to filter
#' @param poly_order Polynomial order for Butterworth filter
#' @param fs Sampling frequency
#' @param cutoff_freq Cutoff frequency for filter
#' @return Filtered column
apply_padding_and_filter <- function(column, poly_order, fs, cutoff_freq = 5) {
  # Use vectorized pracma::hampel instead of custom for-loop for much better performance
  column <- pracma::hampel(column, k = 7, t0 = 3)$y

  # Calculate the number of points to pad (half the frame size generally works well)
  pad_width <- 20

  # Create mirrored padding
  padding_start <- rev(column[1:pad_width])
  padding_end <- rev(column[(length(column) - pad_width + 1):length(column)])

  # Pad the column
  padded_column <- c(padding_start, column, padding_end)

  # Apply Butterworth filter to the padded data
  b <- butter(poly_order, cutoff_freq / (fs / 2)) # 4th order Butterworth filter
  filtered_column <- filtfilt(b, padded_column)

  # Remove the padding
  filtered_column <- filtered_column[(pad_width + 1):(length(filtered_column) - pad_width)]

  return(filtered_column)
}
