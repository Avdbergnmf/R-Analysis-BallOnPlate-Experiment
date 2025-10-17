#' Gait Data Preprocessing Functions
#'
#' Main preprocessing pipeline for gait data

#' Preprocess data for a specific participant, trial, and data type
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param dataName Data type name (e.g., "leftfoot", "rightfoot", "hip")
#' @return Preprocessed dataframe
preprocess_data <- function(participant, trialNum, dataName) {
  data <- get_t_data(participant, dataName, trialNum)

  # Check if data is empty
  if (is.null(data) || nrow(data) == 0) {
    message(sprintf("preprocess_data: No data found for participant %s, trial %s, dataName %s. Returning empty data frame.", participant, trialNum, dataName))
    return(data.frame())
  }

  if (is_kinematic_data(data)) {
    rotation <- load_rotations(participant, trialNum)
    if (length(rotation) > 0) { # If rotation is found, apply it
      rotation <- rotation[1] # Take the first rotation if multiple are found
      data <- rotate_y(data, rotation)
    }
    moveSpeed <- get_move_speed(participant, trialNum)
    data$actual_pos_z <- data$pos_z + moveSpeed * data$time
  }

  return(data)
}

#' Get preprocessed data for multiple data types
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param dataList List of data types to preprocess
#' @return List of preprocessed dataframes
get_preprocessed_data <- function(participant, trialNum, dataList = c("leftfoot", "rightfoot", "hip")) {
  result <- list()
  for (dataName in dataList) {
    # Use a more descriptive name for each element in the list
    # e.g., "leftFoot", "rightFoot", "hip", etc.
    result[[dataName]] <- preprocess_data(participant, trialNum, dataName)
  }
  return(result)
}
