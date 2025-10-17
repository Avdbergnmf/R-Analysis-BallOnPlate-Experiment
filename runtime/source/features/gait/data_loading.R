#' Gait Data Loading Functions
#'
#' Functions for loading rotation data and other preprocessing data

#' Get rotations data from file
#' @return Dataframe with rotation data or empty vector if file doesn't exist
get_rotations_data <- function() {
  rotations_file <- file.path(dataExtraFolder, rotationsFile) # manually created file with participant, trial, and rotation (in degrees)
  rotations <- c()
  if (file.exists(rotations_file)) {
    rotations <- as.data.frame(data.table::fread(rotations_file))
  }
  return(rotations)
}

#' Load rotations for a specific participant and trial
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param rotations_data Dataframe with rotation data (passed as parameter)
#' @return Vector of rotations or empty vector if not found
load_rotations <- function(participant, trialNum, rotations_data) {
  if (is.null(rotations_data) || length(rotations_data) == 0) {
    return(c())
  }

  rotations <- rotations_data[rotations_data$participant == participant & rotations_data$trial == trialNum, "rotation"]

  if (length(rotations) == 0) { # If not found, look for any rotation for this participant
    rotations <- rotations_data[rotations_data$participant == participant, "rotation"]
  }
  return(rotations)
}
