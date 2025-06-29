# Optimized data loading with fast CSV reading using data.table::fread instead of read.csv
# Note: All packages now loaded centrally in setup.R

################ PARAMETER INITIALIZATION ################
# All parameters are now defined in initialization.R for centralized configuration
# Note: ensure_global_data_initialized() is called by functions that need the data
################ Data retrieval / helper methods ################

# Data retrieval functions
get_p_dir <- function(participant) {
  ensure_global_data_initialized()
  return(file.path(dataFolder, participant))
}

get_p_resultsFile <- function(participant) {
  return(file.path(get_p_dir(participant), "trial_results.csv"))
}

get_p_results <- function(participant, settingName, trialNumber) {
  # get the path to the settings file for the participant
  resultsFile <- get_p_resultsFile(participant)
  # Optimized CSV reading: use fread instead of read.csv for better performance
  results <- as.data.frame(data.table::fread(resultsFile))

  # find the row where trial_num matches the requested trialNumber
  row_index <- which(results$trial_num == trialNumber)

  if (length(row_index) == 0) {
    warning(sprintf("No trial found with trial_num = %s for participant %s", trialNumber, participant))
    return(NA)
  }

  if (length(row_index) > 1) {
    warning(sprintf("Multiple trials found with trial_num = %s for participant %s, using first match", trialNumber, participant))
    row_index <- row_index[1]
  }

  # retrieve the value of the specific detail using the found row index
  resultValue <- results[[settingName]][row_index]

  return(resultValue)
}

get_move_speed <- function(participant, trialNum) { # return move speed in m/s
  return(get_p_results(participant, "move_speed", trialNum))
}

get_p_detail <- function(participant, detail) {
  # get the path to the details file for the participant
  detailsFile <- file.path(get_p_dir(participant), "session_info/participant_details.csv")

  # Optimized CSV reading: use fread instead of read.csv for better performance
  details <- as.data.frame(data.table::fread(detailsFile))

  # retrieve the value of the specific detail
  detailValue <- details[[detail]][1]

  return(detailValue)
}

calculate_stats <- function(x) {
  x <- as.numeric(x)
  stats <- list(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE)
  )
  return(stats)
}

calculate_participant_details <- function(participants) {
  details <- data.frame(participant = participants)

  # Retrieve details for each participant
  details$age <- sapply(participants, get_p_detail, detail = "age")
  details$gender <- sapply(participants, get_p_detail, detail = "gender")
  details$weight <- sapply(participants, get_p_detail, detail = "weight")
  details$education <- sapply(participants, get_p_detail, detail = "education")
  details$vr_experience <- sapply(participants, get_p_detail, detail = "vr_experience")
  details$motion <- sapply(participants, get_p_detail, detail = "motion")
  details$move_speed <- sapply(participants, get_move_speed, trialNum = 2)

  # Calculate statistics for age
  age_stats <- calculate_stats(details$age)
  age_mean_sd <- sprintf("%.2f, SD: %.2f", age_stats$mean, age_stats$sd)
  age_median_min_max_iqr <- sprintf("%d [%.0f, %.0f], IQR=%.2f", age_stats$median, age_stats$min, age_stats$max, age_stats$iqr)

  # Calculate statistics for move speed
  move_speed_stats <- calculate_stats(details$move_speed)
  move_speed_mean_sd <- sprintf("%.2f, SD: %.2f", move_speed_stats$mean, move_speed_stats$sd)
  move_speed_median_min_max_iqr <- sprintf("%.2f [%.2f, %.2f], IQR=%.2f", move_speed_stats$median, move_speed_stats$min, move_speed_stats$max, move_speed_stats$iqr)

  # Calculate statistics for weight
  weight_stats <- calculate_stats(details$weight)
  weight_mean_sd <- sprintf("%.2f, SD: %.2f", weight_stats$mean, weight_stats$sd)
  weight_median_min_max_iqr <- sprintf("%.2f [%.2f, %.2f], IQR=%.2f", weight_stats$median, weight_stats$min, weight_stats$max, weight_stats$iqr)

  # Count the occurrences of each unique value in other columns
  gender_counts <- table(details$gender)
  education_counts <- table(details$education)
  vr_experience_counts <- table(details$vr_experience)
  motion_counts <- table(details$motion)

  # Create a result data frame
  result <- data.frame(
    Detail = c(
      "Age (Mean, SD)", "Age (Median [Min, Max], IQR)",
      "Move Speed (Mean, SD)", "Move Speed (Median [Min, Max], IQR)",
      "Weight (Mean, SD)", "Weight (Median [Min, Max], IQR)",
      rep("Gender", length(gender_counts)),
      rep("Education", length(education_counts)),
      rep("VR Experience", length(vr_experience_counts)),
      rep("Motion", length(motion_counts))
    ),
    Value = c(
      age_mean_sd, age_median_min_max_iqr,
      move_speed_mean_sd, move_speed_median_min_max_iqr,
      weight_mean_sd, weight_median_min_max_iqr,
      as.vector(gender_counts),
      as.vector(education_counts),
      as.vector(vr_experience_counts),
      as.vector(motion_counts)
    )
  )

  # Add the names of the counted categories to the details
  result$Category <- c(
    "", "", "", "", "", "",
    names(gender_counts),
    names(education_counts),
    names(vr_experience_counts),
    names(motion_counts)
  )

  return(result)
}

# does the trial have VFD
has_perturbations <- function(participant, trial) {
  result <- get_p_results(participant, "perturbations", trial)
  return(result == "True" || result == TRUE || result == "true")
}

has_visualizations <- function(participant, trial) {
  result <- get_p_results(participant, "visualizations", trial)
  return(result == "True" || result == TRUE || result == "true")
}

has_task <- function(participant, trial) {
  result <- get_p_results(participant, "task", trial)
  return(result == "True" || result == TRUE || result == "true")
}

condition_number <- function(participant) {
  # check what state the training was in
  p <- has_perturbations(participant, 7)
  v <- has_visualizations(participant, 7)

  if (p && v) {
    return("perturbation_visualization") # vis and pert
  }
  if (p) {
    return("perturbation") # pert
  }
  return("baseline") # neither
}

has_simulation_data <- function(participant, trial) {
  return(check_file_exists(participant, "sim", trial))
}

# Helper function to check if a file exists for a given participant, tracker type, and trial
check_file_exists <- function(participant, trackerType, trialNum) {
  ensure_global_data_initialized()
  # Validate trackerType
  if (!trackerType %in% names(filenameDict)) {
    warning(sprintf("Invalid tracker type specified: %s", trackerType))
    return(FALSE)
  }

  # Get the file prefix from the dictionary
  prefix <- filenameDict[[trackerType]]
  filename <- paste0(prefix, "_T", sprintf("%03d", as.numeric(trialNum)), ".csv")
  filePath <- file.path(get_p_dir(participant), "trackers", filename)

  return(file.exists(filePath))
}

# Helper function to check if a file has data (only use when necessary)
check_file_has_data <- function(participant, trackerType, trialNum) {
  # Validate trackerType
  if (!trackerType %in% names(filenameDict)) {
    warning(sprintf("Invalid tracker type specified: %s", trackerType))
    return(FALSE)
  }

  # Get the file prefix from the dictionary
  prefix <- filenameDict[[trackerType]]
  filename <- paste0(prefix, "_T", sprintf("%03d", as.numeric(trialNum)), ".csv")
  filePath <- file.path(get_p_dir(participant), "trackers", filename)

  if (!file.exists(filePath)) {
    return(FALSE)
  }

  tryCatch(
    {
      # Performance optimization: use file size check instead of reading entire CSV
      # Files with only headers are typically < 100 bytes, data files are much larger
      file_size <- file.info(filePath)$size
      if (file_size > 100) {
        return(TRUE) # Likely has data beyond headers
      }

      # For very small files, do a minimal check by reading just the first few lines
      # This is much faster than reading the entire file
      first_lines <- readLines(filePath, n = 3, warn = FALSE)
      return(length(first_lines) > 1) # Header + at least one data row
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

# get any type of data
get_t_data <- function(participant, trackerType, trialNum) {
  # Check if file exists
  if (!check_file_exists(participant, trackerType, trialNum)) {
    message(sprintf("File does not exist for participant %s, tracker %s, trial %d", participant, trackerType, trialNum))
    return(NULL)
  }

  # Get the file prefix from the dictionary
  prefix <- filenameDict[[trackerType]]
  filename <- paste0(prefix, "_T", sprintf("%03d", as.numeric(trialNum)), ".csv")
  filePath <- file.path(get_p_dir(participant), "trackers", filename)

  # Use tryCatch for more robust error handling
  data <- tryCatch(
    {
      # Optimized CSV reading: use fread instead of read.csv for better performance
      as.data.frame(data.table::fread(filePath))
    },
    error = function(e) {
      message("Failed to read the file: ", filePath, " - ", e$message)
      return(NULL)
    }
  )

  return(data)
}

# For changing up the selection inputs - bit costly, but not really needed to optimize this
getOptions <- function(tracker, trialNum = 5) { # trial 5 is always available - except for haptic...
  ensure_global_data_initialized()
  exampleData <- get_t_data(participants[1], tracker, trialNum)
  # Fail gracefully if exampleData is empty or NULL
  if (is.null(exampleData) || nrow(exampleData) == 0) {
    return(c("data not available"))
  }
  numericTypes <- sapply(exampleData, is.numeric)
  numeric_cols <- names(exampleData[numericTypes])
  return(numeric_cols)
}

get_q_file <- function(participant, qType) { # qType = IMI / SSQ / VEQ
  ensure_global_data_initialized()
  return(file.path(get_p_dir(participant), "Questionnaires", paste0("questionnaireID_", qType, "_ALL_answers.csv")))
}

################ Questionnaires ################

get_q_data <- function(participant, qType) {
  # Get the path to the questionnaire file for the participant
  questionnaireFile <- get_q_file(participant, qType)

  # Optimized CSV reading: use fread instead of read.csv for better performance
  questionnaire <- as.data.frame(data.table::fread(questionnaireFile))

  # Extract the QuestionID and answer columns
  result <- questionnaire[, c("QuestionID", qAnswers)]

  return(result)
}

# Caching environment for questionnaire metadata to avoid repeated CSV loading
.questionnaire_cache <- new.env(parent = emptyenv())

# Cached questionnaire info loading - avoids repeated CSV reads for better performance
get_question_info <- function(qType) { # qType = IMI / SSQ / VEQ
  ensure_global_data_initialized()
  cache_key <- paste0("info_", qType)

  # Return cached data if available
  if (exists(cache_key, envir = .questionnaire_cache)) {
    return(get(cache_key, envir = .questionnaire_cache))
  }

  # Load and cache if not present
  qInfopath <- file.path(questionnaireInfoFolder, paste0(qType, ".csv"))
  questionnaire <- as.data.frame(data.table::fread(qInfopath))
  assign(cache_key, questionnaire, envir = .questionnaire_cache)

  return(questionnaire)
}

# Cached questionnaire weights loading - avoids repeated CSV reads for better performance
get_question_weights <- function(qType) { # qType = IMI / UserExperience
  ensure_global_data_initialized()
  cache_key <- paste0("weights_", qType)

  # Return cached data if available
  if (exists(cache_key, envir = .questionnaire_cache)) {
    return(get(cache_key, envir = .questionnaire_cache))
  }

  # Load and cache if not present
  qpath <- file.path(questionnaireInfoFolder, paste0(qType, "_weights.csv"))
  questionnaire <- as.data.frame(data.table::fread(qpath))
  assign(cache_key, questionnaire, envir = .questionnaire_cache)

  return(questionnaire)
}
