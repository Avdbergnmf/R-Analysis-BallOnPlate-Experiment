####### DEFINITIONS
# Getting types for later use
xOptions <- c("time", "pos_x", "pos_y", "pos_z", "actual_pos_z")
xOptions2D <- colnames(get_t_data(participants[1], "leftfoot", 1)) # options for pos rot trackers
categories <- c("participant", "condition", "trialNum") #  "heelStrikes.foot"
# categoriesInputs <- append(categories, "None")
columns_to_not_summarize <- c("visualizations", "perturbations") # these are categorical columns we may want to use for our statistics but we dont want to summarize in our mu table
categoriesExtra <- c(categories, columns_to_not_summarize)
categoriesExtraInputs <- append(categoriesExtra, c("heelStrikes.foot", "slice_index", "None"))

getTypes <- function(dt) {
  numericDataTypes <- sapply(dt, is.numeric)
  logicalDataTypes <- sapply(dt, is.logical)
  dataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes])
}

############ FUNCTIONS
# Helper function to load or calculate and save data
load_or_calculate <- function(filePath, calculate_function) {
  if (file.exists(filePath)) {
    data <- readRDS(filePath)
  } else {
    data <- calculate_function()
    saveRDS(data, filePath)
  }
  return(data)
}

add_identifiers_and_categories <- function(data, participant, trial) {
  data <- add_identifiers(data, participant, trial)
  data <- add_category_columns(data)
  return(data)
}

add_identifiers <- function(data, participant, trial) {
  data$participant <- as.factor(participant)
  data$trialNum <- as.ordered(trial)
  return(data)
}

add_category_columns <- function(data) {
  trial <- as.numeric(as.character(data$trialNum))
  participant <- as.character(data$participant)

  # Add categorical columns
  data$perturbations <- as.factor(mapply(has_perturbations, participant, trial))
  data$visualizations <- as.factor(mapply(has_visualizations, participant, trial))
  data$task <- as.factor(mapply(has_task, participant, trial))
  data$treadmillSpeed <- as.numeric(mapply(get_move_speed, participant))

  data$condition <- as.factor(sapply(participant, condition_number))

  return(data)
}

get_data_from_loop_parallel <- function(get_data_function, ...) {
  # Create a data frame of all participant and trial combinations
  combinations <- expand.grid(participant = participants, trial = allTrials)

  # Set up parallel backend to use multiple processors
  numCores <- detectCores() - 1 # Leave one core free for system processes
  cl <- makeCluster(numCores)
  registerDoParallel(cl)

  # Export necessary variables and functions to the cluster
  clusterExport(cl, c("participants", "allTrials", "get_data_function", "add_identifiers_and_categories"), envir = environment())

  # Run the loop in parallel using foreach
  data_list <- foreach(
    i = 1:nrow(combinations),
    .combine = rbind,
    .packages = c() # "data.table", "readxl", "dplyr", "purrr", "jsonlite", "signal", "zoo", "" ... just did whole setup.R for now
  ) %dopar% {
    # Source the scripts containing your functions
    source("source/setup.R", local = FALSE)
    source("source/data_loading.R", local = FALSE)
    source("source/pre_processing.R", local = FALSE)
    source("source/find_foot_events.R", local = FALSE)
    source("source/calc_all_gait_params.R", local = FALSE)

    # Extract participant and trial for this iteration
    participant <- combinations$participant[i]
    trial <- combinations$trial[i]

    # Calculate gait data and parameters with optional arguments
    newData <- get_data_function(participant, trial, ...)
    newData <- add_identifiers_and_categories(as.data.frame(newData), participant, trial)
    newData # Return the new data frame
  }

  # Stop the cluster
  stopCluster(cl)

  # Combine the list of data frames into one data frame
  data <- as.data.frame(data_list)

  return(data)
}

get_data_from_loop <- function(get_data_function, ...) {
  # Initialize an empty data frame
  data <- data.frame()

  # Variables for progress bar
  total_iterations <- length(participants) * length(allTrials)
  current_iteration <- 0

  for (participant in participants) {
    print(paste("Participant:", participant))

    for (trial in allTrials) {
      # PRINT PROGRESS BAR
      current_iteration <- current_iteration + 1 # Increment the iteration count
      progress_percent <- (current_iteration / total_iterations) * 100 # Calculate the percentage of progress
      progress_bar_length <- 50 # Length of the progress bar
      num_hashes <- floor(progress_bar_length * progress_percent / 100)
      num_dashes <- progress_bar_length - num_hashes
      progress_bar <- paste0("[", paste(rep("#", num_hashes), collapse = ""), paste(rep("-", num_dashes), collapse = ""), "]")

      # Print progress bar with the percentage
      cat(sprintf("\rProgress: %s %.2f%% On Participant: %s, Trial: %s\n", progress_bar, progress_percent, participant, trial))
      flush.console() # Ensure the console updates immediately

      # Calculate gait data and parameters
      newData <- get_data_function(participant, trial, ...)
      newData <- add_identifiers_and_categories(as.data.frame(newData), participant, trial)
      data <- rbind(data, newData) # Bind this participant's gait parameters to the overall data frame
    }
  }

  # Print final progress bar to indicate completion
  cat("\n")

  return(data)
}


calc_all_gait_params <- function() {
  return(get_data_from_loop(calculate_gait_parameters)) # _parallel
}
