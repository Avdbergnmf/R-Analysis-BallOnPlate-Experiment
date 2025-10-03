# Optimized data loading with fast CSV reading using data.table::fread instead of read.csv
# Note: All packages now loaded centrally in setup.R

################ PARAMETER INITIALIZATION ################
# All parameters are now defined in initialization.R for centralized configuration
# Note: ensure_global_data_initialized() is called by functions that need the data
################ Data retrieval / helper methods ################

# New helper: convert trial identifiers such as "T5" to the numeric value 5
sanitize_trial_num <- function(trialNum) {
  as.numeric(gsub("[^0-9]", "", as.character(trialNum)))
}

# Data retrieval functions
get_p_dir <- function(participant) {
  ensure_global_data_initialized()
  return(file.path(dataFolder, participant))
}

get_p_resultsFile <- function(participant) {
  return(file.path(get_p_dir(participant), "trial_results.csv"))
}

# Environment for caching trial result files per participant
.p_results_cache <- new.env(parent = emptyenv())

get_p_results <- function(participant, settingName, trialNumber) {
  # use cached results if available
  cache_key <- paste0(participant)
  if (exists(cache_key, envir = .p_results_cache)) {
    results <- get(cache_key, envir = .p_results_cache)
  } else {
    resultsFile <- get_p_resultsFile(participant)
    results <- as.data.frame(data.table::fread(resultsFile))
    assign(cache_key, results, envir = .p_results_cache)
  }

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

get_trial_phase <- function(participant, trialNum) { # return phase name for trial
  # Use the centralized phase mapping from initialization.R
  ensure_global_data_initialized()
  trial_key <- as.character(trialNum)
  if (trial_key %in% names(default_phases)) {
    return(default_phases[[trial_key]])
  } else {
    warning(sprintf("Unknown trial number: %s", trialNum))
    return("unknown")
  }
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
  details$move_speed <- sapply(participants, get_move_speed, trialNum = 3)
  details$height_meters <- as.numeric(sapply(participants, get_p_detail, detail = "height_scale")) * avatar_height_m

  # -------------------------------------------------------------
  # Calculate summary statistics for each continuous variable
  # -------------------------------------------------------------

  # Calculate statistics for age
  age_stats <- calculate_stats(details$age)
  age_mean_sd <- sprintf("%.2f, SD: %.2f", age_stats$mean, age_stats$sd)
  age_median_min_max_iqr <- sprintf("%d [%.0f, %.0f], IQR=%.2f", age_stats$median, age_stats$min, age_stats$max, age_stats$iqr)

  # Calculate statistics for move speed
  move_speed_stats <- calculate_stats(details$move_speed)
  move_speed_mean_sd <- sprintf("%.2f, SD: %.2f", move_speed_stats$mean, move_speed_stats$sd)
  move_speed_median_min_max_iqr <- sprintf("%.2f [%.2f, %.2f], IQR=%.2f", move_speed_stats$median, move_speed_stats$min, move_speed_stats$max, move_speed_stats$iqr)

  # Calculate statistics for participant height in meters
  height_stats <- calculate_stats(details$height_meters)
  height_mean_sd <- sprintf("%.2f, SD: %.2f", height_stats$mean, height_stats$sd)
  height_median_min_max_iqr <- sprintf("%.2f [%.2f, %.2f], IQR=%.2f", height_stats$median, height_stats$min, height_stats$max, height_stats$iqr)

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
      "Height (Mean, SD)", "Height (Median [Min, Max], IQR)",
      "Weight (Mean, SD)", "Weight (Median [Min, Max], IQR)",
      rep("Gender", length(gender_counts)),
      rep("Education", length(education_counts)),
      rep("VR Experience", length(vr_experience_counts)),
      rep("Motion", length(motion_counts))
    ),
    Value = c(
      age_mean_sd, age_median_min_max_iqr,
      move_speed_mean_sd, move_speed_median_min_max_iqr,
      height_mean_sd, height_median_min_max_iqr,
      weight_mean_sd, weight_median_min_max_iqr,
      as.vector(gender_counts),
      as.vector(education_counts),
      as.vector(vr_experience_counts),
      as.vector(motion_counts)
    )
  )

  # Add the names of the counted categories to the details
  result$Category <- c(
    "", "", "", "", "", "", "", "",
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
  return("control") # neither
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
  trialNum_numeric <- sanitize_trial_num(trialNum)
  filename <- sprintf("%s_T%03d.csv", prefix, trialNum_numeric)
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
  trialNum_numeric <- sanitize_trial_num(trialNum)
  filename <- sprintf("%s_T%03d.csv", prefix, trialNum_numeric)
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

get_start_time <- function(participant, trialNum) {
  # Get UDP time ranges (these are based on trialTime)
  time_ranges <- get_udp_time_ranges(participant, trialNum)
  return(time_ranges$valid_ranges$start_time[1])
}

shift_trial_time_lookup <- function(data, participant, trialNum, time_column = "time") {
  start_time <- get_start_time(participant, trialNum)
  data[[time_column]] <- data[[time_column]] - start_time
  return(data)
}

# get any type of data
get_t_data <- function(participant, trackerType, trialNum, apply_udp_trimming = TRUE) {
  # Check if file exists
  if (!check_file_exists(participant, trackerType, trialNum)) {
    message(sprintf("File does not exist for participant %s, tracker %s, trial %s", participant, trackerType, as.character(trialNum)))
    return(NULL)
  }

  # Get the file prefix from the dictionary
  prefix <- filenameDict[[trackerType]]
  trialNum_numeric <- sanitize_trial_num(trialNum)
  filename <- sprintf("%s_T%03d.csv", prefix, trialNum_numeric)
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

  # Apply UDP time-based trimming if requested and data is available
  if (!is.null(data) && apply_udp_trimming) {
    if (trackerType == "udp") {
      # For UDP dataset itself, propagate errors
      data <- apply_udp_time_trimming(data, participant, trialNum, "time")
    } else {
      # For other datasets, be tolerant: if UDP info missing, keep data untrimmed
      data <- tryCatch(
        apply_udp_time_trimming(data, participant, trialNum, "time"),
        error = function(e) {
          message(sprintf("[WARN] Trimming skipped for %s P%s T%s: %s", trackerType, participant, trialNum, e$message))
          data
        }
      )
    }
  }

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
  return(file.path(get_p_dir(participant), questionnaireInfoFolder, paste0("questionnaireID_", qType, "_ALL_answers.csv")))
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

# Get trial duration using predefined defaults
get_trial_duration <- function(trialNum) {
  trialNum <- sanitize_trial_num(trialNum)
  # Use the centralized duration mapping from initialization.R
  ensure_global_data_initialized()

  trial_key <- as.character(trialNum)

  # Use default duration if available, otherwise fallback to 180s
  if (trial_key %in% names(default_durations)) {
    return(default_durations[[trial_key]])
  } else {
    warning(sprintf("No default duration found for trial %s, using 180s", trialNum))
    return(180)
  }
}

############### UDP time-based trimming info ###############

# Helper to build valid time ranges from UDP data for a specific participant/trial
# Returns a list with valid time ranges that should be kept
get_udp_time_ranges <- function(participant, trialNum) {
  ensure_global_data_initialized()

  # 1. Try global table first --------------------------------------------------
  if (exists("udpTimeTrimInfo", envir = .GlobalEnv)) {
    result <- get_trim_info_from_table(participant, trialNum)
    if (!is.null(result)) {
      return(result)
    }
  }

  # 2. Load UDP dataset --------------------------------------------------------
  if (!check_file_exists(participant, "udp", trialNum)) {
    stop(sprintf("UDP file missing for participant %s trial %s", participant, trialNum))
  }

  prefix <- filenameDict[["udp"]]
  trialNum_numeric <- sanitize_trial_num(trialNum)
  filePath <- file.path(get_p_dir(participant), "trackers", sprintf("%s_T%03d.csv", prefix, trialNum_numeric))

  cols <- c("time", "trialTime", "isError", "startTrial")
  dt <- tryCatch(data.table::fread(filePath, select = cols, showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(dt) || nrow(dt) == 0) {
    stop(sprintf("Empty or unreadable UDP file for participant %s trial %s", participant, trialNum))
  }

  # 3. Cap by prescribed trial duration ---------------------------------------
  duration_cap <- get_trial_duration(trialNum)

  exceed_rows <- which(dt$trialTime > duration_cap)
  if (length(exceed_rows) > 0) {
    end_row <- exceed_rows[1] # include the first exceeding sample
  } else {
    end_row <- which.max(dt$trialTime) # row with the maximum trialTime
  }

  dt <- dt[1:end_row]

  # 4. Identify pause rows (error OR startTrial==0) ----------------------------
  pause_mask <- (dt$isError == 1) | (dt$startTrial == 0)

  # 5. Extract valid segments --------------------------------------------------
  rle_mask <- rle(!pause_mask) # TRUE for valid rows
  idx_end <- cumsum(rle_mask$lengths)
  idx_start <- c(1, head(idx_end, -1) + 1)

  seg_df <- data.frame(start_time = numeric(0), end_time = numeric(0))
  for (i in seq_along(rle_mask$values)) {
    if (rle_mask$values[i]) { # valid segment
      s <- idx_start[i]
      e <- idx_end[i]
      seg_df <- rbind(seg_df, data.frame(start_time = dt$time[s], end_time = dt$time[e]))
    }
  }

  num_segments <- nrow(seg_df)
  if (num_segments == 0) {
    stop(sprintf("No valid segments detected for participant %s trial %s", participant, trialNum))
  }

  # 6. Compute total valid duration -------------------------------------------
  total_valid <- sum(seg_df$end_time - seg_df$start_time)

  list(
    valid_ranges           = seg_df,
    trial_duration         = duration_cap,
    num_segments           = num_segments,
    total_valid_duration   = total_valid,
    has_udp_data           = TRUE
  )
}

# Apply UDP-based time trimming to any dataset with a time column
apply_udp_time_trimming <- function(data, participant, trialNum, time_column = "time") {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }

  # Check if the dataset has the required time column
  if (!time_column %in% colnames(data)) {
    warning(sprintf("Time column '%s' not found in dataset, skipping UDP time trimming", time_column))
    return(data)
  }

  # Get UDP time ranges (these are based on trialTime)
  time_ranges <- get_udp_time_ranges(participant, trialNum)

  # If no UDP data available, apply basic duration cap and normalization
  if (!time_ranges$has_udp_data || nrow(time_ranges$valid_ranges) == 0) {
    # Apply basic duration cap and normalize time to start from 0
    trial_duration <- time_ranges$trial_duration
    minTime <- min(data[[time_column]], na.rm = TRUE)

    # Normalize time to start from 0
    data[[time_column]] <- data[[time_column]] - minTime

    # Apply duration cap
    data <- data[data[[time_column]] <= trial_duration, ]
    return(data)
  }

  # For UDP datasets, we need to handle trialTime vs time differently
  if ("trialTime" %in% colnames(data)) {
    # UDP dataset: use trialTime for filtering, then normalize time column

    # Step 1: Apply time-based filtering using UDP ranges (based on trialTime)
    valid_rows <- rep(FALSE, nrow(data))

    for (i in seq_len(nrow(time_ranges$valid_ranges))) {
      range_start <- time_ranges$valid_ranges$start_time[i]
      range_end <- time_ranges$valid_ranges$end_time[i]

      # Find rows within this time range using trialTime
      in_range <- data$trialTime >= range_start & data$trialTime <= range_end
      valid_rows <- valid_rows | in_range
    }

    # Filter data to valid rows
    filtered_data <- data[valid_rows, ]

    # Step 2: Normalize the time column to start from 0 and cap at trial duration
    if (nrow(filtered_data) > 0) {
      minTime <- min(filtered_data$time, na.rm = TRUE)
      filtered_data$time <- filtered_data$time - minTime

      # Apply duration cap to normalized time
      filtered_data <- filtered_data[filtered_data$time <= time_ranges$trial_duration, ]
    }
  } else {
    # Non-UDP dataset: use time column for filtering and normalization

    # Step 1: Apply time-based filtering using UDP ranges
    valid_rows <- rep(FALSE, nrow(data))

    for (i in seq_len(nrow(time_ranges$valid_ranges))) {
      range_start <- time_ranges$valid_ranges$start_time[i]
      range_end <- time_ranges$valid_ranges$end_time[i]

      # Find rows within this time range using time column
      in_range <- data[[time_column]] >= range_start & data[[time_column]] <= range_end
      valid_rows <- valid_rows | in_range
    }

    # Filter data to valid rows
    filtered_data <- data[valid_rows, ]

    # Step 2: Normalize the time column to start from 0 and cap at trial duration
    if (nrow(filtered_data) > 0) {
      minTime <- min(filtered_data[[time_column]], na.rm = TRUE)
      filtered_data[[time_column]] <- filtered_data[[time_column]] - minTime

      # Apply duration cap to normalized time
      filtered_data <- filtered_data[filtered_data[[time_column]] <= time_ranges$trial_duration, ]
    }
  }

  # --------------------------------------------------------------------------
  #  Final step: make the time column continuous across valid segments --------
  # --------------------------------------------------------------------------
  # Vectorised helper that removes pauses by collapsing large gaps in the
  # timestamp series (after initial min-time normalization).  Any consecutive
  # gap larger than `gap_threshold` seconds is treated as a pause whose full
  # duration is removed from all subsequent samples.
  make_time_continuous <- function(df, time_col, gap_threshold = 1) {
    n <- nrow(df)
    if (n == 0) {
      return(df)
    }

    tvec <- df[[time_col]]
    # Calculate time differences between successive samples
    dt <- c(0, diff(tvec))

    # Identify gaps larger than threshold (likely pauses)
    large_gaps <- pmax(0, dt - gap_threshold)

    # Cumulative pause time up to each row
    cum_pause <- cumsum(large_gaps)

    # Subtract cumulative pause to obtain continuous time
    df[[time_col]] <- tvec - cum_pause
    return(df)
  }

  # Apply continuity fix to the filtered data (time column only)
  filtered_data <- make_time_continuous(filtered_data, time_column)

  # Log trimming results (after continuity fix)
  if (nrow(filtered_data) != nrow(data)) {
    final_time_range <- if (nrow(filtered_data) > 0) {
      sprintf("%.1f-%.1fs", min(filtered_data[[time_column]], na.rm = TRUE), max(filtered_data[[time_column]], na.rm = TRUE))
    } else {
      "empty"
    }

    # Calculate percentage kept
    percent_kept <- (nrow(filtered_data) / nrow(data)) * 100

    # Only print if less than 99.99% of data is kept
    if (FALSE && percent_kept < 99) { # removed this for now.
      cat(sprintf(
        "UDP TRIM APPLIED: %s T%03d | %d → %d rows (%.1f%% kept) | Final time range: %s\n",
        participant, sanitize_trial_num(trialNum), nrow(data), nrow(filtered_data),
        percent_kept, final_time_range
      ))
    }
  }

  return(filtered_data)
}


#--------------------------------------------------------------------
# Wrapper used by load_or_calculate
#--------------------------------------------------------------------
load_or_create_udp_time_trim_info <- function(loop_function) {
  ensure_global_data_initialized()

  if (missing(loop_function) || !is.function(loop_function)) {
    stop("load_or_create_udp_time_trim_info: valid 'loop_function' must be supplied")
  }

  # Build via provided loop_function (can be parallel or sequential)
  build_row <- function(p, tr, ...) {
    rng <- get_udp_time_ranges(p, tr)
    data.frame(
      participant           = p,
      trial                 = as.numeric(tr),
      has_udp_data          = rng$has_udp_data,
      trial_duration        = rng$trial_duration,
      total_valid_duration  = rng$total_valid_duration,
      num_segments          = rng$num_segments,
      valid_ranges          = I(list(rng$valid_ranges)),
      stringsAsFactors      = FALSE
    )
  }

  df <- loop_function(build_row, datasets_to_verify = c("udp"))
  return(df)
}

# Helper function to get trim info from the global udpTimeTrimInfo table
get_trim_info_from_table <- function(participant, trialNum) {
  # If global table not yet built or row missing, return NULL silently; caller will compute on-the-fly.
  if (!exists("udpTimeTrimInfo", envir = .GlobalEnv)) {
    return(NULL)
  }

  trim_row <- udpTimeTrimInfo[udpTimeTrimInfo$participant == participant & udpTimeTrimInfo$trial == trialNum, ]
  if (nrow(trim_row) == 0) {
    return(NULL)
  }

  # Convert back to the expected format
  return(list(
    valid_ranges = trim_row$valid_ranges[[1]],
    trial_duration = trim_row$trial_duration,
    has_udp_data = trim_row$has_udp_data,
    total_valid_duration = trim_row$total_valid_duration,
    num_segments = trim_row$num_segments
  ))
}
