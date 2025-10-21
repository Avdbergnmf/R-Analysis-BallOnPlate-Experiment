sanitize_trial_num <- function(trialNum) {
  as.numeric(gsub("[^0-9]", "", as.character(trialNum)))
}

tracker_cache_logger <- create_module_logger("TRACKER-CACHE")
dataset_cache_logger <- create_module_logger("DATASET-CACHE")

get_tracker_cache_key <- function(participant, trialNum, trackerType, apply_udp_trimming = TRUE) {
  sprintf("%s|%03d|%s|trim:%s", participant, sanitize_trial_num(trialNum), trackerType, ifelse(apply_udp_trimming, "1", "0"))
}

get_simulation_cache_key <- function(participant, trial) {
  sprintf("%s|%03d|sim", participant, sanitize_trial_num(trial))
}

#' Convert condition names to prettier labels for plotting
#' @param conditions Vector of condition names
#' @return Vector of pretty condition labels
get_pretty_condition_labels <- function(conditions) {
  # Use the global condition mapping from initialization.R
  ensure_global_data_initialized()
  
  # Apply the mapping, keeping original names if not found in map
  pretty_labels <- ifelse(conditions %in% names(condition_map),
    condition_map[conditions],
    conditions
  )
  
  return(pretty_labels)
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
.p_results_cache <- new.env(parent = baseenv())

get_p_results <- function(participant, settingName, trialNumber) {
  # use cached results if available
  cache_key <- paste0(participant)
  if (exists(cache_key, envir = .p_results_cache)) {
    results <- base::get(cache_key, envir = .p_results_cache)
  } else {
    resultsFile <- get_p_resultsFile(participant)
    results <- as.data.frame(data.table::fread(resultsFile))
    base::assign(cache_key, results, envir = .p_results_cache)
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


# For changing up the selection inputs - bit costly, but not really needed to optimize this
get_tracker_options <- function(tracker, trialNum = 5) { # trial 5 is always available - except for haptic...
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

task_num_lookup <- function(trialNum) {
  ensure_global_data_initialized()
  
  # Handle vectorized input
  if (length(trialNum) > 1) {
    return(sapply(trialNum, task_num_lookup))
  }
  
  trial_key <- as.character(trialNum)
  if (trial_key %in% names(taskNum)) {
    return(as.numeric(taskNum[[trial_key]]))
  }

  warning(sprintf("Unknown trial number for task count: %s", trialNum))
  return(NA_real_)
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

#' Fetch simulation data while handling module availability
#' @param participant Participant identifier
#' @param trial Trial number
#' @return Simulation data frame
fetch_simulation_data <- function(participant, trial, use_cache = TRUE,
                                  cache_to_disk = TRUE, force_refresh = FALSE) {
    cache_key <- get_simulation_cache_key(participant, trial)
    tracker_cache_logger("DEBUG", sprintf(
      "Simulation request: participant=%s trial=%03d force_refresh=%s",
      participant, sanitize_trial_num(trial), force_refresh
    ))
  
    if (!exists("simulation", envir = .GlobalEnv)) {
      stop("Simulation feature module is not loaded.")
    }
  
    load_function <- function() {
      tracker_cache_logger("INFO", sprintf(
        "Loading simulation data for P%s T%03d%s",
        participant, sanitize_trial_num(trial),
        if (force_refresh) " (forced refresh)" else ""
      ))
      simulation$get_simulation_data(participant, trial)
    }
  
    force_rewrite <- isTRUE(force_refresh)
    data <- NULL
    if (use_cache && cache_to_disk && exists("load_with_cache")) {
      cache_dir <- file.path("cache", "simulation")
      data <- load_with_cache(
        cache_key,
        load_function,
        cache_dir = cache_dir,
        force_rewrite = force_rewrite
      )
    } else {
      if (force_rewrite && (!use_cache || !cache_to_disk)) {
        tracker_cache_logger("WARN", sprintf(
          "Force refresh requested for simulation cache %s but caching disabled; loading fresh without persisting",
          cache_key
        ))
      }
      data <- load_function()
    }
  
  data
}

get_dataset_cache_key <- function(file_path) {
  base <- tools::file_path_sans_ext(basename(file_path))
  gsub("[^A-Za-z0-9]+", "_", base)
}

get_dataset_cache_dir <- function(subdir = "datasets") {
  dir <- file.path("cache", subdir)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  dir
}

load_cached_dataset <- function(file_path, force_refresh = FALSE, subdir = "datasets") {
  cache_key <- get_dataset_cache_key(file_path)
  cache_dir <- get_dataset_cache_dir(subdir)

  dataset_cache_logger("INFO", sprintf(
    "Loading dataset cache key=%s (force_refresh=%s)",
    cache_key, force_refresh
  ))

  load_with_cache(
    cache_key = cache_key,
    load_function = function() {
      dataset_cache_logger("INFO", sprintf("Reading cache source: %s", file_path))
      read_cache_file(file_path)
    },
    cache_dir = cache_dir,
    force_rewrite = force_refresh
  )
}

save_cached_dataset <- function(file_path, data, subdir = "datasets") {
  cache_key <- get_dataset_cache_key(file_path)
  cache_dir <- get_dataset_cache_dir(subdir)
  cache_file <- file.path(cache_dir, paste0(cache_key, ".qs"))

  dataset_cache_logger("INFO", sprintf("Updating dataset cache %s", cache_file))
  compression_level <- if (exists("CACHE_COMPRESSION_LEVEL")) CACHE_COMPRESSION_LEVEL else 6
  qs::qsave(data, cache_file, preset = "high", compress_level = compression_level)
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
get_t_data <- function(participant, trackerType, trialNum, apply_udp_trimming = TRUE,
                       use_cache = TRUE, cache_to_disk = TRUE, force_refresh = FALSE) {
    trialNum_numeric <- sanitize_trial_num(trialNum)
    cache_key <- get_tracker_cache_key(participant, trialNum_numeric, trackerType, apply_udp_trimming)
    tracker_cache_logger("DEBUG", sprintf(
      "Tracker request: tracker=%s participant=%s trial=%03d trim=%s force_refresh=%s",
      trackerType, participant, trialNum_numeric, apply_udp_trimming, force_refresh
    ))
  
    # Check if file exists
    if (!check_file_exists(participant, trackerType, trialNum)) {
      tracker_cache_logger("WARN", sprintf(
        "Missing tracker CSV for %s P%s T%03d (cache_key=%s)",
        trackerType, participant, trialNum_numeric, cache_key
      ))
      return(NULL)
    }
  
    # Get the file prefix from the dictionary
    prefix <- filenameDict[[trackerType]]
    filename <- sprintf("%s_T%03d.csv", prefix, trialNum_numeric)
    filePath <- file.path(get_p_dir(participant), "trackers", filename)
  
    load_function <- function() {
      tracker_cache_logger("INFO", sprintf(
        "Loading tracker CSV for %s P%s T%03d%s",
        trackerType, participant, trialNum_numeric,
        if (force_refresh) " (forced refresh)" else ""
      ))
      data_local <- tryCatch(
        {
          data.table::fread(filePath)
        },
        error = function(e) {
          message("Failed to read the file: ", filePath, " - ", e$message)
          return(NULL)
        }
      )
  
      if (is.null(data_local)) {
        return(NULL)
      }
  
      data_local <- as.data.frame(data_local)
  
      if (apply_udp_trimming) {
        if (trackerType == "udp") {
          data_local <- apply_udp_time_trimming(data_local, participant, trialNum, "time")
        } else {
          data_local <- tryCatch(
            apply_udp_time_trimming(data_local, participant, trialNum, "time"),
            error = function(e) {
              message(sprintf("[WARN] Trimming skipped for %s P%s T%s: %s", trackerType, participant, trialNum, e$message))
              data_local
            }
          )
        }
      }
  
      data_local
    }
  
    force_rewrite <- isTRUE(force_refresh)
    data <- NULL
    if (use_cache && cache_to_disk && exists("load_with_cache")) {
      cache_dir <- file.path("cache", "trackers")
      data <- load_with_cache(
        cache_key,
        load_function,
        cache_dir = cache_dir,
        force_rewrite = force_rewrite
      )
      if (!is.null(data)) {
        data <- as.data.frame(data)
        tracker_cache_logger("DEBUG", sprintf(
          "Loaded tracker cache entry %s (%d rows)",
          cache_key, nrow(data)
        ))
      }
    } else {
      if (force_rewrite && (!use_cache || !cache_to_disk)) {
        tracker_cache_logger("WARN", sprintf(
          "Force refresh requested for %s but caching disabled; loading fresh without persisting",
          cache_key
        ))
      }
      data <- load_function()
    }
  
    return(data)
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
.questionnaire_cache <- new.env(parent = baseenv())

# Cached questionnaire info loading - avoids repeated CSV reads for better performance
get_question_info <- function(qType) { # qType = IMI / SSQ / VEQ
  ensure_global_data_initialized()
  cache_key <- paste0("info_", qType)

  # Return cached data if available
  if (exists(cache_key, envir = .questionnaire_cache)) {
    return(base::get(cache_key, envir = .questionnaire_cache))
  }

  # Load and cache if not present
  qInfopath <- file.path(questionnaireInfoFolder, paste0(qType, ".csv"))
  questionnaire <- as.data.frame(data.table::fread(qInfopath))
  base::assign(cache_key, questionnaire, envir = .questionnaire_cache)

  return(questionnaire)
}

# Cached questionnaire weights loading - avoids repeated CSV reads for better performance
get_question_weights <- function(qType) { # qType = IMI / UserExperience
  ensure_global_data_initialized()
  cache_key <- paste0("weights_", qType)

  # Return cached data if available
  if (exists(cache_key, envir = .questionnaire_cache)) {
    return(base::get(cache_key, envir = .questionnaire_cache))
  }

  # Load and cache if not present
  qpath <- file.path(questionnaireInfoFolder, paste0(qType, "_weights.csv"))
  questionnaire <- as.data.frame(data.table::fread(qpath))
  base::assign(cache_key, questionnaire, envir = .questionnaire_cache)

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

  # Step 1: Apply time-based filtering using UDP ranges (based on trialTime)
  valid_rows <- rep(FALSE, nrow(data))

  # is_udp_data <- "trialTime" %in% colnames(data) && FALSE
  # time_column_to_use <- if (is_udp_data) "trialTime" else "time"

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

# =============================================================================
# DATA PROCESSING AND CATEGORIZATION
# =============================================================================

#' Add identifiers and categories to gait data
#' @param data Input data frame
#' @param participant Participant ID
#' @param trial Trial number
#' @return Data frame with identifiers and categories added
add_identifiers_and_categories <- function(data, participant, trial) {
  data <- add_identifiers(data, participant, trial)
  data <- add_category_columns(data)
  return(data)
}

#' Add participant and trial identifiers to data
#' @param data Input data frame
#' @param participant Participant ID
#' @param trial Trial number
#' @return Data frame with identifiers added
add_identifiers <- function(data, participant, trial) {
  # Handle empty data frames gracefully
  if (nrow(data) == 0) {
    return(data)
  }

  data$participant <- as.factor(participant)
  data$trialNum <- as.ordered(trial)
  return(data)
}

#' Add category columns to gait data
#' @param data Input data frame
#' @return Data frame with category columns added
add_category_columns <- function(data) {
  # Handle empty data frames gracefully
  if (nrow(data) == 0) {
    return(data)
  }

  # Ensure global data is initialized (this function needs global variables)
  if (exists("ensure_global_data_initialized")) {
    ensure_global_data_initialized()
  }

  # Get unique participant and trial combinations to avoid redundant calculations
  unique_combinations <- data %>%
    dplyr::select(participant, trialNum) %>%
    dplyr::distinct()

  # Pre-calculate all values for unique combinations
  combination_data <- unique_combinations %>%
    dplyr::mutate(
      perturbations = has_perturbations(as.character(participant), as.numeric(as.character(trialNum))),
      visualizations = has_visualizations(as.character(participant), as.numeric(as.character(trialNum))),
      task = has_task(as.character(participant), as.numeric(as.character(trialNum))),
      treadmillSpeed = get_move_speed(as.character(participant), as.numeric(as.character(trialNum))),
      condition = condition_number(as.character(participant)),
      phase = get_trial_phase(as.character(participant), as.numeric(as.character(trialNum))),
      trialNumWithinPhase = ifelse(as.numeric(as.character(trialNum)) == 8, 2, 1),
      phaseNum = match(phase, allPhases),
      taskNum = task_num_lookup(as.numeric(as.character(trialNum))),
      is_training_trial = as.numeric(as.character(trialNum)) %in% c(7, 8),

      # Demographic / questionnaire details
      gender = get_p_detail(as.character(participant), "gender"),
      motion = get_p_detail(as.character(participant), "motion"),
      age = as.numeric(get_p_detail(as.character(participant), "age")),
      weight = as.numeric(get_p_detail(as.character(participant), "weight")),
      education = get_p_detail(as.character(participant), "education"),
      vr_experience = get_p_detail(as.character(participant), "vr_experience"),
      height_meters = as.numeric(get_p_detail(as.character(participant), "height_scale")) * avatar_height_m
    )

  # Join the pre-calculated data back to the original data
  data <- data %>%
    dplyr::left_join(
      combination_data,
      by = c("participant", "trialNum"),
      suffix = c("", "_lookup")
    )

  lookup_cols <- grep("_lookup$", names(data), value = TRUE)
  if (length(lookup_cols) > 0) {
    for (lookup_name in lookup_cols) {
      base_name <- sub("_lookup$", "", lookup_name)
      if (!base_name %in% names(data)) {
        data[[base_name]] <- data[[lookup_name]]
      } else {
        data[[base_name]] <- dplyr::coalesce(data[[base_name]], data[[lookup_name]])
      }
      data[[lookup_name]] <- NULL
    }
  }

  # Convert categorical variables to factors
  data <- data %>%
    dplyr::mutate(
      perturbations   = as.factor(perturbations),
      visualizations  = as.factor(visualizations),
      task            = as.factor(task),
      condition       = as.factor(condition),
      phase           = as.factor(phase),
      gender          = as.factor(gender),
      motion          = as.factor(motion),
      education       = as.factor(education),
      vr_experience   = as.factor(vr_experience)
    )

  return(data)
}

# =============================================================================
# DATA PREPROCESSING FUNCTIONS
# =============================================================================

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

  if (gait$is_kinematic_data(data)) {
    # Get rotations data from global variable (loaded in initialization.R)
    if (exists("rotations_data") && !is.null(rotations_data)) {
      rotation <- gait$load_rotations(participant, trialNum, rotations_data)
      if (length(rotation) > 0) { # If rotation is found, apply it
        rotation <- rotation[1] # Take the first rotation if multiple are found
        data <- rotate_y(data, rotation)
      }
    }
    moveSpeed <- get_move_speed(participant, trialNum)
    data$actual_pos_z <- data$pos_z + moveSpeed * data$time
  }

  return(data)
}

#' Get preprocessed data for multiple data types
#' @param participant Participant identifier
#' @param trialNum Trial number (will be converted to numeric if needed)
#' @param dataList List of data types to preprocess
#' @return List of preprocessed dataframes
get_preprocessed_data <- function(participant, trialNum, dataList = c("leftfoot", "rightfoot", "hip")) {
  # Convert trialNum to numeric if it's not already
  trialNum <- as.numeric(trialNum)
  
  result <- list()
  for (dataName in dataList) {
    # Use a more descriptive name for each element in the list
    # e.g., "leftFoot", "rightFoot", "hip", etc.
    result[[dataName]] <- preprocess_data(participant, trialNum, dataName)
  }
  return(result)
}

# =============================================================================
# DATA PREPROCESSING AND ROTATION FUNCTIONS - Note: in the end we didnt need any rotations for this data, but the code is kept for future reference
# =============================================================================

#' Preprocess tracker data (apply rotations if kinematic data)
#' @param data Raw tracker data
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param dataName Data type name (e.g., "leftfoot", "rightfoot", "hip")
#' @return Preprocessed dataframe
preprocess_tracker_data <- function(data, participant, trialNum, dataName) {
  # Check if data is empty
  if (is.null(data) || nrow(data) == 0) {
    message(sprintf("No data found for participant %s, trial %s, dataName %s", participant, trialNum, dataName))
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
