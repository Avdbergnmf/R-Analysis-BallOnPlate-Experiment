#' Filter Manager - Intermediate Layer for Reactive/Non-Reactive Code
#'
#' This module provides a clean interface between Shiny reactive values
#' and non-reactive analysis functions. It handles the complexity of
#' managing filter states and provides both reactive and non-reactive access.
#'
#' @author Alex van den Berg
#' @version 1.0

# Create module-specific logger for filter manager operations
filter_manager_logger <- create_module_logger("FILTER-MANAGER")
filter_manager_logger("INFO", "Filter manager module loaded")

# =============================================================================
# DATA TABLE OPTIMIZATION AND CACHING
# =============================================================================

# Optimized filtering using data.table for better performance in Shiny reactives
# Create data.table version once for efficient filtering
.allGaitParams_dt <- NULL

# Simple cache for get_mu_dyn_long_data to prevent repeated expensive calculations
.mu_dyn_long_cache <- list()
.mu_dyn_long_cache_key <- NULL

# Initialize data.table version when first called
init_gait_data_table <- function() {
  if (is.null(.allGaitParams_dt)) {
    filter_manager_logger("INFO", "Initializing data.table version of allGaitParams for efficient filtering")
    # Convert to data.table and set keys for efficient filtering
    .allGaitParams_dt <<- data.table::as.data.table(allGaitParams)
    data.table::setkey(.allGaitParams_dt, participant, trialNum, condition)
    filter_manager_logger("INFO", sprintf("Data.table initialized with %d rows and keys set", nrow(.allGaitParams_dt)))
  }
}

# =============================================================================
# FILTER STATE MANAGEMENT
# =============================================================================

# Global filter state (updated by Shiny reactive functions)
.filter_state <- list(
  participants = NULL,
  trials = NULL,
  conditions = NULL,
  phases = NULL,
  outliers = NULL,
  suspect = NULL,
  sides = NULL,
  do_slicing = FALSE,
  slice_length = NULL,
  avg_feet = FALSE,
  add_diff = FALSE,
  remove_middle_slices = FALSE
)

# =============================================================================
# FILTER STATE ACCESSORS
# =============================================================================

#' Get current filter state
#' @return List of current filter parameters
get_filter_state <- function() {
  filter_manager_logger("DEBUG", "get_filter_state called")
  return(.filter_state)
}


#' Update filter state from Shiny inputs
#' @param input Shiny input object
update_filter_state <- function(input) {
  filter_manager_logger("DEBUG", "update_filter_state called")
  
  # Log current input values
  filter_manager_logger("DEBUG", sprintf("Input participants: %s", 
    if(is.null(input$filterParticipants)) "NULL" else paste(input$filterParticipants, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Input trials: %s", 
    if(is.null(input$filterTrials)) "NULL" else paste(input$filterTrials, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Input conditions: %s", 
    if(is.null(input$filterCondition)) "NULL" else paste(input$filterCondition, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Input phases: %s", 
    if(is.null(input$filterPhase)) "NULL" else paste(input$filterPhase, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Input outliers: %s", 
    if(is.null(input$filterOutliers)) "NULL" else paste(input$filterOutliers, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Input suspect: %s", 
    if(is.null(input$filterSuspect)) "NULL" else paste(input$filterSuspect, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Input sides: %s", 
    if(is.null(input$filterSide)) "NULL" else paste(input$filterSide, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Input do_slicing: %s", 
    if(is.null(input$do_slicing)) "NULL" else input$do_slicing))
  filter_manager_logger("DEBUG", sprintf("Input slice_length: %s", 
    if(is.null(input$slice_length)) "NULL" else input$slice_length))
  filter_manager_logger("DEBUG", sprintf("Input avg_feet: %s", 
    if(is.null(input$avg_feet)) "NULL" else input$avg_feet))
  filter_manager_logger("DEBUG", sprintf("Input add_diff: %s", 
    if(is.null(input$add_diff)) "NULL" else input$add_diff))
  filter_manager_logger("DEBUG", sprintf("Input remove_middle_slices: %s", 
    if(is.null(input$remove_middle_slices)) "NULL" else input$remove_middle_slices))
  
  # Update filter state
  .filter_state$participants <<- input$filterParticipants
  .filter_state$trials <<- input$filterTrials
  .filter_state$conditions <<- input$filterCondition
  .filter_state$phases <<- input$filterPhase
  .filter_state$outliers <<- input$filterOutliers
  .filter_state$suspect <<- input$filterSuspect
  .filter_state$sides <<- input$filterSide
  .filter_state$do_slicing <<- input$do_slicing
  .filter_state$slice_length <<- input$slice_length
  .filter_state$avg_feet <<- input$avg_feet
  .filter_state$add_diff <<- input$add_diff
  .filter_state$remove_middle_slices <<- input$remove_middle_slices
  
  # Log updated filter state
  filter_manager_logger("INFO", "Filter state updated successfully")
  filter_manager_logger("DEBUG", sprintf("Updated participants: %s", 
    if(is.null(.filter_state$participants)) "NULL" else paste(.filter_state$participants, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Updated trials: %s", 
    if(is.null(.filter_state$trials)) "NULL" else paste(.filter_state$trials, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("Updated conditions: %s", 
    if(is.null(.filter_state$conditions)) "NULL" else paste(.filter_state$conditions, collapse = ", ")))
}

#' Set filter state manually (for non-Shiny contexts)
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param conditions Vector of conditions
#' @param phases Vector of phases
#' @param outliers Vector of outlier statuses
#' @param suspect Vector of suspect statuses
#' @param sides Vector of sides
#' @param do_slicing Whether to use slicing
#' @param slice_length Slice length for slicing
#' @param avg_feet Whether to average across feet
#' @param add_diff Whether to add difference columns
#' @param remove_middle_slices Whether to remove middle slices
set_filter_state <- function(participants = NULL, trials = NULL, conditions = NULL, 
                           phases = NULL, outliers = NULL, suspect = NULL, sides = NULL,
                           do_slicing = FALSE, slice_length = NULL, avg_feet = FALSE,
                           add_diff = FALSE, remove_middle_slices = FALSE) {
  .filter_state$participants <<- participants
  .filter_state$trials <<- trials
  .filter_state$conditions <<- conditions
  .filter_state$phases <<- phases
  .filter_state$outliers <<- outliers
  .filter_state$suspect <<- suspect
  .filter_state$sides <<- sides
  .filter_state$do_slicing <<- do_slicing
  .filter_state$slice_length <<- slice_length
  .filter_state$avg_feet <<- avg_feet
  .filter_state$add_diff <<- add_diff
  .filter_state$remove_middle_slices <<- remove_middle_slices
}

# =============================================================================
# FILTER APPLICATION FUNCTIONS
# =============================================================================

#' Apply trial-based filtering (participants, trials, condition, phase)
#' @param data Data frame to filter
#' @return Filtered data frame
apply_trial_filters <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  # Apply participant filter
  if (!is.null(input$filterParticipants) && length(input$filterParticipants) > 0) {
    data <- data[data$participant %in% input$filterParticipants, ]
  }

  # Apply trial filter
  if (!is.null(input$filterTrials) && length(input$filterTrials) > 0) {
    data <- data[data$trialNum %in% input$filterTrials, ]
  }

  # Apply condition filter
  if (!is.null(input$filterCondition) && length(input$filterCondition) > 0) {
    data <- data[data$condition %in% input$filterCondition, ]
  }

  # Apply phase filter
  if (!is.null(input$filterPhase) && length(input$filterPhase) > 0) {
    data <- data[data$phase %in% input$filterPhase, ]
  }

  return(data)
}

#' Apply step-based filtering (outlierSteps, suspect, foot)
#' @param data Data frame to filter
#' @return Filtered data frame
apply_step_filters <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }

  # Apply outlier filter (if column exists)
  if ("outlierSteps" %in% colnames(data) && !is.null(input$filterOutliers) && length(input$filterOutliers) > 0) {
    data <- data[data$outlierSteps %in% input$filterOutliers, ]
  }

  # Apply suspect filter (if column exists)
  if ("suspect" %in% colnames(data) && !is.null(input$filterSuspect) && length(input$filterSuspect) > 0) {
    data <- data[data$suspect %in% input$filterSuspect, ]
  }

  # Apply side filter (if column exists)
  if ("foot" %in% colnames(data) && !is.null(input$filterSide) && length(input$filterSide) > 0) {
    data <- data[data$foot %in% input$filterSide, ]
  }

  return(data)
}

# =============================================================================
# DATA ACCESS FUNCTIONS
# =============================================================================

# Non-reactive version for use outside Shiny context
get_filtered_params <- function(participants = NULL, trials = NULL, conditions = NULL, phases = NULL, 
                               outliers = NULL, suspect = NULL, sides = NULL) {
  filter_manager_logger("DEBUG", "Starting get_filtered_params")
  
  # Check if allGaitParams exists and has data
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    filter_manager_logger("WARN", "allGaitParams not available, returning empty data frame")
    return(data.frame()) # Return empty dataframe if no data available
  }

  # Initialize data.table version if needed
  init_gait_data_table()

  # Convert to data.frame for filtering
  dt_data <- as.data.frame(.allGaitParams_dt)

  # Apply filters with provided parameters
  if (!is.null(participants) && length(participants) > 0) {
    dt_data <- dt_data[dt_data$participant %in% participants, ]
  }
  if (!is.null(trials) && length(trials) > 0) {
    dt_data <- dt_data[dt_data$trialNum %in% trials, ]
  }
  if (!is.null(conditions) && length(conditions) > 0) {
    dt_data <- dt_data[dt_data$condition %in% conditions, ]
  }
  if (!is.null(phases) && length(phases) > 0) {
    dt_data <- dt_data[dt_data$phase %in% phases, ]
  }
  if (!is.null(outliers) && length(outliers) > 0 && "outlierSteps" %in% colnames(dt_data)) {
    dt_data <- dt_data[dt_data$outlierSteps %in% outliers, ]
  }
  if (!is.null(suspect) && length(suspect) > 0 && "suspect" %in% colnames(dt_data)) {
    dt_data <- dt_data[dt_data$suspect %in% suspect, ]
  }
  if (!is.null(sides) && length(sides) > 0 && "foot" %in% colnames(dt_data)) {
    dt_data <- dt_data[dt_data$foot %in% sides, ]
  }

  filter_manager_logger("DEBUG", sprintf("get_filtered_params completed, returning %d rows", nrow(dt_data)))
  return(dt_data)
}

# Non-reactive version for use outside Shiny context
get_mu_dyn_long_data <- function(participants = NULL, trials = NULL, conditions = NULL, phases = NULL,
                                outliers = NULL, suspect = NULL, sides = NULL,
                                avg_feet = FALSE, add_diff = FALSE) {
  # Create cache key from parameters
  cache_key <- digest::digest(list(
    participants = participants, trials = trials, conditions = conditions, phases = phases,
    outliers = outliers, suspect = suspect, sides = sides,
    avg_feet = avg_feet, add_diff = add_diff
  ))
  
  # Check cache first
  if (!is.null(.mu_dyn_long_cache_key) && .mu_dyn_long_cache_key == cache_key && 
      !is.null(.mu_dyn_long_cache) && nrow(.mu_dyn_long_cache) > 0) {
    filter_manager_logger("DEBUG", "Returning cached result for get_mu_dyn_long_data")
    return(.mu_dyn_long_cache)
  }
  
  filter_manager_logger("DEBUG", "Starting get_mu_dyn_long_data (cache miss)")
  
  # Check if required global data objects are available
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    filter_manager_logger("WARN", "allGaitParams not available, returning empty data frame")
    # Return a minimal data frame with expected structure instead of NULL
    return(data.frame(
      participant = character(0),
      trialNum = numeric(0),
      condition = character(0),
      phase = character(0),
      foot = character(0)
    ))
  }

  # Initialize data.table and apply ONLY step-based filters (not trial filters yet)
  init_gait_data_table()
  step_filtered_data <- as.data.frame(.allGaitParams_dt)
  
  # Apply step-based filters with provided parameters
  if (!is.null(outliers) && length(outliers) > 0 && "outlierSteps" %in% colnames(step_filtered_data)) {
    step_filtered_data <- step_filtered_data[step_filtered_data$outlierSteps %in% outliers, ]
  }
  if (!is.null(suspect) && length(suspect) > 0 && "suspect" %in% colnames(step_filtered_data)) {
    step_filtered_data <- step_filtered_data[step_filtered_data$suspect %in% suspect, ]
  }
  if (!is.null(sides) && length(sides) > 0 && "foot" %in% colnames(step_filtered_data)) {
    step_filtered_data <- step_filtered_data[step_filtered_data$foot %in% sides, ]
  }

  # Get the regular gait mu data using gait feature module
  mu_gait <- gait$get_full_mu(step_filtered_data, categories, avg_feet, add_diff)

  # Check if all required data is available, return early if not
  if (!exists("allQResults") || !exists("allTaskMetrics") || !exists("allComplexityMetrics")) {
    missing_data <- c()
    if (!exists("allQResults")) missing_data <- c(missing_data, "allQResults")
    if (!exists("allTaskMetrics")) missing_data <- c(missing_data, "allTaskMetrics")
    if (!exists("allComplexityMetrics")) missing_data <- c(missing_data, "allComplexityMetrics")
    filter_manager_logger("WARN", sprintf("Missing required data: %s, returning empty data frame", paste(missing_data, collapse = ", ")))
    # Return a minimal data frame with expected structure instead of stopping
    return(data.frame(
      participant = character(0),
      trialNum = numeric(0),
      condition = character(0),
      phase = character(0),
      foot = character(0)
    ))
  }

  # Use new unified merge approach - prefix naming handled automatically by merge_mu_with_data
  # This now preserves all data from all sources
  filter_manager_logger("DEBUG", "Starting data merging process")
  mu <- mu_gait
  # Use questionnaire feature module for preparation
  q_prepped <- questionnaire$prep_questionnaire_for_merge(allQResults)
  mu <- gait$merge_mu_with_data(mu, q_prepped, "questionnaire", c("participant", "trialNum"))
  filter_manager_logger("DEBUG", sprintf("Merged questionnaire data, mu now has %d rows", nrow(mu)))
  mu <- gait$merge_mu_with_data(mu, allTaskMetrics, "task", c("participant", "trialNum"))
  filter_manager_logger("DEBUG", sprintf("Merged task metrics, mu now has %d rows", nrow(mu)))
  mu <- gait$merge_mu_with_data(mu, allComplexityMetrics, "complexity", c("participant", "trialNum"))
  filter_manager_logger("DEBUG", sprintf("Merged complexity metrics, mu now has %d rows", nrow(mu)))

  # Inject global derived metrics from full MU (before trial filtering)
  defs <- tryCatch(global_derived_metric_defs(), error = function(e) NULL)
  if (!is.null(defs) && nrow(defs) > 0) {
    if ("trialNum" %in% names(mu)) {
      mu$trialNum_num <- suppressWarnings(as.numeric(as.character(mu$trialNum)))
    }
    for (i in seq_len(nrow(defs))) {
      var_name <- defs$var[i]
      scope <- defs$scope[i]
      level <- defs$level[i]
      col_name <- defs$colname[i]
      if (!(var_name %in% names(mu))) next
      if (identical(scope, "phase") && ("phase" %in% names(mu))) {
        lookup <- mu[as.character(mu$phase) == as.character(level), c("participant", var_name)]
      } else if (identical(scope, "trial") && ("trialNum" %in% names(mu))) {
        if ("trialNum_num" %in% names(mu) && suppressWarnings(!is.na(as.numeric(level)))) {
          lvl_num <- suppressWarnings(as.numeric(level))
          lookup <- mu[mu$trialNum_num == lvl_num, c("participant", var_name)]
        } else {
          lookup <- mu[as.character(mu$trialNum) == as.character(level), c("participant", var_name)]
        }
      } else {
        next
      }
      if (nrow(lookup) == 0) next
      agg <- stats::aggregate(lookup[[var_name]], by = list(participant = lookup$participant), FUN = function(x) suppressWarnings(mean(as.numeric(x), na.rm = TRUE)))
      names(agg)[2] <- col_name
      mu <- base::merge(mu, agg, by = "participant", all.x = TRUE, sort = FALSE)
    }
  }

  # NOW apply trial-based filters to the complete merged dataset
  filter_manager_logger("DEBUG", sprintf("Applying trial-based filters to %d rows", nrow(mu)))
  if (!is.null(participants) && length(participants) > 0) {
    mu <- mu[mu$participant %in% participants, ]
    filter_manager_logger("DEBUG", sprintf("After participant filter: %d rows", nrow(mu)))
  }
  if (!is.null(trials) && length(trials) > 0) {
    mu <- mu[mu$trialNum %in% trials, ]
    filter_manager_logger("DEBUG", sprintf("After trial filter: %d rows", nrow(mu)))
  }
  if (!is.null(conditions) && length(conditions) > 0) {
    mu <- mu[mu$condition %in% conditions, ]
    filter_manager_logger("DEBUG", sprintf("After condition filter: %d rows", nrow(mu)))
  }
  if (!is.null(phases) && length(phases) > 0) {
    mu <- mu[mu$phase %in% phases, ]
    filter_manager_logger("DEBUG", sprintf("After phase filter: %d rows", nrow(mu)))
  }

  # Update global numeric choices for derived metrics UI
  if (!is.null(mu) && nrow(mu) > 0) {
    exclude_cols <- c("participant", "condition", "trialNum", "phase", "foot", "trialNum_num")
    numeric_cols <- names(mu)[sapply(mu, is.numeric)]
    mu_numeric_choices <<- setdiff(numeric_cols, exclude_cols)
  }

  # Check if data is empty after all processing
  if (is.null(mu) || nrow(mu) == 0) {
    filter_manager_logger("WARN", "No data matches the current filter settings after processing")
    if (exists("showNotification")) {
      showNotification(
        "No data matches the current filter settings. Please adjust your filters in the sidebar.",
        type = "warning",
        duration = 5
      )
    }
    # Return a minimal data frame with the expected structure
    return(data.frame(
      participant = character(0),
      trialNum = numeric(0),
      condition = character(0),
      phase = character(0),
      foot = character(0)
    ))
  }

  filter_manager_logger("INFO", sprintf("get_mu_dyn_long_data completed successfully, returning %d rows with %d columns", nrow(mu), ncol(mu)))
  
  # Update cache
  .mu_dyn_long_cache <<- mu
  .mu_dyn_long_cache_key <<- cache_key
  
  return(mu)
}

#' Get filtered parameters using current filter state
#' @return Filtered data frame
get_current_filtered_params <- function() {
  filter_manager_logger("DEBUG", "get_current_filtered_params called")
  filter_manager_logger("DEBUG", sprintf("Filter state: participants=%s, trials=%s, conditions=%s", 
    if(is.null(.filter_state$participants)) "NULL" else length(.filter_state$participants),
    if(is.null(.filter_state$trials)) "NULL" else length(.filter_state$trials),
    if(is.null(.filter_state$conditions)) "NULL" else length(.filter_state$conditions)
  ))
  
  # Log detailed filter state
  filter_manager_logger("DEBUG", sprintf("Detailed filter state:"))
  filter_manager_logger("DEBUG", sprintf("  participants: %s", 
    if(is.null(.filter_state$participants)) "NULL" else paste(.filter_state$participants, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  trials: %s", 
    if(is.null(.filter_state$trials)) "NULL" else paste(.filter_state$trials, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  conditions: %s", 
    if(is.null(.filter_state$conditions)) "NULL" else paste(.filter_state$conditions, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  phases: %s", 
    if(is.null(.filter_state$phases)) "NULL" else paste(.filter_state$phases, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  outliers: %s", 
    if(is.null(.filter_state$outliers)) "NULL" else paste(.filter_state$outliers, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  suspect: %s", 
    if(is.null(.filter_state$suspect)) "NULL" else paste(.filter_state$suspect, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  sides: %s", 
    if(is.null(.filter_state$sides)) "NULL" else paste(.filter_state$sides, collapse = ", ")))

  result <- get_filtered_params(
    participants = .filter_state$participants,
    trials = .filter_state$trials,
    conditions = .filter_state$conditions,
    phases = .filter_state$phases,
    outliers = .filter_state$outliers,
    suspect = .filter_state$suspect,
    sides = .filter_state$sides
  )

  filter_manager_logger("DEBUG", sprintf("get_current_filtered_params returning %d rows", nrow(result)))
  return(result)
}


#' Get mu dynamic long data using current filter state
#' @return Processed mu data frame
get_current_mu_dyn_long <- function() {
  filter_manager_logger("DEBUG", "get_current_mu_dyn_long called")
  filter_manager_logger("DEBUG", sprintf("Filter state: participants=%s, trials=%s, conditions=%s", 
    if(is.null(.filter_state$participants)) "NULL" else length(.filter_state$participants),
    if(is.null(.filter_state$trials)) "NULL" else length(.filter_state$trials),
    if(is.null(.filter_state$conditions)) "NULL" else length(.filter_state$conditions)
  ))
  
  # Log detailed filter state
  filter_manager_logger("DEBUG", sprintf("Detailed filter state for mu_dyn_long:"))
  filter_manager_logger("DEBUG", sprintf("  participants: %s", 
    if(is.null(.filter_state$participants)) "NULL" else paste(.filter_state$participants, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  trials: %s", 
    if(is.null(.filter_state$trials)) "NULL" else paste(.filter_state$trials, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  conditions: %s", 
    if(is.null(.filter_state$conditions)) "NULL" else paste(.filter_state$conditions, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  phases: %s", 
    if(is.null(.filter_state$phases)) "NULL" else paste(.filter_state$phases, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  outliers: %s", 
    if(is.null(.filter_state$outliers)) "NULL" else paste(.filter_state$outliers, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  suspect: %s", 
    if(is.null(.filter_state$suspect)) "NULL" else paste(.filter_state$suspect, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  sides: %s", 
    if(is.null(.filter_state$sides)) "NULL" else paste(.filter_state$sides, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  avg_feet: %s", 
    if(is.null(.filter_state$avg_feet)) "NULL" else .filter_state$avg_feet))
  filter_manager_logger("DEBUG", sprintf("  add_diff: %s", 
    if(is.null(.filter_state$add_diff)) "NULL" else .filter_state$add_diff))
  
  result <- get_mu_dyn_long_data(
    participants = .filter_state$participants,
    trials = .filter_state$trials,
    conditions = .filter_state$conditions,
    phases = .filter_state$phases,
    outliers = .filter_state$outliers,
    suspect = .filter_state$suspect,
    sides = .filter_state$sides,
    avg_feet = .filter_state$avg_feet,
    add_diff = .filter_state$add_diff
  )

  filter_manager_logger("DEBUG", sprintf("get_current_mu_dyn_long returning %d rows", nrow(result)))
  return(result)
}


#' Get filtered questionnaire results using current filter state
#' @return Filtered questionnaire data frame
get_current_filtered_q_results <- function() {
  filter_manager_logger("DEBUG", "get_current_filtered_q_results called")
  filter_manager_logger("DEBUG", sprintf("Filter state: participants=%s, conditions=%s, phases=%s", 
    if(is.null(.filter_state$participants)) "NULL" else length(.filter_state$participants),
    if(is.null(.filter_state$conditions)) "NULL" else length(.filter_state$conditions),
    if(is.null(.filter_state$phases)) "NULL" else length(.filter_state$phases)
  ))
  
  # Log detailed filter state
  filter_manager_logger("DEBUG", sprintf("Detailed filter state for filtered_q_results:"))
  filter_manager_logger("DEBUG", sprintf("  participants: %s", 
    if(is.null(.filter_state$participants)) "NULL" else paste(.filter_state$participants, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  conditions: %s", 
    if(is.null(.filter_state$conditions)) "NULL" else paste(.filter_state$conditions, collapse = ", ")))
  filter_manager_logger("DEBUG", sprintf("  phases: %s", 
    if(is.null(.filter_state$phases)) "NULL" else paste(.filter_state$phases, collapse = ", ")))
  
  # Validate availability of allQResults
  if (!exists("allQResults") || is.null(allQResults) || nrow(allQResults) == 0) {
    filter_manager_logger("WARN", "allQResults not available in get_current_filtered_q_results")
    return(data.frame())
  }

  df <- allQResults
  filter_manager_logger("DEBUG", sprintf("Starting with %d rows of questionnaire data", nrow(df)))

  # Apply participant filter
  if (!is.null(.filter_state$participants) && length(.filter_state$participants) > 0) {
    filter_manager_logger("DEBUG", sprintf("Applying participant filter: %s", paste(.filter_state$participants, collapse = ", ")))
    df <- df[df$participant %in% .filter_state$participants, ]
    filter_manager_logger("DEBUG", sprintf("After participant filter: %d rows", nrow(df)))
  } else {
    filter_manager_logger("DEBUG", "No participant filter applied")
  }

  # Apply condition filter
  if (!is.null(.filter_state$conditions) && length(.filter_state$conditions) > 0) {
    filter_manager_logger("DEBUG", sprintf("Applying condition filter: %s", paste(.filter_state$conditions, collapse = ", ")))
    df <- df[df$condition %in% .filter_state$conditions, ]
    filter_manager_logger("DEBUG", sprintf("After condition filter: %d rows", nrow(df)))
  } else {
    filter_manager_logger("DEBUG", "No condition filter applied")
  }

  # Apply phase filter (mapped to questionnaire answer_type)
  if (!is.null(.filter_state$phases) && length(.filter_state$phases) > 0) {
    filter_manager_logger("DEBUG", sprintf("Applying phase filter: %s", paste(.filter_state$phases, collapse = ", ")))
    df <- df[df$answer_type %in% .filter_state$phases, ]
    filter_manager_logger("DEBUG", sprintf("After phase filter: %d rows", nrow(df)))
  } else {
    filter_manager_logger("DEBUG", "No phase filter applied")
  }

  # Notify if result empty
  if (nrow(df) == 0) {
    filter_manager_logger("WARN", "No questionnaire data matches the current filter settings")
    if (exists("showNotification")) {
      showNotification(
        "No questionnaire data matches the current filter settings.",
        type = "warning",
        duration = 5
      )
    }
  }

  filter_manager_logger("DEBUG", sprintf("get_current_filtered_q_results returning %d rows", nrow(df)))
  return(df)
}


# =============================================================================
# SHINY INTEGRATION
# =============================================================================

# Note: create_filter_updater function removed - filter state is now updated
# directly in the reactive functions that depend on specific inputs
