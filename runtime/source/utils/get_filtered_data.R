# Optimized filtering using data.table for better performance in Shiny reactives
# Create data.table version once for efficient filtering
.allGaitParams_dt <- NULL

# Create module-specific logger for filtered data operations
filtered_data_logger <- create_module_logger("FILTERED-DATA")

# Simple cache for get_mu_dyn_long_data to prevent repeated expensive calculations
.mu_dyn_long_cache <- list()
.mu_dyn_long_cache_key <- NULL

# Initialize data.table version when first called
init_gait_data_table <- function() {
  if (is.null(.allGaitParams_dt)) {
    filtered_data_logger("INFO", "Initializing data.table version of allGaitParams for efficient filtering")
    # Convert to data.table and set keys for efficient filtering
    .allGaitParams_dt <<- data.table::as.data.table(allGaitParams)
    data.table::setkey(.allGaitParams_dt, participant, trialNum, condition)
    filtered_data_logger("INFO", sprintf("Data.table initialized with %d rows and keys set", nrow(.allGaitParams_dt)))
  }
}

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

# Non-reactive version for use outside Shiny context
get_filtered_params <- function(participants = NULL, trials = NULL, conditions = NULL, phases = NULL, 
                               outliers = NULL, suspect = NULL, sides = NULL) {
  filtered_data_logger("DEBUG", "Starting get_filtered_params")
  
  # Check if allGaitParams exists and has data
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    filtered_data_logger("WARN", "allGaitParams not available, returning empty data frame")
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

  filtered_data_logger("DEBUG", sprintf("get_filtered_params completed, returning %d rows", nrow(dt_data)))
  return(dt_data)
}


# Create reactive objects only in Shiny context
if (exists("input") && is.reactive(input)) {
  # Create filter state updater (this reactive updates the global filter state)
  filter_state_updater <- create_filter_updater()
  
  # Create reactive data accessors using filter manager
  filtered_data_reactives <- create_filtered_data_reactives()
  filteredParams <- filtered_data_reactives$filteredParams
} else {
  # In non-Shiny context, create placeholder functions
  filter_state_updater <- function() { invisible() }
  filteredParams <- function() { get_current_filtered_params() }
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
    filtered_data_logger("DEBUG", "Returning cached result for get_mu_dyn_long_data")
    return(.mu_dyn_long_cache)
  }
  
  filtered_data_logger("DEBUG", "Starting get_mu_dyn_long_data (cache miss)")
  
  # Check if required global data objects are available
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    filtered_data_logger("WARN", "allGaitParams not available, returning empty data frame")
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
  if (exists("gait") && is.function(gait$get_full_mu)) {
    mu_gait <- gait$get_full_mu(step_filtered_data, categories, avg_feet, add_diff)
  } else {
    filtered_data_logger("ERROR", "gait$get_full_mu function not available")
    stop("gait$get_full_mu function not available")
  }

  # Check if all required data is available, return early if not
  if (!exists("allQResults") || !exists("allTaskMetrics") || !exists("allComplexityMetrics")) {
    missing_data <- c()
    if (!exists("allQResults")) missing_data <- c(missing_data, "allQResults")
    if (!exists("allTaskMetrics")) missing_data <- c(missing_data, "allTaskMetrics")
    if (!exists("allComplexityMetrics")) missing_data <- c(missing_data, "allComplexityMetrics")
    filtered_data_logger("WARN", sprintf("Missing required data: %s, returning empty data frame", paste(missing_data, collapse = ", ")))
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
  filtered_data_logger("DEBUG", "Starting data merging process")
  mu <- mu_gait
  q_prepped <- prep_questionnaire_for_merge(allQResults)
  mu <- merge_mu_with_data(mu, q_prepped, "questionnaire", c("participant", "trialNum"))
  filtered_data_logger("DEBUG", sprintf("Merged questionnaire data, mu now has %d rows", nrow(mu)))
  mu <- merge_mu_with_data(mu, allTaskMetrics, "task", c("participant", "trialNum"))
  filtered_data_logger("DEBUG", sprintf("Merged task metrics, mu now has %d rows", nrow(mu)))
  mu <- merge_mu_with_data(mu, allComplexityMetrics, "complexity", c("participant", "trialNum"))
  filtered_data_logger("DEBUG", sprintf("Merged complexity metrics, mu now has %d rows", nrow(mu)))

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
  filtered_data_logger("DEBUG", sprintf("Applying trial-based filters to %d rows", nrow(mu)))
  if (!is.null(participants) && length(participants) > 0) {
    mu <- mu[mu$participant %in% participants, ]
    filtered_data_logger("DEBUG", sprintf("After participant filter: %d rows", nrow(mu)))
  }
  if (!is.null(trials) && length(trials) > 0) {
    mu <- mu[mu$trialNum %in% trials, ]
    filtered_data_logger("DEBUG", sprintf("After trial filter: %d rows", nrow(mu)))
  }
  if (!is.null(conditions) && length(conditions) > 0) {
    mu <- mu[mu$condition %in% conditions, ]
    filtered_data_logger("DEBUG", sprintf("After condition filter: %d rows", nrow(mu)))
  }
  if (!is.null(phases) && length(phases) > 0) {
    mu <- mu[mu$phase %in% phases, ]
    filtered_data_logger("DEBUG", sprintf("After phase filter: %d rows", nrow(mu)))
  }

  # Update global numeric choices for derived metrics UI
  if (!is.null(mu) && nrow(mu) > 0) {
    exclude_cols <- c("participant", "condition", "trialNum", "phase", "foot", "trialNum_num")
    numeric_cols <- names(mu)[sapply(mu, is.numeric)]
    mu_numeric_choices <<- setdiff(numeric_cols, exclude_cols)
  }

  # Check if data is empty after all processing
  if (is.null(mu) || nrow(mu) == 0) {
    filtered_data_logger("WARN", "No data matches the current filter settings after processing")
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

  filtered_data_logger("INFO", sprintf("get_mu_dyn_long_data completed successfully, returning %d rows with %d columns", nrow(mu), ncol(mu)))
  
  # Update cache
  .mu_dyn_long_cache <<- mu
  .mu_dyn_long_cache_key <<- cache_key
  
  return(mu)
}

# Use the reactive from filter manager (only in Shiny context)
if (exists("input") && is.reactive(input)) {
  get_mu_dyn_long <- filtered_data_reactives$get_mu_dyn_long
} else {
  get_mu_dyn_long <- function() { get_current_mu_dyn_long() }
}

# Create reactive questionnaire results only in Shiny context
if (exists("input") && is.reactive(input)) {
  # Reactive: filtered questionnaire results based on sidebar filters
  filteredQResults <- reactive({
    # Force dependency on refresh_trigger()
    refresh_trigger()
    filtered_data_logger("DEBUG", "Starting filteredQResults reactive")

    # Validate availability of allQResults
    if (!exists("allQResults") || is.null(allQResults) || nrow(allQResults) == 0) {
      filtered_data_logger("WARN", "allQResults not available in filteredQResults")
      return(data.frame())
    }

    df <- allQResults

    # Apply participant filter
    if (!is.null(input$filterParticipants) && length(input$filterParticipants) > 0) {
      df <- df[df$participant %in% input$filterParticipants, ]
    }

    # Apply condition filter
    if (!is.null(input$filterCondition) && length(input$filterCondition) > 0) {
      df <- df[df$condition %in% input$filterCondition, ]
    }

    # Apply phase filter (mapped to questionnaire answer_type)
    if (!is.null(input$filterPhase) && length(input$filterPhase) > 0) {
      df <- df[df$answer_type %in% input$filterPhase, ]
    }

    # Notify if result empty
    if (nrow(df) == 0) {
      filtered_data_logger("WARN", "No questionnaire data matches the current filter settings")
      if (exists("showNotification")) {
        showNotification(
          "No questionnaire data matches the current filter settings.",
          type = "warning",
          duration = 5
        )
      }
    }

    filtered_data_logger("DEBUG", sprintf("filteredQResults completed, returning %d rows", nrow(df)))
    return(df)
  })
} else {
  # In non-Shiny context, create placeholder function
  filteredQResults <- function() { 
    if (exists("allQResults") && !is.null(allQResults)) allQResults else data.frame()
  }
}
