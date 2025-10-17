# Optimized filtering using data.table for better performance in Shiny reactives
# Create data.table version once for efficient filtering
.allGaitParams_dt <- NULL

# Initialize data.table version when first called
init_gait_data_table <- function() {
  if (is.null(.allGaitParams_dt)) {
    # Convert to data.table and set keys for efficient filtering
    .allGaitParams_dt <<- data.table::as.data.table(allGaitParams)
    data.table::setkey(.allGaitParams_dt, participant, trialNum, condition)
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
  # Check if allGaitParams exists and has data
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
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

  return(dt_data)
}


# Create filter state updater (this reactive updates the global filter state)
filter_state_updater <- create_filter_updater()

# Create reactive data accessors using filter manager
filtered_data_reactives <- create_filtered_data_reactives()
filteredParams <- filtered_data_reactives$filteredParams

# Non-reactive version for use outside Shiny context
get_mu_dyn_long_data <- function(participants = NULL, trials = NULL, conditions = NULL, phases = NULL,
                                outliers = NULL, suspect = NULL, sides = NULL,
                                do_slicing = FALSE, slice_length = NULL, avg_feet = FALSE, 
                                add_diff = FALSE, remove_middle_slices = FALSE) {
  # Check if required global data objects are available
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    return(NULL)
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

  # Get the regular gait mu data - conditionally use sliced or non-sliced version
  if (do_slicing) {
    # Ensure allQResults exists for sliced version
    qResults <- if (exists("allQResults") && !is.null(allQResults)) allQResults else data.frame()
    mu_gait <- get_full_mu_sliced(step_filtered_data, qResults, categories, slice_length, avg_feet, add_diff, remove_middle_slices)
  } else {
    mu_gait <- get_full_mu(step_filtered_data, categories, avg_feet, add_diff)
  }

  # Verify all required data is available
  if (!exists("allQResults") || !exists("allTaskMetrics") || !exists("allComplexityMetrics")) {
    stop("Missing required data: allQResults, allTaskMetrics, and allComplexityMetrics must all be present")
  }

  # Use new unified merge approach - prefix naming handled automatically by merge_mu_with_data
  # This now preserves all data from all sources
  mu <- mu_gait
  q_prepped <- prep_questionnaire_for_merge(allQResults)
  mu <- merge_mu_with_data(mu, q_prepped, "questionnaire", c("participant", "trialNum"))
  mu <- merge_mu_with_data(mu, allTaskMetrics, "task", c("participant", "trialNum"))
  mu <- merge_mu_with_data(mu, allComplexityMetrics, "complexity", c("participant", "trialNum"))

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
  if (!is.null(participants) && length(participants) > 0) {
    mu <- mu[mu$participant %in% participants, ]
  }
  if (!is.null(trials) && length(trials) > 0) {
    mu <- mu[mu$trialNum %in% trials, ]
  }
  if (!is.null(conditions) && length(conditions) > 0) {
    mu <- mu[mu$condition %in% conditions, ]
  }
  if (!is.null(phases) && length(phases) > 0) {
    mu <- mu[mu$phase %in% phases, ]
  }

  # Update global numeric choices for derived metrics UI
  if (!is.null(mu) && nrow(mu) > 0) {
    exclude_cols <- c("participant", "condition", "trialNum", "phase", "foot", "trialNum_num")
    numeric_cols <- names(mu)[sapply(mu, is.numeric)]
    mu_numeric_choices <<- setdiff(numeric_cols, exclude_cols)
  }

  # Check if data is empty after all processing
  if (is.null(mu) || nrow(mu) == 0) {
    showNotification(
      "No data matches the current filter settings. Please adjust your filters in the sidebar.",
      type = "warning",
      duration = 5
    )
    # Return a minimal data frame with the expected structure
    return(data.frame(
      participant = character(0),
      trialNum = numeric(0),
      condition = character(0),
      phase = character(0),
      foot = character(0)
    ))
  }

  return(mu)
}

# Use the reactive from filter manager
get_mu_dyn_long <- filtered_data_reactives$get_mu_dyn_long

# Reactive: filtered questionnaire results based on sidebar filters
filteredQResults <- reactive({
  # Force dependency on refresh_trigger()
  refresh_trigger()

  # Validate availability of allQResults
  if (!exists("allQResults") || is.null(allQResults) || nrow(allQResults) == 0) {
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
    showNotification(
      "No questionnaire data matches the current filter settings.",
      type = "warning",
      duration = 5
    )
  }

  return(df)
})
