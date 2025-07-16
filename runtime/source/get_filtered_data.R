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

filteredParams <- reactive({
  # Force dependency on `refresh_trigger()`
  refresh_trigger()

  # Check if allGaitParams exists and has data
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    return(data.frame()) # Return empty dataframe if no data available
  }

  # Initialize data.table version if needed
  init_gait_data_table()

  # Use data.table filtering for much better performance on large datasets
  dt_filtered <- .allGaitParams_dt[
    participant %in% input$filterParticipants &
      trialNum %in% input$filterTrials &
      condition %in% input$filterCondition &
      phase %in% input$filterPhase &
      outlierSteps %in% input$filterOutliers &
      suspect %in% input$filterSuspect &
      foot %in% input$filterSide
  ]

  # Convert back to data.frame for compatibility with rest of application
  return(as.data.frame(dt_filtered))
})

get_mu_dyn_long <- reactive({
  # Check if required global data objects are available
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    return(NULL)
  }

  # Get the regular gait mu data - conditionally use sliced or non-sliced version
  if (input$do_slicing) {
    # Ensure allQResults exists for sliced version
    qResults <- if (exists("allQResults") && !is.null(allQResults)) allQResults else data.frame()
    mu_gait <- get_full_mu_sliced(filteredParams(), qResults, categories, input$slice_length, input$avg_feet, input$add_diff, input$remove_middle_slices)
  } else {
    mu_gait <- get_full_mu(filteredParams(), categories, input$avg_feet, input$add_diff)
  }

  # These are all automatically filtered based on gait data availability
  # Use safe versions that handle NULL/missing data
  qResults <- if (exists("allQResults") && !is.null(allQResults)) allQResults else data.frame()
  taskResults <- if (exists("allTaskMetrics") && !is.null(allTaskMetrics)) allTaskMetrics else data.frame()
  complexityResults <- if (exists("allComplexityMetrics") && !is.null(allComplexityMetrics)) allComplexityMetrics else data.frame()

  # Apply time slicing to task and complexity metrics if enabled
  if (input$do_slicing) {
    if (nrow(taskResults) > 0) {
      taskResults <- get_task_metrics_sliced(taskResults, input$filterParticipants, input$filterTrials, 
                                           input$slice_length, input$remove_middle_slices)
    }
    if (nrow(complexityResults) > 0) {
      complexityResults <- get_complexity_metrics_sliced(complexityResults, input$filterParticipants, input$filterTrials,
                                                       input$slice_length, input$remove_middle_slices)
    }
  }

  mu <- merge_mu_with_questionnaire(mu_gait, qResults)
  mu <- merge_mu_with_task(mu, taskResults)
  mu <- merge_mu_with_complexity(mu, complexityResults)

  return(mu)
})
