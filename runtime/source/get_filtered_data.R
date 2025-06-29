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

  # Initialize data.table version if needed
  init_gait_data_table()

  # Use data.table filtering for much better performance on large datasets
  dt_filtered <- .allGaitParams_dt[
    participant %in% input$filterParticipants &
      trialNum %in% input$filterTrials &
      condition %in% input$filterCondition &
      heelStrikes.outlierSteps %in% input$filterOutliers &
      heelStrikes.foot %in% input$filterSide
  ]

  # Convert back to data.frame for compatibility with rest of application
  return(as.data.frame(dt_filtered))
})

get_mu_dyn_long <- reactive({
  # Get the regular gait mu data - conditionally use sliced or non-sliced version
  if (input$do_slicing) {
    mu_gait <- get_full_mu_sliced(filteredParams(), allQResults, categories, input$slice_length, input$avg_feet, input$add_diff, input$remove_middle_slices)
  } else {
    mu_gait <- get_full_mu(filteredParams(), categories, input$avg_feet, input$add_diff)
  }

  # These are all automaticallyfiltered based on gait data availability
  mu <- merge_mu_with_questionnaire(mu_gait, allQResults)
  mu <- merge_mu_with_task(mu, allTaskMetrics)
  mu <- merge_mu_with_complexity(mu, allComplexityMetrics)

  return(mu)
})
