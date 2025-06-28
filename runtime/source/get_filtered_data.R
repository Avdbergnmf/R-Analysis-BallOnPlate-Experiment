# Filter data based on inputs

filteredParams <- reactive({
  # Force dependency on `refresh_trigger()`
  refresh_trigger()

  data <- allGaitParams
  included <- data[["participant"]] %in% input$filterParticipants
  # Trial based
  included <- included & data[["trialNum"]] %in% input$filterTrials
  included <- included & data[["condition"]] %in% input$filterCondition

  # Step based
  included <- included & data[["heelStrikes.outlierSteps"]] %in% input$filterOutliers

  included <- included & data[["heelStrikes.foot"]] %in% input$filterSide

  return(data[included, ])
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
