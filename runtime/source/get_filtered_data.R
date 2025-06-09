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

filteredQResults_new <- reactive({
  data <- allQResults
  included <- data[["participant"]] %in% input$filterParticipants
  ############## SHOULD GO TO PHASES INSTEAD OF TRIALNUMS ?????
  return(data[included, ])
})

get_mu_dyn_long <- reactive({
  # Get the regular gait mu data
  mu_gait <- get_full_mu_sliced(filteredParams(), allQResults, categories, input$slice_length, input$avg_feet, input$add_diff, input$remove_middle_slices)

  # Get task simulation metrics using cached data with same filtering and slicing
  tryCatch(
    {
      cat("Attempting to load task metrics from cached simulation data...\n")

      # Get current filter settings
      selected_participants <- input$filterParticipants
      selected_trials <- input$filterTrials

      # Use cached simulation data if available, otherwise fall back to direct computation
      if (exists("allSimulationData") && !is.null(allSimulationData)) {
        cat("Using cached simulation data\n")
        mu_task <- get_mu_task_cached(
          allSimData = allSimulationData,
          participants = selected_participants,
          trials = selected_trials,
          slice_length = input$slice_length,
          time_col = "simulation_time"
        )
      } else {
        cat("No cached simulation data found, loading from cache file\n")
        mu_task <- get_mu_task_cached(
          participants = selected_participants,
          trials = selected_trials,
          slice_length = input$slice_length,
          time_col = "simulation_time"
        )
      }

      cat("Task metrics loaded successfully:", nrow(mu_task), "rows\n")
      cat("Task columns:", paste(colnames(mu_task), collapse = ", "), "\n")

      # Merge gait and task data
      mu_combined <- merge_mu_with_task(mu_gait, mu_task)
      cat("Combined data has", nrow(mu_combined), "rows and", ncol(mu_combined), "columns\n")

      # Show some task columns in combined data
      task_cols <- grep("^task_", colnames(mu_combined), value = TRUE)
      cat("Task columns in combined data:", paste(head(task_cols, 5), collapse = ", "), "\n")

      return(mu_combined)
    },
    error = function(e) {
      cat("Error loading task metrics:", e$message, "\n")
      cat("Returning gait data only\n")
      return(mu_gait)
    }
  )
})
