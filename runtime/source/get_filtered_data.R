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

# Reactive values for simulation data control (button-only updates with cancellation)
# These will be initialized when first accessed to ensure proper Shiny context
.simulationDataTrigger <- NULL
.simulationDataCancel <- NULL
.simulationDataCache <- NULL
.simulationDataFilters <- NULL
.simulationDataNotificationId <- NULL # Track current notification for cleanup
.simulationDataProgress <- NULL # Track loading progress
.simulationDataLoading <- NULL # Track loading state

# Helper functions to initialize reactive values on first access
simulationDataTrigger <- function(value = NULL) {
  if (is.null(.simulationDataTrigger)) {
    .simulationDataTrigger <<- reactiveVal(0)
  }
  if (is.null(value)) {
    .simulationDataTrigger()
  } else {
    .simulationDataTrigger(value)
  }
}

simulationDataCancel <- function(value = NULL) {
  if (is.null(.simulationDataCancel)) {
    .simulationDataCancel <<- reactiveVal(FALSE)
  }
  if (is.null(value)) {
    .simulationDataCancel()
  } else {
    .simulationDataCancel(value)
  }
}

simulationDataCache <- function(value = NULL) {
  if (is.null(.simulationDataCache)) {
    .simulationDataCache <<- reactiveVal(data.frame())
  }
  if (is.null(value)) {
    .simulationDataCache()
  } else {
    .simulationDataCache(value)
  }
}

simulationDataFilters <- function(value = NULL) {
  if (is.null(.simulationDataFilters)) {
    .simulationDataFilters <<- reactiveVal(list())
  }
  if (is.null(value)) {
    .simulationDataFilters()
  } else {
    .simulationDataFilters(value)
  }
}

simulationDataProgress <- function(value = NULL) {
  if (is.null(.simulationDataProgress)) {
    .simulationDataProgress <<- reactiveVal(list(current = 0, total = 0, combinations = data.frame()))
  }
  if (is.null(value)) {
    .simulationDataProgress()
  } else {
    .simulationDataProgress(value)
  }
}

simulationDataLoading <- function(value = NULL) {
  if (is.null(.simulationDataLoading)) {
    .simulationDataLoading <<- reactiveVal(FALSE)
  }
  if (is.null(value)) {
    .simulationDataLoading()
  } else {
    .simulationDataLoading(value)
  }
}

# Helper function to manage notification cleanup
clearSimulationNotifications <- function() {
  # Clear any existing notification
  if (!is.null(.simulationDataNotificationId)) {
    tryCatch(
      {
        removeNotification(.simulationDataNotificationId)
      },
      error = function(e) {
        # Ignore errors if notification doesn't exist
      }
    )
    .simulationDataNotificationId <<- NULL
  }
}

showSimulationNotification <- function(ui, type = "message", duration = NULL) {
  # Clear any existing notification first
  clearSimulationNotifications()

  # Show new notification and store ID
  .simulationDataNotificationId <<- showNotification(
    ui,
    type = type,
    duration = duration,
    closeButton = TRUE # Always allow manual dismissal
  )

  return(.simulationDataNotificationId)
}

# Function to request simulation data loading (called when update button is clicked)
request_simulation_data <- function() {
  simulationDataTrigger(simulationDataTrigger() + 1)
  simulationDataCancel(FALSE) # Reset cancel flag
  simulationDataLoading(TRUE) # Set loading state
}

# Function to cancel simulation data loading
cancel_simulation_data <- function() {
  simulationDataCancel(TRUE)
  simulationDataLoading(FALSE) # Stop loading
  # Clear any existing notifications and show cancel message
  showSimulationNotification(
    "Simulation data loading cancelled.",
    type = "warning",
    duration = 3
  )
}

# Function to reset simulation data
reset_simulation_data <- function() {
  simulationDataCache(data.frame())
  simulationDataFilters(list())
  simulationDataLoading(FALSE)
  clearSimulationNotifications() # Clear any pending notifications
}

# Background reactive for incremental data loading with cancellation support
simulationDataLoader <- reactive({
  # Check if we should be loading
  if (!simulationDataLoading()) {
    return()
  }

  # Get current progress
  progress <- simulationDataProgress()

  # Check for cancellation
  if (simulationDataCancel()) {
    simulationDataLoading(FALSE)
    return()
  }

  # If we haven't started or finished, continue loading
  if (progress$current < progress$total && progress$total > 0) {
    # Load next batch (process 5 combinations at a time for responsiveness)
    batch_size <- min(5, progress$total - progress$current)

    for (i in 1:batch_size) {
      if (simulationDataCancel()) {
        simulationDataLoading(FALSE)
        return()
      }

      current_idx <- progress$current + i
      participant <- progress$combinations$participant[current_idx]
      trial <- progress$combinations$trial[current_idx]

      # Load this combination's data
      tryCatch(
        {
          sim_data <- get_simulation_data(participant, trial)
          if (!is.null(sim_data) && nrow(sim_data) > 0) {
            # Add condition information
            sim_data$condition <- condition_number(participant)
            sim_data$participant <- participant
            sim_data$trialNum <- trial

            # Add to cached results
            cached_data <- simulationDataCache()
            if (nrow(cached_data) == 0) {
              simulationDataCache(sim_data)
            } else {
              simulationDataCache(rbind(cached_data, sim_data))
            }
          }
        },
        error = function(e) {
          warning(sprintf(
            "Error loading simulation data for participant %s, trial %s: %s",
            participant, trial, e$message
          ))
        }
      )
    }

    # Update progress
    new_progress <- progress
    new_progress$current <- progress$current + batch_size
    simulationDataProgress(new_progress)

    # Print progress to console
    cat(sprintf("Progress: %d/%d combinations loaded\n", new_progress$current, new_progress$total))

    # Continue loading if not finished and not cancelled
    if (new_progress$current < new_progress$total && !simulationDataCancel()) {
      invalidateLater(100, session = getDefaultReactiveDomain()) # Continue in 100ms
    } else if (new_progress$current >= new_progress$total) {
      # Finished loading
      simulationDataLoading(FALSE)
      showSimulationNotification(
        sprintf(
          "✓ Loaded simulation data: %d participants, %d trials, %d total samples",
          length(unique(simulationDataCache()$participant)),
          length(unique(simulationDataCache()$trialNum)),
          nrow(simulationDataCache())
        ),
        type = "message",
        duration = 5
      )
    }
  }
})

# Main simulation data reactive - simplified synchronous approach with cancellation
filteredSimulationData <- reactive({
  # Only depend on the trigger (button clicks), not on filter changes
  trigger_value <- simulationDataTrigger()

  # If trigger is 0, no update has been requested yet
  if (trigger_value == 0) {
    return(data.frame())
  }

  # Use isolate() to prevent reactive dependencies on filter inputs
  # Only read filter values when button is clicked, not when filters change
  current_filters <- isolate({
    # Check if required inputs exist
    if (is.null(input$filterParticipants) || is.null(input$filterTrials)) {
      showSimulationNotification(
        "Please select participants and trials before loading simulation data.",
        type = "warning",
        duration = 5
      )
      return(NULL)
    }

    # Get current filter state
    list(
      participants = input$filterParticipants,
      trials = input$filterTrials,
      condition = input$filterCondition
    )
  })

  # If filter validation failed, return empty data frame
  if (is.null(current_filters)) {
    return(data.frame())
  }

  # Smart caching: Check what data we already have and what we need to load
  cached_data <- simulationDataCache()
  cached_filters <- simulationDataFilters()

  # Create requested participant-trial combinations
  requested_combinations <- expand.grid(
    participant = current_filters$participants,
    trial = current_filters$trials,
    stringsAsFactors = FALSE
  )

  # If we have cached data, check what we can reuse
  reusable_data <- data.frame()
  combinations_to_load <- requested_combinations

  if (nrow(cached_data) > 0) {
    # Create identifier for existing cached combinations
    cached_data$combo_id <- paste(cached_data$participant, cached_data$trialNum, sep = "_")
    requested_combinations$combo_id <- paste(requested_combinations$participant, requested_combinations$trial, sep = "_")

    # Find which combinations we already have
    available_combos <- unique(cached_data$combo_id)
    requested_combos <- unique(requested_combinations$combo_id)
    reusable_combos <- intersect(available_combos, requested_combos)
    missing_combos <- setdiff(requested_combos, available_combos)

    # Extract reusable data
    if (length(reusable_combos) > 0) {
      reusable_data <- cached_data[cached_data$combo_id %in% reusable_combos, ]
      # Remove the temporary combo_id column
      reusable_data$combo_id <- NULL

      # Only load combinations we don't have
      combinations_to_load <- requested_combinations[requested_combinations$combo_id %in% missing_combos, ]
      # Remove the temporary combo_id column
      combinations_to_load$combo_id <- NULL
    }

    # Clean up temporary columns
    cached_data$combo_id <- NULL
    requested_combinations$combo_id <- NULL
  }

  # If we have all the data we need, return it immediately
  if (nrow(combinations_to_load) == 0 && nrow(reusable_data) > 0) {
    # Apply condition filter to reusable data
    filtered_data <- reusable_data
    if (!is.null(current_filters$condition) && length(current_filters$condition) > 0) {
      filtered_data <- filtered_data[filtered_data$condition %in% current_filters$condition, ]
    }

    showSimulationNotification(
      sprintf(
        "Using cached simulation data (%d participants, %d trials).",
        length(unique(filtered_data$participant)),
        length(unique(filtered_data$trialNum))
      ),
      type = "message",
      duration = 2
    )
    return(filtered_data)
  }

  # Reset cancel flag and start loading
  simulationDataCancel(FALSE)

  # Vectorized check for simulation data existence (only for combinations we need to load)
  valid_combinations_to_load <- data.frame()
  if (nrow(combinations_to_load) > 0) {
    has_data <- mapply(has_simulation_data, combinations_to_load$participant, combinations_to_load$trial)
    valid_combinations_to_load <- combinations_to_load[has_data, ]

    if (nrow(valid_combinations_to_load) == 0 && nrow(reusable_data) == 0) {
      showSimulationNotification(
        "No simulation data found for the selected filters.",
        type = "warning",
        duration = 5
      )
      return(data.frame())
    }
  }

  # Prepare progress information (after valid_combinations_to_load is properly set)
  reusable_count <- if (nrow(reusable_data) > 0) length(unique(paste(reusable_data$participant, reusable_data$trialNum))) else 0
  total_new_combinations <- nrow(valid_combinations_to_load)

  # Load simulation data with progress bar and built-in cancellation
  new_sim_data_list <- list()

  if (total_new_combinations > 0) {
    # Use withProgress for proper cancellation support
    progress_message <- if (reusable_count > 0) {
      sprintf("Loading %d new combinations (%d cached)", total_new_combinations, reusable_count)
    } else {
      sprintf("Loading %d combinations", total_new_combinations)
    }

    # Use tryCatch to handle user interruption (cancellation)
    new_sim_data_list <- tryCatch(
      {
        withProgress(
          message = progress_message,
          detail = "Press Esc to cancel",
          value = 0,
          {
            data_list <- list()

            for (i in seq_len(total_new_combinations)) {
              # Check for cancellation via button click
              if (simulationDataCancel()) {
                showSimulationNotification(
                  "Simulation data loading was cancelled.",
                  type = "warning",
                  duration = 3
                )
                return(data_list) # Return partial results
              }

              # Update progress
              incProgress(1 / total_new_combinations,
                detail = sprintf("Loading combination %d of %d", i, total_new_combinations)
              )

              participant <- valid_combinations_to_load$participant[i]
              trial <- valid_combinations_to_load$trial[i]

              tryCatch(
                {
                  # Pre-load cancellation check
                  if (simulationDataCancel()) {
                    return(data_list)
                  }

                  # Load simulation data (this might take a while per combination)
                  cat(sprintf("Loading P%s-T%s...\n", participant, trial))
                  sim_data <- get_simulation_data(participant, trial)
                  cat(sprintf("Completed P%s-T%s\n", participant, trial))

                  # Post-load cancellation check (in case get_simulation_data took a long time)
                  if (simulationDataCancel()) {
                    showSimulationNotification(
                      "Simulation data loading was cancelled.",
                      type = "warning",
                      duration = 3
                    )
                    return(data_list)
                  }

                  if (!is.null(sim_data) && nrow(sim_data) > 0) {
                    # Check again before processing
                    if (simulationDataCancel()) {
                      return(data_list)
                    }

                    # Add condition information
                    sim_data$condition <- condition_number(participant)
                    sim_data$participant <- participant
                    sim_data$trialNum <- trial
                    data_list[[i]] <- sim_data
                  }
                },
                error = function(e) {
                  warning(sprintf(
                    "Error loading simulation data for participant %s, trial %s: %s",
                    participant, trial, e$message
                  ))
                }
              )

              # Force UI updates and allow cancellation to be processed
              # Flush any pending reactive updates
              if (exists("shiny:::flushReact")) {
                try(shiny:::flushReact(), silent = TRUE)
              }

              # Longer delay to allow UI updates and button clicks to be processed
              Sys.sleep(0.1)
            }

            return(data_list)
          }
        )
      },
      interrupt = function(e) {
        # Handle user interruption (Esc key or browser stop)
        showSimulationNotification(
          "Simulation data loading was cancelled by user.",
          type = "warning",
          duration = 3
        )
        # Return partial data if available
        if (nrow(reusable_data) > 0) {
          return(list()) # Will combine with reusable_data later
        } else {
          stop("Loading cancelled") # This will cause the reactive to return early
        }
      }
    )
  }

  # Note: withProgress() handles cancellation automatically via user interrupt
  # If the user cancels, the code will stop executing and return to the caller

  # Remove NULL entries from newly loaded data
  new_sim_data_list <- new_sim_data_list[!sapply(new_sim_data_list, is.null)]

  # Combine reusable data with newly loaded data
  combined_data <- data.frame()

  # Start with reusable data
  if (nrow(reusable_data) > 0) {
    combined_data <- reusable_data
  }

  # Add newly loaded data
  if (length(new_sim_data_list) > 0) {
    new_data <- do.call(rbind, new_sim_data_list)
    if (nrow(combined_data) == 0) {
      combined_data <- new_data
    } else {
      combined_data <- rbind(combined_data, new_data)
    }
  }

  # Check if we have any data at all
  if (nrow(combined_data) == 0) {
    showSimulationNotification(
      "No valid simulation data found.",
      type = "warning",
      duration = 5
    )
    return(data.frame())
  }

  # Filter by condition (vectorized)
  if (!is.null(current_filters$condition) && length(current_filters$condition) > 0) {
    combined_data <- combined_data[combined_data$condition %in% current_filters$condition, ]
  }

  if (nrow(combined_data) == 0) {
    showSimulationNotification(
      "No simulation data matches the condition filter.",
      type = "warning",
      duration = 5
    )
    return(data.frame())
  }

  # Cache the results
  simulationDataCache(combined_data)
  simulationDataFilters(current_filters)

  # Show success notification with caching info
  reused_combinations <- if (nrow(reusable_data) > 0) length(unique(paste(reusable_data$participant, reusable_data$trialNum))) else 0
  new_combinations <- if (length(new_sim_data_list) > 0) nrow(valid_combinations_to_load) else 0

  if (new_combinations > 0 && reused_combinations > 0) {
    message <- sprintf(
      "✓ Loaded simulation data: %d participants, %d trials, %d samples (%d cached, %d newly loaded)",
      length(unique(combined_data$participant)),
      length(unique(combined_data$trialNum)),
      nrow(combined_data),
      reused_combinations,
      new_combinations
    )
  } else if (reused_combinations > 0) {
    message <- sprintf(
      "✓ Using cached simulation data: %d participants, %d trials, %d samples",
      length(unique(combined_data$participant)),
      length(unique(combined_data$trialNum)),
      nrow(combined_data)
    )
  } else {
    message <- sprintf(
      "✓ Loaded simulation data: %d participants, %d trials, %d samples",
      length(unique(combined_data$participant)),
      length(unique(combined_data$trialNum)),
      nrow(combined_data)
    )
  }

  showSimulationNotification(message, type = "message", duration = 5)

  return(combined_data)
})

# Observer for cancel button (needs to be added to server context where this file is sourced)
# This should be called in the server context:
# observeEvent(input$cancelSimData, { cancel_simulation_data() })

get_mu_dyn_long <- reactive({
  # Check if required global data objects are available
  if (!exists("allGaitParams") || is.null(allGaitParams) || nrow(allGaitParams) == 0) {
    return(NULL)
  }

  # Get filtered data and check if it's empty
  filtered_data <- filteredParams()
  if (nrow(filtered_data) == 0) {
    # Show user-friendly error message
    showNotification(
      "No data matches the current filter settings. Please adjust your filters in the sidebar.",
      type = "warning",
      duration = 5
    )
    # Return a minimal data frame with the expected structure to prevent crashes
    return(data.frame(
      participant = character(0),
      trialNum = numeric(0),
      condition = character(0),
      phase = character(0),
      foot = character(0)
    ))
  }

  # Get the regular gait mu data - conditionally use sliced or non-sliced version
  if (input$do_slicing) {
    # Ensure allQResults exists for sliced version
    qResults <- if (exists("allQResults") && !is.null(allQResults)) allQResults else data.frame()
    mu_gait <- get_full_mu_sliced(filtered_data, qResults, categories, input$slice_length, input$avg_feet, input$add_diff, input$remove_middle_slices)
  } else {
    mu_gait <- get_full_mu(filtered_data, categories, input$avg_feet, input$add_diff)
  }

  # Check if summarization resulted in empty data
  if (is.null(mu_gait) || nrow(mu_gait) == 0) {
    showNotification(
      "No data available after summarization. Please check your filter settings.",
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

  # Verify all required data is available
  if (!exists("allQResults") || !exists("allTaskMetrics") || !exists("allComplexityMetrics")) {
    stop("Missing required data: allQResults, allTaskMetrics, and allComplexityMetrics must all be present")
  }

  # Use new unified merge approach - prefix naming handled automatically by merge_mu_with_data
  mu <- mu_gait
  q_prepped <- prep_questionnaire_for_merge(allQResults)
  mu <- merge_mu_with_data(mu, q_prepped, "questionnaire", c("participant", "trialNum"))
  mu <- merge_mu_with_data(mu, allTaskMetrics, "task", c("participant", "trialNum"))
  mu <- merge_mu_with_data(mu, allComplexityMetrics, "complexity", c("participant", "trialNum"))

  return(mu)
})

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
