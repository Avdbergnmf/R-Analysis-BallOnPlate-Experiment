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

# New function to collect simulation data based on sidebar filters
filteredSimulationData <- reactive({
  # Force dependency on `refresh_trigger()`
  refresh_trigger()

  # Check if required inputs exist
  if (is.null(input$filterParticipants) || is.null(input$filterTrials)) {
    return(data.frame())
  }

  # Check if simulation data has been explicitly requested via the update button
  # We'll use a reactive value to track if the update button has been clicked
  if (!exists("simulation_data_requested", envir = .GlobalEnv) || !get("simulation_data_requested", envir = .GlobalEnv)) {
    return(data.frame())
  }

  # Show progress notification
  showNotification(
    "Loading simulation data... This may take a while for large datasets.",
    type = "message",
    duration = 3
  )

  # Create all participant-trial combinations that match filters
  combinations <- expand.grid(
    participant = input$filterParticipants,
    trial = input$filterTrials,
    stringsAsFactors = FALSE
  )

  # Vectorized check for simulation data existence
  has_data <- mapply(has_simulation_data, combinations$participant, combinations$trial)
  valid_combinations <- combinations[has_data, ]

  if (nrow(valid_combinations) == 0) {
    showNotification("No simulation data found for the selected filters.", type = "warning", duration = 5)
    return(data.frame())
  }

  # Load all simulation data in parallel using lapply
  sim_data_list <- lapply(seq_len(nrow(valid_combinations)), function(i) {
    participant <- valid_combinations$participant[i]
    trial <- valid_combinations$trial[i]

    tryCatch(
      {
        sim_data <- get_simulation_data(participant, trial)
        if (!is.null(sim_data) && nrow(sim_data) > 0) {
          # Add condition information (assuming condition_number function exists)
          sim_data$condition <- condition_number(participant)
          sim_data$participant <- participant
          sim_data$trialNum <- trial
          return(sim_data)
        }
        return(NULL)
      },
      error = function(e) {
        warning(sprintf(
          "Error loading simulation data for participant %s, trial %s: %s",
          participant, trial, e$message
        ))
        return(NULL)
      }
    )
  })

  # Remove NULL entries and combine
  sim_data_list <- sim_data_list[!sapply(sim_data_list, is.null)]

  if (length(sim_data_list) == 0) {
    showNotification("No valid simulation data found.", type = "warning", duration = 5)
    return(data.frame())
  }

  # Combine all data
  combined_data <- do.call(rbind, sim_data_list)

  # Filter by condition (vectorized)
  if (!is.null(input$filterCondition) && length(input$filterCondition) > 0) {
    combined_data <- combined_data[combined_data$condition %in% input$filterCondition, ]
  }

  if (nrow(combined_data) == 0) {
    showNotification("No simulation data matches the condition filter.", type = "warning", duration = 5)
    return(data.frame())
  }

  # Show success notification
  showNotification(
    sprintf(
      "Loaded simulation data: %d participants, %d trials, %d total samples",
      length(unique(combined_data$participant)),
      length(unique(combined_data$trialNum)),
      nrow(combined_data)
    ),
    type = "message",
    duration = 5
  )

  return(combined_data)
})

# Function to request simulation data loading (called when update button is clicked)
request_simulation_data <- function() {
  assign("simulation_data_requested", TRUE, envir = .GlobalEnv)
}

# Function to reset simulation data request flag
reset_simulation_data_request <- function() {
  assign("simulation_data_requested", FALSE, envir = .GlobalEnv)
}

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
