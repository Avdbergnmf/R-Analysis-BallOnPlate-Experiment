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

# Make filter manager functions available globally
filter_manager_logger("INFO", "Making get_filter_state globally available")
get_filter_state <<- get_filter_state

#' Update filter state from Shiny inputs
#' @param input Shiny input object
update_filter_state <- function(input) {
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
# DATA ACCESS FUNCTIONS
# =============================================================================

#' Get filtered parameters using current filter state
#' @return Filtered data frame
get_current_filtered_params <- function() {
  filter_manager_logger("DEBUG", "get_current_filtered_params called")
  filter_manager_logger("DEBUG", sprintf("Filter state: participants=%s, trials=%s, conditions=%s", 
    if(is.null(.filter_state$participants)) "NULL" else length(.filter_state$participants),
    if(is.null(.filter_state$trials)) "NULL" else length(.filter_state$trials),
    if(is.null(.filter_state$conditions)) "NULL" else length(.filter_state$conditions)
  ))
  
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

# Make filter manager functions available globally
filter_manager_logger("INFO", "Making get_current_filtered_params globally available")
get_current_filtered_params <<- get_current_filtered_params

#' Get mu dynamic long data using current filter state
#' @return Processed mu data frame
get_current_mu_dyn_long <- function() {
  filter_manager_logger("DEBUG", "get_current_mu_dyn_long called")
  filter_manager_logger("DEBUG", sprintf("Filter state: participants=%s, trials=%s, conditions=%s, do_slicing=%s", 
    if(is.null(.filter_state$participants)) "NULL" else length(.filter_state$participants),
    if(is.null(.filter_state$trials)) "NULL" else length(.filter_state$trials),
    if(is.null(.filter_state$conditions)) "NULL" else length(.filter_state$conditions),
    .filter_state$do_slicing
  ))
  
  result <- get_mu_dyn_long_data(
    participants = .filter_state$participants,
    trials = .filter_state$trials,
    conditions = .filter_state$conditions,
    phases = .filter_state$phases,
    outliers = .filter_state$outliers,
    suspect = .filter_state$suspect,
    sides = .filter_state$sides,
    do_slicing = .filter_state$do_slicing,
    slice_length = .filter_state$slice_length,
    avg_feet = .filter_state$avg_feet,
    add_diff = .filter_state$add_diff,
    remove_middle_slices = .filter_state$remove_middle_slices
  )
  
  filter_manager_logger("DEBUG", sprintf("get_current_mu_dyn_long returning %d rows", nrow(result)))
  return(result)
}

# Make filter manager functions available globally
filter_manager_logger("INFO", "Making get_current_mu_dyn_long globally available")
get_current_mu_dyn_long <<- get_current_mu_dyn_long

# =============================================================================
# SHINY INTEGRATION
# =============================================================================

#' Create reactive filter state updater
#' @return Reactive function that updates filter state
create_filter_updater <- function() {
  reactive({
    # This reactive will run whenever any input changes
    # and update the global filter state
    update_filter_state(input)
  })
}

#' Create reactive filtered data accessors
#' @return List of reactive functions for data access
create_filtered_data_reactives <- function() {
  list(
    # Reactive version that uses current filter state
    filteredParams = reactive({
      refresh_trigger()  # Force dependency on refresh trigger
      get_current_filtered_params()
    }),
    
    # Reactive version that uses current filter state
    get_mu_dyn_long = reactive({
      refresh_trigger()  # Force dependency on refresh trigger
      get_current_mu_dyn_long()
    })
  )
}
