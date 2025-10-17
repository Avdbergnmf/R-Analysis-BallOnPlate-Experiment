#' Filter Manager - Intermediate Layer for Reactive/Non-Reactive Code
#'
#' This module provides a clean interface between Shiny reactive values
#' and non-reactive analysis functions. It handles the complexity of
#' managing filter states and provides both reactive and non-reactive access.
#'
#' @author Alex van den Berg
#' @version 1.0

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
  return(.filter_state)
}

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
  get_filtered_params(
    participants = .filter_state$participants,
    trials = .filter_state$trials,
    conditions = .filter_state$conditions,
    phases = .filter_state$phases,
    outliers = .filter_state$outliers,
    suspect = .filter_state$suspect,
    sides = .filter_state$sides
  )
}

#' Get mu dynamic long data using current filter state
#' @return Processed mu data frame
get_current_mu_dyn_long <- function() {
  get_mu_dyn_long_data(
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
}

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
      get_current_mu_dyn_long()
    })
  )
}
