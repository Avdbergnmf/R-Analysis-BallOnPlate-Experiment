#' Data Caching Setup
#'
#' This file contains the setup of data loaders and validators for specific data types.
#' Add new data types here by defining their loader and validator functions.

# =============================================================================
# CONFIGURATION
# =============================================================================

#' Configuration for Shiny inputs relevant to each data type
#' Add new data types and their relevant inputs here
shiny_inputs_config <<- list(
  "raw_tracker" = c("rawTracker"),
  "power_spectrum" = c("psTracker", "psVar", "psMaxFreq"),
  "simulation" = character(0) # No Shiny inputs needed
)

# =============================================================================
# DATA LOADER FUNCTIONS
# =============================================================================

#' Define data loader functions for different data types
#' Each function should take (participant, trial) and return a data.frame
data_loaders <<- list(
  "simulation" = list(
    loader = function(participant, trial) {
      # Load simulation data for a specific participant and trial
      sim_data <- get_simulation_data(participant, trial)
      if (!is.null(sim_data) && nrow(sim_data) > 0) {
      }
      return(sim_data)
    },
    datasets_to_verify = c("sim")
  ),
  "power_spectrum" = list(
    loader = function(participant, trial) {
      # Load power spectrum data for a specific participant and trial
      # This will be called from Shiny context where input$psTracker and input$psVar are available
      if (!exists("input") || is.null(input$psTracker) || is.null(input$psVar)) {
        return(data.frame())
      }

      # Load raw tracker data first
      data <- get_t_data(participant, input$psTracker, trial)
      if (is.null(data) || nrow(data) == 0) {
        return(data.frame())
      }

      # Check if we have the required variable
      if (!input$psVar %in% names(data)) {
        return(data.frame())
      }

      # Compute power spectrum for this participant-trial combination
      tryCatch(
        {
          # Clean data
          valid_mask <- is.finite(data[[input$psVar]]) & is.finite(data$time)
          values <- data[[input$psVar]][valid_mask]
          times <- data$time[valid_mask]

          if (length(values) < 50) {
            return(data.frame())
          }

          # Calculate sampling frequency
          if (length(times) > 1) {
            dt <- median(diff(times), na.rm = TRUE)
            fs <- if (is.finite(dt) && dt > 0) 1 / dt else 120
          } else {
            fs <- 120 # Default
          }

          # Compute PSD
          psd_df <- compute_single_psd(
            values,
            fs = fs,
            max_freq = input$psMaxFreq,
            normalize = TRUE
          )

          if (is.null(psd_df)) {
            return(data.frame())
          }


          return(psd_df)
        },
        error = function(e) {
          warning(sprintf(
            "Error computing power spectrum for participant %s, trial %s: %s",
            participant, trial, e$message
          ))
          return(data.frame())
        }
      )
    },
    datasets_to_verify = function() {
      # Dynamic datasets_to_verify based on selected tracker
      if (!exists("input") || is.null(input$psTracker)) {
        return(c("leftfoot", "rightfoot", "hip")) # Default fallback
      }
      return(input$psTracker)
    }
  ),
  "raw_tracker" = list(
    loader = function(participant, trial) {
      debug_logger <- create_module_logger("RAW-TRACKER-LOADER")
      debug_logger("DEBUG", "=== raw_tracker loader called ===")
      debug_logger("DEBUG", "participant:", participant, "trial:", trial)
      debug_logger("DEBUG", "input exists:", exists("input"))
      if (exists("input")) {
        debug_logger("DEBUG", "input$rawTracker exists:", !is.null(input$rawTracker))
        if (!is.null(input$rawTracker)) {
          debug_logger("DEBUG", "input$rawTracker value:", input$rawTracker)
        }
      }

      # Load raw tracker data for a specific participant and trial
      # This will be called from Shiny context where input$rawTracker is available
      if (!exists("input") || is.null(input$rawTracker)) {
        debug_logger("WARN", "input$rawTracker not available, returning empty data frame")
        return(data.frame())
      }

      debug_logger("DEBUG", "About to call get_t_data with tracker:", input$rawTracker)
      # Load raw tracker data using get_t_data
      tracker_data <- get_t_data(participant, input$rawTracker, trial)
      debug_logger("DEBUG", "get_t_data returned:", if (is.null(tracker_data)) "NULL" else paste(nrow(tracker_data), "rows"))

      if (is.null(tracker_data) || nrow(tracker_data) == 0) {
        debug_logger("WARN", "No tracker data found, returning empty data frame")
        return(data.frame())
      }

      debug_logger("DEBUG", "Returning tracker data with", nrow(tracker_data), "rows")
      # Metadata (participant, trialNum, condition) is added by the load_or_calc loop system
      return(tracker_data)
    },
    datasets_to_verify = function() {
      debug_logger <- create_module_logger("RAW-TRACKER-DATASETS")
      debug_logger("DEBUG", "=== raw_tracker datasets_to_verify called ===")
      debug_logger("DEBUG", "input exists:", exists("input"))
      if (exists("input")) {
        debug_logger("DEBUG", "input$rawTracker exists:", !is.null(input$rawTracker))
        if (!is.null(input$rawTracker)) {
          debug_logger("DEBUG", "input$rawTracker value:", input$rawTracker)
        }
      }

      # Dynamic datasets_to_verify based on selected tracker
      if (!exists("input") || is.null(input$rawTracker)) {
        debug_logger("WARN", "input$rawTracker not available, using default fallback")
        return(c("leftfoot", "rightfoot", "hip")) # Default fallback
      }
      debug_logger("DEBUG", "Returning datasets_to_verify:", input$rawTracker)
      return(input$rawTracker)
    }
  )

  # Add more data loaders here as needed
  # "gait" = function(participant, trial) {
  #   # Load gait data for a specific participant and trial
  #   gait_data <- get_gait_data(participant, trial)
  #   if (!is.null(gait_data) && nrow(gait_data) > 0) {
  #     gait_data$condition <- condition_number(participant)
  #     gait_data$participant <- participant
  #     gait_data$trialNum <- trial
  #   }
  #   return(gait_data)
  # },
  #
  # "questionnaire" = function(participant, trial) {
  #   # Load questionnaire data for a specific participant and trial
  #   q_data <- get_questionnaire_data(participant, trial)
  #   if (!is.null(q_data) && nrow(q_data) > 0) {
  #     q_data$condition <- condition_number(participant)
  #     q_data$participant <- participant
  #     q_data$trialNum <- trial
  #   }
  #   return(q_data)
  # }
)


# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Get list of available data types
#' @return Vector of data type names
get_available_data_types <- function() {
  return(names(data_loaders))
}

#' Check if a data type is supported
#' @param data_type The data type to check
#' @return TRUE if supported, FALSE otherwise
is_data_type_supported <- function(data_type) {
  return(data_type %in% names(data_loaders))
}

#' Get data loader for a specific data type
#' @param data_type The data type
#' @return The loader function or NULL if not found
get_data_loader <- function(data_type) {
  loader_info <- data_loaders[[data_type]]
  if (is.null(loader_info)) {
    return(NULL)
  }

  # Handle both old format (direct function) and new format (list with loader)
  if (is.function(loader_info)) {
    return(loader_info)
  } else if (is.list(loader_info) && !is.null(loader_info$loader)) {
    return(loader_info$loader)
  }

  return(NULL)
}

#' Get datasets to verify for a specific data type
#' @param data_type The data type
#' @return Vector of dataset names to verify or NULL if not found
get_datasets_to_verify <- function(data_type) {
  loader_info <- data_loaders[[data_type]]
  if (is.null(loader_info)) {
    return(NULL)
  }

  # Handle both old format (no datasets_to_verify) and new format (list with datasets_to_verify)
  if (is.function(loader_info)) {
    return(c("sim")) # Default fallback for old format
  } else if (is.list(loader_info) && !is.null(loader_info$datasets_to_verify)) {
    datasets_to_verify <- loader_info$datasets_to_verify
    # Handle both static vector and dynamic function
    if (is.function(datasets_to_verify)) {
      return(datasets_to_verify())
    } else {
      return(datasets_to_verify)
    }
  }

  return(c("sim")) # Default fallback
}

#' Get relevant Shiny inputs for a specific data type
#' @param data_type The data type
#' @return Vector of input names that are relevant for this data type
get_relevant_shiny_inputs <- function(data_type) {
  # Get relevant inputs from centralized configuration
  relevant_inputs <- shiny_inputs_config[[data_type]]

  # Return empty vector if not found
  if (is.null(relevant_inputs)) {
    return(character(0))
  }

  return(relevant_inputs)
}

#' Capture current Shiny input values for a specific data type
#' @param data_type The data type
#' @return List of current input values, or empty list if no inputs available
capture_shiny_inputs <- function(data_type) {
  relevant_inputs <- get_relevant_shiny_inputs(data_type)
  shiny_inputs <- list()

  if (exists("input") && length(relevant_inputs) > 0) {
    for (input_name in relevant_inputs) {
      if (!is.null(input[[input_name]])) {
        shiny_inputs[[input_name]] <- input[[input_name]]
      }
    }
  }

  return(shiny_inputs)
}


#' Add a new data type to the system
#' @param data_type The name of the data type
#' @param loader_function Function that loads data for a participant-trial combination
#' @param datasets_to_verify Vector of dataset names to verify (optional, defaults to c("sim"))
#' @param relevant_inputs Vector of Shiny input names that affect this data type (optional, defaults to character(0))
add_data_type <- function(data_type, loader_function, datasets_to_verify = c("sim"), relevant_inputs = character(0)) {
  if (!is.function(loader_function)) {
    stop("loader_function must be a function")
  }

  # Create the new data type entry
  data_loaders[[data_type]] <<- list(
    loader = loader_function,
    datasets_to_verify = datasets_to_verify
  )

  # Update the shiny inputs configuration
  shiny_inputs_config[[data_type]] <<- relevant_inputs

  cat(sprintf("Added data type '%s' to caching system\n", data_type))
  if (length(relevant_inputs) > 0) {
    cat(sprintf("  Relevant Shiny inputs: %s\n", paste(relevant_inputs, collapse = ", ")))
  }
}

#' Remove a data type from the system
#' @param data_type The name of the data type to remove
remove_data_type <- function(data_type) {
  if (data_type %in% names(data_loaders)) {
    data_loaders[[data_type]] <<- NULL
  }

  # Also remove from shiny inputs configuration
  if (data_type %in% names(shiny_inputs_config)) {
    shiny_inputs_config[[data_type]] <<- NULL
  }

  cat(sprintf("Removed data type '%s' from caching system\n", data_type))
}
