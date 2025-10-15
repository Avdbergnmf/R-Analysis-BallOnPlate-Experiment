#' Data Caching Setup
#'
#' This file contains the setup of data loaders and validators for specific data types.
#' Add new data types here by defining their loader and validator functions.

# =============================================================================
# CONFIGURATION
# =============================================================================

#' Configuration for context variables relevant to each data type
#' Context variables are passed to workers and can include:
#' - input.* : Shiny input values (e.g., "input.rawTracker", "input.risk_tau")
#' - cache.* : References to cached data (e.g., "cache.hazard" for hazard samples)
#' - model.* : Model references for smart loading (e.g., "model.risk" for risk model)
context_vars_config <<- list(
  "raw_tracker" = character(0), # No context variables needed - directly accesses input$rawTracker
  "power_spectrum" = character(0), # No context variables needed - directly accesses input$psTracker, input$psVar, input$psMaxFreq
  "simulation" = c("model.risk", "cache.hazard_preds"), # Need risk model and hazard predictions for risk scoring
  "hazard" = character(0), # No context variables needed - directly accesses input$risk_tau and input$sampling_freq
  "hazard_preds" = character(0) # No context variables needed - this is a standalone transformation
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
          ps_logger("ERROR", "Error computing power spectrum for participant", participant, "trial", trial, ":", e$message)
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
  ),
  "hazard" = list(
    loader = function(participant, trial) {
      hazard_logger <- create_module_logger("HAZARD-LOADER")
      hazard_logger("DEBUG", "=== hazard loader called ===")
      hazard_logger("DEBUG", "participant:", participant, "trial:", trial)

      # Get risk model parameters from Shiny inputs
      tau <- 0.2 # Default value
      sampling_freq <- 90 # Default value

      if (exists("input")) {
        if (!is.null(input$risk_tau)) {
          tau <- input$risk_tau
        }
        if (!is.null(input$sampling_freq)) {
          sampling_freq <- input$sampling_freq
        }
      }

      hazard_logger("DEBUG", "Using parameters - tau:", tau, "sampling_freq:", sampling_freq)

      # Load simulation data first
      sim_data <- get_simulation_data(participant, trial, enable_risk = FALSE)
      if (is.null(sim_data) || nrow(sim_data) == 0) {
        hazard_logger("WARN", "No simulation data found for participant", participant, "trial", trial)
        return(data.frame())
      }

      # Apply episode-based resampling to simulation data if requested
      if (sampling_freq < 90) {
        hazard_logger("DEBUG", "Applying resampling with frequency:", sampling_freq)
        sim_data <- resample_simulation_per_episode(sim_data, sampling_freq)
      }

      # Build hazard samples from simulation data
      hazard_samples <- build_hazard_samples(sim_data, tau)
      if (nrow(hazard_samples) == 0) {
        hazard_logger("WARN", "No hazard samples generated for participant", participant, "trial", trial)
        return(data.frame())
      }

      # Add participant and trial identifiers
      hazard_samples$participant <- participant
      hazard_samples$trial <- trial

      hazard_logger("DEBUG", "Generated", nrow(hazard_samples), "hazard samples")
      return(hazard_samples)
    },
    datasets_to_verify = c("sim") # Hazard samples depend on simulation data
  ),
  "hazard_preds" = list(
    loader = function(participant, trial) {
      # This loader is not used - hazard_preds are created by annotate_hazard_predictions()
      # and stored directly in cache. This is just a placeholder for the data type.
      return(data.frame())
    },
    datasets_to_verify = character(0) # No datasets needed - this is a transformation of cached data
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

#' Get relevant context variables for a specific data type
#' @param data_type The data type
#' @return Vector of context variable names that are relevant for this data type
get_relevant_context_vars <- function(data_type) {
  # Get relevant context variables from centralized configuration
  relevant_vars <- context_vars_config[[data_type]]

  # Return empty vector if not found
  if (is.null(relevant_vars)) {
    return(character(0))
  }

  return(relevant_vars)
}

#' Capture context variables for a specific data type
#' @param data_type The data type
#' @return List of context variables, or empty list if no context variables available
capture_context_vars <- function(data_type) {
  relevant_vars <- get_relevant_context_vars(data_type)
  context_vars <- list()

  if (length(relevant_vars) > 0) {
    for (var_name in relevant_vars) {
      if (startsWith(var_name, "input.")) {
        # Extract input name (e.g., "input.risk_tau" -> "risk_tau")
        input_name <- substring(var_name, 7) # Remove "input." prefix
        if (exists("input") && !is.null(input[[input_name]])) {
          context_vars[[var_name]] <- input[[input_name]]
        }
      } else if (startsWith(var_name, "cache.")) {
        # Extract cache data type (e.g., "cache.hazard" -> "hazard")
        cache_type <- substring(var_name, 7) # Remove "cache." prefix
        cache_data <- get_current_cache(cache_type)
        if (nrow(cache_data) > 0) {
          context_vars[[var_name]] <- cache_data
        }
      } else if (startsWith(var_name, "model.")) {
        # Extract model name (e.g., "model.risk" -> "risk")
        model_name <- substring(var_name, 7) # Remove "model." prefix

        # Create a lightweight model reference for smart loading
        context_vars[[var_name]] <- list(
          type = "model_reference",
          model_name = model_name,
          model_path = get_model_cache_path(model_name)
        )
      }
    }
  }

  return(context_vars)
}

#' Get the cache path for a specific model
#' @param model_name The name of the model (e.g., "risk")
#' @return The path to the cached model file
get_model_cache_path <- function(model_name = "risk") {
  # Use the same cache folder as other cached data
  cache_folder <- if (exists("cacheFolder", envir = .GlobalEnv)) {
    get("cacheFolder", envir = .GlobalEnv)
  } else {
    "cache"
  }

  # Generate model filename based on model name
  model_filename <- paste0(model_name, "_model.rds")
  return(file.path(cache_folder, model_filename))
}


#' Add a new data type to the system
#' @param data_type The name of the data type
#' @param loader_function Function that loads data for a participant-trial combination
#' @param datasets_to_verify Vector of dataset names to verify (optional, defaults to c("sim"))
#' @param context_vars Vector of context variable names that affect this data type (optional, defaults to character(0))
#' @param relevant_inputs Vector of Shiny input names that affect this data type (optional, defaults to character(0), for backward compatibility)
add_data_type <- function(data_type, loader_function, datasets_to_verify = c("sim"), context_vars = character(0), relevant_inputs = character(0)) {
  if (!is.function(loader_function)) {
    stop("loader_function must be a function")
  }

  # Create the new data type entry
  data_loaders[[data_type]] <<- list(
    loader = loader_function,
    datasets_to_verify = datasets_to_verify
  )

  # Convert old-style relevant_inputs to new context_vars format for backward compatibility
  if (length(relevant_inputs) > 0) {
    input_context_vars <- paste0("input.", relevant_inputs)
    context_vars <- c(context_vars, input_context_vars)
  }

  # Update the context variables configuration
  context_vars_config[[data_type]] <<- context_vars

  cat(sprintf("Added data type '%s' to caching system\n", data_type))
  if (length(context_vars) > 0) {
    cat(sprintf("  Relevant context variables: %s\n", paste(context_vars, collapse = ", ")))
  }
}

#' Remove a data type from the system
#' @param data_type The name of the data type to remove
remove_data_type <- function(data_type) {
  if (data_type %in% names(data_loaders)) {
    data_loaders[[data_type]] <<- NULL
  }

  # Remove from context variables configuration
  if (data_type %in% names(context_vars_config)) {
    context_vars_config[[data_type]] <<- NULL
  }

  cat(sprintf("Removed data type '%s' from caching system\n", data_type))
}
