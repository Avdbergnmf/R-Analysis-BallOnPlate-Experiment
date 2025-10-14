#' Data Caching System
#'
#' A flexible caching system that can be applied to different types of datasets.
#' Supports RDS file persistence, incremental loading, and Shiny integration.

# =============================================================================
# GLOBAL CACHE STORAGE
# =============================================================================

# Global cache storage for different data types
# Structure: list(type_name = list(data = data.frame(), filters = list()))
global_data_cache <<- list()

# =============================================================================
# SHINY INTEGRATION FUNCTIONS
# =============================================================================

#' Create a Shiny-compatible cache manager
#' @param cache_name Unique name for this cache (e.g., "simulation", "power_spectrum")
#' @return List of functions to manage the cache with Shiny integration
create_shiny_cache_manager <- function(cache_name) {
  # Create unique variable names for this cache
  trigger_var <- paste0(".", cache_name, "DataTrigger")
  cancel_var <- paste0(".", cache_name, "DataCancel")
  cache_var <- paste0(".", cache_name, "DataCache")
  filters_var <- paste0(".", cache_name, "DataFilters")
  notification_var <- paste0(".", cache_name, "DataNotificationId")
  loading_var <- paste0(".", cache_name, "DataLoading")
  
  # Initialize reactive values on first access
  trigger <- function(value = NULL) {
    if (!exists(trigger_var, envir = .GlobalEnv) || is.null(get(trigger_var, envir = .GlobalEnv))) {
      assign(trigger_var, reactiveVal(0), envir = .GlobalEnv)
    }
    trigger_val <- get(trigger_var, envir = .GlobalEnv)
    if (is.null(value)) {
      trigger_val()
    } else {
      trigger_val(value)
    }
  }
  
  cancel <- function(value = NULL) {
    if (!exists(cancel_var, envir = .GlobalEnv) || is.null(get(cancel_var, envir = .GlobalEnv))) {
      assign(cancel_var, reactiveVal(FALSE), envir = .GlobalEnv)
    }
    cancel_val <- get(cancel_var, envir = .GlobalEnv)
    if (is.null(value)) {
      cancel_val()
    } else {
      cancel_val(value)
    }
  }
  
  cache <- function(value = NULL) {
    if (!exists(cache_var, envir = .GlobalEnv) || is.null(get(cache_var, envir = .GlobalEnv))) {
      assign(cache_var, reactiveVal(data.frame()), envir = .GlobalEnv)
    }
    cache_val <- get(cache_var, envir = .GlobalEnv)
    if (is.null(value)) {
      cache_val()
    } else {
      cache_val(value)
    }
  }
  
  filters <- function(value = NULL) {
    if (!exists(filters_var, envir = .GlobalEnv) || is.null(get(filters_var, envir = .GlobalEnv))) {
      assign(filters_var, reactiveVal(list()), envir = .GlobalEnv)
    }
    filters_val <- get(filters_var, envir = .GlobalEnv)
    if (is.null(value)) {
      filters_val()
    } else {
      filters_val(value)
    }
  }
  
  loading <- function(value = NULL) {
    if (!exists(loading_var, envir = .GlobalEnv) || is.null(get(loading_var, envir = .GlobalEnv))) {
      assign(loading_var, reactiveVal(FALSE), envir = .GlobalEnv)
    }
    loading_val <- get(loading_var, envir = .GlobalEnv)
    if (is.null(value)) {
      loading_val()
    } else {
      loading_val(value)
    }
  }
  
  # Notification management
  clear_notifications <- function() {
    if (exists(notification_var, envir = .GlobalEnv)) {
      notification_id <- get(notification_var, envir = .GlobalEnv)
      if (!is.null(notification_id)) {
        tryCatch(
          {
            removeNotification(notification_id)
          },
          error = function(e) {
            # Ignore errors if notification doesn't exist
          }
        )
        assign(notification_var, NULL, envir = .GlobalEnv)
      }
    }
  }
  
  show_notification <- function(ui, type = "message", duration = NULL) {
    clear_notifications()
    notification_id <- showNotification(
      ui,
      type = type,
      duration = duration,
      closeButton = TRUE
    )
    assign(notification_var, notification_id, envir = .GlobalEnv)
    return(notification_id)
  }
  
  # Control functions
  request_data <- function() {
    trigger(trigger() + 1)
    cancel(FALSE)
    loading(TRUE)
  }
  
  cancel_data <- function() {
    cancel(TRUE)
    loading(FALSE)
    show_notification(
      paste(cache_name, "data loading cancelled."),
      type = "warning",
      duration = 3
    )
  }
  
  reset_data <- function() {
    cache(data.frame())
    filters(list())
    loading(FALSE)
    clear_notifications()
  }
  
  # Return the manager functions
  list(
    trigger = trigger,
    cancel = cancel,
    cache = cache,
    filters = filters,
    loading = loading,
    clear_notifications = clear_notifications,
    show_notification = show_notification,
    request_data = request_data,
    cancel_data = cancel_data,
    reset_data = reset_data
  )
}

#' Create a Shiny reactive for cached data with progress and cancellation
#' @param cache_manager Cache manager created with create_shiny_cache_manager
#' @param data_type The type of data (e.g., "simulation", "power_spectrum")
#' @return Reactive that returns cached data
create_shiny_cached_data_reactive <- function(cache_manager, data_type) {
  reactive({
    # Only depend on the trigger (button clicks), not on filter changes
    trigger_value <- cache_manager$trigger()
    
    # If trigger is 0, no update has been requested yet - return empty data frame
    if (trigger_value == 0) {
      return(data.frame())
    }
    
    # Use isolate() to prevent reactive dependencies on filter inputs
    # Use the universal filter function from sidebar
    current_filters <- isolate(get_universal_filters())
    
    # If filter validation failed, return empty data frame
    if (is.null(current_filters)) {
      cache_manager$show_notification(
        paste("Please select participants and trials before loading", data_type, "data."),
        type = "warning",
        duration = 5
      )
      return(data.frame())
    }
    
    # Use the main caching system to get data
    tryCatch({
      # Get cached data using the main system
      cached_data <- get_cached_data(
        data_type = data_type,
        participants = current_filters$participants,
        trials = current_filters$trials,
        condition_filter = current_filters$condition
      )
      
      # Update Shiny cache manager state
      cache_manager$cache(cached_data)
      cache_manager$filters(current_filters)
      cache_manager$loading(FALSE)
      
      # Show success notification
      if (nrow(cached_data) > 0) {
        cache_manager$show_notification(
          sprintf(
            "✓ Loaded %s data: %d participants, %d trials, %d samples",
            data_type,
            length(unique(cached_data$participant)),
            length(unique(cached_data$trialNum)),
            nrow(cached_data)
          ),
          type = "message",
          duration = 3
        )
      } else {
        cache_manager$show_notification(
          paste("No", data_type, "data found for the selected filters."),
          type = "warning",
          duration = 5
        )
      }
      
      return(cached_data)
      
    }, error = function(e) {
      cache_manager$loading(FALSE)
      cache_manager$show_notification(
        paste("Error loading", data_type, "data:", e$message),
        type = "error",
        duration = 5
      )
      return(data.frame())
    })
  })
}

# =============================================================================
# DATA TYPE SETUP
# =============================================================================

# Source the data type setup file to load data loaders and validators
source("source/data_caching_setup.R", local = FALSE)

# =============================================================================
# CACHE FILE PATHS
# =============================================================================

#' Get cache file path for a specific data type
#' @param data_type The type of data (e.g., "simulation")
#' @return Path to the RDS cache file
get_cache_file_path <- function(data_type) {
  ensure_global_data_initialized()
  cache_filename <- paste0(data_type, "_cache.rds")
  return(file.path(dataExtraFolder, cache_filename))
}

# =============================================================================
# CACHE MANAGEMENT FUNCTIONS
# =============================================================================

#' Load cached data from RDS file
#' @param data_type The type of data to load
#' @return The cached data or empty data.frame if not found
load_cache_from_file <- function(data_type) {
  cache_path <- get_cache_file_path(data_type)
  
  if (file.exists(cache_path)) {
    tryCatch({
      cached_data <- readRDS(cache_path)
      cat(sprintf("[CACHE] Loaded %s cache from file: %d rows\n", data_type, nrow(cached_data)))
      return(cached_data)
    }, error = function(e) {
      warning(sprintf("Error loading cache file %s: %s", cache_path, e$message))
      return(data.frame())
    })
  } else {
    cat(sprintf("[CACHE] No cache file found for %s at %s\n", data_type, cache_path))
    return(data.frame())
  }
}

#' Save data to RDS cache file
#' @param data_type The type of data
#' @param data The data to save
save_cache_to_file <- function(data_type, data) {
  cache_path <- get_cache_file_path(data_type)
  
  tryCatch({
    saveRDS(data, cache_path)
    cat(sprintf("[CACHE] Saved %s cache to file: %d rows\n", data_type, nrow(data)))
  }, error = function(e) {
    warning(sprintf("Error saving cache file %s: %s", cache_path, e$message))
  })
}

#' Get current cache for a data type
#' @param data_type The type of data
#' @return The cached data or empty data.frame if not found
get_current_cache <- function(data_type) {
  if (data_type %in% names(global_data_cache)) {
    return(global_data_cache[[data_type]]$data)
  } else {
    return(data.frame())
  }
}

#' Set cache for a data type
#' @param data_type The type of data
#' @param data The data to cache
#' @param filters The filters used to generate this data
set_cache <- function(data_type, data, filters = list()) {
  global_data_cache[[data_type]] <<- list(
    data = data,
    filters = filters
  )
}

#' Clear cache for a data type
#' @param data_type The type of data
clear_cache <- function(data_type) {
  if (data_type %in% names(global_data_cache)) {
    global_data_cache[[data_type]] <<- NULL
  }
}


# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Check if data exists for a participant-trial combination
#' @param data_type The type of data
#' @param participant The participant ID
#' @param trial The trial number
#' @return TRUE if data exists, FALSE otherwise
has_data <- function(data_type, participant, trial) {
  validator <- data_validators[[data_type]]
  if (is.null(validator)) {
    warning(sprintf("No validator defined for data type: %s", data_type))
    return(FALSE)
  }
  
  tryCatch({
    return(validator(participant, trial))
  }, error = function(e) {
    warning(sprintf("Error validating %s data for participant %s, trial %s: %s", 
                   data_type, participant, trial, e$message))
  return(FALSE)
  })
}

#' Get all available participant-trial combinations for a data type
#' @param data_type The type of data
#' @param participants Vector of participant IDs to check
#' @param trials Vector of trial numbers to check
#' @return Data frame with participant and trial columns for available combinations
get_available_combinations <- function(data_type, participants, trials) {
  # Create all possible combinations
  all_combinations <- expand.grid(
    participant = participants,
    trial = trials,
    stringsAsFactors = FALSE
  )
  
  # Check which combinations have data
  has_data_mask <- mapply(has_data, data_type, all_combinations$participant, all_combinations$trial)
  available_combinations <- all_combinations[has_data_mask, ]
  
  return(available_combinations)
}

#' Load missing data for a data type
#' @param data_type The type of data
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param existing_data Existing data to avoid reloading
#' @return List of newly loaded data frames
load_missing_data <- function(data_type, participants, trials, existing_data = data.frame()) {
  # Get available combinations
  available_combinations <- get_available_combinations(data_type, participants, trials)
  
  if (nrow(available_combinations) == 0) {
    cat(sprintf("[CACHE] No data available for %s with given participants/trials\n", data_type))
    return(list())
  }
  
  # Check which combinations we already have
  if (nrow(existing_data) > 0) {
    existing_combinations <- unique(paste(existing_data$participant, existing_data$trialNum, sep = "_"))
    available_combinations$combo_id <- paste(available_combinations$participant, available_combinations$trial, sep = "_")
    missing_combinations <- available_combinations[!available_combinations$combo_id %in% existing_combinations, ]
    missing_combinations$combo_id <- NULL
  } else {
    missing_combinations <- available_combinations
  }
  
  if (nrow(missing_combinations) == 0) {
    cat(sprintf("[CACHE] All requested %s data already in cache\n", data_type))
    return(list())
  }
  
  cat(sprintf("[CACHE] Loading %d missing %s combinations\n", nrow(missing_combinations), data_type))
  
  # Load missing data
  data_loader <- data_loaders[[data_type]]
  if (is.null(data_loader)) {
    stop(sprintf("No data loader defined for type: %s", data_type))
  }
  
  loaded_data_list <- list()
  
  for (i in seq_len(nrow(missing_combinations))) {
    participant <- missing_combinations$participant[i]
    trial <- missing_combinations$trial[i]
    
    cat(sprintf("[CACHE] Loading %s data: P%s-T%s\n", data_type, participant, trial))
    
    tryCatch({
      new_data <- data_loader(participant, trial)
      if (!is.null(new_data) && nrow(new_data) > 0) {
        loaded_data_list[[i]] <- new_data
        cat(sprintf("[CACHE] Completed %s data: P%s-T%s (%d rows)\n", data_type, participant, trial, nrow(new_data)))
      }
    }, error = function(e) {
      warning(sprintf("Error loading %s data for participant %s, trial %s: %s", 
                     data_type, participant, trial, e$message))
    })
  }
  
  # Remove NULL entries
  loaded_data_list <- loaded_data_list[!sapply(loaded_data_list, is.null)]
  
  return(loaded_data_list)
}

# =============================================================================
# MAIN CACHING FUNCTION
# =============================================================================

#' Get cached data for a specific type, participants, and trials
#' @param data_type The type of data (e.g., "simulation")
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param condition_filter Optional condition filter to apply
#' @return The requested data
get_cached_data <- function(data_type, participants, trials, condition_filter = NULL) {
  cat(sprintf("[CACHE] Requesting %s data for %d participants, %d trials\n", 
              data_type, length(participants), length(trials)))
  
  # Step 1: Check current cache
  current_cache <- get_current_cache(data_type)
  
  # Step 2: Load from RDS file if cache is empty
  if (nrow(current_cache) == 0) {
    cat(sprintf("[CACHE] Loading %s data from RDS file\n", data_type))
    current_cache <- load_cache_from_file(data_type)
    
    # Update in-memory cache
    if (nrow(current_cache) > 0) {
      set_cache(data_type, current_cache)
    }
  }
  
  # Step 3: Check what data we need vs what we have
  requested_combinations <- expand.grid(
    participant = participants,
    trial = trials,
    stringsAsFactors = FALSE
  )
  
  # Find missing combinations
  if (nrow(current_cache) > 0) {
    existing_combinations <- unique(paste(current_cache$participant, current_cache$trialNum, sep = "_"))
    requested_combinations$combo_id <- paste(requested_combinations$participant, requested_combinations$trial, sep = "_")
    missing_combinations <- requested_combinations[!requested_combinations$combo_id %in% existing_combinations, ]
    missing_combinations$combo_id <- NULL
  } else {
    missing_combinations <- requested_combinations
  }
  
  # Step 4: Load missing data if needed
  if (nrow(missing_combinations) > 0) {
    cat(sprintf("[CACHE] Loading %d missing %s combinations\n", nrow(missing_combinations), data_type))
    
    # Load missing data
    new_data_list <- load_missing_data(data_type, 
                                      missing_combinations$participant, 
                                      missing_combinations$trial, 
                                      current_cache)
    
    # Combine with existing data
    if (length(new_data_list) > 0) {
      new_data <- do.call(rbind, new_data_list)
      
      if (nrow(current_cache) == 0) {
        current_cache <- new_data
      } else {
        current_cache <- rbind(current_cache, new_data)
      }
      
      # Update cache
      set_cache(data_type, current_cache)
      
      # Save to RDS file
      save_cache_to_file(data_type, current_cache)
      
      cat(sprintf("[CACHE] Updated %s cache: %d total rows\n", data_type, nrow(current_cache)))
    }
  }
  
  # Step 5: Filter data for requested participants/trials
  if (nrow(current_cache) > 0) {
    filtered_data <- current_cache[
      current_cache$participant %in% participants & 
      current_cache$trialNum %in% trials, 
    ]
    
    # Apply condition filter if specified
    if (!is.null(condition_filter) && length(condition_filter) > 0) {
      filtered_data <- filtered_data[filtered_data$condition %in% condition_filter, ]
    }
    
    cat(sprintf("[CACHE] Returning %s data: %d rows for %d participants, %d trials\n", 
                data_type, nrow(filtered_data), 
                length(unique(filtered_data$participant)), 
                length(unique(filtered_data$trialNum))))
    
    return(filtered_data)
  } else {
    cat(sprintf("[CACHE] No %s data available\n", data_type))
    return(data.frame())
  }
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Clear all caches (both in-memory and RDS files)
#' @param data_types Vector of data types to clear (NULL for all)
clear_all_caches <- function(data_types = NULL) {
  if (is.null(data_types)) {
    data_types <- names(global_data_cache)
  }
  
  for (data_type in data_types) {
    # Clear in-memory cache
    clear_cache(data_type)
    
    # Remove RDS file
    cache_path <- get_cache_file_path(data_type)
    if (file.exists(cache_path)) {
      file.remove(cache_path)
      cat(sprintf("[CACHE] Removed cache file: %s\n", cache_path))
    }
  }
}

#' Get cache statistics
#' @return List with cache information
get_cache_stats <- function() {
  stats <- list()
  
  for (data_type in names(global_data_cache)) {
    cache_data <- global_data_cache[[data_type]]$data
    cache_path <- get_cache_file_path(data_type)
    
    stats[[data_type]] <- list(
      in_memory_rows = nrow(cache_data),
      in_memory_participants = length(unique(cache_data$participant)),
      in_memory_trials = length(unique(cache_data$trialNum)),
      rds_file_exists = file.exists(cache_path),
      rds_file_size = if (file.exists(cache_path)) file.size(cache_path) else 0
    )
  }
  
  return(stats)
}
