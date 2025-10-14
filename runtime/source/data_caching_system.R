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
#' @param parallel Whether to enable parallel processing (default: FALSE)
#' @return List of functions to manage the cache with Shiny integration
create_shiny_cache_manager <- function(cache_name, parallel = FALSE) {
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
  
  # Create cache-specific logger
  cache_logger <- create_module_logger(paste0("CACHE-", toupper(cache_name)))
  
  # Store parallel setting
  parallel_enabled <- parallel
  cache_logger("DEBUG", "Cache manager created with parallel =", parallel_enabled)
  
  # Control functions
  request_data <- function() {
    cache_logger("DEBUG", "request_data() called")
    trigger(trigger() + 1)
    cancel(FALSE)
    loading(TRUE)
    cache_logger("DEBUG", "trigger incremented to", trigger(), "loading set to TRUE")
  }
  
  cancel_data <- function() {
    cache_logger("DEBUG", "cancel_data() called")
    cancel(TRUE)
    loading(FALSE)
    show_notification(
      paste(cache_name, "data loading cancelled."),
      type = "warning",
      duration = 3
    )
  }
  
  reset_data <- function() {
    cache_logger("DEBUG", "reset_data() called")
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
    reset_data = reset_data,
    parallel_enabled = function() parallel_enabled
  )
}

#' Create a Shiny reactive for cached data with progress and cancellation
#' @param cache_manager Cache manager created with create_shiny_cache_manager
#' @param data_type The type of data (e.g., "simulation", "power_spectrum")
#' @return Reactive that returns cached data
create_shiny_cached_data_reactive <- function(cache_manager, data_type) {
  # Create Shiny cache logger
  shiny_logger <- create_module_logger(paste0("SHINY-CACHE-", toupper(data_type)))
  
  reactive({
    shiny_logger("DEBUG", "reactive triggered")
    
    # Only depend on the trigger (button clicks), not on filter changes
    trigger_value <- cache_manager$trigger()
    shiny_logger("DEBUG", "trigger value:", trigger_value)
    
    # If trigger is 0, no update has been requested yet - return empty data frame
    if (trigger_value == 0) {
      shiny_logger("DEBUG", "no update requested yet (trigger=0), returning empty data")
      return(data.frame())
    }
    
    # Use isolate() to prevent reactive dependencies on filter inputs
    # Use the universal filter function from sidebar
    shiny_logger("DEBUG", "getting universal filters...")
    current_filters <- isolate(get_universal_filters())
    
    # If filter validation failed, return empty data frame
    if (is.null(current_filters)) {
      shiny_logger("WARN", "filter validation failed, showing warning")
      cache_manager$show_notification(
        paste("Please select participants and trials before loading", data_type, "data."),
        type = "warning",
        duration = 5
      )
      return(data.frame())
    }
    
    shiny_logger("DEBUG", "filters:", length(current_filters$participants), "participants,", 
                 length(current_filters$trials), "trials, conditions:", paste(current_filters$condition, collapse = ", "))
    
    # Use the main caching system to get data
    tryCatch({
      shiny_logger("DEBUG", "calling get_cached_data...")
      
      # Get cached data using the main system
      cached_data <- get_cached_data(
        data_type = data_type,
        participants = current_filters$participants,
        trials = current_filters$trials,
        condition_filter = current_filters$condition,
        parallel = cache_manager$parallel_enabled()
      )
      
      shiny_logger("DEBUG", "got", nrow(cached_data), "rows from get_cached_data")
      
      # Update Shiny cache manager state
      cache_manager$cache(cached_data)
      cache_manager$filters(current_filters)
      cache_manager$loading(FALSE)
      
      # Show success notification
      if (nrow(cached_data) > 0) {
        shiny_logger("INFO", "showing success notification")
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
        shiny_logger("WARN", "showing no data warning")
        cache_manager$show_notification(
          paste("No", data_type, "data found for the selected filters."),
          type = "warning",
          duration = 5
        )
      }
      
      shiny_logger("DEBUG", "returning", nrow(cached_data), "rows")
      return(cached_data)
      
    }, error = function(e) {
      shiny_logger("ERROR", "ERROR:", e$message)
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
  cache_logger <- create_module_logger("CACHE")
  cache_path <- get_cache_file_path(data_type)
  
  if (file.exists(cache_path)) {
    tryCatch({
      cached_data <- readRDS(cache_path)
      cache_logger("DEBUG", "Loaded", data_type, "cache from file:", nrow(cached_data), "rows")
      return(cached_data)
    }, error = function(e) {
      cache_logger("ERROR", "Error loading cache file", cache_path, ":", e$message)
      warning(sprintf("Error loading cache file %s: %s", cache_path, e$message))
      return(data.frame())
    })
  } else {
    cache_logger("DEBUG", "No cache file found for", data_type, "at", cache_path)
    return(data.frame())
  }
}

#' Save data to RDS cache file
#' @param data_type The type of data
#' @param data The data to save
save_cache_to_file <- function(data_type, data) {
  cache_logger <- create_module_logger("CACHE")
  cache_path <- get_cache_file_path(data_type)
  
  tryCatch({
    saveRDS(data, cache_path)
    cache_logger("DEBUG", "Saved", data_type, "cache to file:", nrow(data), "rows")
  }, error = function(e) {
    cache_logger("ERROR", "Error saving cache file", cache_path, ":", e$message)
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


#' Load missing data for a data type
#' @param data_type The type of data
#' @param combinations Data frame with participant and trial columns, or separate vectors
#' @param existing_data Existing data to avoid reloading
#' @param parallel Whether to use parallel processing (default: FALSE)
#' @return List of newly loaded data frames
load_missing_data <- function(data_type, combinations, existing_data = data.frame(), parallel = FALSE) {
  load_logger <- create_module_logger(paste0("LOAD-", toupper(data_type)))
  
  log_operation_start(load_logger, paste("load_missing_data for", data_type))
  
  # Handle both data frame and separate vector inputs for backward compatibility
  if (is.data.frame(combinations)) {
    # New format: combinations data frame
    load_logger("DEBUG", "Using combinations data frame with", nrow(combinations), "combinations")
    load_logger("DEBUG", "Participants:", paste(unique(combinations$participant), collapse = ", "))
    load_logger("DEBUG", "Trials:", paste(unique(combinations$trial), collapse = ", "))
    missing_combinations <- combinations
  } else {
    # This should not happen with the new interface, but handle gracefully
    load_logger("ERROR", "Invalid combinations parameter - expected data frame")
    return(list())
  }
  
  load_logger("DEBUG", "Existing data:", nrow(existing_data), "rows")
  
  if (nrow(missing_combinations) == 0) {
    load_logger("WARN", "No data available for", data_type, "with given participants/trials")
    return(list())
  }
  
  # Check which combinations we already have
  if (nrow(existing_data) > 0) {
    existing_combinations <- unique(paste(existing_data$participant, existing_data$trialNum, sep = "_"))
    load_logger("DEBUG", "Found", length(existing_combinations), "existing combinations in cache")
    missing_combinations$combo_id <- paste(missing_combinations$participant, missing_combinations$trial, sep = "_")
    missing_combinations <- missing_combinations[!missing_combinations$combo_id %in% existing_combinations, ]
    missing_combinations$combo_id <- NULL
    load_logger("DEBUG", "Need to load", nrow(missing_combinations), "missing combinations")
  } else {
    load_logger("DEBUG", "No existing data, need to load all", nrow(missing_combinations), "combinations")
  }
  
  if (nrow(missing_combinations) == 0) {
    load_logger("INFO", "All requested", data_type, "data already in cache")
    return(list())
  }
  
  load_logger("INFO", "Loading", nrow(missing_combinations), "missing", data_type, "combinations")
  
  # Always use the loop system for efficient batch loading
  load_logger("DEBUG", "Using loop system for batch loading (parallel =", parallel, ", combinations =", nrow(missing_combinations), ")")
  
  # Get the individual data loader
  data_loader <- data_loaders[[data_type]]
  if (is.null(data_loader)) {
    load_logger("ERROR", "No data loader defined for type:", data_type)
    stop(sprintf("No data loader defined for type: %s", data_type))
  }
  
  # Create cache file for this specific batch
  batch_id <- digest::digest(paste(missing_combinations$participant, missing_combinations$trial, collapse = "_"))
  cache_file <- file.path(dataExtraFolder, paste0("batch_", data_type, "_", batch_id, ".rds"))
  
  # Get the datasets to verify for this data type
  datasets_to_verify <- get_datasets_to_verify(data_type)
  
  # Use load_or_calculate with specific combinations
  batch_data <- load_or_calculate(
    filePath = cache_file,
    calculate_function = function(loop_function) {
      # The loop_function will handle the specific combinations automatically
      return(loop_function(data_loader, datasets_to_verify = datasets_to_verify))
    },
    parallel = parallel,
    force_recalc = FALSE,
    combinations_df = missing_combinations
  )
  
  if (nrow(batch_data) > 0) {
    load_logger("INFO", "Successfully loaded", nrow(batch_data), "rows using loop system")
    log_operation_end(load_logger, paste("load_missing_data for", data_type), success = TRUE)
    return(list(batch_data))  # Return as list for consistency
  } else {
    load_logger("WARN", "Loop system returned empty data")
    log_operation_end(load_logger, paste("load_missing_data for", data_type), success = FALSE)
    return(list())
  }
}

# =============================================================================
# MAIN CACHING FUNCTION
# =============================================================================

#' Get cached data for a specific type, participants, and trials
#' @param data_type The type of data (e.g., "simulation")
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param condition_filter Optional condition filter to apply
#' @param parallel Whether to use parallel processing (default: FALSE)
#' @return The requested data
get_cached_data <- function(data_type, participants, trials, condition_filter = NULL, parallel = FALSE) {
  cache_logger <- create_module_logger("CACHE")
  
  log_operation_start(cache_logger, paste("get_cached_data for", data_type))
  cache_logger("DEBUG", "Requesting", data_type, "data for", length(participants), "participants,", length(trials), "trials")
  cache_logger("DEBUG", "Participants:", paste(participants, collapse = ", "))
  cache_logger("DEBUG", "Trials:", paste(trials, collapse = ", "))
  cache_logger("DEBUG", "Condition filter:", if(is.null(condition_filter)) "NULL" else paste(condition_filter, collapse = ", "))
  
  # Step 1: Check current cache
  cache_logger("DEBUG", "Step 1: Checking current in-memory cache for", data_type)
  current_cache <- get_current_cache(data_type)
  cache_logger("DEBUG", "Current cache has", nrow(current_cache), "rows")
  
  # Step 2: Load from RDS file if cache is empty
  if (nrow(current_cache) == 0) {
    cache_logger("DEBUG", "Step 2: Loading", data_type, "data from RDS file")
    current_cache <- load_cache_from_file(data_type)
    cache_logger("DEBUG", "Loaded", nrow(current_cache), "rows from RDS file")
    
    # Update in-memory cache
    if (nrow(current_cache) > 0) {
      cache_logger("DEBUG", "Updating in-memory cache with RDS data")
      set_cache(data_type, current_cache)
    }
  } else {
    cache_logger("DEBUG", "Step 2: Using existing in-memory cache (", nrow(current_cache), "rows)")
  }
  
  # Step 3: Check what data we need vs what we have
  cache_logger("DEBUG", "Step 3: Checking what data we need vs what we have")
  requested_combinations <- expand.grid(
    participant = participants,
    trial = trials,
    stringsAsFactors = FALSE
  )
  cache_logger("DEBUG", "Requested", nrow(requested_combinations), "participant-trial combinations")
  
  # Find missing combinations
  if (nrow(current_cache) > 0) {
    existing_combinations <- unique(paste(current_cache$participant, current_cache$trialNum, sep = "_"))
    cache_logger("DEBUG", "Found", length(existing_combinations), "existing combinations in cache")
    requested_combinations$combo_id <- paste(requested_combinations$participant, requested_combinations$trial, sep = "_")
    missing_combinations <- requested_combinations[!requested_combinations$combo_id %in% existing_combinations, ]
    missing_combinations$combo_id <- NULL
    cache_logger("DEBUG", "Need to load", nrow(missing_combinations), "missing combinations")
  } else {
    missing_combinations <- requested_combinations
    cache_logger("DEBUG", "No existing cache, need to load all", nrow(missing_combinations), "combinations")
  }
  
  # Step 4: Load missing data if needed
  if (nrow(missing_combinations) > 0) {
    cache_logger("DEBUG", "Step 4: Loading", nrow(missing_combinations), "missing", data_type, "combinations")
    
    # Load missing data
    new_data_list <- load_missing_data(data_type, 
                                      missing_combinations, 
                                      current_cache,
                                      parallel)
    
    cache_logger("DEBUG", "Loaded", length(new_data_list), "new data objects")
    
    # Combine with existing data
    if (length(new_data_list) > 0) {
      new_data <- do.call(rbind, new_data_list)
      cache_logger("DEBUG", "Combined new data:", nrow(new_data), "rows")
      
      if (nrow(current_cache) == 0) {
        current_cache <- new_data
        cache_logger("DEBUG", "Using new data as cache (was empty)")
      } else {
        current_cache <- rbind(current_cache, new_data)
        cache_logger("DEBUG", "Combined with existing cache")
      }
      
      # Update cache
      set_cache(data_type, current_cache)
      cache_logger("DEBUG", "Updated in-memory cache")
      
      # Save to RDS file
      save_cache_to_file(data_type, current_cache)
      cache_logger("DEBUG", "Saved to RDS file")
      
      cache_logger("DEBUG", "Updated", data_type, "cache:", nrow(current_cache), "total rows")
    } else {
      cache_logger("DEBUG", "No new data loaded")
    }
  } else {
    cache_logger("DEBUG", "Step 4: All requested data already in cache")
  }
  
  # Step 5: Filter data for requested participants/trials
  cache_logger("DEBUG", "Step 5: Filtering data for requested participants/trials")
  if (nrow(current_cache) > 0) {
    cache_logger("DEBUG", "Filtering", nrow(current_cache), "rows from cache")
    filtered_data <- current_cache[
      current_cache$participant %in% participants & 
      current_cache$trialNum %in% trials, 
    ]
    cache_logger("DEBUG", "After participant/trial filter:", nrow(filtered_data), "rows")
    
    # Apply condition filter if specified
    if (!is.null(condition_filter) && length(condition_filter) > 0) {
      cache_logger("DEBUG", "Applying condition filter:", paste(condition_filter, collapse = ", "))
      filtered_data <- filtered_data[filtered_data$condition %in% condition_filter, ]
      cache_logger("DEBUG", "After condition filter:", nrow(filtered_data), "rows")
    } else {
      cache_logger("DEBUG", "No condition filter applied")
    }
    
    log_operation_end(cache_logger, paste("get_cached_data for", data_type), success = TRUE)
    cache_logger("INFO", "Returning", data_type, "data:", nrow(filtered_data), "rows for", 
                 length(unique(filtered_data$participant)), "participants,", 
                 length(unique(filtered_data$trialNum)), "trials")
    
    return(filtered_data)
  } else {
    cache_logger("WARN", "No", data_type, "data available in cache")
    log_operation_end(cache_logger, paste("get_cached_data for", data_type), success = FALSE)
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
