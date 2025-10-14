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
    
    # Check for changes in Shiny inputs that affect this data type
    input_changed <- FALSE
    if (exists("input")) {
      # Get relevant inputs for this data type from centralized definition
      relevant_inputs <- get_relevant_shiny_inputs(data_type)
      
      # Check if any relevant inputs have changed
      if (length(relevant_inputs) > 0) {
        # Get current input values using centralized function
        current_inputs <- capture_shiny_inputs(data_type)
        
        # Get stored input values from cache manager
        stored_filters <- cache_manager$filters()
        stored_inputs <- list()
        if (!is.null(stored_filters) && !is.null(stored_filters$shiny_inputs)) {
          stored_inputs <- stored_filters$shiny_inputs
        }
        
        # Compare current vs stored inputs
        for (input_name in relevant_inputs) {
          current_val <- current_inputs[[input_name]]
          stored_val <- stored_inputs[[input_name]]
          
          if (!identical(current_val, stored_val)) {
            shiny_logger("DEBUG", "Input changed:", input_name, "from", stored_val, "to", current_val)
            input_changed <- TRUE
            break
          }
        }
        
        # If inputs changed, reset the cache
        if (input_changed) {
          shiny_logger("INFO", "Shiny inputs changed, resetting cache before loading new data")
          cache_manager$reset_data()
        }
      }
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
      shiny_logger("DEBUG", "input exists in reactive context:", exists("input"))
      
      # Debug log relevant inputs for this data type
      if (exists("input")) {
        relevant_inputs <- get_relevant_shiny_inputs(data_type)
        if (length(relevant_inputs) > 0) {
          for (input_name in relevant_inputs) {
            shiny_logger("DEBUG", paste0("input$", input_name, " exists in reactive context:"), !is.null(input[[input_name]]))
            if (!is.null(input[[input_name]])) {
              shiny_logger("DEBUG", paste0("input$", input_name, " value in reactive context:"), input[[input_name]])
            }
          }
        }
      }
      
      # Capture Shiny input values for data loaders that need them using centralized function
      shiny_inputs <- capture_shiny_inputs(data_type)
      
      # Get cached data using the main system
      cached_data <- get_cached_data(
        data_type = data_type,
        participants = current_filters$participants,
        trials = current_filters$trials,
        condition_filter = current_filters$condition,
        parallel = cache_manager$parallel_enabled(),
        shiny_inputs = shiny_inputs
      )
      
      shiny_logger("DEBUG", "got", nrow(cached_data), "rows from get_cached_data")
      
      # Update Shiny cache manager state
      cache_manager$cache(cached_data)
      
      # Store both filters and shiny inputs for change detection
      stored_state <- current_filters
      stored_state$shiny_inputs <- shiny_inputs
      cache_manager$filters(stored_state)
      
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
  
  # Ensure cache directory exists
  if (!dir.exists(cacheFolder)) {
    dir.create(cacheFolder, recursive = TRUE)
  }
  
  cache_filename <- paste0(data_type, "_cache.rds")
  return(file.path(cacheFolder, cache_filename))
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



# =============================================================================
# MAIN CACHING FUNCTION
# =============================================================================

#' Get cached data for a specific type, participants, and trials
#' @param data_type The type of data (e.g., "simulation")
#' @param participants Vector of participant IDs
#' @param trials Vector of trial numbers
#' @param condition_filter Optional condition filter to apply
#' @param parallel Whether to use parallel processing (default: FALSE)
#' @param shiny_inputs Optional list of Shiny input values for data loaders
#' @return The requested data
get_cached_data <- function(data_type, participants, trials, condition_filter = NULL, parallel = FALSE, shiny_inputs = NULL) {
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
  
  # Step 3: Create requested combinations and load data using the loop system
  cache_logger("DEBUG", "Step 3: Creating requested combinations and loading data")
  requested_combinations <- expand.grid(
    participant = participants,
    trial = trials,
    stringsAsFactors = FALSE
  )
  cache_logger("DEBUG", "Requested", nrow(requested_combinations), "participant-trial combinations")
  
  # Step 4: Load data using the loop system - missing combinations logic is handled automatically
  cache_logger("DEBUG", "Step 4: Loading data using loop system with automatic missing combinations handling")
  
  # Get the individual data loader using the helper function
  data_loader <- get_data_loader(data_type)
  if (is.null(data_loader)) {
    cache_logger("ERROR", "No data loader defined for type:", data_type)
    stop(sprintf("No data loader defined for type: %s", data_type))
  }
  
  cache_logger("DEBUG", "data_loader type:", typeof(data_loader))
  cache_logger("DEBUG", "data_loader is function:", is.function(data_loader))
  
  # Create a wrapper function that provides shiny_inputs to the data loader
  if (!is.null(shiny_inputs) && length(shiny_inputs) > 0) {
    cache_logger("DEBUG", "Creating data loader wrapper with shiny_inputs:", paste(names(shiny_inputs), collapse = ", "))
    original_loader <- data_loader
    data_loader <- function(participant, trial) {
      # Temporarily set up input environment for the data loader
      if (!exists("input", envir = .GlobalEnv)) {
        assign("input", list(), envir = .GlobalEnv)
      }
      # Set the input values
      for (name in names(shiny_inputs)) {
        .GlobalEnv$input[[name]] <- shiny_inputs[[name]]
      }
      # Call the original loader
      result <- original_loader(participant, trial)
      return(result)
    }
  }
  
  # Get the datasets to verify for this data type
  datasets_to_verify <- get_datasets_to_verify(data_type)
  
  # If we have shiny_inputs, we need to handle dynamic datasets_to_verify
  if (!is.null(shiny_inputs) && length(shiny_inputs) > 0) {
    # Temporarily set up input environment for datasets_to_verify function
    if (!exists("input", envir = .GlobalEnv)) {
      assign("input", list(), envir = .GlobalEnv)
    }
    # Set the input values
    for (name in names(shiny_inputs)) {
      .GlobalEnv$input[[name]] <- shiny_inputs[[name]]
    }
    # Re-get datasets_to_verify with input context
    datasets_to_verify <- get_datasets_to_verify(data_type)
  }
  
  cache_logger("DEBUG", "datasets_to_verify:", paste(datasets_to_verify, collapse = ", "))
  
  cache_logger("INFO", "Loading", nrow(requested_combinations), data_type, "combinations using loop system")
  
  # Load data using the simplified load_or_calc_from_loop function
  # Let it handle caching internally - no need for separate batch files
  new_data <- load_or_calc_from_loop(
    filePath = get_cache_file_path(data_type),
    data_loader = data_loader,
    datasets_to_verify = datasets_to_verify,
    combinations_df = requested_combinations,
    parallel = parallel,
    force_recalc = FALSE,
    threshold_parallel = NULL
  )
  
  cache_logger("DEBUG", "Loaded", nrow(new_data), "rows from loop system")
  
  # Combine with existing data
  if (nrow(new_data) > 0) {
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
