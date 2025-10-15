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
#' @param use_parallel Whether to enable parallel processing (default: FALSE)
#' @return List of functions to manage the cache with Shiny integration
create_shiny_cache_manager <- function(cache_name, use_parallel = FALSE) {
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

  # Fast cache lookup function
  get_fast_cache <- function(participants = NULL, trials = NULL, condition_filter = NULL, downsample_factor = 1) {
    cache_logger("DEBUG", "get_fast_cache called")
    cache_logger("DEBUG", "participants:", if (is.null(participants)) "NULL" else paste(participants, collapse = ", "))
    cache_logger("DEBUG", "trials:", if (is.null(trials)) "NULL" else paste(trials, collapse = ", "))
    cache_logger("DEBUG", "condition_filter:", if (is.null(condition_filter)) "NULL" else paste(condition_filter, collapse = ", "))
    cache_logger("DEBUG", "downsample_factor:", downsample_factor)

    # Get cached data from global cache (no duplication)
    cached_data <- cache()
    cache_logger("DEBUG", "global cache has", nrow(cached_data), "rows")

    if (nrow(cached_data) == 0) {
      cache_logger("WARN", "No data in global cache - returning NULL")
      return(NULL)
    }

    # Apply filters if specified
    filtered_data <- cached_data

    if (!is.null(participants) && length(participants) > 0) {
      cache_logger("DEBUG", "Filtering by participants")
      filtered_data <- filtered_data[filtered_data$participant %in% participants, ]
      cache_logger("DEBUG", "After participant filter:", nrow(filtered_data), "rows")
    }

    if (!is.null(trials) && length(trials) > 0) {
      cache_logger("DEBUG", "Filtering by trials")
      filtered_data <- filtered_data[filtered_data$trialNum %in% trials, ]
      cache_logger("DEBUG", "After trial filter:", nrow(filtered_data), "rows")
    }

    if (!is.null(condition_filter) && length(condition_filter) > 0) {
      cache_logger("DEBUG", "Filtering by conditions")
      filtered_data <- filtered_data[filtered_data$condition %in% condition_filter, ]
      cache_logger("DEBUG", "After condition filter:", nrow(filtered_data), "rows")
    }

    # Apply downsampling if requested
    if (downsample_factor > 1 && nrow(filtered_data) > 0) {
      cache_logger("DEBUG", "Applying downsampling with factor:", downsample_factor)
      cache_logger("DEBUG", "Before downsampling:", nrow(filtered_data), "rows")

      # Use fast row indexing for downsampling
      indices <- seq(1, nrow(filtered_data), by = downsample_factor)
      filtered_data <- filtered_data[indices, , drop = FALSE]

      cache_logger("DEBUG", "After downsampling:", nrow(filtered_data), "rows")
    }

    cache_logger("DEBUG", "get_fast_cache returning", nrow(filtered_data), "rows")
    return(filtered_data)
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
  parallel_enabled <- use_parallel
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

    # Also clear the global cache and RDS file for this data type
    clear_cache(cache_name)
    cache_path <- get_cache_file_path(cache_name)
    if (file.exists(cache_path)) {
      tryCatch(
        {
          file.remove(cache_path)
          cache_logger("DEBUG", "Removed RDS cache file:", cache_path)
        },
        error = function(e) {
          cache_logger("WARN", "Could not remove RDS cache file:", e$message)
        }
      )
    }
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
    parallel_enabled = parallel_enabled,
    get_fast_cache = get_fast_cache
  )
}

#' Create a Shiny reactive for cached data with progress and cancellation
#' @param cache_manager Cache manager created with create_shiny_cache_manager
#' @param data_type The type of data (e.g., "simulation", "power_spectrum")
#' @param downsample_factor Downsampling factor (1 = no downsampling, 2 = every 2nd row, etc.)
#' @return Reactive that returns cached data (optionally downsampled)
create_shiny_cached_data_reactive <- function(cache_manager, data_type, downsample_factor = 1) {
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

    # Check for changes in context variables that affect this data type
    context_changed <- FALSE
    if (exists("input")) {
      # Get relevant context variables for this data type from centralized definition
      relevant_vars <- get_relevant_context_vars(data_type)
      input_vars <- relevant_vars[startsWith(relevant_vars, "input.")]

      # Check if any relevant context variables have changed
      if (length(relevant_vars) > 0) {
        # Get current context variables using centralized function
        current_context_vars <- capture_context_vars(data_type)

        # Get stored context variables from cache manager
        stored_filters <- cache_manager$filters()
        stored_context_vars <- list()
        if (!is.null(stored_filters) && !is.null(stored_filters$context_vars)) {
          stored_context_vars <- stored_filters$context_vars
        }

        # Compare current vs stored context variables
        for (var_name in names(current_context_vars)) {
          current_val <- current_context_vars[[var_name]]
          stored_val <- stored_context_vars[[var_name]]

          if (!identical(current_val, stored_val)) {
            shiny_logger("DEBUG", "Context variable changed:", var_name, "from", stored_val, "to", current_val)
            context_changed <- TRUE
            break
          }
        }

        # If context variables changed, reset the cache and force recalculation
        if (context_changed) {
          shiny_logger("INFO", "Context variables changed, resetting cache and forcing recalculation")
          cache_manager$reset_data()
          # Clear the global cache as well to ensure fresh data
          clear_cache(data_type)
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

    shiny_logger(
      "DEBUG", "filters:", length(current_filters$participants), "participants,",
      length(current_filters$trials), "trials, conditions:", paste(current_filters$condition, collapse = ", ")
    )

    # Use the main caching system to get data
    tryCatch(
      {
        shiny_logger("DEBUG", "calling get_cached_data...")
        shiny_logger("DEBUG", "input exists in reactive context:", exists("input"))

        # Debug log relevant context variables for this data type
        if (exists("input")) {
          relevant_vars <- get_relevant_context_vars(data_type)
          input_vars <- relevant_vars[startsWith(relevant_vars, "input.")]
          if (length(input_vars) > 0) {
            for (var_name in input_vars) {
              input_name <- substring(var_name, 7) # Remove "input." prefix
              shiny_logger("DEBUG", paste0("input$", input_name, " exists in reactive context:"), !is.null(input[[input_name]]))
              if (!is.null(input[[input_name]])) {
                shiny_logger("DEBUG", paste0("input$", input_name, " value in reactive context:"), input[[input_name]])
              }
            }
          }
        }

        # Capture context variables (including shiny inputs) for data loaders that need them
        context_vars <- capture_context_vars(data_type)

        # Get cached data using the main system
        cached_data <- get_cached_data(
          data_type = data_type,
          participants = current_filters$participants,
          trials = current_filters$trials,
          condition_filter = current_filters$condition,
          use_parallel = cache_manager$parallel_enabled,
          context_vars = context_vars
        )

        shiny_logger("DEBUG", "got", nrow(cached_data), "rows from get_cached_data")

        # Update Shiny cache manager state
        cache_manager$cache(cached_data)

        # Store both filters and context variables for change detection
        stored_state <- current_filters
        stored_state$context_vars <- context_vars
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
      },
      error = function(e) {
        shiny_logger("ERROR", "ERROR:", e$message)
        cache_manager$loading(FALSE)
        cache_manager$show_notification(
          paste("Error loading", data_type, "data:", e$message),
          type = "error",
          duration = 5
        )
        return(data.frame())
      }
    )
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

#' Validate cache metadata against current context
#' @param cached_metadata The metadata stored in the cache
#' @param current_metadata The current context metadata
#' @param data_type The type of data being validated
#' @return TRUE if metadata is valid, FALSE otherwise
validate_cache_metadata <- function(cached_metadata, current_metadata, data_type) {
  cache_logger <- create_module_logger("CACHE")

  # If either metadata is NULL, consider it valid (backward compatibility)
  if (is.null(cached_metadata) || is.null(current_metadata)) {
    cache_logger("DEBUG", "Metadata validation skipped (NULL metadata)")
    return(TRUE)
  }

  # Get relevant context variables for this data type
  relevant_vars <- get_relevant_context_vars(data_type)
  input_vars <- relevant_vars[startsWith(relevant_vars, "input.")]

  if (length(input_vars) == 0) {
    cache_logger("DEBUG", "No input variables to validate for", data_type)
    return(TRUE)
  }

  # Check each relevant input variable
  for (var_name in input_vars) {
    input_name <- substring(var_name, 7) # Remove "input." prefix

    cached_val <- cached_metadata[[input_name]]
    current_val <- current_metadata[[input_name]]

    if (!identical(cached_val, current_val)) {
      cache_logger(
        "DEBUG", "Metadata mismatch for", input_name, ":",
        "cached =", cached_val, "current =", current_val
      )
      return(FALSE)
    }
  }

  cache_logger("DEBUG", "All metadata validation checks passed for", data_type)
  return(TRUE)
}

#' Load cached data from RDS file with metadata validation
#' @param data_type The type of data to load
#' @param current_metadata Optional current metadata to validate against cached metadata
#' @return List with 'data' and 'metadata' fields, or empty data.frame if not found/invalid
load_cache_from_file <- function(data_type, current_metadata = NULL) {
  cache_logger <- create_module_logger("CACHE")
  cache_path <- get_cache_file_path(data_type)

  if (file.exists(cache_path)) {
    tryCatch(
      {
        # Get file info for diagnostics
        file_info <- file.info(cache_path)
        file_size_mb <- round(file_info$size / (1024 * 1024), 2)
        cache_logger("DEBUG", "RDS file size:", file_size_mb, "MB")

        # Time the readRDS operation
        start_time <- Sys.time()
        cache_object <- readRDS(cache_path)
        end_time <- Sys.time()
        read_duration <- round(as.numeric(end_time - start_time, units = "secs"), 2)

        cache_logger("DEBUG", "readRDS completed in", read_duration, "seconds")

        # Handle both old format (direct data.frame) and new format (list with data and metadata)
        if (is.data.frame(cache_object)) {
          # Old format - no metadata
          cache_logger("DEBUG", "Loaded legacy cache format for", data_type, ":", nrow(cache_object), "rows")
          return(list(data = cache_object, metadata = NULL))
        } else if (is.list(cache_object) && "data" %in% names(cache_object)) {
          # New format with metadata
          cached_data <- cache_object$data
          cached_metadata <- cache_object$metadata
          cache_timestamp <- cache_object$timestamp

          cache_logger("DEBUG", "Loaded", data_type, "cache from file:", nrow(cached_data), "rows")
          cache_logger("DEBUG", "Cache timestamp:", as.character(cache_timestamp))

          if (!is.null(cached_metadata)) {
            cache_logger("DEBUG", "Cached metadata:", paste(names(cached_metadata), collapse = ", "))
          }

          # Validate metadata if current metadata is provided
          if (!is.null(current_metadata) && !is.null(cached_metadata)) {
            metadata_valid <- validate_cache_metadata(cached_metadata, current_metadata, data_type)
            if (!metadata_valid) {
              cache_logger("INFO", "Cache metadata validation failed - cache is invalid for current context")
              return(list(data = data.frame(), metadata = NULL))
            }
            cache_logger("DEBUG", "Cache metadata validation passed")
          }

          # Additional diagnostics
          if (nrow(cached_data) > 0) {
            cache_logger("DEBUG", "Data frame memory usage:", format(object.size(cached_data), units = "MB"))
            cache_logger("DEBUG", "Column names:", paste(names(cached_data), collapse = ", "))
            cache_logger("DEBUG", "Data types:", paste(sapply(cached_data, class), collapse = ", "))
          }

          return(list(data = cached_data, metadata = cached_metadata))
        } else {
          cache_logger("ERROR", "Invalid cache object format for", data_type)
          return(list(data = data.frame(), metadata = NULL))
        }
      },
      error = function(e) {
        cache_logger("ERROR", "Error loading cache file", cache_path, ":", e$message)
        # If file is corrupted, try to remove it
        tryCatch(
          {
            file.remove(cache_path)
            cache_logger("INFO", "Removed corrupted cache file:", cache_path)
          },
          error = function(remove_error) {
            cache_logger("WARN", "Could not remove corrupted cache file:", remove_error$message)
          }
        )
        return(list(data = data.frame(), metadata = NULL))
      }
    )
  } else {
    cache_logger("DEBUG", "No cache file found for", data_type, "at", cache_path)
    return(list(data = data.frame(), metadata = NULL))
  }
}

#' Save data to RDS cache file with metadata
#' @param data_type The type of data
#' @param data The data to save
#' @param metadata Optional metadata to save alongside the data
save_cache_to_file <- function(data_type, data, metadata = NULL) {
  cache_logger <- create_module_logger("CACHE")
  cache_path <- get_cache_file_path(data_type)

  tryCatch(
    {
      # Get data info for diagnostics
      data_size_mb <- round(object.size(data) / (1024 * 1024), 2)
      cache_logger("DEBUG", "Data frame size:", data_size_mb, "MB, rows:", nrow(data))

      # Create cache object with data and metadata
      cache_object <- list(
        data = data,
        metadata = metadata,
        timestamp = Sys.time(),
        data_type = data_type
      )

      # Time the saveRDS operation without compression
      start_time <- Sys.time()
      saveRDS(cache_object, cache_path, compress = FALSE) # No compression for maximum reliability
      end_time <- Sys.time()
      save_duration <- round(as.numeric(end_time - start_time, units = "secs"), 2)

      # Get final file size
      file_info <- file.info(cache_path)
      file_size_mb <- round(file_info$size / (1024 * 1024), 2)

      cache_logger("DEBUG", "saveRDS completed in", save_duration, "seconds")
      cache_logger("DEBUG", "Saved", data_type, "cache to file:", nrow(data), "rows, file size:", file_size_mb, "MB")
      if (!is.null(metadata)) {
        cache_logger("DEBUG", "Saved with metadata:", paste(names(metadata), collapse = ", "))
      }
    },
    error = function(e) {
      cache_logger("ERROR", "Error saving cache file", cache_path, ":", e$message)
    }
  )
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

#' Get current cache filters for a data type
#' @param data_type The type of data
#' @return The cached filters or empty list if not found
get_current_cache_filters <- function(data_type) {
  if (data_type %in% names(global_data_cache)) {
    return(global_data_cache[[data_type]]$filters)
  } else {
    return(list())
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
#' @param use_parallel Whether to use parallel processing (default: FALSE)
#' @param context_vars Optional list of context variables for data loaders (including shiny inputs)
#' @return The requested data
get_cached_data <- function(data_type, participants, trials, condition_filter = NULL, use_parallel = FALSE, context_vars = NULL) {
  cache_logger <- create_module_logger("CACHE")

  log_operation_start(cache_logger, paste("get_cached_data for", data_type))
  cache_logger("DEBUG", "Requesting", data_type, "data for", length(participants), "participants,", length(trials), "trials")
  cache_logger("DEBUG", "Participants:", paste(participants, collapse = ", "))
  cache_logger("DEBUG", "Trials:", paste(trials, collapse = ", "))
  cache_logger("DEBUG", "Condition filter:", if (is.null(condition_filter)) "NULL" else paste(condition_filter, collapse = ", "))

  # Step 1: Capture current context metadata for validation
  cache_logger("DEBUG", "Step 1: Capturing current context metadata")
  current_metadata <- capture_context_vars(data_type)
  cache_logger("DEBUG", "Current metadata:", if (is.null(current_metadata)) "NULL" else paste(names(current_metadata), collapse = ", "))

  # Step 2: Check current cache
  cache_logger("DEBUG", "Step 2: Checking current in-memory cache for", data_type)
  current_cache <- get_current_cache(data_type)
  cache_logger("DEBUG", "Current cache has", nrow(current_cache), "rows")

  # Step 3: Load from RDS file if cache is empty, with metadata validation
  if (nrow(current_cache) == 0) {
    cache_logger("DEBUG", "Step 3: Loading", data_type, "data from RDS file with metadata validation")
    cache_result <- load_cache_from_file(data_type, current_metadata)
    current_cache <- cache_result$data
    cached_metadata <- cache_result$metadata
    cache_logger("DEBUG", "Loaded", nrow(current_cache), "rows from RDS file")

    # Update in-memory cache
    if (nrow(current_cache) > 0) {
      cache_logger("DEBUG", "Updating in-memory cache with RDS data")
      set_cache(data_type, current_cache)
    }
  } else {
    cache_logger("DEBUG", "Step 3: Using existing in-memory cache (", nrow(current_cache), "rows)")
    # For in-memory cache, we need to get the cached metadata from the stored filters
    stored_filters <- get_current_cache_filters(data_type)
    if (!is.null(stored_filters) && !is.null(stored_filters$context_vars)) {
      cached_metadata <- stored_filters$context_vars
    } else {
      cached_metadata <- NULL
    }
  }

  # Step 4: Create requested combinations and load data using the loop system
  cache_logger("DEBUG", "Step 4: Creating requested combinations and loading data")
  requested_combinations <- expand.grid(
    participant = participants,
    trial = trials,
    stringsAsFactors = FALSE
  )
  cache_logger("DEBUG", "Requested", nrow(requested_combinations), "participant-trial combinations")

  # Step 5: Load data using the loop system - missing combinations logic is handled automatically
  cache_logger("DEBUG", "Step 5: Loading data using loop system with automatic missing combinations handling")

  # Get the individual data loader using the helper function
  data_loader <- get_data_loader(data_type)
  if (is.null(data_loader)) {
    cache_logger("ERROR", "No data loader defined for type:", data_type)
    stop(sprintf("No data loader defined for type: %s", data_type))
  }

  cache_logger("DEBUG", "data_loader type:", typeof(data_loader))
  cache_logger("DEBUG", "data_loader is function:", is.function(data_loader))

  # Capture and pass context variables (including shiny inputs) to the data loader
  context_vars <- capture_context_vars(data_type)
  if (length(context_vars) > 0) {
    cache_logger("DEBUG", "Creating data loader wrapper with context variables:", paste(names(context_vars), collapse = ", "))

    # Set up global context environment for all context variables
    if (!exists("context", envir = .GlobalEnv)) {
      assign("context", list(), envir = .GlobalEnv)
    }

    # Set up context variables in global environment
    for (name in names(context_vars)) {
      if (startsWith(name, "input.")) {
        input_name <- substring(name, 7) # Remove "input." prefix
        .GlobalEnv$context[[input_name]] <- context_vars[[name]]
      } else if (startsWith(name, "cache.")) {
        cache_name <- substring(name, 7) # Remove "cache." prefix
        .GlobalEnv$context[[cache_name]] <- context_vars[[name]]
      } else if (startsWith(name, "model.")) {
        model_name <- substring(name, 7) # Remove "model." prefix
        .GlobalEnv$context[[model_name]] <- context_vars[[name]]
      }
    }
  }

  # Get the datasets to verify for this data type
  datasets_to_verify <- get_datasets_to_verify(data_type)

  # If we have context variables with input.*, we need to handle dynamic datasets_to_verify
  if (length(context_vars) > 0) {
    input_vars <- context_vars[startsWith(names(context_vars), "input.")]
    if (length(input_vars) > 0) {
      # Re-get datasets_to_verify with input context (input environment already set up above)
      datasets_to_verify <- get_datasets_to_verify(data_type)
    }
  }

  cache_logger("DEBUG", "datasets_to_verify:", paste(datasets_to_verify, collapse = ", "))

  cache_logger("INFO", "Loading", nrow(requested_combinations), data_type, "combinations using loop system")

  # Prepare context variables for parallel workers
  extra_global_vars <- NULL
  if (!is.null(context_vars) && length(context_vars) > 0) {
    # Convert context variables to a format that can be exported to workers
    # We'll create a special environment that workers can access
    extra_global_vars <- c("context_vars", "context")
    # Store context variables in global environment for workers to access
    assign("context_vars", context_vars, envir = .GlobalEnv)

    # Also ensure the context environment is available for workers
    # The context is already set up above, but we need to make sure it's exported
    cache_logger("DEBUG", "Context environment set up with variables:", paste(names(.GlobalEnv$context), collapse = ", "))
  }

  # Determine if we need to force recalculation based on context changes
  force_recalc <- FALSE
  if (nrow(current_cache) > 0 && !is.null(current_metadata) && !is.null(cached_metadata)) {
    force_recalc <- !validate_cache_metadata(cached_metadata, current_metadata, data_type)
    if (force_recalc) {
      cache_logger("INFO", "Context variables changed - forcing complete recalculation")
    }
  }

  # Load data using the simplified load_or_calc_from_loop function
  # Let it handle caching internally - no need for separate batch files
  data <- load_or_calc_from_loop(
    filePath = get_cache_file_path(data_type),
    data_loader = data_loader,
    datasets_to_verify = datasets_to_verify,
    combinations_df = requested_combinations,
    use_parallel = use_parallel,
    force_recalc = force_recalc,
    threshold_parallel = NULL,
    extra_global_vars = extra_global_vars
  )

  cache_logger("DEBUG", "Loaded", nrow(data), "rows from loop system")

  # Update cache with new data
  if (nrow(data) > 0) {
    cache_logger("DEBUG", "New data loaded:", nrow(data), "rows")

    # The loop system now handles context changes and missing combinations properly
    # So we can simply use the returned data as the new cache
    current_cache <- data
    cache_logger("DEBUG", "Updated cache with new data from loop system")

    # Update cache
    set_cache(data_type, current_cache)
    cache_logger("DEBUG", "Updated in-memory cache")

    # Save to RDS file with metadata
    save_cache_to_file(data_type, current_cache, current_metadata)
    cache_logger("DEBUG", "Saved to RDS file with metadata")

    cache_logger("DEBUG", "Updated", data_type, "cache:", nrow(current_cache), "total rows")
  } else {
    cache_logger("DEBUG", "No new data loaded")
  }

  # Step 6: Filter data for requested participants/trials
  cache_logger("DEBUG", "Step 6: Filtering data for requested participants/trials")
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
    cache_logger(
      "INFO", "Returning", data_type, "data:", nrow(filtered_data), "rows for",
      length(unique(filtered_data$participant)), "participants,",
      length(unique(filtered_data$trialNum)), "trials"
    )

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

#' Clear corrupted cache files
#' @param data_types Vector of data types to check (NULL for all)
clear_corrupted_caches <- function(data_types = NULL) {
  cache_logger <- create_module_logger("CACHE")

  if (is.null(data_types)) {
    # Get all potential data types from the setup
    data_types <- names(context_vars_config)
  }

  for (data_type in data_types) {
    cache_path <- get_cache_file_path(data_type)
    if (file.exists(cache_path)) {
      # Try to read the cache file to check if it's corrupted
      tryCatch(
        {
          cache_object <- readRDS(cache_path)
          cache_logger("DEBUG", "Cache file", cache_path, "is valid")
        },
        error = function(e) {
          cache_logger("WARN", "Corrupted cache file detected:", cache_path, "-", e$message)
          tryCatch(
            {
              file.remove(cache_path)
              cache_logger("INFO", "Removed corrupted cache file:", cache_path)
              # Also clear in-memory cache
              clear_cache(data_type)
            },
            error = function(remove_error) {
              cache_logger("ERROR", "Could not remove corrupted cache file:", remove_error$message)
            }
          )
        }
      )
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

#' Debug cache information for a specific data type
#' @param data_type The type of data to debug
#' @return List with detailed cache information
debug_cache <- function(data_type) {
  cache_logger <- create_module_logger("CACHE-DEBUG")

  cache_logger("INFO", "=== Cache Debug Information for", data_type, "===")

  # Check in-memory cache
  in_memory_data <- get_current_cache(data_type)
  in_memory_filters <- get_current_cache_filters(data_type)

  cache_logger("INFO", "In-memory cache:")
  cache_logger("INFO", "  Rows:", nrow(in_memory_data))
  if (nrow(in_memory_data) > 0) {
    cache_logger("INFO", "  Participants:", length(unique(in_memory_data$participant)))
    cache_logger("INFO", "  Trials:", length(unique(in_memory_data$trialNum)))
    cache_logger("INFO", "  Columns:", paste(names(in_memory_data), collapse = ", "))
  }

  cache_logger("INFO", "In-memory filters:")
  cache_logger("INFO", "  Context vars:", if (is.null(in_memory_filters$context_vars)) "NULL" else paste(names(in_memory_filters$context_vars), collapse = ", "))

  # Check RDS file
  cache_path <- get_cache_file_path(data_type)
  cache_logger("INFO", "RDS file:")
  cache_logger("INFO", "  Path:", cache_path)
  cache_logger("INFO", "  Exists:", file.exists(cache_path))

  if (file.exists(cache_path)) {
    file_info <- file.info(cache_path)
    cache_logger("INFO", "  Size:", round(file_info$size / (1024 * 1024), 2), "MB")
    cache_logger("INFO", "  Modified:", as.character(file_info$mtime))

    # Try to read the file to check for corruption
    tryCatch(
      {
        cache_object <- readRDS(cache_path)
        if (is.data.frame(cache_object)) {
          cache_logger("INFO", "  Format: Legacy (data.frame only)")
          cache_logger("INFO", "  RDS Rows:", nrow(cache_object))
        } else if (is.list(cache_object) && "data" %in% names(cache_object)) {
          cache_logger("INFO", "  Format: New (with metadata)")
          cache_logger("INFO", "  RDS Rows:", nrow(cache_object$data))
          cache_logger("INFO", "  RDS Timestamp:", as.character(cache_object$timestamp))
          if (!is.null(cache_object$metadata)) {
            cache_logger("INFO", "  RDS Metadata:", paste(names(cache_object$metadata), collapse = ", "))
          }
        }
      },
      error = function(e) {
        cache_logger("ERROR", "  Status: CORRUPTED -", e$message)
      }
    )
  }

  # Check current context
  current_context <- capture_context_vars(data_type)
  cache_logger("INFO", "Current context:")
  cache_logger("INFO", "  Variables:", if (is.null(current_context)) "NULL" else paste(names(current_context), collapse = ", "))
  if (!is.null(current_context)) {
    for (var_name in names(current_context)) {
      cache_logger("INFO", "  ", var_name, ":", current_context[[var_name]])
    }
  }

  cache_logger("INFO", "=== End Cache Debug Information ===")

  return(list(
    in_memory_data = in_memory_data,
    in_memory_filters = in_memory_filters,
    cache_path = cache_path,
    current_context = current_context
  ))
}
