# Generic data caching system for Shiny applications
# This provides reusable caching functionality similar to the simulation data system

#' Create a data cache manager
#' @param cache_name Unique name for this cache (e.g., "simulation", "tracker_hip", "tracker_pelvis")
#' @return List of functions to manage the cache
create_data_cache_manager <- function(cache_name) {
  # Create unique variable names for this cache
  trigger_var <- paste0(".", cache_name, "DataTrigger")
  cancel_var <- paste0(".", cache_name, "DataCancel")
  cache_var <- paste0(".", cache_name, "DataCache")
  filters_var <- paste0(".", cache_name, "DataFilters")
  notification_var <- paste0(".", cache_name, "DataNotificationId")
  progress_var <- paste0(".", cache_name, "DataProgress")
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
  
  progress <- function(value = NULL) {
    if (!exists(progress_var, envir = .GlobalEnv) || is.null(get(progress_var, envir = .GlobalEnv))) {
      assign(progress_var, reactiveVal(list(current = 0, total = 0, combinations = data.frame())), envir = .GlobalEnv)
    }
    progress_val <- get(progress_var, envir = .GlobalEnv)
    if (is.null(value)) {
      progress_val()
    } else {
      progress_val(value)
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
    progress = progress,
    loading = loading,
    clear_notifications = clear_notifications,
    show_notification = show_notification,
    request_data = request_data,
    cancel_data = cancel_data,
    reset_data = reset_data
  )
}

#' Create a generic data loading reactive with caching
#' @param cache_manager Cache manager created with create_data_cache_manager
#' @param load_function Function that loads data for a single participant-trial combination
#' @param filter_function Function that extracts current filter state
#' @param validate_function Function that validates if data exists for participant-trial
#' @param cache_name Name for notifications
#' @return Reactive that returns cached data
create_cached_data_reactive <- function(cache_manager, load_function, filter_function, validate_function, cache_name) {
  reactive({
    # Only depend on the trigger (button clicks), not on filter changes
    trigger_value <- cache_manager$trigger()
    
    # If trigger is 0, no update has been requested yet - return empty data frame
    if (trigger_value == 0) {
      return(data.frame())
    }
    
    # Use isolate() to prevent reactive dependencies on filter inputs
    current_filters <- isolate(filter_function())
    
    # If filter validation failed, return empty data frame
    if (is.null(current_filters)) {
      cache_manager$show_notification(
        paste("Please select participants and trials before loading", cache_name, "data."),
        type = "warning",
        duration = 5
      )
      return(data.frame())
    }
    
    # Smart caching: Check what data we already have and what we need to load
    cached_data <- cache_manager$cache()
    cached_filters <- cache_manager$filters()
    
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
        reusable_data$combo_id <- NULL
        
        # Only load combinations we don't have
        combinations_to_load <- requested_combinations[requested_combinations$combo_id %in% missing_combos, ]
        combinations_to_load$combo_id <- NULL
      }
      
      # Clean up temporary columns
      cached_data$combo_id <- NULL
      requested_combinations$combo_id <- NULL
    }
    
    # If we have all the data we need, return it immediately
    if (nrow(combinations_to_load) == 0 && nrow(reusable_data) > 0) {
      # Apply additional filters to reusable data
      filtered_data <- reusable_data
      if (!is.null(current_filters$condition) && length(current_filters$condition) > 0) {
        filtered_data <- filtered_data[filtered_data$condition %in% current_filters$condition, ]
      }
      
      cache_manager$show_notification(
        sprintf(
          "Using cached %s data (%d participants, %d trials).",
          cache_name,
          length(unique(filtered_data$participant)),
          length(unique(filtered_data$trialNum))
        ),
        type = "message",
        duration = 2
      )
      return(filtered_data)
    }
    
    # Reset cancel flag and start loading
    cache_manager$cancel(FALSE)
    
    # Vectorized check for data existence (only for combinations we need to load)
    valid_combinations_to_load <- data.frame()
    if (nrow(combinations_to_load) > 0) {
      has_data <- mapply(validate_function, combinations_to_load$participant, combinations_to_load$trial)
      valid_combinations_to_load <- combinations_to_load[has_data, ]
      
      if (nrow(valid_combinations_to_load) == 0 && nrow(reusable_data) == 0) {
        cache_manager$show_notification(
          paste("No", cache_name, "data found for the selected filters."),
          type = "warning",
          duration = 5
        )
        return(data.frame())
      }
    }
    
    # Prepare progress information
    reusable_count <- if (nrow(reusable_data) > 0) length(unique(paste(reusable_data$participant, reusable_data$trialNum))) else 0
    total_new_combinations <- nrow(valid_combinations_to_load)
    
    # Load data with progress bar and built-in cancellation
    new_data_list <- list()
    
    if (total_new_combinations > 0) {
      progress_message <- if (reusable_count > 0) {
        sprintf("Loading %d new combinations (%d cached)", total_new_combinations, reusable_count)
      } else {
        sprintf("Loading %d combinations", total_new_combinations)
      }
      
      # Use tryCatch to handle user interruption (cancellation)
      new_data_list <- tryCatch(
        {
          withProgress(
            message = progress_message,
            detail = "Press Esc to cancel",
            value = 0,
            {
              data_list <- list()
              
              for (i in seq_len(total_new_combinations)) {
                # Check for cancellation via button click
                if (cache_manager$cancel()) {
                  cache_manager$show_notification(
                    paste(cache_name, "data loading was cancelled."),
                    type = "warning",
                    duration = 3
                  )
                  return(data_list)
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
                    if (cache_manager$cancel()) {
                      return(data_list)
                    }
                    
                    # Load data (this might take a while per combination)
                    cat(sprintf("Loading P%s-T%s...\n", participant, trial))
                    data <- load_function(participant, trial)
                    cat(sprintf("Completed P%s-T%s\n", participant, trial))
                    
                    # Post-load cancellation check
                    if (cache_manager$cancel()) {
                      cache_manager$show_notification(
                        paste(cache_name, "data loading was cancelled."),
                        type = "warning",
                        duration = 3
                      )
                      return(data_list)
                    }
                    
                    if (!is.null(data) && nrow(data) > 0) {
                      # Check again before processing
                      if (cache_manager$cancel()) {
                        return(data_list)
                      }
                      
                      # Add identifiers
                      data$participant <- participant
                      data$trialNum <- trial
                      data_list[[i]] <- data
                    }
                  },
                  error = function(e) {
                    warning(sprintf(
                      "Error loading %s data for participant %s, trial %s: %s",
                      cache_name, participant, trial, e$message
                    ))
                  }
                )
                
                # Force UI updates and allow cancellation to be processed
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
          cache_manager$show_notification(
            paste(cache_name, "data loading was cancelled by user."),
            type = "warning",
            duration = 3
          )
          if (nrow(reusable_data) > 0) {
            return(list())
          } else {
            stop("Loading cancelled")
          }
        }
      )
    }
    
    # Remove NULL entries from newly loaded data
    new_data_list <- new_data_list[!sapply(new_data_list, is.null)]
    
    # Combine reusable data with newly loaded data
    combined_data <- data.frame()
    
    # Start with reusable data
    if (nrow(reusable_data) > 0) {
      combined_data <- reusable_data
    }
    
    # Add newly loaded data
    if (length(new_data_list) > 0) {
      new_data <- do.call(rbind, new_data_list)
      if (nrow(combined_data) == 0) {
        combined_data <- new_data
      } else {
        combined_data <- rbind(combined_data, new_data)
      }
    }
    
    # Check if we have any data at all
    if (nrow(combined_data) == 0) {
      cache_manager$show_notification(
        paste("No valid", cache_name, "data found."),
        type = "warning",
        duration = 5
      )
      return(data.frame())
    }
    
    # Apply additional filters
    if (!is.null(current_filters$condition) && length(current_filters$condition) > 0) {
      combined_data <- combined_data[combined_data$condition %in% current_filters$condition, ]
    }
    
    if (nrow(combined_data) == 0) {
      cache_manager$show_notification(
        paste("No", cache_name, "data matches the condition filter."),
        type = "warning",
        duration = 5
      )
      return(data.frame())
    }
    
    # Cache the results
    cache_manager$cache(combined_data)
    cache_manager$filters(current_filters)
    
    # Show success notification with caching info
    reused_combinations <- if (nrow(reusable_data) > 0) length(unique(paste(reusable_data$participant, reusable_data$trialNum))) else 0
    new_combinations <- if (length(new_data_list) > 0) nrow(valid_combinations_to_load) else 0
    
    if (new_combinations > 0 && reused_combinations > 0) {
      message <- sprintf(
        "✓ Loaded %s data: %d participants, %d trials, %d samples (%d cached, %d newly loaded)",
        cache_name,
        length(unique(combined_data$participant)),
        length(unique(combined_data$trialNum)),
        nrow(combined_data),
        reused_combinations,
        new_combinations
      )
    } else if (reused_combinations > 0) {
      message <- sprintf(
        "✓ Using cached %s data: %d participants, %d trials, %d samples",
        cache_name,
        length(unique(combined_data$participant)),
        length(unique(combined_data$trialNum)),
        nrow(combined_data)
      )
    } else {
      message <- sprintf(
        "✓ Loaded %s data: %d participants, %d trials, %d samples",
        cache_name,
        length(unique(combined_data$participant)),
        length(unique(combined_data$trialNum)),
        nrow(combined_data)
      )
    }
    
    cache_manager$show_notification(message, type = "message", duration = 5)
    
    return(combined_data)
  })
}
