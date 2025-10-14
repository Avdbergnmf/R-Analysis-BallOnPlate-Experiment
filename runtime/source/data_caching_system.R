#' Data Caching System
#'
#' A flexible caching system that can be applied to different types of datasets.
#' Supports RDS file persistence and incremental loading of missing data.

# =============================================================================
# GLOBAL CACHE STORAGE
# =============================================================================

# Global cache storage for different data types
# Structure: list(type_name = list(data = data.frame(), filters = list()))
global_data_cache <<- list()

# =============================================================================
# DATA LOADER FUNCTIONS
# =============================================================================

#' Define data loader functions for different data types
#' Each function should take (participant, trial) and return a data.frame
data_loaders <<- list(
  "simulation" = function(participant, trial) {
    # Load simulation data for a specific participant and trial
    sim_data <- get_simulation_data(participant, trial)
    if (!is.null(sim_data) && nrow(sim_data) > 0) {
      # Add metadata
      sim_data$condition <- condition_number(participant)
      sim_data$participant <- participant
      sim_data$trialNum <- trial
    }
    return(sim_data)
  }
  # Add more data loaders here as needed
  # "gait" = function(participant, trial) { ... },
  # "questionnaire" = function(participant, trial) { ... }
)

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
  if (data_type == "simulation") {
    return(has_simulation_data(participant, trial))
  }
  # Add checks for other data types here
  return(FALSE)
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
