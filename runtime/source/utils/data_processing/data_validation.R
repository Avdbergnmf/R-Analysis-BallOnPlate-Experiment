# Helper function to verify data availability for a trial
verify_trial_data <- function(participant, trial, datasets_to_verify) {
    # Initialize results list
    results <- list()

    # Check each required dataset
    for (dataset in datasets_to_verify) {
        # Check if file exists using the helper function
        exists <- check_file_exists(participant, dataset, trial)

        results[[dataset]] <- list(
            exists = exists,
            has_data = exists # We'll assume if it exists, it has data - actual check will happen during processing
        )
    }

    return(results)
}

# Helper function to print verification results
print_verification_results <- function(verification_results) {
    verification_logger <- create_module_logger("VERIFICATION")
    
    # Build list of results instead of repeated rbind for efficiency
    results_list <- list()

    for (participant in names(verification_results)) {
        for (trial in names(verification_results[[participant]])) {
            trial_results <- verification_results[[participant]][[trial]]

            # Create rows for each dataset
            for (dataset in names(trial_results)) {
                result <- trial_results[[dataset]]
                results_list[[length(results_list) + 1]] <- data.frame(
                    participant = participant,
                    trial = trial,
                    dataset = dataset,
                    exists = result$exists,
                    has_data = result$has_data,
                    stringsAsFactors = FALSE
                )
            }
        }
    }

    # Combine all results efficiently using rbindlist
    results_df <- data.table::rbindlist(results_list)

    # Print summary
    verification_logger("INFO", "Data Verification Results:\n========================")

    # Group by participant and trial
    for (participant in unique(results_df$participant)) {
        verification_logger("INFO", sprintf("Participant: %s", participant))
        for (trial in unique(results_df$trial[results_df$participant == participant])) {
            verification_logger("INFO", sprintf("  Trial %s:", trial))
            trial_data <- results_df[results_df$participant == participant & results_df$trial == trial, ]

            # Check if all required datasets are present and have data
            all_valid <- all(trial_data$exists & trial_data$has_data)

            for (i in 1:nrow(trial_data)) {
                status <- if (trial_data$exists[i] & trial_data$has_data[i]) "✓" else "✗"
                verification_logger("INFO", sprintf(
                    "    %s %s: %s",
                    status,
                    trial_data$dataset[i],
                    if (trial_data$exists[i]) {
                        if (trial_data$has_data[i]) "has data" else "empty file"
                    } else {
                        "missing file"
                    }
                ))
            }

            # Only mark as VALID if all required files exist and have data
            verification_logger("INFO", sprintf("    Overall: %s", if (all_valid) "VALID" else "INVALID"))
        }
    }

    return(as.data.frame(results_df))
}

get_valid_combinations <- function(datasets_to_verify) {
    combinations_logger <- create_module_logger("COMBINATIONS")
    
    # Verify data availability for all combinations
    verification_results <- list()
    valid_combinations_list <- list() # Use list instead of growing data frame

    combinations_logger("INFO", "Verifying data availability...")
    for (participant in participants) {
        for (trial in allTrials) {
            # Initialize participant in results if needed
            if (is.null(verification_results[[participant]])) {
                verification_results[[participant]] <- list()
            }

            # Verify data for this combination
            verification_results[[participant]][[trial]] <- verify_trial_data(participant, trial, datasets_to_verify)

            # Check if all required datasets are present and have data
            all_valid <- all(sapply(verification_results[[participant]][[trial]], function(x) x$exists && x$has_data))

            if (all_valid) {
                # Add to list instead of rbind for efficiency
                valid_combinations_list[[length(valid_combinations_list) + 1]] <- data.frame(participant = participant, trial = trial)
            }
        }
    }

    # Combine valid combinations efficiently using rbindlist
    valid_combinations <- if (length(valid_combinations_list) > 0) {
        data.table::rbindlist(valid_combinations_list)
    } else {
        data.frame()
    }
    valid_combinations <- as.data.frame(valid_combinations) # Convert back for compatibility

    # Print verification results
    print_verification_results(verification_results)

    if (nrow(valid_combinations) == 0) {
        combinations_logger("WARN", sprintf(
            "No valid combinations found for datasets: %s. Please check your data files.",
            paste(datasets_to_verify, collapse = ", ")
        ))
        return(data.frame())
    }

    combinations_logger("INFO", sprintf(
        "Found %d valid combinations out of %d total combinations.",
        nrow(valid_combinations), length(participants) * length(allTrials)
    ))

    return(valid_combinations)
}

#' Get valid combinations - either from provided combinations_df or by discovering all valid combinations
#' @param combinations_df Optional data frame with participant and trial columns
#' @param datasets_to_verify Vector of dataset names to verify
#' @param logger Logger function for output
#' @return Data frame with valid participant-trial combinations
get_valid_combinations_for_processing <- function(combinations_df = NULL, datasets_to_verify = c("leftfoot", "rightfoot", "hip"), logger = NULL) {
    if (!is.null(combinations_df)) {
        if (!is.null(logger)) {
            logger("INFO", sprintf("Using %d specific combinations", nrow(combinations_df)))
        }
        return(combinations_df)
    } else {
        return(get_valid_combinations(datasets_to_verify))
    }
}

#' Handle missing combinations in loaded data
#' This is a helper function to check for missing combinations and load them
#' if allow_add_missing is enabled
#'
#' @param data Existing loaded data
#' @param combinations_df Requested combinations data frame
#' @param calculate_function Function to extract data loader from
#' @param threshold_parallel Threshold for using parallel processing (uses global THRESHOLD_PARALLEL if NULL)
#' @param extra_global_vars Additional global variables for parallel processing
#' @param filePath Path to save updated data
#' @return Updated data with missing combinations loaded
handle_missing_combinations <- function(data, combinations_df, calculate_function, 
                                       threshold_parallel, extra_global_vars, filePath) {
    # Use global threshold if not provided
    if (is.null(threshold_parallel)) {
        ensure_global_data_initialized()
        threshold_parallel <- THRESHOLD_PARALLEL
    }
    
    missing_logger <- create_module_logger("MISSING-CHECK")
    missing_logger("DEBUG", "Checking for missing combinations in loaded data")
    
    # Get requested combinations
    requested_combinations <- combinations_df
    missing_logger("DEBUG", "Requested", nrow(requested_combinations), "combinations")
    
    # Check which combinations we have
    existing_combinations <- unique(paste(data$participant, data$trialNum, sep = "_"))
    missing_logger("DEBUG", "Found", length(existing_combinations), "existing combinations in loaded data")
    
    # Find missing combinations
    requested_combinations$combo_id <- paste(requested_combinations$participant, requested_combinations$trial, sep = "_")
    missing_combinations <- requested_combinations[!requested_combinations$combo_id %in% existing_combinations, ]
    missing_combinations$combo_id <- NULL
    
    if (nrow(missing_combinations) > 0) {
        missing_logger("INFO", "Found", nrow(missing_combinations), "missing combinations, loading them...")
        
        # Determine if we should use parallel for missing combinations
        use_parallel_missing <- nrow(missing_combinations) >= threshold_parallel
        missing_logger("DEBUG", "Using parallel for missing combinations:", use_parallel_missing, 
                      "(threshold:", threshold_parallel, ", missing:", nrow(missing_combinations), ")")
        
        # Extract the data loader function from calculate_function
        loader_info <- extract_data_loader_from_calculate_function(calculate_function)
        
        if (!is.null(loader_info)) {
            # Load missing combinations using the extracted data loader
            missing_data <- load_missing_combinations(
                data_loader = loader_info$data_loader,
                datasets_to_verify = loader_info$datasets_to_verify,
                missing_combinations = missing_combinations,
                use_parallel = use_parallel_missing,
                extra_global_vars = extra_global_vars
            )
            
            if (nrow(missing_data) > 0) {
                missing_logger("INFO", "Successfully loaded", nrow(missing_data), "rows for missing combinations")
                
                # Combine with existing data
                data <- rbind(data, missing_data)
                missing_logger("INFO", "Combined data now has", nrow(data), "total rows")
                
                # Save the updated data
                saveRDS(data, filePath)
                missing_logger("DEBUG", "Saved updated data to", filePath)
            } else {
                missing_logger("WARN", "No data was loaded for missing combinations")
            }
        } else {
            missing_logger("ERROR", "Could not extract data loader function from calculate_function")
        }
    } else {
        missing_logger("DEBUG", "All requested combinations already present in loaded data")
    }
    
    return(data)
}

#' Load missing combinations using the loop system
#' This is a helper function to load missing combinations with appropriate
#' parallel/sequential processing based on the number of missing combinations
#'
#' @param data_loader Function to load data for a participant-trial combination
#' @param datasets_to_verify Vector of dataset names to verify
#' @param missing_combinations Data frame with participant and trial columns
#' @param use_parallel Whether to use parallel processing
#' @param extra_global_vars Additional global variables to export to parallel workers
#' @return Data frame with loaded missing combinations data
load_missing_combinations <- function(data_loader, datasets_to_verify, missing_combinations, 
                                     use_parallel, extra_global_vars = NULL) {
  missing_logger <- create_module_logger("LOAD-MISSING")
  
  missing_logger("DEBUG", "Loading", nrow(missing_combinations), "missing combinations")
  missing_logger("DEBUG", "Using parallel:", use_parallel)
  
  if (use_parallel) {
    # Use parallel processing for missing combinations
    if (!exists(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv) ||
        is.null(.GlobalEnv$.GLOBAL_PARALLEL_CLUSTER)) {
      assign(".GLOBAL_PARALLEL_CLUSTER", create_parallel_cluster(), envir = .GlobalEnv)
    }
    cl <- .GlobalEnv$.GLOBAL_PARALLEL_CLUSTER
    
    missing_data <- get_data_from_loop_parallel(
      get_data_function = data_loader,
      datasets_to_verify = datasets_to_verify,
      cl = cl,
      log_to_file = FALSE, # Don't create separate log files for missing data
      combinations_df = missing_combinations,
      extra_global_vars = extra_global_vars
    )
  } else {
    # Use sequential processing for missing combinations
    missing_data <- get_data_from_loop(
      get_data_function = data_loader,
      datasets_to_verify = datasets_to_verify,
      combinations_df = missing_combinations
    )
  }
  
  missing_logger("DEBUG", "Loaded", nrow(missing_data), "rows for missing combinations")
  return(missing_data)
}
