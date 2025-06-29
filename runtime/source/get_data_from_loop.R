getTypes <- function(dt) {
    numericDataTypes <- sapply(dt, is.numeric)
    logicalDataTypes <- sapply(dt, is.logical)
    dataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes])
}

############ FUNCTIONS
# Helper function to load or calculate and save data
load_or_calculate <- function(filePath, calculate_function) {
    if (file.exists(filePath)) {
        data <- readRDS(filePath)
    } else {
        data <- calculate_function()
        saveRDS(data, filePath)
    }
    return(data)
}

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
    cat("\nData Verification Results:\n")
    cat("========================\n")

    # Group by participant and trial
    for (participant in unique(results_df$participant)) {
        cat(sprintf("\nParticipant: %s\n", participant))
        for (trial in unique(results_df$trial[results_df$participant == participant])) {
            cat(sprintf("  Trial %s:\n", trial))
            trial_data <- results_df[results_df$participant == participant & results_df$trial == trial, ]

            # Check if all required datasets are present and have data
            all_valid <- all(trial_data$exists & trial_data$has_data)

            for (i in 1:nrow(trial_data)) {
                status <- if (trial_data$exists[i] & trial_data$has_data[i]) "✓" else "✗"
                cat(sprintf(
                    "    %s %s: %s\n",
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
            cat(sprintf("    Overall: %s\n", if (all_valid) "VALID" else "INVALID"))
        }
    }
    cat("\n")

    return(as.data.frame(results_df))
}

get_data_from_loop_parallel <- function(get_data_function, datasets_to_verify = c("leftfoot", "rightfoot", "hip"), ...) {
    valid_combinations <- get_valid_combinations(datasets_to_verify)

    # Set up parallel backend to use multiple processors
    numCores <- detectCores() - 1 # Leave one core free for system processes
    cl <- makeCluster(numCores)
    registerDoParallel(cl)

    # Load packages once on each worker
    clusterEvalQ(cl, {
        source("source/setup.R", local = FALSE)
    })

    # Export necessary variables and functions to the cluster - improved exports for efficiency
    clusterExport(cl, c(
        "participants", "allTrials", "get_data_function", "add_identifiers_and_categories",
        "dataFolder", "dataExtraFolder", "questionnaireInfoFolder", "qTypes", "qAnswers",
        "trackers", "filenameDict", "categories"
    ), envir = environment())

    # Source only essential functions on each worker (in dependency order)
    clusterEvalQ(cl, {
        source("source/data_loading.R", local = FALSE)
        source("source/pre_processing.R", local = FALSE)
        source("source/find_foot_events.R", local = FALSE)
        source("source/calc_all_gait_params.R", local = FALSE)
        source("source/profile_shapes.R", local = FALSE)
        source("source/simulation_core.R", local = FALSE)
        source("source/get_simulation_data.R", local = FALSE)
        source("source/summarize_simulation.R", local = FALSE)
        source("source/summarize_gaitparams.R", local = FALSE)
    })

    # Use list building instead of .combine = rbind for much better performance
    data_list <- foreach(
        i = 1:nrow(valid_combinations),
        .packages = c("data.table", "dplyr", "pracma", "signal", "zoo")
    ) %dopar% {
        tryCatch(
            {
                # Extract participant and trial for this iteration
                participant <- valid_combinations$participant[i]
                trial <- valid_combinations$trial[i]

                # Calculate gait data and parameters with optional arguments
                newData <- get_data_function(participant, trial, ...)
                newData <- add_identifiers_and_categories(as.data.frame(newData), participant, trial)
                newData # Return the new data frame
            },
            error = function(e) {
                # Return detailed error information
                list(
                    error = TRUE,
                    message = e$message,
                    participant = if (exists("participant")) participant else "unknown",
                    trial = if (exists("trial")) trial else "unknown",
                    call = as.character(e$call)
                )
            }
        )
    }

    # Stop the cluster
    stopCluster(cl)

    # Check for errors in the results
    error_results <- sapply(data_list, function(x) is.list(x) && !is.null(x$error) && x$error)
    if (any(error_results)) {
        error_indices <- which(error_results)
        cat("Errors occurred in parallel processing:\n")
        for (idx in error_indices) {
            error_info <- data_list[[idx]]
            cat(sprintf(
                "  Task %d (Participant: %s, Trial: %s): %s\n",
                idx, error_info$participant, error_info$trial, error_info$message
            ))
            if (!is.null(error_info$call)) {
                cat(sprintf("    Call: %s\n", error_info$call))
            }
        }
        # Remove error results from data_list
        data_list <- data_list[!error_results]
    }

    if (length(data_list) == 0) {
        warning("No data was successfully processed from the valid combinations.")
        return(data.frame())
    }

    # Combine the list of data frames efficiently using rbindlist instead of rbind
    data <- data.table::rbindlist(data_list, fill = TRUE)
    data <- as.data.frame(data) # Convert back to data.frame for compatibility

    if (nrow(data) == 0) {
        warning("No data was successfully processed from the valid combinations.")
    }

    return(data)
}

get_data_from_loop <- function(get_data_function, datasets_to_verify = c("leftfoot", "rightfoot", "hip"), ...) {
    valid_combinations <- get_valid_combinations(datasets_to_verify)

    # Initialize list to store data frames instead of growing data frame
    data_list <- list()

    # Variables for progress bar
    total_iterations <- nrow(valid_combinations)
    current_iteration <- 0

    for (i in 1:nrow(valid_combinations)) {
        participant <- valid_combinations$participant[i]
        trial <- valid_combinations$trial[i]

        # PRINT PROGRESS BAR
        current_iteration <- current_iteration + 1
        progress_percent <- (current_iteration / total_iterations) * 100
        progress_bar_length <- 50
        num_hashes <- floor(progress_bar_length * progress_percent / 100)
        num_dashes <- progress_bar_length - num_hashes
        progress_bar <- paste0("[", paste(rep("#", num_hashes), collapse = ""), paste(rep("-", num_dashes), collapse = ""), "]")

        # Print progress bar with the percentage
        cat(sprintf("\rProgress: %s %.2f%% On Participant: %s, Trial: %s\n", progress_bar, progress_percent, participant, trial))
        flush.console()

        # Calculate gait data and parameters
        newData <- get_data_function(participant, trial, ...)
        newData <- add_identifiers_and_categories(as.data.frame(newData), participant, trial)

        # Store in list instead of repeated rbind for efficiency
        data_list[[i]] <- newData
    }

    # Print final progress bar to indicate completion
    cat("\n")

    # Combine all data frames efficiently using rbindlist
    data <- data.table::rbindlist(data_list, fill = TRUE)
    data <- as.data.frame(data) # Convert back to data.frame for compatibility

    if (nrow(data) == 0) {
        warning("No data was successfully processed from the valid combinations.")
    }

    return(data)
}

get_valid_combinations <- function(datasets_to_verify) {
    # Verify data availability for all combinations
    verification_results <- list()
    valid_combinations_list <- list() # Use list instead of growing data frame

    cat("Verifying data availability...\n")
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
        warning(sprintf(
            "No valid combinations found for datasets: %s. Please check your data files.",
            paste(datasets_to_verify, collapse = ", ")
        ))
        return(data.frame())
    }

    cat(sprintf(
        "\nFound %d valid combinations out of %d total combinations.\n",
        nrow(valid_combinations), length(participants) * length(allTrials)
    ))

    return(valid_combinations)
}
