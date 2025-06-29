getTypes <- function(dt) {
    numericDataTypes <- sapply(dt, is.numeric)
    logicalDataTypes <- sapply(dt, is.logical)
    dataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes])
}

############ FUNCTIONS
#' Load cached results or calculate them if not available
#'
#' This function checks if a file exists and loads it. If not, it runs the
#' calculation function with the appropriate loop function and saves the result.
#' This is useful for expensive calculations that you want to cache.
#'
#' The parallel processing decision is made here and the appropriate loop function
#' (get_data_from_loop or get_data_from_loop_parallel) is passed to the calculation
#' function, providing clean separation of concerns.
#'
#' @param filePath Path to the cached file (usually .rds)
#' @param calculate_function Function to call if cache doesn't exist (must accept loop_function as first parameter)
#' @param parallel Whether to use parallel processing (default: USE_PARALLEL global setting)
#' @param ... Additional arguments passed to calculate_function
#' @return The loaded or calculated data
#'
#' @examples
#' # Basic usage with parallel processing controlled by global setting
#' allGaitParams <- load_or_calculate("results/gait.rds", calc_all_gait_params)
#'
#' # Examples with different functions (loop function is chosen automatically)
#' allQResults <- load_or_calculate("results/quest.rds", get_all_questionnaire_results)
#' allTaskMetrics <- load_or_calculate("results/task.rds", get_all_task_metrics)
#'
#' # You can still override the global setting if needed
#' allGaitParams <- load_or_calculate("results/gait.rds", calc_all_gait_params, parallel = FALSE)
#'
load_or_calculate <- function(filePath, calculate_function, parallel = USE_PARALLEL, force_recalc = FORCE_RECALC) {
    if (!force_recalc && file.exists(filePath)) {
        data <- readRDS(filePath)
    } else {
        # Choose the appropriate loop function based on parallel setting
        if (parallel) {
            # Extract the main function name for meaningful log file names
            main_func_name <- deparse(substitute(calculate_function))
            log_file_name <- sprintf("parallel_log_%s.txt", main_func_name)

            loop_function <- function(calc_func, ...) {
                get_data_from_loop_parallel(calc_func, ..., log_to_file = ENABLE_FILE_LOGGING, log_file = log_file_name)
            }
        } else {
            loop_function <- get_data_from_loop
        }

        # Pass the loop function to the calculation function
        data <- calculate_function(loop_function)

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

#' Create and configure a parallel cluster optimized for efficiency
#' This function creates a cluster with efficient data handling and
#' minimal data copying to reduce overhead.
#'
#' @param numCores Number of cores to use. If NULL, will auto-detect (detectCores() - 1)
#' @param maxJobs Maximum number of jobs that will be processed. Cores will not exceed this number.
#' @return A configured cluster object so it can be reused across multiple calls.
create_parallel_cluster <- function(numCores = NULL, maxJobs = NULL) {
    # Use standard core detection if not specified
    available_cores <- detectCores()
    if (is.null(numCores)) {
        numCores <- max(1, available_cores - 1) # Leave 1 core for system
        cat(sprintf("[%s] Auto-detected %d cores (using %d of %d available)\n", format(Sys.time(), "%H:%M:%S"), numCores, numCores, available_cores))
    } else {
        cat(sprintf("[%s] Using %d cores (requested: %d, available: %d)\n", format(Sys.time(), "%H:%M:%S"), numCores, numCores, available_cores))
    }

    # Ensure we have at least 1 core
    numCores <- max(1, numCores)

    # Don't use more cores than jobs available
    if (!is.null(maxJobs) && maxJobs > 0) {
        if (numCores > maxJobs) {
            cat(sprintf("[%s] Optimizing: reducing cores from %d to %d to match number of jobs\n", format(Sys.time(), "%H:%M:%S"), numCores, maxJobs))
            numCores <- maxJobs
        } else {
            cat(sprintf("[%s] Core allocation: using %d cores for %d jobs\n", format(Sys.time(), "%H:%M:%S"), numCores, maxJobs))
        }
    }

    cat(sprintf("[%s] Creating cluster with %d worker processes...\n", format(Sys.time(), "%H:%M:%S"), numCores))
    cl <- makeCluster(numCores)
    registerDoParallel(cl)

    cat(sprintf("[%s] Initializing worker processes (loading packages and data)...\n", format(Sys.time(), "%H:%M:%S")))

    # Load packages and source files once on each worker
    clusterEvalQ(cl, {
        # Set a flag to prevent recursive initialization during sourcing
        .WORKER_INITIALIZING <<- TRUE

        tryCatch(
            {
                # Source files in dependency order
                source("source/setup.R", local = FALSE)
                source("source/initialization.R", local = FALSE) # Load initialization first
                source("source/pre_processing.R", local = FALSE) # Load pre_processing before data_loading
                source("source/data_loading.R", local = FALSE)
                source("source/complexity.R", local = FALSE)
                source("source/find_foot_events.R", local = FALSE)
                source("source/calc_all_gait_params.R", local = FALSE)
                source("source/profile_shapes.R", local = FALSE)
                source("source/simulation_core.R", local = FALSE)
                source("source/get_simulation_data.R", local = FALSE)
                source("source/summarize_simulation.R", local = FALSE)
                source("source/summarize_gaitparams.R", local = FALSE)

                # Clear the flag before initialization
                rm(.WORKER_INITIALIZING, envir = .GlobalEnv)

                # Initialize heavy data once per worker (after all functions are available)
                ensure_global_data_initialized()

                # Force garbage collection after initialization
                gc()
            },
            error = function(e) {
                # Clean up flag on error
                if (exists(".WORKER_INITIALIZING", envir = .GlobalEnv)) {
                    rm(.WORKER_INITIALIZING, envir = .GlobalEnv)
                }
                stop("Worker initialization failed: ", e$message)
            }
        )
    })

    cat(sprintf("[%s] Worker initialization completed.\n", format(Sys.time(), "%H:%M:%S")))

    return(cl)
}

#' Parallel processing with optimized comprehensive logging
#'
#' This function processes data in parallel with optional comprehensive logging
#' that captures all console output from parallel workers and consolidates it
#' into a single log file. Logging is optimized to collect messages in memory
#' and write to file only once at the end for better performance.
#'
#' @param get_data_function Function to call for each participant/trial combination
#' @param datasets_to_verify Vector of dataset names to verify before processing
#' @param cl Existing cluster object (if NULL, creates new cluster)
#' @param log_to_file Logical, whether to enable logging to file (default: TRUE)
#' @param log_file Character, custom log filename (default: "parallel_log_[function].txt", overwrites previous runs)
#' @param ... Additional arguments passed to get_data_function
#'
#' @examples
#' # Basic usage with logging (default) - creates parallel_log_calc_all_gait_params.txt
#' data <- get_data_from_loop_parallel(calc_all_gait_params)
#'
#' # Disable logging
#' data <- get_data_from_loop_parallel(calc_all_gait_params, log_to_file = FALSE)
#'
#' # Custom log filename
#' data <- get_data_from_loop_parallel(calc_all_gait_params,
#'     log_file = "my_parallel_log.txt"
#' )
#'
#' # Use existing cluster
#' cl <- create_parallel_cluster()
#' data <- get_data_from_loop_parallel(calc_all_gait_params, cl = cl)
#'
get_data_from_loop_parallel <- function(get_data_function, datasets_to_verify = c("leftfoot", "rightfoot", "hip"),
                                        cl = NULL, log_to_file = TRUE, log_file = NULL, ...) {
    start_time <- Sys.time()
    valid_combinations <- get_valid_combinations(datasets_to_verify)

    # Set up logging if requested
    log_messages <- character(0) # Collect log messages in memory
    log_file_path <- NULL

    if (log_to_file) {
        # Create default log filename if not provided (no timestamp for reuse)
        if (is.null(log_file)) {
            func_name <- deparse(substitute(get_data_function))
            log_file <- sprintf("parallel_log_%s.txt", func_name)
        }

        # Ensure results directory exists
        if (!dir.exists("results")) {
            dir.create("results", recursive = TRUE)
        }
        log_file_path <- file.path("results", log_file)

        # Store header information in memory
        log_messages <- c(
            log_messages,
            sprintf("=== Parallel Processing Log Started: %s ===", Sys.time()),
            sprintf("Function: %s", deparse(substitute(get_data_function))),
            sprintf("Datasets to verify: %s", paste(datasets_to_verify, collapse = ", ")),
            sprintf("Total jobs: %d", nrow(valid_combinations)),
            ""
        )
    }

    # Optimized logging function - collect in memory, write at end
    log_output <- function(text) {
        cat(text) # Always show in console for parallel processing
        if (log_to_file) {
            # Clean up text and add to memory collection
            clean_text <- gsub("\r", "", gsub("\n$", "", text))
            log_messages <<- c(log_messages, clean_text)
        }
    }

    # Log parallel processing setup
    log_output(sprintf("\n=== Starting Parallel Processing ===\n"))
    if (log_to_file) {
        log_output(sprintf("[%s] Logging enabled: %s\n", format(Sys.time(), "%H:%M:%S"), log_file_path))
    }
    log_output(sprintf("[%s] Function: %s\n", format(Sys.time(), "%H:%M:%S"), deparse(substitute(get_data_function))))
    log_output(sprintf("[%s] Total jobs: %d\n", format(Sys.time(), "%H:%M:%S"), nrow(valid_combinations)))

    # Use existing cluster or create our own
    own_cluster <- FALSE
    if (is.null(cl)) {
        cl <- create_parallel_cluster(maxJobs = nrow(valid_combinations))
        own_cluster <- TRUE
        log_output(sprintf("[%s] Created new cluster with %d cores\n", format(Sys.time(), "%H:%M:%S"), length(cl)))
    } else {
        registerDoParallel(cl)
        log_output(sprintf("[%s] Using existing cluster with %d cores\n", format(Sys.time(), "%H:%M:%S"), length(cl)))
    }

    # Export only essential variables - minimize memory copying
    # Only export what's absolutely necessary for the worker functions
    essential_vars <- c("get_data_function")

    # Check which variables actually exist before exporting
    available_vars <- ls(envir = environment())
    vars_to_export <- intersect(essential_vars, available_vars)

    if (length(vars_to_export) > 0) {
        clusterExport(cl, vars_to_export, envir = environment())
    }

    # Start parallel processing
    log_output(sprintf("[%s] Starting parallel execution...\n", format(Sys.time(), "%H:%M:%S")))
    processing_start <- Sys.time()

    # Use list building instead of .combine = rbind for much better performance
    data_list <- foreach(
        i = 1:nrow(valid_combinations),
        .packages = c("data.table", "dplyr", "pracma", "signal", "zoo"),
        .options.snow = list(preschedule = FALSE) # Better load balancing
    ) %dopar% {
        tryCatch(
            {
                # Extract participant and trial for this iteration
                participant <- valid_combinations$participant[i]
                trial <- valid_combinations$trial[i]

                # Track timing for this job
                job_start <- Sys.time()

                # Capture output from the calculation function
                captured_output <- capture.output({
                    newData <- get_data_function(participant, trial, ...)
                    newData <- add_identifiers_and_categories(as.data.frame(newData), participant, trial)
                })

                # Force garbage collection after processing each item
                gc()

                job_end <- Sys.time()
                job_duration <- as.numeric(difftime(job_end, job_start, units = "secs"))

                # Return successful result with metadata and captured output
                list(
                    success = TRUE,
                    data = newData,
                    participant = participant,
                    trial = trial,
                    duration = job_duration,
                    rows = nrow(newData),
                    captured_output = captured_output
                )
            },
            error = function(e) {
                # Return detailed error information
                list(
                    error = TRUE,
                    success = FALSE,
                    message = e$message,
                    participant = if (exists("participant")) participant else "unknown",
                    trial = if (exists("trial")) trial else "unknown",
                    call = as.character(e$call)
                )
            }
        )
    }

    processing_end <- Sys.time()
    processing_duration <- as.numeric(difftime(processing_end, processing_start, units = "secs"))
    log_output(sprintf("[%s] Parallel execution completed in %.2f seconds\n", format(Sys.time(), "%H:%M:%S"), processing_duration))

    # Stop the cluster if we created it inside this function
    if (own_cluster) {
        log_output(sprintf("[%s] Shutting down cluster...\n", format(Sys.time(), "%H:%M:%S")))
        tryCatch(
            {
                stopCluster(cl)
                log_output(sprintf("[%s] Cluster shutdown completed.\n", format(Sys.time(), "%H:%M:%S")))
            },
            error = function(e) {
                warning("Error during cluster shutdown: ", e$message)
            }
        )
    }

    # Process results and separate successful from failed jobs
    successful_results <- sapply(data_list, function(x) is.list(x) && !is.null(x$success) && x$success)
    error_results <- sapply(data_list, function(x) is.list(x) && !is.null(x$error) && x$error)

    # Collect captured output from all workers in memory
    if (log_to_file && any(successful_results)) {
        log_output(sprintf("\n=== Detailed Worker Output ===\n"))
        successful_data <- data_list[successful_results]
        for (i in 1:length(successful_data)) {
            result <- successful_data[[i]]
            if (!is.null(result$captured_output) && length(result$captured_output) > 0) {
                # Add worker output header to log messages
                log_messages <- c(
                    log_messages,
                    sprintf(
                        "\n--- P%s-T%s Output (%.2fs, %d rows) ---",
                        result$participant, result$trial, result$duration, result$rows
                    )
                )
                # Add all captured output lines
                log_messages <- c(log_messages, result$captured_output)
            }
        }
        log_output(sprintf("=== End Detailed Worker Output ===\n\n"))
    }

    # Log detailed results summary
    log_output(sprintf("\n=== Processing Results Summary ===\n"))
    log_output(sprintf("[%s] Total jobs: %d\n", format(Sys.time(), "%H:%M:%S"), length(data_list)))
    log_output(sprintf("[%s] Successful: %d\n", format(Sys.time(), "%H:%M:%S"), sum(successful_results)))
    log_output(sprintf("[%s] Failed: %d\n", format(Sys.time(), "%H:%M:%S"), sum(error_results)))

    # Report on successful jobs
    if (any(successful_results)) {
        successful_data <- data_list[successful_results]
        total_rows <- sum(sapply(successful_data, function(x) x$rows))
        avg_duration <- mean(sapply(successful_data, function(x) x$duration))
        log_output(sprintf("[%s] Total rows processed: %d\n", format(Sys.time(), "%H:%M:%S"), total_rows))
        log_output(sprintf("[%s] Average job duration: %.2f seconds\n", format(Sys.time(), "%H:%M:%S"), avg_duration))

        # Show details for each successful job
        log_output(sprintf("[%s] Successful jobs:\n", format(Sys.time(), "%H:%M:%S")))
        for (i in 1:length(successful_data)) {
            result <- successful_data[[i]]
            log_output(sprintf(
                "  ✓ P%s-T%s: %d rows (%.2fs)\n",
                result$participant, result$trial, result$rows, result$duration
            ))
        }
    }

    # Report errors if any occurred
    if (any(error_results)) {
        error_data <- data_list[error_results]
        log_output(sprintf("\n[%s] Errors occurred in %d jobs:\n", format(Sys.time(), "%H:%M:%S"), length(error_data)))
        for (i in 1:length(error_data)) {
            error_info <- error_data[[i]]
            log_output(sprintf(
                "  ✗ P%s-T%s: %s\n",
                error_info$participant, error_info$trial, error_info$message
            ))
            if (!is.null(error_info$call)) {
                log_output(sprintf("    Call: %s\n", error_info$call))
            }
        }
    }

    if (!any(successful_results)) {
        warning("No data was successfully processed from the valid combinations.")
        return(data.frame())
    }

    # Extract just the data frames from successful results
    log_output(sprintf("[%s] Extracting data from successful results...\n", format(Sys.time(), "%H:%M:%S")))
    successful_data_frames <- lapply(data_list[successful_results], function(x) x$data)

    # Handle factor level mismatches before combining
    log_output(sprintf("[%s] Preparing data for combination (handling factor levels)...\n", format(Sys.time(), "%H:%M:%S")))
    successful_data_frames <- lapply(successful_data_frames, function(df) {
        # Convert all factors to characters to avoid any level mismatch issues
        # This is the safest approach when combining data from parallel processes
        for (col_name in names(df)) {
            if (is.factor(df[[col_name]]) || is.ordered(df[[col_name]])) {
                df[[col_name]] <- as.character(df[[col_name]])
            }
        }
        return(df)
    })

    # Combine the list of data frames efficiently using rbindlist instead of rbind
    log_output(sprintf("[%s] Combining %d data frames...\n", format(Sys.time(), "%H:%M:%S"), length(successful_data_frames)))
    data <- tryCatch(
        {
            data.table::rbindlist(successful_data_frames, fill = TRUE)
        },
        error = function(e) {
            log_output(sprintf("[%s] Error with rbindlist, falling back to do.call(rbind, ...)...\n", format(Sys.time(), "%H:%M:%S")))
            warning("rbindlist failed, using rbind fallback: ", e$message)
            do.call(rbind, successful_data_frames)
        }
    )
    log_output(sprintf("[%s] Data combination completed, converting to data.frame...\n", format(Sys.time(), "%H:%M:%S")))
    data <- as.data.frame(data) # Convert back to data.frame for compatibility

    # Clean up memory
    log_output(sprintf("[%s] Cleaning up memory...\n", format(Sys.time(), "%H:%M:%S")))
    rm(data_list, successful_data_frames)
    suppressWarnings(gc()) # Suppress connection closing warnings during cleanup
    log_output(sprintf("[%s] Memory cleanup completed.\n", format(Sys.time(), "%H:%M:%S")))

    # Final summary
    end_time <- Sys.time()
    total_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

    log_output(sprintf("\n=== Final Summary ===\n"))
    log_output(sprintf("[%s] Total processing time: %.2f seconds\n", format(Sys.time(), "%H:%M:%S"), total_duration))
    log_output(sprintf("[%s] Final dataset: %d rows, %d columns\n", format(Sys.time(), "%H:%M:%S"), nrow(data), ncol(data)))

    if (nrow(data) == 0) {
        warning("No data was successfully processed from the valid combinations.")
    } else {
        log_output(sprintf("[%s] ✓ Parallel processing completed successfully!\n\n", format(Sys.time(), "%H:%M:%S")))
    }

    # Write all collected log messages to file at once (optimized I/O)
    if (log_to_file && !is.null(log_file_path)) {
        tryCatch(
            {
                # Add final completion message
                log_messages <- c(
                    log_messages,
                    sprintf("=== Parallel Processing Log Completed: %s ===", Sys.time())
                )

                # Write all messages to file in a single operation
                writeLines(log_messages, log_file_path)
                cat(sprintf(
                    "[%s] Log written to: %s (%d lines)\n",
                    format(Sys.time(), "%H:%M:%S"), log_file_path, length(log_messages)
                ))
            },
            error = function(e) {
                warning("Error writing log file: ", e$message)
            }
        )
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

    cat(sprintf("[%s] Verifying data availability...\n", format(Sys.time(), "%H:%M:%S")))
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
        "\n[%s] Found %d valid combinations out of %d total combinations.\n",
        format(Sys.time(), "%H:%M:%S"), nrow(valid_combinations), length(participants) * length(allTrials)
    ))

    return(valid_combinations)
}
