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
#' @param combinations_df Optional data frame with participant and trial columns to process only specific combinations (default: NULL for all combinations)
#' @param allow_add_missing Whether to check for and load missing combinations when loading from cache (default: FALSE, but automatically enabled when combinations_df is provided)
#' @param threshold_parallel Threshold for using parallel processing when loading missing combinations (default: uses global THRESHOLD_PARALLEL)
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
#' # Load specific combinations and allow adding missing ones
#' specific_combinations <- data.frame(participant = c("101", "102"), trial = c("1", "2"))
#' data <- load_or_calculate("results/data.rds", calc_function, 
#'                          combinations_df = specific_combinations, 
#'                          allow_add_missing = TRUE)
#'
# -----------------------------------------------------------------------------
# load_or_calculate ------------------------------------------------------------
# Added global cluster reuse: a single parallel cluster is created on the very
# first call (when `parallel = TRUE`) and stored in `.GLOBAL_PARALLEL_CLUSTER`.
# Subsequent calls reuse the same cluster so we avoid the high overhead of
# repeatedly spawning worker processes.  Pass `stop_cluster = TRUE` on the **last**
# call to gracefully shut the workers down.
# -----------------------------------------------------------------------------
load_or_calculate <- function(filePath,
                              calculate_function,
                              parallel = USE_PARALLEL,
                              force_recalc = FORCE_RECALC,
                              stop_cluster = FALSE,
                              extra_global_vars = NULL,
                              combinations_df = NULL,
                              allow_add_missing = FALSE,
                              threshold_parallel = NULL) {
    
    # Create logger for this function
    logger <- create_module_logger("LOAD-OR-CALC")
    
    logger("DEBUG", "=== load_or_calculate called ===")
    logger("DEBUG", "filePath:", filePath)
    logger("DEBUG", "calculate_function type:", typeof(calculate_function))
    logger("DEBUG", "calculate_function class:", class(calculate_function))
    logger("DEBUG", "parallel:", parallel)
    logger("DEBUG", "force_recalc:", force_recalc)
    logger("DEBUG", "combinations_df:", if(is.null(combinations_df)) "NULL" else paste("rows:", nrow(combinations_df)))
    
    # Unified cluster cleanup: ensure we stop the global cluster on exit when requested
    if (parallel && stop_cluster) {
        logger("DEBUG", "Setting up cluster cleanup on exit")
        on.exit(
            {
                if (exists(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv)) {
                    tryCatch(
                        {
                            cl_to_stop <- get(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv)
                            logger("DEBUG", "Stopping global parallel cluster")
                            stopCluster(cl_to_stop)
                        },
                        error = function(e) {
                            warning("Failed to stop global parallel cluster: ", e$message)
                        }
                    )
                    rm(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv)
                }
            },
            add = TRUE
        )
    }

    # Determine if we need to recalculate
    logger("DEBUG", "Checking if recalculation is needed...")
    logger("DEBUG", "force_recalc:", force_recalc)
    logger("DEBUG", "file.exists(filePath):", file.exists(filePath))
    
    should_recalc <- force_recalc || 
                     !file.exists(filePath) || 
                     (file.exists(filePath) && !is.null(combinations_df) && nrow(readRDS(filePath)) == 0)
    
    logger("DEBUG", "should_recalc:", should_recalc)
    
    if (should_recalc) {
        # Recalculate data
        if (force_recalc) {
            logger("INFO", "Force recalc requested")
        } else if (!file.exists(filePath)) {
            logger("INFO", "No cache file found")
        } else {
            logger("INFO", "Cache file has 0 rows, recalculating")
        }
        
        logger("DEBUG", "About to call calculate_data")
        logger("DEBUG", "calculate_function type:", typeof(calculate_function))
        logger("DEBUG", "calculate_function class:", class(calculate_function))
        
        tryCatch({
            data <- calculate_data(calculate_function, parallel, combinations_df, extra_global_vars, logger)
            logger("DEBUG", "calculate_data returned successfully with", nrow(data), "rows")
        }, error = function(e) {
            logger("ERROR", "Error in calculate_data:", e$message)
            logger("ERROR", "calculate_function type:", typeof(calculate_function))
            stop(e)
        })
        
        logger("INFO", "Saving data to cache file:", filePath)
        saveRDS(data, filePath)
        logger("INFO", "Cache file saved successfully")
    } else {
        # Use existing cache
        logger("INFO", "Loading existing cache file:", filePath)
        data <- readRDS(filePath)
        logger("DEBUG", "Loaded", nrow(data), "rows from cache file")
        
        # Check for missing combinations if needed
        if (!is.null(combinations_df) && nrow(data) > 0) {
            logger("DEBUG", "Checking for missing combinations...")
            data <- handle_missing_combinations(data, combinations_df, calculate_function, 
                                               threshold_parallel, extra_global_vars, filePath)
            logger("DEBUG", "After handling missing combinations:", nrow(data), "rows")
        }
    }
    
    logger("INFO", "=== load_or_calculate completed ===")
    logger("INFO", "Final result:", nrow(data), "rows")
    return(data)
}

#' Helper function to calculate data using the appropriate processing method
#' @param calculate_function The function to calculate data
#' @param parallel Whether to use parallel processing
#' @param combinations_df Data frame of combinations to process
#' @param extra_global_vars Extra global variables for parallel processing
#' @param logger Logger function for debugging
#' @return Calculated data
calculate_data <- function(calculate_function, parallel, combinations_df, extra_global_vars, logger = NULL) {
    # Create logger for this function if not provided
    if (is.null(logger)) {
        logger <- create_module_logger("CALC-DATA")
    }
    
    logger("DEBUG", "=== calculate_data called ===")
    logger("DEBUG", "calculate_function type:", typeof(calculate_function))
    logger("DEBUG", "calculate_function class:", class(calculate_function))
    logger("DEBUG", "parallel:", parallel)
    logger("DEBUG", "combinations_df:", if(is.null(combinations_df)) "NULL" else paste("rows:", nrow(combinations_df)))
    
    # Choose the appropriate loop function based on parallel setting
    if (parallel) {
        logger("INFO", "Using parallel processing")
        
        # Obtain or create a reusable cluster
        if (!exists(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv) ||
            is.null(get(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv))) {
            logger("DEBUG", "Creating new global parallel cluster")
            assign(".GLOBAL_PARALLEL_CLUSTER", create_parallel_cluster(), envir = .GlobalEnv)
        } else {
            logger("DEBUG", "Using existing global parallel cluster")
        }

        cl <- get(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv)
        main_func_name <- deparse(substitute(calculate_function))
        log_file_name <- sprintf("parallel_log_%s.txt", main_func_name)
        logger("DEBUG", "Parallel log file:", log_file_name)

        loop_function <- function(calc_func, ...) {
            logger("DEBUG", "=== parallel loop_function called ===")
            result <- get_data_from_loop_parallel(
                calc_func, ..., cl = cl,
                log_to_file = ENABLE_FILE_LOGGING, log_file = log_file_name,
                extra_global_vars = extra_global_vars, combinations_df = combinations_df
            )
            logger("DEBUG", "Parallel processing returned", nrow(result), "rows")
            return(result)
        }
    } else {
        logger("INFO", "Using sequential processing")
        loop_function <- function(calc_func, ...) {
            logger("DEBUG", "=== sequential loop_function called ===")
            result <- get_data_from_loop(calc_func, combinations_df = combinations_df, ...)
            logger("DEBUG", "Sequential processing returned", nrow(result), "rows")
            return(result)
        }
    }

    # Call the calculation function with the loop function
    logger("INFO", "Calling calculate_function with loop_function")
    logger("DEBUG", "loop_function type:", typeof(loop_function))
    logger("DEBUG", "loop_function class:", class(loop_function))
    logger("DEBUG", "calculate_function type:", typeof(calculate_function))
    logger("DEBUG", "calculate_function class:", class(calculate_function))
    
    tryCatch({
        data <- calculate_function(loop_function)
        logger("INFO", "calculate_function returned", nrow(data), "rows")
        return(data)
    }, error = function(e) {
        logger("ERROR", "Error calling calculate_function:", e$message)
        logger("ERROR", "loop_function type:", typeof(loop_function))
        logger("ERROR", "calculate_function type:", typeof(calculate_function))
        stop(e)
    })
}

#' Load or calculate data using loop system with automatic loop function handling
#' This is a simplified version that handles the loop logic internally
#'
#' @param filePath Path to the cached file (usually .rds)
#' @param data_loader Function that loads data for a single participant-trial combination
#' @param datasets_to_verify Vector of dataset names to verify before loading
#' @param combinations_df Optional data frame with participant and trial columns to process only specific combinations (default: NULL for all combinations)
#' @param use_parallel Whether to use parallel processing (default: USE_PARALLEL global setting)
#' @param force_recalc Whether to force recalculation even if cache exists (default: FORCE_RECALC global setting)
#' @param stop_cluster Whether to stop the parallel cluster after this operation (default: FALSE)
#' @param extra_global_vars Additional global variables to pass to parallel workers
#' @param allow_add_missing Whether to check for and load missing combinations when loading from cache (default: FALSE, but automatically enabled when combinations_df is provided)
#' @param threshold_parallel Threshold for using parallel processing when loading missing combinations (default: uses global THRESHOLD_PARALLEL)
#' @return The loaded or calculated data
load_or_calc_from_loop <- function(filePath, 
                                   data_loader, 
                                   datasets_to_verify = NULL,
                                   combinations_df = NULL,
                                   use_parallel = USE_PARALLEL,
                                   force_recalc = FORCE_RECALC,
                                   stop_cluster = FALSE,
                                   extra_global_vars = NULL,
                                   allow_add_missing = FALSE,
                                   threshold_parallel = NULL) {
  
  # Create logger for this function
  loop_logger <- create_module_logger("LOAD-OR-CALC-FROM-LOOP")
  
  loop_logger("DEBUG", "=== load_or_calc_from_loop called ===")
  loop_logger("DEBUG", "filePath:", filePath)
  loop_logger("DEBUG", "data_loader type:", typeof(data_loader))
  loop_logger("DEBUG", "data_loader class:", class(data_loader))
  loop_logger("DEBUG", "data_loader name:", if(is.function(data_loader)) "function" else "not a function")
  loop_logger("DEBUG", "datasets_to_verify:", paste(datasets_to_verify, collapse = ", "))
  loop_logger("DEBUG", "combinations_df rows:", if(is.null(combinations_df)) "NULL" else nrow(combinations_df))
  loop_logger("DEBUG", "use_parallel:", use_parallel)
  loop_logger("DEBUG", "force_recalc:", force_recalc)
  
  # Create a simple calculate function that uses the provided data_loader
  calculate_function <- function(loop_function) {
    loop_logger("DEBUG", "=== calculate_function called ===")
    loop_logger("DEBUG", "loop_function type:", typeof(loop_function))
    loop_logger("DEBUG", "loop_function class:", class(loop_function))
    loop_logger("DEBUG", "About to call loop_function with data_loader and datasets_to_verify")
    loop_logger("DEBUG", "data_loader in calculate_function:", if(is.function(data_loader)) "function" else "NOT a function")
    loop_logger("DEBUG", "datasets_to_verify:", paste(datasets_to_verify, collapse = ", "))
    
    tryCatch({
      result <- loop_function(data_loader, datasets_to_verify = datasets_to_verify)
      loop_logger("DEBUG", "loop_function returned successfully with", nrow(result), "rows")
      return(result)
    }, error = function(e) {
      loop_logger("ERROR", "Error in calculate_function:", e$message)
      loop_logger("ERROR", "data_loader:", if(is.function(data_loader)) "function" else "NOT a function")
      loop_logger("ERROR", "datasets_to_verify:", paste(datasets_to_verify, collapse = ", "))
      stop(e)
    })
  }
  
  loop_logger("DEBUG", "About to call load_or_calculate")
  loop_logger("DEBUG", "calculate_function type:", typeof(calculate_function))
  loop_logger("DEBUG", "calculate_function class:", class(calculate_function))
  
  # Use the existing load_or_calculate function with our simple calculate_function
  tryCatch({
    result <- load_or_calculate(
      filePath = filePath,
      calculate_function = calculate_function,
      parallel = use_parallel,
      force_recalc = force_recalc,
      stop_cluster = stop_cluster,
      extra_global_vars = extra_global_vars,
      combinations_df = combinations_df,
      allow_add_missing = allow_add_missing,
      threshold_parallel = threshold_parallel
    )
    loop_logger("DEBUG", "load_or_calculate returned successfully with", nrow(result), "rows")
    return(result)
  }, error = function(e) {
    loop_logger("ERROR", "Error in load_or_calculate:", e$message)
    loop_logger("ERROR", "calculate_function type:", typeof(calculate_function))
    loop_logger("ERROR", "calculate_function class:", class(calculate_function))
    loop_logger("ERROR", "data_loader:", if(is.function(data_loader)) "function" else "NOT a function")
    loop_logger("ERROR", "datasets_to_verify:", paste(datasets_to_verify, collapse = ", "))
    stop(e)
  })
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

#' Create and configure a parallel cluster optimized for efficiency
#' This function creates a cluster with efficient data handling and
#' minimal data copying to reduce overhead.
#'
#' @param numCores Number of cores to use. If NULL, will auto-detect (detectCores() - 1)
#' @param maxJobs Maximum number of jobs that will be processed. Cores will not exceed this number.
#' @return A configured cluster object so it can be reused across multiple calls.
create_parallel_cluster <- function(numCores = NULL, maxJobs = NULL) {
    cluster_logger <- create_module_logger("CLUSTER")
    
    # Use standard core detection if not specified
    available_cores <- detectCores()
    if (is.null(numCores)) {
        numCores <- max(1, available_cores - 1) # Leave 1 core for system
        cluster_logger("INFO", sprintf("Auto-detected %d cores (using %d of %d available)", numCores, numCores, available_cores))
    } else {
        cluster_logger("INFO", sprintf("Using %d cores (requested: %d, available: %d)", numCores, numCores, available_cores))
    }

    # Ensure we have at least 1 core
    numCores <- max(1, numCores)

    # Don't use more cores than jobs available
    if (!is.null(maxJobs) && maxJobs > 0) {
        if (numCores > maxJobs) {
            cluster_logger("INFO", sprintf("Optimizing: reducing cores from %d to %d to match number of jobs", numCores, maxJobs))
            numCores <- maxJobs
        } else {
            cluster_logger("INFO", sprintf("Core allocation: using %d cores for %d jobs", numCores, maxJobs))
        }
    }

    cluster_logger("INFO", sprintf("Creating cluster with %d worker processes...", numCores))
    cl <- makeCluster(numCores)
    registerDoParallel(cl)

    cluster_logger("INFO", "Initializing worker processes (loading packages and data)...")

    # Load packages and source files once on each worker
    clusterEvalQ(cl, {
        # Set a flag to prevent recursive initialization during sourcing
        .WORKER_INITIALIZING <<- TRUE

        tryCatch(
            {
                # Source files in dependency order
                source("source/setup.R", local = FALSE)
                source("source/initialization.R", local = FALSE) # Load initialization first
                source("source/data_loading.R", local = FALSE)
                source("source/complexity.R", local = FALSE)
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

    cluster_logger("INFO", "Worker initialization completed.")

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
                                        cl = NULL, log_to_file = TRUE, log_file = NULL, extra_global_vars = NULL, combinations_df = NULL, ...) {
    parallel_logger <- create_module_logger("PARALLEL")
    start_time <- Sys.time()
    
    # Get valid combinations using the helper function
    valid_combinations <- get_valid_combinations_for_processing(combinations_df, datasets_to_verify, parallel_logger)

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
        parallel_logger("INFO", text) # Use the new logging system
        if (log_to_file) {
            # Clean up text and add to memory collection
            clean_text <- gsub("\r", "", gsub("\n$", "", text))
            log_messages <<- c(log_messages, clean_text)
        }
    }

    # Log parallel processing setup
    parallel_logger("INFO", "=== Starting Parallel Processing ===")
    if (log_to_file) {
        parallel_logger("INFO", sprintf("Logging enabled: %s", log_file_path))
    }
    parallel_logger("INFO", sprintf("Function: %s", deparse(substitute(get_data_function))))
    parallel_logger("INFO", sprintf("Total jobs: %d", nrow(valid_combinations)))

    # Use existing cluster or create our own
    own_cluster <- FALSE
    if (is.null(cl)) {
        cl <- create_parallel_cluster(maxJobs = nrow(valid_combinations))
        own_cluster <- TRUE
        parallel_logger("INFO", sprintf("Created new cluster with %d cores", length(cl)))
    } else {
        registerDoParallel(cl)
        parallel_logger("INFO", sprintf("Using existing cluster with %d cores", length(cl)))
    }

    # Export essential variables and global variables needed by worker functions
    essential_vars <- c("get_data_function")

    # Add global variables that might be needed by get_simulation_data and risk_model functions
    global_vars <- c("risk_model_path", "dataExtraFolder", "risk_model_file")

    # Add any extra global variables passed in
    if (!is.null(extra_global_vars)) {
        global_vars <- c(global_vars, extra_global_vars)
    }

    # Check which variables actually exist before exporting
    available_vars <- ls(envir = environment())
    available_globals <- ls(envir = .GlobalEnv)

    vars_to_export <- intersect(essential_vars, available_vars)
    globals_to_export <- intersect(global_vars, available_globals)

    if (length(vars_to_export) > 0) {
        clusterExport(cl, vars_to_export, envir = environment())
    }

    if (length(globals_to_export) > 0) {
        clusterExport(cl, globals_to_export, envir = .GlobalEnv)
    }

    # Start parallel processing
    parallel_logger("INFO", "Starting parallel execution...")
    processing_start <- Sys.time()

    # Use list building instead of .combine = rbind for much better performance
    data_list <- foreach(
        i = 1:nrow(valid_combinations),
        .packages = c("data.table", "dplyr", "pracma", "signal", "zoo", "mgcv"),
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
    parallel_logger("INFO", sprintf("Parallel execution completed in %.2f seconds", processing_duration))

    # Stop the cluster if we created it inside this function
    if (own_cluster) {
        parallel_logger("INFO", "Shutting down cluster...")
        tryCatch(
            {
                stopCluster(cl)
                parallel_logger("INFO", "Cluster shutdown completed.")
            },
            error = function(e) {
                parallel_logger("ERROR", sprintf("Error during cluster shutdown: %s", e$message))
            }
        )
    }

    # Process results and separate successful from failed jobs
    successful_results <- sapply(data_list, function(x) is.list(x) && !is.null(x$success) && x$success)
    error_results <- sapply(data_list, function(x) is.list(x) && !is.null(x$error) && x$error)

    # Collect captured output from all workers in memory
    if (log_to_file && any(successful_results)) {
        parallel_logger("INFO", "=== Detailed Worker Output ===")
        successful_data <- data_list[successful_results]
        for (i in 1:length(successful_data)) {
            result <- successful_data[[i]]
            if (!is.null(result$captured_output) && length(result$captured_output) > 0) {
                # Add worker output header to log messages
                log_messages <- c(
                    log_messages,
                    sprintf(
                        "--- P%s-T%s Output (%.2fs, %d rows) ---",
                        result$participant, result$trial, result$duration, result$rows
                    )
                )
                # Add all captured output lines
                log_messages <- c(log_messages, result$captured_output)
            }
        }
        parallel_logger("INFO", "=== End Detailed Worker Output ===")
    }

    # Log detailed results summary
    parallel_logger("INFO", "=== Processing Results Summary ===")
    parallel_logger("INFO", sprintf("Total jobs: %d", length(data_list)))
    parallel_logger("INFO", sprintf("Successful: %d", sum(successful_results)))
    parallel_logger("INFO", sprintf("Failed: %d", sum(error_results)))

    # Report on successful jobs
    if (any(successful_results)) {
        successful_data <- data_list[successful_results]
        total_rows <- sum(sapply(successful_data, function(x) x$rows))
        avg_duration <- mean(sapply(successful_data, function(x) x$duration))
        parallel_logger("INFO", sprintf("Total rows processed: %d", total_rows))
        parallel_logger("INFO", sprintf("Average job duration: %.2f seconds", avg_duration))

        # Show details for each successful job
        parallel_logger("INFO", "Successful jobs:")
        for (i in 1:length(successful_data)) {
            result <- successful_data[[i]]
            parallel_logger("INFO", sprintf(
                "  ✓ P%s-T%s: %d rows (%.2fs)",
                result$participant, result$trial, result$rows, result$duration
            ))
        }
    }

    # Report errors if any occurred
    if (any(error_results)) {
        error_data <- data_list[error_results]
        parallel_logger("ERROR", sprintf("Errors occurred in %d jobs:", length(error_data)))
        for (i in 1:length(error_data)) {
            error_info <- error_data[[i]]
            parallel_logger("ERROR", sprintf(
                "  ✗ P%s-T%s: %s",
                error_info$participant, error_info$trial, error_info$message
            ))
            if (!is.null(error_info$call)) {
                parallel_logger("ERROR", sprintf("    Call: %s", error_info$call))
            }
        }
    }

    if (!any(successful_results)) {
        warning("No data was successfully processed from the valid combinations.")
        return(data.frame())
    }

    # Extract just the data frames from successful results
    parallel_logger("INFO", "Extracting data from successful results...")
    successful_data_frames <- lapply(data_list[successful_results], function(x) x$data)

    # Handle factor level mismatches before combining
    parallel_logger("INFO", "Preparing data for combination (handling factor levels)...")
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
    parallel_logger("INFO", sprintf("Combining %d data frames...", length(successful_data_frames)))
    data <- tryCatch(
        {
            data.table::rbindlist(successful_data_frames, fill = TRUE)
        },
        error = function(e) {
            parallel_logger("WARN", "Error with rbindlist, falling back to do.call(rbind, ...)...")
            warning("rbindlist failed, using rbind fallback: ", e$message)
            do.call(rbind, successful_data_frames)
        }
    )
    parallel_logger("INFO", "Data combination completed, converting to data.frame...")
    data <- as.data.frame(data) # Convert back to data.frame for compatibility

    # Clean up memory
    parallel_logger("INFO", "Cleaning up memory...")
    rm(data_list, successful_data_frames)
    suppressWarnings(gc()) # Suppress connection closing warnings during cleanup
    parallel_logger("INFO", "Memory cleanup completed.")

    # Final summary
    end_time <- Sys.time()
    total_duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

    parallel_logger("INFO", "=== Final Summary ===")
    parallel_logger("INFO", sprintf("Total processing time: %.2f seconds", total_duration))
    parallel_logger("INFO", sprintf("Final dataset: %d rows, %d columns", nrow(data), ncol(data)))

    if (nrow(data) == 0) {
        parallel_logger("WARN", "No data was successfully processed from the valid combinations.")
    } else {
        parallel_logger("INFO", "✓ Parallel processing completed successfully!")
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
                parallel_logger("INFO", sprintf(
                    "Log written to: %s (%d lines)",
                    log_file_path, length(log_messages)
                ))
            },
            error = function(e) {
                warning("Error writing log file: ", e$message)
            }
        )
    }

    return(data)
}

get_data_from_loop <- function(get_data_function, datasets_to_verify = c("leftfoot", "rightfoot", "hip"), combinations_df = NULL, ...) {
    loop_logger <- create_module_logger("LOOP")
    
    # Get valid combinations using the helper function
    valid_combinations <- get_valid_combinations_for_processing(combinations_df, datasets_to_verify, loop_logger)

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
        loop_logger("INFO", sprintf("Progress: %s %.2f%% On Participant: %s, Trial: %s", progress_bar, progress_percent, participant, trial))

        # Calculate gait data and parameters
        newData <- get_data_function(participant, trial, ...)
        loop_logger("DEBUG", "get_data_function returned", nrow(newData), "rows")
        
        newData <- add_identifiers_and_categories(as.data.frame(newData), participant, trial)
        loop_logger("DEBUG", "add_identifiers_and_categories completed, final data has", nrow(newData), "rows")

        # Store in list instead of repeated rbind for efficiency
        data_list[[i]] <- newData
        loop_logger("DEBUG", "Stored data in list, moving to next iteration")
    }

    # Combine all data frames efficiently using rbindlist
    data <- data.table::rbindlist(data_list, fill = TRUE)
    data <- as.data.frame(data) # Convert back to data.frame for compatibility

    if (nrow(data) == 0) {
        warning("No data was successfully processed from the valid combinations.")
    }

    return(data)
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
        is.null(get(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv))) {
      assign(".GLOBAL_PARALLEL_CLUSTER", create_parallel_cluster(), envir = .GlobalEnv)
    }
    cl <- get(".GLOBAL_PARALLEL_CLUSTER", envir = .GlobalEnv)
    
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