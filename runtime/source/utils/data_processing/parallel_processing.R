#' Safely derive the number of CPU cores available without relying on
#' external system commands that might be restricted in sandboxed environments.
#' Tries configuration, then environment hints, and finally (optionally) the
#' standard parallel::detectCores().
#'
#' @param default Fallback core count when no information is available.
#' @param allow_autodetect Whether to attempt parallel::detectCores().
#'                         This can also be enabled via the ALLOW_PARALLEL_DETECT
#'                         environment variable (set to "1", "true", or "yes")
#'                         or the `allow.parallel.detect` option.
#' @return Integer core count (>= 1).
get_configured_core_count <- function(default = 1L, allow_autodetect = FALSE) {
    default <- max(1L, as.integer(default))

    # Helper to coerce environment variable to integer
    read_env_int <- function(var) {
        val <- suppressWarnings(as.integer(Sys.getenv(var, NA_character_)))
        if (is.na(val) || !is.finite(val) || val <= 0) {
            return(NA_integer_)
        }
        val
    }

    # 1) Explicit configuration takes precedence
    if (base::exists("MAX_CORES", envir = .GlobalEnv, inherits = FALSE)) {
        max_cfg <- base::get("MAX_CORES", envir = .GlobalEnv, inherits = FALSE)
        if (is.numeric(max_cfg) && length(max_cfg) == 1) {
            if (is.na(max_cfg)) {
                max_cfg <- default
            }
            max_cfg <- as.integer(max_cfg)
            if (max_cfg > 0) {
                return(max(1L, max_cfg))
            }
            if (max_cfg == 0) {
                return(1L)
            }
            # max_cfg < 0 means "use all available" – fall through to env hints
        }
    }

    # 2) Environment hints commonly provided by schedulers / OS
    env_vars <- c(
        "R_PARALLEL_NUM_THREADS",
        "MC_CORES",
        "OMP_NUM_THREADS",
        "SLURM_CPUS_ON_NODE",
        "PBS_NUM_PPN",
        "PBS_NP",
        "NSLOTS",
        "NUMBER_OF_PROCESSORS"
    )
    for (var in env_vars) {
        env_val <- read_env_int(var)
        if (!is.na(env_val)) {
            return(max(1L, env_val))
        }
    }

    # 3) Optional auto-detection (disabled by default for stability)
    flag_env <- tolower(Sys.getenv("ALLOW_PARALLEL_DETECT", ""))
    allow_autodetect <- allow_autodetect ||
        identical(flag_env, "1") ||
        identical(flag_env, "true") ||
        identical(flag_env, "yes") ||
        isTRUE(getOption("allow.parallel.detect", FALSE))

    if (allow_autodetect) {
        detected <- tryCatch(
            parallel::detectCores(),
            error = function(e) NA_integer_,
            warning = function(w) NA_integer_
        )
        if (!is.na(detected) && is.finite(detected) && detected > 0) {
            return(max(1L, as.integer(detected)))
        }
    }

    default
}

#' Create and configure a parallel cluster optimized for efficiency
#' This function creates a cluster with efficient data handling and
#' minimal data copying to reduce overhead.
#'
#' @param numCores Number of cores to use. If NULL, relies on MAX_CORES config
#'                 (or environment hints) while leaving at least one core free.
#' @param maxJobs Maximum number of jobs that will be processed. Cores will not exceed this number.
#' @param extra_global_vars Character vector of global object names that must be available on workers.
#'        Used for export planning and memory estimation.
#' @return A configured cluster object so it can be reused across multiple calls.
create_parallel_cluster <- function(numCores = NULL, maxJobs = NULL, extra_global_vars = NULL) {
    cluster_logger <- create_module_logger("CLUSTER")

    # Get max cores from configuration
    max_cores_config <- if (base::exists("MAX_CORES", envir = .GlobalEnv, inherits = FALSE)) base::get("MAX_CORES", envir = .GlobalEnv, inherits = FALSE) else -1

    # Determine whether we should auto-detect cores (config <= 0 or explicit request)
    auto_detect <- FALSE
    if (is.null(numCores)) {
        auto_detect <- max_cores_config <= 0
    } else if (is.numeric(numCores) && length(numCores) == 1 && numCores <= 0) {
        auto_detect <- TRUE
    }

    # Estimate available cores (allow auto-detection only when requested)
    available_cores <- get_configured_core_count(
        default = 1L,
        allow_autodetect = auto_detect
    )

    if (is.null(numCores) || (is.numeric(numCores) && length(numCores) == 1 && numCores <= 0)) {
        # Apply max cores configuration
        if (max_cores_config <= 0) {
            # -1 or missing: use all cores, but leave 1 for system only if we would use all cores
            if (available_cores > 1) {
                numCores <- available_cores - 1
            } else {
                numCores <- 1
            }
        } else if (max_cores_config == 1) {
            # 1: disable parallel processing
            cluster_logger("INFO", "Parallel processing disabled by max_cores=1")
            return(NULL)
        } else {
            # >1: limit to specified number of cores, but only subtract 1 if we would use all available cores
            if (max_cores_config >= available_cores && available_cores > 1) {
                # We would use all cores, so leave 1 for system
                numCores <- available_cores - 1
            } else {
                # We're not using all cores, so use the specified limit
                numCores <- max_cores_config
            }
        }
        cluster_logger(
            "INFO",
            sprintf(
                "Configured %d worker cores (available estimate: %d, max_cores=%d)",
                numCores, available_cores, max_cores_config
            )
        )
    } else {
        # If numCores is explicitly specified, still respect max_cores limit
        if (max_cores_config > 1) {
            numCores <- min(numCores, max_cores_config)
        }
        cluster_logger(
            "INFO",
            sprintf(
                "Using %d cores (requested: %d, available estimate: %d, max_cores=%d)",
                numCores, numCores, available_cores, max_cores_config
            )
        )
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

    # Prepare objects from the global environment for export and estimate memory usage
    # Get all objects from global environment
    global_objects <- ls(envir = .GlobalEnv, all.names = TRUE)

    # Filter out objects we don't want to export (like the cluster itself)
    objects_to_exclude <- c(".GLOBAL_PARALLEL_CLUSTER", "cl", "parallel_logger")
    objects_to_export <- setdiff(global_objects, objects_to_exclude)
    if (!is.null(extra_global_vars)) {
        objects_to_export <- unique(c(objects_to_export, extra_global_vars))
    }

    # Ensure all requested exports exist
    objects_to_export <- objects_to_export[
        vapply(
            objects_to_export,
            function(nm) base::exists(nm, envir = .GlobalEnv, inherits = FALSE),
            logical(1)
        )
    ]

    cluster_logger(
        "INFO",
        sprintf("Preparing %d global objects for worker export", length(objects_to_export))
    )

    # Estimate memory footprint of exported objects to prevent over-allocation
    estimate_object_mb <- function(name) {
        if (!base::exists(name, envir = .GlobalEnv, inherits = FALSE)) {
            return(0)
        }
        obj <- base::get(name, envir = .GlobalEnv, inherits = FALSE)
        size_bytes <- tryCatch(
            as.numeric(object.size(obj)),
            error = function(e) NA_real_
        )
        if (is.na(size_bytes)) {
            return(0)
        }
        size_bytes / (1024^2)
    }

    export_sizes_mb <- numeric(0)
    if (length(objects_to_export) > 0) {
        export_sizes_mb <- vapply(objects_to_export, estimate_object_mb, numeric(1))
        names(export_sizes_mb) <- objects_to_export
    }

    total_export_mb <- sum(export_sizes_mb)
    replication_multiplier <- getOption("parallel.worker.replication.multiplier", 1.35)
    worker_overhead_mb <- getOption("parallel.worker.overhead.mb", 512)
    per_worker_mb <- total_export_mb * replication_multiplier + worker_overhead_mb

    cluster_logger(
        "DEBUG",
        sprintf(
            "Export footprint≈%.1fMB (multiplier=%.2f, overhead≈%.1fMB) -> per-worker estimate≈%.1fMB",
            total_export_mb, replication_multiplier, worker_overhead_mb, per_worker_mb
        )
    )

    if (total_export_mb > 50) {
        top_heavy <- sort(export_sizes_mb, decreasing = TRUE)
        top_heavy <- top_heavy[is.finite(top_heavy) & top_heavy > 1]
        if (length(top_heavy) > 0) {
            preview <- paste(
                head(sprintf("%s=%.1fMB", names(top_heavy), top_heavy), 5),
                collapse = ", "
            )
            cluster_logger("DEBUG", sprintf("Largest exported globals: %s", preview))
        }
    }

    adjust_workers_for_memory <- function(current_cores, per_worker_mb_estimate) {
        if (is.na(per_worker_mb_estimate) || per_worker_mb_estimate <= 0) {
            return(current_cores)
        }

        mem_limit_mb <- tryCatch(
            as.numeric(base::memory.limit()),
            error = function(e) NA_real_
        )
        mem_used_mb <- tryCatch(
            as.numeric(base::memory.size()),
            error = function(e) NA_real_
        )

        if (!is.finite(mem_limit_mb) || mem_limit_mb <= 0 ||
            !is.finite(mem_used_mb) || mem_used_mb <= 0) {
            return(current_cores)
        }

        safety_margin_mb <- getOption("parallel.memory.safety.margin.mb", 1024)
        available_mb <- mem_limit_mb - mem_used_mb - safety_margin_mb
        if (available_mb <= 0) {
            cluster_logger("WARN", "Insufficient available memory detected; limiting to a single worker")
            return(1L)
        }

        max_workers_mem <- floor(available_mb / per_worker_mb_estimate)
        max_workers_mem <- max(1L, max_workers_mem)

        if (max_workers_mem < current_cores) {
            cluster_logger(
                "INFO",
                sprintf(
                    "Downscaling workers from %d to %d based on estimated memory usage (export=%.1fMB, per-worker≈%.1fMB, available≈%.1fMB)",
                    current_cores, max_workers_mem, total_export_mb, per_worker_mb_estimate, available_mb
                )
            )
            return(max_workers_mem)
        }

        current_cores
    }

    numCores <- adjust_workers_for_memory(numCores, per_worker_mb)

    cluster_logger("INFO", sprintf("Creating cluster with %d worker processes...", numCores))
    cl <- makeCluster(numCores)
    registerDoParallel(cl)

    cluster_logger("INFO", "Initializing worker processes (loading packages and data)...")

    cluster_logger("INFO", "Exporting global objects to workers...")

    if (length(objects_to_export) > 0) {
        cluster_logger("INFO", sprintf("Exporting %d objects to workers", length(objects_to_export)))
        # Export all global objects to workers
        clusterExport(cl, objects_to_export, envir = .GlobalEnv)
    } else {
        cluster_logger("INFO", "No global objects queued for export")
    }

    # Export the cluster_logger function to workers
    clusterExport(cl, "cluster_logger", envir = environment())

    # Just load required packages on workers (much lighter than re-sourcing everything)
    clusterEvalQ(cl, {
        # Load only the essential packages
        library(data.table)
        library(dplyr)
        library(pracma)
        library(signal)
        library(zoo)
        library(mgcv)

        # Set a flag to indicate this is a worker process
        .IS_WORKER_PROCESS <<- TRUE

        # Check if cluster_logger is available
        if (exists("cluster_logger")) {
            cluster_logger("DEBUG", "Worker process initialized with global environment")
        } else {
            cat("[WORKER] WARNING: cluster_logger not found in worker environment\n")
            cat("[WORKER] Available objects:", paste(ls(envir = .GlobalEnv), collapse = ", "), "\n")
        }
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
    cluster_logger <- create_module_logger("CLUSTER") # Create cluster_logger for this function
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
        cl <- create_parallel_cluster(
            maxJobs = nrow(valid_combinations),
            extra_global_vars = extra_global_vars
        )
        if (is.null(cl)) {
            # Parallel processing was disabled by configuration
            parallel_logger("INFO", "Parallel processing disabled by configuration, falling back to sequential processing")
            return(get_data_from_loop(get_data_function, datasets_to_verify, combinations_df, ...))
        }
        own_cluster <- TRUE
        parallel_logger("INFO", sprintf("Created new cluster with %d cores", length(cl)))
    } else {
        registerDoParallel(cl)
        parallel_logger("INFO", sprintf("Using existing cluster with %d cores", length(cl)))
    }

    # Export the specific function and any extra variables needed for this job
    # (Note: most variables are already exported during cluster creation)
    vars_to_export <- c("get_data_function")

    # Add any extra global variables passed in
    if (!is.null(extra_global_vars)) {
        vars_to_export <- c(vars_to_export, extra_global_vars)
    }

    # Export only the variables that aren't already in the global environment
    available_vars <- ls(envir = environment())
    vars_to_export <- intersect(vars_to_export, available_vars)

    if (length(vars_to_export) > 0) {
        cluster_logger("DEBUG", sprintf("Exporting additional variables: %s", paste(vars_to_export, collapse = ", ")))
        clusterExport(cl, vars_to_export, envir = environment())
    }

    # Export cluster_logger to workers
    clusterExport(cl, "cluster_logger", envir = environment())

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

        # Also print a summary of all error messages for easier debugging
        parallel_logger("ERROR", "=== ALL ERROR MESSAGES ===")
        for (i in 1:length(error_data)) {
            error_info <- error_data[[i]]
            parallel_logger("ERROR", sprintf("Error %d: %s", i, error_info$message))
        }
        parallel_logger("ERROR", "=== END ERROR MESSAGES ===")
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

                # Strip ANSI color codes from log messages for clean file output
                clean_log_messages <- gsub("\033\\[[0-9;]*m", "", log_messages)

                # Write all messages to file in a single operation
                writeLines(clean_log_messages, log_file_path)
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
