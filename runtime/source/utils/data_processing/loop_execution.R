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
            is.null(.GlobalEnv$.GLOBAL_PARALLEL_CLUSTER)) {
            logger("DEBUG", "Creating new global parallel cluster")
            assign(".GLOBAL_PARALLEL_CLUSTER", create_parallel_cluster(), envir = .GlobalEnv)
        } else {
            logger("DEBUG", "Using existing global parallel cluster")
        }

        cl <- .GlobalEnv$.GLOBAL_PARALLEL_CLUSTER
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
