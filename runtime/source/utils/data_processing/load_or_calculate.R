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
    
    # Merge any extra globals declared on the calculate function
    extra_globals_attr <- attr(calculate_function, "extra_globals")
    if (!is.null(extra_globals_attr)) {
        logger("DEBUG", "Merging extra global variables:", paste(extra_globals_attr, collapse = ", "))
        if (is.null(extra_global_vars)) {
            extra_global_vars <- extra_globals_attr
        } else {
            extra_global_vars <- unique(c(extra_global_vars, extra_globals_attr))
        }
    }
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
                            cl_to_stop <- .GlobalEnv$.GLOBAL_PARALLEL_CLUSTER
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
        
        # Run preload hook if provided
        preload_fn <- attr(calculate_function, "preload")
        if (is.function(preload_fn)) {
            logger("DEBUG", "Running preload hook for calculate_function")
            tryCatch(
                preload_fn(),
                error = function(e) logger("WARN", "Preload hook failed:", e$message)
            )
        }
        
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
