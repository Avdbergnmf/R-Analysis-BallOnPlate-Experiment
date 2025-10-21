#' Data Processing Utilities - Main Entry Point
#'
#' This is the main entry point for all data processing utilities.
#' It sources all the individual utility files and provides a unified interface.
#'
#' @section Files loaded:
#' - load_or_calculate.R: Core caching and loading logic
#' - parallel_processing.R: Parallel cluster management and execution
#' - data_validation.R: Data verification and validation utilities
#' - loop_execution.R: Sequential and parallel loop execution
#'
#' @section Usage:
#' This file is automatically sourced by global.R and makes all data processing
#' functions available globally. No manual loading is required.

# Source all the split utility files
source("source/utils/data_processing/load_or_calculate.R", local = FALSE)
source("source/utils/data_processing/parallel_processing.R", local = FALSE)
source("source/utils/data_processing/data_validation.R", local = FALSE)
source("source/utils/data_processing/loop_execution.R", local = FALSE)

# Utility function for getting data types (legacy compatibility)
getTypes <- function(dt) {
    numericDataTypes <- sapply(dt, is.numeric)
    logicalDataTypes <- sapply(dt, is.logical)
    dataTypes <- names(numericDataTypes[numericDataTypes | logicalDataTypes])
}