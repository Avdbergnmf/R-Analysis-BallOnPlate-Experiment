#!/usr/bin/env Rscript

#' Clear Corrupted Cache Files
#'
#' This script clears corrupted cache files that may cause issues.
#' Run this if you encounter "error reading from connection" or similar cache errors.

# Source the necessary modules
source("source/logging_utils.R")
source("source/data_caching_setup.R")
source("source/data_caching_system.R")

# Initialize global data
ensure_global_data_initialized()

cat("=== Clearing Corrupted Cache Files ===\n")

# Clear corrupted caches
clear_corrupted_caches()

cat("=== Cache cleanup completed ===\n")

# Show cache statistics
cat("\n=== Current Cache Statistics ===\n")
stats <- get_cache_stats()
if (length(stats) > 0) {
    for (data_type in names(stats)) {
        cat(sprintf(
            "%s: %d rows in memory, RDS file: %s (%.2f MB)\n",
            data_type,
            stats[[data_type]]$in_memory_rows,
            if (stats[[data_type]]$rds_file_exists) "exists" else "missing",
            stats[[data_type]]$rds_file_size / (1024 * 1024)
        ))
    }
} else {
    cat("No caches currently loaded\n")
}
