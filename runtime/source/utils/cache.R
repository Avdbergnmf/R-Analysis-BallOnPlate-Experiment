#' Cache Utilities for Data Loading
#'
#' This file provides utilities for caching data using qs::qsave/qread
#' with configuration management from config.yml
#'
#' @author Alex van den Berg
#' @version 1.0

# Create module-specific logger for cache operations
cache_logger <- create_module_logger("CACHE")

# =============================================================================
# CACHE FILE FORMAT HELPERS
# =============================================================================

#' Resolve the cache format for a given file path
#' Falls back to global CACHE_FORMAT when extension missing.
resolve_cache_format <- function(file_path = NULL, format = NULL) {
    if (!is.null(format) && nzchar(format)) {
        fmt <- tolower(format)
    } else if (!is.null(file_path)) {
        ext <- tolower(tools::file_ext(file_path))
        if (nzchar(ext) && ext %in% c("qs", "rds")) {
            fmt <- ext
        } else if (exists("CACHE_FORMAT", envir = .GlobalEnv)) {
            fmt <- tolower(get("CACHE_FORMAT", envir = .GlobalEnv))
        } else {
            fmt <- "rds"
        }
    } else if (exists("CACHE_FORMAT", envir = .GlobalEnv)) {
        fmt <- tolower(get("CACHE_FORMAT", envir = .GlobalEnv))
    } else {
        fmt <- "rds"
    }

    if (!fmt %in% c("qs", "rds")) {
        fmt <- "rds"
    }
    fmt
}

#' Determine the standard file extension for a cache format
cache_extension_for_format <- function(format = NULL) {
    fmt <- resolve_cache_format(format = format)
    if (fmt == "qs") {
        return("qs")
    }
    "rds"
}

#' Read a cache file supporting both RDS and QS formats
read_cache_file <- function(file_path, format = NULL) {
    fmt <- resolve_cache_format(file_path, format)

    if (!file.exists(file_path)) {
        stop(sprintf("Cache file does not exist: %s", file_path))
    }

    if (fmt == "qs") {
        return(qs::qread(file_path))
    }

    readRDS(file_path)
}

#' Write a cache file supporting both RDS and QS formats
write_cache_file <- function(data,
                             file_path,
                             format = NULL,
                             compression_level = NULL,
                             rds_compress = "xz") {
    fmt <- resolve_cache_format(file_path, format)

    # Ensure target directory exists
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }

    if (fmt == "qs") {
        if (is.null(compression_level)) {
            compression_level <- if (exists("CACHE_COMPRESSION_LEVEL", envir = .GlobalEnv)) {
                get("CACHE_COMPRESSION_LEVEL", envir = .GlobalEnv)
            } else {
                6
            }
        }
        qs::qsave(data, file_path, preset = "high", compress_level = compression_level)
        return(invisible(file_path))
    }

    # RDS fallback
    saveRDS(data, file_path, compress = rds_compress)
    invisible(file_path)
}

# =============================================================================
# CONFIGURATION ACCESS
# =============================================================================

# Configuration is loaded in global.R and available via cfg_get()
# This section provides fallback defaults if global.R hasn't been loaded

# =============================================================================
# CACHING FUNCTIONS
# =============================================================================

#' Load data with caching support
#' @param cache_key Unique key for this cached data
#' @param load_function Function to load data if not cached
#' @param cache_dir Directory to store cache files
#' @param force_rewrite Whether to force cache rewrite (uses global config if NULL)
#' @param compression_level Compression level for qs files (uses global config if NULL)
#' @param ... Additional arguments passed to load_function
#' @return Loaded data
load_with_cache <- function(cache_key, load_function, cache_dir = "cache",
                            force_rewrite = NULL, compression_level = NULL, ...) {
    # Use global configuration if not specified
    if (is.null(force_rewrite)) {
        force_rewrite <- if (exists("CACHE_FORCE_REWRITE")) CACHE_FORCE_REWRITE else FALSE
    }
    if (is.null(compression_level)) {
        compression_level <- if (exists("CACHE_COMPRESSION_LEVEL")) CACHE_COMPRESSION_LEVEL else 6
    }

    # Ensure cache directory exists
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }

    # Create cache file path
    cache_file <- file.path(cache_dir, paste0(cache_key, ".qs"))

    # Check if cache exists and we're not forcing rewrite
    if (file.exists(cache_file) && !force_rewrite) {
        tryCatch(
            {
                cache_logger("DEBUG", sprintf("Loading %s from cache...", cache_key))
                start_time <- Sys.time()
                data <- qs::qread(cache_file)
                load_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
                cache_logger("DEBUG", sprintf("Loaded %s in %.2f seconds", cache_key, load_time))
                return(data)
            },
            error = function(e) {
                cache_logger("WARN", sprintf("Error loading cache file %s: %s", cache_file, e$message))
                cache_logger("DEBUG", "Falling back to fresh load...")
            }
        )
    }

    # Load data fresh
    cache_logger("DEBUG", sprintf("Loading %s fresh...", cache_key))
    start_time <- Sys.time()
    data <- load_function(...)
    load_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    cache_logger("DEBUG", sprintf("Loaded %s fresh in %.2f seconds", cache_key, load_time))

    # Save to cache
    if (!is.null(data)) {
        tryCatch(
            {
                cache_logger("DEBUG", sprintf("Saving %s to cache...", cache_key))
                save_start <- Sys.time()
                qs::qsave(data, cache_file, preset = "high", compress_level = compression_level)
                save_time <- as.numeric(difftime(Sys.time(), save_start, units = "secs"))
                cache_logger("DEBUG", sprintf("Saved %s to cache in %.2f seconds", cache_key, save_time))
            },
            error = function(e) {
                cache_logger("WARN", sprintf("Error saving cache file %s: %s", cache_file, e$message))
            }
        )
    }

    return(data)
}

#' Load CSV file with caching
#' @param file_path Path to CSV file
#' @param cache_key Unique key for caching
#' @param cache_dir Directory to store cache files
#' @param force_rewrite Whether to force cache rewrite (uses global config if NULL)
#' @param compression_level Compression level for qs files (uses global config if NULL)
#' @param ... Additional arguments passed to data.table::fread
#' @return Loaded data
load_csv_with_cache <- function(file_path, cache_key, cache_dir = "cache",
                                force_rewrite = NULL, compression_level = NULL, ...) {
    load_function <- function() {
        if (!file.exists(file_path)) {
            warning("File not found: ", file_path)
            return(NULL)
        }
        return(data.table::fread(file_path, ...))
    }

    return(load_with_cache(cache_key, load_function, cache_dir, force_rewrite, compression_level))
}

#' Load RDS file with caching
#' @param file_path Path to RDS file
#' @param cache_key Unique key for caching
#' @param cache_dir Directory to store cache files
#' @param force_rewrite Whether to force cache rewrite (uses global config if NULL)
#' @param compression_level Compression level for qs files (uses global config if NULL)
#' @return Loaded data
load_rds_with_cache <- function(file_path, cache_key, cache_dir = "cache",
                                force_rewrite = NULL, compression_level = NULL) {
    load_function <- function() {
        if (!file.exists(file_path)) {
            warning("File not found: ", file_path)
            return(NULL)
        }
        return(readRDS(file_path))
    }

    return(load_with_cache(cache_key, load_function, cache_dir, force_rewrite, compression_level))
}

#' Load directory listing with caching
#' @param dir_path Path to directory
#' @param cache_key Unique key for caching
#' @param cache_dir Directory to store cache files
#' @param force_rewrite Whether to force cache rewrite (uses global config if NULL)
#' @param compression_level Compression level for qs files (uses global config if NULL)
#' @param full_names Whether to return full names
#' @param recursive Whether to list recursively
#' @return Directory listing
load_dir_with_cache <- function(dir_path, cache_key, cache_dir = "cache",
                                force_rewrite = NULL, compression_level = NULL,
                                full_names = FALSE, recursive = FALSE) {
    load_function <- function() {
        if (!dir.exists(dir_path)) {
            warning("Directory not found: ", dir_path)
            return(character(0))
        }
        return(list.dirs(path = dir_path, full.names = full_names, recursive = recursive))
    }

    return(load_with_cache(cache_key, load_function, cache_dir, force_rewrite, compression_level))
}

# =============================================================================
# CACHE MANAGEMENT
# =============================================================================

#' Clear all cache files
#' @param cache_dir Directory containing cache files
#' @param pattern Pattern to match cache files (default: "*.qs")
clear_cache <- function(cache_dir = "cache", pattern = "*.qs") {
    if (!dir.exists(cache_dir)) {
        cache_logger("WARN", sprintf("Cache directory does not exist: %s", cache_dir))
        return(invisible(NULL))
    }

    cache_files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
    if (length(cache_files) == 0) {
        cache_logger("INFO", "No cache files found to clear")
        return(invisible(NULL))
    }

    cache_logger("INFO", sprintf("Clearing %d cache files...", length(cache_files)))
    file.remove(cache_files)
    cache_logger("INFO", "Cache cleared successfully")
}

#' Get cache statistics
#' @param cache_dir Directory containing cache files
#' @return Data frame with cache statistics
get_cache_stats <- function(cache_dir = "cache") {
    if (!dir.exists(cache_dir)) {
        return(data.frame())
    }

    cache_files <- list.files(cache_dir, pattern = "*.qs", full.names = TRUE)
    if (length(cache_files) == 0) {
        return(data.frame())
    }

    file_info <- file.info(cache_files)
    file_info$file <- basename(cache_files)
    file_info$size_mb <- round(file_info$size / 1024^2, 2)

    return(file_info[, c("file", "size_mb", "mtime")])
}

#' Print cache statistics
#' @param cache_dir Directory containing cache files
print_cache_stats <- function(cache_dir = "cache") {
    stats <- get_cache_stats(cache_dir)
    if (nrow(stats) == 0) {
        cache_logger("INFO", "No cache files found")
        return(invisible(NULL))
    }

    total_size <- sum(stats$size_mb)
    cache_logger("INFO", sprintf("Cache Statistics (%s):", cache_dir))
    cache_logger("INFO", sprintf("  Files: %d", nrow(stats)))
    cache_logger("INFO", sprintf("  Total size: %.2f MB", total_size))
    cache_logger("INFO", "  Files:")
    for (i in 1:nrow(stats)) {
        cache_logger("INFO", sprintf(
            "    %s: %.2f MB (%s)",
            stats$file[i], stats$size_mb[i],
            format(stats$mtime[i], "%Y-%m-%d %H:%M:%S")
        ))
    }
}
