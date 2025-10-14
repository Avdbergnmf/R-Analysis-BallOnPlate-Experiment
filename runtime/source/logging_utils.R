#' Logging Utilities for Data Analysis
#'
#' Centralized logging system for consistent debug output across all modules.
#' Supports different log levels and message collection for parallel processing.
#'
#' @author Data Analysis Team
#' @version 1.0

# =============================================================================
# LOGGING CONFIGURATION
# =============================================================================

# Global logging configuration
GLOBAL_LOGGING_ENABLED <- TRUE
GLOBAL_LOG_LEVEL <- "DEBUG"  # DEBUG, INFO, WARN, ERROR

# Log level priority mapping (higher number = higher priority)
LOG_LEVEL_PRIORITIES <- list(
  "DEBUG" = 1,
  "INFO" = 2,
  "WARN" = 3,
  "ERROR" = 4
)

# =============================================================================
# LOGGING FUNCTIONS
# =============================================================================

#' Create a logging function with different log levels
#' @param enabled Whether logging is enabled (default: TRUE)
#' @param messages Vector to collect messages (for parallel processing)
#' @param module_name Optional module name to prefix log messages
#' @return A logging function that can be called with level and message
create_logger <- function(enabled = TRUE, messages = NULL, module_name = NULL) {
  function(level = "DEBUG", ...) {
    # Check if logging is enabled globally and locally
    if (!GLOBAL_LOGGING_ENABLED || !enabled) {
      return(invisible(NULL))
    }
    
    # Check log level priority using the global mapping
    level_priority <- LOG_LEVEL_PRIORITIES[[level]]
    if (is.null(level_priority)) {
      level_priority <- LOG_LEVEL_PRIORITIES[["DEBUG"]]  # default to DEBUG
    }
    
    global_priority <- LOG_LEVEL_PRIORITIES[[GLOBAL_LOG_LEVEL]]
    if (is.null(global_priority)) {
      global_priority <- LOG_LEVEL_PRIORITIES[["DEBUG"]]  # default to DEBUG
    }
    
    # Only log if current level is >= global level (higher or equal priority)
    if (level_priority < global_priority) {
      return(invisible(NULL))
    }
    
    # Create message prefix
    prefix <- switch(level,
      "DEBUG" = "[DEBUG]",
      "INFO" = "[INFO]",
      "WARN" = "[WARN]",
      "ERROR" = "[ERROR]",
      "[DEBUG]" # default
    )
    
    # Add module name if provided
    if (!is.null(module_name)) {
      prefix <- paste0(prefix, " [", module_name, "]")
    }
    
    # Create timestamp
    timestamp <- format(Sys.time(), "%H:%M:%S")
    
    # Create the full message with timestamp
    msg <- paste(timestamp, prefix, paste(..., collapse = " "))
    
    # Collect messages if messages vector is provided (for parallel processing)
    if (!is.null(messages)) {
      # Use parent environment to modify the messages vector
      parent_env <- parent.frame()
      if (exists("debug_messages", envir = parent_env)) {
        assign("debug_messages", c(get("debug_messages", envir = parent_env), msg), envir = parent_env)
      }
    }
    
    # Always output immediately for capture.output() to catch it
    cat(msg, "\n")
    flush.console()
  }
}

#' Create a module-specific logger
#' @param module_name Name of the module (e.g., "CACHE", "COMPLEXITY", "DATA-LOADING")
#' @param enabled Whether logging is enabled for this module
#' @param messages Vector to collect messages (for parallel processing)
#' @return A logging function with module prefix
create_module_logger <- function(module_name, enabled = TRUE, messages = NULL) {
  create_logger(enabled = enabled, messages = messages, module_name = module_name)
}

# =============================================================================
# CONVENIENCE LOGGERS
# =============================================================================

# Note: Specific logger functions have been removed. Use create_module_logger() directly:
# 
# For cache logging:
#   cache_logger <- create_module_logger("CACHE-SIMULATION")
#   cache_logger <- create_module_logger("CACHE-POWER-SPECTRUM")
#
# For data loading:
#   load_logger <- create_module_logger("LOAD-SIMULATION")
#   load_logger <- create_module_logger("LOAD-POWER-SPECTRUM")
#
# For complexity analysis:
#   complexity_logger <- create_module_logger("COMPLEXITY")

# =============================================================================
# LOGGING CONFIGURATION FUNCTIONS
# =============================================================================

#' Enable or disable global logging
#' @param enabled Whether to enable logging
set_global_logging <- function(enabled = TRUE) {
  GLOBAL_LOGGING_ENABLED <<- enabled
  if (enabled) {
    cat("[LOGGING] Global logging enabled\n")
  } else {
    cat("[LOGGING] Global logging disabled\n")
  }
}

#' Set the global log level
#' @param level Log level ("DEBUG", "INFO", "WARN", "ERROR")
set_global_log_level <- function(level = "DEBUG") {
  valid_levels <- names(LOG_LEVEL_PRIORITIES)
  if (!level %in% valid_levels) {
    warning("Invalid log level. Valid levels are: ", paste(valid_levels, collapse = ", "), ". Using DEBUG.")
    level <- "DEBUG"
  }
  GLOBAL_LOG_LEVEL <<- level
  cat(sprintf("[LOGGING] Global log level set to: %s (priority: %d)\n", level, LOG_LEVEL_PRIORITIES[[level]]))
}

#' Get current logging configuration
#' @return List with current logging settings
get_logging_config <- function() {
  list(
    enabled = GLOBAL_LOGGING_ENABLED,
    level = GLOBAL_LOG_LEVEL,
    level_priority = LOG_LEVEL_PRIORITIES[[GLOBAL_LOG_LEVEL]],
    available_levels = names(LOG_LEVEL_PRIORITIES)
  )
}

#' Show available log levels and their priorities
#' @return Data frame with log levels and priorities
show_log_levels <- function() {
  levels_df <- data.frame(
    Level = names(LOG_LEVEL_PRIORITIES),
    Priority = unlist(LOG_LEVEL_PRIORITIES),
    Description = c(
      "Detailed debug information (all messages)",
      "General information messages",
      "Warning messages (potential issues)",
      "Error messages (serious problems)"
    ),
    stringsAsFactors = FALSE
  )
  
  # Mark current level
  levels_df$Current <- levels_df$Level == GLOBAL_LOG_LEVEL
  
  return(levels_df)
}

#' Test logging levels by outputting a message at each level
test_logging_levels <- function() {
  test_logger <- create_module_logger("TEST")
  
  cat("Testing logging levels (current level:", GLOBAL_LOG_LEVEL, "):\n")
  cat("==========================================\n")
  
  test_logger("DEBUG", "This is a DEBUG message")
  test_logger("INFO", "This is an INFO message")
  test_logger("WARN", "This is a WARN message")
  test_logger("ERROR", "This is an ERROR message")
  
  cat("==========================================\n")
  cat("Only messages at or above the current level (", GLOBAL_LOG_LEVEL, ") should be visible above.\n")
}


# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Log a separator line for better readability
#' @param logger Logger function to use
#' @param char Character to use for separator (default: "=")
#' @param length Length of separator line (default: 50)
log_separator <- function(logger, char = "=", length = 50) {
  separator <- paste(rep(char, length), collapse = "")
  logger("DEBUG", separator)
}

#' Log start of a major operation
#' @param logger Logger function to use
#' @param operation_name Name of the operation
log_operation_start <- function(logger, operation_name) {
  log_separator(logger, "=", 60)
  logger("INFO", "START:", operation_name)
  log_separator(logger, "=", 60)
}

#' Log end of a major operation
#' @param logger Logger function to use
#' @param operation_name Name of the operation
#' @param success Whether the operation was successful
log_operation_end <- function(logger, operation_name, success = TRUE) {
  status <- if (success) "COMPLETED" else "FAILED"
  log_separator(logger, "=", 60)
  logger("INFO", "END:", operation_name, "-", status)
  log_separator(logger, "=", 60)
}

#' Log progress information
#' @param logger Logger function to use
#' @param current Current step number
#' @param total Total number of steps
#' @param message Additional message
log_progress <- function(logger, current, total, message = "") {
  percentage <- round((current / total) * 100, 1)
  progress_msg <- sprintf("[%d/%d] (%s%%) %s", current, total, percentage, message)
  logger("INFO", progress_msg)
}
