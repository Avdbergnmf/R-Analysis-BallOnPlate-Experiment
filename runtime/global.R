#' Global Configuration and Setup
#'
#' This file sets up global configuration using the config package
#' following the standard pattern for Shiny/Rmd applications.
#'
#' @author Alex van den Berg
#' @version 1.0

# Load configuration from config.yml
# The config package automatically picks the environment based on R_CONFIG_ACTIVE
cfg <- config::get(file = "config.yml")

# Store configuration in options for easy access
options(app.cfg = cfg)

# Convenience accessor
cfg_get <- function(...) {
  getOption("app.cfg") |> (\(x) purrr::pluck(x, ...))()
}

# =============================================================================
# CONFIGURATION VALIDATION
# =============================================================================

#' Validate configuration settings
#' @param cfg Configuration object
stop_if_bad <- function(cfg) {
  stopifnot(
    is.logical(cfg$cache$force_rewrite),
    is.numeric(cfg$cache$compression_level),
    cfg$cache$compression_level >= 1 && cfg$cache$compression_level <= 22,
    is.logical(cfg$performance$use_parallel),
    is.numeric(cfg$performance$parallel_threshold),
    cfg$performance$parallel_threshold > 0
  )
  
  # Check for required environment variables in production
  if (identical(Sys.getenv("R_CONFIG_ACTIVE", "default"), "production")) {
    # Add any production-specific validation here
    cat("[CONFIG] Production environment detected\n")
  }
}

# Validate the loaded configuration
stop_if_bad(cfg)

# =============================================================================
# CONVENIENCE VARIABLES (for backward compatibility)
# =============================================================================

# Cache settings
CACHE_FORCE_REWRITE <- cfg_get("cache", "force_rewrite")
CACHE_COMPRESSION_LEVEL <- cfg_get("cache", "compression_level")
CACHE_FORMAT <- cfg_get("cache", "format")

# Performance settings
USE_PARALLEL <- cfg_get("performance", "use_parallel")
ENABLE_FILE_LOGGING <- cfg_get("performance", "enable_file_logging")
THRESHOLD_PARALLEL <- cfg_get("performance", "parallel_threshold")
FORCE_RECALC <- cfg_get("performance", "force_recalc")

# Data processing settings
USE_CONTINUOUS_FILTERING <- cfg_get("data", "use_continuous_filtering")
DEFAULT_SAMPLING_FREQ <- cfg_get("data", "default_sampling_freq")
DEFAULT_CUTOFF_FREQ <- cfg_get("data", "default_cutoff_freq")

# Logging settings
LOGGING_ENABLED <- cfg_get("logging", "enabled")
LOG_LEVEL <- cfg_get("logging", "level")
MEMORY_MONITORING <- cfg_get("logging", "memory_monitoring")

# =============================================================================
# LOAD UTILITIES
# =============================================================================

# Load utility functions
cat("[GLOBAL] Loading utility functions...\n")
source("source/utils/logging.R")
cat("[GLOBAL] ✓ logging.R loaded\n")


source("source/utils/cache.R")
cat("[GLOBAL] ✓ cache.R loaded\n")
source("source/utils/data_loading.R")
cat("[GLOBAL] ✓ data_loading.R loaded\n")
source("source/utils/dynamic_input.R")
cat("[GLOBAL] ✓ dynamic_input.R loaded\n")
source("source/utils/get_filtered_data.R")  # Load before filter_manager since it defines get_mu_dyn_long_data
cat("[GLOBAL] ✓ get_filtered_data.R loaded\n")
source("source/utils/filter_manager.R")
cat("[GLOBAL] ✓ filter_manager.R loaded\n")
cat("[GLOBAL] All utility functions loaded successfully\n")

# =============================================================================
# FEATURE MODULE LOADING
# =============================================================================
# Note: Feature modules are loaded separately after initialization
# to avoid circular dependencies.
#

#' Load a specific feature module by name
#' @param feature_name Name of the feature to load (e.g., "stats", "psd", "outliers")
#' @return The loaded feature module as a list
load_feature <- function(feature_name) {
  # Check if feature is already loaded
  env_var_name <- paste0(feature_name, "_env")
  list_var_name <- feature_name
  
  if (exists(list_var_name, envir = .GlobalEnv)) {
    return(get(list_var_name, envir = .GlobalEnv))
  }
  
  # Load the feature module
  feature_path <- file.path("source/features", feature_name)
  if (!dir.exists(feature_path)) {
    stop("Feature '", feature_name, "' not found at path: ", feature_path)
  }
  
  # Create environment for the feature with global environment as parent
  # This allows features to access all global functions without manual assignment
  feature_env <- new.env(parent = .GlobalEnv)
  
  cat(sprintf("[GLOBAL] Created %s feature environment with global access\n", feature_name))
  
  # Load all R files in the feature directory
  r_files <- list.files(feature_path, "\\.R$", full.names = TRUE)
  if (length(r_files) == 0) {
    stop("No R files found in feature directory: ", feature_path)
  }
  
  for (f in r_files) {
    cat(sprintf("[GLOBAL] Loading feature file: %s\n", basename(f)))
    tryCatch({
      sys.source(f, envir = feature_env, chdir = TRUE)
      cat(sprintf("[GLOBAL] ✓ %s loaded successfully\n", basename(f)))
    }, error = function(e) {
      cat(sprintf("[GLOBAL] ✗ ERROR loading %s: %s\n", basename(f), e$message))
      stop(e)
    })
  }
  
  # Convert to list and store globally
  feature_list <- as.list(feature_env)
  assign(list_var_name, feature_list, envir = .GlobalEnv)
  assign(env_var_name, feature_env, envir = .GlobalEnv)
  
  return(feature_list)
}

# =============================================================================
# INITIALIZATION
# =============================================================================

# Print configuration summary on load
if (interactive()) {
  active_env <- Sys.getenv("R_CONFIG_ACTIVE", unset = "default")
  cat(sprintf("[GLOBAL] Configuration loaded (environment: %s)\n", active_env))
  cat(sprintf("[GLOBAL] Cache force rewrite: %s\n", CACHE_FORCE_REWRITE))
  cat(sprintf("[GLOBAL] Log level: %s\n", LOG_LEVEL))
}
