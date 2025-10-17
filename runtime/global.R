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
source("source/utils/logging.R")
source("source/utils/cache.R")
source("source/utils/data_loading.R")
source("source/utils/dynamic_input.R")

# =============================================================================
# FEATURE MODULE LOADING
# =============================================================================
# Note: Feature modules are loaded separately after initialization
# to avoid circular dependencies.

#' Load all feature modules
#' This function should be called after initialization is complete
load_feature_modules <- function() {
  # Load stats feature module
  stats_env <<- new.env(parent = emptyenv())
  for (f in list.files("source/features/stats", "\\.R$", full.names = TRUE)) {
    sys.source(f, envir = stats_env)
  }
  stats <<- as.list(stats_env)  # use as stats$validate_stats_data()
  
  # Load PSD feature module
  psd_env <<- new.env(parent = emptyenv())
  for (f in list.files("source/features/psd", "\\.R$", full.names = TRUE)) {
    sys.source(f, envir = psd_env)
  }
  psd <<- as.list(psd_env)  # use as psd$compute_psd()
  
  # Load outliers feature module
  outliers_env <<- new.env(parent = emptyenv())
  for (f in list.files("source/features/outliers", "\\.R$", full.names = TRUE)) {
    sys.source(f, envir = outliers_env)
  }
  outliers <<- as.list(outliers_env)  # use as outliers$find_heel_strikes()
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
