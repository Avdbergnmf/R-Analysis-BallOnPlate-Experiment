#' Data Caching Utilities Loader
#'
#' Central entry-point to load the data caching setup and system files.
#' Exposes a named list `data_cache` for convenient, namespaced access
#' while retaining the existing global functions for backward compatibility.

# Load setup first so loader registries are available for the system module
source("source/utils/data_caching/setup.R", local = FALSE)

# Load the caching system implementation
source("source/utils/data_caching/system.R", local = FALSE)

# Provide a lightweight namespace-style accessor to the main entry points.
data_cache <- list(
  create_shiny_cache_manager = create_shiny_cache_manager,
  create_shiny_cached_data_reactive = create_shiny_cached_data_reactive,
  get_cached_data = get_cached_data,
  get_available_data_types = get_available_data_types,
  is_data_type_supported = is_data_type_supported,
  get_data_loader = get_data_loader,
  get_datasets_to_verify = get_datasets_to_verify,
  get_relevant_shiny_inputs = get_relevant_shiny_inputs,
  capture_shiny_inputs = capture_shiny_inputs,
  add_data_type = add_data_type,
  remove_data_type = remove_data_type,
  clear_data_cache = clear_data_cache,
  clear_all_caches = clear_all_caches,
  get_data_cache_stats = get_data_cache_stats,
  get_cache_stats = get_data_cache_stats
)
