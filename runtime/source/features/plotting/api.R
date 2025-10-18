#' Plotting Feature Module API
#'
#' Public interface for plotting functions in the BallOnPlate experiment.
#' This module provides plotting capabilities through a clean API.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

# =============================================================================
# PUBLIC API FUNCTIONS
# =============================================================================

# Note: All functions are automatically available through the feature module loading system.
# The load_feature() function in global.R loads all .R files in the feature directory
# and makes all functions available through the module object (e.g., plotting$function_name).
#
# Available functions:
# - Basic plotting: create_error_plot, validate_data_and_columns, plot_2d
# - Advanced plotting: plot_steps, make_histogram, plot_questionnaire_data, make_pie_chart, make_scatter_plot_steps
# - Utilities: get_proper_legend, get_sized_theme
#

plotting_logger("INFO", "Plotting feature module API loaded successfully")
