#' Plotting Feature Module API
#'
#' Public interface for plotting functions in the BallOnPlate experiment.
#' This module provides plotting capabilities through a clean API.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

# =============================================================================
# PUBLIC API FUNCTIONS
# =============================================================================

# Data access functions
preprocess_tracker_data <- preprocess_tracker_data
is_kinematic_data <- is_kinematic_data
rotate_y <- rotate_y

# Basic plotting functions
create_error_plot <- create_error_plot
validate_data_and_columns <- validate_data_and_columns
plot_2d <- plot_2d

# Advanced plotting functions
# plot_steps(filteredGaitParams, preprocessedData, participant, trialNum, ...)
plot_steps <- plot_steps
make_histogram <- make_histogram

# Utility functions
get_proper_legend <- get_proper_legend

plotting_logger("INFO", "Plotting feature module API loaded successfully")
