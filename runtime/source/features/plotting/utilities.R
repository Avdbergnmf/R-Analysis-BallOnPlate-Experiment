#' Plotting Utility Functions
#'
#' Utility functions for plotting configuration and styling.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

#' Get sized theme for plots
#' @param baseSize Base font size
#' @return ggplot theme object
get_sized_theme <- function(baseSize) {
  return(theme(
    axis.title = element_text(size = baseSize * 2),
    axis.text = element_text(size = baseSize * 1.5),
    plot.title = element_text(size = baseSize * 2.5),
    legend.title = element_text(size = baseSize * 1.8),
    legend.text = element_text(size = baseSize * 1.5),
    strip.text = element_text(size = baseSize * 1.5)
  ))
}

#' Get proper legend configuration
#' @param show_legend Whether to show legend
#' @param position Legend position
#' @return ggplot theme object
get_proper_legend <- function(show_legend, position = "inside") {
  if (!show_legend) {
    return(theme(legend.position = "none"))
  } else {
    if (position == "inside") {
      return(theme(legend.position = position, legend.position.inside = c(0.95, 0.15)))
    } else {
      theme(legend.position = position)
    }
  }
}

#' Helper function to create hover text for plotly plots
#' @param data Data frame with data
#' @param datatype Data type name
#' @param x_value X-axis value (optional)
#' @param x_label X-axis label (optional)
#' @return Formatted hover text string
create_plotly_hover_text <- function(data, datatype, x_value = NULL, x_label = "Category") {
  # Base hover text
  hover_text <- paste0(
    "<b>", datatype, "</b><br>",
    "Value: ", round(data$value, 3)
  )

  # Add x-axis information if provided
  if (!is.null(x_value)) {
    hover_text <- paste0(hover_text, "<br>", x_label, ": ", x_value)
  }

  # Add participant info if available
  if ("participant" %in% colnames(data)) {
    hover_text <- paste0(hover_text, "<br>Participant: ", data$participant)
  }

  # Add trial info if available
  if ("trialNum" %in% colnames(data)) {
    hover_text <- paste0(hover_text, "<br>Trial: ", data$trialNum)
  }

  # Add condition info if available
  if ("condition" %in% colnames(data)) {
    hover_text <- paste0(hover_text, "<br>Condition: ", data$condition)
  }

  return(hover_text)
}

#' Helper function to apply proper factor ordering to variables that have defined orderings
#' @param data_long Data frame in long format
#' @param columns_to_check Vector of column names to check for factor ordering
#' @return Data frame with proper factor ordering applied
apply_factor_ordering <- function(data_long, columns_to_check = NULL) {
  plotting_logger("DEBUG", "Starting apply_factor_ordering function")
  plotting_logger("DEBUG", "Columns to check:", paste(columns_to_check, collapse = ", "))

  # Ensure global data is initialized
  ensure_global_data_initialized()

  # Define the mapping of column names to their ordered levels
  factor_mappings <- list(
    "trialNum" = list(levels = allTrials, convert_to_numeric = TRUE),
    "phase" = list(levels = allPhases, convert_to_numeric = FALSE),
    "taskNum" = list(levels = allTaskNums, convert_to_numeric = TRUE)
  )

  plotting_logger("DEBUG", "Available factor mappings:", paste(names(factor_mappings), collapse = ", "))

  # If no specific columns to check, use all available factor mappings
  if (is.null(columns_to_check)) {
    columns_to_check <- names(factor_mappings)
    plotting_logger("DEBUG", "No specific columns provided, using all factor mappings:", paste(columns_to_check, collapse = ", "))
  }

  # Find overlapping columns between factor mappings, data column names, and columns to check
  overlapping_cols <- intersect(intersect(names(factor_mappings), names(data_long)), columns_to_check)

  plotting_logger("DEBUG", "Overlapping columns to process:", paste(overlapping_cols, collapse = ", "))

  # Only loop through columns that actually exist in the data and are requested
  for (col_name in overlapping_cols) {
    plotting_logger("DEBUG", "Processing column:", col_name)
    mapping <- factor_mappings[[col_name]]

    plotting_logger("DEBUG", "Mapping for", col_name, "- convert_to_numeric:", mapping$convert_to_numeric)
    plotting_logger("DEBUG", "Available levels for", col_name, ":", paste(mapping$levels, collapse = ", "))

    if (mapping$convert_to_numeric) {
      # Convert to numeric first for trials
      plotting_logger("DEBUG", "Converting", col_name, "to numeric")
      x_processed <- as.numeric(as.character(data_long[[col_name]]))
      present_levels <- unique(x_processed)
      plotting_logger("DEBUG", "Present numeric levels in data:", paste(present_levels, collapse = ", "))
    } else {
      # Keep as character for phases
      plotting_logger("DEBUG", "Keeping", col_name, "as character")
      x_processed <- as.character(data_long[[col_name]])
      present_levels <- unique(x_processed)
      plotting_logger("DEBUG", "Present character levels in data:", paste(present_levels, collapse = ", "))
    }

    # Filter to only include levels that are actually present in the data
    ordered_present_levels <- mapping$levels[mapping$levels %in% present_levels]

    plotting_logger("DEBUG", "Ordered levels to apply for", col_name, ":", paste(ordered_present_levels, collapse = ", "))

    # Apply factor ordering
    data_long[[col_name]] <- factor(x_processed, levels = ordered_present_levels)

    plotting_logger("DEBUG", "Factor applied to", col_name, "- final levels:", paste(levels(data_long[[col_name]]), collapse = ", "))
  }

  plotting_logger("DEBUG", "Factor ordering completed for", length(overlapping_cols), "columns")
  return(data_long)
}
