### Layout helpers

# Create logger for plotting module
plot_logger <- create_module_logger("PLOT")


# plotting$create_error_plot moved to runtime/source/features/plotting/basic_plots.R

# Helper function to create hover text for plotly plots
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

# Helper function to apply proper factor ordering to variables that have defined orderings
apply_factor_ordering <- function(data_long, columns_to_check = NULL) {
  plot_logger("DEBUG", "Starting apply_factor_ordering function")
  plot_logger("DEBUG", "Columns to check:", paste(columns_to_check, collapse = ", "))

  # Ensure global data is initialized
  ensure_global_data_initialized()

  # Define the mapping of column names to their ordered levels
  factor_mappings <- list(
    "trialNum" = list(levels = allTrials, convert_to_numeric = TRUE),
    "phase" = list(levels = allPhases, convert_to_numeric = FALSE),
    "taskNum" = list(levels = allTaskNums, convert_to_numeric = TRUE)
  )

  plot_logger("DEBUG", "Available factor mappings:", paste(names(factor_mappings), collapse = ", "))

  # If no specific columns to check, use all available factor mappings
  if (is.null(columns_to_check)) {
    columns_to_check <- names(factor_mappings)
    plot_logger("DEBUG", "No specific columns provided, using all factor mappings:", paste(columns_to_check, collapse = ", "))
  }

  # Find overlapping columns between factor mappings, data column names, and columns to check
  overlapping_cols <- intersect(intersect(names(factor_mappings), names(data_long)), columns_to_check)

  plot_logger("DEBUG", "Overlapping columns to process:", paste(overlapping_cols, collapse = ", "))

  # Only loop through columns that actually exist in the data and are requested
  for (col_name in overlapping_cols) {
    plot_logger("DEBUG", "Processing column:", col_name)
    mapping <- factor_mappings[[col_name]]

    plot_logger("DEBUG", "Mapping for", col_name, "- convert_to_numeric:", mapping$convert_to_numeric)
    plot_logger("DEBUG", "Available levels for", col_name, ":", paste(mapping$levels, collapse = ", "))

    if (mapping$convert_to_numeric) {
      # Convert to numeric first for trials
      plot_logger("DEBUG", "Converting", col_name, "to numeric")
      x_processed <- as.numeric(as.character(data_long[[col_name]]))
      present_levels <- unique(x_processed)
      plot_logger("DEBUG", "Present numeric levels in data:", paste(present_levels, collapse = ", "))
    } else {
      # Keep as character for phases
      plot_logger("DEBUG", "Keeping", col_name, "as character")
      x_processed <- as.character(data_long[[col_name]])
      present_levels <- unique(x_processed)
      plot_logger("DEBUG", "Present character levels in data:", paste(present_levels, collapse = ", "))
    }

    # Filter to only include levels that are actually present in the data
    ordered_present_levels <- mapping$levels[mapping$levels %in% present_levels]

    plot_logger("DEBUG", "Ordered levels to apply for", col_name, ":", paste(ordered_present_levels, collapse = ", "))

    # Apply factor ordering
    data_long[[col_name]] <- factor(x_processed, levels = ordered_present_levels)

    plot_logger("DEBUG", "Factor applied to", col_name, "- final levels:", paste(levels(data_long[[col_name]]), collapse = ", "))
  }

  plot_logger("DEBUG", "Factor ordering completed for", length(overlapping_cols), "columns")
  return(data_long)
}

plot_boxplots <- function(mu, datatype, xaxis = c("condition"), color_var = NULL, shape_var = NULL, baseSize = 10) {
  plot_logger("DEBUG", "Starting plot_boxplots function")
  plot_logger("DEBUG", "Input parameters - datatype:", datatype, "xaxis:", paste(xaxis, collapse = ", "), "color_var:", color_var, "shape_var:", shape_var)

  # Validate inputs
  if (is.null(mu) || !is.data.frame(mu) || nrow(mu) == 0) {
    plot_logger("ERROR", "Invalid input data: mu is null, not a dataframe, or empty")
    return(plotting$create_error_plot("No data available for plotting", baseSize))
  }

  if (is.null(datatype) || !datatype %in% colnames(mu)) {
    plot_logger("ERROR", "Invalid datatype:", datatype, "- not found in data columns:", paste(colnames(mu), collapse = ", "))
    return(plotting$create_error_plot(paste("Variable", datatype, "not found in data"), baseSize))
  }

  plot_logger("DEBUG", "Data validation passed - data has", nrow(mu), "rows and", ncol(mu), "columns")

  # Reshape data to long format for ggplot
  plot_logger("DEBUG", "Reshaping data to long format")
  data_long <- mu %>%
    pivot_longer(
      cols = !!datatype,
      names_to = "variable",
      values_to = "value"
    )

  plot_logger("DEBUG", "Data reshaped - long format has", nrow(data_long), "rows")

  # Apply pretty condition labels
  if ("condition" %in% colnames(data_long)) {
    plot_logger("DEBUG", "Applying pretty condition labels")
    data_long$condition <- get_pretty_condition_labels(data_long$condition)
    plot_logger("DEBUG", "Condition labels applied. Unique conditions:", paste(unique(data_long$condition), collapse = ", "))
  }

  # Apply proper factor ordering to x-axis variables that have defined orderings
  plot_logger("DEBUG", "Applying factor ordering to x-axis variables:", paste(xaxis, collapse = ", "))
  data_long <- apply_factor_ordering(data_long, columns_to_check = xaxis)

  # Log the factor levels after ordering
  for (col in xaxis) {
    if (col %in% colnames(data_long)) {
      if (is.factor(data_long[[col]])) {
        plot_logger("DEBUG", "Factor levels for", col, ":", paste(levels(data_long[[col]]), collapse = ", "))
      } else {
        plot_logger("DEBUG", "Column", col, "is not a factor, unique values:", paste(unique(data_long[[col]]), collapse = ", "))
      }
    }
  }

  # Create a combined x-axis variable with proper ordering
  plot_logger("DEBUG", "Creating combined x-axis variable")

  # First, create the combined variable as character
  data_long$xaxis_combined <- apply(data_long[, xaxis, drop = FALSE], 1, function(row) {
    paste(row, collapse = "_")
  })

  plot_logger("DEBUG", "Combined x-axis variable created. Unique combinations:", length(unique(data_long$xaxis_combined)))
  plot_logger("DEBUG", "Combined x-axis combinations:", paste(unique(data_long$xaxis_combined), collapse = ", "))

  # Now create proper ordering for the combined variable
  # Get all unique combinations
  unique_combinations <- unique(data_long$xaxis_combined)

  # Sort combinations based on the original factor ordering of individual variables
  if (length(xaxis) == 1) {
    # Single variable - use its factor levels directly
    if (xaxis %in% colnames(data_long) && is.factor(data_long[[xaxis]])) {
      ordered_combinations <- levels(data_long[[xaxis]])
      plot_logger("DEBUG", "Single variable ordering - using factor levels:", paste(ordered_combinations, collapse = ", "))
    } else {
      # Not a factor, sort alphabetically
      ordered_combinations <- sort(unique_combinations)
      plot_logger("DEBUG", "Single variable ordering - alphabetical sort:", paste(ordered_combinations, collapse = ", "))
    }
  } else {
    # Multiple variables - create cross-product ordering
    plot_logger("DEBUG", "Multiple variables - creating cross-product ordering")

    # Get ordered levels for each variable
    ordered_levels <- list()
    for (col in xaxis) {
      if (col %in% colnames(data_long) && is.factor(data_long[[col]])) {
        ordered_levels[[col]] <- levels(data_long[[col]])
        plot_logger("DEBUG", "Variable", col, "ordered levels:", paste(ordered_levels[[col]], collapse = ", "))
      } else {
        ordered_levels[[col]] <- sort(unique(data_long[[col]]))
        plot_logger("DEBUG", "Variable", col, "alphabetical levels:", paste(ordered_levels[[col]], collapse = ", "))
      }
    }

    # Create cross-product of all ordered levels
    ordered_combinations <- do.call(expand.grid, ordered_levels)
    ordered_combinations <- apply(ordered_combinations, 1, function(row) {
      paste(row, collapse = "_")
    })

    # Filter to only include combinations that actually exist in the data
    ordered_combinations <- ordered_combinations[ordered_combinations %in% unique_combinations]

    plot_logger("DEBUG", "Cross-product ordering created:", paste(ordered_combinations, collapse = ", "))
  }

  # Convert to factor with proper ordering
  data_long$xaxis_combined <- factor(data_long$xaxis_combined, levels = ordered_combinations)

  plot_logger("DEBUG", "Combined x-axis variable converted to factor with", length(levels(data_long$xaxis_combined)), "levels")
  plot_logger("DEBUG", "Final factor levels:", paste(levels(data_long$xaxis_combined), collapse = ", "))

  # Create hover text for points
  plot_logger("DEBUG", "Creating hover text for plotly")
  data_long$hover_text <- create_plotly_hover_text(data_long, datatype, data_long$xaxis_combined, "Category")

  # Set up aesthetics
  plot_logger("DEBUG", "Setting up plot aesthetics")
  if (is.null(color_var) || color_var == "None") {
    aesString <- aes_string()
    plot_logger("DEBUG", "No color variable specified")
  } else {
    aesString <- aes_string(color = color_var)
    plot_logger("DEBUG", "Color variable set to:", color_var)
  }

  # Dynamically add shape if shape_var is provided
  if (!(is.null(shape_var) || shape_var == "None")) {
    aesString <- modifyList(aesString, aes_string(shape = shape_var))
    plot_logger("DEBUG", "Shape variable set to:", shape_var)
  }

  # Create the plot
  plot_logger("DEBUG", "Creating ggplot object")
  p <- ggplot(data_long, aes(x = xaxis_combined, y = value, text = hover_text)) +
    # Draw one box for every x-axis combination, regardless of colour/shape mappings on the points
    geom_boxplot(aes(group = xaxis_combined)) +
    geom_jitter(aesString, width = 0.2, size = baseSize / 4, alpha = 0.7) +
    labs(x = paste(xaxis, collapse = " + "), y = datatype, title = datatype) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_viridis_d(option = "turbo") +
    get_sized_theme(baseSize)

  plot_logger("DEBUG", "Base plot created successfully")

  # Special case for trialNum color and shape
  if (color_var == "trialNum" && shape_var == "trialNum") {
    plot_logger("DEBUG", "Applying special trialNum color and shape mapping")
    shapes <- c(15, 15, 16, 16) # Square for 1, 2 and Circle for 3, 4
    colors <- c("darkred", "pink", "darkblue", "lightblue") # Dark/light for 1, 2 and 3, 4

    p <- p +
      scale_color_manual(name = "Trial Number", values = colors) +
      scale_shape_manual(name = "Trial Number", values = shapes)
  }

  plot_logger("DEBUG", "Boxplot creation completed successfully")
  return(p)
}

plot_paired <- function(mu, datatype, xPaired, xaxis = NULL, color_var = NULL, shape_var = NULL, baseSize = 10) {
  plot_logger("DEBUG", "Starting plot_paired function")
  plot_logger("DEBUG", "Input parameters - datatype:", datatype, "xPaired:", xPaired, "xaxis:", paste(xaxis, collapse = ", "), "color_var:", color_var, "shape_var:", shape_var)

  # Validate inputs
  if (is.null(mu) || !is.data.frame(mu) || nrow(mu) == 0) {
    plot_logger("ERROR", "Invalid input data: mu is null, not a dataframe, or empty")
    return(plotting$create_error_plot("No data available for plotting", baseSize))
  }

  if (is.null(datatype) || !datatype %in% colnames(mu)) {
    plot_logger("ERROR", "Invalid datatype:", datatype, "- not found in data columns:", paste(colnames(mu), collapse = ", "))
    return(plotting$create_error_plot(paste("Variable", datatype, "not found in data"), baseSize))
  }

  if (is.null(xPaired) || !xPaired %in% colnames(mu)) {
    plot_logger("ERROR", "Invalid xPaired variable:", xPaired, "- not found in data columns:", paste(colnames(mu), collapse = ", "))
    return(plotting$create_error_plot(paste("Paired variable", xPaired, "not found in data"), baseSize))
  }

  plot_logger("DEBUG", "Data validation passed - data has", nrow(mu), "rows and", ncol(mu), "columns")

  # Reshape data to long format for ggplot
  plot_logger("DEBUG", "Reshaping data to long format")
  data_long <- mu %>%
    pivot_longer(
      cols = !!datatype,
      names_to = "variable",
      values_to = "value"
    )

  plot_logger("DEBUG", "Data reshaped - long format has", nrow(data_long), "rows")

  # Apply pretty condition labels
  if ("condition" %in% colnames(data_long)) {
    plot_logger("DEBUG", "Applying pretty condition labels")
    data_long$condition <- get_pretty_condition_labels(data_long$condition)
    plot_logger("DEBUG", "Condition labels applied. Unique conditions:", paste(unique(data_long$condition), collapse = ", "))
  }

  # Apply proper factor ordering to x-axis variables that have defined orderings
  columns_to_check <- c(xPaired, xaxis)
  plot_logger("DEBUG", "Applying factor ordering to variables:", paste(columns_to_check, collapse = ", "))
  data_long <- apply_factor_ordering(data_long, columns_to_check = columns_to_check)

  # Log the factor levels after ordering
  for (col in columns_to_check) {
    if (!is.null(col) && col %in% colnames(data_long)) {
      if (is.factor(data_long[[col]])) {
        plot_logger("DEBUG", "Factor levels for", col, ":", paste(levels(data_long[[col]]), collapse = ", "))
      } else {
        plot_logger("DEBUG", "Column", col, "is not a factor, unique values:", paste(unique(data_long[[col]]), collapse = ", "))
      }
    }
  }

  # Create hover text for points
  plot_logger("DEBUG", "Creating hover text for plotly")
  data_long$hover_text <- create_plotly_hover_text(data_long, datatype, data_long[[xPaired]], "X-axis")

  # Set up aesthetics
  plot_logger("DEBUG", "Setting up plot aesthetics")
  if (is.null(color_var) || color_var == "None") {
    aesString <- aes_string()
    plot_logger("DEBUG", "No color variable specified")
  } else {
    aesString <- aes_string(color = color_var)
    plot_logger("DEBUG", "Color variable set to:", color_var)
  }

  # Dynamically add shape if shape_var is provided
  if (!(is.null(shape_var) || shape_var == "None")) {
    aesString <- modifyList(aesString, aes_string(shape = shape_var))
    plot_logger("DEBUG", "Shape variable set to:", shape_var)
  }

  # Create the plot using ggplot
  plot_logger("DEBUG", "Creating ggplot object for paired plot")
  p <- ggplot(data_long, aes_string(x = xPaired, y = "value", group = "participant", text = "hover_text")) +
    geom_line(aes(group = participant), color = "gray", size = 0.4) +
    geom_point(aesString, size = baseSize / 4) +
    labs(x = xPaired, y = datatype, title = datatype) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_viridis_d(option = "turbo") +
    get_sized_theme(baseSize)

  plot_logger("DEBUG", "Base paired plot created successfully")

  # Add facets if split_vars are provided
  if (!is.null(xaxis) && length(xaxis) > 0) {
    plot_logger("DEBUG", "Adding facets for variables:", paste(xaxis, collapse = ", "))
    # Create facet formula
    facet_formula <- paste("~", paste(xaxis, collapse = " + "))
    plot_logger("DEBUG", "Facet formula:", facet_formula)
    p <- p + facet_wrap(as.formula(facet_formula))
  }

  plot_logger("DEBUG", "Paired plot creation completed successfully")
  return(p)
}