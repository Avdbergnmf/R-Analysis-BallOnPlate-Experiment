### Layout helpers

#' Convert condition names to prettier labels for plotting
#' @param conditions Vector of condition names
#' @return Vector of pretty condition labels
get_pretty_condition_labels <- function(conditions) {
  # Use the global condition mapping from initialization.R
  ensure_global_data_initialized()

  # Apply the mapping, keeping original names if not found in map
  pretty_labels <- ifelse(conditions %in% names(condition_map),
    condition_map[conditions],
    conditions
  )

  return(pretty_labels)
}

get_sized_theme <- function(baseSize) {
  return(theme(
    axis.title = element_text(size = baseSize * 2),
    axis.text = element_text(size = baseSize),
    legend.title = element_text(size = baseSize * 1.5),
    legend.text = element_text(size = baseSize * 1.5),
    plot.title = element_text(size = baseSize * 2.5),
    strip.text = element_text(size = baseSize * 1.5)
  ))
}

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

plot_steps <- function(filteredGaitParams, participant, trialNum, x_axis = "time", y_axis = "pos_z", doFilter = FALSE, show_legend = TRUE, extraTitle = "", baseSize = 5, xlim = NULL, ylim = NULL, preprocessedData = NULL) { # start=first step to plot, end=last step to plot
  filteredGaitParams <- filteredGaitParams[filteredGaitParams$participant == participant & filteredGaitParams$trialNum == trialNum, ]

  if (is.null(preprocessedData)) {
    preprocessedData <- get_preprocessed_data(participant, trialNum)
  }

  rightData <- preprocessedData$rightfoot
  leftData <- preprocessedData$leftfoot

  if (doFilter) {
    numeric_columns <- sapply(rightData, is.numeric) # Identify numeric columns
    rightData[numeric_columns] <- lapply(rightData[numeric_columns], function(column) {
      apply_padding_and_filter(column, 4, 90)
    })
    leftData[numeric_columns] <- lapply(leftData[numeric_columns], function(column) {
      apply_padding_and_filter(column, 4, 90)
    })
  }

  # Add a new column to both dataframes to identify the foot
  rightData$foot <- "Right"
  leftData$foot <- "Left"
  # Combine the dataframes
  both <- rbind(rightData, leftData)
  both <- both[order(both$time), ] # Order by time

  rParams <- filteredGaitParams[filteredGaitParams$foot == "Right", ]
  lParams <- filteredGaitParams[filteredGaitParams$foot == "Left", ]

  # Separate outliers from non-outliers
  rOutliers <- rParams[rParams$outlierSteps, ]
  lOutliers <- lParams[lParams$outlierSteps, ]
  rParams <- rParams[!rParams$outlierSteps, ]
  lParams <- lParams[!lParams$outlierSteps, ]

  # Create the plot
  targetSize <- round(baseSize / 2)
  footEventSize <- round(baseSize / 4)
  outlierSize <- round(baseSize / 2) # Use a larger size for outliers

  p <- ggplot(both, aes(x = .data[[x_axis]], y = .data[[y_axis]], color = .data$foot)) +
    geom_path() +
    # toeOffs
    # geom_point(data = rParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "red", size = footEventSize) +
    # geom_point(data = lParams, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, color = "blue", size = footEventSize) + # 12=empty square
    # heelstrikes
    geom_point(data = rParams, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, color = "red", size = footEventSize) + # 16=ball
    geom_point(data = lParams, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, color = "blue", size = footEventSize) +
    # outlier steps (with filled icons and larger size)
    geom_point(data = rOutliers, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, fill = "red", alpha = 0.5, color = "red", size = outlierSize) +
    geom_point(data = lOutliers, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, fill = "blue", alpha = 0.5, color = "blue", size = outlierSize) + # 21 = filled circle
    # geom_point(data = rOutliers, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, fill = "red", alpha = 0.5, color = "red", size = outlierSize) +
    # geom_point(data = lOutliers, aes(x = .data[[paste0("toeOffs.", x_axis)]], y = .data[[paste0("toeOffs.", y_axis)]]), shape = 24, fill = "blue", alpha = 0.5, color = "blue", size = outlierSize) +
    scale_color_manual(values = c("Right" = "black", "Left" = "grey")) +
    ggtitle(extraTitle) +
    theme_minimal(base_size = baseSize) # get_sized_theme(baseSize)

  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }

  xlimValid <- xlim[1] < xlim[2] && length(xlim) == 2 && sum(is.na(xlim)) == 0
  ylimValid <- ylim[1] < ylim[2] && length(ylim) == 2 && sum(is.na(ylim)) == 0
  if (!is.null(xlim) && xlimValid) {
    p <- p + xlim(xlim)
  }
  if (!is.null(ylim) && ylimValid) {
    p <- p + ylim(ylim)
  }

  p <- p + get_proper_legend(show_legend)

  return(p)
}

add_lines <- function(p, footEvents, rightData, leftData, start, end, x_axis = "time", y_axis = "pos_z") { # start=first step to plot, end=last step to plot
  heelStrikes <- footEvents$heelStrikes

  # Filter out a subset of the steps if needed
  if (start > 1) {
    start <- start - 1
  } # make sure we get the step before
  steps <- start:end

  if (length(steps) > 1) {
    heelStrikes <- subset(heelStrikes, step %in% steps)
    # Filter out the steps we want to plot
    rHeelStrikes <- subset(heelStrikes, .data$foot == "Right")
    timeMin <- min(rHeelStrikes$time)
    timeMax <- max(rHeelStrikes$time)
    rightData <- subset(rightData, time > timeMin & time < timeMax)
    leftData <- subset(leftData, time > timeMin & time < timeMax)
  }

  # Add a new column to both dataframes to identify the foot
  rightData$foot <- "Right"
  leftData$foot <- "Left"

  # Create the plot
  p <- p + geom_path(data = rightData, aes(x = .data[[x_axis]], y = .data[[y_axis]]), color = "pink") +
    geom_path(data = leftData, aes(x = .data[[x_axis]], y = .data[[y_axis]]), color = "lightblue")
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }
  return(p)
}

plot_steps_with_overlay <- function(data, selected_participant, selected_trialNum, axis_to_plot, doFilter, alpha = 0.15, show_legend = FALSE, extraTitle = "", baseSize = 10) {
  p <- plot_steps(data, selected_participant, selected_trialNum, "time", axis_to_plot, doFilter, show_legend, extraTitle, baseSize)

  condition <- condition_number(selected_participant)

  # Assign colors based on condition instead of VFD
  if (condition == "control") {
    color <- "yellow" # control condition
  } else if (condition == "perturbation") {
    color <- "green" # perturbations only
  } else if (condition == "perturbation_visualization") {
    color <- "blue" # visualizations + perturbations
  } else {
    color <- "gray" # fallback
  }

  # Add a semi-transparent square (replace xmin, xmax, ymin, ymax with your desired coordinates)
  p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = color, alpha = alpha)

  return(p)
}

# Helper function to create error plots
create_error_plot <- function(message, baseSize = 10) {
  cat("DEBUG:", message, "\n")
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = message, size = 4, hjust = 0.5) +
    xlim(0, 1) +
    ylim(0, 1) +
    labs(title = "Error") +
    theme_minimal(base_size = baseSize) +
    theme(axis.text = element_blank(), axis.ticks = element_blank())
}

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
  # Ensure global data is initialized
  ensure_global_data_initialized()
  
  # Define the mapping of column names to their ordered levels
  factor_mappings <- list(
    "trialNum" = list(levels = allTrials, convert_to_numeric = TRUE),
    "phase" = list(levels = allPhases, convert_to_numeric = FALSE),
    "taskNum" = list(levels = allTaskNums, convert_to_numeric = TRUE)
  )
  
  # If no specific columns to check, use all available factor mappings
  if (is.null(columns_to_check)) {
    columns_to_check <- names(factor_mappings)
  }
  
  # Find overlapping columns between factor mappings, data column names, and columns to check
  overlapping_cols <- intersect(intersect(names(factor_mappings), names(data_long)), columns_to_check)
  
  # Only loop through columns that actually exist in the data and are requested
  for (col_name in overlapping_cols) {
    mapping <- factor_mappings[[col_name]]
    
    if (mapping$convert_to_numeric) {
      # Convert to numeric first for trials
      x_processed <- as.numeric(as.character(data_long[[col_name]]))
      present_levels <- unique(x_processed)
    } else {
      # Keep as character for phases
      x_processed <- as.character(data_long[[col_name]])
      present_levels <- unique(x_processed)
    }
    
    # Filter to only include levels that are actually present in the data
    ordered_present_levels <- mapping$levels[mapping$levels %in% present_levels]
    
    # Apply factor ordering
    data_long[[col_name]] <- factor(x_processed, levels = ordered_present_levels)
  }
  
  return(data_long)
}


# Helper function to validate data and columns
validate_data_and_columns <- function(xData, yData, xtracker, ytracker, x_axis, y_axis, participant, trialNum, baseSize) {
  # Check if data is NULL
  if (is.null(xData)) {
    return(create_error_plot(paste("No data available for participant", participant, "trial", trialNum, "xtracker:", xtracker), baseSize))
  }

  if (is.null(yData)) {
    return(create_error_plot(paste("No data available for participant", participant, "trial", trialNum, "ytracker:", ytracker), baseSize))
  }

  # Check if requested columns exist
  if (!x_axis %in% colnames(xData)) {
    return(create_error_plot(paste("Column", x_axis, "not found in", xtracker, "data"), baseSize))
  }

  if (!y_axis %in% colnames(yData)) {
    return(create_error_plot(paste("Column", y_axis, "not found in", ytracker, "data"), baseSize))
  }

  return(NULL) # No errors
}

plot_2d <- function(xtracker, ytracker, participant, trialNum, x_axis = "time", y_axis = "pos_z", plotlines = TRUE, plotpoints = FALSE, extraTitle = "", baseSize = 10) {
  # Load data for both trackers
  xData <- preprocess_data(participant, trialNum, xtracker)
  yData <- preprocess_data(participant, trialNum, ytracker)

  # Validate data and columns - return error plot if validation fails
  error_plot <- validate_data_and_columns(xData, yData, xtracker, ytracker, x_axis, y_axis, participant, trialNum, baseSize)
  if (!is.null(error_plot)) {
    return(error_plot)
  }

  # Combine the dataframes
  both <- data.frame(
    x = xData[[x_axis]],
    y = yData[[y_axis]]
  )

  # Create the plot
  p <- ggplot(xData, aes(.data[[x_axis]], .data[[y_axis]])) +
    labs(x = x_axis, y = y_axis) +
    theme_minimal(base_size = baseSize) +
    ggtitle(paste(extraTitle, ",", x_axis, "vs.", y_axis))

  # Add geometries based on options
  if (plotlines) {
    p <- p + geom_path()
  }

  if (plotpoints) {
    p <- p + geom_point()
  }

  # Set equal coordinates for position plots
  if (x_axis != "time" && y_axis != "time") {
    p <- p + coord_equal()
  }

  return(p)
}


plot_questionnaire_data <- function(data, qType, cols_to_include = c(), baseSize = 10) {
  data <- filter_questionnaire_results(data, qType)

  # Only keep the columns to include in the plot
  if (length(cols_to_include) == 0) {
    cols_to_include <- setdiff(colnames(data), c("participant", "answer_type", "condition", "none"))
  }
  data <- data[, c("participant", "answer_type", "condition", cols_to_include), drop = FALSE]

  # Convert condition names to pretty labels
  data$condition <- get_pretty_condition_labels(data$condition)

  # Reshape the data to long format for ggplot
  data_long <- reshape2::melt(data, id.vars = c("participant", "answer_type", "condition"))

  qweights <- get_question_weights(qType)
  min_plot <- if ("min_plot" %in% qweights$category) qweights[qweights$category == "min_plot", "weight"] else NULL
  max_plot <- if ("max_plot" %in% qweights$category) qweights[qweights$category == "max_plot", "weight"] else NULL

  # Create the plot showing changes across phases, colored by condition
  p <- ggplot(data_long, aes(x = answer_type, y = value, group = participant, color = factor(condition))) +
    geom_line(alpha = 0.6, size = 0.4) +
    geom_point(alpha = 0.8, size = 1) +
    facet_wrap(~variable, scales = "free", ncol = length(cols_to_include)) +
    labs(x = "Phase", y = "Score", color = "Condition") +
    ggtitle(paste0(qType, " Scores Across Phases")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    get_sized_theme(baseSize) +
    scale_color_viridis_d()

  # Conditionally add y-axis limits if min_plot and max_plot are provided
  if (!is.null(min_plot) && !is.null(max_plot)) {
    p <- p + ylim(min_plot, max_plot)
  }

  return(p)
}



make_histogram <- function(data, mu_data, showMeans, group, split, xinput, binwidth, position, baseSize) {
  # Check if the variable is categorical or numeric
  is_categorical <- !is.numeric(data[[xinput]])

  if (is_categorical) {
    # For categorical variables, create a bar chart
    if (group != "None") {
      # When grouping, use fill aesthetic and position
      p <- ggplot(data, aes(x = .data[[xinput]], fill = .data[[group]])) +
        geom_bar(alpha = if (position == "identity") 0.5 else 1, position = position) +
        theme_minimal(base_size = baseSize) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (split != "None") {
        p <- p + facet_grid(as.formula(paste("~", split)))
      }
    } else {
      # When not grouping, use fixed fill color
      p <- ggplot(data, aes(x = .data[[xinput]])) +
        geom_bar(fill = "grey", alpha = 1) +
        theme_minimal(base_size = baseSize) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (split != "None") {
        p <- p + facet_grid(reformulate(".", split))
      }
    }

    return(p)
  }

  # For numeric variables, use the existing histogram logic
  # ------------------------------------------------------------------
  # Safety check: avoid generating an excessive number of bins which
  # would lock up the dashboard. If the estimated number of bins
  # exceeds MAX_BINS, we return a placeholder plot with a warning.
  # ------------------------------------------------------------------
  MAX_BINS <- 5000
  if (is.numeric(binwidth) && binwidth > 0 && xinput %in% names(data)) {
    rng <- range(data[[xinput]], na.rm = TRUE)
    if (is.finite(rng[1]) && is.finite(rng[2])) {
      n_bins_est <- ceiling((rng[2] - rng[1]) / binwidth)
      if (n_bins_est > MAX_BINS) {
        warning(sprintf("Requested ~%d bins (> %d). Histogram skipped.", n_bins_est, MAX_BINS))
        return(
          ggplot() +
            annotate(
              "text",
              x = 0.5, y = 0.5,
              label = sprintf("Bin width too small: ~%d bins. Increase bin width.", n_bins_est),
              hjust = 0.5, vjust = 0.5, size = 4, color = "red"
            ) +
            xlim(0, 1) +
            ylim(0, 1) +
            theme_void() +
            ggtitle("Histogram Skipped")
        )
      }
    }
  }

  aes <- aes_string(x = xinput)
  a <- 1
  fill <- "grey"
  if (group != "None") {
    fill <- "white"
    aes <- modifyList(aes, aes_string(col = group))
    if (position == "identity") {
      a <- 1 / 2
    }
  }

  p <- ggplot(data, aes) +
    geom_histogram(binwidth = binwidth, fill = fill, alpha = a, position = position) +
    theme_minimal(base_size = baseSize)

  if (split != "None") {
    p <- p + facet_grid(sym(split))
  }

  return(p)
}

plot_boxplots <- function(mu, datatype, xaxis = c("condition"), color_var = NULL, shape_var = NULL, baseSize = 10) {
  # Reshape data to long format for ggplot
  data_long <- mu %>%
    pivot_longer(
      cols = !!datatype,
      names_to = "variable",
      values_to = "value"
    )

  data_long$condition <- get_pretty_condition_labels(data_long$condition)
  
  # Apply proper factor ordering to x-axis variables that have defined orderings
  data_long <- apply_factor_ordering(data_long, columns_to_check = xaxis)

  # Create a combined x-axis variable
  data_long$xaxis_combined <- apply(data_long[, xaxis], 1, paste, collapse = "_")


  # Create hover text for points
  data_long$hover_text <- create_plotly_hover_text(data_long, datatype, data_long$xaxis_combined, "Category")

  if (is.null(color_var) || color_var == "None") { #
    aesString <- aes_string()
  } else {
    aesString <- aes_string(color = color_var)
  }

  # Dynamically add shape if shape_var is provided
  if (!(is.null(shape_var) || shape_var == "None")) {
    aesString <- modifyList(aesString, aes_string(shape = shape_var))
  }

  # Create the plot
  p <- ggplot(data_long, aes(x = xaxis_combined, y = value, text = hover_text)) +
    # Draw one box for every x-axis combination, regardless of colour/shape mappings on the points
    geom_boxplot(aes(group = xaxis_combined)) +
    geom_jitter(aesString, width = 0.2, size = baseSize / 4, alpha = 0.7) + # << make this point?
    labs(x = paste(xaxis, collapse = " + "), y = datatype, title = datatype) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_viridis_d(option = "turbo") + # Choose "viridis", "plasma", "magma", etc.
    # scale_shape_manual(values = c(1:25)) +
    get_sized_theme(baseSize)

  if (color_var == "trialNum" && shape_var == "trialNum") { # special case for trialnum, because we used this in our paper and we wanted this layout there
    shapes <- c(15, 15, 16, 16) # Square for 1, 2 and Circle for 3, 4
    colors <- c("darkred", "pink", "darkblue", "lightblue") # Dark/light for 1, 2 and 3, 4

    p <- p +
      scale_color_manual(name = "Trial Number", values = colors) +
      scale_shape_manual(name = "Trial Number", values = shapes)
  }

  return(p)
}

plot_paired <- function(mu, datatype, xPaired, xaxis = NULL, color_var = NULL, shape_var = NULL, baseSize = 10) {
  # Reshape data to long format for ggplot
  data_long <- mu %>%
    pivot_longer(
      cols = !!datatype,
      names_to = "variable",
      values_to = "value"
    )

  data_long$condition <- get_pretty_condition_labels(data_long$condition)
  
  # Apply proper factor ordering to x-axis variables that have defined orderings
  data_long <- apply_factor_ordering(data_long, columns_to_check = c(xPaired, xaxis))

  # Create hover text for points
  data_long$hover_text <- create_plotly_hover_text(data_long, datatype, data_long[[xPaired]], "X-axis")

  if (is.null(color_var) || color_var == "None") { #
    aesString <- aes_string()
  } else {
    aesString <- aes_string(color = color_var)
  }

  # Dynamically add shape if shape_var is provided
  if (!(is.null(shape_var) || shape_var == "None")) {
    aesString <- modifyList(aesString, aes_string(shape = shape_var))
  }

  # Create the plot using ggplot
  p <- ggplot(data_long, aes_string(x = xPaired, y = "value", group = "participant", text = "hover_text")) +
    geom_line(aes(group = participant), color = "gray", size = 0.4) +
    geom_point(aesString, size = baseSize / 4) +
    labs(x = xPaired, y = datatype, title = datatype) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_viridis_d(option = "turbo") + # Choose "viridis", "plasma", "magma", etc.
    # scale_shape_manual(values = c(1:25)) +
    get_sized_theme(baseSize) #+ scale_color_jco()

  # Add facets if split_vars are provided
  if (!is.null(xaxis) && length(xaxis) > 0) {
    # Create facet formula
    facet_formula <- paste("~", paste(xaxis, collapse = " + "))
    p <- p + facet_wrap(as.formula(facet_formula))
  }

  return(p)
}



make_pie_chart <- function(data, extraTitle = "", show_legend = TRUE, baseSize = 10) {
  # Calculate step categories
  outlierSteps <- sum(data$outlierSteps == TRUE, na.rm = TRUE)
  suspectSteps <- sum(data$suspect == TRUE & data$outlierSteps == FALSE, na.rm = TRUE)
  included <- sum(data$outlierSteps == FALSE & data$suspect == FALSE, na.rm = TRUE)
  total_steps <- nrow(data)

  # Create a data frame for ggplot
  df_filtered <- data.frame(
    StepType = factor(c("Outlier", "Suspect", "Included"), levels = c("Outlier", "Suspect", "Included")),
    TotalCount = c(outlierSteps, suspectSteps, included)
  )

  # Remove categories with zero counts
  df_filtered <- df_filtered[df_filtered$TotalCount > 0, ]

  # Calculate label positions for the pie chart
  df_filtered$label_pos <- cumsum(df_filtered$TotalCount) - df_filtered$TotalCount / 2

  # Generate the pie chart
  p <- ggplot(df_filtered, aes(x = "", y = TotalCount, fill = StepType)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y", start = 0) +
    theme_void() +
    scale_fill_manual(values = c("Outlier" = "#FF9999", "Suspect" = "#FFD699", "Included" = "#99FF99")) +
    geom_text(aes(label = TotalCount, y = label_pos), color = "black", size = round(baseSize / 2)) +
    ggtitle(paste0(extraTitle, "Total steps = ", total_steps)) +
    theme_minimal(base_size = baseSize)

  p <- p + get_proper_legend(show_legend, "right")

  return(p)
}



circleFun <- function(center = c(0, 0), r = 1, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


### Scatter plots
make_scatter_plot_steps <- function(data, group, xplot, yplot, show_legend = FALSE, baseSize = 10) {
  if (group == "None") {
    aes <- aes_string(x = xplot, y = yplot)
  } else {
    aes <- aes_string(x = xplot, y = yplot, col = group)
  }

  p <- ggplot(data, aes) +
    geom_point(alpha = 0.5, size = baseSize / 4) + # Set the alpha to make overlapping points more visible
    theme_minimal(base_size = baseSize)

  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  # else {
  #  p <- p + theme(legend.position = "inside", legend.position.inside = c(0.95, 0.15))
  # }

  both_contain_pos <- grepl("pos", xplot, ignore.case = TRUE) && grepl("pos", yplot, ignore.case = TRUE)
  if (both_contain_pos) {
    p <- p + coord_equal()
  }

  # Add marginal density plots
  p <- ggMarginal(p, type = "density", margins = "both", groupColour = TRUE, groupFill = TRUE)

  return(p)
}

###### Extra

# Function to save plot as an image
save_plot <- function(plot, filename, width = 8, height = 4, pdf = FALSE) {
  if (!pdf) {
    ggsave(filename, plot, device = "png", width = width, height = height)
  } else {
    pdf(filename, width = width, height = height)
    print(plot)
    dev.off()
  }
}

plot_correlation_stats <- function(data, x_var_name, y_var_name, type = "parametric", base_size = 12, do_heatmap = FALSE, heatmap_bins = 30) {
  x_var <- sym(x_var_name)
  y_var <- sym(y_var_name)
  # Create the base plot with ggscatterstats or a heatmap based on plot_type
  if (!do_heatmap) {
    # Scatter plot with statistical summary
    p <- ggscatterstats(
      data = data,
      x = !!x_var,
      y = !!y_var,
      type = type
    ) +
      theme_minimal(base_size = base_size)
  } else {
    # Heatmap to visualize density of points
    p <- ggplot(data, aes(x = !!x_var, y = !!y_var)) +
      stat_bin2d(bins = heatmap_bins) +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal(base_size = base_size) +
      labs(
        title = paste("Heatmap of", x_var_name, "and", y_var_name),
        x = x_var_name,
        y = y_var_name,
        fill = "Count"
      )
  }

  return(p)
}

#### Simulation Data Plotting Functions ####

#' Plot simulation time series
#'
#' @param data Simulation data from get_simulation_data()
#' @param vars Vector of variable names to plot
#' @param downsampling Downsampling factor
#' @param plot_width Plot width
#' @param plot_height Plot height
#' @return plotly object
plot_simulation_timeseries <- function(data, vars, downsampling = 1,
                                       plot_width = 800, plot_height = 600) {
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    return(plot_ly() %>% layout(title = "No simulation data available"))
  }

  if (length(vars) == 0) {
    return(plot_ly() %>% layout(title = "No variables selected"))
  }

  # Apply downsampling
  if (downsampling > 1) {
    data <- data[seq(1, nrow(data), by = downsampling), ]
  }

  # Color palette for different variables
  colors <- RColorBrewer::brewer.pal(min(8, max(3, length(vars))), "Set1")

  # Get variable name mapping from get_simulation_data.R
  var_name_map <- get_simulation_variable_names()

  # Create time series plot
  p <- plot_ly(
    width = plot_width,
    height = min(plot_height, 600)
  )

  for (i in seq_along(vars)) {
    var <- vars[i]
    if (var %in% colnames(data)) {
      # Get pretty name
      var_name <- var_name_map[var]
      if (is.na(var_name)) var_name <- var

      # Coerce logical/factor to numeric so they can plot alongside continuous data
      y_values <- data[[var]]
      if (is.logical(y_values)) {
        y_values <- as.integer(y_values) # TRUE->1, FALSE->0
      } else if (is.factor(y_values)) {
        y_values <- as.numeric(y_values)
      }

      p <- p %>% add_trace(
        x = data$time, # Use trial time instead of simulation time
        y = y_values,
        type = "scatter",
        mode = "lines",
        name = var_name,
        line = list(color = colors[i %% length(colors) + 1], width = 2),
        hovertemplate = paste0(
          "<b>", var_name, ":</b> %{y:.4f}<br>",
          "<b>Trial Time:</b> %{x:.2f}s<br>",
          "<extra></extra>"
        )
      )
    }
  }

  p <- p %>% layout(
    title = "Simulation Variables Over Time",
    xaxis = list(title = "Trial Time (s)"),
    yaxis = list(title = "Value"),
    legend = list(orientation = "v", x = 1.02, y = 1)
  )

  return(p)
}
