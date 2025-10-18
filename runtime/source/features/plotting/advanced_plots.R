#' Advanced Plotting Functions
#'
#' Advanced plotting functions for gait steps and histograms.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

#' Plot gait steps with foot events and outliers
#' @param filteredGaitParams Filtered gait parameters data
#' @param preprocessedData Preprocessed data (required)
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param x_axis X-axis variable name
#' @param y_axis Y-axis variable name
#' @param doFilter Whether to apply filtering
#' @param show_legend Whether to show legend
#' @param extraTitle Additional title text
#' @param baseSize Base font size
#' @param xlim X-axis limits
#' @param ylim Y-axis limits
#' @return ggplot object
plot_steps <- function(filteredGaitParams, preprocessedData, participant, trialNum, x_axis = "time", y_axis = "pos_z", doFilter = FALSE, show_legend = TRUE, extraTitle = "", baseSize = 5, xlim = NULL, ylim = NULL) {
  plotting_logger("DEBUG", "Starting plot_steps function")
  plotting_logger("DEBUG", sprintf("Input parameters - participant: %s, trialNum: %s, x_axis: %s, y_axis: %s, doFilter: %s", participant, trialNum, x_axis, y_axis, doFilter))

  # Convert trialNum to numeric for consistent filtering
  trialNum <- as.numeric(trialNum)
  filteredGaitParams <- filteredGaitParams[filteredGaitParams$participant == participant & filteredGaitParams$trialNum == trialNum, ]

  if (is.null(preprocessedData)) {
    plotting_logger("ERROR", "preprocessedData is required for plot_steps function")
    return(create_error_plot("No preprocessed data provided", baseSize))
  }

  rightData <- preprocessedData$rightfoot
  leftData <- preprocessedData$leftfoot

  if (doFilter) {
    plotting_logger("WARN", "Filtering requested but not implemented in self-contained plotting module")
    # Note: Filtering would need to be applied to the data before passing to this function
    # or the filtering function would need to be passed as a parameter
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
    # heelstrikes
    geom_point(data = rParams, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, color = "red", size = footEventSize) +
    geom_point(data = lParams, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, color = "blue", size = footEventSize) +
    # outlier steps (with filled icons and larger size)
    geom_point(data = rOutliers, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, fill = "red", alpha = 0.5, color = "red", size = outlierSize) +
    geom_point(data = lOutliers, aes(x = .data[[x_axis]], y = .data[[y_axis]]), shape = 25, fill = "blue", alpha = 0.5, color = "blue", size = outlierSize) +
    scale_color_manual(values = c("Right" = "black", "Left" = "grey")) +
    ggtitle(extraTitle) +
    theme_minimal(base_size = baseSize)

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

  plotting_logger("DEBUG", "plot_steps function completed successfully")
  return(p)
}

#' Create histogram with safety checks and error handling
#' @param data Input data frame
#' @param mu_data Additional data (unused but kept for compatibility)
#' @param showMeans Whether to show means (unused but kept for compatibility)
#' @param group Grouping variable
#' @param split Split variable for faceting
#' @param xinput X-axis variable name
#' @param binwidth Bin width for histogram
#' @param position Position adjustment for bars
#' @param baseSize Base font size
#' @param disable_size_check Whether to disable size checks
#' @return ggplot object
make_histogram <- function(data, mu_data, showMeans, group, split, xinput, binwidth, position, baseSize, disable_size_check = FALSE) {
  plotting_logger("DEBUG", "Starting make_histogram function")
  plotting_logger("DEBUG", sprintf("Input parameters - xinput: %s, group: %s, split: %s, binwidth: %s, position: %s", xinput, group, split, binwidth, position))

  # Check if the variable is categorical or numeric
  is_categorical <- !is.numeric(data[[xinput]])
  plotting_logger("DEBUG", "Variable is categorical:", is_categorical)

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

    plotting_logger("DEBUG", "Categorical histogram completed successfully")
    return(p)
  }

  # For numeric variables, use the existing histogram logic
  # ------------------------------------------------------------------
  # Safety check: avoid generating an excessive number of bins which
  # would lock up the dashboard. If the estimated number of bins
  # exceeds MAX_BINS, we return a placeholder plot with a warning.
  # ------------------------------------------------------------------
  plotting_logger("DEBUG", "Processing numeric variable histogram")
  MAX_BINS <- 5000
  if (is.numeric(binwidth) && binwidth > 0 && xinput %in% names(data)) {
    plotting_logger("DEBUG", "Calculating data range for bin estimation")
    rng <- range(data[[xinput]], na.rm = TRUE)
    plotting_logger("DEBUG", "Data range:", rng[1], "to", rng[2])

    if (is.finite(rng[1]) && is.finite(rng[2])) {
      n_bins_est <- ceiling((rng[2] - rng[1]) / binwidth)
      plotting_logger("DEBUG", "Estimated number of bins:", n_bins_est)

      if (n_bins_est > MAX_BINS) {
        plotting_logger("WARN", "Too many bins requested:", n_bins_est, ">", MAX_BINS, "- skipping histogram")
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

  plotting_logger("DEBUG", "Setting up histogram aesthetics")
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

  plotting_logger("DEBUG", "About to create ggplot with geom_histogram")
  plotting_logger("DEBUG", "Data size before histogram:", nrow(data), "rows")
  plotting_logger("DEBUG", "Memory usage before histogram:", format(object.size(data), units = "MB"))

  # Additional safety check for very large datasets
  MAX_ROWS_FOR_HISTOGRAM <- 1000000 # 1 million rows
  if (!disable_size_check && nrow(data) > MAX_ROWS_FOR_HISTOGRAM) {
    plotting_logger("WARN", "Dataset too large for histogram:", nrow(data), "rows >", MAX_ROWS_FOR_HISTOGRAM)
    return(ggplot() +
      annotate("text",
        x = 0.5, y = 0.5,
        label = sprintf("Dataset too large for histogram: %d rows. Consider using summarized data or filtering.", nrow(data)),
        hjust = 0.5, vjust = 0.5, size = 4, color = "red"
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      theme_void() +
      ggtitle("Dataset Too Large"))
  }

  tryCatch(
    {
      p <- ggplot(data, aes) +
        geom_histogram(binwidth = binwidth, fill = fill, alpha = a, position = position) +
        theme_minimal(base_size = baseSize)

      plotting_logger("DEBUG", "ggplot created successfully")

      if (split != "None") {
        plotting_logger("DEBUG", "Adding facet_grid for split:", split)
        p <- p + facet_grid(sym(split))
      }

      plotting_logger("DEBUG", "Histogram creation completed successfully")
      return(p)
    },
    error = function(e) {
      plotting_logger("ERROR", "Error creating histogram:", e$message)
      plotting_logger("ERROR", "Data size:", nrow(data), "rows")
      plotting_logger("ERROR", "Variable:", xinput)
      plotting_logger("ERROR", "Bin width:", binwidth)

      # Return error plot
      return(ggplot() +
        annotate("text",
          x = 0.5, y = 0.5,
          label = paste("Error creating histogram:", e$message),
          hjust = 0.5, vjust = 0.5, size = 4, color = "red"
        ) +
        xlim(0, 1) +
        ylim(0, 1) +
        theme_void() +
        ggtitle("Histogram Creation Error"))
    }
  )
}

#' Plot questionnaire data across phases
#' @param data Questionnaire results data frame (already filtered)
#' @param qType Questionnaire type (e.g., "IMI", "UserExperience")
#' @param cols_to_include Vector of column names to include in plot
#' @param baseSize Base font size
#' @param min_plot Minimum y-axis value (optional)
#' @param max_plot Maximum y-axis value (optional)
#' @return ggplot object
plot_questionnaire_data <- function(data, qType, cols_to_include = c(), baseSize = 10, min_plot = NULL, max_plot = NULL) {
  plotting_logger("DEBUG", "Starting plot_questionnaire_data function")
  plotting_logger("DEBUG", "Input parameters - qType:", qType, "cols_to_include:", paste(cols_to_include, collapse = ", "))

  # Only keep the columns to include in the plot
  if (length(cols_to_include) == 0) {
    cols_to_include <- setdiff(colnames(data), c("participant", "answer_type", "condition", "none"))
  }
  data <- data[, c("participant", "answer_type", "condition", cols_to_include), drop = FALSE]

  # Reshape the data to long format for ggplot
  data_long <- reshape2::melt(data, id.vars = c("participant", "answer_type", "condition"))

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

#' Create pie chart showing step categories
#' @param data Data frame with outlierSteps and suspect columns
#' @param extraTitle Additional title text
#' @param show_legend Whether to show legend
#' @param baseSize Base font size
#' @return ggplot object
make_pie_chart <- function(data, extraTitle = "", show_legend = TRUE, baseSize = 10) {
  plotting_logger("DEBUG", "Starting make_pie_chart function")
  
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

  plotting_logger("DEBUG", "make_pie_chart function completed successfully")
  return(p)
}

#' Create scatter plot with marginal density plots
#' @param data Data frame to plot
#' @param group Grouping variable
#' @param xplot X-axis variable name
#' @param yplot Y-axis variable name
#' @param show_legend Whether to show legend
#' @param baseSize Base font size
#' @return ggplot object with marginal plots
make_scatter_plot_steps <- function(data, group, xplot, yplot, show_legend = FALSE, baseSize = 10) {
  plotting_logger("DEBUG", "Starting make_scatter_plot_steps function")
  plotting_logger("DEBUG", sprintf("Input parameters - group: %s, xplot: %s, yplot: %s", group, xplot, yplot))
  
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

  both_contain_pos <- grepl("pos", xplot, ignore.case = TRUE) && grepl("pos", yplot, ignore.case = TRUE)
  if (both_contain_pos) {
    p <- p + coord_equal()
  }

  # Add marginal density plots
  p <- ggMarginal(p, type = "density", margins = "both", groupColour = TRUE, groupFill = TRUE)

  plotting_logger("DEBUG", "make_scatter_plot_steps function completed successfully")
  return(p)
}

#' Create correlation plot with statistical summary or heatmap
#' @param data Data frame to plot
#' @param x_var_name X-axis variable name
#' @param y_var_name Y-axis variable name
#' @param type Statistical test type ("parametric" or "nonparametric")
#' @param base_size Base font size
#' @param do_heatmap Whether to create a heatmap instead of scatter plot
#' @param heatmap_bins Number of bins for heatmap
#' @return ggplot object
plot_correlation_stats <- function(data, x_var_name, y_var_name, type = "parametric", base_size = 12, do_heatmap = FALSE, heatmap_bins = 30) {
  plotting_logger("DEBUG", "Starting plot_correlation_stats function")
  plotting_logger("DEBUG", sprintf("Input parameters - x_var: %s, y_var: %s, type: %s, do_heatmap: %s", x_var_name, y_var_name, type, do_heatmap))
  
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

  plotting_logger("DEBUG", "plot_correlation_stats function completed successfully")
  return(p)
}
