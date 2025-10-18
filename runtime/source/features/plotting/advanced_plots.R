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
