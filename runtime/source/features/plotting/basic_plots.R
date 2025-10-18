#' Basic Plotting Functions
#'
#' Core plotting functions for creating 2D plots and error plots.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

#' Create error plot with message
#' @param message Error message to display
#' @param baseSize Base font size
#' @return ggplot object with error message
create_error_plot <- function(message, baseSize = 10) {
  plotting_logger("WARN", sprintf("Creating error plot: %s", message))
  
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = message, size = baseSize, hjust = 0.5, vjust = 0.5) +
    xlim(0, 1) + ylim(0, 1) +
    theme_void() +
    theme(plot.background = element_rect(fill = "lightgray", color = "red", size = 2))
  
  return(p)
}

#' Validate data and columns for plotting
#' @param xData X-axis data
#' @param yData Y-axis data
#' @param xtracker X tracker name
#' @param ytracker Y tracker name
#' @param x_axis X-axis column name
#' @param y_axis Y-axis column name
#' @param participant Participant ID
#' @param trialNum Trial number
#' @param baseSize Base font size
#' @return Error plot if validation fails, NULL if validation passes
validate_data_and_columns <- function(xData, yData, xtracker, ytracker, x_axis, y_axis, participant, trialNum, baseSize) {
  # Check if data is empty
  if (is.null(xData) || nrow(xData) == 0) {
    return(create_error_plot(paste("No data available for", xtracker, "in participant", participant, "trial", trialNum), baseSize))
  }

  if (is.null(yData) || nrow(yData) == 0) {
    return(create_error_plot(paste("No data available for", ytracker, "in participant", participant, "trial", trialNum), baseSize))
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

#' Create 2D plot of tracker data
#' @param xData X-axis tracker data
#' @param yData Y-axis tracker data
#' @param xtracker X-axis tracker name
#' @param ytracker Y-axis tracker name
#' @param participant Participant identifier
#' @param trialNum Trial number
#' @param x_axis X-axis column name
#' @param y_axis Y-axis column name
#' @param plotlines Whether to plot lines
#' @param plotpoints Whether to plot points
#' @param extraTitle Additional title text
#' @param baseSize Base font size
#' @return ggplot object
plot_2d <- function(xData, yData, xtracker, ytracker, participant, trialNum, x_axis = "time", y_axis = "pos_z", plotlines = TRUE, plotpoints = FALSE, extraTitle = "", baseSize = 10) {
  plotting_logger("DEBUG", "Starting plot_2d function")
  plotting_logger("DEBUG", sprintf("Input parameters - xtracker: %s ytracker: %s participant: %s trialNum: %s x_axis: %s y_axis: %s", xtracker, ytracker, participant, trialNum, x_axis, y_axis))

  # Preprocess data for both trackers
  xData <- preprocess_tracker_data(xData, participant, trialNum, xtracker)
  yData <- preprocess_tracker_data(yData, participant, trialNum, ytracker)

  # Validate data and columns - return error plot if validation fails
  error_plot <- validate_data_and_columns(xData, yData, xtracker, ytracker, x_axis, y_axis, participant, trialNum, baseSize)
  if (!is.null(error_plot)) {
    return(error_plot)
  }

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

  plotting_logger("DEBUG", "plot_2d function completed successfully")
  return(p)
}
