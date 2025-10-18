#' Plotting Utility Functions
#'
#' Utility functions for plotting configuration and styling.

# Create module-specific logger
plotting_logger <- create_module_logger("PLOTTING")

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
