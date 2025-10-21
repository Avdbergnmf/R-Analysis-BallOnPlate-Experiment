#' Power Spectrum Plotting Functions
#'
#' Functions for visualizing power spectral density data

#' Plot power spectrum
#' @param ps_data Power spectrum data frame
#' @param stack Whether to stack the spectra
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_power_spectrum <- function(ps_data, stack = TRUE, split_by = NULL, base_size = 10) {
  
  if (is.null(ps_data) || nrow(ps_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Determine grouping variable (the one that's not frequency, power, or split_by)
  group_var <- setdiff(names(ps_data), c("frequency", "power", split_by))
  if (length(group_var) == 0) {
    group_var <- NULL
  } else {
    group_var <- group_var[1]
  }
  
  # Create base plot
  p <- ggplot(ps_data, aes(x = frequency, y = power))
  
  if (stack && !is.null(group_var)) {
    # Stacked area chart
    p <- p + geom_area(aes_string(fill = group_var), position = "stack", alpha = 0.7)
  } else if (!is.null(group_var)) {
    # Line plot with different colors for each group
    p <- p + geom_line(aes_string(color = group_var), size = 1.2)
  } else {
    # Simple line plot
    p <- p + geom_line(size = 1.2, color = "steelblue")
  }
  
  # Add faceting if split_by is specified
  if (!is.null(split_by) && split_by %in% names(ps_data)) {
    p <- p + facet_wrap(as.formula(paste("~", split_by)), ncol = 1, scales = "free_y")
  }
  
  # Styling
  p <- p +
    labs(
      x = "Frequency (Hz)",
      y = if (stack && !is.null(group_var)) "Stacked Power" else "Power",
      title = "Power Spectral Density"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      legend.title = element_blank()
    )
  
  return(p)
}

#' Plot power spectrum with log scale
#' @param ps_data Power spectrum data frame
#' @param log_x Whether to use log scale for x-axis
#' @param log_y Whether to use log scale for y-axis
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_power_spectrum_log <- function(ps_data, log_x = FALSE, log_y = TRUE, split_by = NULL, base_size = 10) {
  
  if (is.null(ps_data) || nrow(ps_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Determine grouping variable
  group_var <- setdiff(names(ps_data), c("frequency", "power", split_by))
  if (length(group_var) == 0) {
    group_var <- NULL
  } else {
    group_var <- group_var[1]
  }
  
  # Create base plot
  p <- ggplot(ps_data, aes(x = frequency, y = power))
  
  if (!is.null(group_var)) {
    p <- p + geom_line(aes_string(color = group_var), size = 1.2)
  } else {
    p <- p + geom_line(size = 1.2, color = "steelblue")
  }
  
  # Add faceting if split_by is specified
  if (!is.null(split_by) && split_by %in% names(ps_data)) {
    p <- p + facet_wrap(as.formula(paste("~", split_by)), ncol = 1, scales = "free_y")
  }
  
  # Apply log scales
  if (log_x) {
    p <- p + scale_x_log10()
  }
  if (log_y) {
    p <- p + scale_y_log10()
  }
  
  # Styling
  p <- p +
    labs(
      x = "Frequency (Hz)",
      y = "Power (log scale)",
      title = "Power Spectral Density (Log Scale)"
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      legend.title = element_blank()
    )
  
  return(p)
}

#' Plot power spectrum with confidence intervals
#' @param ps_data Power spectrum data frame with multiple trials
#' @param confidence_level Confidence level for intervals (default: 0.95)
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_power_spectrum_with_ci <- function(ps_data, confidence_level = 0.95, split_by = NULL, base_size = 10) {
  
  if (is.null(ps_data) || nrow(ps_data) == 0) {
    return(ggplot() + theme_void())
  }
  
  # Determine grouping variable
  group_var <- setdiff(names(ps_data), c("frequency", "power", split_by))
  if (length(group_var) == 0) {
    group_var <- NULL
  } else {
    group_var <- group_var[1]
  }
  
  # Calculate confidence intervals
  alpha <- 1 - confidence_level
  ci_data <- ps_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("frequency", group_var, split_by)))) %>%
    dplyr::summarize(
      mean_power = mean(power, na.rm = TRUE),
      se_power = sd(power, na.rm = TRUE) / sqrt(n()),
      ci_lower = mean_power - qt(1 - alpha/2, n() - 1) * se_power,
      ci_upper = mean_power + qt(1 - alpha/2, n() - 1) * se_power,
      .groups = "drop"
    )
  
  # Create base plot
  p <- ggplot(ci_data, aes(x = frequency, y = mean_power))
  
  # Add confidence intervals
  p <- p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = !!sym(group_var)), alpha = 0.3)
  
  # Add mean line
  if (!is.null(group_var)) {
    p <- p + geom_line(aes_string(color = group_var), size = 1.2)
  } else {
    p <- p + geom_line(size = 1.2, color = "steelblue")
  }
  
  # Add faceting if split_by is specified
  if (!is.null(split_by) && split_by %in% names(ci_data)) {
    p <- p + facet_wrap(as.formula(paste("~", split_by)), ncol = 1, scales = "free_y")
  }
  
  # Styling
  p <- p +
    labs(
      x = "Frequency (Hz)",
      y = "Power",
      title = paste0("Power Spectral Density (", confidence_level * 100, "% CI)")
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      legend.title = element_blank()
    )
  
  return(p)
}

#' Create power spectrum summary plot
#' @param ps_data Power spectrum data frame
#' @param plot_type Type of plot: "line", "area", "log", "ci"
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
create_psd_summary_plot <- function(ps_data, plot_type = "line", split_by = NULL, base_size = 10) {
  
  switch(plot_type,
    "line" = plot_power_spectrum(ps_data, stack = FALSE, split_by = split_by, base_size = base_size),
    "area" = plot_power_spectrum(ps_data, stack = TRUE, split_by = split_by, base_size = base_size),
    "log" = plot_power_spectrum_log(ps_data, split_by = split_by, base_size = base_size),
    "ci" = plot_power_spectrum_with_ci(ps_data, split_by = split_by, base_size = base_size),
    plot_power_spectrum(ps_data, stack = FALSE, split_by = split_by, base_size = base_size)  # Default
  )
}
