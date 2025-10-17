#' Gait Statistics Functions
#'
#' Functions for calculating gait-related statistics

#' Calculate step statistics for gait data
#' @param data Gait data with step information
#' @param group_vars Variables to group by
#' @return Dataframe with step statistics
calculate_step_statistics <- function(data, group_vars = c("participant")) {
  # Group by specified variables and calculate the number of included and removed steps
  step_summary <- data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      total_steps = n(), # Use n() to count the number of rows
      included_steps = sum(!outlierSteps & !suspect, na.rm = TRUE),
      outlier_steps = sum(outlierSteps, na.rm = TRUE),
      suspect_steps = sum(suspect & !outlierSteps, na.rm = TRUE),
      .groups = "drop"
    )

  # Calculate the average and standard deviation for each metric
  total_steps_avg <- mean(step_summary$total_steps, na.rm = TRUE)
  total_steps_sd <- sd(step_summary$total_steps, na.rm = TRUE)

  included_steps_avg <- mean(step_summary$included_steps, na.rm = TRUE)
  included_steps_sd <- sd(step_summary$included_steps, na.rm = TRUE)

  outlier_steps_avg <- mean(step_summary$outlier_steps, na.rm = TRUE)
  outlier_steps_sd <- sd(step_summary$outlier_steps, na.rm = TRUE)

  suspect_steps_avg <- mean(step_summary$suspect_steps, na.rm = TRUE)
  suspect_steps_sd <- sd(step_summary$suspect_steps, na.rm = TRUE)

  # Create a result data frame
  result <- data.frame(
    Metric = c("Total Steps", "Included Steps", "Outlier Steps", "Suspect Steps"),
    Average = c(total_steps_avg, included_steps_avg, outlier_steps_avg, suspect_steps_avg),
    SD = c(total_steps_sd, included_steps_sd, outlier_steps_sd, suspect_steps_sd)
  )

  return(result)
}
