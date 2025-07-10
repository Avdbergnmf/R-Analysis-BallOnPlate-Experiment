# =============================================================================
# IMPROVED GAIT PARAMETER SUMMARIZATION USING CATEGORY MANAGEMENT
# =============================================================================
# This file demonstrates how to use the new category management system
# for better organization and clarity in gait parameter analysis.

#' Get summary statistics by foot using improved category management
#' @param dataType Type of data to summarize
#' @param data Data frame containing the data
#' @param avg_feet Whether to average over feet
#' @return Summarized data frame
get_summ_by_foot_improved <- function(dataType, data, avg_feet = TRUE) {
  # Use category management system to get appropriate categories
  if (avg_feet) {
    # For averaging over feet, exclude foot from grouping
    categories <- get_gait_analysis_categories(include_foot = FALSE)
  } else {
    # For per-foot analysis, include foot in grouping
    categories <- get_gait_analysis_categories(include_foot = TRUE)
  }
  
  # Validate that required categories exist
  if (!has_required_categories(data, "gait")) {
    missing_cats <- get_missing_categories(data, "gait")
    stop("Missing required categories for gait analysis: ", paste(missing_cats, collapse = ", "))
  }
  
  # Get existing categories (in case some are missing)
  existing_categories <- get_existing_categories(data, categories)
  
  data <- data %>%
    group_by(across(all_of(existing_categories))) %>%
    summarise(
      mean = mean(.data[[dataType]], na.rm = TRUE),
      sd = sd(.data[[dataType]], na.rm = TRUE),
      cv = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
      .groups = "drop"
    )

  if (avg_feet && "foot" %in% colnames(data)) {
    # Remove foot from grouping for averaging
    categories_no_foot <- setdiff(existing_categories, "foot")
    
    data <- data %>%
      group_by(across(all_of(categories_no_foot))) %>%
      summarise(
        mean = mean(mean, na.rm = TRUE),
        sd = mean(sd, na.rm = TRUE),
        cv = mean(cv, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(data)
}

#' Calculate complexity metrics with improved category management
#' @param dataType Type of data to analyze
#' @param data Data frame containing the data
#' @return Complexity analysis results
get_complexity_combined_improved <- function(dataType, data) {
  # Get categories for gait analysis without foot (since we combine feet)
  categories <- get_gait_analysis_categories(include_foot = FALSE)
  
  # Validate categories
  if (!has_required_categories(data, "gait")) {
    missing_cats <- get_missing_categories(data, "gait")
    stop("Missing required categories for gait analysis: ", paste(missing_cats, collapse = ", "))
  }
  
  # Get existing categories
  existing_categories <- get_existing_categories(data, categories)

  # Calculate complexity metrics for each group (combining both feet)
  complexity_data <- data %>%
    group_by(across(all_of(existing_categories))) %>%
    group_modify(~ {
      # Filter out outliers if outlierSteps column exists
      values <- .x[[dataType]]
      if ("outlierSteps" %in% colnames(.x)) {
        values <- values[!.x$outlierSteps]
      }

      # Calculate complexity metrics on combined data from both feet
      complexity_result <- compute_complexity(values) # assumes complexity.R is loaded

      # Convert to data frame
      as.data.frame(complexity_result)
    }) %>%
    ungroup()

  return(complexity_data)
}

#' Average across feet with improved category management
#' @param data Data frame containing the data
#' @param types Types of data to analyze
#' @param add_diff Whether to add foot difference calculations
#' @return List of summarized data frames
average_over_feet_improved <- function(data, types, add_diff = FALSE) {
  # Get categories for gait analysis
  categories_no_foot <- get_gait_analysis_categories(include_foot = FALSE)
  categories_with_foot <- get_gait_analysis_categories(include_foot = TRUE)
  
  # Validate categories
  if (!has_required_categories(data, "gait")) {
    missing_cats <- get_missing_categories(data, "gait")
    stop("Missing required categories for gait analysis: ", paste(missing_cats, collapse = ", "))
  }
  
  # Get summaries averaging over feet
  mu_avg <- lapply(types, function(type) {
    get_summ_by_foot_improved(type, data, avg_feet = TRUE)
  })
  mu_avg <- setNames(mu_avg, types)

  if (add_diff) {
    # Step 1: Get per-foot summaries without averaging over feet
    mu_per_foot <- lapply(types, function(type) {
      get_summ_by_foot_improved(type, data, avg_feet = FALSE)
    })
    mu_per_foot <- setNames(mu_per_foot, types)

    # Step 2: Calculate the differences between left and right foot measurements
    mu_diff <- lapply(mu_per_foot, function(df) {
      # Pivot the data wider to have separate columns for left and right foot measurements
      df_wide <- df %>%
        pivot_wider(names_from = foot, values_from = c(mean, sd, cv))

      # Calculate the difference between left and right foot measurements
      df_wide <- df_wide %>%
        mutate(
          diffFeet_mean = mean_Left - mean_Right,
          diffFeet_sd = sd_Left - sd_Right,
          diffFeet_cv = cv_Left - cv_Right
        ) %>%
        select(-starts_with("mean_"), -starts_with("sd_"), -starts_with("cv_"))

      return(df_wide)
    })
    mu_diff <- setNames(mu_diff, types)

    # Step 3: Add diff columns to mu with averaged feet
    mu <- mapply(function(avg_df, diff_df) {
      # Merge the averaged data with the diff data on categories_no_foot
      merged_df <- avg_df %>%
        left_join(diff_df, by = categories_no_foot)
      return(merged_df)
    }, mu_avg, mu_diff, SIMPLIFY = FALSE)
    mu <- setNames(mu, types)

    return(mu)
  } else {
    return(mu_avg)
  }
}

#' Summarize table with improved category management
#' @param data Data frame to summarize
#' @param avg_feet Whether to average over feet
#' @param add_diff Whether to add foot difference calculations
#' @return Summarized data frame
summarize_table_improved <- function(data, avg_feet = TRUE, add_diff = FALSE) {
  # Get categories for gait analysis
  categories <- get_gait_analysis_categories(include_foot = avg_feet)
  
  # Get data types that should be summarized (exclude categories)
  dataTypes <- get_summarizable_columns(data, categories)
  
  # Get descriptive columns that should not be summarized
  descriptive_cols <- get_descriptive_columns()
  
  # Remove descriptive columns from data and types
  data <- data %>% select(-all_of(descriptive_cols))
  types <- setdiff(dataTypes, descriptive_cols)

  if (avg_feet) {
    mu <- average_over_feet_improved(data, types, add_diff = add_diff)
  } else {
    # If not averaging over feet, compute mu normally
    mu <- lapply(types, function(type) {
      get_summ_by_foot_improved(type, data, avg_feet = FALSE)
    })
    mu <- setNames(mu, types)
  }

  # Combine the list of data frames into one data frame
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols = -c(all_of(categories), dataType),
      names_to = "statistic",
      values_to = "value"
    )

  # Create new column names and pivot to a wider format
  mu_wide <- mu_long %>%
    unite("new_col_name", dataType, statistic, sep = ".") %>%
    pivot_wider(
      names_from = new_col_name,
      values_from = value
    )

  # Add category columns back
  mu_wide <- add_category_columns(mu_wide)

  # Reorder columns using category management
  summary_categories <- get_summary_table_categories()
  existing_summary_categories <- get_existing_categories(mu_wide, summary_categories)
  
  mu_wide <- mu_wide %>%
    select(all_of(existing_summary_categories), everything())

  return(mu_wide)
}

#' Get full summary with improved category management
#' @param allGaitParams Gait parameters data
#' @param avg_feet Whether to average over feet
#' @param add_diff Whether to add foot difference calculations
#' @return Full summary data frame
get_full_mu_improved <- function(allGaitParams, avg_feet = TRUE, add_diff = FALSE) {
  # Validate that we have the required categories for gait analysis
  if (!has_required_categories(allGaitParams, "gait")) {
    missing_cats <- get_missing_categories(allGaitParams, "gait")
    warning("Missing categories for gait analysis: ", paste(missing_cats, collapse = ", "))
  }
  
  # Get categories for gait analysis (include foot for per-foot summaries)
  categories <- get_gait_analysis_categories(include_foot = TRUE)
  
  # Summarize the data
  muGait <- summarize_table_improved(allGaitParams, avg_feet, add_diff)
  
  return(muGait)
}

#' Get full summary for time-sliced data with improved category management
#' @param allGaitParams Gait parameters data
#' @param allQResults Questionnaire results
#' @param slice_length Length of time slices
#' @param avg_feet Whether to average over feet
#' @param add_diff Whether to add foot difference calculations
#' @param remove_middle_slices Whether to remove middle slices
#' @return Full summary data frame for time-sliced data
get_full_mu_sliced_improved <- function(allGaitParams, allQResults, slice_length, 
                                      avg_feet = TRUE, add_diff = FALSE, 
                                      remove_middle_slices = FALSE) {
  # Validate that we have the required categories for sliced analysis
  if (!has_required_categories(allGaitParams, "sliced")) {
    missing_cats <- get_missing_categories(allGaitParams, "sliced")
    warning("Missing categories for sliced analysis: ", paste(missing_cats, collapse = ", "))
  }
  
  # Get categories for sliced analysis (include slice_index for per-slice summaries)
  categories <- get_sliced_analysis_categories(include_slice = TRUE)
  
  # Summarize the sliced data
  muGait <- summarize_table_sliced_improved(
    allGaitParams, allQResults, categories, slice_length, 
    avg_feet = avg_feet, add_diff = add_diff, 
    remove_middle_slices = remove_middle_slices
  )
  
  return(muGait)
}

#' Summarize time-sliced data with improved category management
#' @param data Data frame to summarize
#' @param allQResults Questionnaire results
#' @param categories Categories to use for grouping
#' @param slice_length Length of time slices
#' @param time_col Name of time column
#' @param avg_feet Whether to average over feet
#' @param add_diff Whether to add foot difference calculations
#' @param remove_middle_slices Whether to remove middle slices
#' @return Summarized time-sliced data
summarize_table_sliced_improved <- function(data, allQResults, categories, slice_length, 
                                          time_col = "time", avg_feet = TRUE, 
                                          add_diff = FALSE, remove_middle_slices = FALSE) {
  # Get data types that should be summarized
  dataTypes <- get_summarizable_columns(data, categories)
  
  # Get descriptive columns
  descriptive_cols <- get_descriptive_columns()
  
  # Remove descriptive columns from data and types
  data <- data %>% select(-all_of(descriptive_cols))
  types <- setdiff(dataTypes, descriptive_cols)

  # Create time slices
  data <- data %>%
    mutate(slice_index = floor(time / slice_length))

  if (remove_middle_slices) {
    # Remove middle slices (keep first and last)
    slice_counts <- data %>%
      group_by(participant, trialNum) %>%
      summarise(
        min_slice = min(slice_index, na.rm = TRUE),
        max_slice = max(slice_index, na.rm = TRUE),
        .groups = "drop"
      )
    
    data <- data %>%
      left_join(slice_counts, by = c("participant", "trialNum")) %>%
      filter(slice_index == min_slice | slice_index == max_slice) %>%
      select(-min_slice, -max_slice)
  }

  # Group by categories and slice index
  grouping_cols <- c(categories, "slice_index")
  existing_grouping_cols <- get_existing_categories(data, grouping_cols)

  if (avg_feet) {
    mu <- average_over_feet_improved(data, types, add_diff = add_diff)
    categories <- setdiff(categories, "foot") # remove foot from category list
  } else {
    # If not averaging over feet, compute mu normally
    mu <- lapply(types, function(type) {
      get_summ_by_foot_improved(type, data, avg_feet = FALSE)
    })
    mu <- setNames(mu, types)
  }

  # Combine the list of data frames into one data frame
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols = -c(all_of(existing_grouping_cols), dataType),
      names_to = "statistic",
      values_to = "value"
    )

  # Create new column names and pivot to a wider format
  mu_wide <- mu_long %>%
    unite("new_col_name", dataType, statistic, sep = ".") %>%
    pivot_wider(
      names_from = new_col_name,
      values_from = value
    )

  # Add category columns back
  mu_wide <- add_category_columns(mu_wide)

  # Reorder columns
  summary_categories <- get_summary_table_categories()
  existing_summary_categories <- get_existing_categories(mu_wide, summary_categories)
  
  mu_wide <- mu_wide %>%
    select(all_of(existing_summary_categories), everything())

  return(mu_wide)
}