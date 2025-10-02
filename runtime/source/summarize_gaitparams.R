get_summ_by_foot <- function(dataType, categories, data, avg_feet = TRUE) {
  data <- data %>%
    group_by(across(all_of(categories))) %>%
    summarise(
      mean = mean(.data[[dataType]], na.rm = TRUE),
      sd = sd(.data[[dataType]], na.rm = TRUE),
      cv = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
      .groups = "drop"
    )

  if (avg_feet) {
    data <- data %>%
      group_by(across(all_of(setdiff(categories, "foot")))) %>%
      summarise(
        mean = mean(mean, na.rm = TRUE),
        sd = mean(sd, na.rm = TRUE),
        cv = mean(cv, na.rm = TRUE),
        .groups = "drop"
      )
  }

  return(data)
}

# Calculate complexity metrics for specified data types (combining both feet)
get_complexity_combined <- function(dataType, categories, data) {
  # Remove foot from categories since we want to combine feet
  categories_no_foot <- setdiff(categories, "foot")

  # Calculate complexity metrics for each group (combining both feet)
  complexity_data <- data %>%
    group_by(across(all_of(categories_no_foot))) %>%
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

# Average across feet and also calculate diff between left and right foot and add it to mu
average_over_feet <- function(data, types, categories, add_diff = FALSE) {
  # Get summaries averaging over feet
  categories_no_foot <- setdiff(categories, "foot")
  mu_avg <- lapply(types, get_summ_by_foot, categories_no_foot, data, avg_feet = TRUE)
  mu_avg <- setNames(mu_avg, types)

  if (add_diff) {
    # Keep foot in categories for per-foot summaries
    categories_with_foot <- categories

    # Step 1: Get per-foot summaries without averaging over feet
    mu_per_foot <- lapply(types, get_summ_by_foot, categories_with_foot, data, avg_feet = FALSE)
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
        select(-starts_with("mean_"), -starts_with("sd_"), -starts_with("cv_")) # Remove per-foot columns if not needed

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

# This table is huge (like 160 columns)
summarize_table <- function(data, categories, avg_feet = TRUE, add_diff = FALSE) { # note: add diff only works if also averaging over feet
  # Keep a copy with *all* columns so we can fetch meta info later
  orig_data <- data

  dataTypes <- setdiff(getTypes(data), categories)

  # Remove descriptive columns **after** saving orig_data so they are not included in numeric summaries
  data <- data %>% select(-all_of(columns_to_not_summarize))
  types <- setdiff(dataTypes, columns_to_not_summarize)

  if (avg_feet) {
    mu <- average_over_feet(data, types, categories, add_diff = add_diff)
    categories <- setdiff(categories, "foot") # remove foot from category list
  } else {
    # If not averaging over feet, compute mu normally
    mu <- lapply(types, get_summ_by_foot, categories, data, avg_feet = FALSE)
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

  # Retrieve the non-summarised category columns from **orig_data** (which still contains them)
  meta_cols <- c(categories, columns_to_not_summarize)
  meta <- orig_data %>%
    select(all_of(meta_cols)) %>%
    distinct()
  mu_wide <- mu_wide %>%
    left_join(meta, by = categories)

  # Reorder columns
  mu_wide <- mu_wide %>%
    select(all_of(categories), all_of(columns_to_not_summarize), everything())

  return(mu_wide)
}

get_full_mu <- function(allGaitParams, categories, avg_feet = TRUE, add_diff = FALSE) { # I could not get the optional feet averaging to work without having to pass it all the way down (would be nice to have some post-processing function that averages afterwards, optionally, but in the end went with this cumbersome road)
  # Get the summarized gait data (without questionnaire merging)
  muGait <- summarize_table(allGaitParams, c(categories, "foot"), avg_feet, add_diff) ##### Note that we add foot here as a category, to make sure we summarize each foot individually

  return(muGait)
}

#' Generic function to merge gait data with any other dataset
#' @param mu_gait Gait data (authoritative source for conditions)
#' @param other_data Other dataset to merge (questionnaire, task, complexity, etc.)
#' @param data_type_name Descriptive name for logging (e.g., "questionnaire", "task", "complexity")
#' @param merge_by Vector of column names to merge by (default: c("participant", "trialNum"))
#' @return Merged data frame with all data preserved (missing values filled with NA)
merge_mu_with_data <- function(mu_gait, other_data, data_type_name = "data", merge_by = c("participant", "trialNum")) {
  # Create copies to avoid modifying originals
  mu_gait_copy <- mu_gait
  other_data_copy <- other_data

  # Convert trialNum to numeric in both datasets for compatibility
  if ("trialNum" %in% merge_by) {
    mu_gait_copy$trialNum <- as.numeric(as.character(mu_gait_copy$trialNum))
    other_data_copy$trialNum <- as.numeric(as.character(other_data_copy$trialNum))
  }

  # Check for column conflicts - any column that exists in both datasets
  non_merge_cols <- setdiff(colnames(other_data_copy), merge_by)
  conflicting_cols <- intersect(non_merge_cols, colnames(mu_gait_copy))

  # For conflicting columns, handle them before the join to avoid .x/.y suffixes
  if (length(conflicting_cols) > 0) {
    cat(sprintf(
      "Handling conflicting columns (preferring original, filling missing from %s): %s\n",
      data_type_name, paste(conflicting_cols, collapse = ", ")
    ))

    # For each conflicting column, fill missing values in original data with values from new data
    for (col in conflicting_cols) {
      if (col %in% colnames(mu_gait_copy) && col %in% colnames(other_data_copy)) {
        # Fill missing values in original column with values from new data
        missing_mask <- is.na(mu_gait_copy[[col]]) | mu_gait_copy[[col]] == ""
        mu_gait_copy[[col]][missing_mask] <- other_data_copy[[col]][missing_mask]
      }
    }

    # Remove conflicting columns from other_data_copy since we've merged them into mu_gait_copy
    other_data_copy <- select(other_data_copy, -all_of(conflicting_cols))
  }

  # Add prefix to remaining columns (except for merge_by columns)
  remaining_cols <- setdiff(colnames(other_data_copy), merge_by)
  if (length(remaining_cols) > 0) {
    other_data_copy <- other_data_copy %>%
      rename_with(~ paste0(data_type_name, ".", .), all_of(remaining_cols))
  }

  # Merge based on specified columns (using full_join to preserve all data from both datasets)
  merged_data <- full_join(mu_gait_copy, other_data_copy, by = merge_by)

  return(merged_data)
}

#' Prepare questionnaire data for merging with gait data
#' @param questionnaire_data Raw questionnaire data
#' @return List with prepared_data and merge_by columns
prep_questionnaire_for_merge <- function(questionnaire_data) {
  if (!"answer_type" %in% colnames(questionnaire_data)) {
    stop("Questionnaire data must have 'answer_type' column")
  }

  # Create trial mapping for questionnaire results
  q_results_mapped <- questionnaire_data %>%
    mutate(
      trialNum = case_when(
        answer_type == "baseline_task" ~ 5L,
        answer_type == "retention" ~ 10L,
        answer_type == "training2" ~ 8L,
        answer_type == "transfer" ~ 11L,
        TRUE ~ as.integer(NA)
      )
    ) %>%
    dplyr::filter(!is.na(trialNum)) # Only keep questionnaire results that have a trial mapping

  return(q_results_mapped)
}

#' Filter dataset to only include participant/trial combinations that exist in gait data
#' @param mu_gait Gait data containing the reference participant/trial combinations
#' @param data_to_filter Dataset to filter (task, complexity, etc.)
#' @param data_type_name Descriptive name for error messages
#' @return Filtered dataset
filter_by_gait_combinations <- function(mu_gait, data_to_filter, data_type_name = "data") {
  # Handle empty or NULL input data
  if (is.null(data_to_filter) || nrow(data_to_filter) == 0) {
    stop(sprintf("No %s data to filter", data_type_name))
  }

  # Get unique participant/trial combinations from gait data
  gait_combinations <- mu_gait %>%
    select(participant, trialNum) %>%
    distinct()

  # Filter the dataset to only keep matching combinations
  filtered_data <- data_to_filter %>%
    semi_join(gait_combinations, by = c("participant", "trialNum"))

  return(filtered_data)
}

#' Summarize data across conditions
#' @param data Data frame to summarize
#' @return Summarized data frame
summarize_across_conditions <- function(data) {
  # Check that condition column exists - if not, something went wrong upstream
  if (!"condition" %in% colnames(data)) {
    stop("ERROR: 'condition' column is missing from data. This suggests an issue in the data merging process. Available columns: ", paste(colnames(data), collapse = ", "))
  }

  # Keep a copy with *all* columns so we can fetch demographic info later
  orig_data <- data

  # Determine base grouping columns based on data type
  base_cols <- if ("answer_type" %in% colnames(data)) {
    # Questionnaire data
    c("participant", "answer_type", "condition")
  } else {
    # Gait data - use existing condition column
    c("participant", "condition")
  }

  # Add foot to grouping if it exists
  grouping_cols <- if ("foot" %in% colnames(data)) {
    c(base_cols, "foot")
  } else {
    base_cols
  }

  # Single summarization step
  summarized_data <- data %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  # Add back non-summarizable columns (descriptive/demographic data that doesn't change per participant)
  if (exists("columns_to_not_summarize")) {
    available_meta_cols <- intersect(columns_to_not_summarize, colnames(orig_data))
    if (length(available_meta_cols) > 0) {
      meta_data <- orig_data %>%
        select(participant, all_of(available_meta_cols)) %>%
        distinct()

      summarized_data <- summarized_data %>%
        left_join(meta_data, by = "participant")
    }
  }

  return(summarized_data)
}

#' Summarize data across conditions with time slicing
#' @param data Data frame to summarize
#' @param slice_length Length of each time slice in seconds
#' @param remove_middle_slices Whether to keep only first and last slices
#' @return Summarized data frame
summarize_across_conditions_sliced <- function(data, slice_length = 30, remove_middle_slices = TRUE) {
  # Add time slices to data
  data_sliced <- data %>%
    group_by(participant, trialNum) %>%
    mutate(
      slice_index = ceiling(.data$simulation_time / slice_length)
    ) %>%
    ungroup()

  # Optionally remove middle slices
  if (remove_middle_slices) {
    data_sliced <- data_sliced %>%
      group_by(trialNum) %>%
      filter(slice_index == min(slice_index) | slice_index == max(slice_index)) %>%
      ungroup()
  }

  # Get number of samples per slice
  slice_samples <- data_sliced %>%
    group_by(trialNum, slice_index) %>%
    summarise(n_samples = n(), .groups = "drop")

  # Summarize data across conditions with time slices
  summarized_data <- data_sliced %>%
    group_by(across(all_of(c("participant", "condition", "trialNum", "slice_index")))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  return(summarized_data)
}
