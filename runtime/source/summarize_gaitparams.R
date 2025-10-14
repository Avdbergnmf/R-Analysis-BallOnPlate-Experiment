# Load logging utilities
source("logging_utils.R")

# Create module-specific logger
summarize_logger <- create_module_logger("SUMMARIZE-GAITPARAMS")

get_summ_by_foot <- function(dataType, categories, data, avg_feet = TRUE) {
  summarize_logger("DEBUG", "Starting get_summ_by_foot for dataType:", dataType, "with categories:", paste(categories, collapse = ", "))
  summarize_logger("DEBUG", "Input data dimensions:", nrow(data), "x", ncol(data))
  summarize_logger("DEBUG", "Averaging over feet:", avg_feet)
  
  # Check if dataType exists in data
  if (!dataType %in% colnames(data)) {
    summarize_logger("ERROR", "dataType", dataType, "not found in data columns:", paste(colnames(data), collapse = ", "))
    stop("dataType '", dataType, "' not found in data")
  }
  
  # Check if all categories exist in data
  missing_categories <- setdiff(categories, colnames(data))
  if (length(missing_categories) > 0) {
    summarize_logger("ERROR", "Missing categories in data:", paste(missing_categories, collapse = ", "))
    stop("Missing categories: ", paste(missing_categories, collapse = ", "))
  }
  
  data <- data %>%
    group_by(across(all_of(categories))) %>%
    summarise(
      mean = mean(.data[[dataType]], na.rm = TRUE),
      sd = sd(.data[[dataType]], na.rm = TRUE),
      cv = sd(.data[[dataType]], na.rm = TRUE) / mean(.data[[dataType]], na.rm = TRUE),
      .groups = "drop"
    )
  
  summarize_logger("DEBUG", "After initial summarization:", nrow(data), "groups")

  if (avg_feet) {
    summarize_logger("DEBUG", "Averaging over feet - removing 'foot' from categories")
    data <- data %>%
      group_by(across(all_of(setdiff(categories, "foot")))) %>%
      summarise(
        mean = mean(mean, na.rm = TRUE),
        sd = mean(sd, na.rm = TRUE),
        cv = mean(cv, na.rm = TRUE),
        .groups = "drop"
      )
    summarize_logger("DEBUG", "After averaging over feet:", nrow(data), "groups")
  }

  summarize_logger("DEBUG", "get_summ_by_foot completed for", dataType)
  return(data)
}

# Calculate complexity metrics for specified data types (combining both feet)
get_complexity_combined <- function(dataType, categories, data) {
  summarize_logger("DEBUG", "Starting get_complexity_combined for dataType:", dataType)
  summarize_logger("DEBUG", "Input data dimensions:", nrow(data), "x", ncol(data))
  summarize_logger("DEBUG", "Original categories:", paste(categories, collapse = ", "))
  
  # Remove foot from categories since we want to combine feet
  categories_no_foot <- setdiff(categories, "foot")
  summarize_logger("DEBUG", "Categories without foot:", paste(categories_no_foot, collapse = ", "))
  
  # Check if dataType exists in data
  if (!dataType %in% colnames(data)) {
    summarize_logger("ERROR", "dataType", dataType, "not found in data columns:", paste(colnames(data), collapse = ", "))
    stop("dataType '", dataType, "' not found in data")
  }
  
  # Check if all categories exist in data
  missing_categories <- setdiff(categories_no_foot, colnames(data))
  if (length(missing_categories) > 0) {
    summarize_logger("ERROR", "Missing categories in data:", paste(missing_categories, collapse = ", "))
    stop("Missing categories: ", paste(missing_categories, collapse = ", "))
  }
  
  # Check if compute_complexity function is available
  if (!exists("compute_complexity")) {
    summarize_logger("ERROR", "compute_complexity function not found. Make sure complexity.R is loaded.")
    stop("compute_complexity function not found")
  }

  # Calculate complexity metrics for each group (combining both feet)
  summarize_logger("DEBUG", "Starting complexity calculation for", length(unique(interaction(data[categories_no_foot]))), "groups")
  
  complexity_data <- data %>%
    group_by(across(all_of(categories_no_foot))) %>%
    group_modify(~ {
      # Filter out outliers if outlierSteps column exists
      values <- .x[[dataType]]
      original_count <- length(values)
      
      if ("outlierSteps" %in% colnames(.x)) {
        values <- values[!.x$outlierSteps]
        outlier_count <- original_count - length(values)
        if (outlier_count > 0) {
          summarize_logger("DEBUG", "Filtered out", outlier_count, "outliers for group")
        }
      }

      # Calculate complexity metrics on combined data from both feet
      complexity_result <- compute_complexity(values) # assumes complexity.R is loaded

      # Convert to data frame
      as.data.frame(complexity_result)
    }) %>%
    ungroup()
  
  summarize_logger("DEBUG", "Complexity calculation completed. Result dimensions:", nrow(complexity_data), "x", ncol(complexity_data))
  summarize_logger("DEBUG", "get_complexity_combined completed for", dataType)
  
  return(complexity_data)
}

# Average across feet and also calculate diff between left and right foot and add it to mu
average_over_feet <- function(data, types, categories, add_diff = FALSE) {
  summarize_logger("DEBUG", "Starting average_over_feet with", length(types), "data types")
  summarize_logger("DEBUG", "Data types:", paste(types, collapse = ", "))
  summarize_logger("DEBUG", "Categories:", paste(categories, collapse = ", "))
  summarize_logger("DEBUG", "Add difference calculation:", add_diff)
  summarize_logger("DEBUG", "Input data dimensions:", nrow(data), "x", ncol(data))
  
  # Get summaries averaging over feet
  categories_no_foot <- setdiff(categories, "foot")
  summarize_logger("DEBUG", "Categories without foot:", paste(categories_no_foot, collapse = ", "))
  
  summarize_logger("INFO", "Calculating averaged summaries for", length(types), "data types")
  mu_avg <- lapply(types, get_summ_by_foot, categories_no_foot, data, avg_feet = TRUE)
  mu_avg <- setNames(mu_avg, types)
  summarize_logger("DEBUG", "Averaged summaries completed")

  if (add_diff) {
    summarize_logger("INFO", "Calculating foot differences")
    # Keep foot in categories for per-foot summaries
    categories_with_foot <- categories

    # Step 1: Get per-foot summaries without averaging over feet
    summarize_logger("DEBUG", "Getting per-foot summaries")
    mu_per_foot <- lapply(types, get_summ_by_foot, categories_with_foot, data, avg_feet = FALSE)
    mu_per_foot <- setNames(mu_per_foot, types)

    # Step 2: Calculate the differences between left and right foot measurements
    summarize_logger("DEBUG", "Calculating left-right foot differences")
    mu_diff <- lapply(mu_per_foot, function(df) {
      # Check if foot column exists and has both Left and Right values
      if (!"foot" %in% colnames(df)) {
        summarize_logger("WARN", "No 'foot' column found in per-foot summary data")
        return(df)
      }
      
      foot_values <- unique(df$foot)
      if (!all(c("Left", "Right") %in% foot_values)) {
        summarize_logger("WARN", "Missing Left or Right foot data. Available:", paste(foot_values, collapse = ", "))
        return(df)
      }
      
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
    summarize_logger("DEBUG", "Foot differences calculated")

    # Step 3: Add diff columns to mu with averaged feet
    summarize_logger("DEBUG", "Merging averaged data with foot differences")
    mu <- mapply(function(avg_df, diff_df) {
      # Merge the averaged data with the diff data on categories_no_foot
      merged_df <- avg_df %>%
        left_join(diff_df, by = categories_no_foot)
      return(merged_df)
    }, mu_avg, mu_diff, SIMPLIFY = FALSE)
    mu <- setNames(mu, types)
    summarize_logger("DEBUG", "Merging completed")

    summarize_logger("DEBUG", "average_over_feet completed with differences")
    return(mu)
  } else {
    summarize_logger("DEBUG", "average_over_feet completed without differences")
    return(mu_avg)
  }
}

# This table is huge (like 160 columns)
summarize_table <- function(data, categories, avg_feet = TRUE, add_diff = FALSE) { # note: add diff only works if also averaging over feet
  log_operation_start(summarize_logger, "SUMMARIZE_TABLE")
  summarize_logger("INFO", "Starting summarize_table")
  summarize_logger("DEBUG", "Input data dimensions:", nrow(data), "x", ncol(data))
  summarize_logger("DEBUG", "Categories:", paste(categories, collapse = ", "))
  summarize_logger("DEBUG", "Average over feet:", avg_feet)
  summarize_logger("DEBUG", "Add differences:", add_diff)
  
  # Keep a copy with *all* columns so we can fetch meta info later
  orig_data <- data
  summarize_logger("DEBUG", "Original data preserved for metadata extraction")

  # Check if getTypes function exists
  if (!exists("getTypes")) {
    summarize_logger("ERROR", "getTypes function not found. Make sure required dependencies are loaded.")
    stop("getTypes function not found")
  }
  
  dataTypes <- setdiff(getTypes(data), categories)
  summarize_logger("DEBUG", "Found", length(dataTypes), "data types to summarize")
  summarize_logger("DEBUG", "Data types:", paste(dataTypes, collapse = ", "))

  # Check if columns_to_not_summarize exists
  if (!exists("columns_to_not_summarize")) {
    summarize_logger("WARN", "columns_to_not_summarize not defined, using empty vector")
    columns_to_not_summarize <- character(0)
  } else {
    summarize_logger("DEBUG", "Columns to not summarize:", paste(columns_to_not_summarize, collapse = ", "))
  }

  # Remove descriptive columns **after** saving orig_data so they are not included in numeric summaries
  data <- data %>% select(-all_of(columns_to_not_summarize))
  types <- setdiff(dataTypes, columns_to_not_summarize)
  summarize_logger("DEBUG", "Final types to summarize:", length(types))
  summarize_logger("DEBUG", "Types:", paste(types, collapse = ", "))

  if (avg_feet) {
    summarize_logger("INFO", "Averaging over feet")
    mu <- average_over_feet(data, types, categories, add_diff = add_diff)
    categories <- setdiff(categories, "foot") # remove foot from category list
    summarize_logger("DEBUG", "Categories after removing foot:", paste(categories, collapse = ", "))
  } else {
    summarize_logger("INFO", "Computing summaries without averaging over feet")
    # If not averaging over feet, compute mu normally
    mu <- lapply(types, get_summ_by_foot, categories, data, avg_feet = FALSE)
    mu <- setNames(mu, types)
  }
  summarize_logger("DEBUG", "Summary calculations completed for", length(mu), "data types")

  # Combine the list of data frames into one data frame
  summarize_logger("DEBUG", "Combining summary data frames")
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols = -c(all_of(categories), dataType),
      names_to = "statistic",
      values_to = "value"
    )
  summarize_logger("DEBUG", "Long format dimensions:", nrow(mu_long), "x", ncol(mu_long))

  # Create new column names and pivot to a wider format
  summarize_logger("DEBUG", "Converting to wide format")
  mu_wide <- mu_long %>%
    unite("new_col_name", dataType, statistic, sep = ".") %>%
    pivot_wider(
      names_from = new_col_name,
      values_from = value
    )
  summarize_logger("DEBUG", "Wide format dimensions:", nrow(mu_wide), "x", ncol(mu_wide))

  # Retrieve the non-summarised category columns from **orig_data** (which still contains them)
  meta_cols <- c(categories, columns_to_not_summarize)
  available_meta_cols <- intersect(meta_cols, colnames(orig_data))
  summarize_logger("DEBUG", "Available metadata columns:", paste(available_meta_cols, collapse = ", "))
  
  if (length(available_meta_cols) > 0) {
    meta <- orig_data %>%
      select(all_of(available_meta_cols)) %>%
      distinct()
    summarize_logger("DEBUG", "Metadata dimensions:", nrow(meta), "x", ncol(meta))
    
    mu_wide <- mu_wide %>%
      left_join(meta, by = categories)
    summarize_logger("DEBUG", "Metadata joined successfully")
  } else {
    summarize_logger("WARN", "No metadata columns available for joining")
  }

  # Reorder columns
  summarize_logger("DEBUG", "Reordering columns")
  mu_wide <- mu_wide %>%
    select(all_of(categories), all_of(available_meta_cols), everything())
  
  final_dims <- c(nrow(mu_wide), ncol(mu_wide))
  summarize_logger("INFO", "Final table dimensions:", final_dims[1], "x", final_dims[2])
  log_operation_end(summarize_logger, "SUMMARIZE_TABLE", success = TRUE)

  return(mu_wide)
}

get_full_mu <- function(allGaitParams, categories, avg_feet = TRUE, add_diff = FALSE) { # I could not get the optional feet averaging to work without having to pass it all the way down (would be nice to have some post-processing function that averages afterwards, optionally, but in the end went with this cumbersome road)
  summarize_logger("DEBUG", "Starting get_full_mu")
  summarize_logger("DEBUG", "Input gait params dimensions:", nrow(allGaitParams), "x", ncol(allGaitParams))
  summarize_logger("DEBUG", "Categories:", paste(categories, collapse = ", "))
  summarize_logger("DEBUG", "Average over feet:", avg_feet, "Add differences:", add_diff)
  
  # Get the summarized gait data (without questionnaire merging)
  muGait <- summarize_table(allGaitParams, c(categories, "foot"), avg_feet, add_diff) ##### Note that we add foot here as a category, to make sure we summarize each foot individually
  
  summarize_logger("DEBUG", "get_full_mu completed. Result dimensions:", nrow(muGait), "x", ncol(muGait))
  return(muGait)
}

#' Generic function to merge gait data with any other dataset
#' @param mu_gait Gait data (authoritative source for conditions)
#' @param other_data Other dataset to merge (questionnaire, task, complexity, etc.)
#' @param data_type_name Descriptive name for logging (e.g., "questionnaire", "task", "complexity")
#' @param merge_by Vector of column names to merge by (default: c("participant", "trialNum"))
#' @return Merged data frame with all data preserved (missing values filled with NA)
merge_mu_with_data <- function(mu_gait, other_data, data_type_name = "data", merge_by = c("participant", "trialNum")) {
  log_operation_start(summarize_logger, paste("MERGE_MU_WITH", toupper(data_type_name)))
  summarize_logger("INFO", "Starting merge_mu_with_data for", data_type_name)
  summarize_logger("DEBUG", "Gait data dimensions:", nrow(mu_gait), "x", ncol(mu_gait))
  summarize_logger("DEBUG", "Other data dimensions:", nrow(other_data), "x", ncol(other_data))
  summarize_logger("DEBUG", "Merge by columns:", paste(merge_by, collapse = ", "))
  
  # Validate input data
  if (is.null(mu_gait) || nrow(mu_gait) == 0) {
    summarize_logger("ERROR", "Gait data is empty or NULL")
    stop("Gait data is empty or NULL")
  }
  
  if (is.null(other_data) || nrow(other_data) == 0) {
    summarize_logger("WARN", "Other data is empty or NULL, returning gait data only")
    log_operation_end(summarize_logger, paste("MERGE_MU_WITH", toupper(data_type_name)), success = TRUE)
    return(mu_gait)
  }
  
  # Create copies to avoid modifying originals
  mu_gait_copy <- mu_gait
  other_data_copy <- other_data

  # Convert trialNum to numeric in both datasets for compatibility
  if ("trialNum" %in% merge_by) {
    summarize_logger("DEBUG", "Converting trialNum to numeric for compatibility")
    mu_gait_copy$trialNum <- as.numeric(as.character(mu_gait_copy$trialNum))
    other_data_copy$trialNum <- as.numeric(as.character(other_data_copy$trialNum))
  }

  # Check for column conflicts - any column that exists in both datasets
  non_merge_cols <- setdiff(colnames(other_data_copy), merge_by)
  conflicting_cols <- intersect(non_merge_cols, colnames(mu_gait_copy))
  
  summarize_logger("DEBUG", "Non-merge columns in other data:", length(non_merge_cols))
  summarize_logger("DEBUG", "Conflicting columns:", length(conflicting_cols))
  if (length(conflicting_cols) > 0) {
    summarize_logger("DEBUG", "Conflicting columns:", paste(conflicting_cols, collapse = ", "))
  }

  # Add prefix to non-conflicting columns (except for merge_by columns)
  non_conflicting_cols <- setdiff(non_merge_cols, conflicting_cols)
  if (length(non_conflicting_cols) > 0) {
    summarize_logger("DEBUG", "Adding prefix to", length(non_conflicting_cols), "non-conflicting columns")
    other_data_copy <- other_data_copy %>%
      rename_with(~ paste0(data_type_name, ".", .), all_of(non_conflicting_cols))
  }

  # Merge based on specified columns (using full_join to preserve all data from both datasets)
  summarize_logger("DEBUG", "Performing full join on columns:", paste(merge_by, collapse = ", "))
  merged_data <- full_join(mu_gait_copy, other_data_copy, by = merge_by)
  summarize_logger("DEBUG", "Merged data dimensions:", nrow(merged_data), "x", ncol(merged_data))

  # For conflicting columns, use coalesce to prefer mu_gait values but fill missing ones from other_data
  if (length(conflicting_cols) > 0) {
    summarize_logger("INFO", sprintf(
      "Merging conflicting columns (preferring mu_gait, filling missing from %s): %s",
      data_type_name, paste(conflicting_cols, collapse = ", ")
    ))

    # Use coalesce for each conflicting column (prefers mu_gait, falls back to other_data)
    for (col in conflicting_cols) {
      # After full_join, conflicting columns get .x and .y suffixes
      mu_gait_col <- paste0(col, ".x") # mu_gait values
      other_data_col <- paste0(col, ".y") # other_data values

      if (mu_gait_col %in% colnames(merged_data) && other_data_col %in% colnames(merged_data)) {
        summarize_logger("DEBUG", "Resolving conflict for column:", col)
        # Use coalesce to prefer mu_gait values, fill missing with other_data values
        merged_data[[col]] <- coalesce(merged_data[[mu_gait_col]], merged_data[[other_data_col]])

        # Remove the .x and .y columns since we've merged them
        merged_data <- select(merged_data, -all_of(c(mu_gait_col, other_data_col)))
      }
    }

    # Reorder columns to put conflicting columns right after merge_by columns
    all_cols <- colnames(merged_data)
    merge_by_cols <- intersect(merge_by, all_cols)
    conflicting_cols_present <- intersect(conflicting_cols, all_cols)
    other_cols <- setdiff(all_cols, c(merge_by_cols, conflicting_cols_present))

    # Create new column order: merge_by, conflicting, then everything else
    new_col_order <- c(merge_by_cols, conflicting_cols_present, other_cols)
    merged_data <- select(merged_data, all_of(new_col_order))
    summarize_logger("DEBUG", "Column reordering completed")
  }

  summarize_logger("INFO", "Merge completed successfully. Final dimensions:", nrow(merged_data), "x", ncol(merged_data))
  log_operation_end(summarize_logger, paste("MERGE_MU_WITH", toupper(data_type_name)), success = TRUE)
  return(merged_data)
}

#' Prepare questionnaire data for merging with gait data
#' @param questionnaire_data Raw questionnaire data
#' @return List with prepared_data and merge_by columns
prep_questionnaire_for_merge <- function(questionnaire_data) {
  summarize_logger("DEBUG", "Starting prep_questionnaire_for_merge")
  summarize_logger("DEBUG", "Input questionnaire data dimensions:", nrow(questionnaire_data), "x", ncol(questionnaire_data))
  
  if (!"answer_type" %in% colnames(questionnaire_data)) {
    summarize_logger("ERROR", "Questionnaire data must have 'answer_type' column")
    stop("Questionnaire data must have 'answer_type' column")
  }

  # Check available answer types
  answer_types <- unique(questionnaire_data$answer_type)
  summarize_logger("DEBUG", "Available answer types:", paste(answer_types, collapse = ", "))

  # Create trial mapping for questionnaire results
  # training2 answers are assigned to both trial 7 and trial 8
  summarize_logger("DEBUG", "Creating trial mapping for questionnaire results")
  q_results_mapped <- questionnaire_data %>%
    mutate(
      trialNum = case_when(
        answer_type == "baseline_task" ~ list(5L),
        answer_type == "retention" ~ list(10L),
        answer_type == "training2" ~ list(c(7L, 8L)), # Assign to both training trials
        answer_type == "transfer" ~ list(11L),
        TRUE ~ list(integer(0))
      )
    ) %>%
    tidyr::unnest(trialNum) %>% # Expand rows for multiple trial assignments
    dplyr::filter(!is.na(trialNum)) # Only keep questionnaire results that have a trial mapping

  summarize_logger("DEBUG", "Trial mapping completed. Mapped data dimensions:", nrow(q_results_mapped), "x", ncol(q_results_mapped))
  
  # Log trial assignments
  trial_assignments <- q_results_mapped %>%
    group_by(answer_type, trialNum) %>%
    summarise(count = n(), .groups = "drop")
  summarize_logger("DEBUG", "Trial assignments:")
  for (i in 1:nrow(trial_assignments)) {
    summarize_logger("DEBUG", "  ", trial_assignments$answer_type[i], "-> trial", trial_assignments$trialNum[i], "(", trial_assignments$count[i], "records)")
  }
  
  summarize_logger("DEBUG", "prep_questionnaire_for_merge completed")
  return(q_results_mapped)
}

#' Filter dataset to only include participant/trial combinations that exist in gait data
#' @param mu_gait Gait data containing the reference participant/trial combinations
#' @param data_to_filter Dataset to filter (task, complexity, etc.)
#' @param data_type_name Descriptive name for error messages
#' @return Filtered dataset
filter_by_gait_combinations <- function(mu_gait, data_to_filter, data_type_name = "data") {
  summarize_logger("DEBUG", "Starting filter_by_gait_combinations for", data_type_name)
  summarize_logger("DEBUG", "Gait data dimensions:", nrow(mu_gait), "x", ncol(mu_gait))
  summarize_logger("DEBUG", "Data to filter dimensions:", nrow(data_to_filter), "x", ncol(data_to_filter))
  
  # Handle empty or NULL input data
  if (is.null(data_to_filter) || nrow(data_to_filter) == 0) {
    summarize_logger("ERROR", sprintf("No %s data to filter", data_type_name))
    stop(sprintf("No %s data to filter", data_type_name))
  }

  # Get unique participant/trial combinations from gait data
  gait_combinations <- mu_gait %>%
    select(participant, trialNum) %>%
    distinct()
  
  summarize_logger("DEBUG", "Unique gait combinations:", nrow(gait_combinations))
  
  # Check if required columns exist in data_to_filter
  required_cols <- c("participant", "trialNum")
  missing_cols <- setdiff(required_cols, colnames(data_to_filter))
  if (length(missing_cols) > 0) {
    summarize_logger("ERROR", "Missing required columns in", data_type_name, "data:", paste(missing_cols, collapse = ", "))
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter the dataset to only keep matching combinations
  summarize_logger("DEBUG", "Filtering", data_type_name, "data to match gait combinations")
  filtered_data <- data_to_filter %>%
    semi_join(gait_combinations, by = c("participant", "trialNum"))

  summarize_logger("DEBUG", "Filtering completed. Filtered data dimensions:", nrow(filtered_data), "x", ncol(filtered_data))
  summarize_logger("DEBUG", "Rows removed:", nrow(data_to_filter) - nrow(filtered_data))
  
  summarize_logger("DEBUG", "filter_by_gait_combinations completed for", data_type_name)
  return(filtered_data)
}

#' Summarize data across conditions
#' @param data Data frame to summarize
#' @return Summarized data frame
summarize_across_conditions <- function(data) {
  summarize_logger("DEBUG", "Starting summarize_across_conditions")
  summarize_logger("DEBUG", "Input data dimensions:", nrow(data), "x", ncol(data))
  
  # Check that condition column exists - if not, something went wrong upstream
  if (!"condition" %in% colnames(data)) {
    summarize_logger("ERROR", "'condition' column is missing from data. Available columns:", paste(colnames(data), collapse = ", "))
    stop("ERROR: 'condition' column is missing from data. This suggests an issue in the data merging process. Available columns: ", paste(colnames(data), collapse = ", "))
  }

  # Keep a copy with *all* columns so we can fetch demographic info later
  orig_data <- data
  summarize_logger("DEBUG", "Original data preserved for metadata extraction")

  # Determine base grouping columns based on data type
  base_cols <- if ("answer_type" %in% colnames(data)) {
    # Questionnaire data
    c("participant", "answer_type", "condition")
  } else {
    # Gait data - use existing condition column
    c("participant", "condition")
  }
  summarize_logger("DEBUG", "Base grouping columns:", paste(base_cols, collapse = ", "))

  # Add foot to grouping if it exists
  grouping_cols <- if ("foot" %in% colnames(data)) {
    c(base_cols, "foot")
  } else {
    base_cols
  }
  summarize_logger("DEBUG", "Final grouping columns:", paste(grouping_cols, collapse = ", "))

  # Single summarization step
  summarize_logger("DEBUG", "Performing summarization across conditions")
  summarized_data <- data %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  summarize_logger("DEBUG", "Summarization completed. Result dimensions:", nrow(summarized_data), "x", ncol(summarized_data))

  # Add back non-summarizable columns (descriptive/demographic data that doesn't change per participant)
  if (exists("columns_to_not_summarize")) {
    available_meta_cols <- intersect(columns_to_not_summarize, colnames(orig_data))
    summarize_logger("DEBUG", "Available metadata columns:", paste(available_meta_cols, collapse = ", "))
    
    if (length(available_meta_cols) > 0) {
      meta_data <- orig_data %>%
        select(participant, all_of(available_meta_cols)) %>%
        distinct()
      
      summarize_logger("DEBUG", "Metadata dimensions:", nrow(meta_data), "x", ncol(meta_data))

      summarized_data <- summarized_data %>%
        left_join(meta_data, by = "participant")
      
      summarize_logger("DEBUG", "Metadata joined successfully")
    }
  } else {
    summarize_logger("WARN", "columns_to_not_summarize not defined")
  }

  summarize_logger("DEBUG", "summarize_across_conditions completed. Final dimensions:", nrow(summarized_data), "x", ncol(summarized_data))
  return(summarized_data)
}
