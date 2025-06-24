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
      group_by(across(all_of(setdiff(categories, "heelStrikes.foot")))) %>%
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
  # Source complexity functions if not already loaded
  if (!exists("compute_complexity")) {
    source("source/complexity.R")
  }
  
  # Remove heelStrikes.foot from categories since we want to combine feet
  categories_no_foot <- setdiff(categories, "heelStrikes.foot")
  
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
      complexity_result <- compute_complexity(values)
      
      # Convert to data frame
      as.data.frame(complexity_result)
    }) %>%
    ungroup()

  return(complexity_data)
}

# Average across feet and also calculate diff between left and right foot and add it to mu
average_over_feet <- function(data, types, categories, add_diff = FALSE) {
  # Get summaries averaging over feet
  categories_no_foot <- setdiff(categories, "heelStrikes.foot")
  mu_avg <- lapply(types, get_summ_by_foot, categories_no_foot, data, avg_feet = TRUE)
  mu_avg <- setNames(mu_avg, types)

  if (add_diff) {
    # Keep heelStrikes.foot in categories for per-foot summaries
    categories_with_foot <- categories

    # Step 1: Get per-foot summaries without averaging over feet
    mu_per_foot <- lapply(types, get_summ_by_foot, categories_with_foot, data, avg_feet = FALSE)
    mu_per_foot <- setNames(mu_per_foot, types)

    # Step 2: Calculate the differences between left and right foot measurements
    mu_diff <- lapply(mu_per_foot, function(df) {
      # Pivot the data wider to have separate columns for left and right foot measurements
      df_wide <- df %>%
        pivot_wider(names_from = heelStrikes.foot, values_from = c(mean, sd, cv))

      # Ensure both left and right foot data are available
      # df_wide <- df_wide %>%
      #  filter(!is.na(mean_Left) & !is.na(mean_Right))

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
summarize_table <- function(data, allQResults, categories, avg_feet = TRUE, add_diff = FALSE) { # note: add diff only works if also averaging over feet
  dataTypes <- setdiff(getTypes(data), categories)

  # Define the list of columns to remove - We add these later, but remove them here so they are not considered for the summary table
  data <- data %>% select(-all_of(columns_to_not_summarize)) # Remove the specified columns from the data
  types <- setdiff(dataTypes, columns_to_not_summarize) # Also remove them from our types list

  if (avg_feet) {
    mu <- average_over_feet(data, types, categories, add_diff = add_diff)
    categories <- setdiff(categories, "heelStrikes.foot") # remove heelStrikes.foot from category list
  } else {
    # If not averaging over feet, compute mu normally
    mu <- lapply(types, get_summ_by_foot, categories, data, avg_feet = FALSE)
    mu <- setNames(mu, types)
  }

  # Add complexity metrics for stepTimes and stepWidths (combining both feet)
  complexity_types <- c("stepTimes", "stepWidths")
  available_complexity_types <- intersect(complexity_types, types)
  
  if (length(available_complexity_types) > 0) {
    # Calculate complexity metrics for each available type (combining both feet)
    mu_complexity <- lapply(available_complexity_types, get_complexity_combined, categories, data)
    names(mu_complexity) <- paste0(available_complexity_types, "_complexity")
    
    # Add complexity results to mu
    mu <- c(mu, mu_complexity)
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

  # Properly merge with questionnaire results based on trial mapping
  if ("answer_type" %in% colnames(allQResults)) {
    # Create trial mapping for questionnaire results
    q_results_mapped <- allQResults %>%
      mutate(
        trialNum = case_when(
          answer_type == "baseline_task" ~ 5,
          answer_type == "retention" ~ 10,
          answer_type == "training2" ~ 8,
          answer_type == "transfer" ~ 11,
          TRUE ~ NA_real_
        )
      ) %>%
      filter(!is.na(trialNum)) # Only keep questionnaire results that have a trial mapping

    # Convert trialNum to numeric in both datasets to ensure compatibility
    mu_wide$trialNum <- as.numeric(as.character(mu_wide$trialNum))
    q_results_mapped$trialNum <- as.numeric(q_results_mapped$trialNum)

    # Merge based on participant and trialNum
    mu_full <- left_join(mu_wide, q_results_mapped, by = c("participant", "trialNum"))
  } else {
    # Fallback to original merge if no answer_type column
    mu_full <- merge(allQResults, mu_wide, by = c("participant"), all = TRUE)
  }

  # Create the new column using mutate and sapply
  mu_full <- add_category_columns(mu_full)

  # Reorder columns
  mu_full <- mu_full %>%
    select(all_of(categories), all_of(columns_to_not_summarize), everything())

  return(mu_full)
}


get_full_mu <- function(allGaitParams, allQResults, categories, avg_feet = TRUE, add_diff = FALSE) { # I could not get the optional feet averaging to work without having to pass it all the way down (would be nice to have some post-processing function that averages afterwards, optionally, but in the end went with this cumbersome road)
  # Join the columns
  muGait <- summarize_table(allGaitParams, allQResults, c(categories, "heelStrikes.foot"), avg_feet, add_diff) ##### Note that we add heelStrikes.foot here as a category, to make sure we summarize each foot individually

  return(muGait)
}

### SUMMARIZE AGAIN AND CALCULATE DIFF, SO WE CAN USE FOR CORRELATIONN PLOT

summarize_across_conditions <- function(data) {
  # Check if this is questionnaire data (has answer_type) or gait data
  if ("answer_type" %in% colnames(data)) {
    # This is questionnaire data - group by answer_type and condition
    if ("heelStrikes.foot" %in% colnames(data)) {
      # Group by participant, answer_type, condition, and heelStrikes.foot
      summarized_data <- data %>%
        group_by(participant, answer_type, condition, heelStrikes.foot) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    } else {
      # Group by participant, answer_type, and condition only
      summarized_data <- data %>%
        group_by(participant, answer_type, condition) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    }
  } else {
    # This is gait data - use original logic but adapt for new structure
    # Filter for the relevant trial numbers if trialNum exists
    if ("trialNum" %in% colnames(data)) {
      data <- data %>%
        dplyr::filter(trialNum %in% c(2, 3, 5, 6))

      # Add condition column based on trial numbers
      data <- data %>%
        mutate(trial_condition = case_when(
          trialNum %in% c(2, 3) ~ "condition_1",
          trialNum %in% c(5, 6) ~ "condition_2"
        ))
    }

    # Check if 'heelStrikes.foot' exists in the data
    if ("heelStrikes.foot" %in% colnames(data)) {
      # Group by participant, condition, and heelStrikes.foot
      summarized_data <- data %>%
        group_by(participant, condition, heelStrikes.foot) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    } else {
      # Group by participant and condition only
      summarized_data <- data %>%
        group_by(participant, condition) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    }
  }

  return(summarized_data)
}

############ Added later to answer question of reviewer

summarize_table_sliced <- function(data, allQResults, categories, slice_length, time_col = "time", avg_feet = TRUE, add_diff = FALSE) {
  # Split the data into time slices
  data <- data %>%
    group_by(participant, trialNum) %>%
    mutate(
      slice_index = floor((.data[[time_col]] - min(.data[[time_col]], na.rm = TRUE)) / slice_length) + 1
    ) %>%
    ungroup()

  # data$slice_index <- as.ordered(data$slice_index)

  # Identify which columns to summarize
  dataTypes <- setdiff(getTypes(data), categories)

  # Remove any columns that should not be summarized
  data <- data %>% select(-all_of(columns_to_not_summarize))
  types <- setdiff(dataTypes, columns_to_not_summarize)

  # Summarize parameters within each slice
  # (Reuse existing functions; just add slice_index to the grouping)
  grouping_cols <- c(categories, "slice_index")

  if (avg_feet) {
    mu <- average_over_feet(data, types, grouping_cols, add_diff = add_diff)
    grouping_cols <- setdiff(grouping_cols, "heelStrikes.foot")
  } else {
    mu <- lapply(types, get_summ_by_foot, grouping_cols, data, avg_feet = FALSE)
    mu <- setNames(mu, types)
  }

  # Combine summaries into long format
  mu_long <- bind_rows(mu, .id = "dataType") %>%
    pivot_longer(
      cols      = -c(all_of(grouping_cols), dataType),
      names_to  = "statistic",
      values_to = "value"
    )

  # Pivot to wide format with dataType.statistic columns
  mu_wide <- mu_long %>%
    unite("new_col_name", dataType, statistic, sep = ".") %>%
    pivot_wider(
      names_from  = new_col_name,
      values_from = value
    )

  # Merge with allQResults (similar to summarize_table)
  mu_full <- merge(allQResults, mu_wide, by = c("participant"), all = TRUE)

  # Add any category columns (if your pipeline expects them)
  mu_full <- add_category_columns(mu_full)

  # Reorder columns so that grouping_cols and any excluded columns appear first
  mu_full <- mu_full %>%
    select(all_of(grouping_cols), all_of(columns_to_not_summarize), everything())

  return(mu_full)
}


get_full_mu_sliced <- function(allGaitParams, allQResults, categories,
                               slice_length = 180, avg_feet = TRUE, add_diff = FALSE, remove_middle_slices = FALSE) {
  # Summarize gait parameters in time slices
  # Note we include heelStrikes.foot in categories so that per-foot summaries are computed if needed
  muGait <- summarize_table_sliced(
    data = allGaitParams,
    allQResults = allQResults,
    categories = c(categories, "heelStrikes.foot"),
    slice_length = slice_length,
    time_col = "heelStrikes.time",
    avg_feet = avg_feet,
    add_diff = add_diff
  )


  if (remove_middle_slices) {
    # remove all but the first and the last slice.
    muGait <- muGait %>%
      group_by(trialNum) %>%
      filter(slice_index == min(slice_index, na.rm = TRUE) | slice_index == max(slice_index, na.rm = TRUE)) %>%
      ungroup()
  }

  return(muGait)
}

filter_incomplete_slices <- function(data_sliced) { # sometimes for whatever reason another slice might be detected for some of the participants, we filter those out here.
  # Count how many rows appear in each trialNum/slice_index
  slice_counts <- data_sliced %>%
    group_by(trialNum, slice_index) %>%
    summarise(n_rows = n(), .groups = "drop")

  # For each trial, find the maximum number of rows among all slices
  # and compare each slice's n_rows to that maximum.
  slice_counts <- slice_counts %>%
    group_by(trialNum) %>%
    mutate(max_n_rows_in_trial = max(n_rows)) %>%
    ungroup() %>%
    mutate(ratio_of_max = n_rows / max_n_rows_in_trial)

  # Define "bad" slices as those whose ratio_of_max < 1
  # (or pick a smaller threshold if you only consider < 0.5 etc. "too few")
  bad_slices <- slice_counts %>%
    filter(ratio_of_max < 1)

  # If there are any "bad" slices, print them and remove them
  if (nrow(bad_slices) > 0) {
    message("Debug: The following trialNum/slice_index combos have fewer rows than the max for that trial; removing them now:")
    print(bad_slices)

    # Remove those slices from your data_sliced
    data_sliced <- data_sliced %>%
      anti_join(bad_slices %>% select(trialNum, slice_index),
        by = c("trialNum", "slice_index")
      )
  }

  return(data_sliced)
}

###### To reply to reviewer

calculate_phase_differences <- function(data) {
  return(data) # TODO: Implement this

  # Calculate differences between specific phases for questionnaire data
  # This function creates difference variables comparing phases (e.g., training - baseline)

  if (!"answer_type" %in% colnames(data)) {
    warning("answer_type column not found - returning data unchanged")
    return(data)
  }

  # Split data by phase
  phases <- unique(data$answer_type)

  # Check if we have baseline as a reference
  if (!"baseline" %in% phases) {
    warning("No baseline phase found - returning data unchanged")
    return(data)
  }

  # Get baseline data
  baseline_data <- data %>% dplyr::filter(answer_type == "baseline")

  # Initialize result list
  diff_results <- list()

  # Calculate differences for each non-baseline phase
  for (phase in setdiff(phases, "baseline")) {
    phase_data <- data %>% dplyr::filter(answer_type == !!phase)

    # Check if both datasets have the same participants
    common_participants <- intersect(baseline_data$participant, phase_data$participant)

    if (length(common_participants) == 0) {
      next
    }

    # Filter to common participants and align
    baseline_subset <- baseline_data %>%
      dplyr::filter(participant %in% common_participants) %>%
      dplyr::arrange(participant)

    phase_subset <- phase_data %>%
      dplyr::filter(participant %in% common_participants) %>%
      dplyr::arrange(participant)

    # Calculate differences for numeric columns
    numeric_cols <- names(phase_subset)[sapply(phase_subset, is.numeric)]

    if (length(numeric_cols) > 0) {
      diff_data <- phase_subset %>%
        dplyr::select(participant, condition) %>%
        dplyr::mutate(comparison = paste0(phase, "_vs_baseline"))

      # Add difference columns
      for (col in numeric_cols) {
        diff_col_name <- paste0("diff_", col)
        mean_col_name <- paste0("mean_", col)

        diff_data[[diff_col_name]] <- phase_subset[[col]] - baseline_subset[[col]]
        diff_data[[mean_col_name]] <- (phase_subset[[col]] + baseline_subset[[col]]) / 2
      }

      diff_results[[phase]] <- diff_data
    }
  }

  # Combine all difference results
  if (length(diff_results) > 0) {
    final_result <- do.call(rbind, diff_results)
    return(final_result)
  } else {
    warning("No phase differences could be calculated")
    return(data)
  }
}
