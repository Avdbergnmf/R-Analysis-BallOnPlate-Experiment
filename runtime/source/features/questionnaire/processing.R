#' Questionnaire Processing Functions
#'
#' Functions for processing and analyzing questionnaire data.
#' All functions are self-contained and accept data as parameters.

# Create module-specific logger
questionnaire_logger <- create_module_logger("QUESTIONNAIRE")

#' Compute scores for a single participant and questionnaire type
#' @param pnum Participant number
#' @param qType Questionnaire type (e.g., "IMI", "UserExperience")
#' @param qdata Questionnaire data for the participant
#' @param qinfo Question information data
#' @param qweights Question weights data
#' @param qAnswers Answer column names
#' @return List of scores for each answer type
compute_scores <- function(pnum, qType, qdata, qinfo, qweights, qanswers) {
  questionnaire_logger("DEBUG", sprintf("Computing scores for participant %s, questionnaire %s", pnum, qType))
  
  # Validate input data
  if (is.null(qdata) || nrow(qdata) == 0) {
    questionnaire_logger("ERROR", "No questionnaire data provided")
    stop("No questionnaire data provided")
  }
  
  if (is.null(qinfo) || nrow(qinfo) == 0) {
    questionnaire_logger("ERROR", "No question info provided")
    stop("No question info provided")
  }
  
  if (is.null(qweights) || nrow(qweights) == 0) {
    questionnaire_logger("ERROR", "No question weights provided")
    stop("No question weights provided")
  }

  combined <- base::merge(qdata, qinfo, by = "QuestionID")
  questionnaire_logger("DEBUG", sprintf("Merged data dimensions: %d x %d", nrow(combined), ncol(combined)))

  # Get max and min score for mirroring
  max_score <- qweights[qweights$category == "max_score", "weight"]
  min_score <- qweights[qweights$category == "min_score", "weight"]
  do_average <- qweights[qweights$category == "do_average", "weight"]

  questionnaire_logger("DEBUG", sprintf("Scoring parameters: max=%s, min=%s, do_average=%s", max_score, min_score, do_average))

  # Mirror the scores if needed
  for (column in qanswers) {
    # Ensure mirror column is properly converted to logical
    mirror_logical <- combined$mirror == "true" | combined$mirror == TRUE | combined$mirror == "True"

    combined[[column]] <- ifelse(mirror_logical,
      max_score + min_score - combined[[column]],
      combined[[column]]
    )
  }

  # Find all columns that contain the word 'category'
  category_columns <- grep("category", colnames(combined), value = TRUE)
  questionnaire_logger("DEBUG", sprintf("Found category columns: %s", paste(category_columns, collapse = ", ")))

  # Create a long format dataframe to handle multiple categories
  combined_long <- combined %>%
    tidyr::pivot_longer(cols = all_of(category_columns), names_to = "category_type", values_to = "category") %>%
    dplyr::filter(!is.na(category))
  combined_long <- combined_long[combined_long$category != "", ]
  
  questionnaire_logger("DEBUG", sprintf("Long format data dimensions: %d x %d", nrow(combined_long), ncol(combined_long)))

  # Prepare a list to store results for each answer column
  scores_list <- list()

  # Compute the scores for each answer column (condition)
  for (ans_col in qanswers) {
    questionnaire_logger("DEBUG", sprintf("Processing answer column: %s", ans_col))
    
    if (do_average == 0) {
      # Compute the weighted scores for each category
      combined_long <- combined_long %>%
        dplyr::mutate(
          weight = dplyr::case_when(
            category %in% qweights$category ~ qweights$weight[match(category, qweights$category)],
            TRUE ~ 1
          ),
          weighted_answer = .data[[ans_col]] * weight
        )

      scores <- tapply(combined_long$weighted_answer, combined_long$category, sum, na.rm = TRUE)
      # Compute the total score for each condition (sum of unweighted scores, multiplied by total weight)
      total <- tapply(combined_long[[ans_col]], combined_long$category, sum, na.rm = TRUE)

      # Compute the total weighted score for each condition
      # Check if total weight is found, if not assign a value of 1
      if ("total" %in% qweights$category) {
        total_weight <- qweights[qweights$category == "total", "weight"]
      } else {
        total_weight <- 1
      }

      scores["total"] <- sum(total, na.rm = TRUE) * total_weight
    } else {
      scores <- tapply(combined_long[[ans_col]], combined_long$category, mean, na.rm = TRUE)
      # Compute the total score for each condition
      scores["total"] <- mean(scores, na.rm = TRUE)
    }
    # Store the scores in the list, using the answer column name as the key
    scores_list[[ans_col]] <- scores
  }

  questionnaire_logger("DEBUG", sprintf("Computed scores for %d answer types", length(scores_list)))
  return(scores_list)
}

#' Calculate scores for all participants for a given questionnaire type
#' @param qType Questionnaire type (e.g., "IMI", "UserExperience")
#' @param participants Vector of participant numbers
#' @param get_q_data_func Function to get questionnaire data for a participant
#' @param get_question_info_func Function to get question information
#' @param get_question_weights_func Function to get question weights
#' @param qAnswers Answer column names
#' @return Data frame with scores for all participants
calculate_all_scores <- function(qType, participants, get_q_data_func, get_question_info_func, get_question_weights_func, qAnswers) {
  questionnaire_logger("INFO", sprintf("Calculating scores for questionnaire type: %s", qType))
  questionnaire_logger("DEBUG", sprintf("Processing %d participants", length(participants)))
  
  # Validate input functions
  if (!is.function(get_q_data_func)) {
    questionnaire_logger("ERROR", "get_q_data_func must be a function")
    stop("get_q_data_func must be a function")
  }
  if (!is.function(get_question_info_func)) {
    questionnaire_logger("ERROR", "get_question_info_func must be a function")
    stop("get_question_info_func must be a function")
  }
  if (!is.function(get_question_weights_func)) {
    questionnaire_logger("ERROR", "get_question_weights_func must be a function")
    stop("get_question_weights_func must be a function")
  }
  
  # Get question info and weights once (they're the same for all participants)
  qinfo <- get_question_info_func(qType)
  qweights <- get_question_weights_func(qType)
  
  questionnaire_logger("DEBUG", sprintf("Question info dimensions: %d x %d", nrow(qinfo), ncol(qinfo)))
  questionnaire_logger("DEBUG", sprintf("Question weights dimensions: %d x %d", nrow(qweights), ncol(qweights)))

  # Vectorized computation of scores for all participants
  scores_list <- lapply(participants, function(participant) {
    questionnaire_logger("DEBUG", sprintf("Processing participant: %s", participant))
    
    qdata <- get_q_data_func(participant, qType)
    scores <- compute_scores(participant, qType, qdata, qinfo, qweights, qAnswers)
    flat_scores <- unlist(scores)
    newRow <- as.data.frame(as.list(flat_scores), stringsAsFactors = FALSE)
    newRow$participant <- participant
    return(newRow)
  })

  # Combine all rows into a single data frame
  allScores <- do.call(rbind, scores_list)
  questionnaire_logger("DEBUG", sprintf("Combined scores dimensions: %d x %d", nrow(allScores), ncol(allScores)))

  # Reorder columns to have participant and condition first
  col_order <- c("participant", setdiff(colnames(allScores), "participant"))
  allScores <- allScores[, col_order, drop = FALSE]

  questionnaire_logger("INFO", sprintf("Completed score calculation for %s", qType))
  return(allScores)
}

#' Get all questionnaire results for all questionnaire types
#' @param all_questionnaires Vector of questionnaire types
#' @param participants Vector of participant numbers
#' @param get_q_data_func Function to get questionnaire data for a participant
#' @param get_question_info_func Function to get question information
#' @param get_question_weights_func Function to get question weights
#' @param qAnswers Answer column names
#' @param condition_number_func Function to get condition number for a participant
#' @return Combined questionnaire results data frame
get_all_questionnaire_results <- function(all_questionnaires, participants, get_q_data_func, get_question_info_func, get_question_weights_func, qAnswers, condition_number_func) {
  questionnaire_logger("INFO", "Starting get_all_questionnaire_results")
  questionnaire_logger("DEBUG", sprintf("Questionnaire types: %s", paste(all_questionnaires, collapse = ", ")))
  questionnaire_logger("DEBUG", sprintf("Number of participants: %d", length(participants)))
  
  # Validate input functions
  if (!is.function(get_q_data_func)) {
    questionnaire_logger("ERROR", "get_q_data_func must be a function")
    stop("get_q_data_func must be a function")
  }
  if (!is.function(get_question_info_func)) {
    questionnaire_logger("ERROR", "get_question_info_func must be a function")
    stop("get_question_info_func must be a function")
  }
  if (!is.function(get_question_weights_func)) {
    questionnaire_logger("ERROR", "get_question_weights_func must be a function")
    stop("get_question_weights_func must be a function")
  }
  if (!is.function(condition_number_func)) {
    questionnaire_logger("ERROR", "condition_number_func must be a function")
    stop("condition_number_func must be a function")
  }

  # Calculate scores for all questionnaires
  results_list <- lapply(all_questionnaires, function(q) {
    questionnaire_logger("DEBUG", sprintf("Processing questionnaire: %s", q))
    
    scores_df <- calculate_all_scores(q, participants, get_q_data_func, get_question_info_func, get_question_weights_func, qAnswers)

    # Transform to long format with phase information
    long_data <- scores_df %>%
      tidyr::pivot_longer(
        cols = -participant,
        names_to = c("answer_type", "score_type"),
        names_pattern = "(.+)\\.([^.]+)$",
        values_to = "score"
      ) %>%
      mutate(
        questionnaire = q,
        # Create new column names with questionnaire prefix
        score_name = paste0(questionnaire, ".", score_type)
      ) %>%
      select(participant, answer_type, score_name, score) %>%
      tidyr::pivot_wider(
        names_from = score_name,
        values_from = score
      )

    questionnaire_logger("DEBUG", sprintf("Long format for %s: %d x %d", q, nrow(long_data), ncol(long_data)))
    return(long_data)
  })

  # Merge all questionnaire results by participant and answer_type
  questionnaire_logger("DEBUG", "Merging questionnaire results")
  allQResults <- Reduce(function(x, y) {
    base::merge(x, y, by = c("participant", "answer_type"), all = TRUE)
  }, results_list)
  
  questionnaire_logger("DEBUG", sprintf("Merged results dimensions: %d x %d", nrow(allQResults), ncol(allQResults)))

  # Add condition column (between-participant design)
  questionnaire_logger("DEBUG", "Adding condition column")
  allQResults$condition <- sapply(allQResults$participant, condition_number_func)
  
  questionnaire_logger("INFO", sprintf("Completed questionnaire processing. Final dimensions: %d x %d", nrow(allQResults), ncol(allQResults)))
  return(allQResults)
}

#' Filter questionnaire results by questionnaire type
#' @param allQResults Combined questionnaire results data frame
#' @param qType Questionnaire type to filter for (e.g., "IMI", "UserExperience")
#' @return Filtered questionnaire results data frame
filter_questionnaire_results <- function(allQResults, qType) {
  questionnaire_logger("DEBUG", sprintf("Filtering questionnaire results for type: %s", qType))
  questionnaire_logger("DEBUG", sprintf("Input data dimensions: %d x %d", nrow(allQResults), ncol(allQResults)))
  
  # Validate input data
  if (is.null(allQResults) || nrow(allQResults) == 0) {
    questionnaire_logger("ERROR", "No questionnaire results data provided")
    stop("No questionnaire results data provided")
  }
  
  # Get the columns that belong to the specified questionnaire
  columns_to_keep <- grep(paste0("^", qType, "\\."), colnames(allQResults), value = TRUE)
  columns_to_keep <- c("participant", "answer_type", "condition", columns_to_keep)
  
  questionnaire_logger("DEBUG", sprintf("Columns to keep: %s", paste(columns_to_keep, collapse = ", ")))

  # Filter the data frame to keep only the relevant columns
  filtered_results <- allQResults[, columns_to_keep, drop = FALSE]
  
  questionnaire_logger("DEBUG", sprintf("Filtered results dimensions: %d x %d", nrow(filtered_results), ncol(filtered_results)))
  questionnaire_logger("DEBUG", sprintf("Completed filtering for %s", qType))
  
  return(filtered_results)
}

#' Prepare questionnaire data for merging with gait data
#' @param questionnaire_data Raw questionnaire data
#' @return Prepared questionnaire data with trial mapping
prep_questionnaire_for_merge <- function(questionnaire_data) {
  questionnaire_logger("DEBUG", "Starting prep_questionnaire_for_merge")
  questionnaire_logger("DEBUG", "Input questionnaire data dimensions:", nrow(questionnaire_data), "x", ncol(questionnaire_data))
  
  # Validate input data
  if (is.null(questionnaire_data) || nrow(questionnaire_data) == 0) {
    questionnaire_logger("ERROR", "No questionnaire data provided")
    stop("No questionnaire data provided")
  }
  
  if (!"answer_type" %in% colnames(questionnaire_data)) {
    questionnaire_logger("ERROR", "Questionnaire data must have 'answer_type' column")
    stop("Questionnaire data must have 'answer_type' column")
  }

  # Check available answer types
  answer_types <- unique(questionnaire_data$answer_type)
  questionnaire_logger("DEBUG", "Available answer types:", paste(answer_types, collapse = ", "))

  # Create trial mapping for questionnaire results
  # training2 answers are assigned to both trial 7 and trial 8
  questionnaire_logger("DEBUG", "Creating trial mapping for questionnaire results")
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

  questionnaire_logger("DEBUG", "Trial mapping completed. Mapped data dimensions:", nrow(q_results_mapped), "x", ncol(q_results_mapped))
  
  # Log trial assignments
  trial_assignments <- q_results_mapped %>%
    group_by(answer_type, trialNum) %>%
    summarise(count = n(), .groups = "drop")
  questionnaire_logger("DEBUG", "Trial assignments:")
  for (i in 1:nrow(trial_assignments)) {
    questionnaire_logger("DEBUG", "  ", trial_assignments$answer_type[i], "-> trial", trial_assignments$trialNum[i], "(", trial_assignments$count[i], "records)")
  }
  
  questionnaire_logger("DEBUG", "prep_questionnaire_for_merge completed")
  return(q_results_mapped)
}
