### Questionnaire results calculation

compute_scores <- function(pnum, qType) {
  qdata <- get_q_data(pnum, qType)

  qinfo <- get_question_info(qType)
  combined <- merge(qdata, qinfo, by = "QuestionID")

  # Retrieve the weights and score info
  qweights <- get_question_weights(qType)

  # Get max and min score for mirroring
  max_score <- qweights[qweights$category == "max_score", "weight"]
  min_score <- qweights[qweights$category == "min_score", "weight"]
  do_average <- qweights[qweights$category == "do_average", "weight"]

  # Mirror the scores if needed
  for (column in qAnswers) {
    combined[[column]] <- ifelse(combined$mirror,
      max_score + min_score - combined[[column]],
      combined[[column]]
    )
  }

  # Find all columns that contain the word 'category'
  category_columns <- grep("category", colnames(combined), value = TRUE)

  # Create a long format dataframe to handle multiple categories
  combined_long <- combined %>%
    tidyr::pivot_longer(cols = all_of(category_columns), names_to = "category_type", values_to = "category") %>%
    dplyr::filter(!is.na(category))
  combined_long <- combined_long[combined_long$category != "", ]

  # Prepare a list to store results for each answer column
  scores_list <- list()

  # Compute the scores for each answer column (condition)
  for (ans_col in qAnswers) {
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

  return(scores_list)
}


calculate_all_scores <- function(qType) {
  # Vectorized computation of scores for all participants
  scores_list <- lapply(participants, function(participant) {
    scores <- compute_scores(participant, qType)
    flat_scores <- unlist(scores)
    newRow <- as.data.frame(as.list(flat_scores), stringsAsFactors = FALSE)
    newRow$participant <- participant
    return(newRow)
  })

  # Combine all rows into a single data frame
  allScores <- do.call(rbind, scores_list)

  # Reorder columns to have participant and condition first
  col_order <- c("participant", setdiff(colnames(allScores), "participant"))
  allScores <- allScores[, col_order, drop = FALSE]

  return(allScores)
}

get_all_questionnaire_results <- function(parallel = TRUE) {
  all_questionnaires <- c("IMI", "UserExperience")

  # Calculate scores for all questionnaires
  results_list <- lapply(all_questionnaires, function(q) {
    scores_df <- calculate_all_scores(q)

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

    return(long_data)
  })

  # Merge all questionnaire results by participant and answer_type
  allQResults <- Reduce(function(x, y) {
    merge(x, y, by = c("participant", "answer_type"), all = TRUE)
  }, results_list)

  # Add condition column (between-participant design)
  allQResults$condition <- sapply(allQResults$participant, condition_number)

  return(allQResults)
}

filter_questionnaire_results <- function(allQResults, qType) { # qType= "IMI", "UserExperience"
  # Get the columns that belong to the specified questionnaire
  columns_to_keep <- grep(paste0("^", qType, "\\."), colnames(allQResults), value = TRUE)
  columns_to_keep <- c("participant", "answer_type", "condition", columns_to_keep)

  # Filter the data frame to keep only the relevant columns
  filtered_results <- allQResults[, columns_to_keep, drop = FALSE]

  return(filtered_results)
}
