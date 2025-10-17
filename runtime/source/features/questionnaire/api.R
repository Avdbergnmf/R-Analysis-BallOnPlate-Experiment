#' Questionnaire Feature Module API
#'
#' Public interface for questionnaire processing and analysis functions.
#' This module provides comprehensive questionnaire data processing capabilities.

# Create shared logger for the questionnaire feature
questionnaire_logger <- create_module_logger("QUESTIONNAIRE")

# =============================================================================
# CORE QUESTIONNAIRE PROCESSING
# =============================================================================

#' Compute scores for a single participant and questionnaire type
#' @param pnum Participant number
#' @param qType Questionnaire type (e.g., "IMI", "UserExperience")
#' @param qdata Questionnaire data for the participant
#' @param qinfo Question information data
#' @param qweights Question weights data
#' @param qAnswers Answer column names
#' @return List of scores for each answer type
compute_scores <- function(pnum, qType, qdata, qinfo, qweights, qanswers) {
  compute_scores(pnum, qType, qdata, qinfo, qweights, qanswers)
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
  calculate_all_scores(qType, participants, get_q_data_func, get_question_info_func, get_question_weights_func, qAnswers)
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
  get_all_questionnaire_results(all_questionnaires, participants, get_q_data_func, get_question_info_func, get_question_weights_func, qAnswers, condition_number_func)
}

#' Filter questionnaire results by questionnaire type
#' @param allQResults Combined questionnaire results data frame
#' @param qType Questionnaire type to filter for (e.g., "IMI", "UserExperience")
#' @return Filtered questionnaire results data frame
filter_questionnaire_results <- function(allQResults, qType) {
  filter_questionnaire_results(allQResults, qType)
}

#' Prepare questionnaire data for merging with gait data
#' @param questionnaire_data Raw questionnaire data
#' @return Prepared questionnaire data with trial mapping
prep_questionnaire_for_merge <- function(questionnaire_data) {
  prep_questionnaire_for_merge(questionnaire_data)
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Complete questionnaire analysis pipeline
#' @param all_questionnaires Vector of questionnaire types
#' @param participants Vector of participant numbers
#' @param get_q_data_func Function to get questionnaire data for a participant
#' @param get_question_info_func Function to get question information
#' @param get_question_weights_func Function to get question weights
#' @param qAnswers Answer column names
#' @param condition_number_func Function to get condition number for a participant
#' @return Combined questionnaire results data frame
analyze_questionnaires <- function(all_questionnaires, participants, get_q_data_func, get_question_info_func, get_question_weights_func, qAnswers, condition_number_func) {
  get_all_questionnaire_results(all_questionnaires, participants, get_q_data_func, get_question_info_func, get_question_weights_func, qAnswers, condition_number_func)
}

#' Get questionnaire summary statistics
#' @param questionnaire_data Questionnaire results data frame
#' @param qType Questionnaire type (optional, for filtering)
#' @return List with summary statistics
get_questionnaire_statistics <- function(questionnaire_data, qType = NULL) {
  if (!is.null(qType)) {
    questionnaire_data <- filter_questionnaire_results(questionnaire_data, qType)
  }
  
  list(
    total_participants = length(unique(questionnaire_data$participant)),
    total_questionnaires = length(unique(questionnaire_data$answer_type)),
    questionnaire_types = if (!is.null(qType)) qType else unique(gsub("\\..*", "", grep("^[A-Za-z]+\\.", colnames(questionnaire_data), value = TRUE))),
    answer_types = unique(questionnaire_data$answer_type),
    conditions = unique(questionnaire_data$condition),
    data_dimensions = c(nrow(questionnaire_data), ncol(questionnaire_data))
  )
}
