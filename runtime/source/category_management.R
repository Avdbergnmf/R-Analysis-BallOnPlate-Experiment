# =============================================================================
# CATEGORY MANAGEMENT SYSTEM
# =============================================================================
# This file provides a centralized, organized approach to managing different
# types of categories used throughout the analysis pipeline.

# =============================================================================
# CATEGORY TYPE DEFINITIONS
# =============================================================================

#' Get core identifier categories
#' These are used for joining datasets and should always be included
#' @return Character vector of core identifier category names
get_core_identifiers <- function() {
  return(c("participant", "trialNum"))
}

#' Get experimental condition categories
#' These represent experimental manipulations and are used for analysis grouping
#' @return Character vector of experimental condition category names
get_experimental_conditions <- function() {
  return(c("condition", "phase"))
}

#' Get demographic categories
#' These represent participant characteristics and are used for analysis grouping
#' @return Character vector of demographic category names
get_demographic_categories <- function() {
  return(c("gender", "motion"))
}

#' Get descriptive columns
#' These should be preserved in summary tables but not used for numerical summarization
#' @return Character vector of descriptive column names
get_descriptive_columns <- function() {
  return(c(
    "visualizations", "perturbations", "task", "treadmillSpeed",
    "age", "weight", "education", "vr_experience", "height_scale"
  ))
}

#' Get analysis-specific categories
#' These are used for specific analyses but not core grouping
#' @return Character vector of analysis-specific category names
get_analysis_categories <- function() {
  return(c("foot", "slice_index", "suspect"))
}

# =============================================================================
# COMPOSITE CATEGORY SETS
# =============================================================================

#' Get all categories used for data grouping and analysis
#' This combines core identifiers, experimental conditions, and demographics
#' @return Character vector of all analysis categories
get_analysis_categories_all <- function() {
  return(c(
    get_core_identifiers(),
    get_experimental_conditions(),
    get_demographic_categories()
  ))
}

#' Get categories for summary tables (includes descriptive columns)
#' @return Character vector of categories for summary tables
get_summary_table_categories <- function() {
  return(c(
    get_analysis_categories_all(),
    get_descriptive_columns()
  ))
}

#' Get categories for UI inputs (includes analysis-specific categories)
#' @return Character vector of categories for UI inputs
get_ui_input_categories <- function() {
  return(c(
    get_summary_table_categories(),
    get_analysis_categories(),
    "None"
  ))
}

# =============================================================================
# CATEGORY VALIDATION AND UTILITIES
# =============================================================================

#' Validate that required categories exist in a dataset
#' @param data Data frame to validate
#' @param required_categories Character vector of required category names
#' @param context String describing the context for error messages
#' @return Logical indicating if all required categories are present
validate_categories <- function(data, required_categories, context = "dataset") {
  missing_categories <- setdiff(required_categories, colnames(data))
  
  if (length(missing_categories) > 0) {
    warning(sprintf(
      "Missing required categories in %s: %s",
      context,
      paste(missing_categories, collapse = ", ")
    ))
    return(FALSE)
  }
  
  return(TRUE)
}

#' Get data types that should be summarized (exclude categories)
#' @param data Data frame
#' @param categories Character vector of category names to exclude
#' @return Character vector of column names that should be summarized
get_summarizable_columns <- function(data, categories = get_analysis_categories_all()) {
  all_columns <- colnames(data)
  return(setdiff(all_columns, categories))
}

#' Add category columns to a dataset
#' This is a wrapper around the existing add_category_columns function
#' that provides better error handling and validation
#' @param data Data frame to add categories to
#' @param participant Participant ID
#' @param trial Trial number
#' @return Data frame with category columns added
add_categories_to_data <- function(data, participant, trial) {
  # Add identifiers first
  data <- add_identifiers(data, participant, trial)
  
  # Add all category columns
  data <- add_category_columns(data)
  
  # Validate that all expected categories are present
  expected_categories <- get_summary_table_categories()
  validate_categories(data, expected_categories, "processed dataset")
  
  return(data)
}

# =============================================================================
# CATEGORY-SPECIFIC UTILITIES
# =============================================================================

#' Get unique values for a category across all data
#' @param data Data frame
#' @param category Category name
#' @return Character vector of unique values
get_category_values <- function(data, category) {
  if (!category %in% colnames(data)) {
    warning(sprintf("Category '%s' not found in dataset", category))
    return(character(0))
  }
  
  return(as.character(unique(data[[category]])))
}

#' Filter data by category values
#' @param data Data frame
#' @param category Category name
#' @param values Values to filter by
#' @return Filtered data frame
filter_by_category <- function(data, category, values) {
  if (!category %in% colnames(data)) {
    warning(sprintf("Category '%s' not found in dataset", category))
    return(data)
  }
  
  return(data[data[[category]] %in% values, ])
}

#' Get categories that are suitable for a specific analysis type
#' @param analysis_type Type of analysis ("grouping", "plotting", "statistics")
#' @return Character vector of suitable categories
get_categories_for_analysis <- function(analysis_type) {
  switch(analysis_type,
    "grouping" = get_analysis_categories_all(),
    "plotting" = get_ui_input_categories(),
    "statistics" = get_analysis_categories_all(),
    stop("Unknown analysis type: ", analysis_type)
  )
}

# =============================================================================
# BACKWARD COMPATIBILITY
# =============================================================================
# These functions maintain compatibility with existing code while using
# the new organized structure

#' Get categories (backward compatibility)
#' @return Character vector of categories (same as get_analysis_categories_all())
get_categories <- function() {
  return(get_analysis_categories_all())
}

#' Get categoriesExtra (backward compatibility)
#' @return Character vector of categoriesExtra (same as get_summary_table_categories())
get_categories_extra <- function() {
  return(get_summary_table_categories())
}

#' Get categoriesExtraInputs (backward compatibility)
#' @return Character vector of categoriesExtraInputs (same as get_ui_input_categories())
get_categories_extra_inputs <- function() {
  return(get_ui_input_categories())
}