# =============================================================================
# CATEGORY HELPER FUNCTIONS FOR ANALYSIS
# =============================================================================
# These functions provide convenient ways to work with categories in analysis
# functions, making the code more readable and maintainable.

# =============================================================================
# CATEGORY SELECTION HELPERS
# =============================================================================

#' Get categories for joining datasets
#' @return Character vector of categories used for joining
get_join_categories <- function() {
  return(get_core_identifiers())
}

#' Get categories for statistical analysis
#' @return Character vector of categories for statistical analysis
get_statistical_categories <- function() {
  return(c(
    get_experimental_conditions(),
    get_demographic_categories()
  ))
}

#' Get categories for plotting/faceting
#' @return Character vector of categories suitable for plotting
get_plotting_categories <- function() {
  return(get_ui_input_categories())
}

#' Get categories that should be excluded from summarization
#' @return Character vector of categories to exclude from summarization
get_non_summarizable_categories <- function() {
  return(c(
    get_analysis_categories_all(),
    get_descriptive_columns()
  ))
}

# =============================================================================
# ANALYSIS-SPECIFIC CATEGORY HELPERS
# =============================================================================

#' Get categories for gait parameter analysis
#' @param include_foot Whether to include foot as a category
#' @return Character vector of categories for gait analysis
get_gait_analysis_categories <- function(include_foot = TRUE) {
  base_categories <- get_analysis_categories_all()
  
  if (include_foot) {
    return(c(base_categories, "foot"))
  } else {
    return(base_categories)
  }
}

#' Get categories for questionnaire analysis
#' @return Character vector of categories for questionnaire analysis
get_questionnaire_analysis_categories <- function() {
  return(c(
    get_core_identifiers(),
    get_experimental_conditions(),
    get_demographic_categories(),
    "answer_type"  # questionnaire-specific
  ))
}

#' Get categories for time-sliced analysis
#' @param include_slice Whether to include slice_index as a category
#' @return Character vector of categories for time-sliced analysis
get_sliced_analysis_categories <- function(include_slice = TRUE) {
  base_categories <- get_analysis_categories_all()
  
  if (include_slice) {
    return(c(base_categories, "slice_index"))
  } else {
    return(base_categories)
  }
}

# =============================================================================
# CATEGORY VALIDATION HELPERS
# =============================================================================

#' Check if a dataset has all required categories for analysis
#' @param data Data frame to check
#' @param analysis_type Type of analysis ("gait", "questionnaire", "sliced")
#' @return Logical indicating if all required categories are present
has_required_categories <- function(data, analysis_type) {
  required_categories <- switch(analysis_type,
    "gait" = get_gait_analysis_categories(),
    "questionnaire" = get_questionnaire_analysis_categories(),
    "sliced" = get_sliced_analysis_categories(),
    stop("Unknown analysis type: ", analysis_type)
  )
  
  return(validate_categories(data, required_categories, analysis_type))
}

#' Get missing categories for a specific analysis
#' @param data Data frame to check
#' @param analysis_type Type of analysis
#' @return Character vector of missing categories
get_missing_categories <- function(data, analysis_type) {
  required_categories <- switch(analysis_type,
    "gait" = get_gait_analysis_categories(),
    "questionnaire" = get_questionnaire_analysis_categories(),
    "sliced" = get_sliced_analysis_categories(),
    stop("Unknown analysis type: ", analysis_type)
  )
  
  return(setdiff(required_categories, colnames(data)))
}

# =============================================================================
# CATEGORY FILTERING HELPERS
# =============================================================================

#' Filter categories by type
#' @param categories Character vector of category names
#' @param type Category type ("core", "experimental", "demographic", "descriptive", "analysis")
#' @return Filtered character vector
filter_categories_by_type <- function(categories, type) {
  type_categories <- switch(type,
    "core" = get_core_identifiers(),
    "experimental" = get_experimental_conditions(),
    "demographic" = get_demographic_categories(),
    "descriptive" = get_descriptive_columns(),
    "analysis" = get_analysis_categories(),
    stop("Unknown category type: ", type)
  )
  
  return(intersect(categories, type_categories))
}

#' Get categories that exist in a dataset
#' @param data Data frame
#' @param categories Character vector of category names to check
#' @return Character vector of categories that exist in the dataset
get_existing_categories <- function(data, categories) {
  return(intersect(categories, colnames(data)))
}

#' Get categories that don't exist in a dataset
#' @param data Data frame
#' @param categories Character vector of category names to check
#' @return Character vector of categories that don't exist in the dataset
get_missing_categories_from_list <- function(data, categories) {
  return(setdiff(categories, colnames(data)))
}

# =============================================================================
# CATEGORY DOCUMENTATION HELPERS
# =============================================================================

#' Get category descriptions for documentation
#' @return Data frame with category descriptions
get_category_descriptions <- function() {
  return(data.frame(
    category = c(
      # Core identifiers
      "participant", "trialNum",
      # Experimental conditions
      "condition", "phase",
      # Demographics
      "gender", "motion",
      # Descriptive columns
      "visualizations", "perturbations", "task", "treadmillSpeed",
      "age", "weight", "education", "vr_experience", "height_scale",
      # Analysis-specific
      "foot", "slice_index", "suspect"
    ),
    type = c(
      # Core identifiers
      "core", "core",
      # Experimental conditions
      "experimental", "experimental",
      # Demographics
      "demographic", "demographic",
      # Descriptive columns
      "descriptive", "descriptive", "descriptive", "descriptive",
      "descriptive", "descriptive", "descriptive", "descriptive", "descriptive",
      # Analysis-specific
      "analysis", "analysis", "analysis"
    ),
    description = c(
      # Core identifiers
      "Unique participant identifier", "Trial number within participant",
      # Experimental conditions
      "Experimental condition (baseline/perturbation/perturbation_visualization)", "Trial phase (warmup/baseline/training/etc.)",
      # Demographics
      "Participant gender", "Motion sickness sensitivity",
      # Descriptive columns
      "Whether visualizations were present", "Whether perturbations were present", "Task type", "Treadmill speed",
      "Participant age", "Participant weight", "Education level", "VR experience", "Height scale factor",
      # Analysis-specific
      "Foot side (left/right)", "Time slice index", "Data quality flag"
    ),
    usage = c(
      # Core identifiers
      "Joining datasets", "Joining datasets",
      # Experimental conditions
      "Statistical analysis", "Statistical analysis",
      # Demographics
      "Statistical analysis", "Statistical analysis",
      # Descriptive columns
      "Preserved in summaries", "Preserved in summaries", "Preserved in summaries", "Preserved in summaries",
      "Preserved in summaries", "Preserved in summaries", "Preserved in summaries", "Preserved in summaries", "Preserved in summaries",
      # Analysis-specific
      "Specific analyses", "Time-sliced analysis", "Data quality filtering"
    )
  ))
}

#' Print category summary for documentation
#' @param analysis_type Optional analysis type to filter categories
print_category_summary <- function(analysis_type = NULL) {
  descriptions <- get_category_descriptions()
  
  if (!is.null(analysis_type)) {
    relevant_categories <- switch(analysis_type,
      "gait" = get_gait_analysis_categories(),
      "questionnaire" = get_questionnaire_analysis_categories(),
      "sliced" = get_sliced_analysis_categories(),
      stop("Unknown analysis type: ", analysis_type)
    )
    descriptions <- descriptions[descriptions$category %in% relevant_categories, ]
  }
  
  cat("Category Summary:\n")
  cat("================\n\n")
  
  for (type in unique(descriptions$type)) {
    cat(sprintf("%s Categories:\n", toupper(type)))
    cat(paste(rep("-", nchar(type) + 10), collapse = ""), "\n")
    
    type_desc <- descriptions[descriptions$type == type, ]
    for (i in 1:nrow(type_desc)) {
      cat(sprintf("  %-20s: %s\n", type_desc$category[i], type_desc$description[i]))
    }
    cat("\n")
  }
}