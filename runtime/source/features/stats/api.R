#' Statistical Analysis API
#'
#' Public interface for statistical analysis functions.
#' This is the main entry point for all statistical operations.

# Create shared logger for the stats feature
stats_logger <- create_module_logger("STATS")

# =============================================================================
# DATA VALIDATION API
# =============================================================================

#' Validate statistical data
#' @param data The dataset to validate
#' @param dep_var The dependent variable name (optional)
#' @return TRUE if data is valid for statistical analysis
validate_stats_data <- function(data, dep_var = NULL) {
    is_stats_data_valid(data, dep_var)
}

#' Handle invalid data with user notification
#' @param message Warning message to display to user
#' @return Minimal data frame schema
handle_invalid_data <- function(message = "No data available for statistical analysis. Please check your filter settings.") {
    handle_invalid_stats_data(message)
}

# =============================================================================
# DATA PREPROCESSING API
# =============================================================================

#' Apply data transformations
#' @param data The dataset
#' @param dep_var The dependent variable column name
#' @param transform_type The type of transformation to apply ("none", "log10", "logit")
#' @return The dataset with the transformed dependent variable, or NULL if validation fails
transform_dependent_variable <- function(data, dep_var, transform_type = "none") {
    apply_depvar_transform(data, dep_var, transform_type)
}

#' Apply rank transformation to dependent variable
#' @param data The dataset
#' @param dep_col The dependent variable column name
#' @param method The rank transformation method
#' @return List with transformed data and new column name
apply_rank_transformation <- function(data, dep_col, method = "rank") {
    apply_rank_transform(data, dep_col, method)
}

#' Apply custom reference levels for specific variables
#' @param data The dataset to modify
#' @param var_name The variable name to set reference level for
#' @param ref_level The reference level to set
#' @return The dataset with updated reference level, or NULL if invalid
set_reference_level <- function(data, var_name, ref_level) {
    apply_custom_reference_level(data, var_name, ref_level)
}

#' Build and prepare statistical dataset
#' @param data The dataset to use for statistical analysis
#' @param average_across Whether to average across conditions
#' @return Processed statistical dataset
prepare_stats_dataset <- function(data, average_across = FALSE) {
    stats_logger("DEBUG", sprintf("prepare_stats_dataset called: average_across=%s", average_across))
    
    # Check if build_stats_data function exists
    if (!exists("build_stats_data")) {
        stats_logger("ERROR", "build_stats_data function not found in current environment")
        stop("build_stats_data function not found")
    }
    
    stats_logger("DEBUG", "Calling build_stats_data with provided data")
    result <- build_stats_data(data, average_across)
    stats_logger("DEBUG", sprintf("build_stats_data returned %d rows", nrow(result)))
    return(result)
}

# =============================================================================
# LINEAR MIXED MODELS API
# =============================================================================

#' Build LMM formula from variables
#' @param dep_var The dependent variable name
#' @param indep_inter_vars Independent interaction variables
#' @param indep_vars Independent variables
#' @return Formula object for LMM
create_lmm_formula <- function(dep_var, indep_inter_vars, indep_vars) {
    build_lmm_formula(dep_var, indep_inter_vars, indep_vars)
}

#' Fit LMM model with data preparation
#' @param data The dataset
#' @param formula The model formula
#' @param dep_var The dependent variable name
#' @param do_centering Whether to center predictors
#' @param do_scaling Whether to scale predictors
#' @return Fitted LMM model
fit_mixed_model <- function(data, formula, dep_var, do_centering = FALSE, do_scaling = FALSE) {
    # Validate data is ready for LMM
    validate_ready_for_lmm(data)
    
    # Center and/or scale predictors if requested
    if (do_centering || do_scaling) {
        data <- center_predictors(data, formula, dep_var, do_scaling)
    }
    
    # Fit the model
    fit_lmm(data, formula)
}

# =============================================================================
# POST-HOC ANALYSIS API
# =============================================================================

#' Run complete post-hoc analysis
#' @param lmm The fitted LMM model
#' @param data The dataset
#' @param indep_inter_vars Independent interaction variables
#' @param indep_vars Independent variables
#' @param force_all Whether to test all effects regardless of significance
#' @param adjust_method Method for p-value adjustment (default: "tukey")
#' @param filter_meaningful Whether to filter to meaningful comparisons only
#' @param format_pvalues Whether to format p-values with bold significance
#' @return List with post-hoc results and metadata
run_posthoc_analysis <- function(lmm, data, indep_inter_vars, indep_vars, 
                                 force_all = FALSE, adjust_method = "tukey",
                                 filter_meaningful = FALSE, format_pvalues = TRUE) {
    # Prepare post-hoc analysis
    posthoc_results <- prepare_posthoc_analysis(lmm, data, indep_inter_vars, indep_vars, force_all, adjust_method)
    
    if (is.null(posthoc_results)) {
        return(list(
            message = "No significant effects found for post-hoc analysis.",
            results = NULL,
            metadata = NULL
        ))
    }
    
    # Post-process results
    final_df <- postprocess_posthoc_results(
        posthoc_results$combined$df, 
        filter_meaningful, 
        if (format_pvalues) c("p.value", "p_adjusted") else NULL
    )
    
    return(list(
        results = final_df,
        metadata = list(
            all_indep_vars = posthoc_results$all_indep_vars,
            posthoc_results = posthoc_results$posthoc_results,
            pretty_names = posthoc_results$combined$pretty_names
        )
    ))
}

# =============================================================================
# PAIRED T-TEST API
# =============================================================================

#' Run paired t-test analysis
#' @param lmm_posthoc List from prepare_posthoc_analysis
#' @param data The dataset for paired tests
#' @param dep_var The dependent variable column name
#' @param num_tests Number of tests for Bonferroni correction
#' @return Paired t-test results data frame with assumption_lookup attribute
run_paired_ttest_analysis <- function(lmm_posthoc, data, dep_var, num_tests = 1) {
    compute_paired_ttest_table(lmm_posthoc, data, dep_var, num_tests)
}

# =============================================================================
# DIFFERENCE-IN-DIFFERENCES API
# =============================================================================

#' Run Difference-in-Differences analysis
#' @param lmm The fitted LMM model
#' @param data The dataset used for the model
#' @param correction_method Method for multiple comparison correction (default: "holm")
#' @return List with DiD results and metadata
run_did_analysis <- function(lmm, data, correction_method = "holm") {
    compute_did_analysis(lmm, data, correction_method)
}

# =============================================================================
# UI HELPER API
# =============================================================================

#' Create assumption check button for paired t-test table
#' @param id The assumption ID
#' @param label Button label
#' @return HTML button string
create_assumption_button <- function(id, label = "Check assumptions") {
    build_assumption_button(id, label)
}

#' Create LMM assumption check button
#' @param label Button label
#' @return HTML button string
create_lmm_assumption_button <- function(label = "Check LMM Assumptions") {
    build_lmm_assumption_button(label)
}

#' Create DiD analysis button
#' @param label Button label
#' @return HTML button string
create_did_button <- function(label = "Run DiD Analysis") {
    build_did_button(label)
}

# =============================================================================
# UTILITY API
# =============================================================================

#' Format numeric data frame columns
#' @param df The data frame to format
#' @param digits Number of decimal places
#' @return Formatted data frame
format_numeric_columns <- function(df, digits = 4) {
    set_digits(df, digits)
}

#' Format data frame with bold significant p-values
#' @param df The data frame to format
#' @param p_value_column The name of the p-value column
#' @param digits Number of decimal places
#' @param alpha Significance level for bold formatting
#' @return Formatted data frame with bold significant p-values
format_with_bold_pvalues <- function(df, p_value_column, digits = 4, alpha = 0.05) {
    set_digits_with_bold_pvalues(df, p_value_column, digits, alpha)
}
