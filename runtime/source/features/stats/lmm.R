#' Linear Mixed Models (LMM) Functions
#'
#' Functions for building, fitting, and working with linear mixed models

#' Build LMM formula from variables
#' @param dep_var The dependent variable name
#' @param indep_inter_vars Independent interaction variables
#' @param indep_vars Independent variables
#' @return Formula object for LMM
build_lmm_formula <- function(dep_var, indep_inter_vars, indep_vars) {
    terms <- character(0)
    if (!is.null(indep_inter_vars) && length(indep_inter_vars) > 0) {
        inter <- paste(indep_inter_vars, collapse = "*")
        if (nzchar(inter)) terms <- c(terms, inter)
    }
    if (!is.null(indep_vars) && length(indep_vars) > 0) {
        add <- paste(indep_vars, collapse = "+")
        if (nzchar(add)) terms <- c(terms, add)
    }
    rhs <- paste(c(terms, "(1 | participant)"), collapse = " + ")
    as.formula(paste(dep_var, "~", rhs))
}

#' Fit LMM model
#' @param data The dataset
#' @param formula The model formula
#' @return Fitted LMM model
fit_lmm <- function(data, formula) {
    lmerTest::lmer(formula, data = data, REML = FALSE)
}

#' Build the stats dataset from inputs and finalize it (validate + reference levels)
#' @param use_summarized Whether to use summarized data
#' @param average_across Whether to average across conditions
#' @return Processed statistical dataset
build_stats_data <- function(use_summarized, average_across) {
    stats_logger("DEBUG", sprintf("build_stats_data called: use_summarized=%s, average_across=%s", use_summarized, average_across))
    
    # Check if required functions exist
    if (!exists("get_current_mu_dyn_long")) {
        stats_logger("ERROR", "get_current_mu_dyn_long function not found in current environment")
        stop("get_current_mu_dyn_long function not found")
    }
    if (!exists("get_current_filtered_params")) {
        stats_logger("ERROR", "get_current_filtered_params function not found in current environment")
        stop("get_current_filtered_params function not found")
    }
    
    stats_logger("DEBUG", "Required functions found, proceeding with data loading")
    
    dt <- if (use_summarized) {
        stats_logger("DEBUG", "Loading summarized data via get_current_mu_dyn_long")
        x <- get_current_mu_dyn_long()
        stats_logger("DEBUG", sprintf("get_current_mu_dyn_long returned %d rows", nrow(x)))
        if (average_across) {
            stats_logger("DEBUG", "Applying summarize_across_conditions")
            result <- summarize_across_conditions(x)
            stats_logger("DEBUG", sprintf("summarize_across_conditions returned %d rows", nrow(result)))
            result
        } else {
            x
        }
    } else {
        stats_logger("DEBUG", "Loading filtered params via get_current_filtered_params")
        result <- get_current_filtered_params()
        stats_logger("DEBUG", sprintf("get_current_filtered_params returned %d rows", nrow(result)))
        result
    }

    if (!is_stats_data_valid(dt)) {
        return(handle_invalid_stats_data())
    }

    apply_reference_levels(dt)
}
