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
#' @param data The dataset to use for statistical analysis
#' @param average_across Whether to average across conditions
#' @return Processed statistical dataset
build_stats_data <- function(data, average_across = FALSE) {
    stats_logger("DEBUG", sprintf("build_stats_data called: average_across=%s", average_across))
    
    # Validate required data is provided
    if (is.null(data) || nrow(data) == 0) {
        stats_logger("ERROR", "No data provided to build_stats_data")
        stop("No data provided to build_stats_data")
    }
    
    stats_logger("DEBUG", sprintf("Data provided with %d rows", nrow(data)))
    
    # Apply summarize_across_conditions if requested (works for both dataset types)
    dt <- if (average_across) {
        stats_logger("DEBUG", "Applying summarize_across_conditions")
        # Check if gait module is available and has the function
        if (exists("gait") && is.function(gait$summarize_across_conditions)) {
            result <- gait$summarize_across_conditions(data)
        } else {
            stats_logger("ERROR", "gait$summarize_across_conditions function not available")
            stop("gait$summarize_across_conditions function not available")
        }
        stats_logger("DEBUG", sprintf("summarize_across_conditions returned %d rows", nrow(result)))
        result
    } else {
        stats_logger("DEBUG", "Using data as-is")
        data
    }

    if (!is_stats_data_valid(dt)) {
        return(handle_invalid_stats_data())
    }

    apply_reference_levels(dt)
}
