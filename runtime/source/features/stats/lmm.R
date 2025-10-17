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
    dt <- if (use_summarized) {
        x <- get_mu_dyn_long()
        if (average_across) summarize_across_conditions(x) else x
    } else {
        filteredParams()
    }

    if (!is_stats_data_valid(dt)) {
        return(handle_invalid_stats_data())
    }

    apply_reference_levels(dt)
}
