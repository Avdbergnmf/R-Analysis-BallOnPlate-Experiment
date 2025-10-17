#' Data Preprocessing Functions
#'
#' Functions for transforming and preparing data for statistical analysis

#' Apply rank transformation to dependent variable
#' @param data The dataset
#' @param dep_col The dependent variable column name
#' @param method The rank transformation method
#' @return List with transformed data and new column name
apply_rank_transform <- function(data, dep_col, method = "rank") {
    ranked_col_name <- ".dep_ranked"
    x <- data[[dep_col]]
    r <- rank(x, ties.method = "average", na.last = "keep")
    n <- sum(!is.na(x))

    if (n > 0) {
        if (identical(method, "rank")) {
            data[[ranked_col_name]] <- r
        } else if (identical(method, "fraction")) {
            data[[ranked_col_name]] <- r / (n + 1)
        } else if (identical(method, "vdw")) {
            data[[ranked_col_name]] <- stats::qnorm(r / (n + 1))
        } else if (identical(method, "blom")) {
            data[[ranked_col_name]] <- stats::qnorm((r - 3 / 8) / (n + 1 / 4))
        } else if (identical(method, "tukey")) {
            data[[ranked_col_name]] <- stats::qnorm((r - 1 / 3) / (n + 1 / 3))
        } else {
            data[[ranked_col_name]] <- r
        }
        return(list(data = data, dep_col = ranked_col_name))
    }

    return(list(data = data, dep_col = dep_col))
}

#' Apply transformation to dependent variable
#' @param data The dataset
#' @param dep_var The dependent variable column name
#' @param transform_type The type of transformation to apply ("none", "log10", "logit")
#' @return The dataset with the transformed dependent variable, or NULL if validation fails
apply_depvar_transform <- function(data, dep_var, transform_type = "none") {
    if (transform_type == "none" || is.null(transform_type)) {
        return(data)
    }

    dep_var_values <- data[[dep_var]]

    if (transform_type == "log10") {
        # Check if all values are positive for log10 transformation
        if (any(dep_var_values <= 0, na.rm = TRUE)) {
            if (exists("showNotification")) {
                showNotification(
                    "Warning: Some values are <= 0. Cannot apply log10 transformation. Consider adding a constant or using a different transformation.",
                    type = "warning"
                )
            }
            return(NULL)
        }
        # Apply log10 transformation
        data[[dep_var]] <- log10(data[[dep_var]])
    } else if (transform_type == "logit") {
        # Check if all values are between 0 and 1 for logit transformation
        if (any(dep_var_values <= 0 | dep_var_values >= 1, na.rm = TRUE)) {
            if (exists("showNotification")) {
                showNotification(
                    "Warning: Some values are <= 0 or >= 1. Cannot apply logit transformation. Values must be strictly between 0 and 1 (proportions).",
                    type = "warning"
                )
            }
            return(NULL)
        }
        # Apply logit transformation: log(p / (1 - p))
        data[[dep_var]] <- log(data[[dep_var]] / (1 - data[[dep_var]]))
    }

    return(data)
}

#' Apply custom reference levels for specific variables
#' @param data The dataset to modify
#' @param var_name The variable name to set reference level for
#' @param ref_level The reference level to set
#' @return The dataset with updated reference level, or NULL if invalid
apply_custom_reference_level <- function(data, var_name, ref_level) {
    # Validate inputs
    if (is.null(var_name) || !nzchar(var_name) || !var_name %in% names(data)) {
        return(data)
    }

    if (is.null(ref_level) || !nzchar(as.character(ref_level))) {
        return(data)
    }

    # Check if the reference level exists in the data
    available_levels <- unique(data[[var_name]])
    if (!ref_level %in% available_levels) {
        warning(sprintf(
            "Reference level '%s' not found in variable '%s'. Available levels: %s",
            ref_level, var_name,
            paste(available_levels, collapse = ", ")
        ))
        return(data)
    }

    # Set the reference level
    data[[var_name]] <- relevel(factor(data[[var_name]]), ref = as.character(ref_level))

    return(data)
}

#' Apply reference levels to factor variables in a dataset
#' @param data The dataset to modify
#' @return The dataset with proper reference levels set for factors
apply_reference_levels <- function(data) {
    # Check if reference_levels is available
    if (!exists("reference_levels", envir = .GlobalEnv)) {
        return(data)
    }
    
    # Apply reference levels to each variable that has a defined reference level
    for (var_name in names(reference_levels)) {
        if (var_name %in% names(data)) {
            # Check if the variable exists and has the specified reference level
            if (reference_levels[[var_name]] %in% unique(data[[var_name]])) {
                data[[var_name]] <- relevel(factor(data[[var_name]]), ref = reference_levels[[var_name]])
                # cat(sprintf("[INFO] Set reference level for %s to '%s'\n", var_name, reference_levels[[var_name]]))
            } else {
                warning(sprintf(
                    "Reference level '%s' not found in variable '%s'. Available levels: %s",
                    reference_levels[[var_name]], var_name,
                    paste(unique(data[[var_name]]), collapse = ", ")
                ))
            }
        }
    }

    return(data)
}

#' Center and scale numeric predictors
#' @param data The dataset
#' @param formula The model formula
#' @param dep_var The dependent variable name
#' @param do_scaling Whether to scale (standardize) the variables
#' @return The dataset with centered/scaled predictors
center_predictors <- function(data, formula, dep_var, do_scaling) {
    numeric_cols <- sapply(data, is.numeric)
    model_vars <- setdiff(all.vars(formula), dep_var)
    numeric_vars <- intersect(names(data)[numeric_cols], model_vars)
    excluded <- c("trialNum", "trialNumWithinCondition", "trialNumWithoutPractice", "conditionNumber", "slice_index")
    numeric_vars <- setdiff(numeric_vars, excluded)
    if (length(numeric_vars) == 0) {
        return(data)
    }
    data[numeric_vars] <- lapply(data[numeric_vars], function(x) {
        as.numeric(scale(x, center = TRUE, scale = do_scaling))
    })
    data
}
