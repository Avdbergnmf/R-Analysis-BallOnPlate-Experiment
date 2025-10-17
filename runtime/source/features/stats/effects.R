#' Effect Processing Functions
#'
#' Functions for cleaning and processing effect names from statistical models

#' Escape special regex characters in a string
#' @param str The string to escape
#' @return The escaped string
escape_regex_chars <- function(str) {
    gsub("([.^$*+?()[\\]|])", "\\\\\\1", str)
}

#' Clean effect names by extracting base variable names from factor effects
#' @param effect_name The effect name from the model (e.g., "conditionperturbation")
#' @param input_vars Vector of input variable names to match against
#' @return The cleaned base variable name (e.g., "condition")
clean_effect_name <- function(effect_name, input_vars) {
    # Define factor levels that appear in effect names
    factor_levels <- c(
        # Boolean levels
        "TRUE", "FALSE",
        # Polynomial contrasts
        ".L", ".Q", ".C",
        # Numeric levels
        "[0-9.]+",
        # Condition levels
        c("control", "perturbation", "perturbation_visualization"),
        # Phase levels
        c("baseline", "training", "retention", "transfer", "washout", 
          "familiarisation_walk", "familiarisation_stand", "familiarisation_training")
    )

    # Handle edge cases
    if (is.null(effect_name) || effect_name == "" || length(effect_name) == 0) {
        return("")
    }

    # If effect_name is already in input_vars, return it as is
    if (effect_name %in% input_vars) {
        return(effect_name)
    }

    # Try to match against actual input variables first (most specific match first)
    # Sort input_vars by length (longest first) to ensure we match the most specific variable name
    sorted_vars <- input_vars[order(nchar(input_vars), decreasing = TRUE)]

    for (var in sorted_vars) {
        # Escape special regex characters in the variable name
        escaped_var <- escape_regex_chars(var)
        # Use word boundary or end of string to ensure we match the complete variable name
        # Avoid matching substrings of the variable name by requiring the rest to be recognized factor levels
        pattern <- paste0("^", escaped_var, "(?=", paste(factor_levels, collapse = "|"), "|$)")
        if (grepl(pattern, effect_name, perl = TRUE)) {
            return(var)
        }
    }

    # If no match found but the effect_name looks like a simple variable name
    # (no factor levels attached), return it as-is
    if (!grepl(paste(factor_levels, collapse = "|"), effect_name)) {
        return(effect_name)
    }

    # Fallback: try to match common factor patterns
    factor_pattern <- paste(factor_levels, collapse = "|")
    base_var_match <- gsub(paste0("^(.*?)(", factor_pattern, ")$"), "\\1", effect_name)

    # If the pattern didn't change anything, return the original name
    if (base_var_match == effect_name) {
        return(effect_name)
    }

    return(base_var_match)
}

#' Create pretty labels for factor effects using predefined mappings
#' @param effect_name The effect name from the model
#' @param input_vars Vector of input variable names
#' @param data The dataset to check factor levels
#' @return Pretty formatted effect name
get_pretty_factor_labels <- function(effect_name, input_vars, data = NULL) {
    ensure_global_data_initialized()

    # Try to match against actual input variables
    for (var in input_vars) {
        # Escape special regex characters in the variable name
        escaped_var <- escape_regex_chars(var)
        if (grepl(paste0("^", escaped_var), effect_name)) {
            # Extract the level part
            level <- gsub(paste0("^", escaped_var), "", effect_name)

            # For condition variable, use the condition map
            if (var == "condition" && level %in% names(condition_map)) {
                return(var) # Just return "condition" instead of "conditionP"
            }

            # For other variables, check if they're factors and get their levels
            if (!is.null(data) && var %in% names(data) && is.factor(data[[var]])) {
                # Get the actual levels from the data
                var_levels <- levels(data[[var]])
                if (level %in% var_levels) {
                    # Return just the base variable name for factor variables
                    return(var)
                }
            }

            # If no special handling, return the original effect name
            return(effect_name)
        }
    }

    # For other variables, return as is
    return(effect_name)
}

#' Clean effect name to create grouping key
#' @param effect The effect name
#' @param all_indep_vars All independent variables
#' @return Cleaned effect key
clean_effect_key <- function(effect, all_indep_vars) {
    if (grepl(":", effect)) {
        parts <- unlist(strsplit(effect, ":"))
        base <- sapply(parts, function(p) clean_effect_name(p, all_indep_vars))
        paste(sort(base), collapse = " × ")
    } else {
        clean_effect_name(effect, all_indep_vars)
    }
}

#' Deduplicate effects by base variable name
#' @param effects Vector of effect names
#' @param all_indep_vars All independent variables
#' @return Deduplicated effect names
dedupe_effects_by_base <- function(effects, all_indep_vars) {
    processed <- character(0)
    out <- character(0)
    for (e in effects) {
        key <- clean_effect_key(e, all_indep_vars)
        if (!(key %in% processed)) {
            processed <- c(processed, key)
            out <- c(out, e)
        }
    }
    out
}

#' Select effects to test for post-hoc analysis
#' @param coefficients Model coefficients matrix
#' @param force_all Whether to force testing all effects
#' @return Vector of effect names to test
select_effects_to_test <- function(coefficients, force_all = FALSE) {
    all_effects <- setdiff(rownames(coefficients), "(Intercept)")
    if (force_all) {
        return(unique(all_effects))
    }
    sig <- rownames(coefficients)[coefficients[, "Pr(>|t|)"] < 0.05]
    unique(setdiff(sig, "(Intercept)"))
}
