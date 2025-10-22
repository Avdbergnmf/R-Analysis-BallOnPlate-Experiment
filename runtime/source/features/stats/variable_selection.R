#' Variable Selection Utilities for Statistical Analysis
#'
#' Provides pattern matching and variable selection functionality for statistical modules.
#' Supports wildcards, exclusions, AND/OR logic for flexible variable selection.
#'
#' @author Alex van den Berg
#' @version 1.0

# =============================================================================
# VARIABLE SELECTION FUNCTIONS
# =============================================================================

#' Match variables using advanced pattern matching
#'
#' Supports wildcards (*), exclusions (!), AND/OR logic for flexible variable selection.
#'
#' @param pattern String pattern to match against variable names
#' @param variables Vector of variable names to search in
#' @return Vector of matching variable names
#'
#' @details
#' Pattern matching supports:
#' - Substring matching: "task" matches any variable containing "task"
#' - Wildcards: * matches any characters
#' - Exclusions: !pattern excludes matching variables
#' - AND logic: pattern1 AND pattern2 (both must match)
#' - OR logic: pattern1 OR pattern2 (either can match)
#'
#' @examples
#' variables <- c("task.q_mean", "task.p_sd", "step.mean", "step.sd", "complexity.var")
#' match_variables("task", variables) # Returns task variables (substring match)
#' match_variables("_sd", variables) # Returns variables containing _sd
#' match_variables("task AND _sd", variables) # Returns task variables containing _sd
#' match_variables("task OR step", variables) # Returns task or step variables
#' match_variables("!_sd", variables) # Returns all except _sd variables
match_variables <- function(pattern, variables, depth = 0) {
    # Prevent infinite recursion
    if (depth > 10) {
        warning("Maximum recursion depth reached in match_variables")
        return(character(0))
    }

    if (is.null(pattern) || nchar(pattern) == 0) {
        return(character(0))
    }

    # Handle AND logic (multiple patterns that must all match)
    if (grepl(" AND ", pattern, ignore.case = TRUE)) {
        patterns <- strsplit(pattern, " AND ", fixed = TRUE)[[1]]
        patterns <- trimws(patterns)

        # Start with all variables
        result_vars <- variables

        # Apply each pattern (intersection)
        for (p in patterns) {
            if (nchar(p) > 0) {
                pattern_matches <- match_variables(p, result_vars, depth + 1)
                result_vars <- intersect(result_vars, pattern_matches)
            }
        }
        return(result_vars)
    }

    # Handle OR logic (multiple patterns where any can match)
    if (grepl(" OR ", pattern, ignore.case = TRUE)) {
        patterns <- strsplit(pattern, " OR ", fixed = TRUE)[[1]]
        patterns <- trimws(patterns)

        # Start with empty result
        result_vars <- character(0)

        # Apply each pattern (union)
        for (p in patterns) {
            if (nchar(p) > 0) {
                pattern_matches <- match_variables(p, variables, depth + 1)
                result_vars <- union(result_vars, pattern_matches)
            }
        }
        return(result_vars)
    }

    # Handle exclusion patterns (starting with !)
    if (substr(pattern, 1, 1) == "!") {
        exclusion_pattern <- substr(pattern, 2, nchar(pattern))
        # Convert wildcards to regex for exclusion
        regex_pattern <- gsub("\\*", ".*", exclusion_pattern)
        # By default, match substrings (not full strings)
        matches <- !grepl(regex_pattern, variables, ignore.case = TRUE)
        return(variables[matches])
    }

    # Handle simple inclusion patterns
    # Convert wildcards to regex
    regex_pattern <- gsub("\\*", ".*", pattern)
    # By default, match substrings (not full strings) - more intuitive
    tryCatch(
        {
            matches <- grepl(regex_pattern, variables, ignore.case = TRUE)
            return(variables[matches])
        },
        error = function(e) {
            warning("Error in pattern matching: ", e$message)
            return(character(0))
        }
    )
}

#' Add variables by pattern to existing selection
#'
#' @param pattern String pattern to match
#' @param available_vars Vector of all available variables
#' @param current_selected Vector of currently selected variables
#' @return Vector of updated selected variables
add_variables_by_pattern <- function(pattern, available_vars, current_selected = character(0)) {
    if (is.null(pattern) || nchar(pattern) == 0) {
        return(current_selected)
    }

    matching_vars <- match_variables(pattern, available_vars)
    new_selected <- unique(c(current_selected, matching_vars))
    return(new_selected)
}

#' Remove variables by pattern from existing selection
#'
#' @param pattern String pattern to match
#' @param current_selected Vector of currently selected variables
#' @return Vector of updated selected variables
remove_variables_by_pattern <- function(pattern, current_selected) {
    if (is.null(pattern) || nchar(pattern) == 0) {
        return(current_selected)
    }

    if (is.null(current_selected) || length(current_selected) == 0) {
        return(character(0))
    }

    matching_vars <- match_variables(pattern, current_selected)
    new_selected <- current_selected[!current_selected %in% matching_vars]
    return(new_selected)
}

#' Get common variable groups for quick selection
#'
#' @param variables Vector of all available variables
#' @return List of variable groups with their patterns
get_variable_groups <- function(variables) {
    groups <- list(
        "Task Variables" = "task*",
        "Gait Variables" = "step* OR pos* OR hip_pos* OR treadmillSpeed*",
        "Complexity Variables" = "complexity*",
        "Questionnaire Variables" = "questionnaire*",
        "Mean Variables" = "*_mean",
        "Standard Deviation Variables" = "*_sd",
        "Coefficient of Variation Variables" = "*_cv",
        "Position Variables" = "*pos*",
        "Step Variables" = "step*",
        "Task Performance Variables" = "task* AND !*_sd AND !*_cv"
    )

    # Filter groups to only include those that have matches
    valid_groups <- list()
    for (name in names(groups)) {
        pattern <- groups[[name]]
        matches <- match_variables(pattern, variables)
        if (length(matches) > 0) {
            valid_groups[[name]] <- list(pattern = pattern, count = length(matches), variables = matches)
        }
    }

    return(valid_groups)
}

#' Validate pattern syntax
#'
#' @param pattern String pattern to validate
#' @return List with validation results
validate_pattern <- function(pattern) {
    result <- list(valid = TRUE, message = "", suggestions = character(0))

    if (is.null(pattern) || nchar(pattern) == 0) {
        result$valid <- FALSE
        result$message <- "Pattern cannot be empty"
        return(result)
    }

    # Check for balanced AND/OR operators
    and_count <- length(gregexpr(" AND ", pattern, ignore.case = TRUE)[[1]])
    or_count <- length(gregexpr(" OR ", pattern, ignore.case = TRUE)[[1]])

    if (and_count > 0 && or_count > 0) {
        result$suggestions <- c(result$suggestions, "Consider using parentheses for complex AND/OR combinations")
    }

    # Check for common patterns
    if (grepl("\\*\\*", pattern)) {
        result$suggestions <- c(result$suggestions, "Double wildcards (**) are redundant, use single (*)")
    }

    # Check for empty patterns in AND/OR
    if (grepl(" AND  AND ", pattern, ignore.case = TRUE) ||
        grepl(" OR  OR ", pattern, ignore.case = TRUE)) {
        result$valid <- FALSE
        result$message <- "Empty patterns detected in AND/OR logic"
        return(result)
    }

    return(result)
}

#' Get pattern matching examples
#'
#' @return List of example patterns with descriptions
get_pattern_examples <- function() {
    examples <- list(
        "Simple patterns" = list(
            "task" = "All variables containing 'task'",
            "_sd" = "All variables containing '_sd'",
            "mean" = "All variables containing 'mean'"
        ),
        "Wildcard patterns" = list(
            "task*" = "All variables starting with 'task'",
            "*_sd" = "All variables ending with '_sd'",
            "*mean*" = "All variables containing 'mean'"
        ),
        "Exclusion patterns" = list(
            "!_sd" = "All variables except those containing '_sd'",
            "!task" = "All variables except those containing 'task'"
        ),
        "AND logic" = list(
            "task AND _sd" = "Task variables that contain '_sd'",
            "step AND mean" = "Step variables that contain 'mean'"
        ),
        "OR logic" = list(
            "task OR step" = "Task or step variables",
            "_mean OR _sd" = "Mean or standard deviation variables"
        ),
        "Complex patterns" = list(
            "task AND !_cv" = "Task variables excluding coefficient of variation",
            "step OR pos OR hip_pos" = "Step, position, or hip position variables"
        )
    )

    return(examples)
}
