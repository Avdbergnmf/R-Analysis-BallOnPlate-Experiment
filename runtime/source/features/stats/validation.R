#' Data Validation Functions
#'
#' Functions for validating statistical data and handling edge cases

#' Create minimal data frame schema for empty statistical data
#' @return A minimal data frame with expected statistical data structure
minimal_stats_schema <- function() {
    data.frame(
        participant = character(0),
        trialNum = numeric(0),
        condition = character(0),
        phase = character(0),
        foot = character(0)
    )
}

#' Handle empty statistical data case with user notification
#' @param message Warning message to display to user
#' @return Minimal data frame schema
handle_invalid_stats_data <- function(message = "No data available for statistical analysis. Please check your filter settings.") {
    showNotification(message, type = "warning", duration = 5)
    minimal_stats_schema()
}

#' Check if dependent variable is valid for analysis
#' @param dep_var The dependent variable name
#' @param data The dataset containing the variable
#' @return TRUE if dependent variable is valid
is_dep_var_valid <- function(dep_var, data) {
    !is.null(dep_var) && nzchar(dep_var) && dep_var %in% names(data)
}

#' Check if data is valid for statistical analysis
#' @param data The dataset to validate
#' @param dep_var The dependent variable name (optional)
#' @return TRUE if data is valid for statistical analysis
is_stats_data_valid <- function(data, dep_var = NULL) {
    # Basic data structure validation
    if (is.null(data) || nrow(data) == 0) {
        return(FALSE)
    }
    if (!("participant" %in% names(data))) {
        return(FALSE)
    }

    # Optional dependent variable validation
    if (!is.null(dep_var)) {
        return(is_dep_var_valid(dep_var, data))
    }

    TRUE
}

#' Validate data is ready for LMM analysis
#' @param data The dataset to validate
#' @return TRUE if valid, throws error if not
validate_ready_for_lmm <- function(data) {
    obs_per_participant <- data %>%
        dplyr::group_by(participant) %>%
        dplyr::summarise(n_obs = n(), .groups = "drop")
    max_obs_per_participant <- max(obs_per_participant$n_obs, na.rm = TRUE)
    if (max_obs_per_participant <= 1) {
        stop("ERROR: Each participant has only 1 observation. Mixed-effects models require multiple observations per participant.")
    }
    invisible(TRUE)
}
