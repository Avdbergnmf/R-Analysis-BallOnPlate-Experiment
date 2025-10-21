#' Formatting and UI Helper Functions
#'
#' Functions for formatting results and creating UI elements

#' Format numeric columns in a data frame
#' @param df The data frame to format
#' @param digits Number of decimal places
#' @return Formatted data frame
set_digits <- function(df, digits = 4) {
    # Format the numbers to display more digits
    df <- df %>%
        mutate(across(where(is.numeric), ~ format(round(.x, digits = digits), nsmall = digits)))
    return(df)
}

#' Format numeric columns with bold p-values
#' @param df The data frame to format
#' @param p_value_column The name of the p-value column
#' @param digits Number of decimal places
#' @param alpha Significance level for bold formatting
#' @return Formatted data frame with bold significant p-values
set_digits_with_bold_pvalues <- function(df, p_value_column, digits = 4, alpha = 0.05) {
    # Format all numeric columns
    df <- df %>%
        mutate(across(where(is.numeric), ~ format(round(.x, digits = digits), nsmall = digits)))

    # Make significant p-values bold
    if (p_value_column %in% names(df)) {
        df[[p_value_column]] <- sapply(df[[p_value_column]], function(p_val) {
            if (is.na(p_val) || p_val == "") {
                return(p_val)
            }
            # Strip any HTML tags and convert safely to numeric (avoid warnings)
            p_str <- as.character(p_val)
            p_clean <- gsub("<[^>]+>", "", p_str)
            p_num <- suppressWarnings(as.numeric(p_clean))
            if (!is.na(p_num) && p_num < alpha) {
                return(paste0("<strong>", p_val, "</strong>"))
            } else {
                return(p_val)
            }
        })
    }

    return(df)
}

#' Apply post-processing to final post-hoc results (filtering and p-value formatting)
#' @param final_df The results dataframe
#' @param filter_posthoc Whether to filter meaningful results
#' @param p_value_cols Vector of p-value column names to format
#' @return Post-processed dataframe
postprocess_posthoc_results <- function(final_df, filter_posthoc = FALSE, p_value_cols = NULL) {
    # Optional filtering
    if (filter_posthoc) {
        final_df <- filter_posthoc_meaningful(final_df)
    }

    # Format p-values
    if (!is.null(p_value_cols)) {
        for (col in p_value_cols) {
            if (col %in% names(final_df)) {
                final_df <- set_digits_with_bold_pvalues(final_df, col)
            }
        }
    }

    final_df
}

#' Build assumption check button for paired t-test table
#' @param id The assumption ID
#' @param label Button label
#' @return HTML button string
build_assumption_button <- function(id, label = "Check assumptions") {
    sprintf(
        "<button type='button' class='btn btn-sm btn-secondary' onclick=\"Shiny.setInputValue('ttest_assumption_row', '%s', {priority: 'event'})\">%s</button>",
        id,
        label
    )
}

#' Build LMM assumption check button
#' @param label Button label
#' @return HTML button string
build_lmm_assumption_button <- function(label = "Check LMM Assumptions") {
    sprintf(
        "<button type='button' class='btn btn-sm btn-primary' onclick=\"Shiny.setInputValue('show_lmm_assumptions', Math.random(), {priority: 'event'})\">%s</button>",
        label
    )
}

#' Build DiD analysis button for UI
#' @param label Button label
#' @return HTML button string
build_did_button <- function(label = "Run DiD Analysis") {
    sprintf(
        "<button type='button' class='btn btn-sm btn-info' onclick=\"Shiny.setInputValue('show_did_analysis', Math.random(), {priority: 'event'})\">%s</button>",
        label
    )
}
