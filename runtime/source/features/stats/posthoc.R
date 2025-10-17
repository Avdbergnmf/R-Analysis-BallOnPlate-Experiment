#' Post-hoc Analysis Functions
#'
#' Functions for running and processing post-hoc analyses using emmeans

#' Run post-hoc analysis for a single effect
#' @param lmm The fitted LMM model
#' @param effect The effect name
#' @param all_indep_vars All independent variables
#' @param data The dataset
#' @param adjust_method Method for p-value adjustment (default: "tukey")
#' @return Post-hoc results data frame or NULL
run_posthoc_for_effect <- function(lmm, effect, all_indep_vars, data, adjust_method = "tukey") {
    posthoc <- tryCatch(
        {
            if (grepl(":", effect)) {
                parts <- unlist(strsplit(effect, ":"))
                vars <- sapply(parts, function(p) clean_effect_name(p, all_indep_vars))
                if (length(vars) == 0 || any(vars == "")) {
                    return(NULL)
                }
                emmeans::emmeans(lmm, specs = vars)
            } else {
                var <- clean_effect_name(effect, all_indep_vars)
                # Fallback: if cleaned name not in data, but the raw effect name is a column, use it
                if (!is.null(effect) && !(var %in% names(data)) && effect %in% names(data)) {
                    var <- effect
                }
                if (is.null(var) || var == "" || !(var %in% names(data))) {
                    return(NULL)
                }
                emmeans::emmeans(lmm, specs = var)
            }
        },
        error = function(e) {
            if (grepl("No variable named", conditionMessage(e), fixed = FALSE)) {
                return(NULL)
            }
            stop(e)
        }
    )
    if (is.null(posthoc)) {
        return(NULL)
    }
    res <- tryCatch(
        emmeans::contrast(posthoc, method = "pairwise", adjust = adjust_method),
        error = function(e) {
            if (grepl("No variable named", conditionMessage(e), fixed = FALSE)) {
                return(NULL)
            }
            stop(e)
        }
    )
    if (is.null(res)) {
        return(NULL)
    }
    as.data.frame(summary(res))
}

#' Combine post-hoc results from multiple effects
#' @param posthoc_results List of post-hoc results
#' @param all_indep_vars All independent variables
#' @param data The dataset
#' @return List with combined data frame and pretty names
combine_posthoc_results <- function(posthoc_results, all_indep_vars, data) {
    if (length(posthoc_results) == 0) {
        return(NULL)
    }

    # Pretty effect names for display
    pretty_effect_names <- sapply(names(posthoc_results), function(effect) {
        if (grepl(":", effect)) {
            parts <- unlist(strsplit(effect, ":"))
            pretty_parts <- sapply(parts, function(p) get_pretty_factor_labels(p, all_indep_vars, data))
            paste(pretty_parts, collapse = " × ")
        } else {
            get_pretty_factor_labels(effect, all_indep_vars, data)
        }
    })

    # Combine and group by pretty effect
    unique_cleaned <- unique(pretty_effect_names)
    final_results <- lapply(unique_cleaned, function(name) {
        idx <- which(pretty_effect_names == name)
        do.call(rbind, posthoc_results[idx])
    })
    names(final_results) <- unique_cleaned

    final_df <- do.call(rbind, final_results)
    final_df <- cbind(Effect = rep(names(final_results), sapply(final_results, nrow)), final_df)
    rownames(final_df) <- NULL

    list(df = final_df, pretty_names = pretty_effect_names)
}

#' Check if a comparison is meaningful (at least one level matches)
#' @param contrast_str The contrast string
#' @param effect_name The effect name
#' @return TRUE if meaningful, FALSE otherwise
is_meaningful_comparison_row <- function(contrast_str, effect_name) {
    parts <- strsplit(contrast_str, " - ")[[1]] # Split the contrast into left and right sides

    has_two_parts <- length(parts) == 2 # means we can parse it
    if (!has_two_parts) {
        return(TRUE)
    } # just keep it (shows up in the table)

    # For main effects, just check if the variable names match
    is_main_effect <- !grepl(" × ", effect_name)
    if (is_main_effect) {
        return(TRUE)
    }

    # For interactions, check if at least one level matches
    left_levels <- strsplit(parts[1], " ")[[1]]
    right_levels <- strsplit(parts[2], " ")[[1]]
    for (i in seq_along(left_levels)) {
        if (left_levels[i] == right_levels[i]) {
            return(TRUE)
        }
    }
    FALSE
}

#' Filter post-hoc results to show only meaningful comparisons
#' @param final_df The post-hoc results data frame
#' @return Filtered data frame
filter_posthoc_meaningful <- function(final_df) {
    if (is.null(final_df) || !all(c("contrast", "Effect") %in% names(final_df))) {
        return(final_df)
    }
    keep <- sapply(seq_len(nrow(final_df)), function(i) {
        is_meaningful_comparison_row(as.character(final_df$contrast[i]), as.character(final_df$Effect[i]))
    })
    out <- final_df[keep, , drop = FALSE]
    if (nrow(out) == 0) data.frame(Message = "No meaningful comparisons found after filtering.") else out
}

#' Prepare emmeans results for post-hoc analysis
#' @param lmm The fitted LMM model
#' @param data The dataset
#' @param indep_inter_vars Independent interaction variables
#' @param indep_vars Independent variables
#' @param force_all Whether to test all effects regardless of significance
#' @param adjust_method Method for p-value adjustment (default: "tukey")
#' @return List with posthoc_results, combined results, and metadata
prepare_posthoc_analysis <- function(lmm, data, indep_inter_vars, indep_vars, force_all = FALSE, adjust_method = "tukey") {
    results <- summary(lmm)
    effects_all <- select_effects_to_test(results$coefficients, force_all)
    all_indep_vars <- c(indep_inter_vars, indep_vars)

    effects_to_test <- dedupe_effects_by_base(effects_all, all_indep_vars)

    # Run emmeans for each effect
    posthoc_results <- list()
    for (eff in effects_to_test) {
        res <- run_posthoc_for_effect(lmm, eff, all_indep_vars, data, adjust_method)
        if (!is.null(res)) posthoc_results[[eff]] <- res
    }

    # Combine results
    combined <- combine_posthoc_results(posthoc_results, all_indep_vars, data)
    if (is.null(combined)) {
        return(NULL) # No significant effects found
    }

    return(list(
        posthoc_results = posthoc_results,
        combined = combined,
        all_indep_vars = all_indep_vars
    ))
}
