#' Statistical Analysis Utilities
#'
#' This file contains helper functions for statistical data processing
#' used in page10_statistics.Rmd to make the code more readable and maintainable.

# =============================================================================
# DATA VALIDATION FUNCTIONS
# =============================================================================

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

# =============================================================================
# PREPROCESSING
# =============================================================================

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
            showNotification(
                "Warning: Some values are <= 0. Cannot apply log10 transformation. Consider adding a constant or using a different transformation.", 
                type = "warning"
            )
            return(NULL)
        }
        # Apply log10 transformation
        data[[dep_var]] <- log10(data[[dep_var]])
        
    } else if (transform_type == "logit") {
        # Check if all values are between 0 and 1 for logit transformation
        if (any(dep_var_values <= 0 | dep_var_values >= 1, na.rm = TRUE)) {
            showNotification(
                "Warning: Some values are <= 0 or >= 1. Cannot apply logit transformation. Values must be strictly between 0 and 1 (proportions).", 
                type = "warning"
            )
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

# =============================================================================
# LMM HELPERS
# =============================================================================

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

fit_lmm <- function(data, formula) {
    lmerTest::lmer(formula, data = data, REML = FALSE)
}

# =============================================================================
# POST-HOC HELPERS (emmeans + optional paired t-test)
# =============================================================================

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

#<<<<<<<<<<<<<<<<
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

# =============================================================================
# PAIRED T-TEST HELPERS (alternative to emmeans)
# =============================================================================

#' Parse contrast levels from emmeans contrast string
#' @param contrast_str The contrast string (e.g., "A B - C D")
#' @return List with left and right level vectors, or NULL if parsing fails
parse_contrast_levels <- function(contrast_str) {
    parts <- strsplit(contrast_str, " - ")[[1]]
    if (length(parts) != 2) {
        return(NULL)
    }
    list(
        left = strsplit(parts[1], " ")[[1]],
        right = strsplit(parts[2], " ")[[1]]
    )
}

#' Extract paired values for one contrast from the dataset
#' @param dt2 The dataset
#' @param dep_col The dependent variable column name
#' @param vars_involved Vector of variable names involved in the contrast
#' @param lvl_pair List with left and right level vectors
#' @return List with left_vals, right_vals vectors and participant_ids
extract_paired_values <- function(dt2, dep_col, vars_involved, lvl_pair) {
    left_vals <- c()
    right_vals <- c()
    participant_ids <- c()
    unique_participants <- unique(dt2$participant)

    for (pid in unique_participants) {
        rows_pid <- dt2$participant == pid

        # Build filter for left condition
        rows_left <- rows_pid
        for (j in seq_along(vars_involved)) {
            v <- vars_involved[j]
            if (!(v %in% names(dt2))) {
                rows_left <- FALSE
                break
            }
            rows_left <- rows_left & (as.character(dt2[[v]]) == lvl_pair$left[j])
        }

        # Build filter for right condition
        rows_right <- rows_pid
        for (j in seq_along(vars_involved)) {
            v <- vars_involved[j]
            if (!(v %in% names(dt2))) {
                rows_right <- FALSE
                break
            }
            rows_right <- rows_right & (as.character(dt2[[v]]) == lvl_pair$right[j])
        }

        # Calculate values if both conditions have data
        if (any(rows_left) && any(rows_right)) {
            val_left <- suppressWarnings(mean(dt2[[dep_col]][rows_left], na.rm = TRUE))
            val_right <- suppressWarnings(mean(dt2[[dep_col]][rows_right], na.rm = TRUE))
            if (is.finite(val_left) && is.finite(val_right) && !is.na(val_left) && !is.na(val_right)) {
                left_vals <- c(left_vals, val_left)
                right_vals <- c(right_vals, val_right)
                participant_ids <- c(participant_ids, pid)
            }
        }
    }

    list(left_vals = left_vals, right_vals = right_vals, participant_ids = participant_ids)
}

#' Perform paired t-test for one contrast
#' @param left_vals Vector of left condition values
#' @param right_vals Vector of right condition values
#' @param eff_pretty Pretty effect name
#' @param contrast_str Contrast string
#' @param num_tests Number of tests for Bonferroni correction
#' @return Data frame with t-test results, or NULL if test fails
perform_paired_ttest <- function(left_vals, right_vals, eff_pretty, contrast_str, num_tests = 1) {
    if (length(left_vals) < 2 || length(right_vals) < 2 || length(left_vals) != length(right_vals)) {
        return(NULL)
    }

    tt <- try(stats::t.test(left_vals, right_vals, paired = TRUE), silent = TRUE)
    if (inherits(tt, "try-error")) {
        return(NULL)
    }

    # Calculate effect size (Cohen's d for paired samples)
    differences <- right_vals - left_vals
    cohens_d <- mean(differences) / sd(differences)

    # Apply multiple comparison correction if specified
    corrected_p <- tt$p.value
    if (!is.null(num_tests) && num_tests > 1) {
        corrected_p <- min(1, tt$p.value * num_tests)
    }

    data.frame(
        Effect = eff_pretty,
        Contrast = contrast_str,
        n_pairs = length(left_vals),
        Mean_Diff = mean(differences),
        SD_Diff = sd(differences),
        t_statistic = tt$statistic,
        df = tt$parameter,
        p_value = tt$p.value,
        corrected_p = corrected_p,
        CI_lower = tt$conf.int[1],
        CI_upper = tt$conf.int[2],
        Cohens_d = cohens_d,
        stringsAsFactors = FALSE
    )
}

#' Compute complete paired t-test table with assumption diagnostics
#' @param lmm_posthoc List from prepare_posthoc_analysis
#' @param data The dataset for paired tests
#' @param dep_var The dependent variable column name
#' @param num_tests Number of tests for Bonferroni correction
#' @return Paired t-test results data frame with assumption_lookup attribute
compute_paired_ttest_table <- function(lmm_posthoc, data, dep_var, num_tests = 1) {
    posthoc_data <- lmm_posthoc$combined$df
    posthoc_results <- lmm_posthoc$posthoc_results
    indep_vars <- lmm_posthoc$all_indep_vars

    if (is.null(posthoc_data) || nrow(posthoc_data) == 0 ||
        !all(c("Effect", "contrast") %in% names(posthoc_data))) {
        out <- data.frame(Message = "No valid paired t-test comparisons could be computed.")
        attr(out, "assumption_lookup") <- list()
        return(out)
    }

    # Map pretty names to a representative indep key
    pretty_map <- list()
    for (i in seq_along(lmm_posthoc$combined$pretty_names)) {
        k <- lmm_posthoc$combined$pretty_names[i]
        if (is.null(pretty_map[[k]])) {
            pretty_map[[k]] <- names(posthoc_results)[i]
        }
    }

    ttest_results <- list()
    assumption_lookup <- list()

    for (i in seq_len(nrow(posthoc_data))) {
        eff_pretty <- as.character(posthoc_data$Effect[i])
        contrast_str <- as.character(posthoc_data$contrast[i])
        indep_key <- pretty_map[[eff_pretty]]
        if (is.null(indep_key) || is.na(contrast_str) || !nzchar(contrast_str)) next

        vars_involved_raw <- unlist(strsplit(indep_key, ":"))
        vars_involved <- sapply(vars_involved_raw, function(v) clean_effect_name(v, indep_vars))
        lvl_pair <- parse_contrast_levels(contrast_str)
        if (is.null(lvl_pair)) next

        # Adjust level pairs based on number of variables
        if (length(vars_involved) == 1) {
            lvl_pair$left <- lvl_pair$left[1]
            lvl_pair$right <- lvl_pair$right[1]
        } else if (length(lvl_pair$left) != length(vars_involved) ||
            length(lvl_pair$right) != length(vars_involved)) {
            next
        }

        # Extract paired values for this contrast
        paired_vals <- extract_paired_values(data, dep_var, vars_involved, lvl_pair)
        diffs <- paired_vals$right_vals - paired_vals$left_vals

        # Perform paired t-test
        ttest_result <- perform_paired_ttest(
            paired_vals$left_vals,
            paired_vals$right_vals,
            eff_pretty,
            contrast_str,
            num_tests
        )

        if (!is.null(ttest_result)) {
            row_id <- sprintf("ttest_%03d", length(ttest_results) + 1)
            ttest_result$AssumptionID <- row_id
            ttest_results[[row_id]] <- ttest_result

            # Store assumption diagnostics data
            assumption_lookup[[row_id]] <- list(
                effect = eff_pretty,
                contrast = contrast_str,
                differences = diffs,
                left_vals = paired_vals$left_vals,
                right_vals = paired_vals$right_vals,
                participant_ids = paired_vals$participant_ids,
                vars = vars_involved,
                left_levels = lvl_pair$left,
                right_levels = lvl_pair$right,
                n_pairs = length(diffs)
            )
        }
    }

    if (length(ttest_results) == 0) {
        out <- data.frame(Message = "No valid paired t-test comparisons could be computed.")
        attr(out, "assumption_lookup") <- assumption_lookup
        return(out)
    }

    out <- do.call(rbind, ttest_results)
    out <- cbind(AssumptionID = names(ttest_results), out, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    attr(out, "assumption_lookup") <- assumption_lookup
    out
}

# =============================================================================
# T-TEST ASSUMPTION HELPERS
# =============================================================================

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

# =============================================================================
# POST-HOC PROCESSING
# =============================================================================

set_digits <- function(df, digits = 4) {
    # Format the numbers to display more digits
    df <- df %>%
        mutate(across(where(is.numeric), ~ format(round(.x, digits = digits), nsmall = digits)))
    return(df)
}

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

# =============================================================================
# DIFFERENCE-IN-DIFFERENCES (DiD) ANALYSIS
# =============================================================================

#' Compute Difference-in-Differences (DiD) analysis per phase between conditions
#' 
#' This function performs a targeted DiD analysis using emmeans to compare
#' how changes from baseline phase differ between conditions.
#' 
#' Uses the reference level of 'phase' from the model frame as baseline.
#' Applies Kenward-Roger method for degrees of freedom calculation.
#' Applies Holm correction for multiple comparisons on p-values.
#' 
#' @param lmm The fitted LMM model from lmer
#' @param data The dataset used for the model (not directly used; model frame is extracted)
#' @return A list with four elements:
#'   \item{within_condition}{Tibble of within-condition phase comparisons vs baseline}
#'   \item{within_phase}{Tibble of within-phase condition pairwise comparisons}
#'   \item{did}{Tibble of DiD results comparing condition differences in phase changes}
#'   \item{baseline_used}{Character string of the baseline phase label used}
#'   Returns a list with 'message' element if error occurs.
#' @export
compute_did_analysis <- function(lmm, data) {
    # Check if phase and condition variables exist in the model
    model_vars <- all.vars(formula(lmm))
    
    if (!("phase" %in% model_vars)) {
        return(list(
            message = "Error: 'phase' variable not found in model. DiD analysis requires phase variable.",
            within_condition = NULL,
            within_phase = NULL,
            did = NULL
        ))
    }
    
    if (!("condition" %in% model_vars)) {
        return(list(
            message = "Error: 'condition' variable not found in model. DiD analysis requires condition variable.",
            within_condition = NULL,
            within_phase = NULL,
            did = NULL
        ))
    }
    
    # Get phase levels from the model frame (not from data, which may differ)
    mf <- model.frame(lmm)
    if (!("phase" %in% names(mf))) {
        return(list(
            message = "Error: Could not extract phase from model frame.",
            within_condition = NULL,
            within_phase = NULL,
            did = NULL
        ))
    }
    
    phase_levels <- levels(mf$phase)
    if (is.null(phase_levels)) {
        phase_levels <- unique(as.character(mf$phase))
    }
    
    if (length(phase_levels) < 2) {
        return(list(
            message = "Error: Need at least 2 phases for DiD analysis.",
            within_condition = NULL,
            within_phase = NULL,
            did = NULL
        ))
    }
    
    # Use the reference level (first level for factors) as baseline
    baseline_label <- phase_levels[1]
    
    # Try to compute emmeans
    result <- tryCatch({
        # Step 1a: Get estimated marginal means for phase | condition using Kenward-Roger df
        emm_phase_by_cond <- emmeans::emmeans(lmm, ~ phase | condition, lmer.df = "kenward-roger")
        
        # Step 1b: Get estimated marginal means for condition | phase (for within-phase comparisons)
        emm_cond_by_phase <- emmeans::emmeans(lmm, ~ condition | phase, lmer.df = "kenward-roger")
        
        # Step 2: Get baseline changes (within each condition)
        base_changes <- emmeans::contrast(emm_phase_by_cond, method = "trt.vs.ctrl", ref = baseline_label)
        base_changes_summary <- summary(base_changes, infer = TRUE, adjust = "none")
        base_changes_df <- as.data.frame(base_changes_summary)
        
        # Step 3: Get within-phase condition comparisons
        within_phase_contrasts <- emmeans::contrast(emm_cond_by_phase, method = "pairwise")
        within_phase_summary <- summary(within_phase_contrasts, infer = TRUE, adjust = "none")
        within_phase_df <- as.data.frame(within_phase_summary)
        
        # Step 4: Get difference-in-differences (between conditions) with confidence intervals
        did <- emmeans::contrast(base_changes, method = "revpairwise", by = "contrast")
        did_summary <- summary(did, infer = TRUE, adjust = "none")  # infer=TRUE adds CIs, adjust manually with holm later
        
        # Convert to data frame
        did_df <- as.data.frame(did_summary)
        
        # Check if we have results
        if (is.null(did_df) || nrow(did_df) == 0) {
            return(list(
                message = "No DiD comparisons could be computed.",
                within_condition = NULL,
                within_phase = NULL,
                did = NULL
            ))
        }
        
        # Process within-condition comparisons (base_changes)
        within_results <- list()
        if (!is.null(base_changes_df) && nrow(base_changes_df) > 0) {
            for (i in seq_len(nrow(base_changes_df))) {
                within_results[[i]] <- data.frame(
                    condition = as.character(base_changes_df$condition[i]),
                    phase_comparison = as.character(base_changes_df$contrast[i]),
                    estimate = base_changes_df$estimate[i],
                    SE = base_changes_df$SE[i],
                    df = base_changes_df$df[i],
                    t_ratio = base_changes_df$t.ratio[i],
                    p_value = base_changes_df$p.value[i],
                    lower_CI = if ("lower.CL" %in% names(base_changes_df)) base_changes_df$lower.CL[i] else NA,
                    upper_CI = if ("upper.CL" %in% names(base_changes_df)) base_changes_df$upper.CL[i] else NA,
                    stringsAsFactors = FALSE
                )
            }
            within_cond_df <- do.call(rbind, within_results)
            rownames(within_cond_df) <- NULL
            within_cond_df$p_adjusted <- p.adjust(within_cond_df$p_value, method = "holm")
        } else {
            within_cond_df <- NULL
        }
        
        # Process within-phase comparisons (condition contrasts within each phase)
        within_phase_results <- list()
        if (!is.null(within_phase_df) && nrow(within_phase_df) > 0) {
            for (i in seq_len(nrow(within_phase_df))) {
                within_phase_results[[i]] <- data.frame(
                    phase = as.character(within_phase_df$phase[i]),
                    condition_comparison = as.character(within_phase_df$contrast[i]),
                    estimate = within_phase_df$estimate[i],
                    SE = within_phase_df$SE[i],
                    df = within_phase_df$df[i],
                    t_ratio = within_phase_df$t.ratio[i],
                    p_value = within_phase_df$p.value[i],
                    lower_CI = if ("lower.CL" %in% names(within_phase_df)) within_phase_df$lower.CL[i] else NA,
                    upper_CI = if ("upper.CL" %in% names(within_phase_df)) within_phase_df$upper.CL[i] else NA,
                    stringsAsFactors = FALSE
                )
            }
            within_phase_final <- do.call(rbind, within_phase_results)
            rownames(within_phase_final) <- NULL
            within_phase_final$p_adjusted <- p.adjust(within_phase_final$p_value, method = "holm")
        } else {
            within_phase_final <- NULL
        }
        
        # Detect columns by content rather than name (more robust)
        # Look for columns containing " - " which indicate contrast columns
        contrast_candidates <- character(0)
        for (col_name in names(did_df)) {
            if (is.character(did_df[[col_name]]) || is.factor(did_df[[col_name]])) {
                # Check if column contains contrast syntax (" - ")
                sample_val <- as.character(did_df[[col_name]][1])
                if (!is.na(sample_val) && grepl(" - ", sample_val, fixed = TRUE)) {
                    contrast_candidates <- c(contrast_candidates, col_name)
                }
            }
        }
        
        # Identify phase and condition columns from contrast candidates
        # Phase column should contain phase level names from baseline_label
        phase_col <- NULL
        condition_col <- NULL
        
        if (length(contrast_candidates) >= 1) {
            # First candidate is typically the phase contrast (from by="contrast")
            phase_col <- contrast_candidates[1]
            
            # Second candidate (if exists) is the condition contrast
            if (length(contrast_candidates) >= 2) {
                condition_col <- contrast_candidates[2]
            }
        }
        
        # Fallback if detection fails
        if (is.null(phase_col)) phase_col <- if ("contrast" %in% names(did_df)) "contrast" else names(did_df)[1]
        if (is.null(condition_col)) {
            # Try standard names
            if ("contrast_1" %in% names(did_df)) {
                condition_col <- "contrast_1"
            } else if ("condition" %in% names(did_df)) {
                condition_col <- "condition"
            } else {
                condition_col <- setdiff(names(did_df)[1:2], phase_col)[1]
            }
        }
        
        # Build clean results
        result_list <- list()
        for (i in seq_len(nrow(did_df))) {
            # Parse phase contrast (e.g., "retention - baseline_task")
            phase_comparison <- as.character(did_df[[phase_col]][i])
            
            # Parse condition contrast (e.g., "perturbation - control")
            condition_comparison <- as.character(did_df[[condition_col]][i])
            condition_parts <- strsplit(condition_comparison, " - ")[[1]]
            
            condition_cmp <- if (length(condition_parts) >= 1) trimws(condition_parts[1]) else condition_comparison
            condition_ref <- if (length(condition_parts) >= 2) trimws(condition_parts[2]) else "reference"
            
            result_list[[i]] <- data.frame(
                phase_comparison = phase_comparison,
                condition_ref = condition_ref,
                condition_cmp = condition_cmp,
                estimate = did_df$estimate[i],
                SE = did_df$SE[i],
                df = did_df$df[i],
                t_ratio = did_df$t.ratio[i],
                p_value = did_df$p.value[i],
                lower_CI = if ("lower.CL" %in% names(did_df)) did_df$lower.CL[i] else NA,
                upper_CI = if ("upper.CL" %in% names(did_df)) did_df$upper.CL[i] else NA,
                stringsAsFactors = FALSE
            )
        }
        
        # Combine DiD results
        result_df <- do.call(rbind, result_list)
        rownames(result_df) <- NULL
        
        # Apply holm correction for multiple comparisons
        result_df$p_adjusted <- p.adjust(result_df$p_value, method = "holm")
        
        # Return all three tables as a list with metadata
        list(
            within_condition = if (!is.null(within_cond_df)) tibble::as_tibble(within_cond_df) else NULL,
            within_phase = if (!is.null(within_phase_final)) tibble::as_tibble(within_phase_final) else NULL,
            did = tibble::as_tibble(result_df),
            baseline_used = baseline_label
        )
        
    }, error = function(e) {
        list(
            message = paste0("Error computing DiD analysis: ", conditionMessage(e)),
            within_condition = NULL,
            within_phase = NULL,
            did = NULL
        )
    })
    
    return(result)
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