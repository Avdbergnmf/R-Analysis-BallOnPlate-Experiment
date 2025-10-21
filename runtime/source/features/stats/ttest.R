#' Paired T-test Functions
#'
#' Functions for running paired t-tests as an alternative to emmeans post-hoc analysis

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
