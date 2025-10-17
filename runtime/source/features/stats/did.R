#' Difference-in-Differences (DiD) Analysis Functions
#'
#' Functions for performing DiD analysis using emmeans

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
#' @param correction_method Method for multiple comparison correction (default: "holm")
#' @return A list with four elements:
#'   \item{within_condition}{Tibble of within-condition phase comparisons vs baseline}
#'   \item{within_phase}{Tibble of within-phase condition pairwise comparisons}
#'   \item{did}{Tibble of DiD results comparing condition differences in phase changes}
#'   \item{baseline_used}{Character string of the baseline phase label used}
#'   Returns a list with 'message' element if error occurs.
#' @export
compute_did_analysis <- function(lmm, data, correction_method = "holm") {
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
    result <- tryCatch(
        {
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
            did_summary <- summary(did, infer = TRUE, adjust = "none") # infer=TRUE adds CIs, adjust manually with holm later

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
                within_cond_df$p_adjusted <- p.adjust(within_cond_df$p_value, method = correction_method)
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
                within_phase_final$p_adjusted <- p.adjust(within_phase_final$p_value, method = correction_method)
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

            # Apply correction for multiple comparisons
            result_df$p_adjusted <- p.adjust(result_df$p_value, method = correction_method)

            # Return all three tables as a list with metadata
            list(
                within_condition = if (!is.null(within_cond_df)) tibble::as_tibble(within_cond_df) else NULL,
                within_phase = if (!is.null(within_phase_final)) tibble::as_tibble(within_phase_final) else NULL,
                did = tibble::as_tibble(result_df),
                baseline_used = baseline_label
            )
        },
        error = function(e) {
            list(
                message = paste0("Error computing DiD analysis: ", conditionMessage(e)),
                within_condition = NULL,
                within_phase = NULL,
                did = NULL
            )
        }
    )

    return(result)
}
