#' Principal Component Analysis (PCA) Functions
#'
#' Functions for performing PCA analysis and correlation studies

#' Perform Principal Component Analysis on a dataset
#' @param data The dataset to analyze
#' @param variables Vector of variable names to include in PCA (if NULL, uses all numeric variables)
#' @param exclude_cols Columns to exclude from PCA analysis
#' @return List containing PCA results and plots
perform_pca_analysis <- function(data, variables = NULL, exclude_cols = c("participant", "condition", "trialNum", "phase", "foot")) {
    pca_logger <- create_module_logger("PCA")
    pca_logger("INFO", "Starting PCA analysis")

    # Validate input data
    if (is.null(data) || nrow(data) == 0) {
        pca_logger("ERROR", "No data provided for PCA analysis")
        return(list(error = "No data provided for PCA analysis"))
    }

    # Determine variables to use
    if (is.null(variables)) {
        # Get all numeric columns, excluding specified columns
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        variables <- setdiff(numeric_cols, exclude_cols)
    }

    # Check if we have enough variables
    if (length(variables) < 2) {
        pca_logger("ERROR", "Need at least 2 variables for PCA analysis")
        return(list(error = "Need at least 2 variables for PCA analysis"))
    }

    # Check if variables exist in data
    missing_vars <- setdiff(variables, names(data))
    if (length(missing_vars) > 0) {
        pca_logger("WARN", "Missing variables:", paste(missing_vars, collapse = ", "))
        variables <- intersect(variables, names(data))
    }

    if (length(variables) < 2) {
        pca_logger("ERROR", "Not enough valid variables for PCA analysis")
        return(list(error = "Not enough valid variables for PCA analysis"))
    }

    pca_logger("INFO", "Using", length(variables), "variables for PCA analysis")
    pca_logger("DEBUG", "Variables:", paste(variables, collapse = ", "))

    # Extract data for PCA
    X <- data[, variables, drop = FALSE]

    # Remove rows with all NA values
    complete_rows <- complete.cases(X)
    X_clean <- X[complete_rows, , drop = FALSE]

    if (nrow(X_clean) < 2) {
        pca_logger("ERROR", "Not enough complete cases for PCA analysis")
        return(list(error = "Not enough complete cases for PCA analysis"))
    }

    pca_logger("INFO", "Using", nrow(X_clean), "complete cases out of", nrow(X), "total rows")

    # Log missing data details
    missing_counts <- colSums(is.na(X))
    vars_with_missing <- missing_counts[missing_counts > 0]

    if (length(vars_with_missing) > 0) {
        pca_logger("WARN", "Variables with missing data:")
        for (var_name in names(vars_with_missing)) {
            pca_logger("WARN", "  -", var_name, ":", vars_with_missing[var_name], "missing values")

            # Show participant/trial combinations that are missing
            missing_rows <- which(is.na(X[, var_name]))

            # Show participant and trial info if available in original data
            if ("participant" %in% names(data) && "trialNum" %in% names(data)) {
                # Create participant-trial combinations for missing rows
                missing_combos <- paste0(data$participant[missing_rows], "-T", data$trialNum[missing_rows])
                unique_combos <- unique(missing_combos)

                if (length(unique_combos) <= 20) {
                    pca_logger("WARN", "    Missing in participant-trial combinations:", paste(unique_combos, collapse = ", "))
                } else {
                    pca_logger("WARN", "    Missing in participant-trial combinations:", paste(unique_combos[1:10], collapse = ", "), "...", paste(unique_combos[(length(unique_combos) - 9):length(unique_combos)], collapse = ", "), "(showing first/last 10 of", length(unique_combos), "total)")
                }

                # Also show unique participants and trials
                missing_participants <- unique(data$participant[missing_rows])
                missing_trials <- unique(data$trialNum[missing_rows])
                pca_logger("WARN", "    Affected participants:", paste(missing_participants, collapse = ", "))
                pca_logger("WARN", "    Affected trials:", paste(missing_trials, collapse = ", "))
            } else if ("participant" %in% names(data)) {
                missing_participants <- unique(data$participant[missing_rows])
                pca_logger("WARN", "    Affected participants:", paste(missing_participants, collapse = ", "))
            } else {
                # Fallback to row numbers if no participant info available
                if (length(missing_rows) <= 20) {
                    pca_logger("WARN", "    Missing in rows:", paste(missing_rows, collapse = ", "))
                } else {
                    pca_logger("WARN", "    Missing in rows:", paste(missing_rows[1:10], collapse = ", "), "...", paste(missing_rows[(length(missing_rows) - 9):length(missing_rows)], collapse = ", "), "(showing first/last 10 of", length(missing_rows), "total)")
                }
            }
        }
        pca_logger("INFO", "Only", nrow(X_clean), "rows have complete data across all", ncol(X), "variables")
    } else {
        pca_logger("INFO", "No missing data found - all", nrow(X), "rows are complete")
    }

    # Check if we have enough observations relative to variables
    n_vars <- ncol(X_clean)
    n_obs <- nrow(X_clean)
    if (n_obs < n_vars) {
        pca_logger("WARN", "Fewer complete cases (", n_obs, ") than variables (", n_vars, ")")
        pca_logger("INFO", "This may cause issues with PCA. Consider selecting fewer variables.")
    }

    # Convert to matrix
    X_matrix <- as.matrix(X_clean)

    # Perform PCA using the custom function
    pca_results <- my_pca_function(X_matrix, original_data = data)

    # Add metadata
    pca_results$metadata <- list(
        n_observations = nrow(X_clean),
        n_variables = length(variables),
        variables_used = variables,
        excluded_rows = nrow(X) - nrow(X_clean)
    )

    pca_logger("INFO", "PCA analysis completed successfully")
    return(pca_results)
}

#' Custom PCA function based on the provided example
#' @param X Matrix of data for PCA analysis
#' @return List containing PCA results and plots
my_pca_function <- function(X, original_data = NULL) {
    pca_logger <- create_module_logger("PCA")

    # 1- Preparation of the dataset and diagonalization
    # Check for variables with zero variance and remove them
    zero_var_cols <- which(apply(X, 2, function(x) sd(x, na.rm = TRUE) == 0))
    if (length(zero_var_cols) > 0) {
        pca_logger(
            "WARN", "Removing", length(zero_var_cols), "variables with zero variance:",
            paste(colnames(X)[zero_var_cols], collapse = ", ")
        )
        X <- X[, -zero_var_cols, drop = FALSE]
    }

    if (ncol(X) < 2) {
        pca_logger("ERROR", "Not enough variables remaining after removing zero-variance variables")
        return(list(error = "Not enough variables with non-zero variance for PCA"))
    }

    Xnorm <- matrix(nrow = nrow(X), ncol = ncol(X))
    for (j in 1:ncol(X)) {
        Xnorm[, j] <- (X[, j] - mean(X[, j], na.rm = TRUE)) / sd(X[, j], na.rm = TRUE)
    }
    # Calculate correlation matrix with error handling
    cormat <- tryCatch(
        {
            cor(x = X, use = "pairwise.complete.obs")
        },
        error = function(e) {
            pca_logger("ERROR", "Failed to calculate correlation matrix:", e$message)
            return(NULL)
        }
    )

    if (is.null(cormat)) {
        return(list(error = "Correlation matrix calculation failed"))
    }

    # Check for infinite or missing values in correlation matrix
    if (any(!is.finite(cormat))) {
        pca_logger("ERROR", "Correlation matrix contains infinite or missing values")
        return(list(error = "Correlation matrix contains infinite or missing values"))
    }

    # Perform eigen decomposition with error handling
    res_pca <- tryCatch(
        {
            eigen(cormat)
        },
        error = function(e) {
            pca_logger("ERROR", "Eigen decomposition failed:", e$message)
            return(NULL)
        }
    )

    if (is.null(res_pca)) {
        return(list(error = "Eigen decomposition failed"))
    }

    # 2- Scree plot
    plot_scree <- ggplot2::ggplot(
        data = NULL,
        ggplot2::aes(
            x = 1:length(res_pca$values),
            y = res_pca$values / sum(res_pca$values)
        )
    ) +
        ggplot2::geom_col() +
        ggplot2::xlab("Principal components") +
        ggplot2::ylab("Eigenvalue / Sum(eigenvalues)") +
        ggplot2::theme_minimal()

    # 3- Enhanced Biplot with arrows for all variables (PC1 vs PC2)
    vecpropres_1 <- res_pca$vectors[, 1]
    vecpropres_2 <- res_pca$vectors[, 2]
    vecpropres_12 <- cbind(vecpropres_1, vecpropres_2)
    obs_firstplane <- Xnorm %*% vecpropres_12

    # Create data frame for biplot with arrows
    biplot_data <- data.frame(
        PC1 = cormat[, 1],
        PC2 = cormat[, 2],
        Variable = colnames(X)
    )

    # Calculate arrow scaling factor
    max_radius <- max(sqrt(cormat[, 1]^2 + cormat[, 2]^2))
    arrow_scale <- 0.8 / max_radius

    # Create the biplot with observations and variable arrows
    PCA12 <- ggplot2::ggplot() +
        # Plot observations
        ggplot2::geom_point(ggplot2::aes(
            x = obs_firstplane[, 1],
            y = obs_firstplane[, 2]
        ), size = 1.5, alpha = 0.6, color = "gray50") +
        # Add variable arrows
        ggplot2::geom_segment(
            data = biplot_data,
            ggplot2::aes(
                x = 0, y = 0,
                xend = PC1 * arrow_scale,
                yend = PC2 * arrow_scale
            ),
            arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches")),
            color = "blue", alpha = 0.7, size = 0.5
        ) +
        # Add variable labels
        ggplot2::geom_text(
            data = biplot_data,
            ggplot2::aes(
                x = PC1 * arrow_scale * 1.1,
                y = PC2 * arrow_scale * 1.1,
                label = Variable
            ),
            size = 3, hjust = 0, vjust = 0.5, color = "darkblue"
        ) +
        # Add reference lines
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.5) +
        # Styling
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "Principal Components Biplot (PC1 vs PC2)",
            subtitle = "Points = observations, Arrows = variable loadings",
            x = paste0("PC1 (", round(res_pca$values[1] / sum(res_pca$values) * 100, 1), "% variance)"),
            y = paste0("PC2 (", round(res_pca$values[2] / sum(res_pca$values) * 100, 1), "% variance)")
        ) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(size = 10),
            axis.title = ggplot2::element_text(size = 12),
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 10, color = "gray60")
        ) +
        ggplot2::coord_fixed(ratio = 1)

    # 4- Correlation circle
    val_plus <- res_pca$values * as.numeric(res_pca$values >= 0)

    cor.factors <- NULL
    for (j in 1:ncol(X)) {
        rf <- sqrt(val_plus[j]) * res_pca$vectors[, j]
        cor.factors <- cbind(cor.factors, rf)
    }
    rownames(cor.factors) <- colnames(X)
    colnames(cor.factors) <- paste("F", 1:ncol(X), sep = "")

    # 5- Factor loadings
    DataFrameLoadings <- data.frame(
        Variable = factor(rep(colnames(X), times = 2),
            levels = sort(colnames(X), decreasing = TRUE)
        ),
        Loadings = c(cor.factors[, 1], cor.factors[, 2]),
        Factor = rep(c("PC1", "PC2"), each = ncol(X))
    )

    PlotLoadings <- ggplot2::ggplot(DataFrameLoadings) +
        ggplot2::geom_col(ggplot2::aes(x = Variable, y = Loadings)) +
        ggplot2::facet_grid(cols = ggplot2::vars(Factor)) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal()

    # 6- Correlation matrix plot
    cor_matrix_plot <- create_correlation_plot(cormat)

    # 7- Summary statistics
    summary_stats <- create_pca_summary(res_pca, cormat)

    return(list(
        res_pca = res_pca,
        plot_scree = plot_scree,
        PCA12 = PCA12,
        cor.factors = cor.factors,
        PlotLoadings = PlotLoadings,
        cormat = cormat,
        cor_matrix_plot = cor_matrix_plot,
        summary_stats = summary_stats,
        Xnorm = Xnorm,
        obs_firstplane = obs_firstplane
    ))
}

#' Create correlation matrix plot
#' @param cormat Correlation matrix
#' @return ggplot object
create_correlation_plot <- function(cormat) {
    # Convert correlation matrix to long format for plotting
    cor_data <- expand.grid(Var1 = rownames(cormat), Var2 = colnames(cormat))
    cor_data$Correlation <- as.vector(cormat)

    # Create heatmap
    cor_plot <- ggplot2::ggplot(cor_data, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(
            low = "blue", mid = "white", high = "red",
            midpoint = 0, limit = c(-1, 1), space = "Lab"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = "Correlation Matrix", x = "", y = "") +
        ggplot2::coord_fixed()

    return(cor_plot)
}

#' Create PCA summary statistics
#' @param res_pca PCA results from eigen()
#' @param cormat Correlation matrix
#' @return List of summary statistics
create_pca_summary <- function(res_pca, cormat) {
    # Calculate variance explained
    eigenvalues <- res_pca$values
    variance_explained <- eigenvalues / sum(eigenvalues) * 100
    cumulative_variance <- cumsum(variance_explained)

    # Find number of components explaining 80% and 95% of variance
    n_comp_80 <- which(cumulative_variance >= 80)[1]
    n_comp_95 <- which(cumulative_variance >= 95)[1]

    # Create summary table
    summary_table <- data.frame(
        Component = 1:length(eigenvalues),
        Eigenvalue = eigenvalues,
        Variance_Explained = variance_explained,
        Cumulative_Variance = cumulative_variance
    )

    return(list(
        summary_table = summary_table,
        n_components_80_percent = n_comp_80,
        n_components_95_percent = n_comp_95,
        total_variance_explained = sum(variance_explained)
    ))
}

#' Get available variables for PCA analysis
#' @param data The dataset
#' @param exclude_cols Columns to exclude
#' @return Vector of available variable names
get_pca_variables <- function(data, exclude_cols = c("participant", "condition", "trialNum", "phase", "foot")) {
    # Fast early return for empty data
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
        return(character(0))
    }

    # Get all numeric columns, excluding specified columns
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    available_vars <- setdiff(numeric_cols, exclude_cols)

    return(available_vars)
}

#' Create PCA button for UI
#' @return HTML string for PCA button
create_pca_button <- function() {
    return('<button type="button" class="btn btn-primary" id="show_pca_analysis" style="margin-top: 10px;">
            <i class="fa fa-chart-line"></i> Run PCA Analysis
            </button>')
}

#' Validate PCA data
#' @param data The dataset to validate
#' @param variables Variables to use for PCA
#' @return List with validation results
validate_pca_data <- function(data, variables = NULL) {
    validation_logger <- create_module_logger("PCA-VALIDATION")

    if (is.null(data) || nrow(data) == 0) {
        return(list(valid = FALSE, message = "No data provided"))
    }

    if (is.null(variables)) {
        variables <- get_pca_variables(data)
    }

    if (length(variables) < 2) {
        return(list(valid = FALSE, message = "Need at least 2 variables for PCA analysis"))
    }

    # Check for missing variables
    missing_vars <- setdiff(variables, names(data))
    if (length(missing_vars) > 0) {
        return(list(valid = FALSE, message = paste("Missing variables:", paste(missing_vars, collapse = ", "))))
    }

    # Check for complete cases
    X <- data[, variables, drop = FALSE]
    complete_rows <- complete.cases(X)
    n_complete <- sum(complete_rows)

    if (n_complete < 2) {
        return(list(valid = FALSE, message = "Not enough complete cases for PCA analysis"))
    }

    if (n_complete < nrow(data) * 0.5) {
        validation_logger("WARN", "Only", n_complete, "out of", nrow(data), "rows have complete data")
    }

    return(list(
        valid = TRUE,
        message = paste("Data validation passed. Using", n_complete, "complete cases out of", nrow(data), "total rows"),
        n_complete = n_complete,
        n_total = nrow(data),
        variables = variables
    ))
}
