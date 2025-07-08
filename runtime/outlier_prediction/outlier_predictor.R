## ========================================================================== ##
## heelstrike_outlier_predictor.R - Predict false heel strikes and outliers  ##
## ========================================================================== ##
## Based on false heel strikes and outliers files                     ##
## Focuses on false heel strike detection and step outlier detection         ##
## -------------------------------------------------------------------------- ##

# ============================================================================ #
#                           CONFIGURATION SETTINGS                            #
# ============================================================================ #

# FORCE RETRAIN - Set to TRUE to force training even if model exists
FORCE_RETRAIN <- TRUE # Suggested: FALSE for faster execution, TRUE for fresh models

# EXECUTION CONTROL - Enable/disable specific detection tasks
DO_HEELSTRIKES <- TRUE # Set to FALSE to skip false heel strike detection
DO_OUTLIERS <- TRUE # Set to FALSE to skip outlier detection

# NEIGHBOR FEATURES - Number of neighboring heel strikes to include as features
NEIGHBOR_K <- 3 # Suggested: 2-3 (higher = more context but slower training)

# PREDICTION THRESHOLD - Probability threshold for classifying outliers
PRED_THRESHOLD <- 0.5 # Suggested: 0.5 (lower = more sensitive, higher = more conservative)

# SEPARATE THRESHOLDS for different models due to class imbalance differences
HEELSTRIKE_THRESHOLD <- 0.5 # False heel strikes: 2.7% minority class
STEP_THRESHOLD <- 0.1 # Step outliers: 0.5% minority class - much more sensitive threshold needed

# TRAINING SPLIT - Proportion of data used for training (rest for testing)
TRAIN_SPLIT <- 1.0 # Reduced from 0.75 to have more test data

# FEATURE SELECTION - Limit features to prevent overfitting
MAX_FEATURES <- 20 # Use only top 10 most important features
USE_FEATURE_SELECTION <- TRUE # Enable feature selection

# RANDOM FOREST PARAMETERS - More conservative to prevent overfitting
RF_NTREE <- 300 # Reduced from 150
RF_NODESIZE <- 10 # Increased from 5 to prevent overfitting
RF_MAXNODES <- 50 # Reduced from 200 to prevent overfitting
RF_MTRY_FRACTION <- 0.33 # Use sqrt(p)/p fraction of features at each split

# CROSS-VALIDATION SETTINGS
USE_CV <- FALSE # Enable cross-validation
CV_FOLDS <- 5 # Number of CV folds
CV_REPEATS <- 3 # Number of CV repeats

# ========================== PARALLEL TRAINING METHODS ====================== #

# Configuration for parallel training
ENABLE_PARALLEL <- TRUE # Set to FALSE to disable parallel processing
PARALLEL_CORES <- max(1, parallel::detectCores() - 1) # Leave one core free

# Generic model configuration
create_model_config <- function(model_type) {
    if (model_type == "false_heelstrike") {
        list(
            standardize_func = standardise_heelstrikes,
            add_features_func = add_heelstrike_neighbors,
            time_column = "time",
            exclude_cols = c("participant", "trialNum", "time", "foot", "is_outlier"),
            model_name = "false heel strike",
            data_type = "false_heelstrikes",
            model_file = file.path(models_dir, "false_heelstrike_model.rds"),
            requires_context = FALSE
        )
    } else if (model_type == "outlier") {
        list(
            standardize_func = standardise_steps,
            add_features_func = function(df, context = NULL) {
                add_step_neighbors_and_heelstrike_context(df, context)
            },
            time_column = "stepTime",
            exclude_cols = c("participant", "trialNum", "stepTime", "time", "is_outlier"),
            model_name = "step outlier",
            data_type = "outliers",
            model_file = file.path(models_dir, "step_outlier_model.rds"),
            requires_context = TRUE
        )
    } else {
        stop("Unknown model type: ", model_type)
    }
}

# Generic training data creation function
create_training_data_generic <- function(train_files, outliers_data, config, context_data = NULL) {
    message("[DEBUG] Starting ", config$model_name, " training data creation...")

    # Get trials with outliers
    message("[DEBUG] Getting trials with outliers for config$data_type: ", config$data_type)
    relevant_trials <- get_trials_with_data(outliers_data, config$data_type)

    if (nrow(relevant_trials) == 0) {
        message("[WARN] No ", config$model_name, " outliers found for training")
        return(NULL)
    }

    message("[DEBUG] ", config$model_name, " training trials: ", nrow(relevant_trials))

    # Process files in parallel if enabled
    if (ENABLE_PARALLEL && length(train_files) > 1) {
        message("[INFO] Processing ", length(train_files), " files in parallel using ", PARALLEL_CORES, " cores")

        # Setup parallel cluster
        cl <- parallel::makeCluster(PARALLEL_CORES)
        on.exit(parallel::stopCluster(cl))

        # Export required objects to cluster
        parallel::clusterExport(cl, c(
            "config", "relevant_trials", "context_data",
            "standardize_join_columns", "inner_join", "message"
        ))

        # Load required packages on cluster
        parallel::clusterEvalQ(cl, {
            library(dplyr)
            library(data.table)
        })

        # Process files in parallel
        train_dfs <- parallel::parLapply(cl, train_files, function(f) {
            message("[INFO] Loading ", f, " for ", config$model_name, " training")
            start_time <- Sys.time()
            raw_data <- readRDS(f)
            load_time <- Sys.time()

            # Filter to relevant trials
            filtered_data <- raw_data %>%
                standardize_join_columns() %>%
                inner_join(relevant_trials, by = c("participant", "trialNum"))

            if (nrow(filtered_data) == 0) {
                message("[WARN] No relevant ", config$model_name, " trials found in ", f)
                return(NULL)
            }

            # Standardize data
            result <- config$standardize_func(filtered_data)
            message(
                "[DEBUG] ", config$model_name, " file ", basename(f), " processed: ",
                nrow(result), " rows in ", round(difftime(Sys.time(), start_time, units = "secs"), 2), " seconds"
            )
            result
        })

        # Combine results
        train_df <- dplyr::bind_rows(train_dfs)
    } else {
        # Sequential processing
        message("[INFO] Processing ", length(train_files), " files sequentially")
        train_df <- dplyr::bind_rows(lapply(train_files, function(f) {
            message("[INFO] Loading ", f, " for ", config$model_name, " training")
            start_time <- Sys.time()
            raw_data <- readRDS(f)
            load_time <- Sys.time()
            message("[DEBUG] File loaded in ", round(difftime(load_time, start_time, units = "secs"), 2), " seconds")

            # Filter to relevant trials
            filter_start <- Sys.time()
            filtered_data <- raw_data %>%
                standardize_join_columns() %>%
                inner_join(relevant_trials, by = c("participant", "trialNum"))
            filter_time <- Sys.time()
            message("[DEBUG] Data filtered in ", round(difftime(filter_time, filter_start, units = "secs"), 2), " seconds")

            if (nrow(filtered_data) == 0) {
                message("[WARN] No relevant ", config$model_name, " trials found in ", f)
                return(NULL)
            }

            # Standardize data
            std_start <- Sys.time()
            result <- config$standardize_func(filtered_data)
            std_time <- Sys.time()
            message("[DEBUG] Data standardized in ", round(difftime(std_time, std_start, units = "secs"), 2), " seconds")
            result
        }))
    }

    if (is.null(train_df) || nrow(train_df) == 0) {
        message("[WARN] No ", config$model_name, " training data available")
        return(NULL)
    }

    message("[DEBUG] Combined ", config$model_name, " training data dimensions: ", nrow(train_df), " x ", ncol(train_df))

    # Add features (with context if required)
    feature_start <- Sys.time()
    if (config$requires_context) {
        train_df <- config$add_features_func(train_df, context_data)
    } else {
        train_df <- config$add_features_func(train_df)
    }
    feature_time <- Sys.time()
    message("[DEBUG] Features added in ", round(difftime(feature_time, feature_start, units = "secs"), 2), " seconds")

    # ------------------------------------------------------------
    # REMOVE heel strike context features for step outlier model
    # ------------------------------------------------------------
    if (config$model_name == "step outlier") {
        # Drop any columns that indicate heel-strike outlier context
        hs_cols <- grep("hs_outlier|nearby_hs_outliers", names(train_df), value = TRUE)
        if (length(hs_cols) > 0) {
            message("[INFO] Removing heel-strike context columns from step training data: ", paste(hs_cols, collapse = ", "))
            train_df <- train_df %>% dplyr::select(-all_of(hs_cols))
        }
    }

    # Add outlier labels
    label_start <- Sys.time()
    message("[DEBUG] Adding outlier labels for data_type: ", config$data_type, ", time_column: ", config$time_column)
    message("[DEBUG] Input training data before labeling: ", nrow(train_df), " rows")
    train_df <- add_labels(train_df, outliers_data, config$data_type, config$time_column)
    message("[DEBUG] Training data after labeling: ", nrow(train_df), " rows")
    if (nrow(train_df) > 0) {
        label_dist <- table(train_df$is_outlier)
        message("[DEBUG] Label distribution: ", paste(names(label_dist), "=", label_dist, collapse = ", "))
    }
    label_time <- Sys.time()
    message("[DEBUG] Outlier labels added in ", round(difftime(label_time, label_start, units = "secs"), 2), " seconds")

    # Exclude trials
    exclude_start <- Sys.time()
    train_df <- exclude_trials(train_df)
    exclude_time <- Sys.time()
    message("[DEBUG] Trials excluded in ", round(difftime(exclude_time, exclude_start, units = "secs"), 2), " seconds")

    # Handle class imbalance for step models
    if (config$model_name == "step outlier") {
        outlier_counts <- table(train_df$is_outlier)
        if (length(outlier_counts) == 1) {
            message("[DEBUG] Only one class found, adding artificial examples for training...")
            if (names(outlier_counts)[1] == "TRUE") {
                n_to_add <- min(10, nrow(train_df) %/% 10)
                artificial_rows <- train_df[sample(nrow(train_df), n_to_add), ]
                artificial_rows$is_outlier <- FALSE
                train_df <- rbind(train_df, artificial_rows)
                message("[DEBUG] Added ", n_to_add, " artificial non-outlier examples")
            } else {
                n_to_add <- min(10, nrow(train_df) %/% 10)
                artificial_rows <- train_df[sample(nrow(train_df), n_to_add), ]
                artificial_rows$is_outlier <- TRUE
                train_df <- rbind(train_df, artificial_rows)
                message("[DEBUG] Added ", n_to_add, " artificial outlier examples")
            }
        }
    }

    message("[DEBUG] Final ", config$model_name, " training data dimensions: ", nrow(train_df), " x ", ncol(train_df))
    train_df
}

# Generic model training and loading function
train_or_load_model <- function(config, train_df = NULL) {
    model_start <- Sys.time()

    # Check if model exists and we don't want to force retrain
    if (!FORCE_RETRAIN && file.exists(config$model_file)) {
        message("[INFO] Existing ", config$model_name, " model found at: ", normalizePath(config$model_file))
        model_result <- readRDS(config$model_file)
        model_time <- Sys.time()
        message(
            "[TIMING] ", config$model_name, " model loaded in: ",
            round(difftime(model_time, model_start, units = "secs"), 2), " seconds"
        )
        return(model_result)
    }

    # Train new model
    if (is.null(train_df) || nrow(train_df) == 0) {
        message("[ERROR] No training data provided for ", config$model_name, " model")
        return(NULL)
    }

    message("[INFO] Training new ", config$model_name, " model...")
    model_result <- train_outlier_model(train_df, config$model_name, config$exclude_cols)

    if (!is.null(model_result)) {
        saveRDS(model_result, config$model_file)
        message("[INFO] ", config$model_name, " model saved to: ", normalizePath(config$model_file))
    } else {
        message("[ERROR] ", config$model_name, " model training returned NULL - model not saved!")
    }

    model_time <- Sys.time()
    message(
        "[TIMING] ", config$model_name, " model preparation completed in: ",
        round(difftime(model_time, model_start, units = "secs"), 2), " seconds"
    )

    model_result
}

# Generic prediction function
predict_outliers_for_model <- function(model_result, config, data_files, outliers_data, context_data = NULL, threshold = PRED_THRESHOLD) {
    if (is.null(model_result)) {
        message("[WARN] No ", config$model_name, " model available for prediction")
        return(data.frame())
    }

    message("[DEBUG] Getting existing trials for prediction, config$data_type: ", config$data_type)
    existing_trials <- get_trials_with_data(outliers_data, config$data_type)

    if (config$requires_context) {
        # Create wrapper function for context-dependent features
        add_features_with_context <- function(std_data) {
            return(config$add_features_func(std_data, context_data))
        }

        return(predict_outliers_generic(
            model_result = model_result,
            data_files = data_files,
            existing_outlier_trials = existing_trials,
            standardize_func = config$standardize_func,
            add_features_func = add_features_with_context,
            model_name = config$model_name,
            time_column = config$time_column,
            threshold = threshold
        ))
    } else {
        return(predict_outliers_generic(
            model_result = model_result,
            data_files = data_files,
            existing_outlier_trials = existing_trials,
            standardize_func = config$standardize_func,
            add_features_func = config$add_features_func,
            model_name = config$model_name,
            time_column = config$time_column,
            threshold = threshold
        ))
    }
}

# Parallel model training function
train_models_parallel <- function(train_files, outliers_data, updated_heelstrikes = NULL) {
    message("[INFO] Starting parallel model training...")

    # Create configurations
    heelstrike_config <- create_model_config("false_heelstrike")
    step_config <- create_model_config("outlier")

    if (ENABLE_PARALLEL) {
        message("[INFO] Training models in parallel using ", PARALLEL_CORES, " cores")

        # Setup cluster
        cl <- parallel::makeCluster(min(2, PARALLEL_CORES)) # Max 2 cores for 2 models
        on.exit(parallel::stopCluster(cl))

        # Export the entire workspace environment to parallel workers
        # This ensures all variables and functions are available
        message("[DEBUG] Exporting entire workspace to parallel workers...")
        parallel::clusterExport(cl, ls(envir = .GlobalEnv), envir = .GlobalEnv)

        # Load required packages (same as main script)
        parallel::clusterEvalQ(cl, {
            library(dplyr)
            library(tidyr)
            library(caret)
            library(randomForest)
            library(data.table)
            library(parallel)
        })

        # Train models in parallel
        results <- parallel::parLapply(cl, list("false_heelstrike", "outlier"), function(model_type) {
            # Create config inside worker
            config <- create_model_config(model_type)

            # Set context based on model type
            if (model_type == "false_heelstrike") {
                context <- NULL
            } else {
                # Use updated_heelstrikes if it exists, otherwise NULL
                context <- if (exists("updated_heelstrikes")) updated_heelstrikes else NULL
            }

            # Create training data
            train_df <- create_training_data_generic(train_files, outliers_data, config, context)

            # Train or load model
            model_result <- train_or_load_model(config, train_df)

            list(
                model_type = model_type,
                config = config,
                model_result = model_result,
                train_df = train_df
            )
        })

        # Extract results
        heelstrike_result <- results[[1]]
        step_result <- results[[2]]
    } else {
        message("[INFO] Training models sequentially")

        # Train heel strike model
        heelstrike_train_df <- create_training_data_generic(train_files, outliers_data, heelstrike_config)
        heelstrike_model <- train_or_load_model(heelstrike_config, heelstrike_train_df)
        heelstrike_result <- list(
            model_type = "false_heelstrike",
            config = heelstrike_config,
            model_result = heelstrike_model,
            train_df = heelstrike_train_df
        )

        # Train step model
        step_train_df <- create_training_data_generic(train_files, outliers_data, step_config, updated_heelstrikes)
        step_model <- train_or_load_model(step_config, step_train_df)
        step_result <- list(
            model_type = "outlier",
            config = step_config,
            model_result = step_model,
            train_df = step_train_df
        )
    }

    return(list(
        heelstrike = heelstrike_result,
        step = step_result
    ))
}

# ========================== END PARALLEL METHODS =========================== #

# ============================================================================ #

cat("========================================\n")
cat("FALSE HEEL STRIKE AND OUTLIER PREDICTOR\n")
cat("========================================\n")
cat("Configuration Settings:\n")
cat("- Force Retrain:", FORCE_RETRAIN, "\n")
cat("- Do Heel Strikes:", DO_HEELSTRIKES, "\n")
cat("- Do Outliers:", DO_OUTLIERS, "\n")
cat("- Neighbor K:", NEIGHBOR_K, "\n")
cat("- Heel Strike Threshold:", HEELSTRIKE_THRESHOLD, "\n")
cat("- Step Outlier Threshold:", STEP_THRESHOLD, "(lower due to severe class imbalance)\n")
cat("- Training Split:", TRAIN_SPLIT, "\n")
cat("- Max Features:", MAX_FEATURES, "\n")
cat("- Feature Selection:", USE_FEATURE_SELECTION, "\n")
cat("- Cross-Validation:", USE_CV, "\n")
cat("- RF Trees:", RF_NTREE, "\n")
cat("- RF Node Size:", RF_NODESIZE, "\n")
cat("- RF Max Nodes:", RF_MAXNODES, "\n")
cat("========================================\n")
cat("OVERFITTING PREVENTION MEASURES:\n")
cat("- Limited to top", MAX_FEATURES, "features\n")
cat("- Conservative RF parameters\n")
cat("- Correlation filtering (0.9 threshold)\n")
cat("- Cross-validation enabled:", USE_CV, "\n")
cat("- Reduced training split:", TRAIN_SPLIT, "\n")
cat("========================================\n\n")

# Detect the correct paths based on directory structure
# Check if current directory contains data_extra and outlier_prediction (workspace/runtime)
if (file.exists("data_extra") && file.exists("outlier_prediction")) {
    # We're in the workspace/runtime directory
    runtime_dir <- "."
    data_extra_dir <- "data_extra"
    outlier_prediction_dir <- "outlier_prediction"
} else if (file.exists("runtime") && file.exists(file.path("runtime", "data_extra"))) {
    # We're in parent directory, runtime is a subdirectory
    runtime_dir <- "runtime"
    data_extra_dir <- file.path("runtime", "data_extra")
    outlier_prediction_dir <- file.path("runtime", "outlier_prediction")
} else if (file.exists(file.path("..", "data_extra")) && file.exists(file.path("..", "outlier_prediction"))) {
    # We're in a subdirectory of runtime
    runtime_dir <- ".."
    data_extra_dir <- file.path("..", "data_extra")
    outlier_prediction_dir <- file.path("..", "outlier_prediction")
} else {
    stop("Could not find data_extra and outlier_prediction directories. Please ensure you're running from the correct location.")
}

# Set up file paths now that directories are defined
false_heelstrikes_path <- file.path(data_extra_dir, falseHeelStrikesFile)
outliers_path <- file.path(data_extra_dir, outliersFile)

# Create models directory if it doesn't exist
models_dir <- file.path(outlier_prediction_dir, "models")
if (!dir.exists(models_dir)) {
    dir.create(models_dir, recursive = TRUE)
    message("[INFO] Created models directory: ", models_dir)
}

# Training data file (RDS file with all gait parameters) - from results folder
training_data_dir <- file.path(runtime_dir, "results")
train_files <- c(file.path(training_data_dir, "allGaitParams.rds"))

# Since we're using the same file for training and prediction, we'll predict on trials
# that don't have existing outliers within the same dataset

## -------------------------------------------------------------------------- ##

# --------------------------- 1. Packages & Initialization ------------------ #
required <- c("dplyr", "tidyr", "caret", "randomForest", "data.table", "parallel", "pROC", "foreach")
new_pkgs <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, quiet = TRUE)
lapply(required, library, character.only = TRUE)

# Load initialization script for file path definitions
source(file.path(runtime_dir, "source", "initialization.R"))
initialize_global_parameters()

# --------------------------- 2. Load data ---------------------------------- #
load_outliers <- function() {
    # Verify paths exist
    message("[DEBUG] Looking for data files in: ", normalizePath(data_extra_dir, mustWork = FALSE))
    message("[DEBUG] False heel strikes file: ", normalizePath(false_heelstrikes_path, mustWork = FALSE))
    message("[DEBUG] Outliers file: ", normalizePath(outliers_path, mustWork = FALSE))
    message("[DEBUG] Directory exists: ", dir.exists(data_extra_dir))
    message("[DEBUG] Files in data_extra: ", paste(list.files(data_extra_dir, full.names = FALSE), collapse = ", "))

    false_heelstrikes <- NULL
    outliers <- NULL

    if (file.exists(false_heelstrikes_path)) {
        message("[INFO] Loading false heel strikes from: ", false_heelstrikes_path)
        false_heelstrikes <- read.csv(false_heelstrikes_path, stringsAsFactors = FALSE)
        message("[DEBUG] False heel strikes columns: ", paste(names(false_heelstrikes), collapse = ", "))
        message("[DEBUG] False heel strikes sample participants: ", paste(unique(false_heelstrikes$participant)[1:min(5, length(unique(false_heelstrikes$participant)))], collapse = ", "))
        message("[DEBUG] False heel strikes ALL trials: ", paste(sort(unique(false_heelstrikes$trialNum)), collapse = ", "))
        false_heelstrikes$participant <- as.character(false_heelstrikes$participant)
        false_heelstrikes$trialNum <- as.numeric(false_heelstrikes$trialNum)
        false_heelstrikes$time <- as.numeric(false_heelstrikes$time)
    } else {
        message("[WARN] False heel strikes file not found: ", false_heelstrikes_path)
    }

    if (file.exists(outliers_path)) {
        message("[INFO] Loading outliers from: ", outliers_path)
        outliers <- read.csv(outliers_path, stringsAsFactors = FALSE)
        message("[DEBUG] Outliers columns: ", paste(names(outliers), collapse = ", "))
        message("[DEBUG] Outliers sample participants: ", paste(unique(outliers$participant)[1:min(5, length(unique(outliers$participant)))], collapse = ", "))
        message("[DEBUG] Outliers ALL trials: ", paste(sort(unique(outliers$trialNum)), collapse = ", "))
        outliers$participant <- as.character(outliers$participant)
        outliers$trialNum <- as.numeric(outliers$trialNum)
        outliers$time <- as.numeric(outliers$time)
    } else {
        message("[WARN] Outliers file not found: ", outliers_path)
    }

    list(false_heelstrikes = false_heelstrikes, outliers = outliers)
}

# --------------------------- 3. Identify relevant trials ------------------- #
get_relevant_trials <- function(outliers_data) {
    relevant_trials <- data.frame(
        participant = character(0),
        trialNum = numeric(0),
        has_heelstrike_outliers = logical(0),
        has_step_outliers = logical(0),
        stringsAsFactors = FALSE
    )

    if (!is.null(outliers_data$false_heelstrikes) && nrow(outliers_data$false_heelstrikes) > 0) {
        message("[DEBUG] Processing false heel strike trials...")
        hs_trials <- outliers_data$false_heelstrikes %>%
            select(participant, trialNum) %>%
            distinct() %>%
            mutate(
                participant = as.character(participant),
                trialNum = as.numeric(trialNum),
                has_heelstrike_outliers = TRUE,
                has_step_outliers = FALSE
            )
        message("[DEBUG] False heel strike trials: ", nrow(hs_trials), " unique trials")
        message("[DEBUG] HS trials columns: ", paste(names(hs_trials), collapse = ", "))
        relevant_trials <- bind_rows(relevant_trials, hs_trials)
        message("[DEBUG] Relevant trials after HS: ", nrow(relevant_trials), " rows")
    }

    if (!is.null(outliers_data$outliers) && nrow(outliers_data$outliers) > 0) {
        message("[DEBUG] Processing step trials...")
        step_trials <- outliers_data$outliers %>%
            select(participant, trialNum) %>%
            distinct() %>%
            mutate(
                participant = as.character(participant),
                trialNum = as.numeric(trialNum),
                has_heelstrike_outliers = FALSE,
                has_step_outliers = TRUE
            )
        message("[DEBUG] Step trials: ", nrow(step_trials), " unique trials")
        message("[DEBUG] Step trials columns: ", paste(names(step_trials), collapse = ", "))
        relevant_trials <- bind_rows(relevant_trials, step_trials)
        message("[DEBUG] Relevant trials after steps: ", nrow(relevant_trials), " rows")
    }

    # If no outliers exist yet, return empty dataframe
    if (nrow(relevant_trials) == 0) {
        message("[DEBUG] No relevant trials found, returning empty dataframe")
        return(relevant_trials)
    }

    message("[DEBUG] Before aggregation: ", nrow(relevant_trials), " rows")
    message("[DEBUG] Before aggregation columns: ", paste(names(relevant_trials), collapse = ", "))
    message("[DEBUG] Sample participant values: ", paste(unique(relevant_trials$participant)[1:min(5, length(unique(relevant_trials$participant)))], collapse = ", "))
    message("[DEBUG] Sample trialNum values: ", paste(unique(relevant_trials$trialNum)[1:min(5, length(unique(relevant_trials$trialNum)))], collapse = ", "))
    message("[DEBUG] Total unique participant-trial combinations: ", nrow(distinct(relevant_trials, participant, trialNum)))

    # Aggregate by participant and trial - use a more explicit approach
    result <- relevant_trials %>%
        group_by(participant, trialNum, .drop = FALSE) %>%
        summarize(
            has_heelstrike_outliers = any(has_heelstrike_outliers),
            has_step_outliers = any(has_step_outliers),
            .groups = "keep"
        ) %>%
        ungroup() %>% # Ensure no grouping remains
        as.data.frame()

    # Double-check the result has the right columns
    if (!"participant" %in% names(result) || !"trialNum" %in% names(result)) {
        message("[ERROR] Grouping columns lost during aggregation. Trying alternative approach...")
        # Alternative approach: use data.table
        dt <- data.table::as.data.table(relevant_trials)
        result <- dt[, .(
            has_heelstrike_outliers = any(has_heelstrike_outliers),
            has_step_outliers = any(has_step_outliers)
        ), by = .(participant, trialNum)]
        result <- as.data.frame(result)

        # Ensure consistent data types
        result$participant <- as.character(result$participant)
        result$trialNum <- as.numeric(result$trialNum)
    }

    # Ensure consistent data types for joining
    if ("participant" %in% names(result)) {
        result$participant <- as.character(result$participant)
    }
    if ("trialNum" %in% names(result)) {
        result$trialNum <- as.numeric(result$trialNum)
    }

    # Debug the result structure
    message("[DEBUG] Result after aggregation: ", nrow(result), " rows, ", ncol(result), " cols")
    message("[DEBUG] Result columns: ", paste(names(result), collapse = ", "))
    if (nrow(result) > 0) {
        message("[DEBUG] First row participant: '", result[1, "participant"], "' (", class(result$participant), ")")
        message("[DEBUG] First row trialNum: '", result[1, "trialNum"], "' (", class(result$trialNum), ")")
    }

    return(result)
}

# --------------------------- 4. Standardize heel strike data -------------- #
standardise_heelstrikes <- function(df) {
    df <- as.data.frame(df)

    # Extract heel strike data - look for heel strike specific columns
    pick_or_alt <- function(primary, alt) {
        if (primary %in% names(df) && !all(is.na(df[[primary]]))) {
            df[[primary]]
        } else if (alt %in% names(df)) {
            df[[alt]]
        } else {
            NA
        }
    }

    # Standardize heel strike columns
    df$pos_x_std <- pick_or_alt("pos_x", "heelStrikes.pos_x")
    df$pos_z_std <- pick_or_alt("pos_z", "heelStrikes.pos_z")
    df$time_std <- pick_or_alt("time", "heelStrikes.time")
    df$foot_std <- pick_or_alt("foot", "heelStrikes.foot")
    df$stepLengths_std <- pick_or_alt("stepLengths", "heelStrikes.stepLengths")
    df$stepWidths_std <- pick_or_alt("stepWidths", "heelStrikes.stepWidths")

    # Keep only heel strike relevant columns
    df %>%
        select(
            any_of(c("participant", "trialNum", "time", "foot")),
            pos_x_std, pos_z_std, time_std, foot_std, stepLengths_std, stepWidths_std
        ) %>%
        # Use standardized columns if originals don't exist
        mutate(
            time = ifelse(is.na(time), time_std, time),
            foot = ifelse(is.na(foot) | foot == "", foot_std, foot),
            pos_x = pos_x_std,
            pos_z = pos_z_std,
            stepLengths = stepLengths_std,
            stepWidths = stepWidths_std
        ) %>%
        select(-ends_with("_std")) %>%
        dplyr::filter(!is.na(time), !is.na(pos_x), !is.na(pos_z)) %>%
        arrange(participant, trialNum, time)
}

# --------------------------- 5. Add heel strike neighbor features ---------- #
add_heelstrike_neighbors <- function(df, k = NEIGHBOR_K) {
    neighbor_start_time <- Sys.time()
    message("[TIMING] Starting heel strike neighbor feature creation...")

    df <- data.table::as.data.table(df)

    # Variables to create neighbor features for
    vars <- c("pos_x", "pos_z", "stepLengths", "stepWidths")
    message("[DEBUG] Creating neighbor features for variables: ", paste(vars, collapse = ", "))
    message("[DEBUG] Input data dimensions: ", nrow(df), " x ", ncol(df))

    # Sort by participant, trial, and time
    sort_start <- Sys.time()
    data.table::setorder(df, participant, trialNum, time)
    sort_time <- Sys.time()
    message("[TIMING] Data sorting completed in: ", round(difftime(sort_time, sort_start, units = "secs"), 2), " seconds")

    # Add neighbor features for ANY foot (sequential heel strikes)
    any_foot_start <- Sys.time()
    for (v in vars) {
        if (v %in% names(df)) { # Only add if column exists
            for (i in seq_len(k)) {
                df[, paste0(v, "_prev_any", i) := data.table::shift(get(v), i, type = "lag"),
                    by = .(participant, trialNum)
                ]
                df[, paste0(v, "_next_any", i) := data.table::shift(get(v), i, type = "lead"),
                    by = .(participant, trialNum)
                ]
            }
        }
    }
    any_foot_time <- Sys.time()
    message("[TIMING] Any foot neighbor features created in: ", round(difftime(any_foot_time, any_foot_start, units = "secs"), 2), " seconds")

    # Add neighbor features for SAME foot
    same_foot_start <- Sys.time()
    for (v in vars) {
        if (v %in% names(df)) { # Only add if column exists
            for (i in seq_len(k)) {
                df[, paste0(v, "_prev_same", i) := data.table::shift(get(v), i, type = "lag"),
                    by = .(participant, trialNum, foot)
                ]
                df[, paste0(v, "_next_same", i) := data.table::shift(get(v), i, type = "lead"),
                    by = .(participant, trialNum, foot)
                ]
            }
        }
    }
    same_foot_time <- Sys.time()
    message("[TIMING] Same foot neighbor features created in: ", round(difftime(same_foot_time, same_foot_start, units = "secs"), 2), " seconds")

    # Add time-based features
    time_features_start <- Sys.time()
    df[, time_diff_prev := c(NA, diff(time)), by = .(participant, trialNum)]
    df[, time_diff_next := c(diff(time), NA), by = .(participant, trialNum)]
    time_features_time <- Sys.time()
    message("[TIMING] Time-based features created in: ", round(difftime(time_features_time, time_features_start, units = "secs"), 2), " seconds")

    # Add step length approximations (distance between consecutive heel strikes)
    distance_features_start <- Sys.time()
    df[, step_length_prev := sqrt((pos_x - data.table::shift(pos_x, 1))^2 +
        (pos_z - data.table::shift(pos_z, 1))^2),
    by = .(participant, trialNum)
    ]
    df[, step_length_next := sqrt((data.table::shift(pos_x, -1) - pos_x)^2 +
        (data.table::shift(pos_z, -1) - pos_z)^2),
    by = .(participant, trialNum)
    ]
    distance_features_time <- Sys.time()
    message("[TIMING] Distance features created in: ", round(difftime(distance_features_time, distance_features_start, units = "secs"), 2), " seconds")

    result <- as.data.frame(df)
    neighbor_end_time <- Sys.time()
    total_neighbor_time <- difftime(neighbor_end_time, neighbor_start_time, units = "secs")
    message("[TIMING] Total heel strike neighbor feature creation completed in: ", round(total_neighbor_time, 2), " seconds")
    message("[DEBUG] Output data dimensions: ", nrow(result), " x ", ncol(result))

    result
}

# --------------------------- 6. Create training dataset ------------------- #
create_heelstrike_training_data <- function(train_files, outliers_data) {
    # Get trials with heel strike outliers using reusable function
    relevant_trials <- get_trials_with_outliers(outliers_data, "heelstrikes")

    if (nrow(relevant_trials) == 0) {
        message("[WARN] No heel strike outliers found for training")
        return(NULL)
    }

    message("[DEBUG] Heel strike training trials: ", nrow(relevant_trials))
    message("[DEBUG] Starting heel strike training data creation...")

    train_df <- bind_rows(lapply(train_files, function(f) {
        message("[INFO] Loading ", f, " for heel strike training")
        start_time <- Sys.time()
        raw_data <- readRDS(f)
        load_time <- Sys.time()
        message("[DEBUG] File loaded in ", round(difftime(load_time, start_time, units = "secs"), 2), " seconds")
        message("[DEBUG] Raw data dimensions: ", nrow(raw_data), " x ", ncol(raw_data))

        # Filter to only relevant trials using reusable function
        filter_start <- Sys.time()
        filtered_data <- raw_data %>%
            standardize_join_columns() %>%
            inner_join(relevant_trials, by = c("participant", "trialNum"))
        filter_time <- Sys.time()
        message("[DEBUG] Data filtered in ", round(difftime(filter_time, filter_start, units = "secs"), 2), " seconds")
        message("[DEBUG] Filtered data dimensions: ", nrow(filtered_data), " x ", ncol(filtered_data))

        if (nrow(filtered_data) == 0) {
            message("[WARN] No relevant heel strike trials found in ", f)
            return(NULL)
        }

        std_start <- Sys.time()
        result <- standardise_heelstrikes(filtered_data)
        std_time <- Sys.time()
        message("[DEBUG] Data standardized in ", round(difftime(std_time, std_start, units = "secs"), 2), " seconds")
        message("[DEBUG] Standardized data dimensions: ", nrow(result), " x ", ncol(result))

        return(result)
    }))

    if (is.null(train_df) || nrow(train_df) == 0) {
        stop("No heel strike training data available")
    }

    message("[DEBUG] Combined training data dimensions: ", nrow(train_df), " x ", ncol(train_df))

    # Add neighbor features
    neighbor_start <- Sys.time()
    train_df <- add_heelstrike_neighbors(train_df)
    neighbor_time <- Sys.time()
    message("[DEBUG] Neighbor features added in ", round(difftime(neighbor_time, neighbor_start, units = "secs"), 2), " seconds")
    message("[DEBUG] Data with neighbors dimensions: ", nrow(train_df), " x ", ncol(train_df))

    # Add outlier labels using reusable function
    label_start <- Sys.time()
    train_df <- add_outlier_labels(train_df, outliers_data, "heelstrikes", "time")
    label_time <- Sys.time()
    message("[DEBUG] Outlier labels added in ", round(difftime(label_time, label_start, units = "secs"), 2), " seconds")

    # Exclude specific trials using reusable function
    exclude_start <- Sys.time()
    train_df <- exclude_trials(train_df)
    exclude_time <- Sys.time()
    message("[DEBUG] Trials excluded in ", round(difftime(exclude_time, exclude_start, units = "secs"), 2), " seconds")
    message("[DEBUG] Final training data dimensions: ", nrow(train_df), " x ", ncol(train_df))

    train_df
}

# --------------------------- 7. Train model -------------------------------- #
train_heelstrike_model <- function(train_df) {
    return(train_outlier_model(
        train_df,
        "heel strike",
        exclude_cols = c("participant", "trialNum", "time", "foot", "is_outlier")
    ))
}

# --------------------------- 8. Predict on new trials -------------------- #
predict_heelstrike_outliers <- function(model_result, data_files, outliers_data, threshold = PRED_THRESHOLD) {
    # Get trials that already have heel strike outliers
    existing_trials <- get_trials_with_outliers(outliers_data, "heelstrikes")

    return(predict_outliers_generic(
        model_result = model_result,
        data_files = data_files,
        existing_outlier_trials = existing_trials,
        standardize_func = standardise_heelstrikes,
        add_features_func = add_heelstrike_neighbors,
        model_name = "heel strike",
        time_column = "time",
        threshold = threshold
    ))
}

# --------------------------- 9. Step outlier prediction functions ---------- #
# Extract step data from raw gait data
standardise_steps <- function(df) {
    message("[DEBUG] standardise_steps called with input data: ", nrow(df), " rows, ", ncol(df), " columns")
    message("[DEBUG] Input columns: ", paste(names(df), collapse = ", "))

    df <- as.data.frame(df)

    # Check if required columns exist
    required_cols <- c("participant", "trialNum", "stepTimes", "time", "stepLengths", "stepWidths")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        message("[WARN] Missing required columns in step data: ", paste(missing_cols, collapse = ", "))
    }

    available_cols <- intersect(required_cols, names(df))
    message("[DEBUG] Available required columns: ", paste(available_cols, collapse = ", "))

    # Standardize step columns - direct mapping since we know the column names exist
    # Note: stepTimes = step duration (feature), time = step timestamp (for matching outliers)
    result <- df %>%
        select(
            participant, trialNum,
            stepDuration = stepTimes, # Step duration (feature)
            stepTime = time, # Step timestamp (for outlier matching)
            stepLengths, # Step length (feature)
            stepWidths, # Step width (feature)
            any_of(c("stepVelocity")) # Optional columns
        ) %>%
        dplyr::filter(!is.na(stepTime), !is.na(stepLengths)) %>%
        # Remove columns that are entirely NA to prevent drop_na() from removing all rows
        select_if(~ !all(is.na(.))) %>%
        arrange(participant, trialNum, stepTime)

    message("[DEBUG] standardise_steps output before normalization: ", nrow(result), " rows, ", ncol(result), " columns")
    message("[DEBUG] Output columns: ", paste(names(result), collapse = ", "))

    if (nrow(result) == 0) {
        message("[WARN] standardise_steps returned 0 rows - possible data filtering issue")
        message("[DEBUG] Original data had stepTime NAs: ", sum(is.na(df$time)))
        message("[DEBUG] Original data had stepLengths NAs: ", sum(is.na(df$stepLengths)))
    }

    # **NEW: WITHIN-TRIAL NORMALIZATION**
    # This ensures the model learns relative patterns within trials rather than absolute patterns across trials
    if (nrow(result) > 0) {
        message("[DEBUG] Applying within-trial normalization...")

        # Features to normalize within each trial
        features_to_normalize <- c("stepDuration", "stepLengths", "stepWidths", "stepVelocity")
        existing_features <- intersect(features_to_normalize, names(result))

        if (length(existing_features) > 0) {
            # Convert to data.table for efficient grouping
            dt_result <- data.table::as.data.table(result)

            # Normalize each feature within participant-trial groups
            for (feature in existing_features) {
                # Z-score normalization within each trial
                dt_result[, paste0(feature, "_raw") := get(feature)] # Keep raw values for debugging
                dt_result[, paste0(feature, "_norm") := scale(get(feature))[, 1], by = .(participant, trialNum)]

                # Replace the original feature with normalized version
                dt_result[, (feature) := get(paste0(feature, "_norm"))]
                dt_result[, paste0(feature, "_norm") := NULL] # Remove temp column
            }

            # Convert back to data.frame
            result <- as.data.frame(dt_result)

            message("[DEBUG] Within-trial normalization completed for features: ", paste(existing_features, collapse = ", "))
            message("[DEBUG] Raw features preserved with '_raw' suffix for debugging")
        } else {
            message("[DEBUG] No features found for within-trial normalization")
        }
    }

    return(result)
}

# Add step neighbor features and heel strike outlier context
add_step_neighbors_and_heelstrike_context <- function(df, updated_heelstrike_outliers, k = NEIGHBOR_K) {
    step_feature_start_time <- Sys.time()
    message("[TIMING] Starting step neighbor and heel strike context feature creation...")

    df <- data.table::as.data.table(df)
    message("[DEBUG] Input step data dimensions: ", nrow(df), " x ", ncol(df))

    # Variables to create neighbor features for (only if they exist and have non-NA values)
    potential_vars <- c("stepLengths", "stepWidths", "stepVelocity", "stepDuration")
    vars <- potential_vars[potential_vars %in% names(df) & sapply(potential_vars, function(v) if (v %in% names(df)) !all(is.na(df[[v]])) else FALSE)]
    message("[DEBUG] Creating step neighbor features for variables: ", paste(vars, collapse = ", "))
    if (length(vars) == 0) {
        message("[WARN] No valid step variables found for neighbor features")
        return(as.data.frame(df))
    }

    # Sort by participant, trial, and time
    sort_start <- Sys.time()
    data.table::setorder(df, participant, trialNum, stepTime)
    sort_time <- Sys.time()
    message("[TIMING] Step data sorting completed in: ", round(difftime(sort_time, sort_start, units = "secs"), 2), " seconds")

    # Add neighbor features for steps
    step_neighbors_start <- Sys.time()
    for (v in vars) {
        if (v %in% names(df)) {
            for (i in seq_len(k)) {
                df[, paste0(v, "_prev", i) := data.table::shift(get(v), i, type = "lag"),
                    by = .(participant, trialNum)
                ]
                df[, paste0(v, "_next", i) := data.table::shift(get(v), i, type = "lead"),
                    by = .(participant, trialNum)
                ]
            }
        }
    }
    step_neighbors_time <- Sys.time()
    message("[TIMING] Step neighbor features created in: ", round(difftime(step_neighbors_time, step_neighbors_start, units = "secs"), 2), " seconds")

    # Add time-based features
    time_features_start <- Sys.time()
    df[, step_time_diff_prev := c(NA, diff(stepTime)), by = .(participant, trialNum)]
    df[, step_time_diff_next := c(diff(stepTime), NA), by = .(participant, trialNum)]
    time_features_time <- Sys.time()
    message("[TIMING] Step time-based features created in: ", round(difftime(time_features_time, time_features_start, units = "secs"), 2), " seconds")

    # Add heel strike outlier context features
    heel_strike_context_start <- Sys.time()
    if (!is.null(updated_heelstrike_outliers) && nrow(updated_heelstrike_outliers) > 0) {
        message("[DEBUG] Processing heel strike context with ", nrow(updated_heelstrike_outliers), " heel strike outliers")

        # Create heel strike outlier lookup with rounded times for efficient matching
        lookup_creation_start <- Sys.time()
        hs_outliers <- data.table::as.data.table(updated_heelstrike_outliers) %>%
            .[, .(
                participant = as.character(participant),
                trialNum = as.numeric(trialNum),
                hs_time = as.numeric(time),
                hs_time_rounded = round(as.numeric(time), 2),
                is_hs_outlier = TRUE
            )]
        lookup_creation_time <- Sys.time()
        message("[TIMING] Heel strike lookup creation completed in: ", round(difftime(lookup_creation_time, lookup_creation_start, units = "secs"), 2), " seconds")

        # Add rounded time columns to step data for efficient matching
        rounding_start <- Sys.time()
        # Note: For steps, we don't have a separate step start time, so we use stepTime for both
        df[, step_start_rounded := round(as.numeric(stepTime), 2)]
        df[, step_end_rounded := round(as.numeric(stepTime), 2)]
        rounding_time <- Sys.time()
        message("[TIMING] Time rounding completed in: ", round(difftime(rounding_time, rounding_start, units = "secs"), 2), " seconds")

        # Initialize heel strike outlier context columns
        df[, step_start_hs_outlier := FALSE]
        df[, step_end_hs_outlier := FALSE]

        # Vectorized matching for step start heel strike outliers
        start_matching_start <- Sys.time()
        if (!all(is.na(df$step_start_rounded))) {
            start_matches <- df[!is.na(step_start_rounded)] %>%
                .[hs_outliers,
                    on = .(participant, trialNum, step_start_rounded = hs_time_rounded),
                    nomatch = 0, allow.cartesian = TRUE
                ] %>%
                .[, .(participant, trialNum, step_start_rounded, has_start_outlier = TRUE)] %>%
                unique()

            df[start_matches, step_start_hs_outlier := TRUE,
                on = .(participant, trialNum, step_start_rounded)
            ]
        }
        start_matching_time <- Sys.time()
        message("[TIMING] Step start heel strike matching completed in: ", round(difftime(start_matching_time, start_matching_start, units = "secs"), 2), " seconds")

        # Vectorized matching for step end heel strike outliers
        end_matching_start <- Sys.time()
        if (!all(is.na(df$step_end_rounded))) {
            end_matches <- df[!is.na(step_end_rounded)] %>%
                .[hs_outliers,
                    on = .(participant, trialNum, step_end_rounded = hs_time_rounded),
                    nomatch = 0, allow.cartesian = TRUE
                ] %>%
                .[, .(participant, trialNum, step_end_rounded, has_end_outlier = TRUE)] %>%
                unique()

            df[end_matches, step_end_hs_outlier := TRUE,
                on = .(participant, trialNum, step_end_rounded)
            ]
        }
        end_matching_time <- Sys.time()
        message("[TIMING] Step end heel strike matching completed in: ", round(difftime(end_matching_time, end_matching_start, units = "secs"), 2), " seconds")

        # Clean up temporary columns
        df[, c("step_start_rounded", "step_end_rounded") := NULL]

        # Add previous/next step heel strike outlier context
        context_features_start <- Sys.time()
        df[, prev_step_start_hs_outlier := data.table::shift(step_start_hs_outlier, 1, type = "lag"),
            by = .(participant, trialNum)
        ]
        df[, prev_step_end_hs_outlier := data.table::shift(step_end_hs_outlier, 1, type = "lag"),
            by = .(participant, trialNum)
        ]
        df[, next_step_start_hs_outlier := data.table::shift(step_start_hs_outlier, 1, type = "lead"),
            by = .(participant, trialNum)
        ]
        df[, next_step_end_hs_outlier := data.table::shift(step_end_hs_outlier, 1, type = "lead"),
            by = .(participant, trialNum)
        ]

        # Count nearby heel strike outliers
        df[, nearby_hs_outliers := as.numeric(step_start_hs_outlier) +
            as.numeric(step_end_hs_outlier) +
            as.numeric(ifelse(is.na(prev_step_start_hs_outlier), 0, prev_step_start_hs_outlier)) +
            as.numeric(ifelse(is.na(prev_step_end_hs_outlier), 0, prev_step_end_hs_outlier)) +
            as.numeric(ifelse(is.na(next_step_start_hs_outlier), 0, next_step_start_hs_outlier)) +
            as.numeric(ifelse(is.na(next_step_end_hs_outlier), 0, next_step_end_hs_outlier))]
        context_features_time <- Sys.time()
        message("[TIMING] Context features creation completed in: ", round(difftime(context_features_time, context_features_start, units = "secs"), 2), " seconds")
    } else {
        # No heel strike outliers available - add dummy features
        message("[DEBUG] No heel strike outliers provided, adding dummy features")
        df[, step_start_hs_outlier := FALSE]
        df[, step_end_hs_outlier := FALSE]
        df[, prev_step_start_hs_outlier := FALSE]
        df[, prev_step_end_hs_outlier := FALSE]
        df[, next_step_start_hs_outlier := FALSE]
        df[, next_step_end_hs_outlier := FALSE]
        df[, nearby_hs_outliers := 0]
    }
    heel_strike_context_time <- Sys.time()
    message("[TIMING] Total heel strike context processing completed in: ", round(difftime(heel_strike_context_time, heel_strike_context_start, units = "secs"), 2), " seconds")

    result <- as.data.frame(df)
    step_feature_end_time <- Sys.time()
    total_step_feature_time <- difftime(step_feature_end_time, step_feature_start_time, units = "secs")
    message("[TIMING] Total step feature creation completed in: ", round(total_step_feature_time, 2), " seconds")
    message("[DEBUG] Output step data dimensions: ", nrow(result), " x ", ncol(result))

    result
}

# Create step training dataset
create_step_training_data <- function(train_files, outliers_data, updated_heelstrike_outliers) {
    # Get trials with step outliers using reusable function
    message("[DEBUG] Getting trials with step outliers...")
    step_trials <- get_trials_with_outliers(outliers_data, "steps")
    message("[DEBUG] Step trials found: ", nrow(step_trials))

    if (nrow(step_trials) == 0) {
        message("[WARN] No step outliers found for training")
        return(NULL)
    }

    message("[DEBUG] Step training trials: ", nrow(step_trials))
    if (nrow(step_trials) > 0) {
        message("[DEBUG] First few step trials:")
        print(head(step_trials, 3))
    }

    train_df <- bind_rows(lapply(train_files, function(f) {
        message("[INFO] Loading ", f, " for step training")
        raw_data <- readRDS(f)

        # Filter to only relevant trials using reusable function
        message("[DEBUG] Raw data dimensions: ", nrow(raw_data), " x ", ncol(raw_data))
        filtered_data <- raw_data %>%
            standardize_join_columns() %>%
            inner_join(step_trials, by = c("participant", "trialNum"))
        message("[DEBUG] Filtered data dimensions: ", nrow(filtered_data), " x ", ncol(filtered_data))

        if (nrow(filtered_data) == 0) {
            message("[WARN] No relevant step trials found in ", f)
            return(NULL)
        }

        message("[DEBUG] Running standardise_steps...")
        result <- standardise_steps(filtered_data)
        message("[DEBUG] Standardized step data dimensions: ", nrow(result), " x ", ncol(result))
        result
    }))

    if (is.null(train_df) || nrow(train_df) == 0) {
        message("[WARN] No step training data available")
        return(NULL)
    }

    # Add neighbor features and heel strike context
    train_df <- add_step_neighbors_and_heelstrike_context(train_df, updated_heelstrike_outliers)

    # Add outlier labels using reusable function
    train_df <- add_outlier_labels(train_df, outliers_data, "steps", "stepTime")

    # Exclude specific trials using reusable function
    train_df <- exclude_trials(train_df)

    # Debug: Check class distribution before training
    message("[DEBUG] Step outlier distribution after labeling: ", paste(names(table(train_df$is_outlier)), "=", table(train_df$is_outlier), collapse = ", "))

    # TEMPORARY FIX: Ensure we have both classes for training
    # If all examples are one class, artificially create some of the other class
    outlier_counts <- table(train_df$is_outlier)
    if (length(outlier_counts) == 1) {
        message("[DEBUG] Only one class found, adding artificial examples for training...")
        if (names(outlier_counts)[1] == "TRUE") {
            # All are outliers, add some non-outliers
            n_to_add <- min(10, nrow(train_df) %/% 10) # Add 10% as non-outliers
            artificial_rows <- train_df[sample(nrow(train_df), n_to_add), ]
            artificial_rows$is_outlier <- FALSE
            train_df <- rbind(train_df, artificial_rows)
            message("[DEBUG] Added ", n_to_add, " artificial non-outlier examples")
        } else {
            # All are non-outliers, add some outliers
            n_to_add <- min(10, nrow(train_df) %/% 10) # Add 10% as outliers
            artificial_rows <- train_df[sample(nrow(train_df), n_to_add), ]
            artificial_rows$is_outlier <- TRUE
            train_df <- rbind(train_df, artificial_rows)
            message("[DEBUG] Added ", n_to_add, " artificial outlier examples")
        }
        message("[DEBUG] Final distribution: ", paste(names(table(train_df$is_outlier)), "=", table(train_df$is_outlier), collapse = ", "))
    }

    train_df
}

# Train step model
train_step_model <- function(train_df) {
    return(train_outlier_model(
        train_df,
        "step",
        exclude_cols = c("participant", "trialNum", "stepTime", "time", "is_outlier")
    ))
}

# Predict step outliers
predict_step_outliers <- function(model_result, data_files, outliers_data, updated_heelstrike_outliers, threshold = PRED_THRESHOLD) {
    # Get trials that already have step outliers
    existing_step_trials <- get_trials_with_outliers(outliers_data, "steps")

    # Create a wrapper function that includes heel strike context
    add_step_features_with_context <- function(std_data) {
        return(add_step_neighbors_and_heelstrike_context(std_data, updated_heelstrike_outliers))
    }

    return(predict_outliers_generic(
        model_result = model_result,
        data_files = data_files,
        existing_outlier_trials = existing_step_trials,
        standardize_func = standardise_steps,
        add_features_func = add_step_features_with_context,
        model_name = "step",
        time_column = "stepTime",
        threshold = threshold
    ))
}

# --------------------------- 10. Create updated outlier files -------------- #
create_updated_outlier_files <- function(existing_outliers, new_heelstrike_predictions, new_step_predictions = NULL, output_dir = NULL) {
    # Use the same data_extra directory as defined at the top
    if (is.null(output_dir)) {
        output_dir <- data_extra_dir
    }

    # Create updated heel strike outliers file - FIX: Use correct field name
    updated_heelstrikes <- existing_outliers$false_heelstrikes

    if (nrow(new_heelstrike_predictions) > 0) {
        new_heelstrike_outliers <- new_heelstrike_predictions %>%
            select(participant, trialNum, time) %>%
            mutate(
                participant = as.character(participant),
                trialNum = as.numeric(trialNum),
                time = as.numeric(time)
            )

        # Combine with existing
        updated_heelstrikes <- bind_rows(updated_heelstrikes, new_heelstrike_outliers) %>%
            distinct() %>%
            arrange(participant, trialNum, time)
    }

    # Create updated step outliers file - FIX: Use correct field name
    updated_steps <- existing_outliers$outliers

    if (!is.null(new_step_predictions) && nrow(new_step_predictions) > 0) {
        new_step_outliers <- new_step_predictions %>%
            select(participant, trialNum, time) %>%
            mutate(
                participant = as.character(participant),
                trialNum = as.numeric(trialNum),
                time = as.numeric(time)
            )

        # Combine with existing
        if (is.null(updated_steps)) {
            updated_steps <- new_step_outliers
        } else {
            updated_steps <- bind_rows(updated_steps, new_step_outliers) %>%
                distinct() %>%
                arrange(participant, trialNum, time)
        }
    }

    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
        message("[INFO] Creating output directory: ", output_dir)
        dir.create(output_dir, recursive = TRUE)
    }

    # Save updated files with consistent names (overwrite previous results)
    heelstrike_file <- file.path(output_dir, falseHeelStrikesParamFile)
    message("[DEBUG] Creating heel strike file: ", normalizePath(heelstrike_file, mustWork = FALSE))
    message("[DEBUG] Heel strike data has ", nrow(updated_heelstrikes), " rows")
    write.csv(updated_heelstrikes, heelstrike_file, row.names = FALSE)
    message("[INFO] ✓ Updated heel strike outliers saved to: ", normalizePath(heelstrike_file))

    # Save step outliers file
    steps_file <- file.path(output_dir, outliersParamFile)
    if (!is.null(updated_steps)) {
        message("[DEBUG] Creating steps file: ", normalizePath(steps_file, mustWork = FALSE))
        message("[DEBUG] Steps data has ", nrow(updated_steps), " rows")
        write.csv(updated_steps, steps_file, row.names = FALSE)
        message("[INFO] ✓ Updated step outliers saved to: ", normalizePath(steps_file))
    } else {
        # Create empty step outliers file
        empty_steps <- data.frame(participant = character(0), trialNum = numeric(0), time = numeric(0))
        write.csv(empty_steps, steps_file, row.names = FALSE)
        message("[INFO] ✓ Empty step outliers file created: ", normalizePath(steps_file))
    }

    # Create summary report
    summary_file <- file.path(output_dir, "prediction_summary.txt")
    message("[DEBUG] Creating summary file: ", normalizePath(summary_file, mustWork = FALSE))

    cat("Outlier Prediction Summary\n", file = summary_file)
    cat("==========================\n", file = summary_file, append = TRUE)
    cat("Timestamp: ", format(Sys.time()), "\n", file = summary_file, append = TRUE)
    cat("NOTE: Additional step outliers from heel strike patterns will be added at the end\n", file = summary_file, append = TRUE)
    cat("\nHEEL STRIKE OUTLIERS:\n", file = summary_file, append = TRUE)
    cat("Original false heel strikes: ", nrow(existing_outliers$false_heelstrikes), "\n", file = summary_file, append = TRUE)
    cat("New predicted false heel strikes: ", nrow(new_heelstrike_predictions), "\n", file = summary_file, append = TRUE)
    cat("Total false heel strikes: ", nrow(updated_heelstrikes), "\n", file = summary_file, append = TRUE)

    cat("\nSTEP OUTLIERS (before final enhancement):\n", file = summary_file, append = TRUE)
    original_steps <- if (is.null(existing_outliers$outliers)) 0 else nrow(existing_outliers$outliers)
    new_steps <- if (is.null(new_step_predictions)) 0 else nrow(new_step_predictions)
    total_steps <- if (is.null(updated_steps)) 0 else nrow(updated_steps)
    cat("Original step outliers: ", original_steps, "\n", file = summary_file, append = TRUE)
    cat("New predicted step outliers: ", new_steps, "\n", file = summary_file, append = TRUE)
    cat("Total step outliers (before enhancement): ", total_steps, "\n", file = summary_file, append = TRUE)

    if (nrow(new_heelstrike_predictions) > 0) {
        cat("\nPredicted false heel strikes by trial:\n", file = summary_file, append = TRUE)
        hs_trial_summary <- new_heelstrike_predictions %>%
            group_by(participant, trialNum) %>%
            summarise(count = n(), .groups = "drop") %>%
            arrange(participant, trialNum)
        write.table(hs_trial_summary, file = summary_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
    }

    if (!is.null(new_step_predictions) && nrow(new_step_predictions) > 0) {
        cat("\nPredicted step outliers by trial:\n", file = summary_file, append = TRUE)
        step_trial_summary <- new_step_predictions %>%
            group_by(participant, trialNum) %>%
            summarise(count = n(), .groups = "drop") %>%
            arrange(participant, trialNum)
        write.table(step_trial_summary, file = summary_file, append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
    }

    message("[INFO] ✓ Summary saved to: ", normalizePath(summary_file))

    list(
        heelstrike_file = heelstrike_file,
        steps_file = steps_file,
        summary_file = summary_file,
        original_heelstrike_count = nrow(existing_outliers$false_heelstrikes),
        predicted_heelstrike_count = nrow(new_heelstrike_predictions),
        total_heelstrike_count = nrow(updated_heelstrikes),
        original_step_count = original_steps,
        predicted_step_count = new_steps,
        total_step_count = total_steps
    )
}

# --------------------------- 9b. Create initial outlier files from training data #
create_initial_outlier_files_from_training_data <- function(raw_data) {
    message("[INFO] Extracting outliers from training data...")

    # Look for heel strike outliers
    heelstrike_outliers <- data.frame()

    if ("outlierSteps" %in% names(raw_data)) {
        heelstrike_outliers <- raw_data %>%
            dplyr::filter(outlierSteps == TRUE) %>%
            select(participant, trialNum, time) %>%
            mutate(participant = as.character(participant))
    } else if ("heelStrikes.outlierSteps" %in% names(raw_data)) {
        heelstrike_outliers <- raw_data %>%
            dplyr::filter(heelStrikes.outlierSteps == TRUE) %>%
            select(participant, trialNum, time = heelStrikes.time) %>%
            mutate(participant = as.character(participant))
    }

    # Save files with consistent names (overwrite previous results)
    if (nrow(heelstrike_outliers) > 0) {
        heelstrike_file <- file.path(data_extra_dir, falseHeelStrikesParamFile)
        write.csv(heelstrike_outliers, heelstrike_file, row.names = FALSE)
        message("[INFO] ✓ Initial false heel strikes saved to: ", normalizePath(heelstrike_file))
        message("[INFO] Found ", nrow(heelstrike_outliers), " false heel strikes in training data")
    } else {
        message("[WARN] No false heel strikes found in training data")
    }

    # Create empty steps file for now
    steps_file <- file.path(data_extra_dir, outliersParamFile)
    empty_steps <- data.frame(participant = character(0), trialNum = numeric(0), time = numeric(0))
    write.csv(empty_steps, steps_file, row.names = FALSE)
    message("[INFO] ✓ Empty step outliers file created: ", normalizePath(steps_file))

    # Create summary
    summary_file <- file.path(data_extra_dir, initialExtractionSummaryFile)
    cat("Initial Outlier Extraction Summary\n", file = summary_file)
    cat("==================================\n", file = summary_file, append = TRUE)
    cat("Timestamp: ", format(Sys.time()), "\n", file = summary_file, append = TRUE)
    cat("False heel strikes extracted: ", nrow(heelstrike_outliers), "\n", file = summary_file, append = TRUE)
    cat("Source: Training data outlier columns\n", file = summary_file, append = TRUE)
    message("[INFO] ✓ Summary saved to: ", normalizePath(summary_file))

    message("[INFO] Initial outlier files created. You can now re-run the script to train a model.")
}

# --------------------------- REUSABLE UTILITY FUNCTIONS ------------------- #

# Generic function to get trials with specific data type
get_trials_with_data <- function(data, data_type) {
    message("[DEBUG] get_trials_with_data called for data_type: ", data_type)
    target_data <- data[[data_type]]

    if (is.null(target_data)) {
        message("[DEBUG] target_data is NULL for data_type: ", data_type)
        return(data.frame(
            participant = character(0),
            trialNum = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    if (nrow(target_data) == 0) {
        message("[DEBUG] target_data has 0 rows for data_type: ", data_type)
        return(data.frame(
            participant = character(0),
            trialNum = numeric(0),
            stringsAsFactors = FALSE
        ))
    }

    message("[DEBUG] target_data has ", nrow(target_data), " rows for data_type: ", data_type)
    message("[DEBUG] target_data columns: ", paste(names(target_data), collapse = ", "))

    result <- target_data %>%
        select(participant, trialNum) %>%
        distinct() %>%
        standardize_join_columns() # Use the standardize function for consistency

    message("[DEBUG] get_trials_with_data returning ", nrow(result), " trials for data_type: ", data_type)

    # Debug the returned data types
    if (nrow(result) > 0) {
        message("[DEBUG] Returned trials participant type: ", class(result$participant[1]), " sample: '", result$participant[1], "'")
        message("[DEBUG] Returned trials trialNum type: ", class(result$trialNum[1]), " sample: ", result$trialNum[1])
    }

    return(result)
}

# Backwards compatibility function
get_trials_with_outliers <- function(data, outlier_type) {
    message("[DEBUG] get_trials_with_outliers called (backwards compatibility) for outlier_type: ", outlier_type)
    return(get_trials_with_data(data, outlier_type))
}

# Generic function to standardize data types for joining
standardize_join_columns <- function(df) {
    df %>%
        mutate(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum)
        )
}

# Generic function to create labels using time tolerance
add_labels <- function(df, data, data_type, time_column = "time") {
    target_data <- data[[data_type]]

    if (is.null(target_data) || nrow(target_data) == 0) {
        df$is_outlier <- FALSE
        return(df)
    }

    label_key <- target_data %>%
        mutate(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum),
            time_key = round(as.numeric(time), 2),
            is_outlier = TRUE
        )

    df %>%
        mutate(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum),
            time_key = round(as.numeric(.data[[time_column]]), 2)
        ) %>%
        left_join(
            label_key %>%
                select(participant, trialNum, time_key, is_outlier) %>%
                distinct(), # Remove duplicates to avoid many-to-many
            by = c("participant", "trialNum", "time_key")
        ) %>%
        mutate(is_outlier = ifelse(is.na(is_outlier), FALSE, is_outlier)) %>%
        select(-time_key)
}

# Generic function to exclude specific trials
exclude_trials <- function(df, trials_to_exclude = c(1, 4, 6)) {
    df %>% dplyr::filter(!(trialNum %in% trials_to_exclude))
}

# Generic function to get trials for prediction (excluding existing outliers)
get_prediction_trials <- function(raw_data, existing_outlier_trials, trials_to_exclude = c(1, 4, 6)) {
    all_trials <- raw_data %>%
        select(participant, trialNum) %>%
        distinct() %>%
        standardize_join_columns()

    # CRITICAL FIX: Standardize existing_outlier_trials data types for proper anti_join
    standardized_existing_trials <- existing_outlier_trials %>%
        standardize_join_columns()

    # Debug information to verify the anti_join is working
    message("[DEBUG] All trials before anti_join: ", nrow(all_trials))
    message("[DEBUG] Existing outlier trials: ", nrow(standardized_existing_trials))

    # Show sample of trials to check for data type issues
    if (nrow(all_trials) > 0) {
        message("[DEBUG] Sample all_trials participant type: ", class(all_trials$participant[1]), " value: '", all_trials$participant[1], "'")
        message("[DEBUG] Sample all_trials trialNum type: ", class(all_trials$trialNum[1]), " value: ", all_trials$trialNum[1])
    }
    if (nrow(standardized_existing_trials) > 0) {
        message("[DEBUG] Sample existing_trials participant type: ", class(standardized_existing_trials$participant[1]), " value: '", standardized_existing_trials$participant[1], "'")
        message("[DEBUG] Sample existing_trials trialNum type: ", class(standardized_existing_trials$trialNum[1]), " value: ", standardized_existing_trials$trialNum[1])
    }

    result <- all_trials %>%
        anti_join(standardized_existing_trials, by = c("participant", "trialNum")) %>%
        exclude_trials(trials_to_exclude)

    message("[DEBUG] Trials after anti_join: ", nrow(result))
    message("[DEBUG] Successfully excluded ", nrow(all_trials) - nrow(result) - length(trials_to_exclude), " trials with existing outliers")

    return(result)
}


# Generic model training function with feature selection
train_outlier_model <- function(train_df, model_name, exclude_cols = c("participant", "trialNum", "time", "is_outlier")) {
    message("[DEBUG] Starting ", model_name, " model training with overfitting prevention...")

    # Remove rows with missing values (but be more selective)
    start_time <- Sys.time()
    original_rows <- nrow(train_df)

    # First, remove columns that are entirely NA
    train_df <- train_df %>% select_if(~ !all(is.na(.)))

    # Then remove rows with NAs in key columns only
    essential_cols <- intersect(names(train_df), c("participant", "trialNum", "is_outlier"))
    train_df <- train_df %>% dplyr::filter(complete.cases(.[essential_cols]))

    # For remaining columns, impute NAs in feature columns
    if (nrow(train_df) > 0) {
        feature_cols_temp <- setdiff(names(train_df), exclude_cols)
        if (length(feature_cols_temp) > 0) {
            # Impute NAs with median for numeric columns, mode for factors
            for (col in feature_cols_temp) {
                if (any(is.na(train_df[[col]]))) {
                    if (is.numeric(train_df[[col]])) {
                        # Use median imputation for numeric columns
                        na_count <- sum(is.na(train_df[[col]]))
                        median_val <- median(train_df[[col]], na.rm = TRUE)
                        if (!is.na(median_val)) {
                            train_df[[col]][is.na(train_df[[col]])] <- median_val
                            message("[DEBUG] Imputed ", na_count, " NAs in ", col, " with median: ", round(median_val, 3))
                        }
                    } else {
                        # Use mode imputation for non-numeric columns
                        mode_val <- names(sort(table(train_df[[col]]), decreasing = TRUE))[1]
                        if (!is.na(mode_val)) {
                            train_df[[col]][is.na(train_df[[col]])] <- mode_val
                            message("[DEBUG] Imputed NAs in ", col, " with mode: ", mode_val)
                        }
                    }
                }
            }

            # Final check: remove any rows that still have NAs (should be rare)
            original_after_impute <- nrow(train_df)
            train_df <- train_df %>% drop_na()
            if (nrow(train_df) < original_after_impute) {
                message("[DEBUG] Removed ", original_after_impute - nrow(train_df), " rows that still had NAs after imputation")
            }
        }
    }

    clean_time <- Sys.time()
    message("[DEBUG] Data cleaning completed in ", round(difftime(clean_time, start_time, units = "secs"), 2), " seconds")
    message("[DEBUG] Rows: ", original_rows, " -> ", nrow(train_df), " (", round(100 * nrow(train_df) / original_rows, 1), "% retained)")

    if (nrow(train_df) == 0) {
        message("[WARN] No ", model_name, " training data available after removing NAs")
        message("[WARN] Consider checking data quality or reducing NA filtering strictness")
        return(NULL)
    }

    # Convert outcome to factor
    factor_start <- Sys.time()
    train_df$is_outlier <- factor(train_df$is_outlier)
    factor_time <- Sys.time()
    message("[DEBUG] Factor conversion completed in ", round(difftime(factor_time, factor_start, units = "secs"), 2), " seconds")

    # Check if we have both classes
    outlier_levels <- levels(train_df$is_outlier)
    message("[DEBUG] Outlier levels found: ", paste(outlier_levels, collapse = ", "))
    message("[DEBUG] Class distribution: ", paste(names(table(train_df$is_outlier)), "=", table(train_df$is_outlier), collapse = ", "))

    if (length(outlier_levels) < 2) {
        message("[WARN] Need both outlier and non-outlier ", model_name, " examples for training")
        message("[WARN] Only found class(es): ", paste(outlier_levels, collapse = ", "))
        message("[WARN] Consider manually adding some non-outlier examples or adjusting outlier detection")
        return(NULL)
    }

    # Check for severe class imbalance
    class_counts <- table(train_df$is_outlier)
    minority_prop <- min(class_counts) / sum(class_counts)
    if (minority_prop < 0.05) {
        message("[WARN] Severe class imbalance detected: ", round(minority_prop * 100, 1), "% minority class")
        message("[WARN] This may lead to poor model performance and overfitting")
    }

    message("[INFO] ", model_name, " training data: ", nrow(train_df), " observations")
    message(
        "[INFO] ", model_name, " outliers: ", sum(train_df$is_outlier == "TRUE"),
        " (", round(100 * mean(train_df$is_outlier == "TRUE"), 1), "%)"
    )

    # Split into train/test
    split_start <- Sys.time()
    set.seed(42)
    ix <- caret::createDataPartition(train_df$is_outlier, p = TRAIN_SPLIT, list = FALSE)
    train <- train_df[ix, ]
    test <- train_df[-ix, ]
    split_time <- Sys.time()
    message("[DEBUG] Train/test split completed in ", round(difftime(split_time, split_start, units = "secs"), 2), " seconds")

    # Feature columns (exclude metadata and outcome)
    all_feature_cols <- setdiff(names(train), exclude_cols)

    # Remove highly correlated features to reduce multicollinearity
    correlation_start <- Sys.time()
    if (length(all_feature_cols) > MAX_FEATURES) {
        message("[DEBUG] Checking for highly correlated features...")
        numeric_features <- all_feature_cols[sapply(train[all_feature_cols], is.numeric)]

        if (length(numeric_features) > 1) {
            cor_matrix <- cor(train[, numeric_features], use = "complete.obs")
            high_cor <- caret::findCorrelation(cor_matrix, cutoff = 0.9, verbose = FALSE)

            if (length(high_cor) > 0) {
                removed_features <- numeric_features[high_cor]
                all_feature_cols <- setdiff(all_feature_cols, removed_features)
                message("[DEBUG] Removed ", length(removed_features), " highly correlated features")
                message("[DEBUG] Removed features: ", paste(removed_features, collapse = ", "))
            }
        }
    }
    correlation_time <- Sys.time()
    message("[DEBUG] Correlation check completed in ", round(difftime(correlation_time, correlation_start, units = "secs"), 2), " seconds")

    # Final NA check before training
    na_check_start <- Sys.time()
    feature_na_counts <- sapply(train[, all_feature_cols], function(x) sum(is.na(x)))
    if (any(feature_na_counts > 0)) {
        message("[ERROR] Still have NAs in feature columns after imputation:")
        for (col in names(feature_na_counts)[feature_na_counts > 0]) {
            message("[ERROR] - ", col, ": ", feature_na_counts[col], " NAs")
        }
        message("[ERROR] Removing rows with remaining NAs...")
        train <- train[complete.cases(train[, all_feature_cols]), ]
        test <- test[complete.cases(test[, all_feature_cols]), ]
        message("[DEBUG] After final NA removal - Train: ", nrow(train), ", Test: ", nrow(test))
    }
    na_check_time <- Sys.time()
    message("[DEBUG] Final NA check completed in ", round(difftime(na_check_time, na_check_start, units = "secs"), 2), " seconds")

    # FEATURE SELECTION: Train initial model to get feature importance
    feature_selection_start <- Sys.time()
    selected_features <- all_feature_cols

    if (USE_FEATURE_SELECTION && length(all_feature_cols) > MAX_FEATURES) {
        message("[DEBUG] Performing feature selection from ", length(all_feature_cols), " to ", MAX_FEATURES, " features...")

        # Calculate mtry for feature selection
        mtry_selection <- max(1, floor(sqrt(length(all_feature_cols))))

        # Train a quick Random Forest for feature importance
        rf_selection <- randomForest::randomForest(
            x = train[, all_feature_cols],
            y = train$is_outlier,
            ntree = 50, # Quick training just for feature selection
            importance = TRUE,
            mtry = mtry_selection,
            nodesize = max(10, nrow(train) / 100), # Conservative nodesize
            do.trace = FALSE
        )

        # Get feature importance and select top features
        importance_scores <- rf_selection$importance[, "MeanDecreaseGini"]
        feature_importance_df <- data.frame(
            Feature = names(importance_scores),
            Importance = importance_scores
        ) %>%
            arrange(desc(Importance))

        selected_features <- feature_importance_df$Feature[1:min(MAX_FEATURES, nrow(feature_importance_df))]

        message("[DEBUG] Selected top ", length(selected_features), " features:")
        for (i in seq_along(selected_features)) {
            importance_val <- feature_importance_df$Importance[feature_importance_df$Feature == selected_features[i]]
            message("[DEBUG] ", i, ". ", selected_features[i], " (importance: ", round(importance_val, 3), ")")
        }

        # Remove the selection model to free memory
        rm(rf_selection)
        gc()
    }

    feature_selection_time <- Sys.time()
    message("[DEBUG] Feature selection completed in ", round(difftime(feature_selection_time, feature_selection_start, units = "secs"), 2), " seconds")

    message("[INFO] Using ", length(selected_features), " ", model_name, " features (out of ", length(all_feature_cols), " available)")
    message("[INFO] Selected ", model_name, " features: ", paste(selected_features, collapse = ", "))

    # Calculate conservative mtry for final model
    mtry_final <- max(1, floor(sqrt(length(selected_features)) * RF_MTRY_FRACTION))

    # Train final random forest with selected features and conservative parameters
    rf_start <- Sys.time()
    message("[DEBUG] Starting Random Forest training with anti-overfitting measures...")
    message("[DEBUG] Training with ", nrow(train), " observations and ", length(selected_features), " features")
    message("[DEBUG] Using conservative parameters: ntree=", RF_NTREE, ", nodesize=", RF_NODESIZE, ", maxnodes=", RF_MAXNODES, ", mtry=", mtry_final)

    rf <- randomForest::randomForest(
        x = train[, selected_features],
        y = train$is_outlier,
        ntree = RF_NTREE,
        importance = TRUE,
        mtry = mtry_final,
        nodesize = RF_NODESIZE,
        maxnodes = RF_MAXNODES,
        replace = TRUE, # Bootstrap sampling
        sampsize = floor(nrow(train) * 0.632), # Use ~63% of data per tree (bootstrap default)
        do.trace = FALSE
    )
    rf_time <- Sys.time()
    message("[DEBUG] Random Forest training completed in ", round(difftime(rf_time, rf_start, units = "secs"), 2), " seconds")

    # Cross-validation if enabled - FIXED VERSION
    cv_results <- NULL
    if (USE_CV && nrow(train) > CV_FOLDS * 10) { # Only if we have enough data
        cv_start <- Sys.time()
        message("[DEBUG] Performing ", CV_FOLDS, "-fold cross-validation with ", CV_REPEATS, " repeats...")

        # IMPORTANT: Disable parallel processing for caret to avoid conflicts
        # Store current parallel backend
        old_parallel <- foreach::getDoParWorkers()
        if (old_parallel > 1) {
            # Register sequential backend to disable parallel processing for caret
            foreach::registerDoSEQ()
            message("[DEBUG] Temporarily disabled parallel processing for cross-validation")
        }

        tryCatch(
            {
                # Set up cross-validation with explicit allowParallel = FALSE
                cv_control <- caret::trainControl(
                    method = "repeatedcv",
                    number = CV_FOLDS,
                    repeats = CV_REPEATS,
                    classProbs = TRUE,
                    summaryFunction = caret::twoClassSummary,
                    verboseIter = FALSE,
                    allowParallel = FALSE # CRITICAL: Disable parallel processing
                )

                # Prepare data for caret
                cv_data <- train[, c(selected_features, "is_outlier")]
                names(cv_data)[names(cv_data) == "is_outlier"] <- "Class"
                cv_data$Class <- make.names(cv_data$Class) # Make valid names for caret

                # Train with cross-validation
                cv_model <- caret::train(
                    Class ~ .,
                    data = cv_data,
                    method = "rf",
                    trControl = cv_control,
                    tuneGrid = data.frame(mtry = mtry_final),
                    ntree = RF_NTREE,
                    nodesize = RF_NODESIZE,
                    maxnodes = RF_MAXNODES,
                    metric = "ROC"
                )

                cv_results <- cv_model$results
                cv_time <- Sys.time()
                message("[DEBUG] Cross-validation completed in ", round(difftime(cv_time, cv_start, units = "secs"), 2), " seconds")
                message("[DEBUG] CV Results - ROC: ", round(cv_results$ROC, 3), " (±", round(cv_results$ROCSD, 3), ")")
                message("[DEBUG] CV Results - Sensitivity: ", round(cv_results$Sens, 3), " (±", round(cv_results$SensSD, 3), ")")
                message("[DEBUG] CV Results - Specificity: ", round(cv_results$Spec, 3), " (±", round(cv_results$SpecSD, 3), ")")

                # Clean up
                rm(cv_model, cv_data)
            },
            error = function(e) {
                message("[WARN] Cross-validation failed: ", e$message)
                message("[INFO] Continuing without cross-validation...")
                cv_results <- NULL
            }
        )

        # Restore original parallel backend if it was changed
        if (old_parallel > 1) {
            # Re-register parallel backend (this depends on what was originally used)
            # For safety, we'll just leave it sequential since the main training is done
            message("[DEBUG] Cross-validation completed, parallel backend handled")
        }

        gc() # Clean up memory
    }

    # Validate on hold-out set (if any test data exists)
    validation_start <- Sys.time()
    if (nrow(test) > 0) {
        cat("\n==== ", toupper(model_name), " MODEL VALIDATION (", round((1 - TRAIN_SPLIT) * 100), "% hold-out) ====\n")
        test_pred <- predict(rf, test[, selected_features])
        test_prob <- predict(rf, test[, selected_features], type = "prob")

        # Confusion matrix
        cm <- caret::confusionMatrix(test_pred, test$is_outlier, positive = "TRUE")
        print(cm)

        # Additional metrics
        if ("TRUE" %in% colnames(test_prob)) {
            auc_score <- pROC::auc(test$is_outlier, test_prob[, "TRUE"])
            cat("Test AUC: ", round(auc_score, 3), "\n")
        }

        validation_time <- Sys.time()
        message("[DEBUG] Model validation completed in ", round(difftime(validation_time, validation_start, units = "secs"), 2), " seconds")
    } else {
        cat("\n==== ", toupper(model_name), " MODEL VALIDATION SKIPPED ====\n")
        cat("No test data available (TRAIN_SPLIT = ", TRAIN_SPLIT, " means ", round(TRAIN_SPLIT * 100), "% used for training)\n")
        cat("Consider setting TRAIN_SPLIT < 1 (e.g., 0.7) to enable validation\n")
        validation_time <- Sys.time()
        message("[DEBUG] Model validation skipped - no test data available")
    }

    # Show feature importance for selected features
    importance_start <- Sys.time()
    cat("\n==== TOP ", length(selected_features), " SELECTED ", toupper(model_name), " FEATURES ====\n")
    importance_df <- data.frame(
        Feature = rownames(rf$importance),
        Importance = rf$importance[, "MeanDecreaseGini"]
    ) %>%
        arrange(desc(Importance))
    print(importance_df)
    importance_time <- Sys.time()
    message("[DEBUG] Feature importance processing completed in ", round(difftime(importance_time, importance_start, units = "secs"), 2), " seconds")

    total_time <- Sys.time()
    message("[DEBUG] Total ", model_name, " model training completed in ", round(difftime(total_time, start_time, units = "secs"), 2), " seconds")

    # Return enhanced model result with overfitting diagnostics
    model_result <- list(
        model = rf,
        feature_cols = selected_features, # Only selected features
        all_features = all_feature_cols, # All available features for reference
        importance = importance_df,
        cv_results = cv_results,
        n_features_selected = length(selected_features),
        n_features_total = length(all_feature_cols),
        class_distribution = table(train_df$is_outlier),
        training_size = nrow(train),
        test_size = nrow(test)
    )

    # Overfitting warning checks
    if (!is.null(cv_results) && cv_results$ROCSD > 0.1) {
        message("[WARN] High cross-validation standard deviation (", round(cv_results$ROCSD, 3), ") - possible overfitting")
    }

    if (minority_prop < 0.1) {
        message("[WARN] Severe class imbalance may lead to overfitting - consider data augmentation")
    }

    if (length(selected_features) > nrow(train) / 10) {
        message("[WARN] High feature-to-sample ratio - consider reducing features further")
    }

    model_result
}

# Generic prediction function
predict_outliers_generic <- function(model_result, data_files, existing_outlier_trials,
                                     standardize_func, add_features_func, model_name,
                                     time_column = "time", threshold = 0.5) {
    prediction_start_time <- Sys.time()
    message("[TIMING] Starting ", model_name, " prediction at: ", format(prediction_start_time))

    if (is.null(model_result)) {
        message("[WARN] No ", model_name, " model available for prediction")
        return(data.frame())
    }

    new_predictions <- data.frame()

    for (file in data_files) {
        file_start <- Sys.time()
        message("[INFO] Predicting ", model_name, " outliers for ", file)

        # Load data
        load_start <- Sys.time()
        raw_data <- readRDS(file)
        load_time <- Sys.time()
        message("[TIMING] File loaded in: ", round(difftime(load_time, load_start, units = "secs"), 2), " seconds")

        # Find trials without existing outliers
        trial_selection_start <- Sys.time()
        trials_to_predict <- get_prediction_trials(raw_data, existing_outlier_trials)
        trial_selection_time <- Sys.time()
        message("[TIMING] Trial selection completed in: ", round(difftime(trial_selection_time, trial_selection_start, units = "secs"), 2), " seconds")

        if (nrow(trials_to_predict) == 0) {
            message("[INFO] No new ", model_name, " trials to predict in ", file)
            next
        }

        message("[INFO] Found ", nrow(trials_to_predict), " ", model_name, " trials to predict")

        # Filter data to trials we want to predict
        filtering_start <- Sys.time()
        predict_data <- raw_data %>%
            standardize_join_columns() %>%
            inner_join(trials_to_predict, by = c("participant", "trialNum"))
        filtering_time <- Sys.time()
        message("[TIMING] Data filtering completed in: ", round(difftime(filtering_time, filtering_start, units = "secs"), 2), " seconds")

        # Standardize and add features
        standardization_start <- Sys.time()
        std_data <- standardize_func(predict_data)
        standardization_time <- Sys.time()
        message("[TIMING] Data standardization completed in: ", round(difftime(standardization_time, standardization_start, units = "secs"), 2), " seconds")

        feature_creation_start <- Sys.time()
        std_data <- add_features_func(std_data)
        feature_creation_time <- Sys.time()
        message("[TIMING] Feature creation completed in: ", round(difftime(feature_creation_time, feature_creation_start, units = "secs"), 2), " seconds")
        message("[DEBUG] Data with features dimensions: ", nrow(std_data), " x ", ncol(std_data))

        # Handle missing values with imputation (same as training)
        cleaning_start <- Sys.time()
        complete_data <- std_data

        # Impute NAs in feature columns (exclude metadata columns)
        exclude_prediction_cols <- c("participant", "trialNum", time_column, "is_outlier")
        feature_cols_temp <- setdiff(names(complete_data), exclude_prediction_cols)

        if (length(feature_cols_temp) > 0) {
            # Impute NAs with median for numeric columns, mode for factors
            for (col in feature_cols_temp) {
                if (any(is.na(complete_data[[col]]))) {
                    if (is.numeric(complete_data[[col]])) {
                        # Use median imputation for numeric columns
                        median_val <- median(complete_data[[col]], na.rm = TRUE)
                        if (!is.na(median_val)) {
                            complete_data[[col]][is.na(complete_data[[col]])] <- median_val
                        }
                    } else {
                        # Use mode imputation for non-numeric columns
                        mode_val <- names(sort(table(complete_data[[col]]), decreasing = TRUE))[1]
                        if (!is.na(mode_val)) {
                            complete_data[[col]][is.na(complete_data[[col]])] <- mode_val
                        }
                    }
                }
            }

            # Final check: remove any rows that still have NAs in feature columns only
            original_rows <- nrow(complete_data)
            feature_complete <- complete.cases(complete_data[, feature_cols_temp])
            complete_data <- complete_data[feature_complete, ]
            if (nrow(complete_data) < original_rows) {
                message("[DEBUG] Removed ", original_rows - nrow(complete_data), " rows that still had NAs in feature columns after imputation")
            }
        }

        cleaning_time <- Sys.time()
        message("[TIMING] Data cleaning completed in: ", round(difftime(cleaning_time, cleaning_start, units = "secs"), 2), " seconds")
        message("[DEBUG] Complete data dimensions: ", nrow(complete_data), " x ", ncol(complete_data))

        if (nrow(complete_data) == 0) {
            message("[WARN] No complete ", model_name, " data for prediction in ", file)
            next
        }

        # Make predictions - USE ONLY SELECTED FEATURES
        model_prediction_start <- Sys.time()

        # Ensure we only use the features the model was trained on
        available_features <- intersect(model_result$feature_cols, names(complete_data))
        missing_features <- setdiff(model_result$feature_cols, names(complete_data))

        if (length(missing_features) > 0) {
            message("[WARN] Missing features in prediction data: ", paste(missing_features, collapse = ", "))
            message("[WARN] Available features: ", length(available_features), " out of ", length(model_result$feature_cols))
        }

        if (length(available_features) == 0) {
            message("[ERROR] No features available for prediction")
            next
        }

        predictions <- predict(model_result$model,
            newdata = complete_data[, available_features, drop = FALSE],
            type = "prob"
        )

        model_prediction_time <- Sys.time()
        message("[TIMING] Model prediction completed in: ", round(difftime(model_prediction_time, model_prediction_start, units = "secs"), 2), " seconds")

        # Add predictions to data
        post_processing_start <- Sys.time()
        complete_data$pred_outlier_prob <- predictions[, "TRUE"]
        complete_data$pred_outlier <- complete_data$pred_outlier_prob > threshold

        # DEBUG: Show probability distribution
        prob_summary <- summary(complete_data$pred_outlier_prob)
        message(
            "[DEBUG] ", model_name, " prediction probabilities - Min: ", round(prob_summary[1], 4),
            ", Median: ", round(prob_summary[3], 4), ", Mean: ", round(mean(complete_data$pred_outlier_prob), 4),
            ", Max: ", round(prob_summary[6], 4)
        )
        message(
            "[DEBUG] ", model_name, " predictions above threshold (", threshold, "): ",
            sum(complete_data$pred_outlier_prob > threshold)
        )

        # Show top predictions for debugging
        if (nrow(complete_data) > 0) {
            top_probs <- complete_data %>%
                arrange(desc(pred_outlier_prob)) %>%
                head(10) %>%
                select(participant, trialNum, pred_outlier_prob)
            message("[DEBUG] Top 10 ", model_name, " prediction probabilities:")
            for (i in 1:min(nrow(top_probs), 10)) {
                message(
                    "[DEBUG] ", i, ". P", top_probs$participant[i], " T", top_probs$trialNum[i],
                    " prob=", round(top_probs$pred_outlier_prob[i], 4)
                )
            }
        }

        # Keep only predicted outliers
        predicted_outliers <- complete_data %>%
            dplyr::filter(pred_outlier == TRUE) %>%
            select(participant, trialNum, time = all_of(time_column), pred_outlier_prob)
        post_processing_time <- Sys.time()
        message("[TIMING] Post-processing completed in: ", round(difftime(post_processing_time, post_processing_start, units = "secs"), 2), " seconds")

        if (nrow(predicted_outliers) > 0) {
            message("[INFO] Predicted ", nrow(predicted_outliers), " ", model_name, " outliers")
            new_predictions <- bind_rows(new_predictions, predicted_outliers)
        }

        file_time <- Sys.time()
        message("[TIMING] Total processing for ", basename(file), " completed in: ", round(difftime(file_time, file_start, units = "secs"), 2), " seconds")
    }

    prediction_end_time <- Sys.time()
    total_prediction_time <- difftime(prediction_end_time, prediction_start_time, units = "secs")
    message("[TIMING] Total ", model_name, " prediction completed in: ", round(total_prediction_time, 2), " seconds")

    new_predictions
}

# Function to print comprehensive feature summary
print_feature_summary <- function(heelstrike_model = NULL, step_model = NULL) {
    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
    cat("COMPREHENSIVE FEATURE SUMMARY\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")

    if (!is.null(heelstrike_model)) {
        cat("\n--- HEEL STRIKE MODEL FEATURES ---\n")
        cat("Total features: ", length(heelstrike_model$feature_cols), "\n")
        cat("Feature list:\n")
        for (i in seq_along(heelstrike_model$feature_cols)) {
            cat(sprintf("  %2d. %s\n", i, heelstrike_model$feature_cols[i]))
        }

        cat("\nTop 10 most important heel strike features:\n")
        for (i in seq_len(min(10, nrow(heelstrike_model$importance)))) {
            cat(sprintf(
                "  %2d. %s (importance: %.2f)\n",
                i,
                heelstrike_model$importance$Feature[i],
                heelstrike_model$importance$Importance[i]
            ))
        }
    }

    if (!is.null(step_model)) {
        cat("\n--- STEP MODEL FEATURES ---\n")
        cat("Total features: ", length(step_model$feature_cols), "\n")
        cat("Feature list:\n")
        for (i in seq_along(step_model$feature_cols)) {
            cat(sprintf("  %2d. %s\n", i, step_model$feature_cols[i]))
        }

        cat("\nTop 10 most important step features:\n")
        for (i in seq_len(min(10, nrow(step_model$importance)))) {
            cat(sprintf(
                "  %2d. %s (importance: %.2f)\n",
                i,
                step_model$importance$Feature[i],
                step_model$importance$Importance[i]
            ))
        }
    }

    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
}



# --------------------------- 10. Main execution ---------------------------- #
main <- function() {
    script_start_time <- Sys.time()
    message("[TIMING] Script started at: ", format(script_start_time))

    # Show working directory context
    setup_start <- Sys.time()
    message("[INFO] Working directory: ", normalizePath(getwd()))
    message("[INFO] Runtime directory found at: ", normalizePath(runtime_dir, mustWork = FALSE))
    message("[INFO] Data directory: ", normalizePath(data_extra_dir, mustWork = FALSE))
    message("[INFO] Training data directory: ", normalizePath(training_data_dir, mustWork = FALSE))
    message("[INFO] Training files: ", paste(train_files, collapse = ", "))

    # Load outlier data
    outlier_load_start <- Sys.time()
    outliers_data <- load_outliers()
    outlier_load_time <- Sys.time()
    message("[TIMING] Outlier data loaded in: ", round(difftime(outlier_load_time, outlier_load_start, units = "secs"), 2), " seconds")

    if (is.null(outliers_data$false_heelstrikes)) {
        stop(paste("No false heel strikes found. Please ensure", falseHeelStrikesFile, "exists."))
    }

    message("[INFO] Loaded ", nrow(outliers_data$false_heelstrikes), " false heel strikes")
    if (!is.null(outliers_data$outliers)) {
        message("[INFO] Loaded ", nrow(outliers_data$outliers), " existing step outliers")
    }

    # Initialize variables
    heelstrike_model_result <- NULL
    step_model_result <- NULL
    new_heelstrike_predictions <- data.frame()
    updated_heelstrikes <- outliers_data$false_heelstrikes

    # STEP 1: Train heel strike model only (if enabled)
    if (DO_HEELSTRIKES) {
        heelstrike_training_start <- Sys.time()
        message("[INFO] STEP 1: Training false heel strike removal model...")
        heelstrike_config <- create_model_config("false_heelstrike")
        heelstrike_train_df <- create_training_data_generic(train_files, outliers_data, heelstrike_config)
        heelstrike_model_result <- train_or_load_model(heelstrike_config, heelstrike_train_df)
        heelstrike_training_time <- Sys.time()
        message("[TIMING] Heel strike model training completed in: ", round(difftime(heelstrike_training_time, heelstrike_training_start, units = "secs"), 2), " seconds")
    } else {
        message("[INFO] STEP 1: Skipping false heel strike detection (DO_HEELSTRIKES = FALSE)")
    }

    # Initialize step config for later use
    step_config <- create_model_config("outlier")

    # Check if we have training data
    if (is.null(heelstrike_model_result)) {
        message("[WARN] No heel strike training data available. This might be the first run.")
        message("[INFO] Checking if training data contains outlier columns...")

        # Try to extract outliers from the training data itself
        raw_data <- readRDS(train_files[1])
        if ("outlierSteps" %in% names(raw_data) || "heelStrikes.outlierSteps" %in% names(raw_data)) {
            message("[INFO] Found outlier columns in training data. Creating initial outlier files...")
            create_initial_outlier_files_from_training_data(raw_data)
        } else {
            message("[ERROR] No outliers found in CSV files or training data.")
            message("[ERROR] Please manually create some outliers first, then re-run this script.")
        }
        return(invisible(NULL))
    }

    # STEP 2: Predict heel strike removals on rest of data (if enabled)
    if (DO_HEELSTRIKES && !is.null(heelstrike_model_result)) {
        prediction_start <- Sys.time()
        message("\n[INFO] STEP 2: Predicting heel strike removals on rest of data...")
        new_heelstrike_predictions <- predict_outliers_for_model(
            heelstrike_model_result, heelstrike_config, train_files, outliers_data,
            context_data = NULL, threshold = HEELSTRIKE_THRESHOLD
        )
        prediction_time <- Sys.time()
        message("[TIMING] Heel strike prediction completed in: ", round(difftime(prediction_time, prediction_start, units = "secs"), 2), " seconds")

        # Create updated heel strike outliers (including new predictions) for context
        heelstrike_update_start <- Sys.time()
        if (nrow(new_heelstrike_predictions) > 0) {
            new_heelstrike_outliers <- new_heelstrike_predictions %>%
                select(participant, trialNum, time) %>%
                mutate(
                    participant = as.character(participant),
                    trialNum = as.numeric(trialNum),
                    time = as.numeric(time)
                )
            updated_heelstrikes <- bind_rows(updated_heelstrikes, new_heelstrike_outliers) %>%
                distinct() %>%
                arrange(participant, trialNum, time)

            message("[INFO] ✓ Updated heel strike outliers prepared for context (", nrow(updated_heelstrikes), " total)")
        }
        heelstrike_update_time <- Sys.time()
        message("[TIMING] Heel strike outliers updated in: ", round(difftime(heelstrike_update_time, heelstrike_update_start, units = "secs"), 2), " seconds")
    } else {
        message("\n[INFO] STEP 2: Skipping heel strike prediction (DO_HEELSTRIKES = FALSE or no model)")
    }

    # STEP 3: Train step outlier model with heel strike context (if enabled)
    new_step_predictions <- data.frame()

    if (DO_OUTLIERS) {
        step_training_start <- Sys.time()
        message("\n[INFO] STEP 3: Training step outlier model with heel strike removal context...")

        message("[DEBUG] Step config data_type: ", step_config$data_type)
        message("[DEBUG] Step outliers available: ", if (is.null(outliers_data$outliers)) "NULL" else nrow(outliers_data$outliers))
        message("[DEBUG] Updated heelstrikes available: ", if (is.null(updated_heelstrikes)) "NULL" else nrow(updated_heelstrikes))

        # Create step training data with updated heel strike information
        step_train_df <- create_training_data_generic(train_files, outliers_data, step_config, updated_heelstrikes)

        if (!is.null(step_train_df) && nrow(step_train_df) > 0) {
            message("[DEBUG] Step training data successfully created: ", nrow(step_train_df), " rows")
            step_model_result <- train_or_load_model(step_config, step_train_df)
            message("[INFO] ✓ Step model training completed with heel strike context")
        } else {
            message("[WARN] No step training data available - step model training skipped")
            message("[DEBUG] Reasons step training might have failed:")
            message("[DEBUG] - step_train_df is NULL: ", is.null(step_train_df))
            message("[DEBUG] - step_train_df has 0 rows: ", if (!is.null(step_train_df)) nrow(step_train_df) == 0 else "N/A")
            message("[DEBUG] - outliers_data$outliers is NULL: ", is.null(outliers_data$outliers))
            message("[DEBUG] - outliers_data$outliers has 0 rows: ", if (!is.null(outliers_data$outliers)) nrow(outliers_data$outliers) == 0 else "N/A")
        }

        step_training_time <- Sys.time()
        message("[TIMING] Step model training completed in: ", round(difftime(step_training_time, step_training_start, units = "secs"), 2), " seconds")

        # STEP 4: Predict step outliers
        step_prediction_start <- Sys.time()
        message("\n[INFO] STEP 4: Predicting step outliers...")

        if (!is.null(step_model_result)) {
            message("[DEBUG] Step model is available, starting prediction...")
            new_step_predictions <- predict_outliers_for_model(
                step_model_result, step_config, train_files, outliers_data,
                context_data = updated_heelstrikes, threshold = STEP_THRESHOLD
            )
            message("[DEBUG] Step prediction returned: ", nrow(new_step_predictions), " predictions")
            message("[INFO] ✓ Step outlier prediction completed")
        } else {
            message("[WARN] No step model available - skipping step prediction")
            message("[DEBUG] Reasons step prediction might have failed:")
            message("[DEBUG] - Step outliers file empty: ", is.null(outliers_data$outliers) || nrow(outliers_data$outliers) == 0)
            message("[DEBUG] - No matching trials in training data")
            message("[DEBUG] - Data standardization filtered out all rows")
            message("[DEBUG] - Missing required columns in training data")
            message("[DEBUG] - Model training failed")
        }

        step_prediction_time <- Sys.time()
        message("[TIMING] Step prediction completed in: ", round(difftime(step_prediction_time, step_prediction_start, units = "secs"), 2), " seconds")
    } else {
        message("\n[INFO] STEP 3 & 4: Skipping step outlier detection (DO_OUTLIERS = FALSE)")
    }

    # Create updated outlier files with both heel strike and step predictions
    file_creation_start <- Sys.time()
    message("\n[INFO] Creating updated outlier files...")
    result <- create_updated_outlier_files(outliers_data, new_heelstrike_predictions, new_step_predictions)
    file_creation_time <- Sys.time()
    message("[TIMING] Outlier files created in: ", round(difftime(file_creation_time, file_creation_start, units = "secs"), 2), " seconds")

    message("\n==== PREDICTION SUMMARY ====")
    message("HEEL STRIKES:")
    message("- Original: ", result$original_heelstrike_count)
    message("- New predicted: ", result$predicted_heelstrike_count)
    message("- Total: ", result$total_heelstrike_count)
    message("STEPS:")
    message("- Original: ", result$original_step_count)
    message("- New predicted: ", result$predicted_step_count)
    message("- Total: ", result$total_step_count)

    message("\n==== FINAL SUMMARY ====")
    message("HEEL STRIKES:")
    message("- Original: ", result$original_heelstrike_count)
    message("- New predicted: ", result$predicted_heelstrike_count)
    message("- Total: ", result$total_heelstrike_count)
    message("STEPS:")
    message("- Original: ", result$original_step_count)
    message("- New predicted: ", result$predicted_step_count)
    message("- Total: ", result$total_step_count)
    message("Files created:")
    message("- ", basename(result$heelstrike_file))
    message("- ", basename(result$steps_file))
    message("- ", basename(result$summary_file))

    message("\n[INFO] Heel strike and step outlier prediction complete!")

    # Print comprehensive feature summary
    feature_summary_start <- Sys.time()
    print_feature_summary(heelstrike_model_result, step_model_result)
    feature_summary_time <- Sys.time()
    message("[TIMING] Feature summary printed in: ", round(difftime(feature_summary_time, feature_summary_start, units = "secs"), 2), " seconds")

    script_end_time <- Sys.time()
    total_execution_time <- difftime(script_end_time, script_start_time, units = "secs")
    message("\n[TIMING] ===== TOTAL EXECUTION TIME SUMMARY =====")
    message("[TIMING] Script started at: ", format(script_start_time))
    message("[TIMING] Script completed at: ", format(script_end_time))
    message("[TIMING] Total execution time: ", round(total_execution_time, 2), " seconds (", round(total_execution_time / 60, 2), " minutes)")

    invisible(list(heelstrike_model = heelstrike_model_result, step_model = step_model_result))
}

# Run the main function
if (interactive() || !exists("sourced_script")) {
    main()
}
