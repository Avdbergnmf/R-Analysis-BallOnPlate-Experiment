#' Initialization Script for Heavy Operations and Global Parameters
#'
#' This file contains operations that are expensive and should only run once
#' during setup, not every time source files are loaded in parallel workers.
#' It also consolidates all configuration parameters in one place.

#' Initialize all global parameters and configuration
initialize_global_parameters <- function() {
    cat(sprintf("[%s] Initializing global parameters...\n", format(Sys.time(), "%H:%M:%S")))

    # =============================================================================
    # FILE PATHS AND DIRECTORIES
    # =============================================================================
    dataFolder <<- "data"
    dataExtraFolder <<- "data_extra"
    questionnaireInfoFolder <<- "questionnaires"
    resultsFolder <<- "results"

    # =============================================================================
    # OUTLIER FILE NAMES
    # =============================================================================
    falseHeelStrikesFile <<- "false_heelstrikes_final.csv"
    outliersFile <<- "outliers_final.csv"
    rotationsFile <<- "rotations_kinematic_data.csv"
    filenameDictFile <<- "filenameDict.csv"

    # Additional outlier prediction files
    falseHeelStrikesParamFile <<- "false_heelstrikes_param.csv"
    outliersParamFile <<- "outliers_param.csv"
    initialExtractionSummaryFile <<- "initial_extraction_summary.txt"

    # =============================================================================
    # COMPONENT FILE PATHS
    # =============================================================================
    # Use absolute path to avoid working directory issues when knitting child documents
    root_dir <<- getwd()
    page16_components_folder <<- file.path(root_dir, "pages", "page16_components")
    page16_utils_file <<- file.path(page16_components_folder, "page16_utils.R")
    page16_data_manager_file <<- file.path(page16_components_folder, "page16_data_manager.R")
    page16_components_html_file <<- file.path(page16_components_folder, "page16_components.html")

    # =============================================================================
    # QUESTIONNAIRE CONFIGURATION
    # =============================================================================
    qTypes <<- c("IMI", "UserExperience")
    qAnswers <<- c("baseline_task", "training2", "retention", "transfer")

    # =============================================================================
    # TRIAL CONFIGURATION
    # =============================================================================
    allTrials <<- c(2, 3, 5, 7, 8, 9, 10, 11)
    # trial 1 is just selecting the walking speed
    # trial 4 is standing familiarization with the task
    # trial 6 is familiarization with condition, and was unstructured and different depending on the condition.

    # Define training trials (trials 7 and 8)
    trainingTrials <<- c(7, 8)

    # Define default durations for each trial (in seconds)
    default_durations <<- list(
        "1" = 120, # warmup - 2 minutes
        "2" = 300, # baseline - 5 minutes
        "3" = 300, # familiarisation_walk - 5 minutes
        "4" = 120, # familiarisation_stand - 2 minutes
        "5" = 300, # baseline_task - 5 minutes
        "6" = 120, # familiarisation_training - 2 minutes
        "7" = 600, # training1 - 10 minutes
        "8" = 600, # training2 - 10 minutes
        "9" = 300, # washout - 5 minutes
        "10" = 300, # retention - 5 minutes
        "11" = 300 # transfer - 5 minutes
    )

    taskNum <<- list(
        "1" = 0, # warmup
        "2" = 0, # baseline
        "3" = 0, # familiarisation_walk
        "4" = 0, # familiarisation_stand
        "5" = 1, # baseline_task
        "6" = 1, # familiarisation_training
        "7" = 2, # training1
        "8" = 3, # training2
        "9" = 3, # washout
        "10" = 4, # retention
        "11" = 5 # transfer
    )

    # Define all phase names in order
    allPhases <<- c(
        "warmup", "baseline", "familiarisation_walk", "familiarisation_stand",
        "baseline_task", "familiarisation_training", "training", "washout",
        "retention", "transfer"
    )

    # Define phase names for each trial (built from allPhases)
    default_phases <<- list(
        "1" = allPhases[1], # warmup
        "2" = allPhases[2], # baseline
        "3" = allPhases[3], # familiarisation_walk
        "4" = allPhases[4], # familiarisation_stand
        "5" = allPhases[5], # baseline_task
        "6" = allPhases[6], # familiarisation_training
        "7" = allPhases[7], # training
        "8" = allPhases[7], # training (same phase for both training trials)
        "9" = allPhases[8], # washout
        "10" = allPhases[9], # retention
        "11" = allPhases[10] # transfer
    )

    # =============================================================================
    # PERFORMANCE CONFIGURATION
    # =============================================================================
    # Parallel processing control
    USE_PARALLEL <<- TRUE # Enable/disable parallel processing for expensive operations
    ENABLE_FILE_LOGGING <<- TRUE # Enable/disable parallel process logging to files

    # Cache control
    FORCE_RECALC <<- FALSE # Set to TRUE to ignore cached RDS files and recompute datasets

    # Signal filtering control
    USE_CONTINUOUS_FILTERING <<- TRUE # Enable/disable 4Hz low-pass filter on continuous data before complexity calculation

    # =============================================================================
    # DATA ANALYSIS CONFIGURATION
    # =============================================================================
    # Column options for data analysis
    xOptions <<- c("time", "pos_x", "pos_y", "pos_z", "actual_pos_z")

    # Categories for data grouping and analysis
    # Added gender (biological sex) and motion-sickness sensitivity so they can
    # be used for grouping / faceting in summary tables and plots.
    categories <<- c("participant", "trialNum", "condition", "phase")

    # Columns that are descriptive (should not be summarized numerically).
    # Include demographic / questionnaire details so they are preserved in
    # summary tables but excluded from mean/SD calculations.
    columns_to_not_summarize <<- c(
        "visualizations", "perturbations",
        "age", "weight", "education", "vr_experience", "height_meters", "gender", "motion", "trialNumWithinPhase", "phaseNum", "taskNum"
    )
    categoriesExtra <<- c(categories, columns_to_not_summarize, "suspect")
    categoriesExtraInputs <<- append(categoriesExtra, c("foot", "slice_index", "None"))

    # =============================================================================
    # OUTLIER DETECTION PARAMETERS
    # =============================================================================
    outlier_col_names <<- c("outlierSteps")
    suspect_col_names <<- c("suspect")

    # =============================================================================
    # SIGNAL PROCESSING PARAMETERS
    # =============================================================================
    # Default parameters for filtering and signal processing
    default_poly_order <<- 4
    default_fs <<- 200 # Default sampling frequency
    default_cutoff_freq <<- 5
    hampel_k <<- 7
    hampel_t0 <<- 3

    # =============================================================================
    # COMPLEXITY ANALYSIS PARAMETERS
    # =============================================================================
    # Default parameters for complexity metrics
    complexity_sampen_m <<- 2
    complexity_sampen_r_factor <<- 0.2
    complexity_mse_max_scale <<- 30
    complexity_dfa_min_box <<- 4
    complexity_dfa_max_frac <<- 0.25
    complexity_dfa_n_points <<- 20
    complexity_lyapunov_emb_dim <<- 5
    complexity_lyapunov_delay <<- 1
    complexity_lyapunov_fit_range <<- c(0, 1)

    # =============================================================================
    # SIMULATION PARAMETERS
    # =============================================================================
    # Default simulation parameters
    default_dt <<- 0.005
    default_sigma_a <<- 1.0
    default_sigma_m <<- 0.00001
    default_min_attempt_duration <<- 0

    # -----------------------------------------------------------------------------
    # LEVEL → ARC-DEG LOOKUP (used to correct mis-recorded 'deg' in level tracker)
    # -----------------------------------------------------------------------------
    # Levels 0–9 map to arc degrees decreasing in 1/3 increments from 8 to 5.
    # We round to 6 decimals to match recorded precision conventions. 
    # This was added later because the level tracker was not recording the correct 
    # values (always logging the previous value).
    arcdeg_lookup_table <<- stats::setNames(
        round(8 - (0:9) / 3, 6),
        as.character(0:9)
    )

    # =============================================================================
    # AVATAR CONFIGURATION
    # =============================================================================
    # Avatar height in meters (used for converting height_scale to actual height)
    avatar_height_m <<- 1.85

    # =============================================================================
    # VISUALIZATION PARAMETERS
    # =============================================================================
    # Default plotting parameters
    default_base_size <<- 10
    default_plot_width <<- 8
    default_plot_height <<- 4
    default_alpha <<- 0.15

    # =============================================================================
    # CONDITION MAPPING
    # =============================================================================
    # Mapping from condition names to abbreviated labels for plotting and tables
    condition_map <<- c(
        "control" = "C",
        "perturbation" = "P",
        "perturbation_visualization" = "PV"
    )

    # =============================================================================
    # PHASE MAPPING
    # =============================================================================
    # Mapping from phase names to abbreviated labels for plotting and tables
    phase_map <<- c(
        "baseline" = "BL",
        "training" = "TR",
        "retention" = "RE",
        "transfer" = "TF",
        "washout" = "WO",
        "familiarisation_walk" = "FW",
        "familiarisation_stand" = "FS",
        "familiarisation_training" = "FT"
    )

    # =============================================================================
    # FACTOR LEVEL DEFINITIONS
    # =============================================================================
    # Common factor levels that appear in effect names (for cleaning variable names)
    factor_levels <<- c(
        # Boolean levels
        "TRUE", "FALSE",
        # Polynomial contrasts
        ".L", ".Q", ".C",
        # Numeric levels
        "[0-9.]+",
        # Condition levels
        names(condition_map),
        # Phase levels
        names(phase_map)
    )

    # =============================================================================
    # VARIABLE MAPPING CONFIGURATION
    # =============================================================================
    # Define which variables should use which mapping for pretty labels
    variable_mappings <<- list(
        "condition" = condition_map,
        "phase" = phase_map
        # Add more mappings here as needed
    )

    # =============================================================================
    # REFERENCE CATEGORY CONFIGURATION
    # =============================================================================
    # Define reference levels for factor variables (for statistical models)
    # These will be used when creating factors in the data
    reference_levels <<- list(
        "condition" = "control", # control will be the reference level
        "phase" = "baseline_task", # baseline_task will be the reference level
        "gender" = "Male", # male will be the reference level
        "motion" = "FALSE", # low motion sensitivity will be the reference level
        "education" = "Higher", # bachelor+ will be the reference level
        "vr_experience" = "Never" # no VR experience will be the reference level
        # Add more reference levels here as needed
    )

    cat(sprintf("[%s] Global parameters initialized.\n", format(Sys.time(), "%H:%M:%S")))
}

#' Initialize global data that's expensive to load - OPTIMIZED VERSION
#' This version uses efficient data structures and cleanup to reduce overhead
initialize_global_data <- function() {
    # Set flag to prevent recursive initialization
    .INITIALIZING_GLOBAL_DATA <<- TRUE

    tryCatch(
        {
            # Ensure parameters are initialized first
            if (!exists("dataFolder", envir = .GlobalEnv)) {
                initialize_global_parameters()
            }

            cat(sprintf("[%s] Loading participant list and file mappings (optimized)...\n", format(Sys.time(), "%H:%M:%S")))

            # Load participant list efficiently
            participants <<- list.dirs(path = dataFolder, full.names = FALSE, recursive = FALSE)

            # Load filename dictionary with efficient approach
            cat(sprintf("[%s] Loading filename dictionary...\n", format(Sys.time(), "%H:%M:%S")))
            filenameDict_raw <- data.table::fread(file.path(dataExtraFolder, filenameDictFile),
                stringsAsFactors = FALSE,
                verbose = FALSE
            ) # Suppress verbose output

            # Create efficient named list and clean up intermediate data
            filenameDict <<- setNames(as.list(filenameDict_raw[[2]]), filenameDict_raw[[1]])
            trackers <<- names(filenameDict)
            trackerPath <<- file.path(file.path(dataFolder, participants[1]), "trackers")

            # Clean up intermediate data immediately
            rm(filenameDict_raw)
            gc()

            # Load outlier data (separate step and heel-strike files)
            cat(sprintf("[%s] Loading outlier data...\n", format(Sys.time(), "%H:%M:%S")))
            tryCatch(
                {
                    step_file <- file.path(dataExtraFolder, outliersFile)
                    heel_file <- file.path(dataExtraFolder, falseHeelStrikesFile)

                    if (file.exists(step_file)) {
                        outliers_steps_data <<- data.table::fread(step_file,
                            stringsAsFactors = FALSE, verbose = FALSE
                        )
                    } else {
                        outliers_steps_data <<- data.table::as.data.table(data.frame(
                            participant = character(0), trialNum = numeric(0), time = numeric(0)
                        ))
                    }

                    if (file.exists(heel_file)) {
                        outliers_heel_data <<- data.table::fread(heel_file,
                            stringsAsFactors = FALSE, verbose = FALSE
                        )
                    } else {
                        outliers_heel_data <<- data.table::as.data.table(data.frame(
                            participant = character(0), trialNum = numeric(0), time = numeric(0)
                        ))
                    }

                    # Ensure both are data.table for downstream key setting
                    outliers_steps_data <<- data.table::as.data.table(outliers_steps_data)
                    outliers_heel_data <<- data.table::as.data.table(outliers_heel_data)

                    data.table::setkey(outliers_steps_data, participant, trialNum, time)
                    data.table::setkey(outliers_heel_data, participant, trialNum, time)

                    cat(sprintf(
                        "[%s] Loaded %d step outliers and %d heel-strike outliers\n",
                        format(Sys.time(), "%H:%M:%S"), nrow(outliers_steps_data), nrow(outliers_heel_data)
                    ))
                },
                error = function(e) {
                    warning("Could not load outlier data: ", e$message)
                    outliers_steps_data <<- data.frame(
                        participant = character(0), trialNum = numeric(0), time = numeric(0)
                    )
                    outliers_heel_data <<- data.frame(
                        participant = character(0), trialNum = numeric(0), time = numeric(0)
                    )
                }
            )

            # Load rotation data with fallback handling
            cat(sprintf("[%s] Loading rotation data (with fallback)...\n", format(Sys.time(), "%H:%M:%S")))
            tryCatch(
                {
                    rotations_data <<- get_rotations_data()
                    # If rotations_data is large, consider converting to data.table for efficiency
                    if (is.data.frame(rotations_data) && nrow(rotations_data) > 1000) {
                        rotations_data <<- data.table::as.data.table(rotations_data)
                    }
                },
                error = function(e) {
                    warning("Could not load rotation data (get_rotations_data not available): ", e$message)
                    rotations_data <<- data.frame() # Empty data frame as fallback
                }
            )

            # Load example data for column names with minimal loading
            cat(sprintf("[%s] Loading column options (minimal data loading)...\n", format(Sys.time(), "%H:%M:%S")))
            if (exists("participants") && length(participants) > 0) {
                tryCatch(
                    {
                        # Instead of loading full data, just get column names efficiently
                        temp_data <- get_t_data(participants[1], "leftfoot", 1)
                        xOptions2D <<- colnames(temp_data)
                        # Clean up immediately
                        rm(temp_data)
                        gc()
                    },
                    error = function(e) {
                        warning("Could not load xOptions2D: ", e$message)
                        xOptions2D <<- c("time", "pos_x", "pos_y", "pos_z", "rot_x", "rot_y", "rot_z")
                    }
                )
            } else {
                warning("participants not defined, using default xOptions2D")
                xOptions2D <<- c("time", "pos_x", "pos_y", "pos_z", "rot_x", "rot_y", "rot_z")
            }

            cat(sprintf("[%s] Global data initialization complete (optimized).\n", format(Sys.time(), "%H:%M:%S")))

            # Clear the initialization flag
            rm(.INITIALIZING_GLOBAL_DATA, envir = .GlobalEnv)

            # Final garbage collection after initialization
            gc()
        },
        error = function(e) {
            # Clean up flag on error
            if (exists(".INITIALIZING_GLOBAL_DATA", envir = .GlobalEnv)) {
                rm(.INITIALIZING_GLOBAL_DATA, envir = .GlobalEnv)
            }
            stop("Global data initialization failed: ", e$message)
        }
    )
}

#' Escape special regex characters in a string
#' @param str The string to escape
#' @return The escaped string
escape_regex_chars <- function(str) {
    gsub("([.^$*+?()[\\]|])", "\\\\\\1", str)
}

#' Efficient version of ensure_global_data_initialized
#' This version includes cleanup but no memory monitoring
ensure_global_data_initialized <- function() {
    # Skip initialization if we're in the middle of worker sourcing to prevent infinite loops
    if (exists(".WORKER_INITIALIZING", envir = .GlobalEnv)) {
        return()
    }

    # Skip initialization if we're already initializing global data to prevent infinite loops
    if (exists(".INITIALIZING_GLOBAL_DATA", envir = .GlobalEnv)) {
        return()
    }

    # Check if parameters are initialized
    if (!exists("dataFolder", envir = .GlobalEnv)) {
        initialize_global_parameters()
    }

    # Check if data is initialized
    if (!exists("participants", envir = .GlobalEnv) ||
        !exists("filenameDict", envir = .GlobalEnv) ||
        !exists("rotations_data", envir = .GlobalEnv) ||
        !exists("outliers_steps_data", envir = .GlobalEnv) ||
        !exists("outliers_heel_data", envir = .GlobalEnv) ||
        !exists("xOptions2D", envir = .GlobalEnv)) {
        initialize_global_data()
    }
}

#' Clean effect names by extracting base variable names from factor effects
#' @param effect_name The effect name from the model (e.g., "conditionperturbation")
#' @param input_vars Vector of input variable names to match against
#' @return The cleaned base variable name (e.g., "condition")
clean_effect_name <- function(effect_name, input_vars) {
    ensure_global_data_initialized()

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

#' Apply reference levels to factor variables in a dataset
#' @param data The dataset to modify
#' @return The dataset with proper reference levels set for factors
apply_reference_levels <- function(data) {
    ensure_global_data_initialized()

    # Apply reference levels to each variable that has a defined reference level
    for (var_name in names(reference_levels)) {
        if (var_name %in% names(data)) {
            # Check if the variable exists and has the specified reference level
            if (reference_levels[[var_name]] %in% unique(data[[var_name]])) {
                data[[var_name]] <- relevel(factor(data[[var_name]]), ref = reference_levels[[var_name]])
                # cat(sprintf("[INFO] Set reference level for %s to '%s'\n", var_name, reference_levels[[var_name]]))
            } else {
                warning(sprintf(
                    "Reference level '%s' not found in variable '%s'. Available levels: %s",
                    reference_levels[[var_name]], var_name,
                    paste(unique(data[[var_name]]), collapse = ", ")
                ))
            }
        }
    }

    return(data)
}
