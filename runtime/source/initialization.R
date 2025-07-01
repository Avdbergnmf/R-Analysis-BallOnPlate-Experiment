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
    categories <<- c("participant", "condition", "trialNum")
    columns_to_not_summarize <<- c("visualizations", "perturbations")
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

    # =============================================================================
    # VISUALIZATION PARAMETERS
    # =============================================================================
    # Default plotting parameters
    default_base_size <<- 10
    default_plot_width <<- 8
    default_plot_height <<- 4
    default_alpha <<- 0.15

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
            filenameDict_raw <- data.table::fread(file.path(dataExtraFolder, "filenameDict.csv"),
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
                    step_file <- file.path(dataExtraFolder, "outliers_steps.csv")
                    heel_file <- file.path(dataExtraFolder, "outliers_heelstrikes.csv")

                    if (file.exists(step_file)) {
                        outliers_steps_data <<- data.table::fread(step_file,
                            stringsAsFactors = FALSE, verbose = FALSE
                        )
                    } else {
                        outliers_steps_data <<- data.frame(
                            participant = character(0), trialNum = numeric(0), time = numeric(0)
                        )
                    }

                    if (file.exists(heel_file)) {
                        outliers_heel_data <<- data.table::fread(heel_file,
                            stringsAsFactors = FALSE, verbose = FALSE
                        )
                    } else {
                        outliers_heel_data <<- data.frame(
                            participant = character(0), trialNum = numeric(0), time = numeric(0)
                        )
                    }

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
