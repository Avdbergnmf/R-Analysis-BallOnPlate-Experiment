#' Initialization Script for Heavy Operations and Global Parameters
#'
#' This file contains operations that are expensive and should only run once
#' during setup, not every time source files are loaded in parallel workers.
#' It also consolidates all configuration parameters in one place.

#' Initialize all global parameters and configuration
initialize_global_parameters <- function() {
    cat("Initializing global parameters...\n")

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
    # DATA ANALYSIS CONFIGURATION
    # =============================================================================
    # Column options for data analysis
    xOptions <<- c("time", "pos_x", "pos_y", "pos_z", "actual_pos_z")

    # Categories for data grouping and analysis
    categories <<- c("participant", "condition", "trialNum")
    columns_to_not_summarize <<- c("visualizations", "perturbations")
    categoriesExtra <<- c(categories, columns_to_not_summarize)
    categoriesExtraInputs <<- append(categoriesExtra, c("heelStrikes.foot", "slice_index", "None"))

    # =============================================================================
    # OUTLIER DETECTION PARAMETERS
    # =============================================================================
    outlier_col_names <<- c("outlierSteps", "heelStrikes.outlierSteps")

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

    cat("Global parameters initialized.\n")
}

#' Initialize global data that's expensive to load
initialize_global_data <- function() {
    # Ensure parameters are initialized first
    if (!exists("dataFolder", envir = .GlobalEnv)) {
        initialize_global_parameters()
    }

    # Load participant list and file system data (from data_loading.R)
    cat("Loading participant list and file mappings...\n")
    participants <<- list.dirs(path = dataFolder, full.names = FALSE, recursive = FALSE)

    # Load filename dictionary (from data_loading.R)
    filenameDict_raw <- data.table::fread(file.path(dataExtraFolder, "filenameDict.csv"), stringsAsFactors = FALSE)
    filenameDict <<- setNames(as.list(filenameDict_raw[[2]]), filenameDict_raw[[1]])
    trackers <<- names(filenameDict)
    trackerPath <<- file.path(file.path(dataFolder, participants[1]), "trackers")

    # Load rotation data once (from pre_processing.R)
    cat("Loading rotation data...\n")
    rotations_data <<- get_rotations_data()

    # Load example data for column names (from calc_all_gait_params.R)
    cat("Loading column options...\n")
    if (exists("participants") && length(participants) > 0) {
        tryCatch(
            {
                xOptions2D <<- colnames(get_t_data(participants[1], "leftfoot", 1))
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

    cat("Global data initialization complete.\n")
}

#' Check if global data is initialized, and initialize if needed
#' Ensure global data and parameters are initialized (lazy initialization)
ensure_global_data_initialized <- function() {
    # Check if parameters are initialized
    if (!exists("dataFolder", envir = .GlobalEnv)) {
        initialize_global_parameters()
    }

    # Check if data is initialized
    if (!exists("rotations_data", envir = .GlobalEnv) ||
        !exists("xOptions2D", envir = .GlobalEnv)) {
        initialize_global_data()
    }
}
