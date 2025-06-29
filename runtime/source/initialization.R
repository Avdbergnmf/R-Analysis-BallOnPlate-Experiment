#' Initialization Script for Heavy Operations
#'
#' This file contains operations that are expensive and should only run once
#' during setup, not every time source files are loaded in parallel workers.
#'
#' These variables are created once and then available globally.

#' Initialize global data that's expensive to load
initialize_global_data <- function() {
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
ensure_global_data_initialized <- function() {
    if (!exists("rotations_data", envir = .GlobalEnv) ||
        !exists("xOptions2D", envir = .GlobalEnv)) {
        initialize_global_data()
    }
}
