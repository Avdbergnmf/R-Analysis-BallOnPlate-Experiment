# Page 16 Data Manager - Extracted from vibe coding mess
# =====================================================

# Data Manager class to handle outlier state management
OutlierDataManager <- R6::R6Class("OutlierDataManager",
    public = list(
        # Initialize empty outlier data frames
        initialize = function() {
            private$.heelstrike_outliers <- data.frame(
                participant = character(0),
                trialNum = numeric(0),
                time = numeric(0),
                stringsAsFactors = FALSE
            )

            private$.step_outliers <- data.frame(
                participant = character(0),
                trialNum = numeric(0),
                time = numeric(0),
                stringsAsFactors = FALSE
            )

            private$.change_tracking <- list(
                changes_made = FALSE,
                last_hs_hash = NULL,
                last_step_hash = NULL,
                last_hs_filename = NULL,
                last_step_filename = NULL
            )
        },

        # Getters
        get_heelstrike_outliers = function() private$.heelstrike_outliers,
        get_step_outliers = function() private$.step_outliers,
        get_current_heelstrike_outliers = function(participant, trial) {
            private$.heelstrike_outliers %>%
                dplyr::filter(participant == !!participant & trialNum == as.numeric(trial))
        },
        get_current_step_outliers = function(participant, trial) {
            private$.step_outliers %>%
                dplyr::filter(participant == !!participant & trialNum == as.numeric(trial))
        },

        # Setters
        set_heelstrike_outliers = function(data) {
            private$.heelstrike_outliers <- data
            private$update_change_tracking()
        },
        set_step_outliers = function(data) {
            private$.step_outliers <- data
            private$update_change_tracking()
        },

        # File operations
        load_outlier_files = function(heel_file, step_file, data_folder) {
            results <- list(heel_loaded = 0, step_loaded = 0, errors = c())

            # Load heel strike outliers
            heel_path <- file.path(data_folder, heel_file)
            if (file.exists(heel_path)) {
                result <- private$load_single_file(heel_path, "heel")
                if (result$success) {
                    private$.heelstrike_outliers <- result$data
                    results$heel_loaded <- nrow(result$data)
                    private$.change_tracking$last_hs_filename <- heel_file
                } else {
                    results$errors <- c(results$errors, result$error)
                }
            }

            # Load step outliers
            step_path <- file.path(data_folder, step_file)
            if (file.exists(step_path)) {
                result <- private$load_single_file(step_path, "step")
                if (result$success) {
                    private$.step_outliers <- result$data
                    results$step_loaded <- nrow(result$data)
                    private$.change_tracking$last_step_filename <- step_file
                } else {
                    results$errors <- c(results$errors, result$error)
                }
            }

            # Reset change tracking after successful load
            if (length(results$errors) == 0) {
                private$reset_change_tracking()
            }

            return(results)
        },
        save_outlier_files = function(data_folder, overwrite = FALSE, custom_filenames = NULL) {
            base_hs <- private$.change_tracking$last_hs_filename %||% "heelstrike_outliers.csv"
            base_step <- private$.change_tracking$last_step_filename %||% "step_outliers.csv"

            # Determine filenames
            if (!is.null(custom_filenames)) {
                filenames <- custom_filenames
            } else if (overwrite) {
                filenames <- list(hs = base_hs, step = base_step)
            } else {
                filenames <- list(
                    hs = create_outlier_filename(base_hs, timestamp = TRUE),
                    step = create_outlier_filename(base_step, timestamp = TRUE)
                )
            }

            # Calculate changes
            changes <- private$calculate_file_changes(data_folder, filenames)

            # Write files
            write.csv(private$.heelstrike_outliers, file.path(data_folder, filenames$hs), row.names = FALSE)
            write.csv(private$.step_outliers, file.path(data_folder, filenames$step), row.names = FALSE)

            # Update tracking
            private$.change_tracking$last_hs_filename <- filenames$hs
            private$.change_tracking$last_step_filename <- filenames$step
            private$reset_change_tracking()

            return(list(
                hs_path = file.path(data_folder, filenames$hs),
                step_path = file.path(data_folder, filenames$step),
                hs_filename = filenames$hs,
                step_filename = filenames$step,
                hs_changes = changes$hs,
                step_changes = changes$step
            ))
        },

        # Outlier manipulation
        toggle_outlier = function(clicked_time, participant, trial, trial_data, mode, threshold = DEFAULT_OUTLIER_THRESHOLD) {
            # Find closest heel strike
            closest_step <- find_closest_heel_strike(clicked_time, participant, trial, trial_data, threshold)

            if (is.null(closest_step)) {
                return(list(success = FALSE, message = "No heel strike found within threshold"))
            }

            # Remove from both lists first (clean slate)
            private$.heelstrike_outliers <- private$.heelstrike_outliers %>%
                dplyr::filter(!(participant == !!participant &
                    trialNum == !!trial &
                    abs(time - closest_step$time) <= threshold))

            private$.step_outliers <- private$.step_outliers %>%
                dplyr::filter(!(participant == !!participant &
                    trialNum == !!trial &
                    abs(time - closest_step$time) <= threshold))

            # Add to appropriate list based on mode
            message <- switch(mode,
                "step" = {
                    new_outlier <- data.frame(
                        participant = participant,
                        trialNum = trial,
                        time = closest_step$time,
                        stringsAsFactors = FALSE
                    )
                    private$.step_outliers <- rbind(private$.step_outliers, new_outlier)
                    "Marked as step outlier"
                },
                "heel" = {
                    new_outlier <- data.frame(
                        participant = participant,
                        trialNum = trial,
                        time = closest_step$time,
                        stringsAsFactors = FALSE
                    )
                    private$.heelstrike_outliers <- rbind(private$.heelstrike_outliers, new_outlier)
                    "Marked for heel strike removal"
                },
                "remove" = "Removed from all outlier lists"
            )

            private$update_change_tracking()

            return(list(success = TRUE, message = message))
        },

        # Change tracking
        has_changes = function() private$.change_tracking$changes_made,
        get_current_filenames = function() {
            list(
                hs = private$.change_tracking$last_hs_filename,
                step = private$.change_tracking$last_step_filename
            )
        }
    ),
    private = list(
        .heelstrike_outliers = NULL,
        .step_outliers = NULL,
        .change_tracking = NULL,
        load_single_file = function(file_path, type) {
            tryCatch(
                {
                    df <- read.csv(file_path, stringsAsFactors = FALSE)
                    validation <- validate_outlier_data(df)

                    if (validation$valid) {
                        return(list(success = TRUE, data = validation$data))
                    } else {
                        return(list(success = FALSE, error = paste(type, "file error:", validation$error)))
                    }
                },
                error = function(e) {
                    return(list(success = FALSE, error = paste(type, "file error:", e$message)))
                }
            )
        },
        update_change_tracking = function() {
            current_hs_hash <- calculate_data_hash(private$.heelstrike_outliers)
            current_step_hash <- calculate_data_hash(private$.step_outliers)

            if (is.null(private$.change_tracking$last_hs_hash)) {
                private$.change_tracking$last_hs_hash <- current_hs_hash
                private$.change_tracking$last_step_hash <- current_step_hash
                private$.change_tracking$changes_made <- FALSE
            } else {
                private$.change_tracking$changes_made <-
                    (current_hs_hash != private$.change_tracking$last_hs_hash) ||
                        (current_step_hash != private$.change_tracking$last_step_hash)
            }
        },
        reset_change_tracking = function() {
            private$.change_tracking$last_hs_hash <- calculate_data_hash(private$.heelstrike_outliers)
            private$.change_tracking$last_step_hash <- calculate_data_hash(private$.step_outliers)
            private$.change_tracking$changes_made <- FALSE
        },
        calculate_file_changes = function(data_folder, filenames) {
            # Load existing files if they exist
            existing_hs <- data.frame(participant = character(0), trialNum = numeric(0), time = numeric(0))
            existing_step <- data.frame(participant = character(0), trialNum = numeric(0), time = numeric(0))

            hs_path <- file.path(data_folder, filenames$hs)
            step_path <- file.path(data_folder, filenames$step)

            if (file.exists(hs_path)) {
                tryCatch(
                    {
                        existing_hs <- read.csv(hs_path, stringsAsFactors = FALSE)
                    },
                    error = function(e) NULL
                )
            }

            if (file.exists(step_path)) {
                tryCatch(
                    {
                        existing_step <- read.csv(step_path, stringsAsFactors = FALSE)
                    },
                    error = function(e) NULL
                )
            }

            return(list(
                hs = calculate_table_changes(existing_hs, private$.heelstrike_outliers),
                step = calculate_table_changes(existing_step, private$.step_outliers)
            ))
        }
    )
)

# Factory function to create data manager
create_outlier_data_manager <- function() {
    if (!requireNamespace("R6", quietly = TRUE)) {
        stop("R6 package is required for OutlierDataManager")
    }
    OutlierDataManager$new()
}
