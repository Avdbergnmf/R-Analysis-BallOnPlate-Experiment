# Constants
DEFAULT_OUTLIER_THRESHOLD <- 0.03
DEFAULT_AUTOSAVE_INTERVAL <- 5
MAX_OUTLIERS_FOR_PLOT <- 500
OUTLIER_WARNING_THRESHOLD <- 100

# File handling utilities
create_outlier_filename <- function(base_name, timestamp = TRUE) {
    if (timestamp) {
        stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        ext <- tools::file_ext(base_name)
        base <- tools::file_path_sans_ext(base_name)
        paste0(base, "_", stamp, ".", ext)
    } else {
        base_name
    }
}

# Data validation utilities
validate_outlier_data <- function(df, required_cols = c("participant", "trialNum", "time")) {
    if (!is.data.frame(df) || nrow(df) == 0) {
        return(list(valid = FALSE, error = "Empty or invalid data frame"))
    }

    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
        return(list(valid = FALSE, error = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
    }

    # Check for valid data types
    tryCatch(
        {
            df$trialNum <- as.numeric(df$trialNum)
            df$participant <- as.character(df$participant)
            df$time <- as.numeric(df$time)

            # Remove invalid rows
            df <- df[!is.na(df$trialNum) & !is.na(df$time), ]

            return(list(valid = TRUE, data = df[, required_cols, drop = FALSE]))
        },
        error = function(e) {
            return(list(valid = FALSE, error = paste("Data conversion error:", e$message)))
        }
    )
}

# Outlier matching utilities
find_closest_heel_strike <- function(clicked_time, participant, trial, trial_data, threshold = DEFAULT_OUTLIER_THRESHOLD) {
    if (is.null(trial_data) || nrow(trial_data) == 0) {
        return(NULL)
    }

    # Find closest heel strike within threshold
    time_diffs <- abs(trial_data$time - clicked_time)
    closest_idx <- which.min(time_diffs)

    if (length(closest_idx) == 0 || time_diffs[closest_idx] > threshold) {
        return(NULL)
    }

    return(trial_data[closest_idx, ])
}

find_closest_heel_strikes <- function(outlier_data, gait_data, threshold = DEFAULT_OUTLIER_THRESHOLD) {
    if (nrow(outlier_data) == 0) {
        return(NULL)
    }

    # This function needs to be implemented based on the original logic
    # For now, return a placeholder
    return(outlier_data)
}

# Hover text creation
create_hover_text <- function(data, x_axis, y_axis, type = "normal", status = "") {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return(character(0))
    }

    # Safety checks for required columns
    required_cols <- c("time", "participant", "trialNum", "foot", "step", x_axis, y_axis)
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
        return(paste("Data incomplete for", type, "hover text"))
    }

    # Create title based on type
    icons <- list(
        normal = "Heel Strike",
        suspect = "🟠 SUSPECT Heel Strike",
        heel_outlier = "❌ HEEL STRIKE OUTLIER",
        step_outlier = "◐ STEP OUTLIER"
    )

    title <- icons[[type]] %||% "Heel Strike"

    tryCatch(
        {
            paste0(
                "<b>", title, "</b><br>",
                "Time: ", round(as.numeric(data$time), 3), " s<br>",
                x_axis, ": ", round(as.numeric(data[[x_axis]]), 3), "<br>",
                y_axis, ": ", round(as.numeric(data[[y_axis]]), 3), "<br>",
                "Participant: ", as.character(data$participant), "<br>",
                "Trial: ", as.character(data$trialNum), "<br>",
                "Foot: ", as.character(data$foot), "<br>",
                "Step: ", as.character(data$step),
                if (status != "") paste0("<br><i>Status: ", status, "</i>") else ""
            )
        },
        error = function(e) {
            paste(title, "- hover text unavailable")
        }
    )
}

# Hash calculation for change tracking
calculate_data_hash <- function(data) {
    if (is.null(data) || nrow(data) == 0) {
        return("empty")
    }
    paste(data$participant, data$trialNum, data$time, collapse = "_")
}

# Change calculation utilities
calculate_table_changes <- function(old_data, new_data) {
    old_key <- if (nrow(old_data) == 0) {
        character(0)
    } else {
        paste(old_data$participant, old_data$trialNum, round(old_data$time, 3), sep = "_")
    }

    new_key <- if (nrow(new_data) == 0) {
        character(0)
    } else {
        paste(new_data$participant, new_data$trialNum, round(new_data$time, 3), sep = "_")
    }

    added <- setdiff(new_key, old_key)
    removed <- setdiff(old_key, new_key)

    list(
        old_count = length(old_key),
        new_count = length(new_key),
        added = length(added),
        removed = length(removed),
        net_change = length(added) - length(removed)
    )
}

# Navigation utilities
get_participant_trials <- function(participant, all_data = NULL) {
    if (exists("allGaitParams") && !is.null(allGaitParams)) {
        participant_trials <- unique(allGaitParams$trialNum[allGaitParams$participant == participant])
        return(sort(as.numeric(participant_trials)))
    } else if (!is.null(all_data)) {
        participant_trials <- unique(all_data$trialNum[all_data$participant == participant])
        return(sort(as.numeric(participant_trials)))
    } else {
        return(sort(as.numeric(allTrials)))
    }
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
