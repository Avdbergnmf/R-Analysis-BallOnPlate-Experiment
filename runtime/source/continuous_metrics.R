# Continuous Position Metrics Helpers
# ------------------------------------
# These utilities provide a central place to work with continuous position signals
# from various trackers (UDP, hip, etc.). The framework is configuration-driven,
# making it easy to add new continuous signals in the future.
#
# HOW TO ADD A NEW SIGNAL:
# ------------------------
# 1. Add a new entry to CONTINUOUS_SIGNALS below with:
#    - tracker: the tracker type (e.g., "hip", "udp")
#    - column: the column name to extract (e.g., "pos_y")
#    - prefix: the prefix for output columns (e.g., "hip_posy")
#    - description: human-readable description for warnings
# 2. That's it! The signal will automatically be included in combined metrics.
#
# Example:
#   hip_posy = list(
#       tracker = "hip",
#       column = "pos_y",
#       prefix = "hip_posy",
#       description = "Hip Tracker Y Position"
#   )

# =============================================================================
# SIGNAL CONFIGURATION
# =============================================================================

#' Define available continuous signals with their tracker and column info
#' To add a new signal, simply add an entry to this list
CONTINUOUS_SIGNALS <- list(
    udp_pelvis = list(
        tracker = "udp",
        column = "PelvisPos",
        prefix = "udp",
        description = "UDP Pelvis Position"
    ),
    hip_posx = list(
        tracker = "hip",
        column = "pos_x",
        prefix = "hip",
        description = "Hip Tracker X Position"
    )
    # Add more signals here as needed:
    # hip_posy = list(tracker = "hip", column = "pos_y", prefix = "hip_posy", description = "Hip Tracker Y Position"),
    # hip_posz = list(tracker = "hip", column = "pos_z", prefix = "hip_posz", description = "Hip Tracker Z Position")
)

# =============================================================================
# CORE SIGNAL EXTRACTION AND ANALYSIS
# =============================================================================

#' Generic signal extraction from any tracker
#'
#' @param participant Participant identifier
#' @param trial Trial identifier (numeric or character)
#' @param tracker_type Type of tracker (e.g., "udp", "hip")
#' @param column_name Name of the column to extract (e.g., "PelvisPos", "pos_x")
#' @param description Human-readable description for warnings
#' @return List with numeric vector `values`, numeric vector `time` (if available),
#'         resolved sampling rate `fs`, and counts of total / valid samples
get_continuous_signal <- function(participant, trial, tracker_type, column_name, description = NULL) {
    if (is.null(description)) {
        description <- sprintf("%s column from %s tracker", column_name, tracker_type)
    }

    tracker_data <- get_t_data(participant, tracker_type, trial)

    if (is.null(tracker_data) || nrow(tracker_data) == 0) {
        return(list(
            values = numeric(0),
            time = numeric(0),
            fs = NA_real_,
            n_total = 0L,
            n_valid = 0L
        ))
    }

    if (!column_name %in% colnames(tracker_data)) {
        warning(sprintf(
            "%s missing for participant %s trial %s (%s dataset)",
            column_name, as.character(participant), as.character(trial), tracker_type
        ))
        return(list(
            values = numeric(0),
            time = numeric(0),
            fs = NA_real_,
            n_total = nrow(tracker_data),
            n_valid = 0L
        ))
    }

    # Ensure data is ordered by time (if available)
    time_col <- NULL
    if ("time" %in% colnames(tracker_data)) {
        tracker_data <- tracker_data[order(tracker_data$time), , drop = FALSE]
        time_col <- tracker_data$time
    }

    values_raw <- as.numeric(tracker_data[[column_name]])
    n_total <- length(values_raw)

    if (!is.null(time_col)) {
        time_col <- as.numeric(time_col)
        if (length(time_col) != n_total) {
            time_col <- NULL
        }
    }

    valid_mask <- is.finite(values_raw)
    values <- values_raw[valid_mask]
    time_vec <- if (is.null(time_col)) numeric(0) else time_col[valid_mask]

    # Estimate sampling rate from time differences if possible
    fs <- NA_real_
    if (length(time_vec) > 1) {
        dt <- stats::median(diff(time_vec), na.rm = TRUE)
        if (is.finite(dt) && dt > 0) {
            fs <- 1 / dt
        }
    }

    list(
        values = values,
        time = time_vec,
        fs = fs,
        n_total = n_total,
        n_valid = length(values)
    )
}

#' Calculate summary statistics for any continuous signal
#'
#' @param values Numeric vector of signal data
#' @return List with mean, sd, cv, and n_valid
summarise_continuous_values <- function(values) {
    valid_values <- values[is.finite(values)]
    n_valid <- length(valid_values)

    if (n_valid == 0) {
        return(list(mean = NA_real_, sd = NA_real_, cv = NA_real_, n_valid = 0L))
    }

    mean_val <- mean(valid_values, na.rm = TRUE)
    sd_val <- stats::sd(valid_values, na.rm = TRUE)

    if (!is.finite(mean_val) || abs(mean_val) < .Machine$double.eps) {
        cv_val <- NA_real_
    } else {
        cv_val <- sd_val / abs(mean_val)
    }

    list(mean = mean_val, sd = sd_val, cv = cv_val, n_valid = n_valid)
}

#' Generic function to calculate metrics for a single signal configuration
#'
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param signal_config Signal configuration list (from CONTINUOUS_SIGNALS)
#' @return Data frame with summary statistics
calculate_signal_metrics <- function(participant, trial, signal_config) {
    signal <- get_continuous_signal(
        participant,
        trial,
        signal_config$tracker,
        signal_config$column,
        signal_config$description
    )

    stats <- summarise_continuous_values(signal$values)

    # Create column names using the prefix
    prefix <- signal_config$prefix
    result <- data.frame(
        mean = stats$mean,
        sd = stats$sd,
        cv = stats$cv,
        n = stats$n_valid,
        stringsAsFactors = FALSE
    )

    # Rename columns with prefix
    # names(result) <- paste0(prefix, "_", names(result))

    return(result)
}

# =============================================================================
# COMBINED METRICS
# =============================================================================

#' Compute metrics for all configured continuous signals
#'
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @param signals List of signal configurations to calculate (defaults to all in CONTINUOUS_SIGNALS)
#' @return Data frame with all signal summary statistics
calculate_combined_continuous_metrics <- function(participant, trial, signals = CONTINUOUS_SIGNALS) {
    # Calculate metrics for each configured signal
    metrics_list <- lapply(signals, function(signal_config) {
        calculate_signal_metrics(participant, trial, signal_config)
    })

    # Combine all metrics into one data frame
    combined <- do.call(cbind, metrics_list)

    return(combined)
}

#' Build combined summary table for all participants/trials
#'
#' @param loop_function Looping helper provided by load_or_calculate
#' @param signals List of signal configurations to calculate (defaults to all in CONTINUOUS_SIGNALS)
#' @return Data frame with all signal metrics across all valid combinations
get_all_combined_continuous_metrics <- function(loop_function, signals = CONTINUOUS_SIGNALS) {
    ensure_global_data_initialized()

    calc_fun <- function(participant, trial) {
        calculate_combined_continuous_metrics(participant, trial, signals)
    }

    # Extract unique tracker types that need to be verified
    trackers_to_verify <- unique(sapply(signals, function(s) s$tracker))

    loop_function(calc_fun, datasets_to_verify = trackers_to_verify)
}
