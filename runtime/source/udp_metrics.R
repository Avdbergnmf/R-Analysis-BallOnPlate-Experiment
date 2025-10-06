# UDP Pelvis Position Metrics Helpers
# -----------------------------------
# These utilities provide a central place to work with the PelvisPos column
# recorded by the UDP tracker. They expose both a summary-table generator and a
# reusable signal-extraction helper that other modules (e.g., complexity
# calculations) can leverage.

#' Extract the PelvisPos signal from UDP tracker data
#'
#' @param participant Participant identifier
#' @param trial Trial identifier (numeric or character)
#' @return List with numeric vector `values`, numeric vector `time` (if available),
#'         resolved sampling rate `fs`, and counts of total / valid samples
get_udp_pelvis_signal <- function(participant, trial) {
    udp_data <- get_t_data(participant, "udp", trial)

    if (is.null(udp_data) || nrow(udp_data) == 0) {
        return(list(
            values = numeric(0),
            time = numeric(0),
            fs = NA_real_,
            n_total = 0L,
            n_valid = 0L
        ))
    }

    if (!"PelvisPos" %in% colnames(udp_data)) {
        warning(sprintf(
            "PelvisPos column missing for participant %s trial %s (UDP dataset)",
            as.character(participant), as.character(trial)
        ))
        return(list(
            values = numeric(0),
            time = numeric(0),
            fs = NA_real_,
            n_total = nrow(udp_data),
            n_valid = 0L
        ))
    }

    # Ensure data is ordered by time (if available)
    time_col <- NULL
    if ("time" %in% colnames(udp_data)) {
        udp_data <- udp_data[order(udp_data$time), , drop = FALSE]
        time_col <- udp_data$time
    }

    values_raw <- as.numeric(udp_data$PelvisPos)
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

#' Calculate summary statistics for PelvisPos
#'
#' @param values Numeric vector of PelvisPos data
#' @return List with mean, sd, cv, and n_valid
summarise_pelvispos_values <- function(values) {
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

#' Compute PelvisPos summary metrics for a participant/trial
#'
#' @param participant Participant identifier
#' @param trial Trial identifier
#' @return Data frame with PelvisPos summary statistics
calculate_udp_pelvis_metrics <- function(participant, trial) {
    signal <- get_udp_pelvis_signal(participant, trial)
    stats <- summarise_pelvispos_values(signal$values)

    data.frame(
        udpPelvisPos_mean = stats$mean,
        udpPelvisPos_sd = stats$sd,
        udpPelvisPos_cv = stats$cv,
        udpPelvisPos_n = stats$n_valid,
        stringsAsFactors = FALSE
    )
}

#' Build PelvisPos summary table for all participants/trials
#'
#' @param loop_function Looping helper provided by load_or_calculate
#' @return Data frame with PelvisPos metrics across all valid combinations
get_all_udp_pelvis_metrics <- function(loop_function) {
    ensure_global_data_initialized()

    calc_fun <- function(participant, trial) {
        calculate_udp_pelvis_metrics(participant, trial)
    }

    loop_function(calc_fun, datasets_to_verify = c("udp"))
}
