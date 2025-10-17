#' Power Spectral Density (PSD) Analysis API
#'
#' Public interface for power spectral density analysis functions.
#' This is the main entry point for all PSD operations.

# =============================================================================
# CORE COMPUTATION API
# =============================================================================

#' Compute power spectral density for a single time series
#' @param x Numeric vector of time series data
#' @param fs Sampling frequency in Hz
#' @param max_freq Maximum frequency to return
#' @param normalize Whether to normalize power to sum to 1
#' @return Data frame with frequency and power columns
compute_psd <- function(x, fs = 30, max_freq = 2.5, normalize = TRUE) {
    compute_single_psd(x, fs, max_freq, normalize)
}

#' Compute power spectral density with custom preprocessing
#' @param x Numeric vector of time series data
#' @param fs Sampling frequency in Hz
#' @param max_freq Maximum frequency to return
#' @param normalize Whether to normalize power to sum to 1
#' @param filter_cutoff Low-pass filter cutoff frequency (Hz)
#' @param downsample_target Target sampling frequency for downsampling
#' @return Data frame with frequency and power columns
compute_psd_advanced <- function(x, fs = 30, max_freq = 2.5, normalize = TRUE, 
                                 filter_cutoff = 6, downsample_target = 30) {
    if (length(x) < 50 || !is.numeric(x)) {
        return(NULL)
    }
    
    # Remove NA values
    x <- x[is.finite(x)]
    if (length(x) < 50) {
        return(NULL)
    }
    
    tryCatch({
        # Apply low-pass filter
        x_filtered <- apply_lowpass_filter(x, fs, filter_cutoff)
        
        # Downsample if needed
        downsampled <- downsample_signal(x_filtered, fs, downsample_target)
        x_filtered <- downsampled$signal
        fs <- downsampled$fs
        
        # Remove linear trend
        x_detrended <- x_filtered - mean(x_filtered)
        
        # Apply Hamming window
        x_windowed <- apply_hamming_window(x_detrended)
        
        # Compute FFT and extract power spectrum
        psd_result <- compute_fft_psd(x_windowed, fs, max_freq)
        
        # Create data frame
        psd_df <- data.frame(
            frequency = psd_result$frequency,
            power = psd_result$power
        )
        
        # Normalize if requested
        if (normalize) {
            psd_df <- normalize_psd(psd_df, method = "sum")
        }
        
        psd_df
    }, error = function(e) {
        NULL
    })
}

# =============================================================================
# SMOOTHING API
# =============================================================================

#' Smooth power spectrum using loess
#' @param psd_df Data frame with frequency and power columns
#' @param bandwidth Smoothing bandwidth in Hz
#' @return Data frame with smoothed power
smooth_psd_loess <- function(psd_df, bandwidth = 0.1) {
    smooth_psd(psd_df, bandwidth)
}

#' Smooth power spectrum using moving average
#' @param psd_df Data frame with frequency and power columns
#' @param window_size Size of moving average window
#' @return Data frame with smoothed power
smooth_psd_moving_avg <- function(psd_df, window_size = 5) {
    smooth_psd_moving_average(psd_df, window_size)
}

#' Smooth power spectrum using Gaussian kernel
#' @param psd_df Data frame with frequency and power columns
#' @param sigma Standard deviation of Gaussian kernel
#' @return Data frame with smoothed power
smooth_psd_gaussian <- function(psd_df, sigma = 0.05) {
    smooth_psd_gaussian(psd_df, sigma)
}

#' Normalize power spectrum
#' @param psd_df Data frame with frequency and power columns
#' @param method Normalization method: "sum", "max", or "area"
#' @return Data frame with normalized power
normalize_psd <- function(psd_df, method = "sum") {
    normalize_psd(psd_df, method)
}

# =============================================================================
# DATA PROCESSING API
# =============================================================================

#' Compute power spectrum data for all groups
#' @param cached_tracker_data Cached tracker data frame
#' @param var_name Variable name to analyze (e.g., "pos_x", "vel_y")
#' @param group_by Grouping variable
#' @param split_by Split variable (NULL for no split)
#' @param max_freq Maximum frequency
#' @param smooth_bandwidth Smoothing bandwidth
#' @param normalize Normalize power
#' @return Data frame with power spectrum for each group
compute_group_psd <- function(cached_tracker_data, var_name, group_by, split_by = NULL,
                              max_freq = 2.5, smooth_bandwidth = 0.1, 
                              normalize = TRUE) {
    compute_power_spectrum_data(cached_tracker_data, var_name, group_by, split_by,
                                max_freq, smooth_bandwidth, normalize)
}

#' Extract and process trial data for PSD analysis
#' @param cached_tracker_data Cached tracker data frame
#' @param participant Participant ID
#' @param trial_num Trial number
#' @param var_name Variable name to extract
#' @return List with values, times, and sampling frequency
extract_trial_for_psd <- function(cached_tracker_data, participant, trial_num, var_name) {
    extract_trial_data(cached_tracker_data, participant, trial_num, var_name)
}

#' Aggregate power spectra across groups
#' @param psd_list List of power spectrum data frames
#' @param group_by Grouping variable
#' @param split_by Split variable
#' @return Aggregated power spectrum data frame
aggregate_psd_data <- function(psd_list, group_by = NULL, split_by = NULL) {
    aggregate_power_spectra(psd_list, group_by, split_by)
}

# =============================================================================
# PLOTTING API
# =============================================================================

#' Create power spectrum plot
#' @param ps_data Power spectrum data frame
#' @param plot_type Type of plot: "line", "area", "log", "ci"
#' @param stack Whether to stack the spectra (for area plots)
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_psd <- function(ps_data, plot_type = "line", stack = TRUE, split_by = NULL, base_size = 10) {
    switch(plot_type,
        "line" = plot_power_spectrum(ps_data, stack = FALSE, split_by = split_by, base_size = base_size),
        "area" = plot_power_spectrum(ps_data, stack = stack, split_by = split_by, base_size = base_size),
        "log" = plot_power_spectrum_log(ps_data, split_by = split_by, base_size = base_size),
        "ci" = plot_power_spectrum_with_ci(ps_data, split_by = split_by, base_size = base_size),
        plot_power_spectrum(ps_data, stack = FALSE, split_by = split_by, base_size = base_size)  # Default
    )
}

#' Create power spectrum line plot
#' @param ps_data Power spectrum data frame
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_psd_lines <- function(ps_data, split_by = NULL, base_size = 10) {
    plot_power_spectrum(ps_data, stack = FALSE, split_by = split_by, base_size = base_size)
}

#' Create power spectrum area plot
#' @param ps_data Power spectrum data frame
#' @param stack Whether to stack the spectra
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_psd_areas <- function(ps_data, stack = TRUE, split_by = NULL, base_size = 10) {
    plot_power_spectrum(ps_data, stack = stack, split_by = split_by, base_size = base_size)
}

#' Create power spectrum log plot
#' @param ps_data Power spectrum data frame
#' @param log_x Whether to use log scale for x-axis
#' @param log_y Whether to use log scale for y-axis
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_psd_log <- function(ps_data, log_x = FALSE, log_y = TRUE, split_by = NULL, base_size = 10) {
    plot_power_spectrum_log(ps_data, log_x, log_y, split_by, base_size)
}

#' Create power spectrum plot with confidence intervals
#' @param ps_data Power spectrum data frame with multiple trials
#' @param confidence_level Confidence level for intervals (default: 0.95)
#' @param split_by Split variable name
#' @param base_size Base font size
#' @return ggplot object
plot_psd_with_ci <- function(ps_data, confidence_level = 0.95, split_by = NULL, base_size = 10) {
    plot_power_spectrum_with_ci(ps_data, confidence_level, split_by, base_size)
}

# =============================================================================
# CONVENIENCE API
# =============================================================================

#' Complete PSD analysis pipeline
#' @param cached_tracker_data Cached tracker data frame
#' @param var_name Variable name to analyze
#' @param group_by Grouping variable
#' @param split_by Split variable (NULL for no split)
#' @param max_freq Maximum frequency
#' @param smooth_bandwidth Smoothing bandwidth
#' @param normalize Normalize power
#' @param plot_type Type of plot to create
#' @param base_size Base font size for plot
#' @return List with PSD data and plot
run_psd_analysis <- function(cached_tracker_data, var_name, group_by, split_by = NULL,
                             max_freq = 2.5, smooth_bandwidth = 0.1, normalize = TRUE,
                             plot_type = "line", base_size = 10) {
    
    # Compute PSD data
    psd_data <- compute_group_psd(cached_tracker_data, var_name, group_by, split_by,
                                  max_freq, smooth_bandwidth, normalize)
    
    # Create plot
    psd_plot <- NULL
    if (!is.null(psd_data)) {
        psd_plot <- plot_psd(psd_data, plot_type, split_by = split_by, base_size = base_size)
    }
    
    list(
        data = psd_data,
        plot = psd_plot
    )
}

#' Get PSD summary statistics
#' @param psd_data Power spectrum data frame
#' @param group_by Grouping variable
#' @return Data frame with summary statistics
get_psd_summary <- function(psd_data, group_by = NULL) {
    if (is.null(psd_data) || nrow(psd_data) == 0) {
        return(NULL)
    }
    
    # Calculate summary statistics
    summary_vars <- c("frequency")
    if (!is.null(group_by) && group_by %in% names(psd_data)) {
        summary_vars <- c(summary_vars, group_by)
    }
    
    psd_data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(summary_vars))) %>%
        dplyr::summarize(
            mean_power = mean(power, na.rm = TRUE),
            sd_power = sd(power, na.rm = TRUE),
            max_power = max(power, na.rm = TRUE),
            total_power = sum(power, na.rm = TRUE),
            .groups = "drop"
        )
}
