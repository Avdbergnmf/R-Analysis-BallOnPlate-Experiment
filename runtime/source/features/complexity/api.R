#' Complexity Feature Module API
#'
#' Public interface for the complexity feature module
#' Provides access to all complexity calculation functions
#'
#' This module provides comprehensive complexity analysis including:
#' - Sample Entropy (SampEn)
#' - Multiscale Entropy (MSE) 
#' - Detrended Fluctuation Analysis (DFA)
#' - Lyapunov Exponent
#' - Power Spectral Density Analysis
#' - Foot Placement Control Metrics
#' - Signal Processing Utilities
#'
#' All functions are automatically available when the complexity module is loaded.
#' No explicit exports are needed as the module loading system handles this.

# Note: All functions from the following files are automatically available:
# - core_metrics.R: sampen, mse, dfa_alpha, lyapunov, psd_analysis
# - foot_placement.R: get_na_results_fp_control, build_fp_rows_per_foot, fit_fp_pos_only, compute_fp_control_metrics_per_foot
# - signal_processing.R: sample_time_series, get_simulation_signal, get_tracker_signal, apply_signal_filtering
# - utilities.R: get_metric_name, calculate_summary_stats, apply_outlier_filtering, add_discrete_complexity, add_continuous_complexity, process_continuous_signal
# - calculation.R: calculate_complexity_single, calc_complexity_for_loop, get_all_complexity_metrics
