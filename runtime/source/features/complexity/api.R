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
#' Public API Functions
#' - `get_all_complexity_metrics()`: main entry point for batch calculations
#' - `calculate_complexity_single()`: helper for single participant/trial combinations
#' - `calc_complexity_for_loop()`: adapter for the looping framework
#'
#' Internal helpers from the other module files remain private and are no longer
#' exported to prevent cross-package dependencies.
