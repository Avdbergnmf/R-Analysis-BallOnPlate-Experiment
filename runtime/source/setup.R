# setup.R - Centralized package loading with optimization packages for improved performance
knitr::opts_chunk$set(echo = FALSE)

# Core data manipulation and performance optimization
library(data.table) # Fast CSV reading (fread) and efficient joins for outlier matching
library(dplyr) # Data manipulation verbs
library(plyr) # Data manipulation functions
library(purrr) # Functional programming tools
library(tidyr) # Data reshaping and tidying

# Statistical analysis and modeling
library(lme4) # Linear mixed-effects models
library(lmerTest) # P-values for mixed-effects models
library(emmeans) # Estimated marginal means
library(MuMIn) # Model selection and averaging
library(rstatix) # Statistical tests
library(car) # Companion to Applied Regression
library(pwr) # Power analysis

# Data visualization
library(ggplot2) # Grammar of graphics plotting
library(ggpubr) # Publication-ready plots
library(RColorBrewer) # Color palettes
library(ggpattern) # Pattern fills for black/white printing
library(plotrix) # Additional plotting functions
library(viridis) # Perceptually uniform color scales
library(ggstatsplot) # Statistical plots with annotations
library(ggside) # Side panels for ggplot
library(ggExtra) # Marginal plots and distributions
library(plotly) # Interactive web-based plots

# Signal processing and filtering (optimization: vectorized operations)
library(signal) # Butterworth filtering and signal processing
library(pracma) # Hampel filter for outlier detection (more efficient than custom loops)
library(zoo) # Rolling statistics and time series

# File I/O and data import
library(readxl) # Excel file reading
library(jsonlite) # JSON file parsing
library(svglite) # SVG graphics device

# Parallel processing (optimization: efficient multi-core computation)
library(foreach) # Parallel for loops
library(doParallel) # Parallel backend for foreach

# Interactive tables and UI components
library(DT) # Interactive data tables
library(shinyBS) # Bootstrap components for Shiny

# Data reshaping and manipulation
library(reshape2) # Data melting and casting
library(Rmisc) # Miscellaneous functions

# Statistical testing and model diagnostics
library(lmtest) # Linear model testing

# Outlier detection
library(dbscan) # Density-based clustering for outlier detection

# Development and utilities
library(devtools) # Development tools
library(markdown) # Markdown rendering for data dictionary
