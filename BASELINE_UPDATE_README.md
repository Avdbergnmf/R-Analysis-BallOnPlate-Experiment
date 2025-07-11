# Baseline Performance Update

## Overview

This update modifies the existing code to explicitly include baseline performance (Trial 5) as an additional categorical variable named 'Baseline' in summary tables and visualizations.

## Changes Made

### 1. New Functions Added (`runtime/source/summarize_gaitparams.R`)

#### `add_baseline_category(data, categories)`
- Creates a new 'Baseline' categorical column that indicates if data corresponds to baseline performance (Trial 5) or subsequent phases
- Only adds this category when Trial 5 baseline data is available in the summary data frame
- Handles scenarios where baseline data is not present intelligently to avoid errors
- Trial 5 is specifically identified as the baseline_task (pre-training reference)

#### `rename_baseline_to_control(data)`
- Renames the existing 'baseline' condition to 'Control' throughout summary tables
- Clearly distinguishes between the new 'Baseline' category (Trial 5 performance) and the 'Control' condition

### 2. Updated Summary Table Functions

#### `get_full_mu()` and `get_full_mu_sliced()`
- Now automatically call `add_baseline_category()` and `rename_baseline_to_control()` after generating the main summary table
- Ensures baseline categorization is seamlessly integrated with current workflow

### 3. Updated Visualization Functions (`runtime/source/plotting.R`)

#### `get_pretty_condition_labels()`
- Changed condition mapping from `"baseline" = "B"` to `"Control" = "C"`
- Updated boxplot and other visualizations to use 'C' instead of 'B' for Control condition

#### `plot_steps_with_overlay()`
- Updated condition check from `condition == "baseline"` to `condition == "Control"`
- Maintains consistent color coding (yellow for Control condition)

### 4. Updated Data Loading (`runtime/source/data_loading.R`)

#### `condition_number()`
- Changed return value from `"baseline"` to `"Control"`
- Ensures consistent naming throughout the data pipeline

### 5. Updated User Interface (`runtime/pages/sidebar_dynamicDataFiltering.Rmd`)

#### Filter Options
- Updated condition filter choices from `"baseline"` to `"Control"`
- Maintains all existing functionality while using the new naming convention

## Key Features

### Robustness and Flexibility
- **Intelligent Handling**: The baseline category is only added when Trial 5 data is comprehensive enough
- **Error Prevention**: Gracefully handles scenarios where baseline data is not present
- **Backward Compatibility**: Existing workflows continue to function without disruption

### Clear Differentiation
- **Baseline Category**: Indicates Trial 5 performance (pre-training reference)
- **Control Condition**: Indicates the experimental condition (no perturbations/visualizations)
- **Training/Retention/Transfer**: All other trials are grouped under this category

### Documentation
- All functions include comprehensive documentation explaining their purpose
- Comments clearly indicate that baseline is derived specifically from Trial 5 data
- Code is self-documenting with clear variable names and logic

## Result

The resulting summary tables now clearly differentiate:
1. **Baseline data points** (Trial 5) - serving as pre-training performance reference
2. **Training, retention, and transfer phases** - all other trials
3. **Control condition** - clearly labeled instead of the ambiguous 'baseline' condition

This facilitates easier downstream analysis by providing explicit baseline performance data alongside the existing experimental conditions.