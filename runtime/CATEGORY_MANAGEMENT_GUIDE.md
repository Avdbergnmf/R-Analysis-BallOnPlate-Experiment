# Category Management System Guide

## Overview

The category management system provides a centralized, organized approach to managing different types of categories used throughout your analysis pipeline. This system addresses the confusion you've been experiencing with categories by clearly separating them by purpose and providing consistent interfaces.

## Category Types

### 1. Core Identifiers (`get_core_identifiers()`)
- **Purpose**: Used for joining datasets
- **Categories**: `participant`, `trialNum`
- **Usage**: Always include these when joining or filtering data

### 2. Experimental Conditions (`get_experimental_conditions()`)
- **Purpose**: Represent experimental manipulations for statistical analysis
- **Categories**: `condition`, `phase`
- **Usage**: Use for grouping in statistical analyses and plotting

### 3. Demographic Categories (`get_demographic_categories()`)
- **Purpose**: Participant characteristics for analysis grouping
- **Categories**: `gender`, `motion`
- **Usage**: Use for statistical analysis and subgroup comparisons

### 4. Descriptive Columns (`get_descriptive_columns()`)
- **Purpose**: Should be preserved in summaries but not used for numerical summarization
- **Categories**: `visualizations`, `perturbations`, `task`, `treadmillSpeed`, `age`, `weight`, `education`, `vr_experience`, `height_scale`
- **Usage**: These are preserved in summary tables but excluded from mean/SD calculations

### 5. Analysis-Specific Categories (`get_analysis_categories()`)
- **Purpose**: Used for specific analyses but not core grouping
- **Categories**: `foot`, `slice_index`, `suspect`
- **Usage**: Include only when needed for specific analyses

## Composite Category Sets

### For Data Analysis (`get_analysis_categories_all()`)
Combines core identifiers, experimental conditions, and demographics:
```r
c("participant", "trialNum", "condition", "phase", "gender", "motion")
```

### For Summary Tables (`get_summary_table_categories()`)
Includes all analysis categories plus descriptive columns:
```r
c("participant", "trialNum", "condition", "phase", "gender", "motion", 
  "visualizations", "perturbations", "task", "treadmillSpeed", 
  "age", "weight", "education", "vr_experience", "height_scale")
```

### For UI Inputs (`get_ui_input_categories()`)
Includes all categories plus analysis-specific ones and "None":
```r
c("participant", "trialNum", "condition", "phase", "gender", "motion", 
  "visualizations", "perturbations", "task", "treadmillSpeed", 
  "age", "weight", "education", "vr_experience", "height_scale",
  "foot", "slice_index", "suspect", "None")
```

## Usage Examples

### Basic Category Usage

```r
# Get categories for joining datasets
join_cats <- get_join_categories()  # c("participant", "trialNum")

# Get categories for statistical analysis
stat_cats <- get_statistical_categories()  # c("condition", "phase", "gender", "motion")

# Get categories for plotting
plot_cats <- get_plotting_categories()  # All categories + "None"
```

### Analysis-Specific Categories

```r
# For gait analysis (with foot)
gait_cats <- get_gait_analysis_categories(include_foot = TRUE)

# For questionnaire analysis
q_cats <- get_questionnaire_analysis_categories()

# For time-sliced analysis
sliced_cats <- get_sliced_analysis_categories(include_slice = TRUE)
```

### Validation and Error Handling

```r
# Check if dataset has required categories
if (!has_required_categories(data, "gait")) {
  missing <- get_missing_categories(data, "gait")
  warning("Missing categories: ", paste(missing, collapse = ", "))
}

# Get only categories that exist in your dataset
existing_cats <- get_existing_categories(data, get_analysis_categories_all())
```

### Working with Data

```r
# Get columns that should be summarized (exclude categories)
summarizable_cols <- get_summarizable_columns(data, get_analysis_categories_all())

# Filter data by category values
filtered_data <- filter_by_category(data, "condition", c("baseline", "perturbation"))

# Get unique values for a category
phase_values <- get_category_values(data, "phase")
```

## Migration from Old System

### Old Way (Confusing)
```r
# Hard-coded categories scattered throughout code
categories <- c("participant", "trialNum", "condition", "gender", "motion", "phase")
categoriesExtra <- c(categories, columns_to_not_summarize, "suspect")
categoriesExtraInputs <- append(categoriesExtra, c("foot", "slice_index", "None"))

# Manual category management
dataTypes <- setdiff(getTypes(data), categories)
```

### New Way (Clear and Organized)
```r
# Use purpose-specific category functions
analysis_cats <- get_analysis_categories_all()
ui_cats <- get_ui_input_categories()

# Automatic category management
summarizable_cols <- get_summarizable_columns(data, analysis_cats)
```

## Benefits of the New System

1. **Clear Separation**: Categories are organized by purpose (joining, analysis, plotting, etc.)
2. **Consistent Interface**: All category access goes through well-defined functions
3. **Better Error Handling**: Validation functions help catch missing categories early
4. **Easier Maintenance**: Adding new categories only requires updating the appropriate function
5. **Documentation**: Each category has a clear purpose and usage description
6. **Backward Compatibility**: Old code continues to work while you migrate gradually

## Adding New Categories

When you need to add a new category:

1. **Determine the category type**:
   - Core identifier? → Add to `get_core_identifiers()`
   - Experimental condition? → Add to `get_experimental_conditions()`
   - Demographic? → Add to `get_demographic_categories()`
   - Descriptive? → Add to `get_descriptive_columns()`
   - Analysis-specific? → Add to `get_analysis_categories()`

2. **Update the category addition function** in `add_category_columns()`

3. **Update documentation** in `get_category_descriptions()`

## Example: Adding the "phase" Category

The "phase" category was recently added and caused confusion. With the new system:

```r
# It's clearly defined as an experimental condition
get_experimental_conditions()  # Returns c("condition", "phase")

# It's automatically included in analysis categories
get_analysis_categories_all()  # Includes "phase"

# It's validated when needed
has_required_categories(data, "gait")  # Checks for "phase" and other required categories
```

This makes it clear that "phase" is used for experimental analysis, not for joining datasets, and ensures it's handled consistently throughout your codebase.

## Getting Help

Use these functions to understand your categories:

```r
# Print a summary of all categories
print_category_summary()

# Print categories for a specific analysis
print_category_summary("gait")

# Get detailed descriptions
descriptions <- get_category_descriptions()
View(descriptions)
```