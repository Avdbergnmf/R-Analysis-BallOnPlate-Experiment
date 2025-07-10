# Page 16 Components

This directory contains the refactored components extracted from the original vibe coding mess in `page16_manualOutlierFiltering.Rmd`.

## Files

### Core Components
- **`page16_utils.R`** - Shared utilities and constants
  - Data validation functions
  - File handling utilities  
  - Outlier matching logic
  - Hash calculation for change tracking

- **`page16_data_manager.R`** - Data management with R6 class
  - Encapsulated outlier data state
  - File I/O operations
  - Change tracking
  - Clean interface for data manipulation

### UI Components
- **`page16_styles.css`** - All CSS styles
  - Drag & drop zone styling
  - Navigation and layout components
  - File info boxes
  - Responsive design

- **`page16_interactions.js`** - JavaScript functionality
  - Keyboard shortcuts (TAB to toggle modes)
  - Drag & drop file handling
  - Navigation events
  - Error handling

- **`page16_components.html`** - Clean HTML structure
  - External CSS and JS references
  - Semantic markup
  - No inline styles or scripts

## Usage

The main page (`page16_manualOutlierFiltering_clean.Rmd`) sources these components:

```r
# Source utilities
source("page16_components/page16_utils.R")
source("page16_components/page16_data_manager.R")

# Include HTML components
includeHTML("page16_components/page16_components.html")
```