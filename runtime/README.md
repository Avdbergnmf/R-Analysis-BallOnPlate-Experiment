# Ball-on-Plate Game Experiment Analysis Code
The code used for the analysis of the Ball-on-Plate Game Experiment investigating visual disturbances to avatar foot position and their effects on step-width variability in immersive VR treadmill walking.

> This repository already contains the calculated datasets (`./runtime/results/*.rds`). If you want to recalculate parameters, delete these files, and put participant data ([found on Zenodo](https://zenodo.org/record/14017075)) folders in `./runtime/data/` (in the same folder as the `index.Rmd`).
> 
> The analysis pipeline now includes sophisticated caching and parallel processing capabilities. Performance settings are controlled through the `config.yml` file. If running on a weaker CPU/low memory PC, you can modify the configuration to use sequential processing instead of parallel processing. Calculating the full parameter tables takes roughly 10 minutes when non-parallelized, and roughly 2-3 minutes when running parallel on a good PC.

## How to use
To run this code with your local R-Studio installation:
1. Within the R instance, navigate to `./runtime/`, and open the `index.Rmd` file.
2. Click `Run Document`. If it asks you to install any packages, click yes. You can find the list of packages used in `./runtime/source/setup.R`.
> Initial calculation of the datasets may take some time (~5 minutes). After the first run, the results should be saved to tables in `results/` and startup should be significantly faster.
3. Browse the tabs and interact with the data

> NOTE: Also see [data_dictionary.md](./data_dictionary.md) for a comprehensive overview of all variable names and their meaning. This includes information on gait parameters, complexity metrics, simulation data, questionnaire responses, and tracker files (data recorded within Unity using [UXF](https://github.com/immersivecognition/unity-experiment-framework)).

> NOTE 2: The system now includes sophisticated outlier detection and processing with multiple levels of quality control. Outliers are automatically processed during data loading, but manual corrections can be applied through the interface.

## Configuration

The analysis pipeline uses a configuration system defined in `config.yml` that allows you to customize performance and behavior settings:

### **Environment Profiles**
- **default**: Standard settings for general use
- **development**: Fast startup with minimal caching (good for development)
- **production**: Full caching with comprehensive logging (good for final analysis)

### **Key Configuration Options**
- **Performance**: Control parallel processing, core usage, and processing thresholds
- **Caching**: Manage cache behavior, compression, and force refresh options
- **Logging**: Configure logging levels and memory monitoring
- **Data Processing**: Set sampling frequencies and filtering parameters

To modify settings, edit the `config.yml` file and restart the analysis. The system will automatically detect your configuration and adjust behavior accordingly.

## Known Issues and Workarounds

### Complexity Calculation Bug

There is a known issue where running complexity calculations immediately after task data calculations can cause errors. The system automatically detects this condition and skips the complexity calculation with a clear message.

**What happens:**
- When task data is calculated, a flag (`.TASK_DATA_JUST_CALCULATED`) is set
- If complexity calculation is attempted while this flag is set, it will be skipped
- You'll see a message explaining why the calculation was skipped

**How to resolve:**
1. **Clear the workspace**: Click the broom icon in RStudio (Clear Workspace)
2. **OR restart R session**: Go to Session -> Restart R
3. **Then run again**: The complexity calculation will proceed normally

This is a technical limitation that we haven't been able to resolve yet, but the workaround is simple and reliable.

### Risk Data Workflow for Simulation Data

The simulation data initially does not include risk predictions. To add risk metrics to your simulation data, follow this workflow:

**Step 1: Train a Risk Model**
1. Go to **Page 17: Simulation Data** (`page17_simulationData.Rmd`)
2. Select participants and trials with simulation data
3. Configure risk model parameters (tau, sampling frequency)
4. Click **"Train New Model"** to create a risk model

**Step 2: Generate Risk Predictions**
1. After training, click **"Predict"** to apply the model to hazard samples
2. This creates hazard samples with risk predictions
3. The predictions are saved to `data_extra/hazard_samples_with_preds.rds`

**Step 3: Regenerate Simulation Data with Risk Metrics**
1. **Delete existing task data**: Remove the task metrics RDS file to force regeneration
2. **Restart the session**: Clear workspace or restart R to reset the flag
3. **Run initialization again**: The system will regenerate task data with risk metrics included

**Optional: Risk Analysis**
- Click **"Perform Risk Analysis"** to run Difference-in-Differences (DiD) analysis
- This compares risk changes across conditions using standardized comparisons
- Provides statistical insights into behavioral changes across experimental conditions

**Important Notes:**
- Risk predictions are only added during data generation, not retroactively
- You must regenerate task data after creating risk predictions
- The `.TASK_DATA_JUST_CALCULATED` flag prevents complexity calculation after task data generation
- Clear the workspace between task data generation and complexity calculation

## Data Processing Pipeline

The analysis pipeline has been completely restructured with the following key features:

### **Modular Architecture**
- **Feature Modules**: Separate modules for gait analysis, complexity metrics, simulation data, questionnaire processing, and statistical analysis
- **Utility Functions**: Shared utilities for data loading, caching, logging, and processing
- **API Layer**: Clean interfaces between modules

The codebase is organized into feature modules (`./source/features/`) and utilities (`./source/utils/`), each with their own functions and dependency hierarchy. These modules are loaded through the global initialization system (`./source/initialization.R` and `./source/global.R`), which sets up the environment and makes all functions available throughout the analysis pipeline.

### **Performance Optimizations**
- **Caching**: Automatic caching using `qs` format for fast data loading
- **Parallel Processing**: Support for both sequential and parallel processing modes
- **Vectorized Operations**: Optimized algorithms for complex calculations
- **Memory Management**: Efficient memory usage with garbage collection

### **Data Quality Control**
- **Outlier Detection**: Multiple levels of outlier detection and correction
- **Signal Processing**: Advanced filtering and preprocessing capabilities
- **Validation**: Comprehensive data validation and error handling
- **Logging**: Detailed logging for debugging and monitoring

### **Statistical Analysis**
- **Linear Mixed Models**: Advanced statistical modeling capabilities
- **Post-hoc Analysis**: Comprehensive post-hoc testing with multiple comparison corrections
- **Difference-in-Differences**: Causal inference analysis
- **Power Analysis**: Statistical power calculations


## Contents
- `index.Rmd` - The main Rmd code to run that loads all the source code, calculates or loads the results, and renders all the pages + sidebar. --> Run this code to start the interface.
    - The first block in this file (setup block) loads all the source code and calculates the gait parameters, complexity metrics, simulation data, and questionnaire results. The first time this is done, it creates RDS files in the results folder, which will be loaded instead of calculated in subsequent runs. To recalculate the datasets, simply delete the old ones, and re-run this first block (or the whole file).
    - Then a series of pages are loaded. For an overview of each page, see below.

- `data/` - Contains all data of all participants.
- `pages/` - Contains the Rmd code for rendering the shiny pages + sidebar (shown below).
- `source/` - Contains the modular source code organized by feature (gait, complexity, simulation, questionnaire, stats, etc.). Each module has its own API and utilities. The modules are loaded through the global initialization system.
- `questionnaires/` - Contains information about the questionnaire questions (IMI and UserExperience questionnaires).
- `results/` - Folder in which the resulting tables are stored (calculated gait parameters, complexity metrics, simulation data, questionnaire results).
- `data_extra/` - Contains additional data files including outlier annotations, risk models, and derived metrics.

## Pages
Click through the different tabs to get a preview and a brief explanation of what can be found there.

- [Ball-on-Plate Game Experiment Analysis Code](#ball-on-plate-game-experiment-analysis-code)
  - [How to use](#how-to-use)
  - [Configuration](#configuration)
    - [**Environment Profiles**](#environment-profiles)
    - [**Key Configuration Options**](#key-configuration-options)
  - [Known Issues and Workarounds](#known-issues-and-workarounds)
    - [Complexity Calculation Bug](#complexity-calculation-bug)
    - [Risk Data Workflow for Simulation Data](#risk-data-workflow-for-simulation-data)
  - [Data Processing Pipeline](#data-processing-pipeline)
    - [**Modular Architecture**](#modular-architecture)
    - [**Performance Optimizations**](#performance-optimizations)
    - [**Data Quality Control**](#data-quality-control)
    - [**Statistical Analysis**](#statistical-analysis)
  - [Contents](#contents)
  - [Pages](#pages)
    - [Page 0: `sidebar_dynamicDataFiltering.Rmd`](#page-0-sidebar_dynamicdatafilteringrmd)
    - [Page 1: `page1_feetTrajectories.Rmd`](#page-1-page1_feettrajectoriesrmd)
    - [Page 2: `page2_removedSteps.Rmd`](#page-2-page2_removedstepsrmd)
    - [Page 3: `page3_rawTrackerData.Rmd`](#page-3-page3_rawtrackerdatarmd)
    - [Page 5: `page5_histograms.Rmd`](#page-5-page5_histogramsrmd)
    - [Page 6: `page6_scatterplots.Rmd`](#page-6-page6_scatterplotsrmd)
    - [Page 8: `page8_questionnaires.Rmd`](#page-8-page8_questionnairesrmd)
    - [Page 9: `page9_boxplots.Rmd`](#page-9-page9_boxplotsrmd)
    - [Page 10: `page10_statistics.Rmd`](#page-10-page10_statisticsrmd)
    - [Page 11: `page11_correlations.Rmd`](#page-11-page11_correlationsrmd)
    - [Page 12: `page12_participantSummary.Rmd`](#page-12-page12_participantsummaryrmd)
    - [Page 13: `Page13_table.Rmd`](#page-13-page13_tablermd)
    - [Page 14: `page14_dataCorrection.Rmd`](#page-14-page14_datacorrectionrmd)
    - [Page 16: `page16_manualOutlierFiltering.Rmd`](#page-16-page16_manualoutlierfilteringrmd)
    - [Page 17: `page17_simulationData.Rmd`](#page-17-page17_simulationdatarmd)
    - [Page 18: `page18_powerSpectrum.Rmd`](#page-18-page18_powerspectrumrmd)


---

### Page 0: `sidebar_dynamicDataFiltering.Rmd`

The first "page" is the sidebar. This contains some settings that are shared across all pages. Most critical of this are the filtering settings, allowing you to filter out steps / trials / participants. There are also settings that change the figure sizes. Note that the filtered data is used for all plots and statistics calculations, and so changing these settings affects all the plots.

![sidebar](./readme_figures/sidebar.png)

- Categories explanation:
    Some pages, or filters in the sidebar have selectors for categories (e.g., the histograms), which allow you to split the data for that visualization. These categories are:
    - **participant**: split data per participant
    - **trialNum**: split data by trial number (1-11)
    - **condition**: categorize data by condition (control, perturbation, perturbation_visualization)
    - **phase**: categorize data by experimental phase (baseline, training, retention, transfer, washout, etc.)
    - **taskNum**: categorize data by task number (0-5)
    - **practice**: categorize data based on whether it's a practice trial or not (TRUE or FALSE)
    - **startedWithNoise**: separate data based on whether the participant started with VFD (noise) condition or not (TRUE or FALSE)
    - **conditionNumber**: categorize data by condition number (first or second)
    - **trialNumWithoutPractice**: split data by trial number, excluding practice trials
    - **trialNumWithinPhase**: categorize data by trial number within each phase (1 or 2)
    - **noticed**: split data based on whether the participant noticed the VFD during the experiment (TRUE or FALSE)
- Some pages allow additional options:
    - Average data across conditions - take average of the two trials of each participant and each condition.
    - Calculate difference + means per participant - Calculate the difference from baseline to VFD condition, as well as mean of both conditions.

---

### Page 1: `page1_feetTrajectories.Rmd`

Plot the trajectory of the feet, with the heelstrike positions overlaid.

![page1](./readme_figures/page1_feetTrajectories.png)

---

### Page 2: `page2_removedSteps.Rmd`

Pie-chart to show which steps have been removed.

![page2](./readme_figures/page2_removedSteps.png)

---

### Page 3: `page3_rawTrackerData.Rmd`

2D plot of raw tracker data of all the different trackers.

![page3](./readme_figures/page3_rawTrackerData.png)

---

### Page 5: `page5_histograms.Rmd`

Histograms of step parameters (heelstrike locations, step width, step length, etc).

![page5](./readme_figures/page5_histograms.png)

---

### Page 6: `page6_scatterplots.Rmd`

Scatter plots to analyze relationships between step parameters.

![page6](./readme_figures/page6_scatterplots.png)

---

### Page 8: `page8_questionnaires.Rmd`

Boxplots of questionnaire results (IMI and UserExperience questionnaires with subcategories + total scores).

![page8](./readme_figures/page8_questionnaires.png)

---

### Page 9: `page9_boxplots.Rmd`

Box plots for step parameters.

![page9](./readme_figures/page9_boxplots.png)

---

### Page 10: `page10_statistics.Rmd`

Statistical analysis of step data using Linear Mixed Models (LMM). Outputs tables with results and performs post-hoc analysis if required. Scroll down to visually evaluate model assumptions (QQ-plots, etc).

![page10](./readme_figures/page10_statistics.png)

---

### Page 11: `page11_correlations.Rmd`

Correlation plots showing relationships between different step parameters. Scroll down to evaluate test assumptions. If violated, you can set data to be non-parametric/Bayesian/robust types to perform different (linear) correlation tests.

![page11](./readme_figures/page11_correlations.png)

---

### Page 12: `page12_participantSummary.Rmd`

Shows information about participant groups (including demographics and category data). 

![page12](./readme_figures/page12_participantSummary.png)

---

### Page 13: `Page13_table.Rmd`

Data table with all (filtered) step data. Enable the "Summarize" checkbox to see aggregated results per trial.

![page13](./readme_figures/page13_table.png)

---

### Page 14: `page14_dataCorrection.Rmd`

Functions to correct step data by rotating the whole dataset around the y-axis (to correctly align the treadmill direction with the data z-axis). Save rotations into a table, shown next to the table.

![page14](./readme_figures/page14_dataCorrection.png)

---

### Page 16: `page16_manualOutlierFiltering.Rmd`

Interface to manually filter out outliers in the data. Save outliers to CSV & import later to continue. Once finished, click "Overwrite Outliers" to mark the selected steps as outliers. Click "Save to Current RDS" to re-save the gait parameters table to the RDS file with the current outliers flagged. Also includes auto-saving functionality.

![page16](./readme_figures/page16_manualOutlierFiltering.png)

---

### Page 17: `page17_simulationData.Rmd`

Analysis and visualization of ball-on-plate game simulation data including performance metrics, risk analysis, and energy dynamics.

![page17](./readme_figures/page17_simulationData.png)

---

### Page 18: `page18_powerSpectrum.Rmd`

Power spectral density analysis of movement patterns and frequency domain characteristics.

![page18](./readme_figures/page18_powerSpectrum.png)
