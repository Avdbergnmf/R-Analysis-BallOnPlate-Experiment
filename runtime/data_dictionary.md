# **Data Dictionary for Ball-on-Plate Game Experiment**

This document provides an overview of the variables included in the dataset for the study titled:  
*"Visual Disturbances to Avatar Foot Position Increase Step-width Variability in Immersive VR Treadmill Walking."*  

Each variable is briefly described to help understand its purpose and how it was derived.

If you are using the interface, the sidebar contains many different ways to filter our complete dataset. All figures and calculations in the windows on the right are using the data that is selected in this sidebar. Data can be filtered based on participants, trials, and steps. Values that are selected in the selection boxes are **included** in the dataset. Removing all data may crash some of the calculations and with that the whole interface, so be aware of that. At the bottom there are some settings you can use to finetune the generation of the figures to get the sizes you require.

## **Data Processing Pipeline Overview**

The data processing pipeline has been completely restructured into modular feature-based components:

- **Gait Analysis**: Foot event detection, step parameters, and gait statistics
- **Complexity Metrics**: Nonlinear dynamics analysis including sample entropy, multiscale entropy, DFA, and Lyapunov exponents
- **Simulation Data**: Ball-on-plate game performance metrics and risk analysis
- **Power Spectral Density**: Frequency domain analysis of movement patterns
- **Questionnaire Data**: IMI, User Experience, and other subjective measures
- **Statistical Analysis**: Linear mixed models, post-hoc tests, and difference-in-differences analysis

## **Outlier Processing**

The system now includes sophisticated outlier detection and processing:

1. **Automatic Detection**: Initial outlier detection based on statistical thresholds
2. **Manual Annotation**: Interface for manual outlier identification and correction
3. **Foot Placement Control**: Advanced foot placement control metrics with outlier filtering
4. **Risk Prediction**: Machine learning-based risk assessment for ball drops

To reproduce results, outliers are automatically processed during data loading, but manual corrections can be applied through the interface.

## **Risk Data Workflow for Simulation Data**

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

---

## **1. Participant and Trial Information**

| Variable Name               | Description                                                   |
|-----------------------------|---------------------------------------------------------------|
| `participant`                | Unique identifier for each participant.                      |
| `trialNum`                   | Trial number within the entire experiment (1-11).                   |
| `condition`                  | Condition name (control, perturbation, perturbation_visualization). |
| `phase`                      | Experimental phase (baseline, training, retention, transfer, washout, etc.). |
| `taskNum`                    | Task number (0-5) indicating the specific task type. |
| `foot`                       | Foot (Left or Right) associated with the measurement.        |
| `practice`                   | Whether the trial was a practice run (1 = Yes, 0 = No).      |
| `startedWithNoise`            | Indicates if the participant started with visual noise (=VFD).      |
| `conditionNumber`             | Currently doing the first or second condition? (1-2)           |
| `trialNumWithoutPractice`      | Trial number excluding practice trials (1-4).                     |
| `trialNumWithinPhase`          | Trial number within a specific phase (1-2).                        |
| `noticed`                     | Whether participants noticed the disturbance (1 = Yes, 0 = No). |
| `suspect`                     | Whether the step is flagged as suspect due to detection issues. |
| `outlierSteps`                | Whether the step is marked as an outlier. |
| `step`                        | Sequential step number within the trial. |

---

## **2. Questionnaire Responses**

### **Intrinsic Motivation Inventory (IMI)**  
Responses from the IMI questionnaire measuring intrinsic motivation:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `IMI.effort_importance`       | Perceived effort and importance of the task.                 |
| `IMI.interest_enjoyment`       | Enjoyment and interest level of the task.                   |
| `IMI.perceived_competence`    | Self-reported competence in performing the task.             |
| `IMI.total`                    | Combined IMI score across all subscales.                     |

### **User Experience Questionnaire**  
Subjective ratings of exoskeleton experience:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `UserExperience.comfort`      | Comfort level while walking in the exoskeleton.              |
| `UserExperience.resistance`   | Perceived resistance from the exoskeleton.                   |
| `UserExperience.agency`       | Sense of control over the exoskeleton.                      |
| `UserExperience.safety`       | Perceived safety while using the exoskeleton.                |
| `UserExperience.total`        | Combined User Experience score across all items.            |

---

## **3. Step Kinematics**

Step duration (rough estimate, requires further processing if it is to be used) and spatial parameters:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `stepTimes.mean/sd/cv`        | Mean, standard deviation, and coefficient of variation of step duration (s). |
| `stepLengths.mean/sd/cv`      | Mean, standard deviation, and coefficient of variation of step length (m). |
| `stepWidths.mean/sd/cv`       | Mean, standard deviation, and coefficient of variation of step width (m). |

### **Centered Step Data**  
Values adjusted for participant+trial-specific means:

| Variable Name                      | Description                                                   |
|------------------------------------|---------------------------------------------------------------|
| `centered_stepLengths.mean/sd/cv`   | Centered values for step length (m).                              |
| `centered_stepWidths.mean/sd/cv`    | Centered values for step width (m).                               |

---

## **4. Speed Metrics**
Speed depends on stepTime (duration) and is therefore more of a rough estimate. Before it can be used properly, stepTime should be processed better.
| Variable Name                 | Description                                                   |
|-------------------------------|---------------------------------------------------------------|
| `speed.mean/sd/cv`             | Mean, standard deviation, and coefficient of variation of walking speed (m/s). |

---

## **5. Gait Event Data**

### **Heel Strike Data**
Measurements related to heel strikes during walking:

| Variable Name                           | Description                                                   |
|-----------------------------------------|---------------------------------------------------------------|
| `heelStrikes.time.mean/sd/cv`            | Timing of heel strikes (mean, standard deviation, CV, in s).        |
| `heelStrikes.pos_x/y/z.mean/sd/cv`       | Heel position in X, Y, Z axes (m).  |
| `heelStrikes.rot_x/y/z.mean/sd/cv`       | Heel rotation in X, Y, Z axes (m).  |
| `heelStrikes.offset_x/y/z.mean/sd/cv`    | VFD offset in each direction (m).                         |
| `heelStrikes.magnitude.mean/sd/cv`       | Magnitude of the applied disturbance (m).                         |
| `heelStrikes.final_pos_x/y/z.mean/sd/cv` | Final foot position after summing with the offset (m).                        |

### **Toe-Off Data**
Toe-off data measures (note that these are not correctly filtered and are a rough estimate and require additional processing if they are to be used for further analyses):

| Variable Name                 | Description                                                   |
|-------------------------------|---------------------------------------------------------------|
| `toeOffs.time.mean/sd/cv`      | Timing of toe-offs (mean, standard deviation, CV, in s).            |
| `toeOffs.pos_x/y/z.mean/sd/cv` | Position at toe-off in X, Y, Z axes (m).                           |
| `toeOffs.rot_x/y/z.mean/sd/cv` | Rotational data at toe-off (degrees, in euler angles).                                    |

### **Relative Heel Strike Data**
These variables represent the positions of the heel strikes and offset (VFD sizes) relative to the previous step with the same foot. They may be used to get a better understanding of how these metrics change from step to step:

| Variable Name                           | Description                                                   |
|-----------------------------------------|---------------------------------------------------------------|
| `relHeelStrikes.pos_x/y/z.mean/sd/cv`    | Relative position of heel strikes.                            |
| `relHeelStrikes.offset_x/y/z.mean/sd/cv` | Relative offset of foot positions.                            |

### **Current Experiment Variables**

The experiment now focuses on **Ball-on-Plate simulation** rather than target stepping. The main datasets contain:

#### **`allComplexityMetrics` (111 variables)**
Nonlinear dynamics analysis of movement patterns to quantify movement complexity and variability:

**Complexity Metrics Explained:**
- **`sampen`**: Sample Entropy - measures irregularity/randomness in time series (higher = more complex/irregular)
- **`mse_index`**: Multiscale Entropy complexity index - sum of entropy across multiple time scales (higher = more complex across scales)
- **`dfa_alpha`**: Detrended Fluctuation Analysis scaling exponent - quantifies long-range correlations (0.5 = random, >0.5 = persistent, <0.5 = anti-persistent)
- **`lyapunov`**: Largest Lyapunov exponent - measures sensitivity to initial conditions/chaos (positive = chaotic, negative = stable)
- **`sd_ratio`**: Standard deviation ratio - ratio of first-difference variance to total variance (measures local vs global variability)

**Frequency Domain Analysis:**
- **`peak_freq`**: Frequency with maximum power in the spectrum (Hz)
- **`mean_freq`**: Weighted average frequency of the power spectrum (Hz)
- **`spectral_entropy`**: Entropy of the power spectrum (measures spectral complexity)
- **`spectral_centroid`**: Center of mass of the power spectrum (Hz)
- **`spectral_bandwidth`**: Spread of frequencies around the centroid (Hz)
- **`power_low/mid/high`**: Power in different frequency bands (adaptive based on step frequency)

**Step Timing & Width Analysis:**
- **`stepTimes_mean/sd/cv/n`**: Basic statistics of time between consecutive heel strikes
- **`stepWidths_mean/sd/cv/n`**: Basic statistics of step width (lateral distance between feet)
- **Complexity metrics**: `sampen/mse_index/dfa_alpha/lyapunov` applied to step timing and width patterns

**Foot Placement Control Analysis:**
- **`step_pos_x_residual_rmse`**: Root mean square error of foot placement prediction model (lower = better control)
- **`step_pos_x_beta0/beta1`**: Intercept and slope of foot placement control model (FP = β₀ + β₁ × hip position)
- **`step_pos_x_r2`**: R-squared of foot placement control model (higher = better prediction)
- **`step_pos_x_n`**: Number of valid foot placement observations used in model
- **Per-foot analysis**: `_left`, `_right`, and `_mean` versions for each foot and averaged metrics

**Body Position Complexity:**
- **`hipPos_raw/filt`**: Hip position time series (raw and filtered) with complexity metrics
- **`pelvisPos_raw/filt`**: Pelvis position time series (raw and filtered) with complexity metrics  
- **`p_raw/filt`**: Plate position time series (raw and filtered) with complexity metrics
- **Each includes**: `mean/sd/cv/n` (basic stats), `sampen/mse_index/dfa_alpha/lyapunov` (complexity), `sd_ratio` (variability ratio), and full frequency domain analysis

**Experimental Design & Demographics:**
- **Design**: `participant/condition/trialNum/perturbations/visualizations/task/treadmillSpeed/phase/trialNumWithinPhase/phaseNum/taskNum/is_training_trial`
- **Demographics**: `gender/motion/age/weight/education/vr_experience/height_meters`

#### **`allGaitParams` (42 variables)**
Basic gait parameters and participant info:
- **Temporal data**: `time/stepTimes/stepLengths/stepWidths/centered_stepLengths/centered_stepWidths/speed`
- **Position data**: `pos_x/y/z/actual_pos_z/centered_pos_x/z/hip_pos_x/y/z`
- **Rotation data**: `rot_x/y/z`
- **Step tracking**: `foot/participant/trialNum/outlierSteps/step`
- **Experimental design**: `perturbations/visualizations/task/treadmillSpeed/condition/phase/trialNumWithinPhase/phaseNum/taskNum/is_training_trial`
- **Demographics**: `gender/motion/age/weight/education/vr_experience/height_meters`

#### **`allQResults` (12 variables)**
Questionnaire responses:
- **IMI (Intrinsic Motivation Inventory)**: `IMI.effort_importance/interest_enjoyment/perceived_competence/total`
- **User Experience**: `UserExperience.agency/comfort/resistance/safety/total`
- **Metadata**: `participant/answer_type/condition`

#### **`allTaskMetrics` (186 variables)**
Ball-on-Plate simulation performance metrics quantifying control, energy, and risk:

**Simulation Performance:**
- **`sim_duration`**: Total simulation time (seconds)
- **`n_samples`**: Number of data points recorded
- **`mean_arcDeg`**: Average arc degrees (ball position on circular plate)
- **`n_arcDeg_changes`**: Number of times ball crossed plate boundaries
- **`n_ball_falls`**: Number of times ball fell off the plate
- **`final_work`**: Total work done by the plate (J/kg)
- **`final_work_plate/world`**: Work in plate-relative vs world coordinates

**Ball Dynamics (Arc-length Coordinates):**
- **`q_mean/sd/cv`**: Arc-length position (distance along plate edge) - basic statistics
- **`qd_mean/sd/cv`**: Arc-length velocity (rate of change along edge)
- **`p_mean/sd/cv`**: Plate position (actual plate movement)
- **`pInput_mean/sd/cv`**: Input plate position (commanded movement)
- **`pVel/pAcc_mean/sd/cv`**: Plate velocity and acceleration
- **`pVelAbsolute/pAccAbsolute_mean/sd/cv`**: Absolute plate velocity and acceleration

**World Coordinate System:**
- **`vx/vy_mean/sd/cv`**: Ball velocity in X (horizontal) and Y (vertical) directions
- **`ax/ay_mean/sd/cv`**: Ball acceleration in X and Y directions  
- **`vx_world/ax_world_mean/sd/cv`**: World-frame velocity and acceleration (includes plate motion)
- **`x/y_mean/sd/cv`**: Ball position in X and Y coordinates
- **`x_world_mean/sd/cv`**: World-frame X position

**Energy Analysis:**
- **`ke_mean/sd/cv`**: Kinetic energy (plate-relative frame)
- **`ke_world_mean/sd/cv`**: Kinetic energy (world frame, includes plate motion)
- **`pe_mean/sd/cv`**: Potential energy (gravitational)
- **`e_mean/sd/cv`**: Total energy (plate-relative frame)
- **`e_world_mean/sd/cv`**: Total energy (world frame)

**Power and Work:**
- **`power_mean/sd/cv`**: Power (plate-relative frame, W/kg)
- **`power_world_mean/sd/cv`**: Power (world frame, W/kg)
- **`power_plate_mean/sd/cv`**: Plate power (plate motor work, W/kg)
- **`work_mean/sd/cv`**: Cumulative work (plate-relative frame, J/kg)
- **`work_world_mean/sd/cv`**: Cumulative work (world frame, J/kg)
- **`work_plate_mean/sd/cv`**: Cumulative plate work (J/kg)

**Safety and Risk Analysis:**
- **`margin_E_mean/sd/cv`**: Energy margin (energy available for escape)
- **`danger_mean/sd/cv`**: Danger level (inverse of safety margin)
- **`dist_to_escape_mean/sd/cv`**: Distance to escape (arc-length to plate edge)
- **`dist_to_escape_ratio_mean/sd/cv`**: Distance to escape as ratio of total circumference
- **`dist_to_escape_arcdeg6_mean/sd/cv`**: Distance to escape in arc degrees (6-degree resolution)
- **`e_total_needed_mean/sd/cv`**: Total energy needed to escape from current position
- **`time_to_escape_mean/sd/cv`**: Time required to escape from current position

**Risk Prediction Variables (from trained models):**
- **`drop_risk_bin_mean/sd/cv`**: Drop risk per time bin (tau seconds)
- **`drop_lambda_mean/sd/cv`**: Drop rate per second (hazard rate)
- **`drop_risk_1s_mean/sd/cv`**: Drop risk over 1-second window
- **`velocity_towards_edge_mean/sd/cv`**: Velocity component toward plate edge
- **`approach_pressure_mean/sd/cv`**: Pressure to approach edge (risk-seeking behavior)
- **`retreat_pressure_mean/sd/cv`**: Pressure to retreat from edge (risk-avoiding behavior)
- **`retreat_share_mean/sd/cv`**: Proportion of time spent retreating
- **`retreat_speed_cond_mean/sd/cv`**: Retreat speed (conditional on retreating)
- **`approach_speed_cond_mean/sd/cv`**: Approach speed (conditional on approaching)
- **`absqd_mean/sd/cv`**: Absolute arc-length velocity (speed along edge)
- **`v_e_mean/sd/cv`**: Escape distance velocity (rate of change in distance to escape)
- **`edge_pressure_mean/sd/cv`**: Edge pressure (force toward edge)
- **`edge_pressure_clamped_mean/sd/cv`**: Clamped edge pressure (bounded values)
- **`log_v_to_edge_mean/sd/cv`**: Log-transformed velocity toward edge
- **`post_log_v_to_edge`**: Log-transformed mean velocity toward edge
- **`post_log_edge_pressure`**: Log-transformed mean edge pressure

**Performance Metrics:**
- **`score_mean/sd/cv`**: Running score during simulation
- **`total_score`**: Total performance score
- **`simulating_mean/sd/cv`**: Ball-on-plate status (1 = on plate, 0 = off)
- **`is_training_trial_mean/sd/cv`**: Training trial indicator

**Experimental Design & Demographics:**
- **Design**: `participant/trialNum/perturbations/visualizations/task/treadmillSpeed/condition/phase/trialNumWithinPhase/phaseNum/taskNum/is_training_trial`
- **Demographics**: `gender/motion/age/weight/education/vr_experience/height_meters`

---

## **6. Difference Metrics**

The diff of each of the metrics (how much they changed from one step to the next, with both feet included):

| Variable Name                     | Description                                                   |
|-----------------------------------|---------------------------------------------------------------|
| `diffData.stepWidth.mean/sd/cv`    | Differences in step width across conditions.                  |
| `diffData.stepLength.mean/sd/cv`   | Differences in step length across conditions.                 |

---

## **7. Complexity Metrics**

Nonlinear dynamics analysis of movement patterns:

### **Sample Entropy (SampEn)**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `stepTimes.sampen`            | Sample entropy of step timing variability.                    |
| `stepWidths.sampen`          | Sample entropy of step width variability.                    |
| `p.sampen`                   | Sample entropy of plate position (simulation data).          |
| `hipPos.sampen`              | Sample entropy of hip position variability.                  |
| `pelvisPos.sampen`           | Sample entropy of pelvis position variability.               |

### **Multiscale Entropy (MSE)**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `stepTimes.mse_index`         | Multiscale entropy complexity index for step timing.          |
| `stepWidths.mse_index`        | Multiscale entropy complexity index for step width.          |
| `p.mse_index`                 | Multiscale entropy complexity index for plate position.      |
| `hipPos.mse_index`           | Multiscale entropy complexity index for hip position.         |
| `pelvisPos.mse_index`        | Multiscale entropy complexity index for pelvis position.     |

### **Detrended Fluctuation Analysis (DFA)**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `stepTimes.dfa_alpha`         | DFA scaling exponent for step timing.                         |
| `stepWidths.dfa_alpha`        | DFA scaling exponent for step width.                         |
| `p.dfa_alpha`                 | DFA scaling exponent for plate position.                     |
| `hipPos.dfa_alpha`           | DFA scaling exponent for hip position.                       |
| `pelvisPos.dfa_alpha`        | DFA scaling exponent for pelvis position.                   |

### **Lyapunov Exponents**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `stepTimes.lyapunov`          | Largest Lyapunov exponent for step timing.                   |
| `stepWidths.lyapunov`         | Largest Lyapunov exponent for step width.                    |
| `p.lyapunov`                  | Largest Lyapunov exponent for plate position.                |
| `hipPos.lyapunov`            | Largest Lyapunov exponent for hip position.                  |
| `pelvisPos.lyapunov`         | Largest Lyapunov exponent for pelvis position.              |

### **Power Spectral Density Analysis**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `stepTimes.peak_freq`         | Peak frequency in step timing spectrum.                      |
| `stepTimes.mean_freq`        | Mean frequency in step timing spectrum.                      |
| `stepTimes.spectral_entropy`  | Spectral entropy of step timing.                             |
| `stepTimes.power_low/mid/high` | Power in low/mid/high frequency bands.                      |
| `stepWidths.peak_freq`        | Peak frequency in step width spectrum.                       |
| `stepWidths.mean_freq`        | Mean frequency in step width spectrum.                       |
| `stepWidths.spectral_entropy` | Spectral entropy of step width.                              |
| `stepWidths.power_low/mid/high` | Power in low/mid/high frequency bands.                      |

---

## **8. Foot Placement Control Metrics**

Advanced foot placement control analysis:

| Variable Name                           | Description                                                   |
|-----------------------------------------|---------------------------------------------------------------|
| `step_pos_x_residual_rmse_left/right`   | Root mean square error of foot placement residuals (per foot). |
| `step_pos_x_beta0_left/right`           | Intercept of foot placement control model (per foot).         |
| `step_pos_x_beta1_left/right`           | Slope of foot placement control model (per foot).             |
| `step_pos_x_r2_left/right`              | R-squared of foot placement control model (per foot).        |
| `step_pos_x_n_left/right`               | Number of valid data points for control model (per foot).     |
| `step_pos_x_residual_rmse_mean`         | Mean RMSE across both feet.                                  |
| `step_pos_x_beta1_mean`                 | Mean slope across both feet.                                 |
| `step_pos_x_r2_mean`                    | Mean R-squared across both feet.                             |
| `step_pos_x_n_total`                    | Total number of valid data points.                           |

---

## **9. Simulation/Task Performance Metrics**

Ball-on-plate game performance and risk analysis:

### **Basic Performance**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `total_score`                 | Total score achieved in the ball-on-plate game.               |
| `final_score`                | Final score before time bonus.                               |
| `time_in_bowl_bonus`         | Bonus points for time spent with ball in bowl.               |
| `n_ball_falls`               | Number of times the ball fell off the plate.                 |
| `sim_duration`               | Total duration of simulation (seconds).                       |
| `n_samples`                  | Number of simulation samples.                                |

### **Position and Movement**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `q.mean/sd/cv`                | Arc-length position (mean, SD, CV).                         |
| `p.mean/sd/cv`                | Plate position (mean, SD, CV).                               |
| `x.mean/sd/cv`                | X position relative to plate (mean, SD, CV).                 |
| `y.mean/sd/cv`                | Y position (height) (mean, SD, CV).                          |
| `vx.mean/sd/cv`               | X velocity relative to plate (mean, SD, CV).                 |
| `vy.mean/sd/cv`               | Y velocity (mean, SD, CV).                                   |
| `ax.mean/sd/cv`               | X acceleration relative to plate (mean, SD, CV).            |
| `ay.mean/sd/cv`               | Y acceleration (mean, SD, CV).                               |

### **Energy and Power**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `e.mean/sd/cv`                | Total energy (mean, SD, CV).                                 |
| `ke.mean/sd/cv`               | Kinetic energy (mean, SD, CV).                               |
| `pe.mean/sd/cv`               | Potential energy (mean, SD, CV).                             |
| `power.mean/sd/cv`            | Power (mean, SD, CV).                                         |
| `work.mean/sd/cv`             | Work done (mean, SD, CV).                                     |

### **Safety and Risk Metrics**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `margin_E.mean/sd/cv`        | Energy margin to escape (mean, SD, CV).                      |
| `danger.mean/sd/cv`          | Danger level (mean, SD, CV).                                 |
| `dist_to_escape.mean/sd/cv`  | Distance to escape (mean, SD, CV).                           |
| `dist_to_escape_ratio_mean_corrected` | Corrected distance to escape ratio.                          |
| `drop_risk_bin.mean/sd/cv`   | Drop risk per time bin (mean, SD, CV).                        |
| `drop_lambda.mean/sd/cv`     | Drop rate per second (mean, SD, CV).                         |
| `drop_risk_1s.mean/sd/cv`    | 1-second drop risk (mean, SD, CV).                           |
| `velocity_towards_edge.mean/sd/cv` | Velocity towards plate edge (mean, SD, CV).                   |
| `edge_pressure.mean/sd/cv`   | Edge pressure (mean, SD, CV).                                |
| `post_log_v_to_edge`         | Log-transformed velocity towards edge.                        |
| `post_log_edge_pressure`     | Log-transformed edge pressure.                                |

### **Arc Degree and Level**
| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `mean_arcDeg`                 | Mean arc degree level during trial.                          |
| `n_arcDeg_changes`            | Number of arc degree level changes.                           |
| `final_work`                  | Final work done (plate-relative).                             |
| `final_work_plate`            | Final work done by plate.                                    |
| `final_work_world`            | Final work done (world coordinates).                          |

---

## **10. Power Spectral Density Analysis**

Frequency domain analysis of movement patterns:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `frequency`                   | Frequency bins in Hz.                                         |
| `power`                       | Power spectral density values.                                |
| `mean_power`                  | Mean power across trials/participants.                       |
| `sd_power`                    | Standard deviation of power.                                 |
| `max_power`                   | Maximum power value.                                          |
| `total_power`                 | Total power across all frequencies.                          |

---

## **11. Statistical Analysis Variables**

Variables used in statistical modeling:

| Variable Name                | Description                                                   |
|------------------------------|---------------------------------------------------------------|
| `age`                         | Participant age.                                              |
| `weight`                      | Participant weight.                                           |
| `height_meters`               | Participant height in meters.                                 |
| `gender`                      | Participant gender (Male/Female).                             |
| `education`                   | Education level.                                              |
| `vr_experience`               | VR experience level.                                          |
| `motion`                      | Motion sickness sensitivity (TRUE/FALSE).                     |
| `step_freq`                   | Calculated step frequency in Hz.                              |

---

## **12. Tracker Data Files**

**File Naming Convention**
All tracker files follow the pattern:

```
runtime/data/[participant]/trackers/[object_name]_movement_T[trial_number].csv
```

Example filenames:

- `camera_movement_T001.csv` → Camera movement data for trial 1.
- `leftfoottracker_movement_T003.csv` → Left foot tracker data for trial 3.

The dataset includes multiple CSV files that contain tracking data recorded during the experiment. These files are named based on the tracked object and trial number. The tracked objects and corresponding variables within each file type are described below.

**Note**: The data processing pipeline now includes sophisticated caching and parallel processing capabilities. Tracker data is automatically cached using the `qs` format for improved performance, and the system supports both sequential and parallel processing modes.

### **Tracked Objects and Variables**

#### **General Tracker Files:**
These files contain positional and rotational data over time for different tracked objects:

- **Tracked Objects:**  
  - `camera`
  - `controllerleft` (left controller)
  - `controllerright` (right controller)
  - `lefthandtracker`
  - `righthandtracker`
  - `hiptracker`
  - `leftfoottracker`
  - `rightfoottracker`
  - `treadmillleft`, `treadmillright`, `treadmillrightback` (used to determine treadmill position in the virtual environment)

- **Variables:**
  ```plaintext
  time, pos_x, pos_y, pos_z, rot_x, rot_y, rot_z
  ```

  - `time`: Timestamp of the recorded frame.
  - `pos_x, pos_y, pos_z`: Position coordinates of the tracked object in the virtual environment (m).
  - `rot_x, rot_y, rot_z`: Rotation angles of the tracked object (degrees, in euler angles).

---

#### **Step Target Tracker Files:**
These files provide information about participants' foot placements relative to visual targets.

- **Filename Example:** `steptargetsmanager_targetsteps_T001.csv`
- **Variables:**
  ```plaintext
  time, foot, score, destroyed, target_x, target_y, target_z, 
  foot_x, foot_y, foot_z, foot_rot_x, foot_rot_y, foot_rot_z, foot_rot_w
  ```

  - `time`: Timestamp of the target event.
  - `foot`: Indicates which foot was used (left/right).
  - `score`: Points awarded based on target accuracy.
  - `destroyed`: Boolean value; `TRUE` if the target was missed (this never happened).
  - `target_x, target_y, target_z`: Target position in the virtual environment.
  - `foot_x, foot_y, foot_z`: (Disturbed) foot position displayed in VR.
  - `foot_rot_x, foot_rot_y, foot_rot_z, foot_rot_w`: Foot orientation in VR.

**Note:** The `foot_x`, `foot_y`, and `foot_z` values represent the disturbed foot position shown in VR, which may differ from the actual foot position if VFD was enabled.

---

#### **Simulation Data Files:**
These files contain ball-on-plate game simulation data:

- **Filename Example:** `simulation_data_T001.csv`
- **Variables:**
  ```plaintext
  time, q, p, x, y, vx, vy, ax, ay, e, ke, pe, power, work, 
  margin_E, danger, dist_to_escape, score, arcDeg, simulating
  ```

  - `time`: Timestamp of the simulation frame.
  - `q`: Arc-length position of the ball.
  - `p`: Plate position.
  - `x, y`: Ball position coordinates.
  - `vx, vy`: Ball velocity components.
  - `ax, ay`: Ball acceleration components.
  - `e, ke, pe`: Total, kinetic, and potential energy.
  - `power, work`: Power and work calculations.
  - `margin_E`: Energy margin to escape.
  - `danger`: Danger level assessment.
  - `dist_to_escape`: Distance to escape the plate.
  - `score`: Running score in the game.
  - `arcDeg`: Arc degree level of the plate.
  - `simulating`: Whether the ball is on the plate.

---

#### **Level Data Files:**
These files contain task level information:

- **Filename Example:** `level_data_T001.csv`
- **Variables:**
  ```plaintext
  time, level, arcDeg
  ```

  - `time`: Timestamp of the level change.
  - `level`: Task level (0-9).
  - `arcDeg`: Arc degree corresponding to the level.

---

#### **Eye Tracker Files:**
Eye tracking data recorded throughout the trials.

- **Filename Example:** `eyetracking_EyeGaze_T001.csv`
- **Variables:**
  ```plaintext
  time, collider, hit_pos_x, hit_pos_y, hit_pos_z, pos_x, pos_y, pos_z, 
  dir_x, dir_y, dir_z, left_openness, left_diam, left_pupil_x, left_pupil_y, 
  left_validity, right_openness, right_diam, right_pupil_x, right_pupil_y, 
  right_validity, combined_conv_dist, combined_conv_validity, combined_openness
  ```

  - `time`: Timestamp of the gaze event.
  - `collider`: Object hit by gaze ray.
  - `hit_pos_x, hit_pos_y, hit_pos_z`: Coordinates of the gaze hit point.
  - `pos_x, pos_y, pos_z`: Position of the eye-tracking device in the virtual environment.
  - `dir_x, dir_y, dir_z`: Direction of the gaze.
  - `left_openness, right_openness`: Eye openness values.
  - `left_diam, right_diam`: Pupil diameters.
  - `left_pupil_x, left_pupil_y, right_pupil_x, right_pupil_y`: Pupil center positions.
  - `left_validity, right_validity`: Validity flags for left and right eye tracking.
  - `combined_conv_dist, combined_conv_validity, combined_openness`: Combined gaze convergence metrics.

**Note:** This data was not used in the final analysis but is included for completeness.

---

#### **Disturbance Noise Tracker Files:**
These files track the disturbances applied to foot positions during the experiment.

- **Filename Example:** `leftfoot_disturbance_noise_T001.csv`
- **Variables:**
  ```plaintext
  time, is_active, is_grounded, offset_x, offset_y, offset_z
  ```

  - `time`: Timestamp of the disturbance event.
  - `is_active`: Boolean indicating whether the disturbance is currently applied.
  - `is_grounded`: Boolean indicating whether the foot is below a certain threshold and considered grounded.
  - `offset_x, offset_y, offset_z`: Offset values applied to the foot position in the VR environment.

---

## **13. Data Processing Pipeline**

The data processing pipeline has been completely restructured with the following key features:

### **Modular Architecture**
- **Feature Modules**: Separate modules for gait, complexity, simulation, questionnaire, and statistical analysis
- **Utility Functions**: Shared utilities for data loading, caching, logging, and processing
- **API Layer**: Clean interfaces between modules

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

### **Visualization and Reporting**
- **Interactive Plots**: Dynamic visualization capabilities
- **Publication-Ready Graphics**: High-quality plots for scientific publications
- **Statistical Annotations**: Automatic statistical annotation on plots
- **Export Capabilities**: Multiple export formats for data and figures

---