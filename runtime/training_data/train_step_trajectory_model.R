## ========================================================================== ##
## train_step_trajectory_model.R – model based on pos_x trajectory per step  ##
## ========================================================================== ##
#  This script builds a per-step training set consisting of the x-axis         #
#  trajectory (top-view) of each step.  A step trajectory is the segment       #
#  from previous heel-strike (same foot) up to the next heel-strike (same      #
#  foot).  The segment is time-normalised and resampled to N points, which     #
#  become the feature vector.  Labels come from the existing `outlierSteps`    #
#  column in allGaitParams*.rds.  The script:                                  #
#     1.  Loads helper functions (initialisation, data loading, etc.).         #
#     2.  Builds a trajectory data set in parallel using                       #
#         `get_data_from_loop_parallel`.                                       #
#     3.  Trains a Random-Forest classifier and stores the model to RDS.       #
#  The code is written so neighbour trajectories can be added later: just      #
#  change `N_NEIGHBOURS` and adapt the `get_step_features()` helper.           #
## -------------------------------------------------------------------------- ##

# --------------------------- 0.  Parameters -------------------------------- #
N_RESAMPLE <- 25 # points per trajectory vector
MODEL_OUT <- "stepTrajectory_PCA_template.rds"
N_CORES <- parallel::detectCores() - 1
EXCLUDE_TRIALS <- c(1, 4, 6) # ignore warm-up / familiarisation trials

# --------------------------- 1.  Libraries --------------------------------- #
req <- c("data.table", "dplyr", "randomForest", "zoo", "foreach", "doParallel")
new <- req[!req %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new, quiet = TRUE)
for (p in req) library(p, character.only = TRUE)

# --------------------------- 2.  Source project code ----------------------- #
SCRIPT_DIR <- dirname(normalizePath(sys.frame(1)$ofile %||% "runtime/training_data"))
RUNTIME_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE) # parent folder 'runtime'

# Source core modules using paths relative to runtime directory
source(file.path(RUNTIME_DIR, "source", "initialization.R"))
source(file.path(RUNTIME_DIR, "source", "data_loading.R"))
source(file.path(RUNTIME_DIR, "source", "find_foot_events.R"))
source(file.path(RUNTIME_DIR, "source", "get_data_from_loop.R"))

# --------------------------- 3.  Helper functions -------------------------- #

# Resample a (t, x) trajectory to fixed length N using linear interpolation.
resample_traj <- function(t, x, n = N_RESAMPLE) {
    if (length(t) < 2 || length(unique(x)) == 1) {
        return(rep(NA_real_, n))
    }
    t_norm <- (t - min(t)) / (max(t) - min(t))
    approx(t_norm, x, xout = seq(0, 1, length.out = n), rule = 2)$y
}

# Given participant/trial/foot and heel-strike index, extract trajectory vector.
get_step_vector <- function(participant, trial, foot, heelData, footData) {
    # Locate current HS row in heelData
    row_idx <- which(heelData$foot == foot)
    if (length(row_idx) == 0) {
        return(NULL)
    }
    # heelData assumed ordered by time
    foot_rows <- heelData[row_idx, ]
    n_steps <- nrow(foot_rows)
    res_list <- vector("list", n_steps)
    for (i in seq_len(n_steps)) {
        t0 <- if (i == 1) NA else foot_rows$time[i - 1]
        t1 <- foot_rows$time[i]
        t2 <- if (i == n_steps) NA else foot_rows$time[i + 1]
        if (is.na(t0) || is.na(t2)) next # cannot form full step
        seg <- footData[footData$time >= t0 & footData$time <= t2, ]
        if (nrow(seg) < 5) next
        vec <- resample_traj(seg$time, seg$pos_x)
        res_list[[i]] <- data.frame(participant,
            trialNum = trial, foot,
            step = foot_rows$step[i],
            outlier = foot_rows$outlierSteps, t(vec)
        )
    }
    dplyr::bind_rows(res_list)
}

# Wrapper called in parallel per (participant, trial)
build_trial_trajectories <- function(participant, trial) {
    pre <- get_preprocessed_data(participant, trial, c("leftfoot", "rightfoot"))
    if (any(vapply(pre, nrow, integer(1)) == 0)) {
        return(data.frame())
    }
    events <- find_foot_events(participant, trial)
    hs <- events$heelStrikes # includes outlier flag already
    out <- list()
    for (ft in c("Left", "Right")) {
        ftDataName <- if (ft == "Left") "leftfoot" else "rightfoot"
        ftData <- pre[[ftDataName]]
        out[[ft]] <- get_step_vector(participant, trial, ft, hs, ftData)
    }
    dplyr::bind_rows(out)
}

# --------------------------- 4.  Build full training set ------------------- #

all_steps <- get_data_from_loop_parallel(
    build_trial_trajectories,
    datasets_to_verify = c("leftfoot", "rightfoot"),
    log_to_file = TRUE
)

if (nrow(all_steps) == 0) stop("No step trajectories extracted!")

# Remove trials 1/4/6 and any rows with NA vectors
valid_cols <- grep("^X", names(all_steps), value = TRUE) # trajectory columns
all_steps <- all_steps %>%
    dplyr::filter(!(trialNum %in% EXCLUDE_TRIALS)) %>%
    tidyr::drop_na(all_of(valid_cols))

# --------------------------- 5.  Build outlier template (PCA) ------------- #

# Keep only known outlier steps
outlier_steps <- dplyr::filter(all_steps, outlier == TRUE)

if (nrow(outlier_steps) < 20) {
    stop("Not enough outlier steps to build template (need at least 20).")
}

X <- as.matrix(outlier_steps[, valid_cols])

# Principal-component template – captures dominant shape variation among outliers
pca_model <- prcomp(X, center = TRUE, scale. = TRUE)

template <- list(
    pca = pca_model,
    center = pca_model$center,
    scale = pca_model$scale,
    n_resample = N_RESAMPLE,
    valid_cols = valid_cols
)

saveRDS(template, MODEL_OUT)
cat("[OK]  PCA outlier template saved to", MODEL_OUT, "\n")
