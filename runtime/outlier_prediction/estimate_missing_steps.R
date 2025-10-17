## ====================================================================== ##
## estimate_missing_steps.R - Estimate missing heel strikes from gaps   ##
## ====================================================================== ##
## This script scans the cleaned gait dataset (after false heel-strike   ##
## removal) for unusually long gaps (>~2× local median step period)      ##
## between consecutive heel strikes of the SAME foot. For each gap it    ##
## (1) infers the expected timestamp of the missing heel strike,         ##
## (2) samples the foot trajectory to estimate the likely position,       ##
## (3) derives a simple confidence metric, and (4) writes a CSV with     ##
## the proposed heel-strike additions.                                    ##
##                                                                       ##
## Methodology (high-level):                                              ##
##   1. Load heel-strike events (after removal of false strikes).         ##
##   2. For each participant/trial/foot:                                  ##
##        • Compute running median step period using a 5-step window.     ##
##        • Identify gaps where Δt > GAP_FACTOR × local median.           ##
##   3. For each candidate gap:                                           ##
##        • Expected time = t_prev + median_period.                       ##
##        • Extract foot trajectory (pos_x,pos_z) 5 samples before & after##
##          expected time (uses get_preprocessed_data → left/rightfoot).  ##
##        • Interpolate positions on both co-ordinates at expected time.  ##
##        • Confidence = 1 − |interp_height − median_height| / IQR_height ##
##          (proxy: how well vertical position matches neighbourhood).    ##
##   4. Export table `missing_steps_estimates.csv` to data_extra.         ##
##                                                                       ##
## NOTE: This is a heuristic, not ML. It trades complexity for            ##
## robustness & explainability.                                           ##
## ---------------------------------------------------------------------- ##

# --------------------------- CONFIG ------------------------------------- #
GAP_FACTOR <- 1.8 # consider gaps larger than 1.8× local median
WINDOW_SIZE <- 5 # window (steps) for local median period
CONF_HEIGHT_VAR <- "pos_z" # axis used for simple confidence metric
OUTPUT_FILE <- "missing_steps_estimates.csv" # written to data_extra

# --------------------------- INITIALISATION ----------------------------- #
source("runtime/source/initialization.R", local = FALSE)
initialize_global_parameters()

# Ensure heavy global data are available (participants list, etc.)
if (exists("ensure_global_data_initialized")) ensure_global_data_initialized()

library(dplyr)
library(data.table)

# Helper: load heel strikes for participant/trial then remove false ones
load_clean_hs <- function(participant, trialNum) {
    events <- find_foot_events(participant, trialNum)
    if (is.null(events) || nrow(events$heelStrikes) == 0) {
        return(data.frame())
    }
    hs <- events$heelStrikes
    # Remove false heel strikes listed in falseHeelStrikesFile
    fhs_path <- file.path(dataExtraFolder, falseHeelStrikesFile)
    if (file.exists(fhs_path)) {
        fhs <- data.table::fread(fhs_path)
        key <- fhs[participant == !!participant & trialNum == !!trialNum]
        if (nrow(key)) {
            hs <- hs[!round(hs$time, 2) %in% round(key$time, 2), ]
        }
    }
    hs
}

# Helper: estimate missing step within a gap
estimate_step <- function(participant, trialNum, foot, t_prev, t_next, median_period, foot_data) {
    est_time <- t_prev + median_period
    # trajectory interpolation around est_time
    window <- 0.1 # seconds before/after for interpolation
    seg <- foot_data[foot_data$time >= est_time - window & foot_data$time <= est_time + window, ]
    if (nrow(seg) < 4) {
        return(NULL)
    } # not enough samples
    # simple linear interpolation on each coordinate
    est_pos <- lapply(c("pos_x", "pos_z"), function(col) {
        approx(seg$time, seg[[col]], xout = est_time, rule = 2)$y
    })
    names(est_pos) <- c("pos_x", "pos_z")
    # confidence via vertical position closeness
    neigh <- foot_data[foot_data$time >= t_prev - median_period * WINDOW_SIZE &
        foot_data$time <= t_next + median_period * WINDOW_SIZE, ]
    neigh_z <- neigh[[CONF_HEIGHT_VAR]]
    conf <- ifelse(length(neigh_z) > 5, 1 - abs(est_pos[["pos_z"]] - median(neigh_z, na.rm = TRUE)) / (IQR(neigh_z, na.rm = TRUE) + 1e-6), NA)
    conf <- max(min(conf, 1), 0)
    data.frame(
        participant = participant, trialNum = trialNum, foot = foot, time = est_time,
        est_pos_x = est_pos$pos_x, est_pos_z = est_pos$pos_z, confidence = conf
    )
}

# Main collector
results <- list()

for (p in participants) {
    for (tr in allTrials) {
        hs <- load_clean_hs(p, tr)
        if (nrow(hs) < 4) next
        for (ft in unique(hs$foot)) {
            ft_hs <- hs %>%
                filter(foot == ft) %>%
                arrange(time)
            times <- ft_hs$time
            # rolling median period
            periods <- diff(times)
            med_periods <- zoo::rollmedian(periods, k = WINDOW_SIZE, fill = NA, align = "center")
            for (i in seq_along(periods)) {
                if (is.na(med_periods[i])) next
                if (periods[i] > GAP_FACTOR * med_periods[i]) {
                    t_prev <- times[i]
                    t_next <- times[i + 1]
                    # load trajectory data for this foot
                    data_name <- if (ft == "Left") "leftfoot" else "rightfoot"
                    foot_data <- get_preprocessed_data(p, tr, data_name)
                    if (is.null(foot_data) || nrow(foot_data) == 0) next
                    est <- estimate_step(p, tr, ft, t_prev, t_next, med_periods[i], foot_data)
                    if (!is.null(est)) results[[length(results) + 1]] <- est
                }
            }
        }
    }
}

missing_steps_df <- dplyr::bind_rows(results)

out_path <- file.path(dataExtraFolder, OUTPUT_FILE)
write.csv(missing_steps_df, out_path, row.names = FALSE)
cat("[INFO] Estimated", nrow(missing_steps_df), "missing heel strikes saved to", out_path, "\n")
