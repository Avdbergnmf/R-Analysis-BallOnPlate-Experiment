## ========================================================================== ##
## combine_outlier_union.R - Merge parameter-based and trajectory-based      ##
## outlier detections, removing duplicates within a time tolerance.          ##
## ========================================================================== ##

# ============================================================================ #
#                              CONFIGURATION                                  #
# ============================================================================ #

TIME_TOLERANCE <- 0.01 # seconds – two detections closer than this are duplicates
OUTPUT_PREFIX <- "combined_unique_outliers" # prefix for CSV & report

# ============================================================================ #
#                          RUNTIME DIRECTORY SET-UP                           #
# ============================================================================ #

cat("==============================\n")
cat("COMBINE OUTLIERS (UNION)\n")
cat("==============================\n")
cat("Merging parameter-based and trajectory-based detections\n")
cat("Duplicate tolerance: ±", TIME_TOLERANCE, " s\n\n")

if (file.exists("data_extra")) {
    data_extra_dir <- "data_extra"
} else if (file.exists(file.path("runtime", "data_extra"))) {
    data_extra_dir <- file.path("runtime", "data_extra")
} else if (file.exists(file.path("..", "data_extra"))) {
    data_extra_dir <- file.path("..", "data_extra")
} else {
    stop("Cannot locate data_extra directory.")
}

# ============================================================================ #
#                              HELPER FUNCTIONS                                #
# ============================================================================ #

suppressPackageStartupMessages({
    if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", quiet = TRUE)
    library(dplyr)
})

most_recent <- function(pattern) {
    # list all CSV files first, then filter with grepl (PCRE enabled)
    files <- list.files(data_extra_dir, pattern = "\\.csv$", full.names = TRUE)
    files <- files[grepl(pattern, basename(files), perl = TRUE)]
    if (length(files) == 0) {
        return(NULL)
    }
    files[which.max(file.mtime(files))]
}

read_csv_safe <- function(path) {
    if (is.null(path) || !file.exists(path)) {
        return(NULL)
    }
    df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
        return(NULL)
    }
    df %>%
        transmute(
            participant = as.character(participant),
            trialNum = as.numeric(trialNum),
            time = as.numeric(time)
        )
}

# remove duplicates within tolerance per (participant, trialNum)
compress_duplicates <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
        return(df)
    }
    df <- df %>% arrange(participant, trialNum, time)
    out <- list()
    idx <- 1
    while (idx <= nrow(df)) {
        current <- df[idx, ]
        out[[length(out) + 1]] <- current
        # skip rows within tolerance
        skip <- which(df$participant == current$participant &
            df$trialNum == current$trialNum &
            abs(df$time - current$time) <= TIME_TOLERANCE)
        idx <- max(skip) + 1
    }
    bind_rows(out)
}

combine_unique <- function(param_df, traj_df) {
    combined <- bind_rows(param_df, traj_df)
    compress_duplicates(combined)
}

# ============================================================================ #
#                               MAIN LOGIC                                    #
# ============================================================================ #

param_fh <- read_csv_safe(most_recent("^(outliers_heelstrikes|false_heelstrikes)(?!.*traj).*\\.csv$"))
traj_fh <- read_csv_safe(most_recent("false_heelstrikes.*traj.*\\.csv$"))
param_st <- read_csv_safe(most_recent("^(outliers_steps|outliers)(?!.*traj).*\\.csv$"))
traj_st <- read_csv_safe(most_recent("outliers.*traj.*\\.csv$"))

combined_fh <- combine_unique(param_fh, traj_fh)
combined_st <- combine_unique(param_st, traj_st)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
if (!dir.exists(data_extra_dir)) dir.create(data_extra_dir, recursive = TRUE)

if (nrow(combined_fh) > 0) {
    fh_file <- file.path(data_extra_dir, paste0(OUTPUT_PREFIX, "_false_heelstrikes_", ts, ".csv"))
    write.csv(combined_fh, fh_file, row.names = FALSE)
    message("[INFO] Saved ", nrow(combined_fh), " combined false heel strikes to ", basename(fh_file))
}
if (nrow(combined_st) > 0) {
    st_file <- file.path(data_extra_dir, paste0(OUTPUT_PREFIX, "_step_outliers_", ts, ".csv"))
    write.csv(combined_st, st_file, row.names = FALSE)
    message("[INFO] Saved ", nrow(combined_st), " combined step outliers to ", basename(st_file))
}

cat("Done.\n")
