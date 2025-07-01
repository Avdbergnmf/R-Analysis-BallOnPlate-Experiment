## ========================================================================== ##
## outlier_predictor.R   – train on two data sets, predict on a third         ##
## ========================================================================== ##
## Files *must* sit in the same folder as this script.                        ##
## -------------------------------------------------------------------------- ##
train_files <- c(
  "allGaitParams-2024-11-28.rds",
  "allGaitParams-2025-06-29_P125.rds",
  "allGaitParams-2025-06-29_P101.rds",
  "allGaitParams-2025-07-01_P124.rds"
)
new_files <- c("allGaitParams.rds") # add more if you like
## -------------------------------------------------------------------------- ##

# --------------------------- 1.  Packages ---------------------------------- #
required <- c("dplyr", "tidyr", "caret", "randomForest")
new_pkgs <- required[!required %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, quiet = TRUE)
lapply(required, library, character.only = TRUE)

# --------------------------- 2.  Harmoniser -------------------------------- #
standardise <- function(df) {
  df <- as.data.frame(df)

  pick_or_alt <- function(primary, alt) {
    if (primary %in% names(df) && !all(is.na(df[[primary]]))) {
      df[[primary]]
    } else if (alt %in% names(df)) {
      df[[alt]]
    } else {
      NA
    }
  }

  df$pos_x_std <- pick_or_alt("pos_x", "heelStrikes.pos_x")
  df$pos_z_std <- pick_or_alt("pos_z", "heelStrikes.pos_z")
  df$stepTimes <- pick_or_alt("stepTimes", "heelStrikes.stepTimes")
  df$stepLengths <- pick_or_alt("stepLengths", "heelStrikes.stepLengths")
  df$stepWidths <- pick_or_alt("stepWidths", "heelStrikes.stepWidths")
  df$outlier <- pick_or_alt("outlierSteps", "heelStrikes.outlierSteps")

  df %>%
    select(
      any_of(c(
        "participant", "trialNum", "time",
        "foot", "step"
      )),
      pos_x_std, pos_z_std,
      stepTimes, stepLengths, stepWidths,
      outlier
    ) %>%
    arrange(across(any_of(c("participant", "trialNum", "time"))))
}

# --------------------------- 3.  Neighbour features ------------------------ #
add_neighbours <- function(df, k = 3) {
  vars <- c(
    "pos_x_std", "pos_z_std",
    "stepTimes", "stepLengths", "stepWidths"
  )

  # Ensure proper ordering within each trial
  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("participant", "trialNum")))) %>%
    dplyr::arrange(time, .by_group = TRUE)

  # Neighbours irrespective of foot (sequence order)
  for (v in vars) {
    for (i in seq_len(k)) {
      df[[paste0(v, "_prev_any", i)]] <- dplyr::lag(df[[v]], i)
      df[[paste0(v, "_next_any", i)]] <- dplyr::lead(df[[v]], i)
    }
  }

  # Neighbours for the SAME foot
  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("participant", "trialNum", "foot")))) %>%
    dplyr::arrange(time, .by_group = TRUE)

  for (v in vars) {
    for (i in seq_len(k)) {
      df[[paste0(v, "_prev_same", i)]] <- dplyr::lag(df[[v]], i)
      df[[paste0(v, "_next_same", i)]] <- dplyr::lead(df[[v]], i)
    }
  }

  df %>% dplyr::ungroup()
}

# --------------------------- 4.  Load + prepare train data ----------------- #
train_df <- bind_rows(lapply(train_files, function(f) {
  message("[INFO] loading ", f)
  readRDS(f) |> standardise()
})) |>
  dplyr::filter(!(trialNum %in% c(1, 4, 6))) |>
  dplyr::filter(!is.na(outlier)) |>
  add_neighbours() |>
  tidyr::drop_na()

train_df$outlier <- factor(train_df$outlier)

# --------------------------- 5.  Train / hold-out test --------------------- #
set.seed(42)
ix <- caret::createDataPartition(train_df$outlier, p = 0.75, list = FALSE)
train <- train_df[ix, ]
test <- train_df[-ix, ]

feature_cols <- setdiff(
  names(train),
  c(
    "participant", "trialNum", "time",
    "foot", "step", "outlier"
  )
)

rf <- randomForest::randomForest(
  x = train[, feature_cols],
  y = train$outlier,
  ntree = 300,
  importance = TRUE
)

cat("\n==== INTERNAL VALIDATION (25 % hold-out) ====\n")
print(caret::confusionMatrix(predict(rf, test[, feature_cols]), test$outlier))

# --------------------------- 6.  Predict on new data ----------------------- #
predict_and_save <- function(file) {
  raw <- readRDS(file)
  std <- raw |>
    standardise() |>
    add_neighbours() |>
    drop_na()

  preds <- predict(rf, newdata = std[, feature_cols], type = "response")
  probs <- predict(rf, newdata = std[, feature_cols], type = "prob")[, "TRUE"]

  out <- raw[match(std$time, raw$time), ] # re-align rows after drop_na
  out$pred_outlier <- preds
  out$pred_outlier_prob <- probs

  rds_out <- sub("\\.rds$", "_withPred.rds", file)
  saveRDS(out, rds_out)
  message("[OK]  predictions saved to ", basename(rds_out))

  if (!all(is.na(std$outlier))) {
    cat("\nConfusion-matrix for ", basename(file), ":\n", sep = "")
    print(caret::confusionMatrix(preds, factor(std$outlier)))
  } else {
    cat("\nCounts for ", basename(file), " (no ground-truth present):\n", sep = "")
    print(table(preds))
  }
}

invisible(lapply(new_files, predict_and_save))
cat("\n==== DONE ====\n")
