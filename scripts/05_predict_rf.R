################################################################################
## This script uses the random forest classification model trained in
## scripts/04_train_rf_model.R to predict 20-year conifer recovery for
## stand-replacing conifer pixels in megafires that haven't yet reached
## 20 years of post-fire record ("younger" fires).
##
## Code by Johanna Schönecker
## 13th February 2025
##
## Inputs:
##   Data/rf_model_class_returned_reburns_plantings.rds (from 04_train_rf_model.R)
##   Data/raster_df_mega_repeated_planting/*.csv         (pixel-level fire data)
## Output:
##   Data/predicted_recovery.csv
################################################################################

source(here::here("scripts", "00_setup.R"))
pacman::p_load(ranger)

rf_model_class <- readRDS(here("Data", "rf_model_class_returned_reburns_plantings.rds"))

# Same predictors the model was trained on (stored on the ranger object itself,
# so this can never drift out of sync with 04_train_rf_model.R)
vars_to_keep <- rf_model_class$forest$independent.variable.names

# -------------------------------------------------------------------
# Step 1 - Identify younger fires (<20 yrs post-fire) and their files
# -------------------------------------------------------------------
younger_fire_IDs <- setdiff(megafire_IDs, fires_20yrs_ids)

younger_files <- list.files(here("Data", "raster_df_mega_repeated_planting"),
                            pattern = "\\.csv$", full.names = TRUE)
younger_files <- younger_files[sapply(younger_files, function(f) {
  fire_id <- as.numeric(str_extract(basename(f), "\\d+"))
  fire_id %in% younger_fire_IDs
})]

# Columns to read from each file (exclude years_since_fire - it's computed below)
needed_cols <- unique(c(
  setdiff(vars_to_keep, "years_since_fire"),
  "RF_pre_veg", "transitioned", "fire_year", "previous_fire_year",
  "reburn_20yrs", "tree_planting", "x", "y"
))

# -------------------------------------------------------------------
# Step 2 - Helper: predict in batches to avoid ranger memory blow-up
# -------------------------------------------------------------------
predict_in_batches <- function(model, newdata, batch = 100000, threads = 2) {
  n <- nrow(newdata)
  if (n == 0) return(numeric(0))
  out <- numeric(n)
  idx <- split(seq_len(n), ceiling(seq_len(n) / batch))
  for (i in idx) {
    pr <- predict(model, data = newdata[i, ], num.threads = threads)
    out[i] <- pr$predictions[, "1"]
  }
  out
}

# -------------------------------------------------------------------
# Step 3 - Process each fire one at a time: read, filter, predict
# -------------------------------------------------------------------
# Per-fire checkpointing: on a memory-constrained machine, predicting on
# every fire in one process can get killed partway through (some per-fire
# CSVs here are >1GB). Each fire's result is written out immediately and
# skipped on re-run if already present, so the script can simply be re-run
# after an interruption instead of losing all prior progress.
checkpoint_dir <- here("Data", "predicted_recovery_checkpoints")
dir.create(checkpoint_dir, showWarnings = FALSE)

for (f in younger_files) {

  fire_id <- as.numeric(str_extract(basename(f), "\\d+"))
  ckpt_file <- file.path(checkpoint_dir, paste0(fire_id, ".csv"))
  if (file.exists(ckpt_file)) next

  cat(sprintf("[%s] fire %d (%.1f MB) ...\n", format(Sys.time(), "%H:%M:%S"), fire_id, file.size(f) / 1e6))

  # Read only needed columns, then filter to stand-replacing conifer pixels
  d <- fread(f, select = needed_cols)
  d <- d[RF_pre_veg == 7 & transitioned == 1 &
           reburn_20yrs == 0 & tree_planting == 0]
  if (nrow(d) == 0) {
    fwrite(data.table(), ckpt_file)  # mark as done, no rows
    next
  }

  # Compute years_since_fire (as in training)
  d[, years_since_fire := fire_year - previous_fire_year]
  d[is.na(years_since_fire), years_since_fire := 100]

  # Predictor matrix, drop NA rows
  pred_input <- d[, ..vars_to_keep]
  ok <- complete.cases(pred_input)
  pred_input <- pred_input[ok]
  if (nrow(pred_input) == 0) {
    fwrite(data.table(), ckpt_file)
    rm(d, pred_input); gc()
    next
  }

  # Predict in batches
  prob_return <- predict_in_batches(rf_model_class, pred_input,
                                    batch = 10000, threads = 1)

  result <- cbind(
    data.table(fire_id = fire_id, x = d$x[ok], y = d$y[ok],
               prob_return = prob_return,
               pred_return = as.integer(prob_return > 0.5)),
    pred_input   # the predictor values for these pixels
  )

  fwrite(result, ckpt_file)
  rm(d, pred_input, result); gc()
}

# -------------------------------------------------------------------
# Step 3b - Combine per-fire checkpoints into the final output
# -------------------------------------------------------------------
younger_results <- rbindlist(
  lapply(list.files(checkpoint_dir, pattern = "\\.csv$", full.names = TRUE), fread),
  fill = TRUE
)

fwrite(younger_results, here("Data", "predicted_recovery.csv"))

# -------------------------------------------------------------------
# Step 4 - Summarise predicted recovery per fire (sanity check)
# -------------------------------------------------------------------
younger_summary <- younger_results %>%
  group_by(fire_id) %>%
  summarise(
    n_pixels          = n(),
    mean_prob_return  = mean(prob_return),
    pct_pred_returned = mean(pred_return) * 100,
    .groups = "drop"
  ) %>%
  left_join(fire_metrics_mega %>% dplyr::select(OBJECTID, FIRE_NAME, YEAR_),
            by = c("fire_id" = "OBJECTID"))

print(younger_summary, n = Inf)
