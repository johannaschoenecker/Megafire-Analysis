################################################################################
## This script selects predictors (via correlation-based clustering), trains,
## and evaluates a weighted random forest classification model distinguishing
## severely burned conifer pixels that return to conifer within 20 years
## post-fire from those that do not.
##
## Younger-fire prediction (using the model trained here) lives in
## scripts/05_predict_rf.R. Variable importance / ALE plots live in
## scripts/fig5_ale_importance.R, scripts/supp_variable_importance.R, and
## scripts/supp_ale_interaction.R.
##
## Code by Johanna Schönecker
## 27th March 2025
################################################################################

source(here::here("scripts", "00_setup.R"))
pacman::p_load(ranger, caret, pROC)

# -------------------------------------------------------------------
# Step 1 - Load and filter pixel data for older fires (>=20 yr record)
# -------------------------------------------------------------------
older_files <- list.files(here("Data", "raster_df_mega_repeated_planting"),
                          pattern = "\\.csv$", full.names = TRUE)
older_files <- older_files[sapply(older_files, function(f) {
  fire_id <- as.numeric(str_extract(basename(f), "\\d+"))
  fire_id %in% fires_20yrs_ids
})]

data <- bind_rows(lapply(older_files, fread)) %>%
  filter(RF_pre_veg == 7, transitioned == 1, reburn_20yrs == 0, tree_planting == 0) %>%
  mutate(
    years_since_fire = fire_year - previous_fire_year,
    years_since_fire = ifelse(is.na(years_since_fire), 100, years_since_fire)
  )

# >20yr to return counts as "not returned"
data$returned[!is.na(data$yrs_to_return) & data$yrs_to_return > 20] <- 0

data <- data %>%
  dplyr::select(returned, dist_unburned, elevation, slope, aspect, tri, tpi,
                ppt_1post, tdmean_1post, tmean_1post, vpdmin_1post, tmax_1post,
                vpdmax_1post, tmin_1post, ppt_1post_abs, tdmean_1post_abs,
                tmean_1post_abs, tmax_1post_abs, vpdmax_1post_abs,
                tmin_1post_abs, years_since_fire) %>%
  drop_na() %>%
  mutate(returned = as.factor(returned))

# -------------------------------------------------------------------
# Step 2 - Feature selection: cluster predictors by correlation, keep
# one representative per cluster (cutoff h = 0.2 on 1 - |correlation|)
# -------------------------------------------------------------------
predictor_data   <- as.data.table(data)
correlation_data <- predictor_data[, !"returned", with = FALSE]

cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs")
cor_dist   <- as.dist(1 - abs(cor_matrix))
clust      <- hclust(cor_dist, method = "average")

plot(clust, main = "Variable clustering by absolute correlation", cex = 0.7)
abline(h = 0.2, col = "red", lty = 2)

# Representatives chosen from each correlation cluster (h = 0.2 cutoff above)
vars_to_keep <- c("dist_unburned", "elevation", "slope", "aspect", "tpi",
                  "ppt_1post", "tdmean_1post", "tmean_1post", "vpdmin_1post",
                  "vpdmax_1post", "tmin_1post", "ppt_1post_abs",
                  "tmean_1post_abs", "vpdmax_1post_abs", "years_since_fire")

reduced_data <- cbind(correlation_data[, ..vars_to_keep], predictor_data[, "returned"])

# -------------------------------------------------------------------
# Step 3 - Train/test split and class weights (for the returned imbalance)
# -------------------------------------------------------------------
set.seed(123)
train_index <- createDataPartition(reduced_data$returned, p = 0.7, list = FALSE)
train_data  <- reduced_data[train_index, ]
test_data   <- reduced_data[-train_index, ]

fwrite(train_data, here("Data", "train_data.csv"))

class_counts  <- table(train_data$returned)
class_weights <- 1 / class_counts[train_data$returned]

# -------------------------------------------------------------------
# Step 4 - Train the weighted random forest
# -------------------------------------------------------------------
rf_model_class <- ranger(
  formula       = returned ~ .,
  data          = train_data,
  splitrule     = "extratrees",
  num.trees     = 500,
  mtry          = 7,
  min.node.size = 7,
  importance    = "impurity",
  probability   = TRUE,
  num.threads   = parallel::detectCores() - 1,
  case.weights  = class_weights
)

print(rf_model_class)

# -------------------------------------------------------------------
# Step 5 - Evaluate on the held-out test set
# -------------------------------------------------------------------
pred          <- predict(rf_model_class, data = test_data)
prob_returned <- pred$predictions[, "1"]

roc_obj <- roc(test_data$returned, prob_returned)
cat("Test AUC:", auc(roc_obj), "\n")
plot(roc_obj, main = "ROC curve")

pred_class <- factor(ifelse(prob_returned > 0.5, "1", "0"), levels = c("0", "1"))
print(confusionMatrix(pred_class, test_data$returned, positive = "1"))

# -------------------------------------------------------------------
# Step 6 - Save the trained model
# -------------------------------------------------------------------
saveRDS(rf_model_class, here("Data", "rf_model_class_returned_reburns_plantings.rds"))
