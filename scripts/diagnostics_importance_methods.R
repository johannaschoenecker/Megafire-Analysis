# ============================================================================
# Diagnostic — Do impurity and permutation importance agree?
# ============================================================================
# Not a manuscript figure: a model-validation sanity check that the feature
# ranking used throughout (impurity importance, from 04_train_rf_model.R)
# isn't an artifact of that particular importance metric. Trains two RF
# models identical to the main one except for the importance mode, then
# compares the resulting rankings.
#
# Input: Data/train_data.csv (from 04_train_rf_model.R)
# Output: printed comparison table + two console plots (not saved to Figures/)
# ============================================================================

source(here::here("scripts", "00_setup.R"))
pacman::p_load(ranger, ggrepel)

train_data <- fread(here("Data", "train_data.csv"))
train_data$returned <- as.factor(train_data$returned)

class_counts  <- table(train_data$returned)
class_weights <- 1 / class_counts[train_data$returned]

set.seed(123)
rf_perm <- ranger(returned ~ ., data = train_data, splitrule = "extratrees",
                  num.trees = 1000, mtry = 7, min.node.size = 7,
                  importance = "permutation", probability = TRUE,
                  num.threads = parallel::detectCores() - 1,
                  case.weights = class_weights)

set.seed(123)
rf_imp <- ranger(returned ~ ., data = train_data, splitrule = "extratrees",
                 num.trees = 1000, mtry = 7, min.node.size = 7,
                 importance = "impurity", probability = TRUE,
                 num.threads = parallel::detectCores() - 1,
                 case.weights = class_weights)

# -------------------------------------------------------------------
# Comparison table: relative importance (%) + rank for each method
# -------------------------------------------------------------------
imp_compare <- tibble(
  feature     = names(rf_perm$variable.importance),
  permutation = rf_perm$variable.importance,
  impurity    = rf_imp$variable.importance
) %>%
  mutate(
    permutation_rel  = permutation / sum(permutation) * 100,
    impurity_rel     = impurity / sum(impurity) * 100,
    permutation_rank = rank(-permutation),
    impurity_rank    = rank(-impurity)
  ) %>%
  arrange(permutation_rank)

print(imp_compare)

# Rank stability across the two methods
imp_compare %>%
  dplyr::select(feature, permutation_rank, impurity_rank) %>%
  pivot_longer(-feature, names_to = "method", values_to = "rank") %>%
  mutate(method = gsub("_rank", "", method)) %>%
  ggplot(aes(x = method, y = rank, group = feature, color = feature)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_y_reverse(breaks = 1:nrow(imp_compare)) +
  geom_text(data = . %>% filter(method == "permutation"),
           aes(label = feature), hjust = 1.1, size = 3) +
  labs(x = NULL, y = "Importance rank", title = "Rank stability across importance methods") +
  theme_classic(base_size = 12) +
  theme(legend.position = "none")

# Do the two measures agree in magnitude, not just rank?
ggplot(imp_compare, aes(x = impurity_rel, y = permutation_rel)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(size = 2.5, color = "#0e4f12") +
  geom_text_repel(aes(label = feature), size = 3) +
  labs(x = "Impurity importance (%)", y = "Permutation importance (%)",
      title = "Do the two importance measures agree?") +
  theme_classic(base_size = 12)
