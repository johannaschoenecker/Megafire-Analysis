# ============================================================================
# Supplementary Figure — 2D ALE interaction: seed source distance x precipitation
# ============================================================================
# Second-order Accumulated Local Effects (ALE) for the two-way interaction
# between distance to the nearest live conifer seed source and post-fire
# precipitation, on P(returned to conifer within 20 years). Grid cells with
# fewer than 5 observations are masked, since ALE is unreliable where the
# training data is sparse - a companion density plot shows why.
#
# Inputs: Data/rf_model_class_returned_reburns_plantings.rds (from 04_train_rf_model.R)
#         Data/train_data.csv                                (from 04_train_rf_model.R)
# Output: Figures/supp_ale_interaction.{pdf,svg}
#         Figures/supp_ale_interaction_density.{pdf,svg}
# ============================================================================

source(here::here("scripts", "00_setup.R"))
pacman::p_load(ranger, iml)

rf_model_class <- readRDS(here("Data", "rf_model_class_returned_reburns_plantings.rds"))
train_data <- fread(here("Data", "train_data.csv"))
train_data$returned <- as.factor(train_data$returned)

set.seed(123)
samp <- train_data[sample(.N, 3000)]

predictor <- Predictor$new(
  model = rf_model_class,
  data  = samp[, setdiff(names(samp), "returned"), with = FALSE],
  y     = samp$returned,
  predict.function = function(model, newdata) {
    predict(model, data = newdata)$predictions[, "1"]
  }
)

# -------------------------------------------------------------------
# 2D ALE: distance to seed source x post-fire precipitation
# -------------------------------------------------------------------
ale_2d <- FeatureEffect$new(
  predictor,
  feature   = c("dist_unburned", "ppt_1post_abs"),
  method    = "ale",
  grid.size = 20
)
ale_2d_df <- ale_2d$results

obs_df <- samp %>% dplyr::select(dist_unburned, ppt_1post_abs)

# -------------------------------------------------------------------
# Mask ALE grid cells with too few observations to trust (<5 obs)
# -------------------------------------------------------------------
x_breaks <- sort(unique(ale_2d_df$dist_unburned))
y_breaks <- sort(unique(ale_2d_df$ppt_1post_abs))

obs_binned <- obs_df %>%
  mutate(
    xbin = x_breaks[findInterval(dist_unburned, x_breaks)],
    ybin = y_breaks[findInterval(ppt_1post_abs, y_breaks)]
  ) %>%
  count(xbin, ybin)

ale_masked <- ale_2d_df %>%
  left_join(obs_binned, by = c("dist_unburned" = "xbin", "ppt_1post_abs" = "ybin")) %>%
  mutate(.value_masked = ifelse(is.na(n) | n < 5, NA, .value))

p_ale_interaction <- ggplot(ale_masked, aes(x = dist_unburned, y = ppt_1post_abs, fill = .value_masked)) +
  geom_tile() +
  scale_fill_gradient2(low = "#A6611A", mid = "white", high = "#2C5F8A",
                       midpoint = 0, na.value = "grey90",
                       name = "ALE\n(effect on\nP(return))") +
  labs(x = "Distance to seed source (m)", y = "Post-fire precipitation (mm)",
      title = "Interaction: dispersal distance x post-fire precipitation") +
  theme_classic(base_size = 12) +
  theme(axis.title = element_text(face = "bold"))

print(p_ale_interaction)

save_fig(p_ale_interaction, "supp_ale_interaction", width = 180, height = 130)

# -------------------------------------------------------------------
# Companion diagnostic: where do observations actually fall? (justifies masking)
# -------------------------------------------------------------------
p_density <- ggplot(obs_df, aes(x = dist_unburned, y = ppt_1post_abs)) +
  geom_bin2d(bins = 20) +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "Obs. count") +
  labs(x = "Distance to seed source (m)", y = "Post-fire precipitation (mm)",
      title = "Data density (where ALE is reliable)") +
  theme_classic(base_size = 12) +
  theme(axis.title = element_text(face = "bold"))

print(p_density)

save_fig(p_density, "supp_ale_interaction_density", width = 180, height = 130)
