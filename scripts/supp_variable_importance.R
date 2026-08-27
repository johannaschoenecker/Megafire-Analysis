# ============================================================================
# Supplementary Figure — Full variable importance (all predictors)
# ============================================================================
# Impurity-based variable importance for every predictor in the 20-year
# conifer recovery random forest model (fig5_ale_importance.R shows the ALE
# shapes for just the top 8; this shows the full ranking, colored by
# predictor category).
#
# Inputs: Data/rf_model_class_returned_reburns_plantings.rds (from 04_train_rf_model.R)
# Output: Figures/supp_variable_importance.{pdf,svg}
# ============================================================================

source(here::here("scripts", "00_setup.R"))
library(ranger)

rf_model_class <- readRDS(here("Data", "rf_model_class_returned_reburns_plantings.rds"))

feature_labels <- c(
  "slope"            = "Slope",
  "elevation"        = "Elevation",
  "tpi"              = "Topographic position index",
  "aspect"           = "Aspect",
  "dist_unburned"    = "Distance to seed source",
  "ppt_1post"        = "Post-fire ppt anomaly",
  "vpdmin_1post"     = "Post-fire min VPD anomaly",
  "vpdmax_1post"     = "Post-fire max VPD anomaly",
  "tdmean_1post"     = "Post-fire mean dew point T anomaly",
  "ppt_1post_abs"    = "Post-fire ppt",
  "tmean_1post"      = "Post-fire mean T anomaly",
  "tmean_1post_abs"  = "Post-fire mean T",
  "vpdmax_1post_abs" = "Post-fire max VPD",
  "tmin_1post"       = "Post-fire min T anomaly",
  "years_since_fire" = "Years since last fire"
)

importance_df <- data.frame(
  Feature    = names(rf_model_class$variable.importance),
  Importance = rf_model_class$variable.importance
) %>%
  mutate(Category = case_when(
    Feature %in% c("elevation", "slope", "aspect", "tpi", "tri", "dist_unburned") ~ "topography",
    Feature == "years_since_fire" ~ "fire history",
    TRUE ~ "climate"
  ))

p_importance <- ggplot(importance_df,
                       aes(x = reorder(Feature, Importance),
                           y = Importance, fill = Category)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = round(Importance, 1)),
           hjust = -0.2, size = 3, color = "grey30") +
  coord_flip() +
  scale_fill_manual(
    values = c("topography" = "#B08D6A", "fire history" = "#4A6741", "climate" = "#6B9EA8"),
    name = NULL
  ) +
  scale_x_discrete(labels = feature_labels) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Variable importance (impurity)") +
  theme_classic(base_size = 12) +
  theme(
    axis.text.y         = element_text(size = 10, color = "grey20"),
    axis.text.x         = element_text(size = 9),
    legend.position     = "top",
    legend.text         = element_text(size = 10),
    panel.grid.major.x  = element_line(color = "grey90", linewidth = 0.4),
    plot.margin         = margin(10, 20, 10, 10)
  )

print(p_importance)

save_fig(p_importance, "supp_variable_importance", width = 210, height = 148.5)
