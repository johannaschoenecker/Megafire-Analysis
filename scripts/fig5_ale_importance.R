# ============================================================================
# Figure 5 — ALE plots of the top 8 predictors of 20-year conifer recovery
# ============================================================================
# Accumulated Local Effects (ALE) plots of the top 8 predictors of the pixel-
# based 20-year conifer recovery random forest model. Each panel shows the
# ALE curve with a rug of observed values, and an embedded lollipop "slider"
# indicating the predictor's relative variable importance.
#
# Aspect is shown on a compass-direction axis (N/E/S/W) rather than raw
# degrees, and the caption spells out how to read aspect and topographic
# position index (TPI) - both are otherwise opaque without a map in hand.
#
# Inputs:
#   - Data/rf_model_class_returned_reburns_plantings.rds  (from 04_train_rf_model.R)
#   - Data/train_data.csv                                 (from 04_train_rf_model.R)
#
# Output: Figures/fig5_ale_importance.{pdf,svg}
# ============================================================================

source(here::here("scripts", "00_setup.R"))
pacman::p_load(ranger, iml, ggh4x)

# --- Load model and training data -------------------------------------------
rf_model_class <- readRDS(here("Data",
                               "rf_model_class_returned_reburns_plantings.rds"))
train_data <- fread(here("Data", "train_data.csv"))
train_data$returned <- as.factor(train_data$returned)

# --- Nice display labels for predictors -------------------------------------
feature_labels <- c(
  "slope"            = "Slope (°)",
  "elevation"        = "Elevation (m)",
  "tpi"              = "TPI",
  "aspect"           = "Aspect",
  "dist_unburned"    = "Distance to seed source",
  "ppt_1post"        = "Post-fire ppt anomaly",
  "vpdmin_1post"     = "Post-fire min VPD anomaly",
  "vpdmax_1post"     = "Post-fire max VPD anomaly",
  "tdmean_1post"     = "Post-fire mean dew point T anomaly",
  "ppt_1post_abs"    = "Post-fire ppt (mm)",
  "tmean_1post"      = "Post-fire mean T anomaly",
  "tmean_1post_abs"  = "Post-fire mean T",
  "vpdmax_1post_abs" = "Post-fire max VPD (hPa)",
  "tmin_1post"       = "Post-fire min T anomaly",
  "years_since_fire" = "Years since last fire"
)

# --- Wrap the ranger model for iml ------------------------------------------
# Subsample for ALE speed (2000-5000 rows is plenty)
set.seed(123)
samp <- train_data[sample(.N, 3000)]

predictor <- Predictor$new(
  model = rf_model_class,
  data  = samp[, setdiff(names(samp), "returned"), with = FALSE],
  y     = samp$returned,
  predict.function = function(model, newdata) {
    predict(model, data = newdata)$predictions[, "1"]   # P(returned)
  }
)

# --- Top 8 predictors by impurity importance --------------------------------
top8 <- names(sort(rf_model_class$variable.importance, decreasing = TRUE))[1:8]

# --- Compute ALE for each of the top 8 --------------------------------------
ale_list <- lapply(top8, function(v) {
  eff <- FeatureEffect$new(predictor, feature = v,
                           method = "ale", grid.size = 20)
  d <- eff$results
  data.frame(feature = v, feature_val = d[[v]], ale = d$.value)
})
ale_df <- do.call(rbind, ale_list)

# Rug of observed values
rug_df <- samp %>%
  dplyr::select(all_of(top8)) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "feature_val")

# --- Relative importance (top 8, as % of total across all vars) -------------
imp_df <- data.frame(
  feature    = names(rf_model_class$variable.importance),
  importance = rf_model_class$variable.importance
) %>%
  mutate(rel_importance = importance / sum(importance) * 100) %>%
  arrange(desc(rel_importance)) %>%
  slice(1:8)

# Order facets by importance (most important first)
feature_order <- as.character(imp_df$feature)

ale_df <- ale_df %>%
  filter(feature %in% feature_order) %>%
  mutate(feature = factor(feature, levels = feature_order))
rug_df <- rug_df %>%
  filter(feature %in% feature_order) %>%
  mutate(feature = factor(feature, levels = feature_order))

# --- Per-facet slider positions in the top of each panel --------------------
facet_ranges <- ale_df %>%
  group_by(feature) %>%
  summarise(
    xmin = min(feature_val), xmax = max(feature_val),
    ymin = min(ale),         ymax = max(ale),
    .groups = "drop"
  ) %>%
  left_join(imp_df %>% dplyr::select(feature, rel_importance), by = "feature") %>%
  mutate(
    feature = factor(feature, levels = feature_order),
    y_pos   = ymax + 0.18 * (ymax - ymin),
    x_start = xmin,
    x_track = xmin + 0.55 * (xmax - xmin),
    x_end   = xmin + (rel_importance / max(rel_importance)) * 0.55 * (xmax - xmin),
    lab     = paste0(round(rel_importance), "%")
  )

# --- Plot -------------------------------------------------------------------
ale_line_color <- "#0e4f12"    # conifer green, ties to the recovery/veg palette used elsewhere
importance_color <- "#C97B2E"  # warm amber, visually distinct "meta" indicator

p_rf_figure <- ggplot(ale_df, aes(x = feature_val, y = ale)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_line(color = ale_line_color, linewidth = 1) +
  geom_rug(data = rug_df, aes(x = feature_val), inherit.aes = FALSE,
           alpha = 0.2, length = unit(0.03, "npc"), color = "grey40") +
  # Faint background track (the "100%" reference)
  geom_segment(data = facet_ranges,
               aes(x = x_start, xend = x_track, y = y_pos, yend = y_pos),
               inherit.aes = FALSE, color = "grey85", linewidth = 2.5) +
  # Importance slider
  geom_segment(data = facet_ranges,
               aes(x = x_start, xend = x_end, y = y_pos, yend = y_pos),
               inherit.aes = FALSE, color = importance_color, linewidth = 2.5) +
  geom_point(data = facet_ranges,
             aes(x = x_end, y = y_pos),
             inherit.aes = FALSE, color = importance_color, size = 3.5) +
  geom_text(data = facet_ranges,
            aes(x = x_track, y = y_pos, label = lab),
            inherit.aes = FALSE, hjust = -0.2, vjust = 0.4,
            size = 3, color = importance_color, fontface = "bold") +
  facet_wrap(~ feature, scales = "free", ncol = 4,
             labeller = as_labeller(feature_labels)) +
  # Aspect gets a compass-direction axis instead of raw degrees; every other
  # panel keeps its own free, auto-chosen breaks
  facetted_pos_scales(
    x = list(
      feature == "aspect" ~ scale_x_continuous(
        breaks = c(0, 90, 180, 270, 360),
        labels = c("N", "E", "S", "W", "N")
      )
    )
  )

caption_text <- paste(
  str_wrap(paste("Aspect: compass direction the slope faces (0°/360° = north-facing,",
                "180° = south-facing; south-facing slopes receive more solar",
                "radiation and run hotter/drier)."), width = 105),
  str_wrap(paste("TPI (topographic position index): local elevation relative to the",
                "surrounding terrain (negative = valley/drainage bottoms;",
                "positive = ridges/convex terrain)."), width = 105),
  sep = "\n"
)

p_rf_figure <- p_rf_figure +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    x = "Predictor value",
    y = "ALE (effect on P(return))",
    caption = caption_text
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title        = element_text(),
    strip.text        = element_text(face = "bold", size = 9),
    strip.background  = element_rect(fill = "grey95", color = NA),
    panel.spacing     = unit(0.6, "lines"),
    plot.caption      = element_text(hjust = 0, color = "grey30", size = 8, lineheight = 1.3),
    plot.caption.position = "plot"
  )

print(p_rf_figure)

# --- Save -------------------------------------------------------------------
save_fig(p_rf_figure, "fig5_ale_importance",
         width = 220, height = 130)
