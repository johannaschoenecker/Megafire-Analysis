# ============================================================================
# Figure 6 — Observed vs. predicted conifer recovery by fire year (mirrored bars)
# ============================================================================
# Mirrored bar chart of stand-replacing conifer area returned/not-returned to
# conifer within 20 years, by fire year, combining:
#   - Observed outcomes for older fires (>=20 yrs of post-fire record)
#   - RF model predictions for younger fires (<20 yrs of post-fire record)
# The lower (negative) half re-uses the same bars to show the proportion not
# returned as a scatter + trend line, read off a secondary percentage axis.
#
# Inputs:
#   Data/predicted_recovery.csv                 (RF predictions for younger
#                                                fires; generated in
#                                                scripts/05_predict_rf.R)
#   Data/raster_df_mega_repeated_planting/*.csv (raw pixel data, for observed
#                                                outcomes on older fires)
# Output: Figures/fig6_recovery_mirror.{pdf,svg,png}
# ============================================================================

source(here::here("scripts", "00_setup.R"))
pacman::p_load(ggpattern)

# -------------------------------------------------------------------
# Step 1 - Predicted outcomes for younger fires (<20 yrs post-fire)
# -------------------------------------------------------------------
younger_results <- fread(here("Data", "predicted_recovery.csv"))

predicted_results <- younger_results %>%
  transmute(fire_id, returned = pred_return, source = "Predicted")

# -------------------------------------------------------------------
# Step 2 - Observed outcomes for older fires (full 20-yr post-fire record)
# -------------------------------------------------------------------
older_files <- list.files(here("Data", "raster_df_mega_repeated_planting"),
                          pattern = "\\.csv$", full.names = TRUE)
older_files <- older_files[sapply(older_files, function(f) {
  fire_id <- as.numeric(str_extract(basename(f), "\\d+"))
  fire_id %in% fires_20yrs_ids
})]

observed_results <- rbindlist(lapply(older_files, function(f) {
  fire_id <- as.numeric(str_extract(basename(f), "\\d+"))
  d <- fread(f, select = c("RF_pre_veg", "transitioned", "reburn_20yrs",
                           "tree_planting", "returned", "yrs_to_return"))
  d <- d[RF_pre_veg == 7 & transitioned == 1 &
           reburn_20yrs == 0 & tree_planting == 0]
  if (nrow(d) == 0) return(NULL)
  # >20yr to return counts as "not returned", matching the RF training rule
  d[!is.na(yrs_to_return) & yrs_to_return > 20, returned := 0]
  d <- d[!is.na(returned)]
  data.table(fire_id = fire_id,
             returned = as.integer(as.character(d$returned)),
             source = "Observed")
}), fill = TRUE)

# -------------------------------------------------------------------
# Step 3 - Combine, convert pixel counts to hectares (0.09 ha/pixel), by year
# -------------------------------------------------------------------
all_outcomes <- bind_rows(observed_results, predicted_results) %>%
  mutate(fire_id = as.integer(fire_id)) %>%
  left_join(fire_metrics_mega %>% dplyr::select(OBJECTID, FIRE_NAME, YEAR_),
            by = c("fire_id" = "OBJECTID"))

year_summary <- all_outcomes %>%
  group_by(YEAR_, source, returned) %>%
  summarise(n_pixels = n(), .groups = "drop") %>%
  mutate(
    ha = n_pixels * 0.09,
    outcome = ifelse(returned == 1, "Returned to conifer", "Not returned"),
    ha_mirror = ifelse(returned == 1, ha, -ha)
  )

# Full year range so empty years still show as slots
all_years <- seq(min(as.integer(as.character(year_summary$YEAR_))),
                 max(as.integer(as.character(year_summary$YEAR_))))

year_summary <- year_summary %>%
  mutate(YEAR_ = factor(as.integer(as.character(YEAR_)), levels = all_years))

# -------------------------------------------------------------------
# Step 4 - Proportion not returned per year, scaled onto the negative axis
# -------------------------------------------------------------------
y_max <- max(abs(year_summary$ha_mirror), na.rm = TRUE)

prop_notreturned <- year_summary %>%
  group_by(YEAR_) %>%
  summarise(
    ha_returned    = sum(ha[returned == 1]),
    ha_notreturned = sum(ha[returned == 0]),
    .groups = "drop"
  ) %>%
  mutate(
    prop_notreturned = ha_notreturned / (ha_returned + ha_notreturned),
    prop_scaled      = -prop_notreturned * y_max,   # negative half
    x_num            = match(as.character(YEAR_), as.character(all_years))
  ) %>%
  filter(!is.na(prop_notreturned))

# -------------------------------------------------------------------
# Step 5 - Plot: mirrored bars + proportion-not-returned scatter/trend
# -------------------------------------------------------------------
theme_friend <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.border     = element_rect(color = "grey40", fill = NA, linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey90", color = "grey40", linewidth = 0.5),
      strip.text       = element_text(size = base_size, margin = margin(3, 3, 3, 3)),
      axis.title       = element_text(size = base_size),
      axis.text        = element_text(size = base_size - 2, color = "grey30"),
      axis.ticks       = element_line(color = "grey40", linewidth = 0.3),
      panel.spacing    = unit(0.4, "lines"),
      legend.position  = "top",
      plot.caption     = element_text(color = "grey50", size = base_size - 3)
    )
}

p_year_mirror <- ggplot() +
  # Mirrored bars: positive = returned/not-returned area, striped by source
  geom_col_pattern(
    data = year_summary,
    aes(x = YEAR_, y = ha_mirror, fill = outcome, pattern = source),
    width = 0.8, color = "grey20", linewidth = 0.2,
    pattern_fill = "grey20", pattern_color = NA,
    pattern_density = 0.1, pattern_spacing = 0.02,
    pattern_key_scale_factor = 0.5
  ) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  # Proportion not-returned: lm trend + points, on the lower (negative) axis
  geom_smooth(
    data = prop_notreturned,
    aes(x = x_num, y = prop_scaled),
    method = "lm", se = TRUE,
    color = "grey20", linewidth = 0.6, inherit.aes = FALSE
  ) +
  geom_point(
    data = prop_notreturned,
    aes(x = x_num, y = prop_scaled),
    color = "grey20", fill = "white", shape = 21, size = 2.5, stroke = 0.8,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c("Returned to conifer" = "#0e4f12", "Not returned" = "#D2B48C"),
    name = NULL
  ) +
  scale_pattern_manual(
    values = c("Observed" = "none", "Predicted" = "stripe"),
    name = NULL
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = function(x) scales::comma(abs(x)),
    expand = expansion(mult = c(0.05, 0.08)),
    sec.axis = sec_axis(
      ~ -. / y_max,                       # invert scaling; negative half -> 0-100%
      name   = "Proportion not returned",
      labels = scales::percent_format()
    )
  ) +
  labs(
    x = "Fire year",
    y = "Stand-replacing conifer area (ha)"
  ) +
  theme_friend(base_size = 11) +
  theme(
    axis.text.x          = element_text(angle = 45, hjust = 1),
    legend.justification = c(0, 1),            # anchor legend's top-left corner
    legend.background    = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.key           = element_rect(fill = NA, color = NA),
    legend.margin        = margin(2, 4, 2, 4)
  ) +
  guides(
    fill    = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "grey85"))
  )

print(p_year_mirror)

save_fig(p_year_mirror, "fig6_recovery_mirror", width = 180, height = 110)

# ---------------------------------------------------------------------------
# Alternative 2-panel version: mirrored bars (a) + proportion scatter (b)
# ---------------------------------------------------------------------------
# Same underlying data, split into two panels instead of one mirrored plot
# with a secondary axis - top panel is just the observed/predicted bars,
# bottom panel is the proportion-not-returned trend on its own plain 0-100%
# axis. The bottom panel's x uses the same 1..n integer positions as the
# bar panel's discrete year axis (matched via `breaks`/`labels`), so the two
# panels line up when stacked.
p_bars <- ggplot(year_summary, aes(x = YEAR_, y = ha_mirror, fill = outcome, pattern = source)) +
  geom_col_pattern(
    width = 0.8, color = "grey20", linewidth = 0.2,
    pattern_fill = "grey20", pattern_color = NA,
    pattern_density = 0.1, pattern_spacing = 0.02,
    pattern_key_scale_factor = 0.5
  ) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.4) +
  scale_fill_manual(
    values = c("Returned to conifer" = "#0e4f12", "Not returned" = "#D2B48C"),
    name = NULL
  ) +
  scale_pattern_manual(
    values = c("Observed" = "none", "Predicted" = "stripe"),
    name = NULL
  ) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = function(x) scales::comma(abs(x)),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  labs(x = NULL, y = "Stand-replacing\nconifer area (ha)") +
  theme_friend(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  guides(
    fill    = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "grey85"))
  )

# Linear trend stats (fit only on years with actual fire data - see above)
trend_fit <- lm(prop_notreturned ~ x_num, data = prop_notreturned)
trend_r2  <- summary(trend_fit)$r.squared
trend_p   <- summary(trend_fit)$coefficients["x_num", "Pr(>|t|)"]
trend_label <- sprintf("R^2 == %.2f * ',' ~ italic(p) == %s",
                       trend_r2,
                       ifelse(trend_p < 0.001, "'<0.001'", sprintf("%.3f", trend_p)))

p_scatter <- ggplot(prop_notreturned, aes(x = x_num, y = prop_notreturned)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 0.6) +
  geom_point(color = "grey20", fill = "white", shape = 21, size = 2.5, stroke = 0.8) +
  annotate("text", x = -Inf, y = Inf, label = trend_label, parse = TRUE,
           hjust = -0.1, vjust = 1.5, size = 3.4, color = "grey20") +
  scale_x_continuous(
    breaks = seq_along(all_years), labels = all_years,
    expand = expansion(add = 0.6)   # matches the discrete expansion used in p_bars
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Fire year", y = "Proportion\nnot returned") +
  theme_friend(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

p_recovery_twopanel <- p_bars / p_scatter +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13))

print(p_recovery_twopanel)

save_fig(p_recovery_twopanel, "fig6_recovery_twopanel", width = 180, height = 150)
