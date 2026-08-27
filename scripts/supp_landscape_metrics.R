# ============================================================================
# Supplementary Figure — Landscape metrics over time (per fire)
# ============================================================================
# Post-fire annual mean values of four landscape metrics for the stand-
# replacing burn patches that have failed to return to conifer forest in each
# year: AWMPS, ENN, shape index, and total core area. Each fire is one facet;
# metrics are rescaled 0-1 within each fire.
#
# Inputs: Data/all_burn_metrics.csv (from earlier landscape-metrics analysis)
# Output: Figures/supp_landscape_metrics.{pdf,svg}
#
# NOTE: The commented-out block near the top of this script recomputes
#       all_burn_metrics from the raster_df_mega CSVs — slow, only rerun
#       when the underlying pixel data has changed.
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# --- (Optional) Rebuild all_burn_metrics ------------------------------------
# Uncomment to recompute. Requires terra + landscapemetrics.
# NOTE: original code calculates area/enn/shape only. If total core area is
# needed (as used below), add "core" to the metric vector in calculate_lsm().

# get_burn_patch_metrics <- function(df, year_col, fire_id, year) {
#   if (year == 0) {
#     df_year <- df[, .(x, y, veg = as.integer(RF_pre_veg == 7 & transitioned == 1))]
#   } else {
#     df_year <- df[, .(x, y, veg = as.integer(RF_pre_veg == 7 & transitioned == 1 &
#                                                get(year_col) != 7))]
#   }
#   r <- rast(nrows = length(unique(df_year$y)),
#             ncols = length(unique(df_year$x)),
#             xmin = min(df_year$x), xmax = max(df_year$x),
#             ymin = min(df_year$y), ymax = max(df_year$y),
#             crs  = "EPSG:3310")
#   r <- rasterize(as.matrix(df_year[, .(x, y)]), r, values = df_year$veg)
#
#   calculate_lsm(r, level = "patch",
#                 metric = c("area", "enn", "shape", "core")) %>%
#     filter(class == 1) %>%
#     mutate(fire_id = fire_id, year_post_fire = year)
# }
#
# all_burn_metrics <- rbindlist(lapply(fires_20yrs_ids, function(id) {
#   fire  <- fread(here("Data", "raster_df_mega", paste0(id, ".csv")))
#   n_yrs <- max(fire$n_yrs_post, na.rm = TRUE)
#   pre   <- get_burn_patch_metrics(fire, "RF_pre_veg", id, 0)
#   post  <- rbindlist(lapply(seq_len(n_yrs), function(yr) {
#     get_burn_patch_metrics(fire, paste0("RF_post", yr, "_veg"), id, yr)
#   }))
#   rbind(pre, post)
# })) %>% mutate(fire_id = as.factor(fire_id))
#
# fwrite(all_burn_metrics, here("Data", "all_burn_metrics.csv"))

# --- Load precomputed landscape metrics -------------------------------------
all_burn_metrics <- fread(here("Data", "all_burn_metrics.csv"))

# --- AWMPS from area, then combine with the other metrics -------------------
burn_awmps <- all_burn_metrics %>%
  filter(metric == "area") %>%
  group_by(fire_id, year_post_fire) %>%
  summarise(value = sum(value^2) / sum(value), .groups = "drop") %>%
  mutate(metric = "awmps")

all_metrics_combined <- all_burn_metrics %>%
  group_by(fire_id, year_post_fire, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  bind_rows(burn_awmps) %>%
  filter(metric %in% c("awmps", "enn", "shape")) %>%
  mutate(
    metric  = factor(metric, levels = c("awmps", "enn", "shape")),
    fire_id = as.integer(as.character(fire_id))
  ) %>%
  left_join(fire_name_lookup %>% dplyr::select(OBJECTID, fire_label),
            by = c("fire_id" = "OBJECTID")) %>%
  mutate(fire_id = as.factor(fire_id))

# --- Total core area per fire per year --------------------------------------
# NOTE: this expects `core` in all_burn_metrics. If your existing
# all_burn_metrics.csv only contains area/enn/shape, rerun the block above
# with "core" added to the metric vector.
burn_core <- all_burn_metrics %>%
  filter(metric == "core" &
           fire_id %in% as.factor(fires_20yrs_ids) &
           year_post_fire <= 20) %>%
  group_by(fire_id, year_post_fire) %>%
  summarise(total_core = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(fire_id = as.integer(as.character(fire_id))) %>%
  left_join(fire_name_lookup %>% dplyr::select(OBJECTID, fire_label),
            by = c("fire_id" = "OBJECTID")) %>%
  mutate(fire_id = as.factor(fire_id))

# --- Rescale each metric 0-1 within fire for shared display -----------------
core_long <- burn_core %>%
  filter(year_post_fire <= 20) %>%
  dplyr::select(fire_id, fire_label, year_post_fire, value = total_core) %>%
  mutate(metric = "core")

metrics_rescaled <- all_metrics_combined %>%
  filter(metric %in% c("awmps", "enn", "shape")) %>%
  dplyr::select(fire_id, fire_label, year_post_fire, metric, value) %>%
  bind_rows(core_long) %>%
  group_by(fire_id, fire_label, metric) %>%
  mutate(
    value_scaled = (value - min(value, na.rm = TRUE)) /
                   (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(metric = factor(metric, levels = c("awmps", "enn", "shape", "core"))) %>%
  filter(fire_id %in% as.factor(fires_20yrs_ids))

# --- Palette and labels -----------------------------------------------------
metric_colors <- setNames(viridisLite::viridis(4, option = "viridis", end = 0.9),
                          c("awmps", "enn", "shape", "core"))

key_metric_labels <- c(
  awmps = "AWMPS",
  enn   = "ENN",
  shape = "Shape index",
  core  = "Total core area"
)

# --- Plot -------------------------------------------------------------------
p_combined <- metrics_rescaled %>%
  ggplot(aes(x = year_post_fire, y = value_scaled, color = metric)) +
  geom_vline(xintercept = 0.5, linetype = "dotted", color = "grey60") +
  geom_point(size = 0.8, alpha = 0.4) +
  geom_point(
    data = . %>% filter(year_post_fire == 0),
    size = 2, shape = 18
  ) +
  geom_smooth(
    data = . %>% filter(year_post_fire > 0),
    method = "loess", se = FALSE, linewidth = 1.2
  ) +
  scale_color_manual(values = metric_colors,
                     labels = key_metric_labels, name = NULL) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  coord_cartesian(xlim = c(0, 20)) +
  facet_wrap(~ fire_label, ncol = 2, scales = "free_x") +
  labs(x = "Years post-fire",
       y = "Metric value (scaled 0-1 within fire)") +
  theme_classic(base_size = 11) +
  theme(
    axis.title       = element_text(face = "bold"),
    strip.text       = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.position  = "bottom",
    panel.spacing    = unit(0.6, "lines")
  )

print(p_combined)

# --- Save -------------------------------------------------------------------
save_fig(p_combined, "supp_landscape_metrics",
         width = 210, height = 297)  # A4 portrait
