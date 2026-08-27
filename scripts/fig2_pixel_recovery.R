# ============================================================================
# Figure 2 — Pixel-scale recovery: classification vs spectral indices
# ============================================================================
# Percentage of transitioned conifer pixels classified as recovered each year
# post-fire, pooled across all megafires with 20+ years post-fire data.
# Recovery defined compositionally for the classification metric (dark green)
# and as a return to pre-fire values for NDVI, EVI, and NIRv.
#
# Inputs:
#   - Data/raster_df_mega/<fire_id>.csv     (pixel-level classifications)
#   - Data/NDVI_individual/<fire_id>_NDVI.tif
#   - Data/EVI_individual/<fire_id>_EVI.tif
#   - Data/NIRv_individual/<fire_id>_NIRv.tif
#   - Data/NBR_individual/<fire_id>_NBR.tif
#
# Output: Figures/fig2_pixel_recovery.{pdf,svg}
#
# NOTE: The recovery_pixels intermediate is expensive to build (loops through
#       every transitioned pixel of every 20-year fire, extracts index values
#       from raster stacks). It's cached to Data/figure_cache/recovery_pixels.csv
#       on first run — delete that file to force rebuild.
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Build or load cached recovery_pixels -----------------------------------
cache_path <- file.path(fig_cache_dir, "recovery_pixels.csv")

if (file.exists(cache_path)) {
  message("Loading cached recovery_pixels from ", cache_path)
  recovery_pixels <- fread(cache_path)
} else {
  message("Building recovery_pixels (this can take a while)...")

  recovery_pixels <- rbindlist(
    lapply(fires_20yrs_ids, function(id) {
      fire <- fread(here("Data", "raster_df_mega", paste0(id, ".csv")))
      sub  <- fire[RF_pre_veg == 7 & transitioned == 1]
      if (nrow(sub) == 0) return(NULL)

      pixels    <- sub[, .(x, y)]
      fire_year <- fire_metrics_mega$YEAR_[fire_metrics_mega$OBJECTID == id]
      post_cols <- paste0("RF_post", 1:20, "_veg")

      # Classification-based recovery: pixel reclassified as conifer (7)
      conifer_long <- rbindlist(lapply(1:20, function(y) {
        data.table(metric = "Conifer (classified)", year = y,
                   recovered = as.integer(sub[[post_cols[y]]] == 7))
      }))

      # Index-based recovery: annual value >= pre-fire value (year -1)
      index_long <- rbindlist(lapply(c("NDVI", "EVI", "NIRv", "NBR"), function(idx) {
        rpath <- here("Data", paste0(idx, "_individual"),
                      paste0(id, "_", idx, ".tif"))
        if (!file.exists(rpath)) return(NULL)
        r   <- rast(rpath)
        ext <- terra::extract(r, pixels)[, -1]
        yrs <- as.integer(gsub(".*_(\\d{4})$", "\\1", names(ext))) - fire_year
        pf  <- which(yrs == -1)
        if (length(pf) == 0) return(NULL)
        prefire_vals <- ext[[pf]]

        rbindlist(lapply(1:20, function(y) {
          col <- which(yrs == y)
          if (length(col) == 0) return(NULL)
          data.table(metric = idx, year = y,
                     recovered = as.integer(ext[[col]] >= prefire_vals))
        }))
      }))

      out <- rbind(conifer_long, index_long)
      out[, fire_id := id]
      out
    })
  )

  fwrite(recovery_pixels, cache_path)
  message("Cached recovery_pixels to ", cache_path)
}

# --- Palette and factor order for the five metrics ---------------------------
metric_levels <- c("Conifer (classified)", "NDVI", "EVI", "NIRv", "NBR")
metric_colors <- c(
  "Conifer (classified)" = "#0e4f12",
  "NDVI"                 = "#88CCEE",
  "EVI"                  = "#DDCC77",
  "NIRv"                 = "#CC6677",
  "NBR"                  = "#332288"
)

# --- Pooled proportion per metric per year ----------------------------------
recovery_pooled_prop <- recovery_pixels %>%
  group_by(metric, year) %>%
  summarise(prop_recovered = mean(recovered, na.rm = TRUE), .groups = "drop") %>%
  mutate(metric = factor(metric, levels = metric_levels))

# --- Plot -------------------------------------------------------------------
p_recovery_pooled <- recovery_pooled_prop %>%
  filter(!is.na(prop_recovered)) %>%
  ggplot(aes(x = year, y = prop_recovered, color = metric, fill = metric)) +
  geom_point(size = 3.5, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1, alpha = 0.12) +
  scale_color_manual(values = metric_colors, name = NULL) +
  scale_fill_manual(values = metric_colors, name = NULL) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(x = "Years since fire",
       y = "Percentage transitioned pixels recovered") +
  theme_classic(base_size = 12) +
  theme(axis.title = element_text(), legend.position = "top")

print(p_recovery_pooled)

# --- Save -------------------------------------------------------------------
save_fig(p_recovery_pooled, "fig2_pixel_recovery",
         width = 210, height = 148.5)
