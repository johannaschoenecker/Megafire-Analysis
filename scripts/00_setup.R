# ============================================================================
# 00_setup.R — shared setup for all figure and analysis scripts
# ============================================================================
# Sources: packages, veg palette/labels, fire ID lookups, output paths
# Run at the top of every figure script via:  source(here::here("scripts", "00_setup.R"))
# ============================================================================

# --- Packages ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyr, raster, ggplot2, ggspatial, sp, sf, reshape,
  patchwork, Rcpp, scales, dplyr, ggpubr, landscapemetrics, landscapetools,
  here, stringr, moments, reshape2, ggeasy, vctrs, mgcv, networkD3,
  ggridges, cowplot, factoextra, gdistance, fs, data.table, terra,
  gridExtra, grid, car, biscale, e1071, devEMF, zoo, svglite, forcats
)

# --- Vegetation class palette and labels ------------------------------------
# Class codes match RF classification output
veg_colors <- c(
  '7' = '#0e4f12',  # Conifer
  '1' = '#956733',  # Shrub
  '4' = '#d8bf58',  # Open woodland
  '5' = '#6dcd2b',  # Herbaceous
  '8' = '#7b8d6a',  # Sagebrush
  '9' = '#e97451',  # Dense woodland
  '2' = '#a7b5a5',  # Bare rock
  '3' = '#0940ca',  # Water
  '6' = '#ffacc9'   # Bare soil
)

veg_labels <- c(
  '1' = "Shrub",
  '2' = "Bare rock",
  '3' = "Water",
  '4' = "Open woodland",
  '5' = "Herbaceous",
  '6' = "Bare soil",
  '7' = "Conifer",
  '8' = "Sagebrush",
  '9' = "Dense woodland"
)

# --- Fire-level metadata and ID lookups -------------------------------------
fire_metrics_mega <- fread(
  here("Data", "SN_L4_ecoregion_mega_1985_2023.csv"),
  header = TRUE
)

# All megafires (n = 46)
megafire_IDs    <- fire_metrics_mega$OBJECTID

# Fires with 20+ years of post-fire observations (burned before 2004)
# — used for the recovery analyses
fires_20yrs_ids <- fire_metrics_mega$OBJECTID[fire_metrics_mega$YEAR_ < 2004]

# Pretty fire labels for figure facets
fire_name_lookup <- fire_metrics_mega %>%
  dplyr::select(OBJECTID, FIRE_NAME, YEAR_) %>%
  mutate(fire_label = paste0(FIRE_NAME, " ", YEAR_))

# --- Output directory -------------------------------------------------------
# All figures land in Figures/ at the project root
fig_dir <- here("Figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

# Intermediate figure data cache (patch summaries, pixel recovery, etc.)
# Prevents recomputing expensive terra/landscapemetrics operations on rerun
fig_cache_dir <- here("Data", "figure_cache")
if (!dir.exists(fig_cache_dir)) dir.create(fig_cache_dir, recursive = TRUE)

# --- Helper: save a plot as PDF, SVG, and PNG --------------------------------
save_fig <- function(plot, name, width, height, units = "mm", dpi = 600) {
  ggsave(file.path(fig_dir, paste0(name, ".pdf")),
         plot = plot, width = width, height = height, units = units, device = "pdf")
  ggsave(file.path(fig_dir, paste0(name, ".svg")),
         plot = plot, width = width, height = height, units = units, device = "svg")
  ggsave(file.path(fig_dir, paste0(name, ".png")),
         plot = plot, width = width, height = height, units = units, device = "png", dpi = dpi)
  invisible(plot)
}
