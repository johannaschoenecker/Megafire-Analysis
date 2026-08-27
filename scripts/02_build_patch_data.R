# ===================================================================
# PATCH-LEVEL RECOVERY ANALYSIS
# Builds `all_patch_summary`: one row per stand-replacing burn patch,
# with size, interior-ness, recovery outcome, and conversion class.
# ===================================================================

library(data.table)
library(terra)
library(landscapemetrics)
library(dplyr)
library(here)

# -------------------------------------------------------------------
# Setup (adjust paths / IDs to your session)
# -------------------------------------------------------------------
fire_metrics_mega <- fread(here("Data", "SN_L4_ecoregion_mega_1985_2023.csv"))
fires_20yrs_ids   <- fire_metrics_mega$OBJECTID[fire_metrics_mega$YEAR_ < 2004]
post_cols         <- paste0("RF_post", 1:20, "_veg")

# -------------------------------------------------------------------
# Per-patch summary function
# -------------------------------------------------------------------
summarise_patch <- function(patch_id, patches_rast, fire_dt, post_cols) {
  
  target <- ifel(patches_rast == patch_id, 1, NA)
  n_pix  <- global(target, "sum", na.rm = TRUE)[1, 1]
  if (is.na(n_pix) || n_pix < 5) return(NULL)     # skip tiny patches
  
  # Distance from each within-patch pixel to the patch edge.
  # terra::distance() fills NA cells with distance to nearest non-NA cell,
  # so we invert: inside = NA, outside = 1, then mask back to the patch.
  outside <- ifel(is.na(target), 1, NA)
  edge_d  <- mask(distance(outside), target)
  
  pxy <- as.data.frame(edge_d, xy = TRUE, na.rm = TRUE)
  setnames(setDT(pxy), c("x", "y", "edge_dist"))
  
  pp <- merge(pxy, fire_dt[, c("x", "y", post_cols), with = FALSE], by = c("x", "y"))
  if (nrow(pp) == 0) return(NULL)
  
  pl <- data.table::melt(pp, id.vars = c("x", "y", "edge_dist"),
             measure.vars = post_cols,
             variable.name = "year", value.name = "veg_class")
  pl[, year := as.integer(gsub("RF_post(\\d+)_veg", "\\1", year))]
  
  # First year each pixel returns to conifer (NA = never within 20 yr)
  ry    <- pl[veg_class == 7, .(return_year = min(year)), by = .(x, y, edge_dist)]
  allpx <- unique(pl[, .(x, y, edge_dist)])
  ry    <- merge(allpx, ry, by = c("x", "y", "edge_dist"), all.x = TRUE)
  
  # Year-20 composition
  yr20 <- pl[year == 20 & !is.na(veg_class)]
  dominant_all        <- yr20[, .N, by = veg_class][order(-N)][1, veg_class]
  dominant_nonconifer <- yr20[veg_class != 7, .N, by = veg_class][order(-N)][1, veg_class]
  
  data.table(
    patch_id               = patch_id,
    n_pixels               = n_pix,
    area_ha                = n_pix * 0.09,
    mean_edge_dist         = mean(ry$edge_dist, na.rm = TRUE),
    max_edge_dist          = max(ry$edge_dist,  na.rm = TRUE),
    pct_returned_20        = mean(!is.na(ry$return_year)) * 100,
    median_return_yr       = median(ry$return_year, na.rm = TRUE),
    # Within-patch correlation of edge distance with return year
    # (positive = conifer returns from the edge inward)
    edge_return_cor        = if (sum(!is.na(ry$return_year)) > 5)
      cor(ry$edge_dist, ry$return_year, use = "complete.obs")
    else NA_real_,
    dominant_class_20      = as.character(dominant_all),
    dominant_nonconifer_20 = as.character(dominant_nonconifer)
  )
}

# -------------------------------------------------------------------
# Run across all 20+ year fires
# -------------------------------------------------------------------
all_patch_summary <- rbindlist(
  lapply(fires_20yrs_ids, function(id) {
    
    message("Processing fire ", id)
    fire <- fread(here("Data", "raster_df_mega", paste0(id, ".csv")))
    
    # Year-0 stand-replacing footprint, built from xyz so the grid aligns
    df0 <- fire[, .(x, y, veg = as.integer(RF_pre_veg == 7 & transitioned == 1))]
    if (sum(df0$veg, na.rm = TRUE) == 0) return(NULL)
    
    r0        <- rast(df0, type = "xyz", crs = "EPSG:3310")
    patches   <- get_patches(r0, class = 1)[[1]][[1]]
    patch_ids <- freq(patches)$value
    
    fire_summary <- rbindlist(
      lapply(patch_ids, summarise_patch,
             patches_rast = patches, fire_dt = fire, post_cols = post_cols),
      fill = TRUE
    )
    if (nrow(fire_summary) == 0) return(NULL)
    
    fire_summary[, fire_id := id]
    fire_summary
  }),
  fill = TRUE
)

# Add fire names
all_patch_summary <- all_patch_summary %>%
  left_join(fire_metrics_mega %>% dplyr::select(OBJECTID, FIRE_NAME, YEAR_),
            by = c("fire_id" = "OBJECTID")) %>%
  mutate(fire_label = paste0(FIRE_NAME, " (", YEAR_, ")"))

# -------------------------------------------------------------------
# SAVE IT so you don't have to rebuild again
# -------------------------------------------------------------------
saveRDS(all_patch_summary, here("Data", "all_patch_summary.rds"))
fwrite(all_patch_summary,  here("Data", "all_patch_summary.csv"))

# Quick sanity checks
nrow(all_patch_summary)
summary(all_patch_summary$area_ha)
summary(all_patch_summary$pct_returned_20)
table(all_patch_summary$dominant_class_20)



all_patch_summary <- fread(here("Data", "all_patch_summary.csv"))
