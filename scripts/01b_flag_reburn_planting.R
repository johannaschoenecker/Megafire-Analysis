# ============================================================================
# 01b_flag_reburn_planting.R
# ============================================================================
# Adds two exclusion flags to each fire's pixel-level dataframe:
#   - tree_planting : pixel falls inside a FACTS "Accomplished" tree-planting
#                      unit completed AFTER the fire (i.e. it was actively
#                      replanted, so its post-fire trajectory isn't natural
#                      regeneration).
#   - reburn_20yrs   : pixel burned again in a LATER fire within 20 years of
#                      the original fire (so its 20-yr recovery outcome is
#                      confounded by a second disturbance).
#
# scripts/04_train_rf_model.R, 05_predict_rf.R, 06_build_fire_table.R, and
# fig6_recovery_mirror.R all filter on these two columns, but the original
# script that produced them (and the shipped
# Data/raster_df_mega_repeated_planting/ folder) was lost. This is a
# best-effort reconstruction, written to reproduce that folder's logic as
# closely as possible from source data:
#   - Data/Shapefiles/FACTS_tree_planting_1986_2024_in_megafires.shp
#   - Data/Rasters/SN_fires_yearly_rasters_upto/fires_up_to_<year>.tif
#     (cumulative "most recent fire year up to and including <year>" raster,
#     already used elsewhere in the pipeline to derive previous_fire_year)
#
# VALIDATION: run against all 12 fires with 20-yr data and compared to the
# shipped Data/raster_df_mega_repeated_planting/ cache (the dataset that
# actually produced every number in the manuscript): 99.71% per-pixel
# agreement on tree_planting, 97.91% on reburn_20yrs (pooled across
# 2,969,984 pixels). The two fires burned in 2002 (MCNALLY, CANNON) show
# lower reburn_20yrs agreement (92-99%) because their 20-yr window extends
# to 2022, one year past the last available fires_up_to_2021.tif -- their
# reburn flag is truncated to a 19-yr window here.
#
# Because this reconstruction isn't a bit-for-bit match, it writes to a
# SEPARATE output folder rather than overwriting the validated
# raster_df_mega_repeated_planting/ that the manuscript results are built
# on. Use this script to extend the pipeline to new fires or to sanity-check
# the cached flags -- not to regenerate the numbers already reported.
#
# Output: one CSV per fire at
#   Data/raster_df_mega_repeated_planting_reconstructed/<OBJECTID>.csv
# ============================================================================

source(here::here("scripts", "00_setup.R"))

out_dir <- here("Data", "raster_df_mega_repeated_planting_reconstructed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

fire_year_lookup <- setNames(fire_metrics_mega$YEAR_, fire_metrics_mega$OBJECTID)

# --- Planting source: FACTS "Accomplished" units, any activity year -------
facts_all <- vect(here("Data", "Shapefiles",
                        "FACTS_tree_planting_1986_2024_in_megafires.shp"))
facts_all <- facts_all[facts_all$STAGE_DESC == "Accomplished", ]

# --- Cumulative fire-history raster years available ------------------------
fires_upto_dir <- here("Data", "Rasters", "SN_fires_yearly_rasters_upto")
avail_years <- as.integer(gsub(".*fires_up_to_(\\d+)\\.tif", "\\1",
                                list.files(fires_upto_dir, pattern = "\\.tif$")))

for (k in as.character(megafire_IDs)) {

  fire_year <- fire_year_lookup[[k]]
  in_path   <- here("Data", "raster_df_mega", paste0(k, ".csv"))
  if (is.null(fire_year) || is.na(fire_year) || !file.exists(in_path)) {
    message("Skipping fire ", k, " - no fire year or no raster_df_mega CSV.")
    next
  }

  message("Flagging fire ", k, " (", fire_year, ")")
  d <- fread(in_path)
  pts <- vect(d, geom = c("x", "y"), crs = "EPSG:3310")

  # --- tree_planting: FACTS unit completed after the fire ------------------
  facts_post   <- facts_all[facts_all$FY_COMPLET > fire_year, ]
  planted_hit  <- is.related(pts, facts_post, "intersects")
  d[, tree_planting := as.integer(planted_hit)]

  # --- reburn_20yrs: a later fire hit this pixel within 20 yrs --------------
  cutoff_year <- min(fire_year + 20, max(avail_years))
  if (cutoff_year < fire_year + 20) {
    message("  NOTE: reburn window truncated to ", cutoff_year,
            " (wanted ", fire_year + 20,
            ") - fires_up_to_ raster stack doesn't extend further.")
  }
  r <- rast(file.path(fires_upto_dir, paste0("fires_up_to_", cutoff_year, ".tif")))
  last_fire_by_cutoff <- terra::extract(r, pts)[, 2]
  d[, reburn_20yrs := as.integer(!is.na(last_fire_by_cutoff) &
                                    last_fire_by_cutoff > fire_year)]

  fwrite(d, file.path(out_dir, paste0(k, ".csv")))
}

message("Done. Wrote ", length(list.files(out_dir, pattern = "\\.csv$")),
        " files to ", out_dir)
