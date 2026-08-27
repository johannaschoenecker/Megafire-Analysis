# ============================================================================
# 01_build_pixel_data.R
# ============================================================================
# Builds pixel-level dataframes for each megafire, combining:
#   - Topography (elevation, slope, aspect, TPI, TRI)
#   - Annual PRISM climate (fire year, 1yr pre, 1yr post — both anomalies and
#     absolute values)
#   - Vegetation classification (year before fire + up to 37 years post-fire)
#   - Distance to unburned conifer (nearest potential seed source)
#   - Previous fire history
#   - Derived recovery metrics: transitioned, returned, yrs_to_return, n_yrs_post
#
# Output: one CSV per fire at Data/raster_df_mega/<OBJECTID>.csv
#
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# Additional packages beyond 00_setup.R
library(foreach)
library(doParallel)
library(purrr)

# ---------------------------------------------------------------------------
# Fire year lookup: OBJECTID -> fire year
# ---------------------------------------------------------------------------
# megafire_IDs is defined by 00_setup.R from fire_metrics_mega$OBJECTID
fire_year_lookup <- setNames(fire_metrics_mega$YEAR_, fire_metrics_mega$OBJECTID)

# ---------------------------------------------------------------------------
# Constants and paths
# ---------------------------------------------------------------------------
n_post_years      <- 37   # max years of post-fire data to try to extract
veg_dir_root      <- here("Data", "Rasters", "veg_annual_individual")
dist_unburned_dir <- here("Data", "Rasters", "dist_unburned_conifer_7")
out_dir           <- here("Data", "raster_df_mega")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Load region-wide topography rasters (same for every fire)
# ---------------------------------------------------------------------------
elev_raster   <- raster(here("Data", "elevation.tif"))
aspect_raster <- raster(here("Data", "aspect.tif"))
slope_raster  <- raster(here("Data", "slope.tif"))
tpi_raster    <- raster(here("Data", "tpi.tif"))
tri_raster    <- raster(here("Data", "tri.tif"))

# ---------------------------------------------------------------------------
# Helper: build the sequence of veg-raster file paths for one fire
# ---------------------------------------------------------------------------
# Returns a named list: `pre` (year-1) and `post1` .. `postN` (years 1..N post).
veg_paths_for_fire <- function(k, fire_year, n_post = n_post_years) {
  years_post <- seq_len(n_post)
  post_paths <- setNames(
    file.path(veg_dir_root, k,
              paste0(fire_year + years_post, "_Mega_", k, ".tif")),
    paste0("post", years_post)
  )
  c(
    list(pre = file.path(veg_dir_root, k,
                         paste0(fire_year - 1, "_Mega_", k, ".tif"))),
    as.list(post_paths)
  )
}

# ---------------------------------------------------------------------------
# Helper: extract veg values from every existing veg raster at a set of points
# ---------------------------------------------------------------------------
# Returns a data.table with columns RF_pre_veg, RF_post1_veg, ..., RF_postN_veg.
# Missing rasters -> NA column (so column layout is consistent across fires).
extract_veg_series <- function(sp_points, veg_paths) {
  out <- vector("list", length(veg_paths))
  names(out) <- names(veg_paths)
  
  for (nm in names(veg_paths)) {
    p <- veg_paths[[nm]]
    if (file.exists(p)) {
      out[[nm]] <- raster::extract(raster(p), sp_points)
    } else {
      warning("Missing veg raster: ", p, " - filling with NA.")
      out[[nm]] <- rep(NA_real_, length(sp_points))
    }
  }
  
  new_names <- ifelse(names(out) == "pre",
                      "RF_pre_veg",
                      paste0("RF_", names(out), "_veg"))
  setNames(as.data.table(out), new_names)
}

# ---------------------------------------------------------------------------
# Main loop: one CSV per megafire
# ---------------------------------------------------------------------------
for (k in as.character(megafire_IDs)) {
  
  message("Processing fire ", k)
  
  fire_year <- fire_year_lookup[[k]]
  if (is.null(fire_year) || is.na(fire_year)) {
    warning("No fire year for OBJECTID ", k, " - skipping.")
    next
  }
  
  # -------------------------------------------------------------------
  # Load per-fire rasters
  # -------------------------------------------------------------------
  prism_fire      <- stack(here("Data", "Rasters", "3310_PRISM_anomalies",
                                paste0("prism_anomaly_", fire_year, ".tif")))
  prism_1pre      <- stack(here("Data", "Rasters", "3310_PRISM_anomalies",
                                paste0("prism_anomaly_", fire_year - 1, ".tif")))
  prism_1post     <- stack(here("Data", "Rasters", "3310_PRISM_anomalies",
                                paste0("prism_anomaly_", fire_year + 1, ".tif")))
  prism_fire_abs  <- stack(here("Data", "Rasters", "3310_PRISM_abs",
                                paste0("prism_annual_", fire_year, ".tif")))
  prism_1pre_abs  <- stack(here("Data", "Rasters", "3310_PRISM_abs",
                                paste0("prism_annual_", fire_year - 1, ".tif")))
  prism_1post_abs <- stack(here("Data", "Rasters", "3310_PRISM_abs",
                                paste0("prism_annual_", fire_year + 1, ".tif")))
  
  last_fire_raster <- raster(here("Data", "Rasters", "SN_fires_yearly_rasters_upto",
                                  paste0("fires_up_to_", fire_year - 1, ".tif")))
  
  # Veg raster paths (pre + N post)
  veg_paths <- veg_paths_for_fire(k, fire_year)
  
  # Use the pre-fire veg raster as the pixel grid
  if (!file.exists(veg_paths$pre)) {
    warning("No pre-fire veg raster for fire ", k, " - skipping.")
    next
  }
  veg_raster_pre <- raster(veg_paths$pre)
  
  # -------------------------------------------------------------------
  # Points: one per non-NA pixel of the pre-fire veg raster
  # -------------------------------------------------------------------
  pts_df    <- as.data.frame(rasterToPoints(veg_raster_pre))
  coords_df <- pts_df[, 1:2]
  sp_points <- SpatialPointsDataFrame(
    coords = coords_df,
    data   = data.frame(pixel_value = pts_df[, 3])
  )
  
  # -------------------------------------------------------------------
  # Extract all variables at each pixel
  # -------------------------------------------------------------------
  ex <- data.table(
    x = coords_df[[1]],
    y = coords_df[[2]],
    elevation = raster::extract(elev_raster,   sp_points),
    aspect    = raster::extract(aspect_raster, sp_points),
    slope     = raster::extract(slope_raster,  sp_points),
    tri       = raster::extract(tri_raster,    sp_points),
    tpi       = raster::extract(tpi_raster,    sp_points),
    previous_fire_year = raster::extract(last_fire_raster, sp_points)
  )
  
  # PRISM anomalies + absolutes (7 bands each: tmean, tmax, tmin, tdmean,
  # vpdmin, vpdmax, ppt)
  prism_names <- c("tmean", "tmax", "tmin", "tdmean", "vpdmin", "vpdmax", "ppt")
  ex <- cbind(ex,
              setNames(as.data.table(raster::extract(prism_1pre,      sp_points)),
                       paste0(prism_names, "_1pre")),
              setNames(as.data.table(raster::extract(prism_fire,      sp_points)),
                       prism_names),
              setNames(as.data.table(raster::extract(prism_1post,     sp_points)),
                       paste0(prism_names, "_1post")),
              setNames(as.data.table(raster::extract(prism_1pre_abs,  sp_points)),
                       paste0(prism_names, "_1pre_abs")),
              setNames(as.data.table(raster::extract(prism_fire_abs,  sp_points)),
                       paste0(prism_names, "_abs")),
              setNames(as.data.table(raster::extract(prism_1post_abs, sp_points)),
                       paste0(prism_names, "_1post_abs"))
  )
  
  # Vegetation time series (pre + post1..postN)
  ex <- cbind(ex, extract_veg_series(sp_points, veg_paths))
  
  # Fire metadata
  ex[, fire_year := fire_year]
  ex[, OBJECTID  := k]
  
  # -------------------------------------------------------------------
  # Save
  # -------------------------------------------------------------------
  fwrite(ex, file.path(out_dir, paste0(k, ".csv")))
}

# ===========================================================================
# Second pass: add derived recovery metrics + distance to unburned conifer
# ===========================================================================
# Done as post-hoc updates so they can be rerun cheaply without redoing the
# raster extraction.

csv_files <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)

# --- Pass 1: n_yrs_post (count of non-NA post-fire veg classifications) -----
for (file in csv_files) {
  df <- fread(file)
  rf_post_cols <- grep("^RF_post", colnames(df), value = TRUE)
  df[, n_yrs_post := rowSums(!is.na(.SD)), .SDcols = rf_post_cols]
  fwrite(df, file)
}

# --- Pass 2: transitioned / returned / yrs_to_return ------------------------
# transitioned : pre-fire veg differs from year-1 post-fire veg
# returned     : pre-fire veg appears somewhere in the post-fire series
# yrs_to_return: NA if never transitioned; 0 if transitioned but never returned;
#                otherwise the first post-fire year at which the pre-fire
#                type reappears
for (file in csv_files) {
  df <- fread(file)
  post_cols <- grep("^RF_post", colnames(df), value = TRUE)
  
  df[, transitioned := as.integer(RF_pre_veg != RF_post1_veg)]
  
  post_mat <- as.matrix(df[, ..post_cols])
  pre_vec  <- df$RF_pre_veg
  
  returned_vec <- vapply(seq_len(nrow(df)), function(i) {
    if (is.na(pre_vec[i])) return(NA_integer_)
    as.integer(pre_vec[i] %in% post_mat[i, ])
  }, integer(1))
  df[, returned := returned_vec]
  
  yrs_to_return <- vapply(seq_len(nrow(df)), function(i) {
    if (is.na(df$transitioned[i]) || df$transitioned[i] == 0) return(NA_integer_)
    if (is.na(returned_vec[i]) || returned_vec[i] == 0)        return(0L)
    hits <- which(post_mat[i, ] == pre_vec[i])
    if (length(hits) == 0) 0L else as.integer(min(hits))
  }, integer(1))
  df[, yrs_to_return := yrs_to_return]
  
  fwrite(df, file)
}

# --- Pass 3: distance to nearest unburned conifer ---------------------------
for (csv_file in csv_files) {
  fire_id     <- stringr::str_extract(basename(csv_file), "\\d+")
  raster_file <- file.path(dist_unburned_dir, paste0(fire_id, "_7.tif"))
  
  if (!file.exists(raster_file)) {
    message("Distance-to-unburned raster missing for fire ", fire_id, " - skipping.")
    next
  }
  
  df <- fread(csv_file)
  if (!all(c("x", "y") %in% colnames(df))) {
    message("No x/y columns in ", csv_file, " - skipping.")
    next
  }
  
  r <- rast(raster_file)
  df[, dist_unburned := terra::extract(r, cbind(x, y))[, 1]]
  fwrite(df, csv_file)
}

message("Pixel data build complete. Wrote ",
        length(list.files(out_dir, pattern = "\\.csv$")),
        " files to ", out_dir)








