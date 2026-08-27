# Prepare coordinates for export to GEE to calculate NDVI and EVI timeseries
# -------------------------------------------------------------------
# Load required packages
# -------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyr, raster, rgdal, ggplot2, ggspatial, sp, sf, rgeos, reshape,
  patchwork, Rcpp, scales, dplyr, ggpubr, landscapemetrics, landscapetools,
  here, stringr, moments, reshape2, ggeasy, vctrs, mgcv, networkD3,
  ggridges, cowplot, factoextra, gdistance, fs, data.table, terra,
  gridExtra, grid, car, biscale, e1071, scales, devEMF
)

#Fire level datasets
fire_metrics_mega <- fread(paste0(here(),"/Data/SN_L4_ecoregion_mega_1985_2023.csv"), header=TRUE)

megafire_IDs <- fire_metrics_mega$OBJECTID

fires_20yrs_ids <- fire_metrics_mega$OBJECTID[fire_metrics_mega$YEAR_ < 2004]


# Conifer that transitioned

# Export coordinates for all fires
coords_export <- rbindlist(
  lapply(fires_20yrs_ids, function(id) {
    fire <- fread(here("Data", "raster_df_mega", paste0(id, ".csv")))
    fire[RF_pre_veg == 7 & transitioned == 1, 
         .(x, y, fire_id = id, returned, yrs_to_return)]
  })
) %>%
  # Convert from EPSG:3310 to WGS84 for GEE
  st_as_sf(coords = c("x", "y"), crs = 3310) %>%
  st_transform(4326) %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

fwrite(coords_export, here("Data", "transitioned_conifer_coords.csv"))
