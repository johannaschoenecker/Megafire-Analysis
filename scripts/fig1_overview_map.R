# ============================================================================
# Figure 1 — Overview map of the Sierra Nevada ecoregion and megafires
# ============================================================================
# SN L3 ecoregion + megafires 1985-2023 (older fires darker, newer fires lighter),
# with 1984 conifer extent (from RF classification) as a green raster underlay.
#
# Inputs:
#   - Data/Shapefiles/SN_Ecoregion_EPA_megafires_1985_2023.shp
#   - Data/Shapefiles/us_eco_l3.shp
#   - Data/Rasters/veg_classification_annual/1984.tif
#
# Output: Figures/fig1_overview_map.{pdf,svg}
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# --- Load shapefiles --------------------------------------------------------
megafires <- st_read(here("Data", "Shapefiles",
                          "SN_Ecoregion_EPA_megafires_1985_2023.shp"),
                     quiet = TRUE)
ecoregion <- st_read(here("Data", "Shapefiles", "us_eco_l3.shp"),
                     quiet = TRUE)

# Match CRS (megafires are in EPSG:3310, CA Albers)
ecoregion <- st_transform(ecoregion, st_crs(megafires))

# --- Classify fires by period ------------------------------------------------
megafires <- megafires %>%
  mutate(period = ifelse(YEAR_ <= 2003, "1985-2003", "2004-2023"))

# --- Identify focal ecoregion (Sierra Nevada) and crop -----------------------
focal_eco <- ecoregion %>% filter(US_L3NAME == "Sierra Nevada")

bb <- st_bbox(megafires)
margin <- 50000  # 50 km margin (units = metres in EPSG:3310)
bb_expanded <- bb + c(-margin, -margin, margin, margin)

ecoregion_crop <- st_crop(ecoregion, bb_expanded)
other_eco      <- ecoregion_crop %>% filter(US_L3NAME != "Sierra Nevada")

# Bounding box of focal ecoregion with small margin
bb_focal <- st_bbox(focal_eco) + c(-10000, -10000, 10000, 10000)

# --- Conifer mask from 1984 classification -----------------------------------
veg1984 <- rast(here("Data", "Rasters", "veg_classification_annual", "1984.tif"))

# Reproject to map CRS (nearest-neighbour for categorical data)
veg1984 <- project(veg1984, "EPSG:3310", method = "near")

# Downsample: 30 m raw is too dense to render across the whole ecoregion;
# factor 20 -> ~600 m pixels (matches the caption in Recovery.docx)
veg1984_agg <- aggregate(veg1984, fact = 20, fun = "modal")

# Keep only conifer (class 7)
conifer <- veg1984_agg
conifer[conifer != 7] <- NA
conifer <- crop(conifer, vect(focal_eco))
conifer <- mask(conifer, vect(focal_eco))

conifer_df <- as.data.frame(conifer, xy = TRUE, na.rm = TRUE)
names(conifer_df)[3] <- "veg"

# --- Build map --------------------------------------------------------------
p_map <- ggplot() +
  # Non-focal ecoregions
  geom_sf(data = other_eco,  fill = "grey96", color = "grey80", linewidth = 0.2) +
  # Focal Sierra Nevada ecoregion
  geom_sf(data = focal_eco,  fill = "grey88", color = "grey20", linewidth = 0.3) +
  # Conifer extent
  geom_raster(data = conifer_df, aes(x = x, y = y),
              fill = "#0e4f12", alpha = 0.6) +
  # Newer megafires (lighter, drawn first so older fires overlay)
  geom_sf(data = megafires %>% filter(period == "2004-2023"),
          aes(fill = period), color = "#B39DDB", alpha = 0.8) +
  # Older megafires (darker, on top)
  geom_sf(data = megafires %>% filter(period == "1985-2003"),
          aes(fill = period), color = "#5E35B1", alpha = 0.85) +
  scale_fill_manual(
    values = c("1985-2003" = "#5E35B1",
               "2004-2023" = "#B39DDB"),
    name   = "Fire period"
  ) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style    = north_arrow_fancy_orienteering,
    height   = unit(1, "cm"), width = unit(1, "cm")
  ) +
  coord_sf(xlim = c(bb_focal["xmin"], bb_focal["xmax"]),
           ylim = c(bb_focal["ymin"], bb_focal["ymax"])) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid      = element_line(color = "grey90", linewidth = 0.2),
    axis.text       = element_text(size = 8, color = "grey50")
  )

print(p_map)

# --- Save -------------------------------------------------------------------
save_fig(p_map, "fig1_overview_map", width = 180, height = 150)
