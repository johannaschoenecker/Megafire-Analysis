library(data.table)
library(dplyr)


# -------------------------------------------------------------------
# Load dataframes
# -------------------------------------------------------------------
# Read in all_burn_metrics if calculated previously
all_burn_metrics <- fread(paste0(here(),"/Data/all_burn_metrics.csv"))
fire_metrics_mega <- fread(here("Data", "SN_L4_ecoregion_mega_1985_2023.csv"), header = TRUE)
megafire_IDs      <- fire_metrics_mega$OBJECTID
fires_20yrs_ids   <- fire_metrics_mega$OBJECTID[fire_metrics_mega$YEAR_ < 2004]

# -------------------------------------------------------------------
# Cause code lookup
# -------------------------------------------------------------------
cause_lookup <- c(
  "1"  = "Lightning",        "2"  = "Equipment Use",
  "3"  = "Smoking",          "4"  = "Campfire",
  "5"  = "Debris",           "6"  = "Railroad",
  "7"  = "Arson",            "8"  = "Playing with fire",
  "9"  = "Miscellaneous",    "10" = "Vehicle",
  "11" = "Powerline",        "12" = "Firefighter Training",
  "13" = "Non-Firefighter Training", "14" = "Unknown/Unidentified",
  "15" = "Structure",        "16" = "Aircraft",
  "17" = "Volcanic"
)

# -------------------------------------------------------------------
# Part 1 - Per-fire quantities from raster CSVs (ALL megafires)
#          Recovery columns only fillable for >=20yr fires
# -------------------------------------------------------------------
fire_table_raster <- rbindlist(lapply(megafire_IDs, function(id) {
  
  fpath <- here("Data", "raster_df_mega_repeated_planting", paste0(id, ".csv"))
  if (!file.exists(fpath)) return(NULL)
  fire <- fread(fpath)
  
  conifer_pre  <- nrow(fire[RF_pre_veg == 7])
  transitioned <- nrow(fire[RF_pre_veg == 7 & transitioned == 1])
  
  # 20-year recovery only for fires burned before 2004 (with a full 20-yr
  # post-fire record) AND where RF_post20_veg exists and isn't all NA.
  # (id %in% fires_20yrs_ids is required here -- some post-2003 fires have a
  # non-NA RF_post20_veg column from data extraction quirks even though they
  # haven't actually reached 20 years post-fire; without this check those
  # fires got spurious recovery values.)
  has_20yr <- id %in% fires_20yrs_ids &&
    "RF_post20_veg" %in% names(fire) &&
    any(!is.na(fire$RF_post20_veg))
  
  if (has_20yr) {
    not_returned_20 <- nrow(fire[RF_pre_veg == 7 & transitioned == 1 &
                                   !is.na(RF_post20_veg) & RF_post20_veg != 7])
    returned_20     <- transitioned - not_returned_20
    pct_rec_20      <- ifelse(transitioned > 0, returned_20 / transitioned * 100, NA)
    ha_notret_20    <- not_returned_20 * 0.09
  } else {
    pct_rec_20   <- NA_real_
    ha_notret_20 <- NA_real_
  }
  
  data.table(
    fire_id                = id,
    area_conifer_pre_ha    = conifer_pre  * 0.09,
    area_standreplacing_ha = transitioned * 0.09,
    ha_notreturned_20yr    = ha_notret_20,
    pct_recovered_20       = pct_rec_20
  )
}), fill = TRUE)

# -------------------------------------------------------------------
# Part 2 - Year-0 landscape metrics (AWMPS, total core, largest patch)
# -------------------------------------------------------------------
metrics_yr0 <- all_burn_metrics %>%
  filter(year_post_fire == 0) %>%
  mutate(fire_id = as.integer(as.character(fire_id))) %>%
  group_by(fire_id) %>%
  summarise(
    awmps_standreplacing = sum(value[metric == "area"]^2) / sum(value[metric == "area"]),
    total_core_ha        = sum(value[metric == "core"], na.rm = TRUE),
    largest_patch_ha     = max(value[metric == "area"], na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------------------
# Part 3 - Assemble, compute BOTH LPI versions, spell out cause
# -------------------------------------------------------------------
megafire_table <- fire_metrics_mega %>%
  filter(OBJECTID %in% megafire_IDs) %>%
  dplyr::select(fire_id = OBJECTID, FIRE_NAME, YEAR_, CAUSE, total_area_ha = area_ha) %>%
  left_join(fire_table_raster, by = "fire_id") %>%
  left_join(metrics_yr0,       by = "fire_id") %>%
  mutate(
    cause_name        = cause_lookup[as.character(CAUSE)],
    lpi_total         = largest_patch_ha / total_area_ha          * 100,  # of whole fire
    lpi_standreplacing = largest_patch_ha / area_standreplacing_ha * 100   # of SR footprint
  ) %>%
  arrange(YEAR_) %>%
  transmute(
    `Fire name`                        = FIRE_NAME,
    Year                               = YEAR_,
    `Ignition cause`                   = cause_name,
    `Total area (ha)`                  = round(total_area_ha),
    `Conifer pre-fire (ha)`            = round(area_conifer_pre_ha),
    `Stand-replacing conifer (ha)`     = round(area_standreplacing_ha),
    `LPI (% of fire)`                  = round(lpi_total, 1),
    `LPI (% of stand-replacing)`       = round(lpi_standreplacing, 1),
    `AWMPS stand-replacing (ha)`       = round(awmps_standreplacing, 1),
    `Total core area (ha)`             = round(total_core_ha, 1),
    `% recovered by year 20`           = round(pct_recovered_20, 1),
    `Ha not returned after 20 yr`      = round(ha_notreturned_20yr)
  )


fwrite(megafire_table, here("Data", "megafire_summary_table.csv"))




# Fire-level analysis
library(dplyr)
library(tidyr)
library(ggradar)
library(ggplot2)

# -------------------------------------------------------------------
# Build an analysis-friendly version (syntactic names, all fires)
# -------------------------------------------------------------------
fire_analysis <- fire_metrics_mega %>%
  filter(OBJECTID %in% megafire_IDs) %>%
  dplyr::select(fire_id = OBJECTID, FIRE_NAME, YEAR_, CAUSE, total_area_ha = area_ha) %>%
  left_join(fire_table_raster, by = "fire_id") %>%
  left_join(metrics_yr0,       by = "fire_id") %>%
  mutate(
    lpi_total          = largest_patch_ha / total_area_ha          * 100,
    lpi_standreplacing = largest_patch_ha / area_standreplacing_ha * 100,
    has_20yr           = fire_id %in% fires_20yrs_ids
  )


library(ggplot2)

ggplot(fire_analysis, aes(x = YEAR_, y = lpi_total / 100)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "glm",
    method.args = list(family = quasibinomial()),
    colour = "blue", fill = "grey70"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
  labs(x = "Year", y = "LPI") +
  theme_classic()

# Variables available for ALL fires (size/structure) — for radar & PCA
radar_vars <- c("total_area_ha", "area_conifer_pre_ha", "area_standreplacing_ha",
                "lpi_standreplacing", "awmps_standreplacing", "total_core_ha")

# Nicer axis labels
var_labels <- c(
  total_area_ha          = "Total area",
  area_conifer_pre_ha    = "Conifer pre-fire",
  area_standreplacing_ha = "Stand-replacing area",
  lpi_standreplacing     = "LPI (SR)",
  awmps_standreplacing   = "AWMPS",
  total_core_ha          = "Core area"
)


# -------------------------------------------------------------------
# PCA ORDINATION — where do all fires sit, and where do the 12 fall?
# -------------------------------------------------------------------
pca_input <- fire_analysis %>%
  dplyr::select(all_of(radar_vars)) %>%
  drop_na()

# Track which rows survived drop_na, to align labels/flags
keep <- complete.cases(fire_analysis %>% dplyr::select(all_of(radar_vars)))

pca <- prcomp(pca_input, scale. = TRUE)
summary(pca)

pca_scores <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(
    FIRE_NAME     = fire_analysis$FIRE_NAME[keep],
    has_20yr      = fire_analysis$has_20yr[keep],
    pct_recovered = fire_analysis$pct_recovered_20[keep],
    total_area_ha = fire_analysis$total_area_ha[keep]   # <- add this line
  )

# Variance explained for axis labels
ve <- round(summary(pca)$importance[2, 1:2] * 100)

p_pca <- ggplot(pca_scores, aes(PC1, PC2)) +
  geom_point(aes(color = has_20yr, size = total_area_ha), alpha = 0.8) +
  ggrepel::geom_text_repel(aes(label = FIRE_NAME), size = 2.3,
                           color = "grey40", max.overlaps = 15) +
  scale_color_manual(values = c(`TRUE` = "#0e4f12", `FALSE` = "grey70"),
                     labels = c(`TRUE` = "Analysed (≥20 yr)", `FALSE` = "Recent (<20 yr)"),
                     name = NULL) +
  labs(x = paste0("PC1 (", ve[1], "%)"),
       y = paste0("PC2 (", ve[2], "%)")) +
  theme_classic(base_size = 11) +
  theme(legend.position = "top")

print(p_pca)




# -------------------------------------------------------------------
# PCA on just the 12 fires with 20-year recovery data
# -------------------------------------------------------------------
fire_12 <- fire_analysis %>% filter(has_20yr)

pca_input <- fire_12 %>%
  dplyr::select(all_of(radar_vars)) %>%
  drop_na()

keep <- fire_12 %>%
  dplyr::select(all_of(radar_vars)) %>%
  complete.cases()

pca <- prcomp(pca_input, scale. = TRUE)
summary(pca)

pca_scores <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(
    FIRE_NAME     = fire_12$FIRE_NAME[keep],
    pct_recovered = fire_12$pct_recovered_20[keep],
    total_area_ha = fire_12$total_area_ha[keep]
  )

ve <- round(summary(pca)$importance[2, 1:2] * 100)

# Variable loadings (arrows), scaled to fit the plot
loadings <- as.data.frame(pca$rotation[, 1:2]) %>%
  tibble::rownames_to_column("var") %>%
  mutate(var = var_labels[var],
         PC1 = PC1 * 3, PC2 = PC2 * 3)   # scale factor for arrow visibility

p_pca <- ggplot(pca_scores, aes(PC1, PC2)) +
  # Loading arrows
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "grey55", linewidth = 0.4,
               inherit.aes = FALSE) +
  geom_text(data = loadings, aes(x = PC1 * 1.1, y = PC2 * 1.1, label = var),
            size = 2.8, color = "grey45", inherit.aes = FALSE) +
  # Fire points coloured by recovery
  geom_point(aes(color = pct_recovered, size = total_area_ha), alpha = 0.9) +
  ggrepel::geom_text_repel(aes(label = FIRE_NAME), size = 2.6, color = "grey30",
                           max.overlaps = 15) +
  scale_color_gradient(low = "#D2B48C", high = "#0e4f12", name = "% recovered") +
  scale_size_continuous(name = "Total area (ha)", labels = scales::comma) +
  labs(x = paste0("PC1 (", ve[1], "%)"),
       y = paste0("PC2 (", ve[2], "%)")) +
  theme_classic(base_size = 11) +
  theme(legend.position = "right")

print(p_pca)
