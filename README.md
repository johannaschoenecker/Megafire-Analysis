# Analysis of megafires in the Sierra Nevada from 1985 to 2023

This repository contains the R scripts and Google Earth Engine scripts (`.txt`) used to compile the dataset and conduct the analyses for the project **"Persistent forest-to-shrub transitions after stand-replacing megafires driven by burn patch size, topography and climate"**.

## Overview

This project combines remote sensing and field data to examine:

- Patterns of megafire occurrence in the Sierra Nevada from 1985-2023
- Compositional (classification-based) versus spectral (NDVI, EVI, NIRv) measures of post-fire conifer recovery, and the systematic differences between them
- The proportion of stand-replacing burn patches that return to conifer forest within 20 years, and how this varies with patch size
- The dominant vegetation classes that establish where conifer forest fails to return
- Topographic, climatic, and landscape drivers of pixel-level 20-year conifer recovery

The scripts allow reproducing the analyses and producing the manuscript figures and tables.

## Data required for analyses

All data used in this project were acquired from public sources. Where preprocessing was carried out, the scripts are provided in the `scripts/` folder.

- **Fire perimeters:** shapefiles obtained as a geodatabase from the [Calfire FRAP website](https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/fire-perimeters), rasterised to 30 m spatial resolution with fire year as the pixel value.
- **Sierra Nevada region:** ecoregion shapefile from the [California State Geoportal](https://gis.data.ca.gov/datasets/2b40b375176f411e8cc829cc1efcca9d_0/explore); EPA Level 3 ecoregion boundaries from the [US EPA](https://www.epa.gov/eco-research/ecoregion-download-files-state-region-9).
- **Vegetation classification training data:** 750 vegetation points from 2016, 2018, 2020 and 2022, randomly selected on a 1 km grid in QGIS, imported into Google Earth Engine and manually assigned a vegetation class after inspection of NAIP imagery for the respective year.
- **PRISM climate data:** annual precipitation, temperature (mean/min/max), dew point, and VPD (min/max) from the [PRISM Climate Group](https://prism.oregonstate.edu/), both raw annual values and anomalies relative to a 30-year climatology.
- **Digital elevation model:** SRTM 30 m, used to derive elevation, slope, aspect, TPI and TRI in Google Earth Engine.
- **Spectral indices:** NDVI, EVI and NIRv computed from Landsat surface reflectance in Google Earth Engine, extracted per pixel for transitioned conifer areas.

## Project structure

```
project_root/
├── scripts/                              # Analysis pipeline + all figure scripts
│   ├── 00_setup.R                        # Shared packages, palette, fire IDs, save helper
│   ├── 01_build_pixel_data.R             # Build pixel-level dataframes per fire
│   ├── 01b_flag_reburn_planting.R        # Flag reburn/replanting exclusion pixels
│   ├── 02_build_patch_data.R             # Build patch-level dataset
│   ├── 03_export_coords_for_GEE.R        # Export transitioned pixel coords for GEE
│   ├── 04_train_rf_model.R               # Train recovery random forest classifier
│   ├── 05_predict_rf.R                   # Predict recovery for post-2003 megafires
│   ├── 06_build_fire_table.R             # Build Table 1
│   ├── fig1_overview_map.R
│   ├── fig2_pixel_recovery.R
│   ├── fig3_patch_ridgeline.R
│   ├── fig4_dominant_class_panels.R
│   ├── fig4_panelB_stats.R               # Significance testing for Fig 4b
│   ├── fig5_ale_importance.R
│   ├── fig6_recovery_mirror.R
│   ├── supp_landscape_metrics.R          # Supplementary landscape-metrics figure
│   ├── supp_ale_interaction.R            # Supplementary 2D ALE interaction figure
│   ├── supp_variable_importance.R        # Supplementary full variable-importance figure
│   └── diagnostics_importance_methods.R  # Not a figure — impurity vs permutation importance check
├── GEE_scripts/                          # Google Earth Engine preprocessing (.txt)
├── run_all_figures.R                     # Source all figure scripts in order
├── Data/                                 # Input/derived data (see "Data layout" below)
├── Figures/                              # Rendered figure PDFs/SVGs/PNGs (created on run)
│   └── final_edited/                     # Hand-finished versions of the published figures
└── README.md
```

## How to use

All R scripts use `here::here()` for paths and assume the project is opened via an RStudio `.Rproj` file at the repo root. Each figure script can be sourced independently — they all pull in shared setup themselves.

**Reproduce all figures from cached intermediate data** (assumes the upstream pipeline has already been run at least once):

```r
source("run_all_figures.R")
```

**Run a single figure:**

```r
source(here::here("scripts", "fig3_patch_ridgeline.R"))
```

**Reproduce the whole pipeline from raw rasters** (slow, hours):

```r
source(here::here("scripts", "01_build_pixel_data.R"))
source(here::here("scripts", "01b_flag_reburn_planting.R"))
source(here::here("scripts", "02_build_patch_data.R"))
source(here::here("scripts", "03_export_coords_for_GEE.R"))
# — run GEE extraction externally to get NDVI/EVI/NIRv per-pixel timeseries —
source(here::here("scripts", "04_train_rf_model.R"))
source(here::here("scripts", "05_predict_rf.R"))
source(here::here("scripts", "06_build_fire_table.R"))
source(here::here("run_all_figures.R"))
```

> **Known gap:** `01b_flag_reburn_planting.R` is a best-effort reconstruction
> of a step whose original script was lost (see the header comment in that
> file for validation details — it reproduces the dataset actually used in
> the manuscript at 99.7% / 97.9% per-pixel agreement on its two flags, but
> isn't a bit-for-bit match). It writes to a separate output folder rather
> than overwriting the validated cache the manuscript results are built on.
> Similarly, the GEE workflow that extracted per-pixel NDVI/EVI/NIRv time
> series (referenced in Methods) has no corresponding script in
> `GEE_scripts/` — it could not be located and would need to be rewritten
> from the Methods description to fully reproduce Fig. 2/S3 from scratch.

## Scripts

### Google Earth Engine preprocessing

| Script | Description | Outputs |
|---|---|---|
| `RF_veg_classification_GEE.txt` | GEE-based random forest vegetation classification workflow. | Annual (1984-2024) vegetation classification maps for the Sierra Nevada region, exported to Google Drive; accuracy assessment tables. |
| `Topography_e_s_a_GEE.txt` | Calculates elevation, slope, aspect, TPI and TRI layers from SRTM DEM. | Topographic variable rasters at 30 m resolution, exported to Google Drive. |
| `PRISM_annual_GEE.txt` | Exports annual PRISM climate data (temperature, precipitation, VPD, dew point). | Annual climate raster images at 2 km resolution for study years, exported to Google Drive. |
| `PRISM_anomalies_GEE.txt` | Computes PRISM climate anomalies relative to 30-year averages. | Annual climate anomaly raster images at 2 km resolution for study years, exported to Google Drive. |

> **Missing:** the GEE workflow that extracts annual NDVI/EVI/NIRv per-pixel
> time series (Methods: "growing season greenest pixel composites... using
> GEE") has no corresponding script in this folder. It produced
> `Data/{NDVI,EVI,NIRv}_individual/*.tif`, which are present and used by
> `scripts/fig2_pixel_recovery.R`, but the extraction script itself could not
> be located and would need to be rewritten to fully reproduce this step.

### R pipeline

| Script | Description | Outputs |
|---|---|---|
| `scripts/00_setup.R` | Shared setup: package loading, vegetation palette and labels, fire ID and name lookups, helper for saving figures. | Objects available to all downstream scripts. |
| `scripts/01_build_pixel_data.R` | Builds a pixel-level data frame for each megafire, combining topography, PRISM climate (fire year, 1 yr pre, 1 yr post; anomalies and absolutes), pre-fire and up to 37 post-fire vegetation classifications, distance to unburned conifer, and previous fire history. Adds derived recovery metrics (`transitioned`, `returned`, `yrs_to_return`, `n_yrs_post`). | One `.csv` per fire in `Data/raster_df_mega/`. |
| `scripts/01b_flag_reburn_planting.R` | Adds `reburn_20yrs` (pixel burned again within 20 yr) and `tree_planting` (pixel was actively replanted, from FACTS records) exclusion flags used by the RF model and downstream figures. See the file header for validation details and a caveat: it's a best-effort reconstruction of a lost script. | One `.csv` per fire in `Data/raster_df_mega_repeated_planting_reconstructed/`. |
| `scripts/02_build_patch_data.R` | Delineates stand-replacing burn patches from the pixel data and aggregates pixel attributes to the patch level. | Patch-level data frames. |
| `scripts/03_export_coords_for_GEE.R` | Exports the coordinates of transitioned pre-fire conifer pixels, ready for spectral index extraction in Google Earth Engine. | `Data/transitioned_conifer_coords.csv`. |
| `scripts/04_train_rf_model.R` | Trains a weighted random forest classifier to predict whether a stand-replacing conifer pixel returns to conifer within 20 years, using megafires with 20+ years of post-fire data and no record of reburn or planting. Includes correlation-based predictor pruning, class weighting, ROC/AUC and confusion matrix. | `Data/rf_model_class_returned_reburns_plantings.rds`; `Data/train_data.csv`. |
| `scripts/05_predict_rf.R` | Applies the trained model to eligible pixels in megafires with less than 20 years post-fire vegetation data (excluding pixels in areas with post-fire tree planting or subsequent reburn). | `Data/predicted_recovery.csv`. |
| `scripts/06_build_fire_table.R` | Assembles Table 1: for each fire, ignition cause, total area, area of conifer pre-fire, area of stand-replacing conifer burn, LPI, AWMPS, total stand-replacing core area, percent of stand-replacing area recovered by year 20 (pre-2004 fires only), and hectares of conifer transitioned after 20 years. | `Data/megafire_summary_table.csv`. |
| `scripts/fig1_overview_map.R` | **Fig 1** — Sierra Nevada L3 ecoregion overview, showing megafire perimeters 1985-2023 (older fires darker, more recent fires lighter) and 1984 conifer extent. | `Figures/fig1_overview_map.{pdf,svg,png}`. |
| `scripts/fig2_pixel_recovery.R` | **Fig 2** — Percentage of transitioned conifer pixels classified as recovered each post-fire year for the compositional metric versus NDVI, EVI and NIRv, pooled across all megafires with 20+ years post-fire data. Caches `recovery_pixels.csv`. | `Figures/fig2_pixel_recovery.{pdf,svg,png}`. |
| `scripts/fig3_patch_ridgeline.R` | **Fig 3** — Density distribution of the proportion of each stand-replacing patch returned to conifer at 20 yr, split by patch size category, with overlaid boxplots. | `Figures/fig3_patch_ridgeline.{pdf,svg,png}`. |
| `scripts/fig4_dominant_class_panels.R` | **Fig 4** — Three-panel figure of the dominant vegetation class 20 yr post-fire: (a) frequency across all patches, (b) patch sizes by dominant class, (c) proportion of patches by dominant class across size classes. | `Figures/fig4_dominant_class_panels.{pdf,svg,png}`. |
| `scripts/fig4_panelB_stats.R` | Kruskal-Wallis + Dunn's post-hoc (Benjamini-Hochberg corrected) significance testing for Fig 4b's patch-size-by-dominant-class comparison. | `Data/figure_cache/fig4_panelB_dunn_test.csv`. |
| `scripts/fig5_ale_importance.R` | **Fig 5** — Accumulated Local Effects (ALE) plots of the top 8 predictors of 20-year pixel-level conifer recovery, with relative variable importance shown as an embedded slider in each panel. | `Figures/fig5_ale_importance.{pdf,svg,png}`. |
| `scripts/fig6_recovery_mirror.R` | **Fig 6** — Mirrored bar chart of annual stand-replacing conifer area by observed (pre-2004 fires) vs. RF-predicted (2004-2023 fires) 20-year recovery outcome, plus a scatter/trend of the proportion not returned. Saves two variants; `fig6_recovery_twopanel` (a/b split) is the one used in the manuscript. | `Figures/fig6_recovery_mirror.{pdf,svg,png}`, `Figures/fig6_recovery_twopanel.{pdf,svg,png}`. |
| `scripts/supp_landscape_metrics.R` | **Supplementary** — Post-fire annual mean values of four landscape metrics (AWMPS, ENN, shape index, total core area) for the stand-replacing burn patches that have failed to return to conifer forest in each year, per fire. | `Figures/supp_landscape_metrics.{pdf,svg,png}`. |
| `scripts/supp_ale_interaction.R` | **Supplementary** — 2D ALE interaction between distance to seed source and post-fire precipitation. | `Figures/supp_ale_interaction.{pdf,svg}`, `Figures/supp_ale_interaction_density.{pdf,svg}`. |
| `scripts/supp_variable_importance.R` | **Supplementary** — Full impurity-based variable importance ranking (all predictors, not just the top 8 shown in Fig 5). | `Figures/supp_variable_importance.{pdf,svg}`. |
| `scripts/diagnostics_importance_methods.R` | Not a manuscript figure — sanity check that impurity and permutation variable importance rankings agree. | Console output only. |
| `run_all_figures.R` | Sources every figure script in order. | All figures rendered to `Figures/`. |

## Pipeline overview

| Step | Script | Reads | Writes | Runtime |
|---|---|---|---|---|
| 1 | `01_build_pixel_data.R` | Topography, PRISM, veg classification, previous-fire rasters | `Data/raster_df_mega/*.csv` | Hours |
| 1b | `01b_flag_reburn_planting.R` | pixel CSVs, FACTS planting shapefile, cumulative fire rasters | `Data/raster_df_mega_repeated_planting_reconstructed/*.csv` | Long |
| 2 | `02_build_patch_data.R` | pixel CSVs | patch-level data frames | Long |
| 3 | `03_export_coords_for_GEE.R` | pixel CSVs | `Data/transitioned_conifer_coords.csv` | Fast |
| — | External GEE step | coords CSV | `Data/{NDVI,EVI,NIRv}_individual/*.tif` | — |
| 4 | `04_train_rf_model.R` | pixel CSVs (reburn/planting-flagged) | `Data/rf_model_class_returned_reburns_plantings.rds`, `Data/train_data.csv` | 5-15 min |
| 5 | `05_predict_rf.R` | model + pixel CSVs | `Data/predicted_recovery.csv` | Minutes |
| 6 | `06_build_fire_table.R` | pixel CSVs, fire metrics | `Data/megafire_summary_table.csv` | Fast |

## Data layout (expected)

```
Data/
├── SN_L4_ecoregion_mega_1985_2023.csv                    # Fire-level metadata
├── train_data.csv                                        # Training data for RF (written by step 4)
├── all_burn_metrics.csv                                  # Landscape metrics over time
├── transitioned_conifer_coords.csv                       # Written by step 3
├── megafire_summary_table.csv                            # Table 1 (written by step 6)
├── raster_df_mega/<fire_id>.csv                          # Per-fire pixel data (step 1)
├── raster_df_mega_repeated_planting/<fire_id>.csv        # + reburn/planting flags (validated
│                                                          #   cache the manuscript results use)
├── raster_df_mega_repeated_planting_reconstructed/       # Same, from 01b's reconstruction
│   └── <fire_id>.csv                                     #   (see 01b's header for caveats)
├── figure_cache/                                         # Auto-created cache (safe to delete)
│   ├── all_patch_summary.csv
│   ├── fig4_panelB_dunn_test.csv
│   └── recovery_pixels.csv
├── {NDVI,EVI,NIRv,kNDVI,NBR}_individual/<fire_id>_<IDX>.tif
├── Shapefiles/
│   ├── SN_Ecoregion_EPA_megafires_1985_2023.shp
│   ├── us_eco_l3.shp
│   ├── FACTS_tree_planting_1986_2024_in_megafires.shp    # Used by 01b
│   └── SN_all_fires_yearly_shp/fires_<year>.shp
└── Rasters/
    ├── veg_annual_individual/<fire_id>/<year>_Mega_<fire_id>.tif
    ├── veg_classification_annual/<year>.tif
    ├── 3310_PRISM_anomalies/prism_anomaly_<year>.tif
    ├── 3310_PRISM_abs/prism_annual_<year>.tif
    ├── SN_fires_yearly_rasters_upto/fires_up_to_<year>.tif
    ├── dist_unburned_conifer_7/<fire_id>_7.tif
    └── tree_planting_year.tif
```

## Author

Johanna Schoenecker
University of Cambridge
📧 jss84@cam.ac.uk

## License

This repository is released under the MIT License. You are free to use, modify, and distribute the code for any purpose, provided that proper credit is given and the same license text is included in any redistributions. See the LICENSE file for full details.

## Citation

If you use this code or analyses in your work, please cite the parent publication.