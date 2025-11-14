# Analysis of megafires in the Sierra Nevada from 1985 to 2023

This repository contains the R scripts and google earth engine scripts (.txt) that were used to compile the dataset and coduct the analyses for the project  
**"Conifer forest recovery is compromised by compact and large high severity burn patches within megafires"**.

## Overview
This project combines remote sensing and field data to address:
- Megafire occurrence over the study period (1985-2023)
- Spatial patterns and configurations of severely burned patches within megafires
- Drivers and area of long-term transitions from conifer forest to other vegetation types post-fire

The scripts provided allow conducting the analyses and producing figures supporting and visualising the findings.


## Data required for analyses

All data used in this project were acquired from public sources and where processing was conducted,
the scripts are provided along with analysis scripts in the 'Scripts pub' folder. 

Fire perimeter database- fire shapefiles obtained as geodatabase from the [Calfire FRAP website](https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/fire-perimeters). This was rasterised to 30m spatial resolution with fire year as the variable.

Sierra Nevada region shapefile obtained from the [California State Geoportal](https://gis.data.ca.gov/datasets/2b40b375176f411e8cc829cc1efcca9d_0/explore?location=38.405092%2C-120.319169%2C6.92)

Vegetation classification training data: this was produced from 750 vegetation points from the years 2016, 2018, 2020 and 2022, which are randomly selected points on a 1km grid that were created in QGIS and imported into google earth engine and then manually assigned a vegetation class after inspecting NAIP imagery for the respective year. 



## Scripts
All scripts for data processing and analysis can be found in the 'Scripts pub' folder.
Each script will specify in its header its functionality, but a brief overview is given here:

| Script                                                                                                                               | Description                                                                                                          | Outputs                                                                                                     |
| ------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------- |
| [**RdNBR_calculations_GEE.txt**](Scripts%20pub/RdNBR_calculations_GEE.txt)                                                           | GEE script calculating Relativized Burn Ratio (RdNBR) from satellite composites.                                     | RdNBR burn severity images (.tif) for each fire, exported to Google Drive |
| [**reclassify_RdNBR.R**](Scripts%20pub/reclassify_RdNBR.R)                                                                           | Reclassifies RdNBR values into standardized burn-severity categories.                       | Categorical burn severity raster images for each fire (low / moderate / high severity)                   |
| [**RF_veg_classification_GEE.txt**](Scripts%20pub/RF_veg_classification_GEE.txt)                                                     | GEE-based random forest vegetation classification workflow.                                                          | Annual (1984-2024) vegetation classification maps for Sierra Nevada region exported to Google Drive; accuracy assessment tables.                                                 |
| [**Topography_e_s_a_GEE.txt**](Scripts%20pub/Topography_e_s_a_GEE.txt)                                                               | GEE script calculating elevation, slope, and aspect layers from DEM data.                                             | Topographic variable rasters at 30m spatial resolution, exported to Google Drive.                                                      |
| [**PRISM_annual_GEE.txt**](Scripts%20pub/PRISM_annual_GEE.txt)                                                                       | GEE export of annual PRISM climate data (temperature, precipitation).                                                | Annual climate raster images at 2,000m spatial resolution (mean annual values for tmax, tmin,tmean, tdmean, vpdmax,vpdmin; cumulative annual ppt) for study years, exported to Google Drive.                                                   |
| [**PRISM_anomalies_GEE.txt**](Scripts%20pub/PRISM_anomalies_GEE.txt)                                                                 | Computes PRISM climate anomalies relative to 30-year averages.                                                       | Annual climate anomaly raster images at 2,000m spatial resolution (mean annual values for tmax, tmin,tmean, tdmean, vpdmax,vpdmin; cumulative annual ppt) for study years, exported to Google Drive.                                       |
| [**pixel_df_mega.R**](Scripts%20pub/pixel_df_mega.R)                                                                                 | Builds pixel-level data frames combining spectral, topographic, and climate variables, extracted from raster images.                               | Large `.CSV` datasets of per-pixel attributes for each fire.                                 |
| [**patches_from_pixels.R**](Scripts%20pub/patches_from_pixels.R)                                                                     | Aggregates pixel-based variables at burn severiy patch level.                                                | Patch-level data frames with summary statistics (size, mean severity, climatic variables, proportion vegetation type by year etc.).                                |
| [**Patch_analyses.R**](Scripts%20pub/Patch_analyses.R)                                                                               | Calculates largest patch index (LPI) and area weighted mean patch size (AWMPS) for every fire in each burn severity class.                                            | Patch metrics tables; LPI and AWMPS by fire and burn severity class                                               |
| [**RF_classification_weighted_returned_repeated_planting.R**](Scripts%20pub/RF_classification_weighted_returned_repeated_planting.R) | Trains and evaluates a weighted random forest model to predict whether a coniferous pixel that burned severely returns to a coniferous vegetation class within 20 years or not. Training based on megafires with at least 20 year recovery data and no record of reburn or planting post-fire.                        | Random forest model of 20-year conifer recovery/ failure to recover; variable importance plots; accuracy summaries.                                        |
| [**predictions_returned_RF_class_reburns_plantings.R**](Scripts%20pub/predictions_returned_RF_class_reburns_plantings.R)             | Generates predictions for 20-year conifer recovery for severely burned coniferous pixels in fires with less than 20 years post-fire vegetation data that have no record of post-fire tree planting or reburn.                             | Prediction data frame including every qualifying pixel; post-fire conifer probabilities.                                |
| [**Figures.qmd**](Scripts%20pub/Figures.qmd)                                                                                         | Quarto document creating all main and supplementary figures for the manuscript, based on compiled data and analyses. | Publication-quality figures and summary tables.                                               |
                          

## Author
Johanna Schoenecker  
University of Cambridge  
📧 jss84@cam.ac.uk 


## License

This repository is released under the MIT License.You are free to use, modify, and distribute the code for any purpose, provided that proper credit is given and the same license text is included in any redistributions.
See the LICENSE file for full details.

## Citation

If you use this code or analyses in your work, please cite the parent publication.

---