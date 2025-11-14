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

Fire perimeter database- fire shapefiles obtained as geodatabase from the [Calfire FRAP website](https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/fire-perimeters)

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
| [**Topography_e_s_a_GEE.txt**](Scripts%20pub/Topography_e_s_a_GEE.txt)                                                               | GEE script calculating elevation, slope, and aspect layers from DEM data.                                             | Topographic variable rasters exported to Google Drive.                                                      |
| [**PRISM_annual_GEE.txt**](Scripts%20pub/PRISM_annual_GEE.txt)                                                                       | GEE export of annual PRISM climate data (temperature, precipitation).                                                | Annual climate raster images at 2,000m spatial resolution (mean annual values for tmax, tmin,tmean, tdmean, vpdmax,vpdmin; cumulative annual ppt) for study years, exported to Google Drive.                                                   |
| [**PRISM_anomalies_GEE.txt**](Scripts%20pub/PRISM_anomalies_GEE.txt)                                                                 | Computes PRISM climate anomalies relative to 30-year averages.                                                       | Annual climate anomaly raster images at 2,000m spatial resolution (mean annual values for tmax, tmin,tmean, tdmean, vpdmax,vpdmin; cumulative annual ppt) for study years, exported to Google Drive.                                       |
| [**pixel_df_mega.R**](Scripts%20pub/pixel_df_mega.R)                                                                                 | Builds pixel-level data frames combining spectral, topographic, and climate variables.                               | Large `.CSV` datasets of per-pixel attributes for each fire.                                 |
| [**patches_from_pixels.R**](Scripts%20pub/patches_from_pixels.R)                                                                     | Aggregates pixel-based classifications into contiguous patch objects.                                                | Patch-level data frames with summary statistics (size, mean severity, etc.).                                |
| [**Patch_analyses.R**](Scripts%20pub/Patch_analyses.R)                                                                               | Performs spatial analyses of burn patch characteristics and connectivity.                                            | Patch metrics tables; connectivity and fragmentation indices.                                               |
| [**RF_classification_weighted_returned_repeated_planting.R**](Scripts%20pub/RF_classification_weighted_returned_repeated_planting.R) | Trains and evaluates a weighted random forest for repeated planting and vegetation recovery.                         | Fitted model objects; variable importance plots; accuracy summaries.                                        |
| [**predictions_returned_RF_class_reburns_plantings.R**](Scripts%20pub/predictions_returned_RF_class_reburns_plantings.R)             | Generates random forest predictions for vegetation recovery after reburns and plantings.                             | Prediction rasters or data frames; post-fire vegetation class probabilities.                                |
| [**Figures.qmd**](Scripts%20pub/Figures.qmd)                                                                                         | Quarto document creating all main and supplementary figures for the manuscript, based on compiled data and analyses. | Publication-quality figures and summary tables.                                               |
                          


## (Intermediate) outputs produced 

RdNBR rasters for each fire- 30m spatial resolution

Annual vegetation classification composites for the Sierra Nevada region- 30m spatial resolution
Elevation, slope, aspect, topographic roughness index (TRI) & topographic position index (TPI) rasters- 30m spatial resolution
Classified RdNBR rasters for each fire- 30m spatial resolution
PRISM annual absolute composites (mean tmean, tmin, tmax, tdmean, vpdmin, vpdmax; sum ppt)- 2,000m spatial resolution
PRISM annual anomalies (based on 30-year normal) - 2,000m spatial resolution

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