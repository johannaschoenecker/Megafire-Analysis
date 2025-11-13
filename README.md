# Megafire Analysis

This repository contains the R scripts and google earth engine scripts (.txt) and analyses for the project  
**"Conifer forest recovery is compromised by compact and large high severity burn patches within megafires"**.

## Overview
This project combines remote sensing and field data to analyze:
- Burn severity across megafires
- Burn patch metrics across megafires
- Vegetation recovery and post-fire dynamics
- Spatial relationships between burn severity, distance to unburned forest, and vegetation type

## Data required for analyses

All data used in this project were acquired from public sources and where processing was conducted,
the scripts are provided along with analysis scripts in the 'Scripts pub' folder. 

Fire perimeter database- fire shapefiles obtained as geodatabase from the [Calfire FRAP website](https://www.fire.ca.gov/what-we-do/fire-resource-assessment-program/fire-perimeters)

Sierra Nevada region shapefile obtained from the [California State Geoportal](https://gis.data.ca.gov/datasets/2b40b375176f411e8cc829cc1efcca9d_0/explore?location=38.405092%2C-120.319169%2C6.92)

Vegetation classification training data: this was produced from 750 vegetation points from the years 2016, 2018, 2020 and 2022, which are randomly selected points on a 1km grid that were created in QGIS and imported into google earth engine and then manually assigned a vegetation class after inspecting NAIP imagery for the respective year. 

## (Intermediate) outputs produced 

RdNBR rasters for each fire- 30m spatial resolution

Annual vegetation classification composites for the Sierra Nevada region- 30m spatial resolution
Elevation, slope, aspect, topographic roughness index (TRI) & topographic position index (TPI) rasters- 30m spatial resolution
Classified RdNBR rasters for each fire- 30m spatial resolution
PRISM annual absolute composites (mean tmean, tmin, tmax, tdmean, vpdmin, vpdmax; sum ppt)- 2,000m spatial resolution
PRISM annual anomalies (based on 30-year normal) - 2,000m spatial resolution

## Scripts
The scripts should be run in the following order to replicate the results of our analyses/
conduct similar analyses on different fires:

(1) In no particular order, 

## Author
Johanna Schoenecker  
University of Cambridge  
📧 jss84@cam.ac.uk 


## License

This repository is released under the MIT License

You are free to use, modify, and distribute the code for any purpose, provided that proper credit is given and the same license text is included in any redistributions.
See the LICENSE file for full details.

## Citation

If you use this code or analyses in your work, please cite the publication.
---