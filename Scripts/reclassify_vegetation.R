################################################################################
## This script reclassifies annual vegetation classification images obtained 
## through GEE using a random forest classifier applied to Landsat imagery to 
## each fire of interest which burned between 1985 and 2023 into coniferous vs. 
## non-coniferous vegetation type

## Code by Johanna Schönecker 
# 12th February 2025

# The following datasets area required to successfully run the script:
# 🔘 Annual vegetation classification images for the study region

################################################################################

### Load required packages

library(terra)

# Define input and output directories
input_dir <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_classification_annual"
output_dir <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/GIS/Megafires/annual_conifer_raster"

# List of target raster filenames
raster_files <- list.files(input_dir, pattern = "\\.tif$", full.names = FALSE)

# Loop through rasters and reclassify
for (raster_name in raster_files) {
  # Define file paths
  input_path <- file.path(input_dir, raster_name)
  output_path <- file.path(output_dir, raster_name)
  
  # Load raster
  r <- rast(input_path)
  
  # Reclassify: keep 7 as 7, set everything else to 0
  r_reclassified <- ifel(r == 7, 7, 0)
  
  # Save reclassified raster
  writeRaster(r_reclassified, output_path, overwrite = TRUE)
  
  cat("Processed:", raster_name, "\n")
}

cat("Reclassification complete.\n")
