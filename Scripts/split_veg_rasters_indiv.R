################################################################################
## This script crops annual vegetation classification images obtained through 
## GEE using a random forest classifier applied to Landsat imagery to each 
## fire of interest which burned between 1985 and 2023
## 

## Code by Johanna Schönecker 
# 12th February 2025

# The following datasets area required to successfully run the script:
# 🔘 Annual vegetation classification images for the study region
# 🔘 Individual perimeter shapefiles for every perimeter of interest


################################################################################

### Load required packages
library(sf)
library(terra)
library(fs)
library(tools)
library(raster)

# loop through years for sn, save individual vegetation rasters (crop using individual fire shapefiles) - yearly

for (i in 1985:2023) {

  # define the folder path for the shapefiles
  folder_path <- paste0("c:/users/jscho/onedrive - university of cambridge/phd/analyses/data/megafire_paper/mega_indiv_shp/", i, "/")

  # check if the folder exists
  if (dir.exists(folder_path)) {

    # list shapefiles in the folder
    files <- list.files(path = folder_path, pattern = "\\.shp$", full.names = TRUE)

    # process each shapefile
    for (f in files) {
      print(f)
      
      # Extract fire ID
      m <- sub(".*Mega_([0-9]+)\\.shp$", "\\1", f)
      
      # Convert to numeric (optional)
      m <- as.numeric(m)
      print(m) 

      # read the shapefile using sf
      indiv_shp <- st_read(f)

      for (k in 1984:2024) {

        # define the path to the vegetation raster
        raster_filename <- paste0("c:/users/jscho/onedrive - university of cambridge/phd/analyses/data/megafire_paper/veg_classification_annual/", k, ".tif")

        # check if the raster file exists
        if (file.exists(raster_filename)) {

          # read the vegetation raster
          r_veg <- rast(raster_filename)

          # crop the raster using the shapefile
          veg_r_cropped <- crop(r_veg, indiv_shp)

          # mask the raster to the shapefile (optional, adds precision)
          veg_r_cropped <- mask(veg_r_cropped, indiv_shp)

          # define the output directory and create it if it doesn't exist
          output_dir <- paste0("c:/users/jscho/onedrive - university of cambridge/phd/analyses/data/megafire_paper/veg_annual_individual/", m, "/")
          if (!dir.exists(output_dir)) {
            dir.create(output_dir, recursive = TRUE)
          }

          # define the output filename
          output_filename <- paste0(output_dir, k, '_', tools::file_path_sans_ext(basename(f)), '.tif')

          # write the cropped raster to the output file
          writeRaster(veg_r_cropped, output_filename, overwrite = TRUE)

        } else {
          print(paste("raster file does not exist for year:", k))
        }
      }
    }
  } else {
    # print a message or take other actions if the folder does not exist
    print(paste("shapefile folder does not exist for year:", i))
  }
}
