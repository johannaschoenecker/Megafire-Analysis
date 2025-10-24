################################################################################
## This script re-classifies raster (.tif) images of raw RdNBR values into RdNBR
## (burn severity) categories according to the thresholds described by Miller and
## Thode (2007) ➡️ unchanged (class 0): < 69; low (class 1): 69-315;
## moderate (class 2): 316-640; high (class 3): >640

## Code by Johanna Schönecker 
# 12th February 2025

# The following datasets area required to successfully run the script:
# 🔘 Raw RdNBR rasters for each megafire


################################################################################

### Load required packages

library(terra)
library(fs)

# 1️⃣ Define Input and Output Folders
input_folder <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/RdNBR_raw"
output_folder <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/RdNBR_classified"

# Ensure output directory exists
dir_create(output_folder)

# 2️⃣ List all .tif files in the input folder
raster_files <- dir_ls(input_folder, glob = "*.tif")

# 3️⃣ Define the reclassification matrix (adjusted for precision handling)
reclass_matrix <- matrix(c(
  -Inf, 69, 0,   # Values <= 69 → 0
  69, 316, 1,   # Values 69 - 316 → 1
  316, 641, 2,   # Values 316 - 641 → 2
  641, Inf, 3    # Values >= 641 → 3
), ncol = 3, byrow = TRUE)

# 4️⃣ Loop Through Files and Process
for (file in raster_files) {
  # Read raster
  r <- rast(file)
  
  # Round values to avoid floating-point issues
  r <- round(r)
  
  # Apply reclassification
  r_classified <- classify(r, reclass_matrix)
  
  # Define output filename
  output_filename <- file.path(output_folder, basename(file))
  
  # Save reclassified raster
  writeRaster(r_classified, output_filename, overwrite = TRUE)
  
  cat("Processed:", file, "→ Saved:", output_filename, "\n")
}


