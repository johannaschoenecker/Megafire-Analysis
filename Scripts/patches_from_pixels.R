################################################################################
## This script compiles a patch-level dataset for specified megafire IDs in the
## Sierra Nevada ecoregion, based on raster images as well as pixel-based
## dataframes

## Code by Johanna Schönecker 
# 13th February 2025

# The following datasets area required to successfully run the script:
# 🔘 Classified RdNBR rasters for each megafire
# 🔘 A dataframe with fire IDs and corresponding fire years
# 🔘 Pixel-metric .csv files for every fire of interest


################################################################################

### Load required packages

library(raster)
library(rgdal)
library(ggplot2)
library(sp)
library(sf)
library(rgeos)
library(reshape)
library(dplyr)
library(landscapemetrics)
library(here)
library(stringr)
library(foreach)
library(doParallel)
library(purrr)
library(data.table)
library(rlang)

# Define the list of megafire IDs
megafire_IDs <- c("5529.tif", "5718.tif", "5721.tif", "8026.tif", "7808.tif", "7747.tif", "7996.tif", "7961.tif", 
                  "7412.tif", "7514.tif", "7242.tif", "9714.tif", "9724.tif", "9487.tif", "8985.tif", "9039.tif", 
                  "8747.tif", "10495.tif", "10498.tif", "10504.tif", "10094.tif", "10273.tif", "1819.tif", "1772.tif", 
                  "2111.tif", "2140.tif", "1210.tif", "1211.tif", "1216.tif", "1215.tif", "1220.tif", "1319.tif", 
                  "1315.tif", "718.tif", "911.tif", "932.tif", "869.tif", "888.tif", "945.tif", "959.tif", "504.tif", 
                  "3363.tif", "3567.tif", "3573.tif", "3574.tif", "3051.tif", "3094.tif", "3188.tif", "2706.tif", 
                  "2147.tif", "2531.tif", "2415.tif", "2618.tif", "5207.tif", "5309.tif", "4277.tif", "4285.tif", 
                  "4306.tif", "4380.tif", "4386.tif", "4374.tif", "4500.tif", "3983.tif", "3992.tif")


# Define a mode function
mode <- function(codes) {
  which.max(tabulate(codes))
}


yrs_ids <- fread(paste0(here(),"/Data/yrs_ids.csv"))

setwd(paste0(here(),"/Data/Rasters/RdNBR_classified/"))
files <- list.files(pattern = "\\.tif$")

for (f in files){

  print(f)
  k<-str_sub(f,1,-5)
  
  i <- yrs_ids$year[yrs_ids$OBJECTID == k]
  
  # Load rdnbr (raw) raster
  raster_name_var<-paste0(here(),"/Data/Rasters/RdNBR_raw/",f)
  dnbr_raster <- raster(raster_name_var)
  
  raster_name_class <- paste0(here(),"/Data/Rasters/RdNBR_classified/",f)
  dnbr_class_raster <- raster(raster_name_class)
  
  # Get patches
  patched_raster <- get_patches(dnbr_class_raster,directions = 8)
  
  # Calculate patch metrics
  core_area_patches <- lsm_p_core(
    dnbr_class_raster,
    directions = 8,
    consider_boundary = FALSE,
    edge_depth = 5)
  
  perimeter_area_patches <- lsm_p_para(dnbr_class_raster, directions = 8)
  
  contiguity_patches <- lsm_p_contig(dnbr_class_raster, directions = 8)
  
  perimeter_patches <- lsm_p_perim(dnbr_class_raster, directions = 8)

  df_name <- paste0(here(),"/Data/raster_df_mega_repeated_planting/",k, ".csv")
  df_pixel <- fread(df_name)
  
  # Vegetation classes and years
  veg_classes <- c(1, 4, 5, 7, 8, 9)
  s <- mode(df_pixel$n_yrs_post)
  post_years <- 1:s
  
  # Create named expressions using `set_names`
  veg_exprs <- unlist(lapply(post_years, function(year) {
    sapply(veg_classes, function(cls) {
      expr <- parse_expr(
        paste0("sum(RF_post", year, "_veg == ", cls, ", na.rm = TRUE)")
      )
      # Name the expression properly
      set_names(list(expr), paste0("count_RF_post", year, "_veg_", cls))
    })
  }), recursive = FALSE)
  
  # Final summarise call
  mn_ele <- df_pixel %>%
    group_by(id) %>%
    summarise(
      mean_rdnbr = mean(rdnbr, na.rm = TRUE),
      rdnbr_class = mode(rdnbr_class),
      mode_veg = as.numeric(names(sort(table(RF_pre_veg), decreasing = TRUE)[1])),
      mean_elevation = mean(elevation, na.rm = TRUE),
      n = n(),
      count_RF_pre_veg_1 = sum(RF_pre_veg == 1, na.rm = TRUE),
      count_RF_pre_veg_4 = sum(RF_pre_veg == 4, na.rm = TRUE),
      count_RF_pre_veg_5 = sum(RF_pre_veg == 5, na.rm = TRUE),
      count_RF_pre_veg_7 = sum(RF_pre_veg == 7, na.rm = TRUE),
      count_RF_pre_veg_8 = sum(RF_pre_veg == 8, na.rm = TRUE),
      count_RF_pre_veg_9 = sum(RF_pre_veg == 9, na.rm = TRUE),
      
      # Add dynamic vegetation class summaries
      !!!veg_exprs
    )
    
    core_area_patches <- core_area_patches %>%
      rename(core_area = value) %>%
      select(core_area,id)
    
    perimeter_area_patches <- perimeter_area_patches %>%
      rename(perimeter_area = value)%>%
      select(perimeter_area,id)
    
    perimeter_patches <- perimeter_patches %>%
      rename(perimeter = value)%>%
      select(perimeter,id)
    
    contiguity_patches <- contiguity_patches %>%
      rename(contiguity = value)%>%
      select(contiguity,id)
    
    mn_ele <- left_join(mn_ele, core_area_patches, by = "id") 
    mn_ele <- left_join(mn_ele, perimeter_area_patches, by = "id") 
    mn_ele <- left_join(mn_ele, perimeter_patches, by = "id") 
    mn_ele <- left_join(mn_ele, contiguity_patches, by = "id") 
    
    mn_ele$OBJECTID <- k
    mn_ele$fire_year <- i
    mn_ele$n_yrs_post <- df_pixel$n_yrs_post[1]
  
 
  fwrite(mn_ele,paste0(here(),"/Data/patch_dfs/",k,'.csv'), row.names = FALSE)
}



# Combine all patch dfs into one:

# Define directory and output path
input_dir <- paste0(here(),"/Data/patch_dfs/")
output_path <- paste0(here(),"/Data/SN_patch_metrics_mega_rdnbr_1985_2023.csv")

# List all CSV files in the directory
csv_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSVs, filling missing columns with NA
all_patches <- lapply(csv_files, fread)
combined_df <- bind_rows(all_patches)

# Save to CSV
fwrite(combined_df, output_path)

