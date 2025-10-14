################################################################################
## This script compiles a patch-level dataset for specified megafire IDs in the
## Sierra Nevada ecoregion

## Code by Johanna Schönecker 
# 13th February 2025

# The following datasets area required to successfully run the script:
# 🔘 Raw RdNBR rasters for each megafire
# 🔘 Classified RdNBR rasters for each megafire
# 🔘 A dataframe with fire IDs and corresponding fire years
# 🔘 Elevation, slope, aspect, TPI and TRI rasters for the study region
# 🔘 Annual (1984-2024) PRISM rasters- 30yr anomalies and absolute values
# 🔘 Annual vegetation classification images for every fire
# 🔘 Raster images for every fire indicating distance from unburned conifer


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

# Custom function to safely calculate summary statistics only for existing columns
safe_summary <- function(data, column, func, na_value = NA, ...) {
  if (column %in% colnames(data)) {
    func(data[[column]], ...)  # Pass additional arguments (like na.rm) to func
  } else {
    na_value  # Return NA if the column doesn't exist
  }
}


  yrs_ids <- fread("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Dataframes/Megafire_paper/yrs_ids.csv")
  
  setwd("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/RdNBR_classified/")
  files <- list.files(pattern = "\\.tif$")
  

  
  # Loading all the other rasters, which every fire should have
  elev_raster <- raster("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/elevation.tif")
  aspect_raster <- raster("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/aspect.tif")
  slope_raster <- raster("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/slope.tif")

  
  #files <- files[-(1:2)] 
  for (f in files){
    print(f)
    k<-str_sub(f,1,-5)
    k <- str_sub(k, -5, -1) 
    
    i <- yrs_ids$year[yrs_ids$OBJECTID == k]
    
    # Create the extended vector of post identifiers
    
    x <- 2023-i
    extended_vector <- paste0("post", x:37)
    
    g <- i-1
    o <- i+1
    m <- i+2
    m2 <- i+3
    m3 <- i+4
    m4 <- i+5
    m5 <- i+6
    m6 <- i+7
    m7 <- i+8
    m8 <- i+9
    m9 <- i+10
    m10 <- i+11
    m11 <- i+12
    m12 <- i+13
    m13 <- i+14
    m14 <- i+15
    m15 <- i+16
    m16 <- i+17
    m17 <- i+18
    m18 <- i+19
    m19 <- i+20
    m20 <- i+21
    m21 <- i+22
    m22 <- i+23
    m23 <- i+24
    m24 <- i+25
    m25 <- i+26
    m26 <- i+27
    m27 <- i+28
    m28 <- i+29
    m29 <- i+30
    m30 <- i+31
    m31 <- i+32
    m32 <- i+33
    m33 <- i+34
    m34 <- i+35
    m35 <- i+36
    m36 <- i+37
    
    prism_fire_year <- stack(paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/3310_PRISM_anomalies/prism_anomaly_",i,".tif"))
    prism_1pre <-stack(paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/3310_PRISM_anomalies/prism_anomaly_",g,".tif"))
    prism_1post <-stack(paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/3310_PRISM_anomalies/prism_anomaly_",o,".tif"))
    prism_fire_year_abs <-stack(paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/3310_PRISM_abs/prism_annual_",i,".tif"))
    prism_1pre_abs <- stack(paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/3310_PRISM_abs/prism_annual_",g,".tif"))
    prism_1post_abs <- stack(paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/3310_PRISM_abs/prism_annual_",o,".tif"))
    
    veg_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", g,"_Mega_",k, ".tif")
    veg_1post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", o,"_Mega_",k,  ".tif")
    veg_2post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/",m,"_Mega_",k,  ".tif")
    veg_3post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m2,"_Mega_",k,  ".tif")
    veg_4post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m3,"_Mega_",k,  ".tif")
    veg_5post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m4,"_Mega_",k,  ".tif")
    veg_6post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m5,"_Mega_",k,  ".tif")
    veg_7post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/",m6,"_Mega_",k,  ".tif")
    veg_8post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/",m7,"_Mega_",k,  ".tif")
    veg_9post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m8,"_Mega_",k,  ".tif")
    veg_10post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m9,"_Mega_",k,  ".tif")
    veg_11post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m10,"_Mega_",k,  ".tif")
    veg_12post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m11,"_Mega_",k,  ".tif")
    veg_13post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m12,"_Mega_",k,  ".tif")
    veg_14post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m13,"_Mega_",k,  ".tif")
    veg_15post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m14,"_Mega_",k,  ".tif")
    veg_16post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m15,"_Mega_",k,  ".tif")
    veg_17post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m16,"_Mega_",k,  ".tif")
    veg_18post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m17,"_Mega_",k,  ".tif")
    veg_19post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m18,"_Mega_",k,  ".tif")
    veg_20post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m19,"_Mega_",k,  ".tif")
    veg_21post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m20,"_Mega_",k,  ".tif")
    veg_22post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m21,"_Mega_",k,  ".tif")
    veg_23post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m22,"_Mega_",k,  ".tif")
    veg_24post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m23,"_Mega_",k,  ".tif")
    veg_25post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m24,"_Mega_",k,  ".tif")
    veg_26post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m25,"_Mega_",k,  ".tif")
    veg_27post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m26,"_Mega_",k,  ".tif")
    veg_28post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m27,"_Mega_",k,  ".tif")
    veg_29post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m28,"_Mega_",k,  ".tif")
    veg_30post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m29,"_Mega_",k,  ".tif")
    veg_31post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m30,"_Mega_",k,  ".tif")
    veg_32post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m31,"_Mega_",k,  ".tif")
    veg_33post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m32,"_Mega_",k,  ".tif")
    veg_34post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m33,"_Mega_",k,  ".tif")
    veg_35post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m34,"_Mega_",k,  ".tif")
    veg_36post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m35,"_Mega_",k,  ".tif")
    veg_37post_filename <- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/",k,"/", m36,"_Mega_",k,  ".tif")
    
    
    
    veg_raster_pre <- raster(veg_filename)
    veg_raster_1post <- raster(veg_1post_filename)
    # Conditional loading of rasters based on file existence
    
    if (file.exists(veg_2post_filename)) {
      veg_raster_2post <- raster(veg_2post_filename)
    } else {
      warning(paste("File does not exist:", veg_2post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_3post_filename)) {
      veg_raster_3post <- raster(veg_3post_filename)
    } else {
      warning(paste("File does not exist:", veg_3post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_4post_filename)) {
      veg_raster_4post <- raster(veg_4post_filename)
    } else {
      warning(paste("File does not exist:", veg_4post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_5post_filename)) {
      veg_raster_5post <- raster(veg_5post_filename)
    } else {
      warning(paste("File does not exist:", veg_5post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_6post_filename)) {
      veg_raster_6post <- raster(veg_6post_filename)
    } else {
      warning(paste("File does not exist:", veg_6post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_7post_filename)) {
      veg_raster_7post <- raster(veg_7post_filename)
    } else {
      warning(paste("File does not exist:", veg_7post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_8post_filename)) {
      veg_raster_8post <- raster(veg_8post_filename)
    } else {
      warning(paste("File does not exist:", veg_8post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_9post_filename)) {
      veg_raster_9post <- raster(veg_9post_filename)
    } else {
      warning(paste("File does not exist:", veg_9post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_10post_filename)) {
      veg_raster_10post <- raster(veg_10post_filename)
    } else {
      warning(paste("File does not exist:", veg_10post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_11post_filename)) {
      veg_raster_11post <- raster(veg_11post_filename)
    } else {
      warning(paste("File does not exist:", veg_11post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_12post_filename)) {
      veg_raster_12post <- raster(veg_12post_filename)
    } else {
      warning(paste("File does not exist:", veg_12post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_13post_filename)) {
      veg_raster_13post <- raster(veg_13post_filename)
    } else {
      warning(paste("File does not exist:", veg_13post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_14post_filename)) {
      veg_raster_14post <- raster(veg_14post_filename)
    } else {
      warning(paste("File does not exist:", veg_14post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_15post_filename)) {
      veg_raster_15post <- raster(veg_15post_filename)
    } else {
      warning(paste("File does not exist:", veg_15post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_16post_filename)) {
      veg_raster_16post <- raster(veg_16post_filename)
    } else {
      warning(paste("File does not exist:", veg_16post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_17post_filename)) {
      veg_raster_17post <- raster(veg_17post_filename)
    } else {
      warning(paste("File does not exist:", veg_17post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_18post_filename)) {
      veg_raster_18post <- raster(veg_18post_filename)
    } else {
      warning(paste("File does not exist:", veg_18post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_19post_filename)) {
      veg_raster_19post <- raster(veg_19post_filename)
    } else {
      warning(paste("File does not exist:", veg_19post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_20post_filename)) {
      veg_raster_20post <- raster(veg_20post_filename)
    } else {
      warning(paste("File does not exist:", veg_20post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_21post_filename)) {
      veg_raster_21post <- raster(veg_21post_filename)
    } else {
      warning(paste("File does not exist:", veg_21post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_22post_filename)) {
      veg_raster_22post <- raster(veg_22post_filename)
    } else {
      warning(paste("File does not exist:", veg_22post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_23post_filename)) {
      veg_raster_23post <- raster(veg_23post_filename)
    } else {
      warning(paste("File does not exist:", veg_23post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_24post_filename)) {
      veg_raster_24post <- raster(veg_24post_filename)
    } else {
      warning(paste("File does not exist:", veg_24post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_25post_filename)) {
      veg_raster_25post <- raster(veg_25post_filename)
    } else {
      warning(paste("File does not exist:", veg_25post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_26post_filename)) {
      veg_raster_26post <- raster(veg_26post_filename)
    } else {
      warning(paste("File does not exist:", veg_26post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_27post_filename)) {
      veg_raster_27post <- raster(veg_27post_filename)
    } else {
      warning(paste("File does not exist:", veg_27post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_28post_filename)) {
      veg_raster_28post <- raster(veg_28post_filename)
    } else {
      warning(paste("File does not exist:", veg_28post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_29post_filename)) {
      veg_raster_29post <- raster(veg_29post_filename)
    } else {
      warning(paste("File does not exist:", veg_29post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_30post_filename)) {
      veg_raster_30post <- raster(veg_30post_filename)
    } else {
      warning(paste("File does not exist:", veg_30post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_31post_filename)) {
      veg_raster_31post <- raster(veg_31post_filename)
    } else {
      warning(paste("File does not exist:", veg_31post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_32post_filename)) {
      veg_raster_32post <- raster(veg_32post_filename)
    } else {
      warning(paste("File does not exist:", veg_32post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_33post_filename)) {
      veg_raster_33post <- raster(veg_33post_filename)
    } else {
      warning(paste("File does not exist:", veg_33post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_34post_filename)) {
      veg_raster_34post <- raster(veg_34post_filename)
    } else {
      warning(paste("File does not exist:", veg_34post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_35post_filename)) {
      veg_raster_35post <- raster(veg_35post_filename)
    } else {
      warning(paste("File does not exist:", veg_35post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_36post_filename)) {
      veg_raster_36post <- raster(veg_36post_filename)
    } else {
      warning(paste("File does not exist:", veg_36post_filename, "- Skipping loading."))
    }
    
    if (file.exists(veg_37post_filename)){
      veg_raster_37post <- raster(veg_37post_filename)
    } else {
      warning(paste("File does not exist:", veg_37post_filename, "- Skipping loading."))
    }
    
    # For individual fire file, load reclassified rdnbr raster
    raster_name<- paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/RdNBR_classified/",f)
    dnbr_class_raster<- raster(raster_name)
    
    df_r<-as.data.frame(dnbr_class_raster)
    df_r<-na.omit(df_r)

    # Load rdnbr (raw) raster
    raster_name_var<-paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/RdNBR_raw/",f)
      dnbr_raster <- raster(raster_name_var)
      
      # Get patches
      patched_raster <- get_patches(dnbr_class_raster,directions = 8)
      patches_0 <- patched_raster$layer_1$class_0
      patches_1 <- patched_raster$layer_1$class_1
      patches_2 <- patched_raster$layer_1$class_2
      patches_3 <- patched_raster$layer_1$class_3
      
      # Calculate patch metrics
      core_area_patches <- lsm_p_core(
        dnbr_class_raster,
        directions = 8,
        consider_boundary = FALSE,
        edge_depth = 1)
      
      perimeter_area_patches <- lsm_p_para(dnbr_class_raster, directions = 8)
      
      contiguity_patches <- lsm_p_contig(dnbr_class_raster, directions = 8)
      
      perimeter_patches <- lsm_p_perim(dnbr_class_raster, directions = 8)

      
      # To check if there are patches in each severity class:
      # Write patch rasters for every severity class to file
      if(length(patches_3)==0){a <- 0}else {a <- 1}
      if(length(patches_2)==0){b <- 0}else {b <- 1}
      if(length(patches_1)==0){c <- 0}else {c <- 1}
      if(length(patches_0)==0){d <- 0}else {d <- 1}
      
      # Checking if there are patches in each severity class
      if (a!=0){
        
        raster_points_df <- as.data.frame(rasterToPoints(patches_3))
        
        sp_points <- SpatialPointsDataFrame(
          coords = raster_points_df[, 1:2],
          data = data.frame(pixel_value = raster_points_df[, 3]))
        
        patchIDs <- raster_points_df$layer
        rasValue_dnbr <- as.data.frame(raster::extract(dnbr_raster,sp_points))
        rasValue_prism <- as.data.frame(raster::extract(prism_fire_year, sp_points))
        rasValue_elevation <- as.data.frame(raster::extract(elev_raster,sp_points))
        rasvalue_veg<- as.data.frame(raster::extract (veg_raster_pre,sp_points))
        rasValue_aspect <- as.data.frame(raster::extract(aspect_raster, sp_points))
        rasValue_slope <- as.data.frame(raster::extract(slope_raster, sp_points))
        rasValue_prism_1pre <- as.data.frame(raster::extract(prism_1pre,sp_points))
        rasValue_prism_1post <- as.data.frame(raster::extract(prism_1post,sp_points))
        rasValue_prism_1pre_abs <- as.data.frame(raster::extract(prism_1pre_abs,sp_points))
        rasValue_prism_1post_abs <- as.data.frame(raster::extract(prism_1post_abs,sp_points))
        rasValue_prism_abs <- as.data.frame(raster::extract(prism_fire_year_abs, sp_points))
        
        
        # Conditional extraction for each raster variable if it exists
        if (exists("veg_raster_1post")) {
          rasValue_veg_1post <- as.data.frame(raster::extract(veg_raster_1post, sp_points))
        } else {
          warning("veg_raster_1post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_2post")) {
          rasValue_veg_2post <- as.data.frame(raster::extract(veg_raster_2post, sp_points))
        } else {
          warning("veg_raster_2post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_3post")) {
          rasValue_veg_3post <- as.data.frame(raster::extract(veg_raster_3post, sp_points))
        } else {
          warning("veg_raster_3post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_4post")) {
          rasValue_veg_4post <- as.data.frame(raster::extract(veg_raster_4post, sp_points))
        } else {
          warning("veg_raster_4post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_5post")) {
          rasValue_veg_5post <- as.data.frame(raster::extract(veg_raster_5post, sp_points))
        } else {
          warning("veg_raster_5post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_6post")) {
          rasValue_veg_6post <- as.data.frame(raster::extract(veg_raster_6post, sp_points))
        } else {
          warning("veg_raster_6post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_7post")) {
          rasValue_veg_7post <- as.data.frame(raster::extract(veg_raster_7post, sp_points))
        } else {
          warning("veg_raster_7post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_8post")) {
          rasValue_veg_8post <- as.data.frame(raster::extract(veg_raster_8post, sp_points))
        } else {
          warning("veg_raster_8post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_9post")) {
          rasValue_veg_9post <- as.data.frame(raster::extract(veg_raster_9post, sp_points))
        } else {
          warning("veg_raster_9post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_10post")) {
          rasValue_veg_10post <- as.data.frame(raster::extract(veg_raster_10post, sp_points))
        } else {
          warning("veg_raster_10post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_11post")) {
          rasValue_veg_11post <- as.data.frame(raster::extract(veg_raster_11post, sp_points))
        } else {
          warning("veg_raster_11post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_12post")) {
          rasValue_veg_12post <- as.data.frame(raster::extract(veg_raster_12post, sp_points))
        } else {
          warning("veg_raster_12post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_13post")) {
          rasValue_veg_13post <- as.data.frame(raster::extract(veg_raster_13post, sp_points))
        } else {
          warning("veg_raster_13post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_14post")) {
          rasValue_veg_14post <- as.data.frame(raster::extract(veg_raster_14post, sp_points))
        } else {
          warning("veg_raster_14post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_15post")) {
          rasValue_veg_15post <- as.data.frame(raster::extract(veg_raster_15post, sp_points))
        } else {
          warning("veg_raster_15post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_16post")) {
          rasValue_veg_16post <- as.data.frame(raster::extract(veg_raster_16post, sp_points))
        } else {
          warning("veg_raster_16post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_17post")) {
          rasValue_veg_17post <- as.data.frame(raster::extract(veg_raster_17post, sp_points))
        } else {
          warning("veg_raster_17post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_18post")) {
          rasValue_veg_18post <- as.data.frame(raster::extract(veg_raster_18post, sp_points))
        } else {
          warning("veg_raster_18post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_19post")) {
          rasValue_veg_19post <- as.data.frame(raster::extract(veg_raster_19post, sp_points))
        } else {
          warning("veg_raster_19post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_20post")) {
          rasValue_veg_20post <- as.data.frame(raster::extract(veg_raster_20post, sp_points))
        } else {
          warning("veg_raster_20post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_21post")) {
          rasValue_veg_21post <- as.data.frame(raster::extract(veg_raster_21post, sp_points))
        } else {
          warning("veg_raster_21post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_22post")) {
          rasValue_veg_22post <- as.data.frame(raster::extract(veg_raster_22post, sp_points))
        } else {
          warning("veg_raster_22post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_23post")) {
          rasValue_veg_23post <- as.data.frame(raster::extract(veg_raster_23post, sp_points))
        } else {
          warning("veg_raster_23post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_24post")) {
          rasValue_veg_24post <- as.data.frame(raster::extract(veg_raster_24post, sp_points))
        } else {
          warning("veg_raster_24post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_25post")) {
          rasValue_veg_25post <- as.data.frame(raster::extract(veg_raster_25post, sp_points))
        } else {
          warning("veg_raster_25post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_26post")) {
          rasValue_veg_26post <- as.data.frame(raster::extract(veg_raster_26post, sp_points))
        } else {
          warning("veg_raster_26post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_27post")) {
          rasValue_veg_27post <- as.data.frame(raster::extract(veg_raster_27post, sp_points))
        } else {
          warning("veg_raster_27post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_28post")) {
          rasValue_veg_28post <- as.data.frame(raster::extract(veg_raster_28post, sp_points))
        } else {
          warning("veg_raster_28post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_29post")) {
          rasValue_veg_29post <- as.data.frame(raster::extract(veg_raster_29post, sp_points))
        } else {
          warning("veg_raster_29post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_30post")) {
          rasValue_veg_30post <- as.data.frame(raster::extract(veg_raster_30post, sp_points))
        } else {
          warning("veg_raster_30post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_31post")) {
          rasValue_veg_31post <- as.data.frame(raster::extract(veg_raster_31post, sp_points))
        } else {
          warning("veg_raster_31post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_32post")) {
          rasValue_veg_32post <- as.data.frame(raster::extract(veg_raster_32post, sp_points))
        } else {
          warning("veg_raster_32post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_33post")) {
          rasValue_veg_33post <- as.data.frame(raster::extract(veg_raster_33post, sp_points))
        } else {
          warning("veg_raster_33post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_34post")) {
          rasValue_veg_34post <- as.data.frame(raster::extract(veg_raster_34post, sp_points))
        } else {
          warning("veg_raster_34post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_35post")) {
          rasValue_veg_35post <- as.data.frame(raster::extract(veg_raster_35post, sp_points))
        } else {
          warning("veg_raster_35post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_36post")) {
          rasValue_veg_36post <- as.data.frame(raster::extract(veg_raster_36post, sp_points))
        } else {
          warning("veg_raster_36post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_37post")) {
          rasValue_veg_37post <- as.data.frame(raster::extract(veg_raster_37post, sp_points))
        } else {
          warning("veg_raster_37post does not exist - Skipping extraction.")
        }
        
        # List of data frame variable names
        df_names <- c(
          "patchIDs", "rasValue_dnbr", "rasValue_elevation","rasValue_aspect","rasValue_slope",
          "rasValue_prism","rasValue_prism_1pre", "rasValue_prism_1post","rasValue_prism_1pre_abs","rasValue_prism_1post_abs","rasValue_prism_abs",
          "rasvalue_veg", "rasValue_veg_1post", "rasValue_veg_2post", "rasValue_veg_3post", "rasValue_veg_4post", "rasValue_veg_5post", 
          "rasValue_veg_6post", "rasValue_veg_7post", "rasValue_veg_8post", "rasValue_veg_9post", "rasValue_veg_10post", 
          "rasValue_veg_11post", "rasValue_veg_12post", "rasValue_veg_13post", "rasValue_veg_14post", "rasValue_veg_15post", 
          "rasValue_veg_16post", "rasValue_veg_17post", "rasValue_veg_18post", "rasValue_veg_19post", "rasValue_veg_20post", 
          "rasValue_veg_21post", "rasValue_veg_22post", "rasValue_veg_23post", "rasValue_veg_24post", "rasValue_veg_25post", 
          "rasValue_veg_26post", "rasValue_veg_27post", "rasValue_veg_28post", "rasValue_veg_29post", "rasValue_veg_30post", 
          "rasValue_veg_31post", "rasValue_veg_32post", "rasValue_veg_33post", "rasValue_veg_34post", "rasValue_veg_35post", 
          "rasValue_veg_36post", "rasValue_veg_37post"
        )
        
        # Initialize an empty list to collect existing data frames
        existing_dfs <- list()
        
        # Loop through each name and add the existing data frames to the list
        for (name in df_names) {
          if (exists(name)) {
            existing_dfs[[name]] <- get(name)  # Use get() to access the data frame by its name
          } else {
            warning(paste(name, "does not exist - Skipping."))
          }
        }
        
        # Combine the data frames using cbind
        rasValue_master <- do.call(cbind, existing_dfs)
        
        
        # Define the desired column names
        desired_colnames <- c(
          "id","rdnbr","elevation","aspect","slope",
          "tmean","tmax", "tmin","tdmean","vpdmin","vpdmax","ppt","tmean_1pre","tmax_1pre", "tmin_1pre","tdmean_1pre","vpdmin_1pre","vpdmax_1pre",
          "ppt_1pre","tmean_1post","tmax_1post", "tmin_1post","tdmean_1post","vpdmin_1post","vpdmax_1post","ppt_1post","tmean_1pre_abs","tmax_1pre_abs", "tmin_1pre_abs","tdmean_1pre_abs",
          "vpdmin_1pre_abs","vpdmax_1pre_abs","ppt_1pre_abs","tmean_1post_abs","tmax_1post_abs", "tmin_1post_abs","tdmean_1post_abs","vpdmin_1post_abs","vpdmax_1post_abs","ppt_1post_abs",
          "tmean_abs","tmax_abs", "tmin_abs","tdmean_abs","vpdmin_abs","vpdmax_abs","ppt_abs",
          "RF_pre_veg","RF_post1_veg", "RF_post2_veg", "RF_post3_veg", "RF_post4_veg", "RF_post5_veg", 
          "RF_post6_veg", "RF_post7_veg", "RF_post8_veg", "RF_post9_veg", "RF_post10_veg", 
          "RF_post11_veg", "RF_post12_veg", "RF_post13_veg", "RF_post14_veg", "RF_post15_veg", 
          "RF_post16_veg", "RF_post17_veg", "RF_post18_veg", "RF_post19_veg", "RF_post20_veg", 
          "RF_post21_veg", "RF_post22_veg", "RF_post23_veg", "RF_post24_veg", "RF_post25_veg", 
          "RF_post26_veg", "RF_post27_veg", "RF_post28_veg", "RF_post29_veg", "RF_post30_veg", 
          "RF_post31_veg", "RF_post32_veg", "RF_post33_veg", "RF_post34_veg", "RF_post35_veg", 
          "RF_post36_veg", "RF_post37_veg"
        )
        
        # Subset the colnames to match the number of columns in rasValue_master
        existing_colnames <- desired_colnames[seq_len(ncol(rasValue_master))]
        
        # Assign the subset of colnames to rasValue_master
        colnames(rasValue_master) <- existing_colnames
        
        # Print the resulting column names for verification
        print(colnames(rasValue_master))
        
        rasValue_master3 <- rasValue_master
        rasValue_master3$rdnbr_class <- 3
        rasValue_master3$fire_year <- i
        rasValue_master3$OBJECTID <- k
        
        # Summary operations with conditional checks for each column
        mn_ele_3 <- rasValue_master %>%
          group_by(id) %>%  # Grouping by 'id'
          summarise(
            mean_rdnbr = safe_summary(cur_data(), "rdnbr", mean, na.rm = TRUE),
            mode_veg = safe_summary(cur_data(), "RF_pre_veg", function(x) mode(x)),
            n = n(),
            mode_veg_2post = safe_summary(cur_data(), "RF_post2_veg", function(x) mode(x)),
            mean_elevation = safe_summary(cur_data(), "elevation", mean, na.rm = TRUE),
            mean_tmean = safe_summary(cur_data(), "tmean", mean, na.rm = TRUE),
            mean_tmax = safe_summary(cur_data(), "tmax", mean, na.rm = TRUE),
            mean_tmin = safe_summary(cur_data(), "tmin", mean, na.rm = TRUE),
            mean_tdmean = safe_summary(cur_data(), "tdmean", mean, na.rm = TRUE),
            mean_vpdmin = safe_summary(cur_data(), "vpdmin", mean, na.rm = TRUE),
            mean_vpdmax = safe_summary(cur_data(), "vpdmax", mean, na.rm = TRUE),
            mean_ppt = safe_summary(cur_data(), "ppt", mean, na.rm = TRUE),
            mean_aspect = safe_summary(cur_data(), "aspect", mean, na.rm = TRUE),
            mean_slope = safe_summary(cur_data(), "slope", mean, na.rm = TRUE),
            mean_tmean_1pre = safe_summary(cur_data(), "tmean_1pre", mean, na.rm = TRUE),
            mean_tmax_1pre = safe_summary(cur_data(), "tmax_1pre", mean, na.rm = TRUE),
            mean_tmin_1pre = safe_summary(cur_data(), "tmin_1pre", mean, na.rm = TRUE),
            mean_tdmean_1pre = safe_summary(cur_data(), "tdmean_1pre", mean, na.rm = TRUE),
            mean_vpdmin_1pre = safe_summary(cur_data(), "vpdmin_1pre", mean, na.rm = TRUE),
            mean_vpdmax_1pre = safe_summary(cur_data(), "vpdmax_1pre", mean, na.rm = TRUE),
            mean_ppt_1pre = safe_summary(cur_data(), "ppt_1pre", mean, na.rm = TRUE),
            mean_tmean_1post = safe_summary(cur_data(), "tmean_1post", mean, na.rm = TRUE),
            mean_tmax_1post = safe_summary(cur_data(), "tmax_1post", mean, na.rm = TRUE),
            mean_tmin_1post = safe_summary(cur_data(), "tmin_1post", mean, na.rm = TRUE),
            mean_tdmean_1post = safe_summary(cur_data(), "tdmean_1post", mean, na.rm = TRUE),
            mean_vpdmin_1post = safe_summary(cur_data(), "vpdmin_1post", mean, na.rm = TRUE),
            mean_vpdmax_1post = safe_summary(cur_data(), "vpdmax_1post", mean, na.rm = TRUE),
            mean_ppt_1post = safe_summary(cur_data(), "ppt_1post", mean, na.rm = TRUE),
            mean_tmean_1pre_abs = safe_summary(cur_data(), "tmean_1pre_abs", mean, na.rm = TRUE),
            mean_tmax_1pre_abs = safe_summary(cur_data(), "tmax_1pre_abs", mean, na.rm = TRUE),
            mean_tmin_1pre_abs = safe_summary(cur_data(), "tmin_1pre_abs", mean, na.rm = TRUE),
            mean_tdmean_1pre_abs = safe_summary(cur_data(), "tdmean_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmin_1pre_abs = safe_summary(cur_data(), "vpdmin_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmax_1pre_abs = safe_summary(cur_data(), "vpdmax_1pre_abs", mean, na.rm = TRUE),
            mean_ppt_1pre_abs = safe_summary(cur_data(), "ppt_1pre_abs", mean, na.rm = TRUE),
            mean_tmean_1post_abs = safe_summary(cur_data(), "tmean_1post_abs", mean, na.rm = TRUE),
            mean_tmax_1post_abs = safe_summary(cur_data(), "tmax_1post_abs", mean, na.rm = TRUE),
            mean_tmin_1post_abs = safe_summary(cur_data(), "tmin_1post_abs", mean, na.rm = TRUE),
            mean_tdmean_1post_abs = safe_summary(cur_data(), "tdmean_1post_abs", mean, na.rm = TRUE),
            mean_vpdmin_1post_abs = safe_summary(cur_data(), "vpdmin_1post_abs", mean, na.rm = TRUE),
            mean_vpdmax_1post_abs = safe_summary(cur_data(), "vpdmax_1post_abs", mean, na.rm = TRUE),
            mean_ppt_1post_abs = safe_summary(cur_data(), "ppt_1post_abs", mean, na.rm = TRUE),
            mean_tmean_abs = safe_summary(cur_data(), "tmean_abs", mean, na.rm = TRUE),
            mean_tmax_abs = safe_summary(cur_data(), "tmax_abs", mean, na.rm = TRUE),
            mean_tmin_abs = safe_summary(cur_data(), "tmin_abs", mean, na.rm = TRUE),
            mean_tdmean_abs = safe_summary(cur_data(), "tdmean_abs", mean, na.rm = TRUE),
            mean_vpdmin_abs = safe_summary(cur_data(), "vpdmin_abs", mean, na.rm = TRUE),
            mean_vpdmax_abs = safe_summary(cur_data(), "vpdmax_abs", mean, na.rm = TRUE),
            mean_ppt_abs = safe_summary(cur_data(), "ppt_abs", mean, na.rm = TRUE),
            count_RF_pre_veg_1 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 1)),
            count_RF_pre_veg_4 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 4)),
            count_RF_pre_veg_5 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 5)),
            count_RF_pre_veg_7 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 7)),
            count_RF_post1_veg_1 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 1)),
            count_RF_post1_veg_4 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 4)),
            count_RF_post1_veg_5 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 5)),
            count_RF_post1_veg_7 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 7)),
            count_RF_post2_veg_1 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 1)),
            count_RF_post2_veg_4 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 4)),
            count_RF_post2_veg_5 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 5)),
            count_RF_post2_veg_7 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 7)),
            count_RF_post3_veg_1 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 1)),
            count_RF_post3_veg_4 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 4)),
            count_RF_post3_veg_5 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 5)),
            count_RF_post3_veg_7 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 7)),
            count_RF_post4_veg_1 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 1)),
            count_RF_post4_veg_4 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 4)),
            count_RF_post4_veg_5 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 5)),
            count_RF_post4_veg_7 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 7)),
            count_RF_post5_veg_1 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 1)),
            count_RF_post5_veg_4 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 4)),
            count_RF_post5_veg_5 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 5)),
            count_RF_post5_veg_7 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 7)),
            count_RF_post6_veg_1 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 1)),
            count_RF_post6_veg_4 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 4)),
            count_RF_post6_veg_5 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 5)),
            count_RF_post6_veg_7 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 7)),
            count_RF_post7_veg_1 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 1)),
            count_RF_post7_veg_4 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 4)),
            count_RF_post7_veg_5 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 5)),
            count_RF_post7_veg_7 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 7)),
            count_RF_post8_veg_1 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 1)),
            count_RF_post8_veg_4 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 4)),
            count_RF_post8_veg_5 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 5)),
            count_RF_post8_veg_7 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 7)),
            count_RF_post9_veg_1 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 1)),
            count_RF_post9_veg_4 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 4)),
            count_RF_post9_veg_5 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 5)),
            count_RF_post9_veg_7 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 7)),
            count_RF_post10_veg_1 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 1)),
            count_RF_post10_veg_4 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 4)),
            count_RF_post10_veg_5 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 5)),
            count_RF_post10_veg_7 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 7)),
            count_RF_post11_veg_1 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 1)),
            count_RF_post11_veg_4 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 4)),
            count_RF_post11_veg_5 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 5)),
            count_RF_post11_veg_7 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 7)),
            count_RF_post12_veg_1 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 1)),
            count_RF_post12_veg_4 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 4)),
            count_RF_post12_veg_5 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 5)),
            count_RF_post12_veg_7 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 7)),
            count_RF_post13_veg_1 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 1)),
            count_RF_post13_veg_4 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 4)),
            count_RF_post13_veg_5 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 5)),
            count_RF_post13_veg_7 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 7)),
            count_RF_post14_veg_1 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 1)),
            count_RF_post14_veg_4 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 4)),
            count_RF_post14_veg_5 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 5)),
            count_RF_post14_veg_7 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 7)),
            count_RF_post15_veg_1 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 1)),
            count_RF_post15_veg_4 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 4)),
            count_RF_post15_veg_5 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 5)),
            count_RF_post15_veg_7 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 7)),
            count_RF_post16_veg_1 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 1)),
            count_RF_post16_veg_4 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 4)),
            count_RF_post16_veg_5 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 5)),
            count_RF_post16_veg_7 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 7)),
            count_RF_post17_veg_1 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 1)),
            count_RF_post17_veg_4 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 4)),
            count_RF_post17_veg_5 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 5)),
            count_RF_post17_veg_7 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 7)),
            count_RF_post18_veg_1 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 1)),
            count_RF_post18_veg_4 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 4)),
            count_RF_post18_veg_5 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 5)),
            count_RF_post18_veg_7 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 7)),
            count_RF_post19_veg_1 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 1)),
            count_RF_post19_veg_4 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 4)),
            count_RF_post19_veg_5 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 5)),
            count_RF_post19_veg_7 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 7)),
            count_RF_post20_veg_1 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 1)),
            count_RF_post20_veg_4 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 4)),
            count_RF_post20_veg_5 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 5)),
            count_RF_post20_veg_7 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 7)),
            count_RF_post21_veg_1 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 1)),
            count_RF_post21_veg_4 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 4)),
            count_RF_post21_veg_5 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 5)),
            count_RF_post21_veg_7 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 7)),
            count_RF_post22_veg_1 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 1)),
            count_RF_post22_veg_4 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 4)),
            count_RF_post22_veg_5 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 5)),
            count_RF_post22_veg_7 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 7)),
            count_RF_post23_veg_1 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 1)),
            count_RF_post23_veg_4 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 4)),
            count_RF_post23_veg_5 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 5)),
            count_RF_post23_veg_7 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 7)),
            count_RF_post24_veg_1 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 1)),
            count_RF_post24_veg_4 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 4)),
            count_RF_post24_veg_5 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 5)),
            count_RF_post24_veg_7 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 7)),
            count_RF_post25_veg_1 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 1)),
            count_RF_post25_veg_4 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 4)),
            count_RF_post25_veg_5 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 5)),
            count_RF_post25_veg_7 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 7)),
            count_RF_post26_veg_1 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 1)),
            count_RF_post26_veg_4 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 4)),
            count_RF_post26_veg_5 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 5)),
            count_RF_post26_veg_7 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 7)),
            count_RF_post27_veg_1 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 1)),
            count_RF_post27_veg_4 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 4)),
            count_RF_post27_veg_5 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 5)),
            count_RF_post27_veg_7 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 7)),
            count_RF_post28_veg_1 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 1)),
            count_RF_post28_veg_4 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 4)),
            count_RF_post28_veg_5 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 5)),
            count_RF_post28_veg_7 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 7)),
            count_RF_post29_veg_1 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 1)),
            count_RF_post29_veg_4 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 4)),
            count_RF_post29_veg_5 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 5)),
            count_RF_post29_veg_7 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 7)),
            count_RF_post30_veg_1 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 1)),
            count_RF_post30_veg_4 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 4)),
            count_RF_post30_veg_5 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 5)),
            count_RF_post30_veg_7 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 7)),
            count_RF_post31_veg_1 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 1)),
            count_RF_post31_veg_4 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 4)),
            count_RF_post31_veg_5 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 5)),
            count_RF_post31_veg_7 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 7)),
            count_RF_post32_veg_1 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 1)),
            count_RF_post32_veg_4 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 4)),
            count_RF_post32_veg_5 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 5)),
            count_RF_post32_veg_7 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 7)),
            count_RF_post33_veg_1 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 1)),
            count_RF_post33_veg_4 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 4)),
            count_RF_post33_veg_5 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 5)),
            count_RF_post33_veg_7 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 7)),
            count_RF_post34_veg_1 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 1)),
            count_RF_post34_veg_4 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 4)),
            count_RF_post34_veg_5 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 5)),
            count_RF_post34_veg_7 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 7)),
            count_RF_post35_veg_1 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 1)),
            count_RF_post35_veg_4 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 4)),
            count_RF_post35_veg_5 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 5)),
            count_RF_post35_veg_7 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 7)),
            count_RF_post36_veg_1 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 1)),
            count_RF_post36_veg_4 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 4)),
            count_RF_post36_veg_5 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 5)),
            count_RF_post36_veg_7 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 7)),
            count_RF_post37_veg_1 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 1)),
            count_RF_post37_veg_4 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 4)),
            count_RF_post37_veg_5 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 5)),
            count_RF_post37_veg_7 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 7))
          )
        
        
        mn_ele_3$rdnbr_class <- 3
        mn_ele_3$fire_year <- i
        mn_ele_3$OBJECTID <- k
        
        core_area_patches_3 <- core_area_patches %>%
          filter(class==3) %>%
          rename(core_area = value) %>%
          select(core_area,id)
        
        perimeter_area_patches_3 <- perimeter_area_patches %>%
          filter(class==3) %>%
          rename(perimeter_area = value)%>%
          select(perimeter_area,id)
        
        perimeter_patches_3 <- perimeter_patches %>%
          filter(class==3) %>%
          rename(perimeter = value)%>%
          select(perimeter,id)
        
        contiguity_patches_3 <- contiguity_patches %>%
          filter(class==3) %>%
          rename(contiguity = value)%>%
          select(contiguity,id)
        
        mn_ele_3 <- left_join(mn_ele_3, core_area_patches_3, by = "id") 
        mn_ele_3 <- left_join(mn_ele_3, perimeter_area_patches_3, by = "id") 
        mn_ele_3 <- left_join(mn_ele_3, perimeter_patches_3, by = "id") 
        mn_ele_3 <- left_join(mn_ele_3, contiguity_patches_3, by = "id") 
      }
      
      if (b!=0){
        
        raster_points_df <- as.data.frame(rasterToPoints(patches_2))
        
        sp_points <- SpatialPointsDataFrame(
          coords = raster_points_df[, 1:2],
          data = data.frame(pixel_value = raster_points_df[, 3]))
        
        patchIDs <- raster_points_df$layer
        rasValue_dnbr <- as.data.frame(raster::extract(dnbr_raster,sp_points))
        rasValue_prism <- as.data.frame(raster::extract(prism_fire_year, sp_points))
        rasValue_elevation <- as.data.frame(raster::extract(elev_raster,sp_points))
        rasvalue_veg<- as.data.frame(raster::extract (veg_raster_pre,sp_points))
        rasValue_aspect <- as.data.frame(raster::extract(aspect_raster, sp_points))
        rasValue_slope <- as.data.frame(raster::extract(slope_raster, sp_points))
        rasValue_prism_1pre <- as.data.frame(raster::extract(prism_1pre,sp_points))
        rasValue_prism_1post <- as.data.frame(raster::extract(prism_1post,sp_points))
        rasValue_prism_1pre_abs <- as.data.frame(raster::extract(prism_1pre_abs,sp_points))
        rasValue_prism_1post_abs <- as.data.frame(raster::extract(prism_1post_abs,sp_points))
        rasValue_prism_abs <- as.data.frame(raster::extract(prism_fire_year_abs, sp_points))
        
        
        # Conditional extraction for each raster variable if it exists
        if (exists("veg_raster_1post")) {
          rasValue_veg_1post <- as.data.frame(raster::extract(veg_raster_1post, sp_points))
        } else {
          warning("veg_raster_1post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_2post")) {
          rasValue_veg_2post <- as.data.frame(raster::extract(veg_raster_2post, sp_points))
        } else {
          warning("veg_raster_2post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_3post")) {
          rasValue_veg_3post <- as.data.frame(raster::extract(veg_raster_3post, sp_points))
        } else {
          warning("veg_raster_3post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_4post")) {
          rasValue_veg_4post <- as.data.frame(raster::extract(veg_raster_4post, sp_points))
        } else {
          warning("veg_raster_4post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_5post")) {
          rasValue_veg_5post <- as.data.frame(raster::extract(veg_raster_5post, sp_points))
        } else {
          warning("veg_raster_5post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_6post")) {
          rasValue_veg_6post <- as.data.frame(raster::extract(veg_raster_6post, sp_points))
        } else {
          warning("veg_raster_6post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_7post")) {
          rasValue_veg_7post <- as.data.frame(raster::extract(veg_raster_7post, sp_points))
        } else {
          warning("veg_raster_7post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_8post")) {
          rasValue_veg_8post <- as.data.frame(raster::extract(veg_raster_8post, sp_points))
        } else {
          warning("veg_raster_8post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_9post")) {
          rasValue_veg_9post <- as.data.frame(raster::extract(veg_raster_9post, sp_points))
        } else {
          warning("veg_raster_9post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_10post")) {
          rasValue_veg_10post <- as.data.frame(raster::extract(veg_raster_10post, sp_points))
        } else {
          warning("veg_raster_10post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_11post")) {
          rasValue_veg_11post <- as.data.frame(raster::extract(veg_raster_11post, sp_points))
        } else {
          warning("veg_raster_11post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_12post")) {
          rasValue_veg_12post <- as.data.frame(raster::extract(veg_raster_12post, sp_points))
        } else {
          warning("veg_raster_12post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_13post")) {
          rasValue_veg_13post <- as.data.frame(raster::extract(veg_raster_13post, sp_points))
        } else {
          warning("veg_raster_13post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_14post")) {
          rasValue_veg_14post <- as.data.frame(raster::extract(veg_raster_14post, sp_points))
        } else {
          warning("veg_raster_14post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_15post")) {
          rasValue_veg_15post <- as.data.frame(raster::extract(veg_raster_15post, sp_points))
        } else {
          warning("veg_raster_15post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_16post")) {
          rasValue_veg_16post <- as.data.frame(raster::extract(veg_raster_16post, sp_points))
        } else {
          warning("veg_raster_16post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_17post")) {
          rasValue_veg_17post <- as.data.frame(raster::extract(veg_raster_17post, sp_points))
        } else {
          warning("veg_raster_17post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_18post")) {
          rasValue_veg_18post <- as.data.frame(raster::extract(veg_raster_18post, sp_points))
        } else {
          warning("veg_raster_18post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_19post")) {
          rasValue_veg_19post <- as.data.frame(raster::extract(veg_raster_19post, sp_points))
        } else {
          warning("veg_raster_19post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_20post")) {
          rasValue_veg_20post <- as.data.frame(raster::extract(veg_raster_20post, sp_points))
        } else {
          warning("veg_raster_20post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_21post")) {
          rasValue_veg_21post <- as.data.frame(raster::extract(veg_raster_21post, sp_points))
        } else {
          warning("veg_raster_21post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_22post")) {
          rasValue_veg_22post <- as.data.frame(raster::extract(veg_raster_22post, sp_points))
        } else {
          warning("veg_raster_22post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_23post")) {
          rasValue_veg_23post <- as.data.frame(raster::extract(veg_raster_23post, sp_points))
        } else {
          warning("veg_raster_23post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_24post")) {
          rasValue_veg_24post <- as.data.frame(raster::extract(veg_raster_24post, sp_points))
        } else {
          warning("veg_raster_24post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_25post")) {
          rasValue_veg_25post <- as.data.frame(raster::extract(veg_raster_25post, sp_points))
        } else {
          warning("veg_raster_25post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_26post")) {
          rasValue_veg_26post <- as.data.frame(raster::extract(veg_raster_26post, sp_points))
        } else {
          warning("veg_raster_26post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_27post")) {
          rasValue_veg_27post <- as.data.frame(raster::extract(veg_raster_27post, sp_points))
        } else {
          warning("veg_raster_27post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_28post")) {
          rasValue_veg_28post <- as.data.frame(raster::extract(veg_raster_28post, sp_points))
        } else {
          warning("veg_raster_28post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_29post")) {
          rasValue_veg_29post <- as.data.frame(raster::extract(veg_raster_29post, sp_points))
        } else {
          warning("veg_raster_29post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_30post")) {
          rasValue_veg_30post <- as.data.frame(raster::extract(veg_raster_30post, sp_points))
        } else {
          warning("veg_raster_30post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_31post")) {
          rasValue_veg_31post <- as.data.frame(raster::extract(veg_raster_31post, sp_points))
        } else {
          warning("veg_raster_31post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_32post")) {
          rasValue_veg_32post <- as.data.frame(raster::extract(veg_raster_32post, sp_points))
        } else {
          warning("veg_raster_32post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_33post")) {
          rasValue_veg_33post <- as.data.frame(raster::extract(veg_raster_33post, sp_points))
        } else {
          warning("veg_raster_33post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_34post")) {
          rasValue_veg_34post <- as.data.frame(raster::extract(veg_raster_34post, sp_points))
        } else {
          warning("veg_raster_34post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_35post")) {
          rasValue_veg_35post <- as.data.frame(raster::extract(veg_raster_35post, sp_points))
        } else {
          warning("veg_raster_35post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_36post")) {
          rasValue_veg_36post <- as.data.frame(raster::extract(veg_raster_36post, sp_points))
        } else {
          warning("veg_raster_36post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_37post")) {
          rasValue_veg_37post <- as.data.frame(raster::extract(veg_raster_37post, sp_points))
        } else {
          warning("veg_raster_37post does not exist - Skipping extraction.")
        }
        
        # List of data frame variable names
        df_names <- c(
          "patchIDs", "rasValue_dnbr", "rasValue_elevation","rasValue_aspect","rasValue_slope",
          "rasValue_prism","rasValue_prism_1pre", "rasValue_prism_1post","rasValue_prism_1pre_abs","rasValue_prism_1post_abs","rasValue_prism_abs",
          "rasvalue_veg", "rasValue_veg_1post", "rasValue_veg_2post", "rasValue_veg_3post", "rasValue_veg_4post", "rasValue_veg_5post", 
          "rasValue_veg_6post", "rasValue_veg_7post", "rasValue_veg_8post", "rasValue_veg_9post", "rasValue_veg_10post", 
          "rasValue_veg_11post", "rasValue_veg_12post", "rasValue_veg_13post", "rasValue_veg_14post", "rasValue_veg_15post", 
          "rasValue_veg_16post", "rasValue_veg_17post", "rasValue_veg_18post", "rasValue_veg_19post", "rasValue_veg_20post", 
          "rasValue_veg_21post", "rasValue_veg_22post", "rasValue_veg_23post", "rasValue_veg_24post", "rasValue_veg_25post", 
          "rasValue_veg_26post", "rasValue_veg_27post", "rasValue_veg_28post", "rasValue_veg_29post", "rasValue_veg_30post", 
          "rasValue_veg_31post", "rasValue_veg_32post", "rasValue_veg_33post", "rasValue_veg_34post", "rasValue_veg_35post", 
          "rasValue_veg_36post", "rasValue_veg_37post"
        )
        
        # Initialize an empty list to collect existing data frames
        existing_dfs <- list()
        
        # Loop through each name and add the existing data frames to the list
        for (name in df_names) {
          if (exists(name)) {
            existing_dfs[[name]] <- get(name)  # Use get() to access the data frame by its name
          } else {
            warning(paste(name, "does not exist - Skipping."))
          }
        }
        
        # Combine the data frames using cbind
        rasValue_master <- do.call(cbind, existing_dfs)
        
        
        # Define the desired column names
        desired_colnames <- c(
          "id","rdnbr","elevation","aspect","slope",
          "tmean","tmax", "tmin","tdmean","vpdmin","vpdmax","ppt","tmean_1pre","tmax_1pre", "tmin_1pre","tdmean_1pre","vpdmin_1pre","vpdmax_1pre",
          "ppt_1pre","tmean_1post","tmax_1post", "tmin_1post","tdmean_1post","vpdmin_1post","vpdmax_1post","ppt_1post","tmean_1pre_abs","tmax_1pre_abs", "tmin_1pre_abs","tdmean_1pre_abs",
          "vpdmin_1pre_abs","vpdmax_1pre_abs","ppt_1pre_abs","tmean_1post_abs","tmax_1post_abs", "tmin_1post_abs","tdmean_1post_abs","vpdmin_1post_abs","vpdmax_1post_abs","ppt_1post_abs",
          "tmean_abs","tmax_abs", "tmin_abs","tdmean_abs","vpdmin_abs","vpdmax_abs","ppt_abs",
          "RF_pre_veg","RF_post1_veg", "RF_post2_veg", "RF_post3_veg", "RF_post4_veg", "RF_post5_veg", 
          "RF_post6_veg", "RF_post7_veg", "RF_post8_veg", "RF_post9_veg", "RF_post10_veg", 
          "RF_post11_veg", "RF_post12_veg", "RF_post13_veg", "RF_post14_veg", "RF_post15_veg", 
          "RF_post16_veg", "RF_post17_veg", "RF_post18_veg", "RF_post19_veg", "RF_post20_veg", 
          "RF_post21_veg", "RF_post22_veg", "RF_post23_veg", "RF_post24_veg", "RF_post25_veg", 
          "RF_post26_veg", "RF_post27_veg", "RF_post28_veg", "RF_post29_veg", "RF_post30_veg", 
          "RF_post31_veg", "RF_post32_veg", "RF_post33_veg", "RF_post34_veg", "RF_post35_veg", 
          "RF_post36_veg", "RF_post37_veg"
        )
        
        # Subset the colnames to match the number of columns in rasValue_master
        existing_colnames <- desired_colnames[seq_len(ncol(rasValue_master))]
        
        # Assign the subset of colnames to rasValue_master
        colnames(rasValue_master) <- existing_colnames
        
        # Print the resulting column names for verification
        print(colnames(rasValue_master))
        
        rasValue_master2 <- rasValue_master
        rasValue_master2$rdnbr_class <- 2
        rasValue_master2$fire_year <- i
        rasValue_master2$OBJECTID <- k
        
        
        # Summary operations with conditional checks for each column
        mn_ele_2 <- rasValue_master %>%
          group_by(id) %>%  # Grouping by 'id'
          summarise(
            mean_rdnbr = safe_summary(cur_data(), "rdnbr", mean, na.rm = TRUE),
            mode_veg = safe_summary(cur_data(), "RF_pre_veg", function(x) mode(x)),
            n = n(),
            mode_veg_2post = safe_summary(cur_data(), "RF_post2_veg", function(x) mode(x)),
            mean_elevation = safe_summary(cur_data(), "elevation", mean, na.rm = TRUE),
            mean_tmean = safe_summary(cur_data(), "tmean", mean, na.rm = TRUE),
            mean_tmax = safe_summary(cur_data(), "tmax", mean, na.rm = TRUE),
            mean_tmin = safe_summary(cur_data(), "tmin", mean, na.rm = TRUE),
            mean_tdmean = safe_summary(cur_data(), "tdmean", mean, na.rm = TRUE),
            mean_vpdmin = safe_summary(cur_data(), "vpdmin", mean, na.rm = TRUE),
            mean_vpdmax = safe_summary(cur_data(), "vpdmax", mean, na.rm = TRUE),
            mean_ppt = safe_summary(cur_data(), "ppt", mean, na.rm = TRUE),
            mean_aspect = safe_summary(cur_data(), "aspect", mean, na.rm = TRUE),
            mean_slope = safe_summary(cur_data(), "slope", mean, na.rm = TRUE),
            mean_tmean_1pre = safe_summary(cur_data(), "tmean_1pre", mean, na.rm = TRUE),
            mean_tmax_1pre = safe_summary(cur_data(), "tmax_1pre", mean, na.rm = TRUE),
            mean_tmin_1pre = safe_summary(cur_data(), "tmin_1pre", mean, na.rm = TRUE),
            mean_tdmean_1pre = safe_summary(cur_data(), "tdmean_1pre", mean, na.rm = TRUE),
            mean_vpdmin_1pre = safe_summary(cur_data(), "vpdmin_1pre", mean, na.rm = TRUE),
            mean_vpdmax_1pre = safe_summary(cur_data(), "vpdmax_1pre", mean, na.rm = TRUE),
            mean_ppt_1pre = safe_summary(cur_data(), "ppt_1pre", mean, na.rm = TRUE),
            mean_tmean_1post = safe_summary(cur_data(), "tmean_1post", mean, na.rm = TRUE),
            mean_tmax_1post = safe_summary(cur_data(), "tmax_1post", mean, na.rm = TRUE),
            mean_tmin_1post = safe_summary(cur_data(), "tmin_1post", mean, na.rm = TRUE),
            mean_tdmean_1post = safe_summary(cur_data(), "tdmean_1post", mean, na.rm = TRUE),
            mean_vpdmin_1post = safe_summary(cur_data(), "vpdmin_1post", mean, na.rm = TRUE),
            mean_vpdmax_1post = safe_summary(cur_data(), "vpdmax_1post", mean, na.rm = TRUE),
            mean_ppt_1post = safe_summary(cur_data(), "ppt_1post", mean, na.rm = TRUE),
            mean_tmean_1pre_abs = safe_summary(cur_data(), "tmean_1pre_abs", mean, na.rm = TRUE),
            mean_tmax_1pre_abs = safe_summary(cur_data(), "tmax_1pre_abs", mean, na.rm = TRUE),
            mean_tmin_1pre_abs = safe_summary(cur_data(), "tmin_1pre_abs", mean, na.rm = TRUE),
            mean_tdmean_1pre_abs = safe_summary(cur_data(), "tdmean_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmin_1pre_abs = safe_summary(cur_data(), "vpdmin_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmax_1pre_abs = safe_summary(cur_data(), "vpdmax_1pre_abs", mean, na.rm = TRUE),
            mean_ppt_1pre_abs = safe_summary(cur_data(), "ppt_1pre_abs", mean, na.rm = TRUE),
            mean_tmean_1post_abs = safe_summary(cur_data(), "tmean_1post_abs", mean, na.rm = TRUE),
            mean_tmax_1post_abs = safe_summary(cur_data(), "tmax_1post_abs", mean, na.rm = TRUE),
            mean_tmin_1post_abs = safe_summary(cur_data(), "tmin_1post_abs", mean, na.rm = TRUE),
            mean_tdmean_1post_abs = safe_summary(cur_data(), "tdmean_1post_abs", mean, na.rm = TRUE),
            mean_vpdmin_1post_abs = safe_summary(cur_data(), "vpdmin_1post_abs", mean, na.rm = TRUE),
            mean_vpdmax_1post_abs = safe_summary(cur_data(), "vpdmax_1post_abs", mean, na.rm = TRUE),
            mean_ppt_1post_abs = safe_summary(cur_data(), "ppt_1post_abs", mean, na.rm = TRUE),
            mean_tmean_abs = safe_summary(cur_data(), "tmean_abs", mean, na.rm = TRUE),
            mean_tmax_abs = safe_summary(cur_data(), "tmax_abs", mean, na.rm = TRUE),
            mean_tmin_abs = safe_summary(cur_data(), "tmin_abs", mean, na.rm = TRUE),
            mean_tdmean_abs = safe_summary(cur_data(), "tdmean_abs", mean, na.rm = TRUE),
            mean_vpdmin_abs = safe_summary(cur_data(), "vpdmin_abs", mean, na.rm = TRUE),
            mean_vpdmax_abs = safe_summary(cur_data(), "vpdmax_abs", mean, na.rm = TRUE),
            mean_ppt_abs = safe_summary(cur_data(), "ppt_abs", mean, na.rm = TRUE),
            count_RF_pre_veg_1 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 1)),
            count_RF_pre_veg_4 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 4)),
            count_RF_pre_veg_5 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 5)),
            count_RF_pre_veg_7 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 7)),
            count_RF_post1_veg_1 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 1)),
            count_RF_post1_veg_4 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 4)),
            count_RF_post1_veg_5 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 5)),
            count_RF_post1_veg_7 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 7)),
            count_RF_post2_veg_1 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 1)),
            count_RF_post2_veg_4 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 4)),
            count_RF_post2_veg_5 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 5)),
            count_RF_post2_veg_7 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 7)),
            count_RF_post3_veg_1 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 1)),
            count_RF_post3_veg_4 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 4)),
            count_RF_post3_veg_5 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 5)),
            count_RF_post3_veg_7 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 7)),
            count_RF_post4_veg_1 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 1)),
            count_RF_post4_veg_4 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 4)),
            count_RF_post4_veg_5 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 5)),
            count_RF_post4_veg_7 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 7)),
            count_RF_post5_veg_1 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 1)),
            count_RF_post5_veg_4 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 4)),
            count_RF_post5_veg_5 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 5)),
            count_RF_post5_veg_7 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 7)),
            count_RF_post6_veg_1 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 1)),
            count_RF_post6_veg_4 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 4)),
            count_RF_post6_veg_5 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 5)),
            count_RF_post6_veg_7 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 7)),
            count_RF_post7_veg_1 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 1)),
            count_RF_post7_veg_4 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 4)),
            count_RF_post7_veg_5 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 5)),
            count_RF_post7_veg_7 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 7)),
            count_RF_post8_veg_1 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 1)),
            count_RF_post8_veg_4 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 4)),
            count_RF_post8_veg_5 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 5)),
            count_RF_post8_veg_7 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 7)),
            count_RF_post9_veg_1 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 1)),
            count_RF_post9_veg_4 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 4)),
            count_RF_post9_veg_5 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 5)),
            count_RF_post9_veg_7 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 7)),
            count_RF_post10_veg_1 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 1)),
            count_RF_post10_veg_4 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 4)),
            count_RF_post10_veg_5 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 5)),
            count_RF_post10_veg_7 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 7)),
            count_RF_post11_veg_1 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 1)),
            count_RF_post11_veg_4 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 4)),
            count_RF_post11_veg_5 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 5)),
            count_RF_post11_veg_7 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 7)),
            count_RF_post12_veg_1 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 1)),
            count_RF_post12_veg_4 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 4)),
            count_RF_post12_veg_5 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 5)),
            count_RF_post12_veg_7 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 7)),
            count_RF_post13_veg_1 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 1)),
            count_RF_post13_veg_4 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 4)),
            count_RF_post13_veg_5 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 5)),
            count_RF_post13_veg_7 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 7)),
            count_RF_post14_veg_1 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 1)),
            count_RF_post14_veg_4 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 4)),
            count_RF_post14_veg_5 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 5)),
            count_RF_post14_veg_7 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 7)),
            count_RF_post15_veg_1 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 1)),
            count_RF_post15_veg_4 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 4)),
            count_RF_post15_veg_5 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 5)),
            count_RF_post15_veg_7 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 7)),
            count_RF_post16_veg_1 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 1)),
            count_RF_post16_veg_4 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 4)),
            count_RF_post16_veg_5 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 5)),
            count_RF_post16_veg_7 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 7)),
            count_RF_post17_veg_1 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 1)),
            count_RF_post17_veg_4 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 4)),
            count_RF_post17_veg_5 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 5)),
            count_RF_post17_veg_7 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 7)),
            count_RF_post18_veg_1 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 1)),
            count_RF_post18_veg_4 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 4)),
            count_RF_post18_veg_5 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 5)),
            count_RF_post18_veg_7 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 7)),
            count_RF_post19_veg_1 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 1)),
            count_RF_post19_veg_4 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 4)),
            count_RF_post19_veg_5 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 5)),
            count_RF_post19_veg_7 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 7)),
            count_RF_post20_veg_1 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 1)),
            count_RF_post20_veg_4 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 4)),
            count_RF_post20_veg_5 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 5)),
            count_RF_post20_veg_7 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 7)),
            count_RF_post21_veg_1 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 1)),
            count_RF_post21_veg_4 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 4)),
            count_RF_post21_veg_5 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 5)),
            count_RF_post21_veg_7 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 7)),
            count_RF_post22_veg_1 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 1)),
            count_RF_post22_veg_4 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 4)),
            count_RF_post22_veg_5 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 5)),
            count_RF_post22_veg_7 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 7)),
            count_RF_post23_veg_1 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 1)),
            count_RF_post23_veg_4 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 4)),
            count_RF_post23_veg_5 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 5)),
            count_RF_post23_veg_7 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 7)),
            count_RF_post24_veg_1 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 1)),
            count_RF_post24_veg_4 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 4)),
            count_RF_post24_veg_5 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 5)),
            count_RF_post24_veg_7 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 7)),
            count_RF_post25_veg_1 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 1)),
            count_RF_post25_veg_4 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 4)),
            count_RF_post25_veg_5 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 5)),
            count_RF_post25_veg_7 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 7)),
            count_RF_post26_veg_1 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 1)),
            count_RF_post26_veg_4 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 4)),
            count_RF_post26_veg_5 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 5)),
            count_RF_post26_veg_7 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 7)),
            count_RF_post27_veg_1 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 1)),
            count_RF_post27_veg_4 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 4)),
            count_RF_post27_veg_5 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 5)),
            count_RF_post27_veg_7 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 7)),
            count_RF_post28_veg_1 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 1)),
            count_RF_post28_veg_4 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 4)),
            count_RF_post28_veg_5 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 5)),
            count_RF_post28_veg_7 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 7)),
            count_RF_post29_veg_1 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 1)),
            count_RF_post29_veg_4 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 4)),
            count_RF_post29_veg_5 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 5)),
            count_RF_post29_veg_7 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 7)),
            count_RF_post30_veg_1 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 1)),
            count_RF_post30_veg_4 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 4)),
            count_RF_post30_veg_5 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 5)),
            count_RF_post30_veg_7 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 7)),
            count_RF_post31_veg_1 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 1)),
            count_RF_post31_veg_4 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 4)),
            count_RF_post31_veg_5 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 5)),
            count_RF_post31_veg_7 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 7)),
            count_RF_post32_veg_1 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 1)),
            count_RF_post32_veg_4 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 4)),
            count_RF_post32_veg_5 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 5)),
            count_RF_post32_veg_7 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 7)),
            count_RF_post33_veg_1 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 1)),
            count_RF_post33_veg_4 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 4)),
            count_RF_post33_veg_5 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 5)),
            count_RF_post33_veg_7 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 7)),
            count_RF_post34_veg_1 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 1)),
            count_RF_post34_veg_4 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 4)),
            count_RF_post34_veg_5 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 5)),
            count_RF_post34_veg_7 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 7)),
            count_RF_post35_veg_1 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 1)),
            count_RF_post35_veg_4 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 4)),
            count_RF_post35_veg_5 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 5)),
            count_RF_post35_veg_7 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 7)),
            count_RF_post36_veg_1 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 1)),
            count_RF_post36_veg_4 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 4)),
            count_RF_post36_veg_5 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 5)),
            count_RF_post36_veg_7 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 7)),
            count_RF_post37_veg_1 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 1)),
            count_RF_post37_veg_4 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 4)),
            count_RF_post37_veg_5 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 5)),
            count_RF_post37_veg_7 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 7))
          )
        
        mn_ele_2$rdnbr_class <- 2
        mn_ele_2$fire_year <- i
        mn_ele_2$OBJECTID <- k
        
        core_area_patches_2 <- core_area_patches %>%
          filter(class==2) %>%
          rename(core_area = value) %>%
          select(core_area,id)
        
        perimeter_area_patches_2 <- perimeter_area_patches %>%
          filter(class==2) %>%
          rename(perimeter_area = value)%>%
          select(perimeter_area,id)
        
        perimeter_patches_2 <- perimeter_patches %>%
          filter(class==2) %>%
          rename(perimeter = value)%>%
          select(perimeter,id)
        
        contiguity_patches_2 <- contiguity_patches %>%
          filter(class==2) %>%
          rename(contiguity = value)%>%
          select(contiguity,id)
        
        mn_ele_2 <- left_join(mn_ele_2, core_area_patches_2, by = "id") 
        mn_ele_2 <- left_join(mn_ele_2, perimeter_area_patches_2, by = "id") 
        mn_ele_2 <- left_join(mn_ele_2, perimeter_patches_2, by = "id") 
        mn_ele_2 <- left_join(mn_ele_2, contiguity_patches_2, by = "id") 
      }
      
      if (c!=0){
        
        raster_points_df <- as.data.frame(rasterToPoints(patches_1))
        
        sp_points <- SpatialPointsDataFrame(
          coords = raster_points_df[, 1:2],
          data = data.frame(pixel_value = raster_points_df[, 3]))
        
        patchIDs <- raster_points_df$layer
        rasValue_dnbr <- as.data.frame(raster::extract(dnbr_raster,sp_points))
        rasValue_prism <- as.data.frame(raster::extract(prism_fire_year, sp_points))
        rasValue_elevation <- as.data.frame(raster::extract(elev_raster,sp_points))
        rasvalue_veg<- as.data.frame(raster::extract (veg_raster_pre,sp_points))
        rasValue_aspect <- as.data.frame(raster::extract(aspect_raster, sp_points))
        rasValue_slope <- as.data.frame(raster::extract(slope_raster, sp_points))
        rasValue_prism_1pre <- as.data.frame(raster::extract(prism_1pre,sp_points))
        rasValue_prism_1post <- as.data.frame(raster::extract(prism_1post,sp_points))
        rasValue_prism_1pre_abs <- as.data.frame(raster::extract(prism_1pre_abs,sp_points))
        rasValue_prism_1post_abs <- as.data.frame(raster::extract(prism_1post_abs,sp_points))
        rasValue_prism_abs <- as.data.frame(raster::extract(prism_fire_year_abs, sp_points))
        
        
        # Conditional extraction for each raster variable if it exists
        if (exists("veg_raster_1post")) {
          rasValue_veg_1post <- as.data.frame(raster::extract(veg_raster_1post, sp_points))
        } else {
          warning("veg_raster_1post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_2post")) {
          rasValue_veg_2post <- as.data.frame(raster::extract(veg_raster_2post, sp_points))
        } else {
          warning("veg_raster_2post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_3post")) {
          rasValue_veg_3post <- as.data.frame(raster::extract(veg_raster_3post, sp_points))
        } else {
          warning("veg_raster_3post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_4post")) {
          rasValue_veg_4post <- as.data.frame(raster::extract(veg_raster_4post, sp_points))
        } else {
          warning("veg_raster_4post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_5post")) {
          rasValue_veg_5post <- as.data.frame(raster::extract(veg_raster_5post, sp_points))
        } else {
          warning("veg_raster_5post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_6post")) {
          rasValue_veg_6post <- as.data.frame(raster::extract(veg_raster_6post, sp_points))
        } else {
          warning("veg_raster_6post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_7post")) {
          rasValue_veg_7post <- as.data.frame(raster::extract(veg_raster_7post, sp_points))
        } else {
          warning("veg_raster_7post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_8post")) {
          rasValue_veg_8post <- as.data.frame(raster::extract(veg_raster_8post, sp_points))
        } else {
          warning("veg_raster_8post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_9post")) {
          rasValue_veg_9post <- as.data.frame(raster::extract(veg_raster_9post, sp_points))
        } else {
          warning("veg_raster_9post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_10post")) {
          rasValue_veg_10post <- as.data.frame(raster::extract(veg_raster_10post, sp_points))
        } else {
          warning("veg_raster_10post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_11post")) {
          rasValue_veg_11post <- as.data.frame(raster::extract(veg_raster_11post, sp_points))
        } else {
          warning("veg_raster_11post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_12post")) {
          rasValue_veg_12post <- as.data.frame(raster::extract(veg_raster_12post, sp_points))
        } else {
          warning("veg_raster_12post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_13post")) {
          rasValue_veg_13post <- as.data.frame(raster::extract(veg_raster_13post, sp_points))
        } else {
          warning("veg_raster_13post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_14post")) {
          rasValue_veg_14post <- as.data.frame(raster::extract(veg_raster_14post, sp_points))
        } else {
          warning("veg_raster_14post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_15post")) {
          rasValue_veg_15post <- as.data.frame(raster::extract(veg_raster_15post, sp_points))
        } else {
          warning("veg_raster_15post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_16post")) {
          rasValue_veg_16post <- as.data.frame(raster::extract(veg_raster_16post, sp_points))
        } else {
          warning("veg_raster_16post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_17post")) {
          rasValue_veg_17post <- as.data.frame(raster::extract(veg_raster_17post, sp_points))
        } else {
          warning("veg_raster_17post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_18post")) {
          rasValue_veg_18post <- as.data.frame(raster::extract(veg_raster_18post, sp_points))
        } else {
          warning("veg_raster_18post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_19post")) {
          rasValue_veg_19post <- as.data.frame(raster::extract(veg_raster_19post, sp_points))
        } else {
          warning("veg_raster_19post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_20post")) {
          rasValue_veg_20post <- as.data.frame(raster::extract(veg_raster_20post, sp_points))
        } else {
          warning("veg_raster_20post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_21post")) {
          rasValue_veg_21post <- as.data.frame(raster::extract(veg_raster_21post, sp_points))
        } else {
          warning("veg_raster_21post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_22post")) {
          rasValue_veg_22post <- as.data.frame(raster::extract(veg_raster_22post, sp_points))
        } else {
          warning("veg_raster_22post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_23post")) {
          rasValue_veg_23post <- as.data.frame(raster::extract(veg_raster_23post, sp_points))
        } else {
          warning("veg_raster_23post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_24post")) {
          rasValue_veg_24post <- as.data.frame(raster::extract(veg_raster_24post, sp_points))
        } else {
          warning("veg_raster_24post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_25post")) {
          rasValue_veg_25post <- as.data.frame(raster::extract(veg_raster_25post, sp_points))
        } else {
          warning("veg_raster_25post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_26post")) {
          rasValue_veg_26post <- as.data.frame(raster::extract(veg_raster_26post, sp_points))
        } else {
          warning("veg_raster_26post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_27post")) {
          rasValue_veg_27post <- as.data.frame(raster::extract(veg_raster_27post, sp_points))
        } else {
          warning("veg_raster_27post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_28post")) {
          rasValue_veg_28post <- as.data.frame(raster::extract(veg_raster_28post, sp_points))
        } else {
          warning("veg_raster_28post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_29post")) {
          rasValue_veg_29post <- as.data.frame(raster::extract(veg_raster_29post, sp_points))
        } else {
          warning("veg_raster_29post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_30post")) {
          rasValue_veg_30post <- as.data.frame(raster::extract(veg_raster_30post, sp_points))
        } else {
          warning("veg_raster_30post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_31post")) {
          rasValue_veg_31post <- as.data.frame(raster::extract(veg_raster_31post, sp_points))
        } else {
          warning("veg_raster_31post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_32post")) {
          rasValue_veg_32post <- as.data.frame(raster::extract(veg_raster_32post, sp_points))
        } else {
          warning("veg_raster_32post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_33post")) {
          rasValue_veg_33post <- as.data.frame(raster::extract(veg_raster_33post, sp_points))
        } else {
          warning("veg_raster_33post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_34post")) {
          rasValue_veg_34post <- as.data.frame(raster::extract(veg_raster_34post, sp_points))
        } else {
          warning("veg_raster_34post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_35post")) {
          rasValue_veg_35post <- as.data.frame(raster::extract(veg_raster_35post, sp_points))
        } else {
          warning("veg_raster_35post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_36post")) {
          rasValue_veg_36post <- as.data.frame(raster::extract(veg_raster_36post, sp_points))
        } else {
          warning("veg_raster_36post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_37post")) {
          rasValue_veg_37post <- as.data.frame(raster::extract(veg_raster_37post, sp_points))
        } else {
          warning("veg_raster_37post does not exist - Skipping extraction.")
        }
        
        # List of data frame variable names
        df_names <- c(
          "patchIDs", "rasValue_dnbr", "rasValue_elevation","rasValue_aspect","rasValue_slope",
          "rasValue_prism","rasValue_prism_1pre", "rasValue_prism_1post","rasValue_prism_1pre_abs","rasValue_prism_1post_abs","rasValue_prism_abs",
          "rasvalue_veg", "rasValue_veg_1post", "rasValue_veg_2post", "rasValue_veg_3post", "rasValue_veg_4post", "rasValue_veg_5post", 
          "rasValue_veg_6post", "rasValue_veg_7post", "rasValue_veg_8post", "rasValue_veg_9post", "rasValue_veg_10post", 
          "rasValue_veg_11post", "rasValue_veg_12post", "rasValue_veg_13post", "rasValue_veg_14post", "rasValue_veg_15post", 
          "rasValue_veg_16post", "rasValue_veg_17post", "rasValue_veg_18post", "rasValue_veg_19post", "rasValue_veg_20post", 
          "rasValue_veg_21post", "rasValue_veg_22post", "rasValue_veg_23post", "rasValue_veg_24post", "rasValue_veg_25post", 
          "rasValue_veg_26post", "rasValue_veg_27post", "rasValue_veg_28post", "rasValue_veg_29post", "rasValue_veg_30post", 
          "rasValue_veg_31post", "rasValue_veg_32post", "rasValue_veg_33post", "rasValue_veg_34post", "rasValue_veg_35post", 
          "rasValue_veg_36post", "rasValue_veg_37post"
        )
        
        # Initialize an empty list to collect existing data frames
        existing_dfs <- list()
        
        # Loop through each name and add the existing data frames to the list
        for (name in df_names) {
          if (exists(name)) {
            existing_dfs[[name]] <- get(name)  # Use get() to access the data frame by its name
          } else {
            warning(paste(name, "does not exist - Skipping."))
          }
        }
        
        # Combine the data frames using cbind
        rasValue_master <- do.call(cbind, existing_dfs)
        
        
        # Define the desired column names
        desired_colnames <- c(
          "id","rdnbr","elevation","aspect","slope",
          "tmean","tmax", "tmin","tdmean","vpdmin","vpdmax","ppt","tmean_1pre","tmax_1pre", "tmin_1pre","tdmean_1pre","vpdmin_1pre","vpdmax_1pre",
          "ppt_1pre","tmean_1post","tmax_1post", "tmin_1post","tdmean_1post","vpdmin_1post","vpdmax_1post","ppt_1post","tmean_1pre_abs","tmax_1pre_abs", "tmin_1pre_abs","tdmean_1pre_abs",
          "vpdmin_1pre_abs","vpdmax_1pre_abs","ppt_1pre_abs","tmean_1post_abs","tmax_1post_abs", "tmin_1post_abs","tdmean_1post_abs","vpdmin_1post_abs","vpdmax_1post_abs","ppt_1post_abs",
          "tmean_abs","tmax_abs", "tmin_abs","tdmean_abs","vpdmin_abs","vpdmax_abs","ppt_abs",
          "RF_pre_veg","RF_post1_veg", "RF_post2_veg", "RF_post3_veg", "RF_post4_veg", "RF_post5_veg", 
          "RF_post6_veg", "RF_post7_veg", "RF_post8_veg", "RF_post9_veg", "RF_post10_veg", 
          "RF_post11_veg", "RF_post12_veg", "RF_post13_veg", "RF_post14_veg", "RF_post15_veg", 
          "RF_post16_veg", "RF_post17_veg", "RF_post18_veg", "RF_post19_veg", "RF_post20_veg", 
          "RF_post21_veg", "RF_post22_veg", "RF_post23_veg", "RF_post24_veg", "RF_post25_veg", 
          "RF_post26_veg", "RF_post27_veg", "RF_post28_veg", "RF_post29_veg", "RF_post30_veg", 
          "RF_post31_veg", "RF_post32_veg", "RF_post33_veg", "RF_post34_veg", "RF_post35_veg", 
          "RF_post36_veg", "RF_post37_veg"
        )
        
        # Subset the colnames to match the number of columns in rasValue_master
        existing_colnames <- desired_colnames[seq_len(ncol(rasValue_master))]
        
        # Assign the subset of colnames to rasValue_master
        colnames(rasValue_master) <- existing_colnames
        
        # Print the resulting column names for verification
        print(colnames(rasValue_master))
        
        rasValue_master1 <- rasValue_master
        rasValue_master1$rdnbr_class <- 1
        rasValue_master1$fire_year <- i
        rasValue_master1$OBJECTID <- k
        
        
        # Summary operations with conditional checks for each column
        mn_ele_1 <- rasValue_master %>%
          group_by(id) %>%  # Grouping by 'id'
          summarise(
            mean_rdnbr = safe_summary(cur_data(), "rdnbr", mean, na.rm = TRUE),
            mode_veg = safe_summary(cur_data(), "RF_pre_veg", function(x) mode(x)),
            n = n(),
            mode_veg_2post = safe_summary(cur_data(), "RF_post2_veg", function(x) mode(x)),
            mean_elevation = safe_summary(cur_data(), "elevation", mean, na.rm = TRUE),
            mean_tmean = safe_summary(cur_data(), "tmean", mean, na.rm = TRUE),
            mean_tmax = safe_summary(cur_data(), "tmax", mean, na.rm = TRUE),
            mean_tmin = safe_summary(cur_data(), "tmin", mean, na.rm = TRUE),
            mean_tdmean = safe_summary(cur_data(), "tdmean", mean, na.rm = TRUE),
            mean_vpdmin = safe_summary(cur_data(), "vpdmin", mean, na.rm = TRUE),
            mean_vpdmax = safe_summary(cur_data(), "vpdmax", mean, na.rm = TRUE),
            mean_ppt = safe_summary(cur_data(), "ppt", mean, na.rm = TRUE),
            mean_aspect = safe_summary(cur_data(), "aspect", mean, na.rm = TRUE),
            mean_slope = safe_summary(cur_data(), "slope", mean, na.rm = TRUE),
            mean_tmean_1pre = safe_summary(cur_data(), "tmean_1pre", mean, na.rm = TRUE),
            mean_tmax_1pre = safe_summary(cur_data(), "tmax_1pre", mean, na.rm = TRUE),
            mean_tmin_1pre = safe_summary(cur_data(), "tmin_1pre", mean, na.rm = TRUE),
            mean_tdmean_1pre = safe_summary(cur_data(), "tdmean_1pre", mean, na.rm = TRUE),
            mean_vpdmin_1pre = safe_summary(cur_data(), "vpdmin_1pre", mean, na.rm = TRUE),
            mean_vpdmax_1pre = safe_summary(cur_data(), "vpdmax_1pre", mean, na.rm = TRUE),
            mean_ppt_1pre = safe_summary(cur_data(), "ppt_1pre", mean, na.rm = TRUE),
            mean_tmean_1post = safe_summary(cur_data(), "tmean_1post", mean, na.rm = TRUE),
            mean_tmax_1post = safe_summary(cur_data(), "tmax_1post", mean, na.rm = TRUE),
            mean_tmin_1post = safe_summary(cur_data(), "tmin_1post", mean, na.rm = TRUE),
            mean_tdmean_1post = safe_summary(cur_data(), "tdmean_1post", mean, na.rm = TRUE),
            mean_vpdmin_1post = safe_summary(cur_data(), "vpdmin_1post", mean, na.rm = TRUE),
            mean_vpdmax_1post = safe_summary(cur_data(), "vpdmax_1post", mean, na.rm = TRUE),
            mean_ppt_1post = safe_summary(cur_data(), "ppt_1post", mean, na.rm = TRUE),
            mean_tmean_1pre_abs = safe_summary(cur_data(), "tmean_1pre_abs", mean, na.rm = TRUE),
            mean_tmax_1pre_abs = safe_summary(cur_data(), "tmax_1pre_abs", mean, na.rm = TRUE),
            mean_tmin_1pre_abs = safe_summary(cur_data(), "tmin_1pre_abs", mean, na.rm = TRUE),
            mean_tdmean_1pre_abs = safe_summary(cur_data(), "tdmean_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmin_1pre_abs = safe_summary(cur_data(), "vpdmin_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmax_1pre_abs = safe_summary(cur_data(), "vpdmax_1pre_abs", mean, na.rm = TRUE),
            mean_ppt_1pre_abs = safe_summary(cur_data(), "ppt_1pre_abs", mean, na.rm = TRUE),
            mean_tmean_1post_abs = safe_summary(cur_data(), "tmean_1post_abs", mean, na.rm = TRUE),
            mean_tmax_1post_abs = safe_summary(cur_data(), "tmax_1post_abs", mean, na.rm = TRUE),
            mean_tmin_1post_abs = safe_summary(cur_data(), "tmin_1post_abs", mean, na.rm = TRUE),
            mean_tdmean_1post_abs = safe_summary(cur_data(), "tdmean_1post_abs", mean, na.rm = TRUE),
            mean_vpdmin_1post_abs = safe_summary(cur_data(), "vpdmin_1post_abs", mean, na.rm = TRUE),
            mean_vpdmax_1post_abs = safe_summary(cur_data(), "vpdmax_1post_abs", mean, na.rm = TRUE),
            mean_ppt_1post_abs = safe_summary(cur_data(), "ppt_1post_abs", mean, na.rm = TRUE),
            mean_tmean_abs = safe_summary(cur_data(), "tmean_abs", mean, na.rm = TRUE),
            mean_tmax_abs = safe_summary(cur_data(), "tmax_abs", mean, na.rm = TRUE),
            mean_tmin_abs = safe_summary(cur_data(), "tmin_abs", mean, na.rm = TRUE),
            mean_tdmean_abs = safe_summary(cur_data(), "tdmean_abs", mean, na.rm = TRUE),
            mean_vpdmin_abs = safe_summary(cur_data(), "vpdmin_abs", mean, na.rm = TRUE),
            mean_vpdmax_abs = safe_summary(cur_data(), "vpdmax_abs", mean, na.rm = TRUE),
            mean_ppt_abs = safe_summary(cur_data(), "ppt_abs", mean, na.rm = TRUE),
            count_RF_pre_veg_1 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 1)),
            count_RF_pre_veg_4 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 4)),
            count_RF_pre_veg_5 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 5)),
            count_RF_pre_veg_7 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 7)),
            count_RF_post1_veg_1 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 1)),
            count_RF_post1_veg_4 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 4)),
            count_RF_post1_veg_5 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 5)),
            count_RF_post1_veg_7 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 7)),
            count_RF_post2_veg_1 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 1)),
            count_RF_post2_veg_4 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 4)),
            count_RF_post2_veg_5 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 5)),
            count_RF_post2_veg_7 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 7)),
            count_RF_post3_veg_1 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 1)),
            count_RF_post3_veg_4 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 4)),
            count_RF_post3_veg_5 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 5)),
            count_RF_post3_veg_7 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 7)),
            count_RF_post4_veg_1 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 1)),
            count_RF_post4_veg_4 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 4)),
            count_RF_post4_veg_5 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 5)),
            count_RF_post4_veg_7 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 7)),
            count_RF_post5_veg_1 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 1)),
            count_RF_post5_veg_4 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 4)),
            count_RF_post5_veg_5 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 5)),
            count_RF_post5_veg_7 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 7)),
            count_RF_post6_veg_1 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 1)),
            count_RF_post6_veg_4 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 4)),
            count_RF_post6_veg_5 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 5)),
            count_RF_post6_veg_7 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 7)),
            count_RF_post7_veg_1 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 1)),
            count_RF_post7_veg_4 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 4)),
            count_RF_post7_veg_5 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 5)),
            count_RF_post7_veg_7 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 7)),
            count_RF_post8_veg_1 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 1)),
            count_RF_post8_veg_4 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 4)),
            count_RF_post8_veg_5 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 5)),
            count_RF_post8_veg_7 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 7)),
            count_RF_post9_veg_1 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 1)),
            count_RF_post9_veg_4 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 4)),
            count_RF_post9_veg_5 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 5)),
            count_RF_post9_veg_7 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 7)),
            count_RF_post10_veg_1 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 1)),
            count_RF_post10_veg_4 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 4)),
            count_RF_post10_veg_5 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 5)),
            count_RF_post10_veg_7 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 7)),
            count_RF_post11_veg_1 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 1)),
            count_RF_post11_veg_4 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 4)),
            count_RF_post11_veg_5 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 5)),
            count_RF_post11_veg_7 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 7)),
            count_RF_post12_veg_1 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 1)),
            count_RF_post12_veg_4 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 4)),
            count_RF_post12_veg_5 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 5)),
            count_RF_post12_veg_7 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 7)),
            count_RF_post13_veg_1 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 1)),
            count_RF_post13_veg_4 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 4)),
            count_RF_post13_veg_5 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 5)),
            count_RF_post13_veg_7 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 7)),
            count_RF_post14_veg_1 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 1)),
            count_RF_post14_veg_4 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 4)),
            count_RF_post14_veg_5 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 5)),
            count_RF_post14_veg_7 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 7)),
            count_RF_post15_veg_1 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 1)),
            count_RF_post15_veg_4 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 4)),
            count_RF_post15_veg_5 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 5)),
            count_RF_post15_veg_7 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 7)),
            count_RF_post16_veg_1 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 1)),
            count_RF_post16_veg_4 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 4)),
            count_RF_post16_veg_5 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 5)),
            count_RF_post16_veg_7 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 7)),
            count_RF_post17_veg_1 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 1)),
            count_RF_post17_veg_4 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 4)),
            count_RF_post17_veg_5 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 5)),
            count_RF_post17_veg_7 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 7)),
            count_RF_post18_veg_1 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 1)),
            count_RF_post18_veg_4 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 4)),
            count_RF_post18_veg_5 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 5)),
            count_RF_post18_veg_7 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 7)),
            count_RF_post19_veg_1 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 1)),
            count_RF_post19_veg_4 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 4)),
            count_RF_post19_veg_5 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 5)),
            count_RF_post19_veg_7 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 7)),
            count_RF_post20_veg_1 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 1)),
            count_RF_post20_veg_4 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 4)),
            count_RF_post20_veg_5 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 5)),
            count_RF_post20_veg_7 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 7)),
            count_RF_post21_veg_1 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 1)),
            count_RF_post21_veg_4 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 4)),
            count_RF_post21_veg_5 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 5)),
            count_RF_post21_veg_7 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 7)),
            count_RF_post22_veg_1 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 1)),
            count_RF_post22_veg_4 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 4)),
            count_RF_post22_veg_5 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 5)),
            count_RF_post22_veg_7 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 7)),
            count_RF_post23_veg_1 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 1)),
            count_RF_post23_veg_4 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 4)),
            count_RF_post23_veg_5 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 5)),
            count_RF_post23_veg_7 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 7)),
            count_RF_post24_veg_1 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 1)),
            count_RF_post24_veg_4 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 4)),
            count_RF_post24_veg_5 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 5)),
            count_RF_post24_veg_7 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 7)),
            count_RF_post25_veg_1 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 1)),
            count_RF_post25_veg_4 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 4)),
            count_RF_post25_veg_5 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 5)),
            count_RF_post25_veg_7 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 7)),
            count_RF_post26_veg_1 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 1)),
            count_RF_post26_veg_4 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 4)),
            count_RF_post26_veg_5 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 5)),
            count_RF_post26_veg_7 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 7)),
            count_RF_post27_veg_1 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 1)),
            count_RF_post27_veg_4 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 4)),
            count_RF_post27_veg_5 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 5)),
            count_RF_post27_veg_7 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 7)),
            count_RF_post28_veg_1 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 1)),
            count_RF_post28_veg_4 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 4)),
            count_RF_post28_veg_5 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 5)),
            count_RF_post28_veg_7 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 7)),
            count_RF_post29_veg_1 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 1)),
            count_RF_post29_veg_4 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 4)),
            count_RF_post29_veg_5 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 5)),
            count_RF_post29_veg_7 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 7)),
            count_RF_post30_veg_1 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 1)),
            count_RF_post30_veg_4 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 4)),
            count_RF_post30_veg_5 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 5)),
            count_RF_post30_veg_7 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 7)),
            count_RF_post31_veg_1 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 1)),
            count_RF_post31_veg_4 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 4)),
            count_RF_post31_veg_5 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 5)),
            count_RF_post31_veg_7 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 7)),
            count_RF_post32_veg_1 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 1)),
            count_RF_post32_veg_4 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 4)),
            count_RF_post32_veg_5 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 5)),
            count_RF_post32_veg_7 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 7)),
            count_RF_post33_veg_1 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 1)),
            count_RF_post33_veg_4 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 4)),
            count_RF_post33_veg_5 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 5)),
            count_RF_post33_veg_7 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 7)),
            count_RF_post34_veg_1 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 1)),
            count_RF_post34_veg_4 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 4)),
            count_RF_post34_veg_5 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 5)),
            count_RF_post34_veg_7 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 7)),
            count_RF_post35_veg_1 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 1)),
            count_RF_post35_veg_4 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 4)),
            count_RF_post35_veg_5 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 5)),
            count_RF_post35_veg_7 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 7)),
            count_RF_post36_veg_1 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 1)),
            count_RF_post36_veg_4 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 4)),
            count_RF_post36_veg_5 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 5)),
            count_RF_post36_veg_7 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 7)),
            count_RF_post37_veg_1 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 1)),
            count_RF_post37_veg_4 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 4)),
            count_RF_post37_veg_5 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 5)),
            count_RF_post37_veg_7 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 7))
          )
        mn_ele_1$rdnbr_class <- 1
        mn_ele_1$fire_year <- i
        mn_ele_1$OBJECTID <- k
        
        core_area_patches_1 <- core_area_patches %>%
          filter(class==1) %>%
          rename(core_area = value) %>%
          select(core_area,id)
        
        perimeter_area_patches_1 <- perimeter_area_patches %>%
          filter(class==1) %>%
          rename(perimeter_area = value)%>%
          select(perimeter_area,id)
        
        perimeter_patches_1 <- perimeter_patches %>%
          filter(class==1) %>%
          rename(perimeter = value)%>%
          select(perimeter,id)
        
        contiguity_patches_1 <- contiguity_patches %>%
          filter(class==1) %>%
          rename(contiguity = value)%>%
          select(contiguity,id)
        
        mn_ele_1 <- left_join(mn_ele_1, core_area_patches_1, by = "id") 
        mn_ele_1 <- left_join(mn_ele_1, perimeter_area_patches_1, by = "id") 
        mn_ele_1 <- left_join(mn_ele_1, perimeter_patches_1, by = "id") 
        mn_ele_1 <- left_join(mn_ele_1, contiguity_patches_1, by = "id") 
      }
      
      if (d!=0){
        
        raster_points_df <- as.data.frame(rasterToPoints(patches_0))
        
        sp_points <- SpatialPointsDataFrame(
          coords = raster_points_df[, 1:2],
          data = data.frame(pixel_value = raster_points_df[, 3]))
        
        patchIDs <- raster_points_df$layer
        rasValue_dnbr <- as.data.frame(raster::extract(dnbr_raster,sp_points))
        rasValue_prism <- as.data.frame(raster::extract(prism_fire_year, sp_points))
        rasValue_elevation <- as.data.frame(raster::extract(elev_raster,sp_points))
        rasvalue_veg<- as.data.frame(raster::extract (veg_raster_pre,sp_points))
        rasValue_aspect <- as.data.frame(raster::extract(aspect_raster, sp_points))
        rasValue_slope <- as.data.frame(raster::extract(slope_raster, sp_points))
        rasValue_prism_1pre <- as.data.frame(raster::extract(prism_1pre,sp_points))
        rasValue_prism_1post <- as.data.frame(raster::extract(prism_1post,sp_points))
        rasValue_prism_1pre_abs <- as.data.frame(raster::extract(prism_1pre_abs,sp_points))
        rasValue_prism_1post_abs <- as.data.frame(raster::extract(prism_1post_abs,sp_points))
        rasValue_prism_abs <- as.data.frame(raster::extract(prism_fire_year_abs, sp_points))
        
        
        # Conditional extraction for each raster variable if it exists
        if (exists("veg_raster_1post")) {
          rasValue_veg_1post <- as.data.frame(raster::extract(veg_raster_1post, sp_points))
        } else {
          warning("veg_raster_1post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_2post")) {
          rasValue_veg_2post <- as.data.frame(raster::extract(veg_raster_2post, sp_points))
        } else {
          warning("veg_raster_2post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_3post")) {
          rasValue_veg_3post <- as.data.frame(raster::extract(veg_raster_3post, sp_points))
        } else {
          warning("veg_raster_3post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_4post")) {
          rasValue_veg_4post <- as.data.frame(raster::extract(veg_raster_4post, sp_points))
        } else {
          warning("veg_raster_4post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_5post")) {
          rasValue_veg_5post <- as.data.frame(raster::extract(veg_raster_5post, sp_points))
        } else {
          warning("veg_raster_5post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_6post")) {
          rasValue_veg_6post <- as.data.frame(raster::extract(veg_raster_6post, sp_points))
        } else {
          warning("veg_raster_6post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_7post")) {
          rasValue_veg_7post <- as.data.frame(raster::extract(veg_raster_7post, sp_points))
        } else {
          warning("veg_raster_7post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_8post")) {
          rasValue_veg_8post <- as.data.frame(raster::extract(veg_raster_8post, sp_points))
        } else {
          warning("veg_raster_8post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_9post")) {
          rasValue_veg_9post <- as.data.frame(raster::extract(veg_raster_9post, sp_points))
        } else {
          warning("veg_raster_9post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_10post")) {
          rasValue_veg_10post <- as.data.frame(raster::extract(veg_raster_10post, sp_points))
        } else {
          warning("veg_raster_10post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_11post")) {
          rasValue_veg_11post <- as.data.frame(raster::extract(veg_raster_11post, sp_points))
        } else {
          warning("veg_raster_11post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_12post")) {
          rasValue_veg_12post <- as.data.frame(raster::extract(veg_raster_12post, sp_points))
        } else {
          warning("veg_raster_12post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_13post")) {
          rasValue_veg_13post <- as.data.frame(raster::extract(veg_raster_13post, sp_points))
        } else {
          warning("veg_raster_13post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_14post")) {
          rasValue_veg_14post <- as.data.frame(raster::extract(veg_raster_14post, sp_points))
        } else {
          warning("veg_raster_14post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_15post")) {
          rasValue_veg_15post <- as.data.frame(raster::extract(veg_raster_15post, sp_points))
        } else {
          warning("veg_raster_15post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_16post")) {
          rasValue_veg_16post <- as.data.frame(raster::extract(veg_raster_16post, sp_points))
        } else {
          warning("veg_raster_16post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_17post")) {
          rasValue_veg_17post <- as.data.frame(raster::extract(veg_raster_17post, sp_points))
        } else {
          warning("veg_raster_17post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_18post")) {
          rasValue_veg_18post <- as.data.frame(raster::extract(veg_raster_18post, sp_points))
        } else {
          warning("veg_raster_18post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_19post")) {
          rasValue_veg_19post <- as.data.frame(raster::extract(veg_raster_19post, sp_points))
        } else {
          warning("veg_raster_19post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_20post")) {
          rasValue_veg_20post <- as.data.frame(raster::extract(veg_raster_20post, sp_points))
        } else {
          warning("veg_raster_20post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_21post")) {
          rasValue_veg_21post <- as.data.frame(raster::extract(veg_raster_21post, sp_points))
        } else {
          warning("veg_raster_21post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_22post")) {
          rasValue_veg_22post <- as.data.frame(raster::extract(veg_raster_22post, sp_points))
        } else {
          warning("veg_raster_22post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_23post")) {
          rasValue_veg_23post <- as.data.frame(raster::extract(veg_raster_23post, sp_points))
        } else {
          warning("veg_raster_23post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_24post")) {
          rasValue_veg_24post <- as.data.frame(raster::extract(veg_raster_24post, sp_points))
        } else {
          warning("veg_raster_24post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_25post")) {
          rasValue_veg_25post <- as.data.frame(raster::extract(veg_raster_25post, sp_points))
        } else {
          warning("veg_raster_25post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_26post")) {
          rasValue_veg_26post <- as.data.frame(raster::extract(veg_raster_26post, sp_points))
        } else {
          warning("veg_raster_26post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_27post")) {
          rasValue_veg_27post <- as.data.frame(raster::extract(veg_raster_27post, sp_points))
        } else {
          warning("veg_raster_27post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_28post")) {
          rasValue_veg_28post <- as.data.frame(raster::extract(veg_raster_28post, sp_points))
        } else {
          warning("veg_raster_28post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_29post")) {
          rasValue_veg_29post <- as.data.frame(raster::extract(veg_raster_29post, sp_points))
        } else {
          warning("veg_raster_29post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_30post")) {
          rasValue_veg_30post <- as.data.frame(raster::extract(veg_raster_30post, sp_points))
        } else {
          warning("veg_raster_30post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_31post")) {
          rasValue_veg_31post <- as.data.frame(raster::extract(veg_raster_31post, sp_points))
        } else {
          warning("veg_raster_31post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_32post")) {
          rasValue_veg_32post <- as.data.frame(raster::extract(veg_raster_32post, sp_points))
        } else {
          warning("veg_raster_32post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_33post")) {
          rasValue_veg_33post <- as.data.frame(raster::extract(veg_raster_33post, sp_points))
        } else {
          warning("veg_raster_33post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_34post")) {
          rasValue_veg_34post <- as.data.frame(raster::extract(veg_raster_34post, sp_points))
        } else {
          warning("veg_raster_34post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_35post")) {
          rasValue_veg_35post <- as.data.frame(raster::extract(veg_raster_35post, sp_points))
        } else {
          warning("veg_raster_35post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_36post")) {
          rasValue_veg_36post <- as.data.frame(raster::extract(veg_raster_36post, sp_points))
        } else {
          warning("veg_raster_36post does not exist - Skipping extraction.")
        }
        
        if (exists("veg_raster_37post")) {
          rasValue_veg_37post <- as.data.frame(raster::extract(veg_raster_37post, sp_points))
        } else {
          warning("veg_raster_37post does not exist - Skipping extraction.")
        }
        
        # List of data frame variable names
        df_names <- c(
          "patchIDs", "rasValue_dnbr", "rasValue_elevation","rasValue_aspect","rasValue_slope",
          "rasValue_prism","rasValue_prism_1pre", "rasValue_prism_1post","rasValue_prism_1pre_abs","rasValue_prism_1post_abs","rasValue_prism_abs",
          "rasvalue_veg", "rasValue_veg_1post", "rasValue_veg_2post", "rasValue_veg_3post", "rasValue_veg_4post", "rasValue_veg_5post", 
          "rasValue_veg_6post", "rasValue_veg_7post", "rasValue_veg_8post", "rasValue_veg_9post", "rasValue_veg_10post", 
          "rasValue_veg_11post", "rasValue_veg_12post", "rasValue_veg_13post", "rasValue_veg_14post", "rasValue_veg_15post", 
          "rasValue_veg_16post", "rasValue_veg_17post", "rasValue_veg_18post", "rasValue_veg_19post", "rasValue_veg_20post", 
          "rasValue_veg_21post", "rasValue_veg_22post", "rasValue_veg_23post", "rasValue_veg_24post", "rasValue_veg_25post", 
          "rasValue_veg_26post", "rasValue_veg_27post", "rasValue_veg_28post", "rasValue_veg_29post", "rasValue_veg_30post", 
          "rasValue_veg_31post", "rasValue_veg_32post", "rasValue_veg_33post", "rasValue_veg_34post", "rasValue_veg_35post", 
          "rasValue_veg_36post", "rasValue_veg_37post"
        )
        
        # Initialize an empty list to collect existing data frames
        existing_dfs <- list()
        
        # Loop through each name and add the existing data frames to the list
        for (name in df_names) {
          if (exists(name)) {
            existing_dfs[[name]] <- get(name)  # Use get() to access the data frame by its name
          } else {
            warning(paste(name, "does not exist - Skipping."))
          }
        }
        
        # Combine the data frames using cbind
        rasValue_master <- do.call(cbind, existing_dfs)
        
        
        # Define the desired column names
        desired_colnames <- c(
          "id","rdnbr","elevation","aspect","slope",
          "tmean","tmax", "tmin","tdmean","vpdmin","vpdmax","ppt","tmean_1pre","tmax_1pre", "tmin_1pre","tdmean_1pre","vpdmin_1pre","vpdmax_1pre",
          "ppt_1pre","tmean_1post","tmax_1post", "tmin_1post","tdmean_1post","vpdmin_1post","vpdmax_1post","ppt_1post","tmean_1pre_abs","tmax_1pre_abs", "tmin_1pre_abs","tdmean_1pre_abs",
          "vpdmin_1pre_abs","vpdmax_1pre_abs","ppt_1pre_abs","tmean_1post_abs","tmax_1post_abs", "tmin_1post_abs","tdmean_1post_abs","vpdmin_1post_abs","vpdmax_1post_abs","ppt_1post_abs",
          "tmean_abs","tmax_abs", "tmin_abs","tdmean_abs","vpdmin_abs","vpdmax_abs","ppt_abs",
          "RF_pre_veg","RF_post1_veg", "RF_post2_veg", "RF_post3_veg", "RF_post4_veg", "RF_post5_veg", 
          "RF_post6_veg", "RF_post7_veg", "RF_post8_veg", "RF_post9_veg", "RF_post10_veg", 
          "RF_post11_veg", "RF_post12_veg", "RF_post13_veg", "RF_post14_veg", "RF_post15_veg", 
          "RF_post16_veg", "RF_post17_veg", "RF_post18_veg", "RF_post19_veg", "RF_post20_veg", 
          "RF_post21_veg", "RF_post22_veg", "RF_post23_veg", "RF_post24_veg", "RF_post25_veg", 
          "RF_post26_veg", "RF_post27_veg", "RF_post28_veg", "RF_post29_veg", "RF_post30_veg", 
          "RF_post31_veg", "RF_post32_veg", "RF_post33_veg", "RF_post34_veg", "RF_post35_veg", 
          "RF_post36_veg", "RF_post37_veg"
        )
        
        # Subset the colnames to match the number of columns in rasValue_master
        existing_colnames <- desired_colnames[seq_len(ncol(rasValue_master))]
        
        # Assign the subset of colnames to rasValue_master
        colnames(rasValue_master) <- existing_colnames
        
        # Print the resulting column names for verification
        print(colnames(rasValue_master))
        
        rasValue_master0 <- rasValue_master
        rasValue_master0$rdnbr_class <- 0
        rasValue_master0$fire_year <- i
        rasValue_master0$OBJECTID <- k
        
        
        # Summary operations with conditional checks for each column
        mn_ele_0 <- rasValue_master %>%
          group_by(id) %>%  # Grouping by 'id'
          summarise(
            mean_rdnbr = safe_summary(cur_data(), "rdnbr", mean, na.rm = TRUE),
            mode_veg = safe_summary(cur_data(), "RF_pre_veg", function(x) mode(x)),
            n = n(),
            mode_veg_2post = safe_summary(cur_data(), "RF_post2_veg", function(x) mode(x)),
            mean_elevation = safe_summary(cur_data(), "elevation", mean, na.rm = TRUE),
            mean_tmean = safe_summary(cur_data(), "tmean", mean, na.rm = TRUE),
            mean_tmax = safe_summary(cur_data(), "tmax", mean, na.rm = TRUE),
            mean_tmin = safe_summary(cur_data(), "tmin", mean, na.rm = TRUE),
            mean_tdmean = safe_summary(cur_data(), "tdmean", mean, na.rm = TRUE),
            mean_vpdmin = safe_summary(cur_data(), "vpdmin", mean, na.rm = TRUE),
            mean_vpdmax = safe_summary(cur_data(), "vpdmax", mean, na.rm = TRUE),
            mean_ppt = safe_summary(cur_data(), "ppt", mean, na.rm = TRUE),
            mean_aspect = safe_summary(cur_data(), "aspect", mean, na.rm = TRUE),
            mean_slope = safe_summary(cur_data(), "slope", mean, na.rm = TRUE),
            mean_tmean_1pre = safe_summary(cur_data(), "tmean_1pre", mean, na.rm = TRUE),
            mean_tmax_1pre = safe_summary(cur_data(), "tmax_1pre", mean, na.rm = TRUE),
            mean_tmin_1pre = safe_summary(cur_data(), "tmin_1pre", mean, na.rm = TRUE),
            mean_tdmean_1pre = safe_summary(cur_data(), "tdmean_1pre", mean, na.rm = TRUE),
            mean_vpdmin_1pre = safe_summary(cur_data(), "vpdmin_1pre", mean, na.rm = TRUE),
            mean_vpdmax_1pre = safe_summary(cur_data(), "vpdmax_1pre", mean, na.rm = TRUE),
            mean_ppt_1pre = safe_summary(cur_data(), "ppt_1pre", mean, na.rm = TRUE),
            mean_tmean_1post = safe_summary(cur_data(), "tmean_1post", mean, na.rm = TRUE),
            mean_tmax_1post = safe_summary(cur_data(), "tmax_1post", mean, na.rm = TRUE),
            mean_tmin_1post = safe_summary(cur_data(), "tmin_1post", mean, na.rm = TRUE),
            mean_tdmean_1post = safe_summary(cur_data(), "tdmean_1post", mean, na.rm = TRUE),
            mean_vpdmin_1post = safe_summary(cur_data(), "vpdmin_1post", mean, na.rm = TRUE),
            mean_vpdmax_1post = safe_summary(cur_data(), "vpdmax_1post", mean, na.rm = TRUE),
            mean_ppt_1post = safe_summary(cur_data(), "ppt_1post", mean, na.rm = TRUE),
            mean_tmean_1pre_abs = safe_summary(cur_data(), "tmean_1pre_abs", mean, na.rm = TRUE),
            mean_tmax_1pre_abs = safe_summary(cur_data(), "tmax_1pre_abs", mean, na.rm = TRUE),
            mean_tmin_1pre_abs = safe_summary(cur_data(), "tmin_1pre_abs", mean, na.rm = TRUE),
            mean_tdmean_1pre_abs = safe_summary(cur_data(), "tdmean_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmin_1pre_abs = safe_summary(cur_data(), "vpdmin_1pre_abs", mean, na.rm = TRUE),
            mean_vpdmax_1pre_abs = safe_summary(cur_data(), "vpdmax_1pre_abs", mean, na.rm = TRUE),
            mean_ppt_1pre_abs = safe_summary(cur_data(), "ppt_1pre_abs", mean, na.rm = TRUE),
            mean_tmean_1post_abs = safe_summary(cur_data(), "tmean_1post_abs", mean, na.rm = TRUE),
            mean_tmax_1post_abs = safe_summary(cur_data(), "tmax_1post_abs", mean, na.rm = TRUE),
            mean_tmin_1post_abs = safe_summary(cur_data(), "tmin_1post_abs", mean, na.rm = TRUE),
            mean_tdmean_1post_abs = safe_summary(cur_data(), "tdmean_1post_abs", mean, na.rm = TRUE),
            mean_vpdmin_1post_abs = safe_summary(cur_data(), "vpdmin_1post_abs", mean, na.rm = TRUE),
            mean_vpdmax_1post_abs = safe_summary(cur_data(), "vpdmax_1post_abs", mean, na.rm = TRUE),
            mean_ppt_1post_abs = safe_summary(cur_data(), "ppt_1post_abs", mean, na.rm = TRUE),
            mean_tmean_abs = safe_summary(cur_data(), "tmean_abs", mean, na.rm = TRUE),
            mean_tmax_abs = safe_summary(cur_data(), "tmax_abs", mean, na.rm = TRUE),
            mean_tmin_abs = safe_summary(cur_data(), "tmin_abs", mean, na.rm = TRUE),
            mean_tdmean_abs = safe_summary(cur_data(), "tdmean_abs", mean, na.rm = TRUE),
            mean_vpdmin_abs = safe_summary(cur_data(), "vpdmin_abs", mean, na.rm = TRUE),
            mean_vpdmax_abs = safe_summary(cur_data(), "vpdmax_abs", mean, na.rm = TRUE),
            mean_ppt_abs = safe_summary(cur_data(), "ppt_abs", mean, na.rm = TRUE),
            count_RF_pre_veg_1 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 1)),
            count_RF_pre_veg_4 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 4)),
            count_RF_pre_veg_5 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 5)),
            count_RF_pre_veg_7 = safe_summary(cur_data(), "RF_pre_veg", function(x) sum(x == 7)),
            count_RF_post1_veg_1 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 1)),
            count_RF_post1_veg_4 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 4)),
            count_RF_post1_veg_5 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 5)),
            count_RF_post1_veg_7 = safe_summary(cur_data(), "RF_post1_veg", function(x) sum(x == 7)),
            count_RF_post2_veg_1 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 1)),
            count_RF_post2_veg_4 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 4)),
            count_RF_post2_veg_5 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 5)),
            count_RF_post2_veg_7 = safe_summary(cur_data(), "RF_post2_veg", function(x) sum(x == 7)),
            count_RF_post3_veg_1 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 1)),
            count_RF_post3_veg_4 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 4)),
            count_RF_post3_veg_5 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 5)),
            count_RF_post3_veg_7 = safe_summary(cur_data(), "RF_post3_veg", function(x) sum(x == 7)),
            count_RF_post4_veg_1 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 1)),
            count_RF_post4_veg_4 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 4)),
            count_RF_post4_veg_5 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 5)),
            count_RF_post4_veg_7 = safe_summary(cur_data(), "RF_post4_veg", function(x) sum(x == 7)),
            count_RF_post5_veg_1 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 1)),
            count_RF_post5_veg_4 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 4)),
            count_RF_post5_veg_5 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 5)),
            count_RF_post5_veg_7 = safe_summary(cur_data(), "RF_post5_veg", function(x) sum(x == 7)),
            count_RF_post6_veg_1 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 1)),
            count_RF_post6_veg_4 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 4)),
            count_RF_post6_veg_5 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 5)),
            count_RF_post6_veg_7 = safe_summary(cur_data(), "RF_post6_veg", function(x) sum(x == 7)),
            count_RF_post7_veg_1 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 1)),
            count_RF_post7_veg_4 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 4)),
            count_RF_post7_veg_5 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 5)),
            count_RF_post7_veg_7 = safe_summary(cur_data(), "RF_post7_veg", function(x) sum(x == 7)),
            count_RF_post8_veg_1 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 1)),
            count_RF_post8_veg_4 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 4)),
            count_RF_post8_veg_5 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 5)),
            count_RF_post8_veg_7 = safe_summary(cur_data(), "RF_post8_veg", function(x) sum(x == 7)),
            count_RF_post9_veg_1 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 1)),
            count_RF_post9_veg_4 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 4)),
            count_RF_post9_veg_5 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 5)),
            count_RF_post9_veg_7 = safe_summary(cur_data(), "RF_post9_veg", function(x) sum(x == 7)),
            count_RF_post10_veg_1 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 1)),
            count_RF_post10_veg_4 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 4)),
            count_RF_post10_veg_5 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 5)),
            count_RF_post10_veg_7 = safe_summary(cur_data(), "RF_post10_veg", function(x) sum(x == 7)),
            count_RF_post11_veg_1 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 1)),
            count_RF_post11_veg_4 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 4)),
            count_RF_post11_veg_5 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 5)),
            count_RF_post11_veg_7 = safe_summary(cur_data(), "RF_post11_veg", function(x) sum(x == 7)),
            count_RF_post12_veg_1 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 1)),
            count_RF_post12_veg_4 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 4)),
            count_RF_post12_veg_5 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 5)),
            count_RF_post12_veg_7 = safe_summary(cur_data(), "RF_post12_veg", function(x) sum(x == 7)),
            count_RF_post13_veg_1 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 1)),
            count_RF_post13_veg_4 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 4)),
            count_RF_post13_veg_5 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 5)),
            count_RF_post13_veg_7 = safe_summary(cur_data(), "RF_post13_veg", function(x) sum(x == 7)),
            count_RF_post14_veg_1 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 1)),
            count_RF_post14_veg_4 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 4)),
            count_RF_post14_veg_5 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 5)),
            count_RF_post14_veg_7 = safe_summary(cur_data(), "RF_post14_veg", function(x) sum(x == 7)),
            count_RF_post15_veg_1 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 1)),
            count_RF_post15_veg_4 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 4)),
            count_RF_post15_veg_5 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 5)),
            count_RF_post15_veg_7 = safe_summary(cur_data(), "RF_post15_veg", function(x) sum(x == 7)),
            count_RF_post16_veg_1 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 1)),
            count_RF_post16_veg_4 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 4)),
            count_RF_post16_veg_5 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 5)),
            count_RF_post16_veg_7 = safe_summary(cur_data(), "RF_post16_veg", function(x) sum(x == 7)),
            count_RF_post17_veg_1 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 1)),
            count_RF_post17_veg_4 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 4)),
            count_RF_post17_veg_5 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 5)),
            count_RF_post17_veg_7 = safe_summary(cur_data(), "RF_post17_veg", function(x) sum(x == 7)),
            count_RF_post18_veg_1 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 1)),
            count_RF_post18_veg_4 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 4)),
            count_RF_post18_veg_5 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 5)),
            count_RF_post18_veg_7 = safe_summary(cur_data(), "RF_post18_veg", function(x) sum(x == 7)),
            count_RF_post19_veg_1 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 1)),
            count_RF_post19_veg_4 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 4)),
            count_RF_post19_veg_5 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 5)),
            count_RF_post19_veg_7 = safe_summary(cur_data(), "RF_post19_veg", function(x) sum(x == 7)),
            count_RF_post20_veg_1 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 1)),
            count_RF_post20_veg_4 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 4)),
            count_RF_post20_veg_5 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 5)),
            count_RF_post20_veg_7 = safe_summary(cur_data(), "RF_post20_veg", function(x) sum(x == 7)),
            count_RF_post21_veg_1 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 1)),
            count_RF_post21_veg_4 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 4)),
            count_RF_post21_veg_5 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 5)),
            count_RF_post21_veg_7 = safe_summary(cur_data(), "RF_post21_veg", function(x) sum(x == 7)),
            count_RF_post22_veg_1 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 1)),
            count_RF_post22_veg_4 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 4)),
            count_RF_post22_veg_5 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 5)),
            count_RF_post22_veg_7 = safe_summary(cur_data(), "RF_post22_veg", function(x) sum(x == 7)),
            count_RF_post23_veg_1 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 1)),
            count_RF_post23_veg_4 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 4)),
            count_RF_post23_veg_5 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 5)),
            count_RF_post23_veg_7 = safe_summary(cur_data(), "RF_post23_veg", function(x) sum(x == 7)),
            count_RF_post24_veg_1 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 1)),
            count_RF_post24_veg_4 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 4)),
            count_RF_post24_veg_5 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 5)),
            count_RF_post24_veg_7 = safe_summary(cur_data(), "RF_post24_veg", function(x) sum(x == 7)),
            count_RF_post25_veg_1 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 1)),
            count_RF_post25_veg_4 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 4)),
            count_RF_post25_veg_5 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 5)),
            count_RF_post25_veg_7 = safe_summary(cur_data(), "RF_post25_veg", function(x) sum(x == 7)),
            count_RF_post26_veg_1 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 1)),
            count_RF_post26_veg_4 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 4)),
            count_RF_post26_veg_5 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 5)),
            count_RF_post26_veg_7 = safe_summary(cur_data(), "RF_post26_veg", function(x) sum(x == 7)),
            count_RF_post27_veg_1 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 1)),
            count_RF_post27_veg_4 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 4)),
            count_RF_post27_veg_5 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 5)),
            count_RF_post27_veg_7 = safe_summary(cur_data(), "RF_post27_veg", function(x) sum(x == 7)),
            count_RF_post28_veg_1 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 1)),
            count_RF_post28_veg_4 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 4)),
            count_RF_post28_veg_5 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 5)),
            count_RF_post28_veg_7 = safe_summary(cur_data(), "RF_post28_veg", function(x) sum(x == 7)),
            count_RF_post29_veg_1 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 1)),
            count_RF_post29_veg_4 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 4)),
            count_RF_post29_veg_5 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 5)),
            count_RF_post29_veg_7 = safe_summary(cur_data(), "RF_post29_veg", function(x) sum(x == 7)),
            count_RF_post30_veg_1 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 1)),
            count_RF_post30_veg_4 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 4)),
            count_RF_post30_veg_5 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 5)),
            count_RF_post30_veg_7 = safe_summary(cur_data(), "RF_post30_veg", function(x) sum(x == 7)),
            count_RF_post31_veg_1 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 1)),
            count_RF_post31_veg_4 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 4)),
            count_RF_post31_veg_5 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 5)),
            count_RF_post31_veg_7 = safe_summary(cur_data(), "RF_post31_veg", function(x) sum(x == 7)),
            count_RF_post32_veg_1 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 1)),
            count_RF_post32_veg_4 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 4)),
            count_RF_post32_veg_5 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 5)),
            count_RF_post32_veg_7 = safe_summary(cur_data(), "RF_post32_veg", function(x) sum(x == 7)),
            count_RF_post33_veg_1 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 1)),
            count_RF_post33_veg_4 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 4)),
            count_RF_post33_veg_5 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 5)),
            count_RF_post33_veg_7 = safe_summary(cur_data(), "RF_post33_veg", function(x) sum(x == 7)),
            count_RF_post34_veg_1 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 1)),
            count_RF_post34_veg_4 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 4)),
            count_RF_post34_veg_5 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 5)),
            count_RF_post34_veg_7 = safe_summary(cur_data(), "RF_post34_veg", function(x) sum(x == 7)),
            count_RF_post35_veg_1 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 1)),
            count_RF_post35_veg_4 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 4)),
            count_RF_post35_veg_5 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 5)),
            count_RF_post35_veg_7 = safe_summary(cur_data(), "RF_post35_veg", function(x) sum(x == 7)),
            count_RF_post36_veg_1 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 1)),
            count_RF_post36_veg_4 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 4)),
            count_RF_post36_veg_5 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 5)),
            count_RF_post36_veg_7 = safe_summary(cur_data(), "RF_post36_veg", function(x) sum(x == 7)),
            count_RF_post37_veg_1 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 1)),
            count_RF_post37_veg_4 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 4)),
            count_RF_post37_veg_5 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 5)),
            count_RF_post37_veg_7 = safe_summary(cur_data(), "RF_post37_veg", function(x) sum(x == 7))
          )
        mn_ele_0$rdnbr_class <- 0
        mn_ele_0$fire_year <- i
        mn_ele_0$OBJECTID <- k
        
        core_area_patches_0 <- core_area_patches %>%
          filter(class==0) %>%
          rename(core_area = value) %>%
          select(core_area,id)
        
        perimeter_area_patches_0 <- perimeter_area_patches %>%
          filter(class==0) %>%
          rename(perimeter_area = value)%>%
          select(perimeter_area,id)
        
        perimeter_patches_0 <- perimeter_patches %>%
          filter(class==0) %>%
          rename(perimeter = value)%>%
          select(perimeter,id)
        
        contiguity_patches_0 <- contiguity_patches %>%
          filter(class==0) %>%
          rename(contiguity = value)%>%
          select(contiguity,id)
        
        mn_ele_0 <- left_join(mn_ele_0, core_area_patches_0, by = "id") 
        mn_ele_0 <- left_join(mn_ele_0, perimeter_area_patches_0, by = "id") 
        mn_ele_0 <- left_join(mn_ele_0, perimeter_patches_0, by = "id") 
        mn_ele_0 <- left_join(mn_ele_0, contiguity_patches_0, by = "id") 
      }
      
      
      # Ensure all data frames have the same columns before rbind
      
      mn_ele <- rbind(if(exists("mn_ele_0")) mn_ele_0,
                      if(exists("mn_ele_1")) mn_ele_1,
                      if(exists("mn_ele_2")) mn_ele_2,
                      if(exists("mn_ele_3")) mn_ele_3)
      
      
      # Add multiple columns programmatically using bind_cols and map_dfc
      mn_ele <- mn_ele %>%
        bind_cols(map_dfc(extended_vector, function(x) {
          setNames(data.frame(matrix(NA, nrow = nrow(mn_ele), ncol = 4)), 
                   paste0("count_RF_", x, "_veg_", c(1,2, 4, 5,6, 7,8,9)))
        }))
      # Save raster df
      # master_raster <- rbind(if(exists("rasValue_master0")) rasValue_master0,
      #                        if(exists("rasValue_master1")) rasValue_master1,
      #                        if(exists("rasValue_master2")) rasValue_master2,
      #                        if(exists("rasValue_master3")) rasValue_master3)
      
      #write.csv(master_raster,paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Dataframes/",k,'test.csv'), row.names = FALSE)}
    
      fwrite(mn_ele,paste0("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Dataframes/Megafire_paper/patch_dfs/",k,'.csv'), row.names = FALSE)
      }


