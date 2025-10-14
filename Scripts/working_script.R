
data_pre2002_returned <- data_pre2002 %>%
  filter(returned==1)

SN_megafires <- fread("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Dataframes/Megafire_paper/SN_megafires_1985_2023.csv")
SN_all_fires <- fread("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Dataframes/Megafire_paper/SN_all_fires_1985_2023.csv")

SN_all_fires$area_ha <- SN_all_fires$Shape_Area / 10000

sum(SN_all_fires$area_ha)
sum(SN_megafires$area_ha)


SN_all_fires_prescribed <- SN_all_fires %>%
  filter(layer == 'fire23_1 — rxburn23_1')

SN_all_fires_wildfires <- SN_all_fires %>%
  filter(layer == 'fire23_1 — firep23_1')

sum(SN_all_fires_wildfires$area_ha)

sum(SN_all_fires_prescribed$area_ha)
'#932191' '#C7594B' '#E17327' '#FF9300'



# Load required package
library(terra)

# Define directory containing the TIFF files
tif_dir <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/718"

# List all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

# Create a raster stack (SpatRaster in terra)
veg_raster_stack <- rast(tif_files)

# Define custom colors for land cover types
colors_all <- c('1' = '#956733', '2' = '#a7b5a5', '3' = '#0940ca', 
                '4' = '#d8bf58', '5' = '#6dcd2b', '6' = '#ffacc9', 
                '7' = '#0e4f12', '8' = 'red')

# Convert color vector to just the colors
color_palette <- unname(colors_all)

# Plot all rasters in the stack with the defined color palette
plot(veg_raster_stack, col = color_palette, main = "Vegetation Classification")







# Load required package
library(terra)

# Define directory containing the TIFF files
tif_dir <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_annual_individual/10504/"


# List all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

# Create a raster stack (SpatRaster)
veg_raster_stack <- rast(tif_files)

# Select the last 10 layers (if there are at least 10)
num_layers <- nlyr(veg_raster_stack)

shp_file <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/mega_indiv_shp/1987/Mega_10504.shp"

# Load shapefile
shapefile <- vect(shp_file)  # Read shapefile

# Crop raster stack to shapefile extent
cropped_stack <- crop(veg_raster_stack, shapefile)

# Mask to keep only data inside the shapefile boundary (optional)
masked_stack <- mask(cropped_stack, shapefile)

veg_raster_stack <- masked_stack


last_10_layers <- if (num_layers >= 15) veg_raster_stack[[ (num_layers-14):num_layers ]] else veg_raster_stack




# Define color mapping for specific land cover classes
colors_all <- c('0' = 'black','1' = '#956733', '2' = '#a7b5a5', '3' = '#0940ca', 
                '4' = '#d8bf58', '5' = '#6dcd2b', '6' = '#ffacc9', 
                '7' = '#0e4f12', '8' = '#7b8d6a', '9' = '#e97451')

# Convert color mapping into a lookup table
color_table <- data.frame(
  class = as.numeric(names(colors_all)),  # Convert class names to numbers
  color = colors_all
)

# Function to plot each raster with correct colors
plot_raster_with_colors <- function(r) {
  unique_values <- unique(values(r))  # Extract unique classes in the raster
  valid_colors <- color_table[color_table$class %in% unique_values, ]  # Match colors
  
  plot(r, col = valid_colors$color, type = "classes", 
       levels = valid_colors$class, main = names(r))
}

# Adjust grid layout for 10 images (e.g., 2 rows x 5 columns)
par(mfrow = c(3, 5), mar = c(3, 3, 2, 1))  # Adjust margins

# Plot the last 10 raster layers
lapply(1:nlyr(last_10_layers), function(i) {
  plot_raster_with_colors(last_10_layers[[i]])
})

# Reset par to default
par(mfrow = c(1,1))

raster_ex <- rast("C:/Users/jscho/Downloads/Veg_classification_fires/2020.tif")





test_ras <- raster("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Megafire_paper/veg_classification_annual/2003.tif")


# Stacked bar veg charts for every fire
library(raster)

  fire_year <- 2021
  num <- 'KNP'
  pre_fire_year <- fire_year - 1
  
  veg_folder <- paste0("C:/Users/jscho/Downloads/Veg_classification_fires/")
  
  # List all TIF files in the folder
  veg_files <- list.files(veg_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Stack the TIF files
  veg_stack <- stack(veg_files)
  
  
  # Convert vegetation stack to dataframe with every column being vegetation in one year
  df_veg_stack <- as.data.frame(veg_stack)
  df_veg_stack <- na.omit(df_veg_stack)
  colnames(df_veg_stack) <- c(paste0("veg_", 2018:2024))
  n<-ncol(df_veg_stack)
  
  # Convert to long format
  df_veg_stack_long<-reshape(df_veg_stack, varying = 1:n, sep = "_", direction = 'long')
  
  # Proportional stacked bar chart of vegetation classes of one specific fire
  sums_stats_veg_1 <-df_veg_stack_long %>%
    group_by(time,veg) %>%
    summarise(n=n())
  
  sums_stats_veg_1$veg <- factor(sums_stats_veg_1$veg, 
                                 levels = c('0', '1', '2', '3', '4', '5', '6', '7','8','9'))
  
  plot_2_1 <- ggplot(sums_stats_veg_1[order(sums_stats_veg_1$veg,decreasing=F),], aes(x=time, y=n, fill=veg)) + 
    geom_bar(position='fill',stat='identity')+
    scale_fill_manual(values = c('0'='black','7'= '#0e4f12','1'='#956733','2'='#a7b5a5','3'='#0940ca','4'='#d8bf58','5'='#6dcd2b','6'='#ffacc9','8' = '#7b8d6a', '9' = '#e97451'),name = "vegetation class", labels = c('no data',"shrub", "bare","water","open","grass/herbaceous","bare soil","coniferous","sagebrush","woodland"))+
    ylab("proportion of high sev burned area")+
    xlab("year")+
    #labs(caption = paste0(fire_name," [", fire_year,"] vegetation composition over time"))+
    
    theme(plot.caption = element_text(hjust=0))+
    annotate(
      x = fire_year, y = 0, label = "I\n*fire", geom = "text",
      color = "red",
      lineheight = .55,
      vjust = .8)+
    theme(legend.position = "none")
  #ggtitle("")

  
  
patch_metrics_mega_large_vlarge <- patch_metrics_mega %>%
  filter(n_bins %in% c("large", "very large"))

#Total area covered by large and very large patches: 

area_large <- sum(patch_metrics_mega_large_vlarge$n)
area_large_ha <- area_large*0.09

# Area large at high severity:
patch_metrics_mega_large_hs <- patch_metrics_mega_large_vlarge %>%
  filter(rdnbr_class == 3)

# Area burned in small patches
patch_metrics_mega_sm_med <- patch_metrics_mega %>%
  filter(n_bins %in% c("small", "medium"))  

area_small <- sum(patch_metrics_mega_sm_med$n)
area_small_ha <- area_small*0.09

area_total <- area_small_ha +area_large_ha

prop_area_large <- area_large_ha/area_total

# Area small at high sev

patch_metrics_mega_small_hs <- patch_metrics_mega_sm_med %>%
  filter(rdnbr_class == 3)


area_large_hs <- sum(patch_metrics_mega_large_hs$n)
area_large_hs_ha <- area_large_hs*0.09

area_small_hs <- sum(patch_metrics_mega_small_hs$n)
area_small_hs_ha <- area_small_hs*0.09


area_total_hs <- area_small_hs_ha+area_large_hs_ha

prop_area_large_hs <- area_large_hs_ha/ area_total_hs

















# Directory containing the CSV files
directory <- "C:/Users/jscho/Documents/Megafires-1985-2023-Data/raster_df_mega"

# Vector of fire IDs
fire_IDs <- c(10495,10498,10504,10273,10094,9714,9724,9487,8985,
              9039,8747,8026,7996,7961,7808,7747,7514,7412,7242)

# Get a list of all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Filter files that contain any of the fire_IDs in their filename
filtered_files <- csv_files[sapply(csv_files, function(file) {
  fire_id <- as.numeric(str_extract(basename(file), "\\d+")) # Extract numeric ID from filename
  fire_id %in% fire_IDs
})]

# Read and bind the filtered files into one dataframe
data <- bind_rows(lapply(filtered_files, fread))

data <- data %>%
  filter(RF_pre_veg == 7)

data <- data %>%
  filter(transitioned==1)


library(dplyr)

# Step 1: Count number of rows per id
patch_sizes <- data %>%
  count(id, name = "n")

# Step 2: Join this count back to the original data
data <- data %>%
  left_join(patch_sizes, by = "id")



data <- data %>%
  mutate(n_bins = cut(n,
                      breaks = c(1, 100, 1000,10000,1500000),
                      labels = c("small","medium","large","very_large"),
                      include.lowest = TRUE))



library(ggplot2)

ggplot(data, aes(x = dist_unburned)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "white") +
  labs(title = "Histogram of Distance to Unburned",
       x = "Distance to Unburned (m)",
       y = "Count") +
  theme_classic()



data_con_trans <- data %>%
  filter(RF_pre_veg == 7)

data_con_trans <- data_con_trans %>%
  filter(transitioned==1)

ggplot(data_con_trans, aes(x = dist_unburned, fill = n_bins)) +
  geom_histogram(binwidth = 50, color = "white", alpha = 0.8) +
  labs(
    title = "Histogram of Distance to Unburned",
    x = "Distance to Unburned (m)",
    y = "Count",
    fill = "Patch Size Class"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )


ggplot(data, aes(x = dist_unburned, fill = n_bins)) +
  geom_histogram(binwidth = 50, color = "white", alpha = 0.8) +
  labs(
    title = "Histogram of Distance to Unburned",
    x = "Distance to Unburned (m)",
    y = "Count",
    fill = "Patch Size Class"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )




# All fires

library(data.table)
library(stringr)
library(dplyr)

# Define directory and fire IDs
directory <- "C:/Users/jscho/Documents/Megafires-1985-2023-Data/raster_df_mega"
fire_IDs <- c(5529, 5718, 5721, 8026, 7808, 7747, 7996, 7961, 
              7412, 7514, 7242, 9714, 9724, 9487, 8985, 9039, 
              8747, 10495, 10498, 10504, 10094, 10273, 1819, 1772, 
              2111, 2140, 1210, 1211, 1216, 1215, 1220, 1319, 
              1315, 718, 911, 932, 869, 888, 945, 959, 504, 
              3363, 3567, 3573, 3574, 3051, 3094, 3188, 2706, 
              2147, 2531, 2415, 2618, 5207, 5309, 4277, 4285, 
              4306, 4380, 4386, 4374, 4500, 3983, 3992)

# List all CSV files
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Filter files matching fire_IDs
filtered_files <- csv_files[sapply(csv_files, function(file) {
  fire_id <- as.numeric(str_extract(basename(file), "\\d+"))
  fire_id %in% fire_IDs
})]

# Define columns to keep
cols_to_read <- c("rdnbr_class", "RF_pre_veg", "id", "OBJECTID", "transitioned", "returned", "dist_unburned")

data <- bind_rows(lapply(filtered_files, function(file) {
  read.csv(file) %>%
    dplyr::select(rdnbr_class, RF_pre_veg, id, OBJECTID, transitioned, returned, dist_unburned)
}))




library(dplyr)

# Step 1: Count number of rows per id
patch_sizes <- data %>%
  count(id, name = "n")

# Step 2: Join this count back to the original data
data <- data %>%
  left_join(patch_sizes, by = "id")



data <- data %>%
  mutate(n_bins = cut(n,
                      breaks = c(1, 100, 1000,10000,1500000),
                      labels = c("small","medium","large","very_large"),
                      include.lowest = TRUE))



library(ggplot2)

ggplot(data, aes(x = dist_unburned)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "white") +
  labs(title = "Histogram of Distance to Unburned",
       x = "Distance to Unburned (m)",
       y = "Count") +
  theme_classic()



data_con_trans <- data %>%
  filter(RF_pre_veg == 7)

data_con_trans <- data_con_trans %>%
  filter(transitioned==1)

ggplot(data_con_trans, aes(x = dist_unburned, fill = n_bins)) +
  geom_histogram(binwidth = 50, color = "white", alpha = 0.8) +
  labs(
    title = "Histogram of Distance to Unburned",
    x = "Distance to Unburned (m)",
    y = "Count",
    fill = "Patch Size Class"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )


ggplot(data, aes(x = dist_unburned, fill = n_bins)) +
  geom_histogram(binwidth = 50, color = "white", alpha = 0.8) +
  labs(
    title = "Histogram of Distance to Unburned",
    x = "Distance to Unburned (m)",
    y = "Count",
    fill = "Patch Size Class"
  ) +
  theme_classic() +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold")
  )



