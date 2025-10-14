################################################################################
## This script was used to produce figures showing the amount of actual and pre-
## dicted conifer recovery and recovery failure 20 years post-fire for high se-
## verity burned pixels in megafires in the Sierra Nevada

## Code by Johanna Schönecker 
# 14th February 2025

# The following datasets area required to successfully run the script:
# 🔘 Dataframe of predicted conifer recovery for high severity megafire pixels


################################################################################

### Load required packages
library(terra)
library(ggplot2)
library(data.table)
library(ggspatial)
library(stringr)
library(ggthemes)


## Load all files of observed transitions/ recovery (megafires 1985-2002)

# Define input directory
directory <- "C:/Users/jscho/Documents/Megafires-1985-2023-Data/raster_df_mega"

# Define the fire_IDs to filter filenames
fire_IDs <- c(10495,10498,10504,10273,10094,9714,9724,9487,8985,
              9039,8747,8026,7996,7961,7808,7747,7514,7412,7242)

# Convert fire_IDs to character for filename matching
fire_IDs <- as.character(fire_IDs)

# List all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Filter files that contain any of the fire_IDs
filtered_files <- csv_files[str_detect(basename(csv_files), paste(fire_IDs, collapse = "|"))]

# Create an empty list to store dataframes
all_data <- list()

# Read and combine all matching CSV files
for (file in filtered_files) {
  temp_data <- fread(file) %>% 
    mutate(source_file = basename(file))  # Add filename as a column for reference
  all_data[[file]] <- temp_data
}

# Combine all data into one dataframe
data_pre2002 <- bind_rows(all_data)

data_pre2002 <- data_pre2002 %>%
  filter(RF_pre_veg == 7)

data_pre2002 <- data_pre2002 %>%
  filter(transitioned==1)

data_pre2002 <- data_pre2002 %>%
  filter(rdnbr_class == 3)


## Input file of predicted transitions/ recovery (megafires 2003-2023)

data_with_predictions <- fread("C:/Users/jscho/Documents/Megafires-1985-2023/all_predictions.csv")

## Create plot of actual and predicted recovery of severely burned coniferous 
## pixels in megafires in the Sierra Nevada 1985-2023

# Group by fire_year and predicted_class, then summarize the counts
class_counts_predicted <- data_with_predictions %>%
  group_by(fire_year, predicted_class) %>%
  summarise(count = n(), .groups = "drop")

class_counts_predicted$type <- 'predicted'


class_counts_measured <- data_pre2002 %>%
  group_by(fire_year,returned)%>%
  summarise(count = n(), .groups = 'drop')

class_counts_measured$type <- 'measured'

class_counts_predicted <- setNames(class_counts_predicted, c("fire_year", "returned","count","type"))
class_counts_measured <- setNames(class_counts_measured, c("fire_year", "returned","count","type"))
class_counts_predicted$returned <- as.numeric(class_counts_predicted$returned)

class_counts <- rbind(class_counts_measured,class_counts_predicted)

class_counts$area <- class_counts$count * 0.09



ggplot(class_counts, aes(x = fire_year, y = area, fill = factor(returned), alpha = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey", "darkgreen")) +  # Manually set the colors
  scale_alpha_manual(values = c(1, 0.5)) +       # Full opacity for one, semi-transparent for another
  labs(title = "",
       x = "Fire year",
       y = "Area (ha)",
       fill = "Predicted Class",
       alpha = "Type") +
  theme_pubr()+labs_pubr()


class_counts_prop <- class_counts %>%
  group_by(fire_year)%>%
  summarise(total_area = sum(area))

merged_data <- class_counts %>%
  inner_join(class_counts_prop, by = "fire_year")

merged_data_0 <- merged_data %>%
  filter(returned==0)

merged_data_0$prop <- merged_data_0$area/merged_data_0$total_area

con_trans_plot <- ggplot() +
  geom_sf(data = SEKI_shp, aes(color = "SEKI Boundary"), fill = NA, size = 0.5) +  # Add to legend
  geom_sf(data = KNP_shp, aes(color = "KNP Boundary"), fill = "grey",alpha=0.3, size = 0.5) +  # Add to legend
  geom_tile(data = new_data_with_predictions, aes(x = x, y = y, fill = factor(predicted_classes))) +  # Plot raster
  scale_fill_manual(
    values = c("0" = "darkviolet", "1" = "darkgreen"),  # Custom colors for values
    name = "Raster Values"
  ) +
  scale_color_manual(
    values = c("SEKI Boundary" = "black", "KNP Boundary" = "transparent"),  # Custom colors for shapefile boundaries
    name = "Boundaries"  # Legend title for shapefiles
  ) +
  labs(
    title = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(knp_bbox["xmin"], knp_bbox["xmax"]),
    ylim = c(knp_bbox["ymin"], knp_bbox["ymax"])
  ) +  # Set the extent of the plot
  annotation_scale(
    location = "bl",  # Bottom-left corner
    width_hint = 0.2  # Width of the scale bar as a fraction of the plot width
  ) +
  annotation_north_arrow(
    location = "br",  # Bottom-right corner
    which_north = "true",  # Use "true" north
    style = north_arrow_fancy_orienteering()
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 12)
  )

## Create maps with predicted recovery classes
# Create a SpatRaster object from x, y, and predicted values
raster_predictions <- rast(
  data.frame(
    x = new_data_with_predictions$x,
    y = new_data_with_predictions$y,
    value = as.numeric(new_data_with_predictions$predicted_class)
  ),
  type = "xyz",
  crs = "EPSG:3310"  # Replace with your CRS
)


# Plot the raster
plot(raster_predictions, col = c("darkviolet", "darkgreen"), main = "Predicted Classes")

# Convert raster to dataframe for ggplot
r_df <- as.data.frame(r, xy = TRUE)
colnames(r_df) <- c("x", "y", "value")  # Ensure column names are clear

# Load KNP fire shapefile
KNP_shp <- st_read('C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/GIS/KNP Analysis/KNP_2021_epsg3310.shp')

# Load the shapefile (SEKI_shp) and convert to an sf object
SEKI_shp <- vect("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/Basefiles/SEKI_shp_epsg3310.shp")

SEKI_shp <- st_as_sf(SEKI_shp)

# Get the bounding box of KNP_shp
knp_bbox <- st_bbox(KNP_shp)







new_data_with_predictions <- new_data_with_predictions %>%
  mutate(dist_class = ifelse(dist_unburned > 150, "far", "near"))

dist_plot <- ggplot() +
  geom_sf(data = SEKI_shp, aes(color = "SEKI Boundary"), fill = NA, size = 0.5) +  # Add to legend
  geom_sf(data = KNP_shp, aes(color = "KNP Boundary"), fill = "grey",alpha=0.3, size = 0.5) +  # Add to legend
  geom_tile(data = new_data_with_predictions, aes(x = x, y = y, fill = factor(dist_class))) +  # Plot raster
  scale_fill_manual(
    values = c("near" = "grey", "far" = "orange"),  # Custom colors for values
    name = "Raster Values"
  ) +
  scale_color_manual(
    values = c("SEKI Boundary" = "black", "KNP Boundary" = "transparent"),  # Custom colors for shapefile boundaries
    name = "Boundaries"  # Legend title for shapefiles
  ) +
  labs(
    title = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(knp_bbox["xmin"], knp_bbox["xmax"]),
    ylim = c(knp_bbox["ymin"], knp_bbox["ymax"])
  ) +  # Set the extent of the plot
  annotation_scale(
    location = "bl",  # Bottom-left corner
    width_hint = 0.2  # Width of the scale bar as a fraction of the plot width
  ) +
  annotation_north_arrow(
    location = "br",  # Bottom-right corner
    which_north = "true",  # Use "true" north
    style = north_arrow_fancy_orienteering()
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(size = 12)
  )


# Combine the plots side by side
combined_plot <- plot_grid(
  dist_plot, con_trans_plot,  # The two plots to combine
  ncol = 2,      # Number of columns (side by side means 2 columns)
  rel_widths = c(1, 1),  # Relative widths of the plots (equal widths here)
  labels = c("a", "b")
)

# For all megafires after 2002

# Load necessary libraries
library(dplyr)
library(data.table)
library(stringr)
library(terra)
library(tidyr)
library(ranger)
library(ggplot2)

rf_model_class <- readRDS(file = "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Outputs/Models/rf_model_class_returned.rds")

# Directory containing the CSV files
directory <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/raster_df_mega"

# Vector of fire IDs
fire_IDs <- c(28976,30032, 32384, 33025, 33113, 33252, 35032, 35053,
              35312, 35322,35874, 36180, 36404, 37023, 37098, 37928,
              37959, 37989, 40325)

megafire_IDs <- c(21678, 21679, 21683, 21684, 21688, 21786, 21791, 22057, 22060, 28976, 30032, 32384, 33025, 33113,
                  33252, 35032, 35053, 35312, 35322, 35874, 36180, 36404, 37023, 37098, 37928, 37959, 37989, 38018,
                  38120, 38341, 38923, 39327, 39335, 39356, 39424, 39430, 39674, 39683, 39756, 39762, 39763, 40325,
                  40570, 40705, 40882, 41219, 41262, 41627, 41747, 41837, 41925, 42055, 42350, 42380, 42387, 42745)

# Extract elements only in megafire_IDs
only_in_megafire_IDs <- setdiff(megafire_IDs, fire_IDs)

# Get a list of all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Filter files that contain any of the megafire IDs in their filename
filtered_files <- csv_files[sapply(csv_files, function(file) {
  fire_id <- as.numeric(str_extract(basename(file), "\\d+")) # Extract numeric ID from filename
  fire_id %in% only_in_megafire_IDs
})]

# Read and bind the filtered files into one dataframe
new_data <- bind_rows(lapply(filtered_files, fread))

# Filter data based on conditions
new_data <- new_data %>%
  filter(RF_pre_veg == 7) %>%
  filter(transitioned == 1) %>%
  filter(rdnbr_class == 3)

# Calculate additional columns
new_data$years_since_fire <- new_data$fire_year - new_data$previous_fire_year

# Replace NA values in years_since_fire with 100
new_data <- new_data %>%
  mutate(years_since_fire = ifelse(is.na(years_since_fire), 100, years_since_fire))

# Drop rows with NA values across relevant predictor columns
new_data <- new_data %>%
  drop_na(dist_unburned, elevation, slope, aspect, tri, tpi,
          ppt_1post, tdmean_1post, tmean_1post, vpdmin_1post, tmax_1post,
          vpdmax_1post, tmin_1post, ppt_1post_abs, tdmean_1post_abs,
          tmean_1post_abs, vpdmin_1post, tmax_1post_abs, vpdmax_1post_abs,
          tmin_1post_abs, years_since_fire)

# Separate additional columns for retention
mapping_columns <- new_data %>%
  dplyr::select(x, y, OBJECTID, fire_year)

# Prepare predictors for the model
model_predictors <- new_data %>%
  dplyr::select(dist_unburned, elevation, slope, aspect, tri, tpi,
         ppt_1post, tdmean_1post, tmean_1post, vpdmin_1post, tmax_1post,
         vpdmax_1post, tmin_1post, ppt_1post_abs, tdmean_1post_abs,
         tmean_1post_abs, vpdmin_1post, tmax_1post_abs, vpdmax_1post_abs,
         tmin_1post_abs, years_since_fire)

# Predict using the trained Random Forest model
predictions <- predict(rf_model_class, data = model_predictors)

# Extract predicted probabilities for class "1" (returned)
predicted_probabilities <- predictions$predictions[, "1"]

# Extract predicted classes (based on the highest probability)
predicted_classes <- ifelse(predicted_probabilities > 0.5, "1", "0")

# Combine mapping columns, predictors, and predictions
new_data_with_predictions <- mapping_columns %>%
  bind_cols(model_predictors) %>%
  mutate(
    predicted_class = predicted_classes,
    predicted_probability = predicted_probabilities
  )

# View the first few rows of the updated dataset
head(new_data_with_predictions)

new_data_with_predictions <- new_data_with_predictions %>%
  filter(fire_year > 2002)

# Save the updated dataset with predictions
output_path <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/2003_2020_mega_with_predictions.csv"
fwrite(new_data_with_predictions, output_path, row.names = FALSE)

predictions_21679 <- new_data_with_predictions %>%
  filter(OBJECTID==42745)


library(terra)

# Create a SpatRaster object from x, y, and predicted values
raster_predictions <- rast(
  data.frame(
    x = predictions_21679$x,
    y = predictions_21679$y,
    value = as.numeric(predictions_21679$predicted_class)
  ),
  type = "xyz",
  crs = "EPSG:3310"  # Replace with your CRS
)


# Plot the raster
plot(raster_predictions, col = c("red", "blue"), main = "Predicted Classes")




# Group by fire_year and predicted_class, then summarize the counts
class_counts <- new_data_with_predictions %>%
  group_by(fire_year, predicted_class) %>%
  summarise(count = n(), .groups = "drop")

class_counts$area <- class_counts$count * 0.09


ggplot(class_counts, aes(x = fire_year, y = area, fill = predicted_class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Predicted Class Counts by Fire Year",
       x = "Fire Year",
       y = "Count",
       fill = "Predicted Class") +
  theme_classic()






####### Read in all megafire files as well
# Directory containing the CSV files
directory <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/raster_df_mega"

# Vector of fire IDs
fire_IDs <- c(28976,30032, 32384, 33025, 33113, 33252, 35032, 35053,
              35312, 35322,35874, 36180, 36404, 37023, 37098, 37928,
              37959, 37989, 40325)


# Get a list of all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Filter files that contain any of the megafire IDs in their filename
filtered_files <- csv_files[sapply(csv_files, function(file) {
  fire_id <- as.numeric(str_extract(basename(file), "\\d+")) # Extract numeric ID from filename
  fire_id %in% fire_IDs
})]

# Read and bind the filtered files into one dataframe
data_pre2002 <- bind_rows(lapply(filtered_files, fread))

# Filter data based on conditions
data_pre2002 <- data_pre2002 %>%
  filter(RF_pre_veg == 7) %>%
  filter(transitioned == 1) %>%
  filter(rdnbr_class == 3)

# Calculate additional columns
data_pre2002$years_since_fire <- data_pre2002$fire_year - data_pre2002$previous_fire_year
data_pre2002$returned[!is.na(data_pre2002$yrs_to_return) & data_pre2002$yrs_to_return > 20] <- 0

# Replace NA values in years_since_fire with 100
data_pre2002 <- data_pre2002 %>%
  mutate(years_since_fire = ifelse(is.na(years_since_fire), 100, years_since_fire))








ggplot(merged_data_0, aes(x = fire_year, y = prop)) +
  geom_point(size = 3, alpha = 0.8, color = "black") +  # Scatter plot
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Linear trendline
  labs(
    title = "Prop. severely burned conifer not recov/ pred. to recov w/in 20 yrs",
    x = "Fire year",
    y = "Proportion not recovered"
  ) +
  theme_classic() +  # Clean theme
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(hjust = 1)  # Rotate x-axis labels if needed
  )


# Fit the linear model
linear_model <- lm(prop ~ fire_year, data = merged_data_0)

# Summary of the model
summary(linear_model)

######## Compare predicted/ recovered values and burn severity shapes
# Burn severity shapes
bimodal <- c(21683,21684,32384,33025,35874,37023,37098,38341,39756, 42350, 40570,21688)

normal <- c(21678,21679,21786,30032,37928,37959,37989,39674,39683,39762,40705,40882,41925,42387,42745,39327)

right_skewed <- c(22057,28976,33252,38120,39424,39763,41262,21791,22060,21688)

left_skewed <- c(33113,35032,35053,35312,35322,36180,36404,38018,38923,39335,39356, 39430,40325,41219,41627,41747,41837, 42055,42380)

# Predictions 2003 - 2020
predictions_2003_2020 <- fread("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/2003_2020_mega_with_predictions.csv")


# Observed reversions 1985-2002

directory <- "C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Data/raster_df_mega"

# Vector of fire IDs
fire_IDs <- c(28976,30032, 32384, 33025, 33113, 33252, 35032, 35053,
              35312, 35322,35874, 36180, 36404, 37023, 37098, 37928,
              37959, 37989, 40325)


# Get a list of all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Filter files that contain any of the megafire IDs in their filename
filtered_files <- csv_files[sapply(csv_files, function(file) {
  fire_id <- as.numeric(str_extract(basename(file), "\\d+")) # Extract numeric ID from filename
  fire_id %in% fire_IDs
})]

# Read and bind the filtered files into one dataframe
data_pre2002 <- bind_rows(lapply(filtered_files, fread))

# Filter data based on conditions
data_pre2002 <- data_pre2002 %>%
  filter(RF_pre_veg == 7) %>%
  filter(transitioned == 1) %>%
  filter(rdnbr_class == 3)

# Calculate additional columns
data_pre2002$years_since_fire <- data_pre2002$fire_year - data_pre2002$previous_fire_year
data_pre2002$returned[!is.na(data_pre2002$yrs_to_return) & data_pre2002$yrs_to_return > 20] <- 0

# Replace NA values in years_since_fire with 100
data_pre2002 <- data_pre2002 %>%
  mutate(years_since_fire = ifelse(is.na(years_since_fire), 100, years_since_fire))




predicted_grouped <- predictions_2003_2020 %>%
  group_by(OBJECTID,predicted_class) %>%
  summarise(count = n())

predicted_grouped <- setNames(predicted_grouped, c("OBJECTID", "returned","count"))

observed_grouped <- data_pre2002 %>%
  group_by(OBJECTID, returned) %>%
  summarise(count = n())

observed_grouped <- setNames(observed_grouped, c("OBJECTID", "returned","count"))


grouped <- rbind(predicted_grouped,observed_grouped)

grouped <- grouped %>%
  group_by(OBJECTID) %>%
  mutate(total = sum(count)) %>%
  ungroup()

grouped$proportion <- grouped$count/grouped$total




# Add shape to dataframe

# Add the `shape` column based on the OBJECTID membership
grouped$shape <- ifelse(grouped$OBJECTID %in% bimodal, "bimodal",
                        ifelse(grouped$OBJECTID %in% normal, "normal",
                               ifelse(grouped$OBJECTID %in% right_skewed, "right_skewed",
                                      ifelse(grouped$OBJECTID %in% left_skewed, "left_skewed", NA))))

grouped_transitioned <- grouped %>%
  filter(returned == 0)

grouped_returned <- grouped %>%
  filter(returned == 1)


# Custom palette and shapes
palette <- c("#648FFF", "#FE6100", "#FFB000", "#DC267F")
shapes <- c(3, 8, 2, 0)  # Shapes for each category


grouped_transitioned <- grouped_transitioned %>%
  filter(total > 100)

# Scatterplot with log scale on x-axis
scatter_shapes <- ggplot(grouped_transitioned, aes(x = count, y = proportion, color = shape, shape = shape)) +
  geom_point(size = 3, alpha = 1) +  # Scatter points with transparency
  scale_x_log10() +  # Log scale for x-axis
  scale_color_manual(values = palette) +  # Custom color palette
  scale_shape_manual(values = shapes) +  # Custom shapes
  labs(
    title = "",
    x = "Count (log scale)",
    y = "Proportion",
    color = "Shape",
    shape = "Shape"
  ) +
  theme_classic() +  # Minimal theme
  theme(
    legend.position = "right",  # Legend on the right
    text = element_text(size = 12)  # Adjust text size
  )



# Custom palette
palette <- c("#648FFF", "#FE6100", "#FFB000", "#DC267F")

# Boxplot with shape on x-axis and proportion on y-axis
box_shapes <- ggplot(grouped_transitioned, aes(x = shape, y = proportion, fill = shape)) +
  geom_boxplot(outlier.color = "black", outlier.shape = 16, alpha = 1) +  # Boxplot with custom outlier settings
  scale_fill_manual(values = palette) +  # Custom color palette
  labs(
    title = "",
    x = "Shape",
    y = "Proportion",
    fill = "Shape"
  ) +
  theme_classic() +  # Clean theme
  theme(
    legend.position = "none",  # Remove legend (optional for fill if x matches shape)
    text = element_text(size = 12)  # Adjust text size
  )

##### No significant difference between means of groups! Have done ANOVA and Kruskall Wallis


library(cowplot)

# Combine the plots side by side
combined_plot <- plot_grid(
  box_shapes, scatter_shapes,  # The two plots to combine
  ncol = 2,      # Number of columns (side by side means 2 columns)
  rel_widths = c(1, 1.6),  # Relative widths of the plots (equal widths here)
  labels = c("a", "b")
  )





# Merge additional information (year and fire name) into `grouped`
grouped <- grouped %>%
  left_join(yrs_ids %>% dplyr::select(OBJECTID, year), by = "OBJECTID") %>%  # Add year
  left_join(SN_fires_attr_mega %>% dplyr::select(OBJECTID, FIRE_NAME), by = "OBJECTID") %>%  # Add fire name
  mutate(
    label = paste0(FIRE_NAME, " (", year, ")")  # Create custom x-axis label
  ) %>%
  arrange(year)  # Sort by year

# Custom colors for the two groups in `returned`
custom_colors <- c("0" = "darkviolet", "1" = "darkgreen")  # Replace Group1/Group2 with actual values

grouped$area <- grouped$count * 0.09

# Barplot
ggplot(grouped, aes(x = factor(label, levels = unique(label)), y = area, fill = factor(returned), alpha = year > 2002)) +
  geom_bar(stat = "identity", position = "dodge") +  # Barplot with dodged bars
  scale_fill_manual(values = custom_colors) +  # Custom colors for the `returned` groups
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.7)) +  # Conditional alpha
  labs(
    title = "",
    x = "Fire Name (Year)",
    y = "Area (ha)",
    fill = "Returned",
    alpha = "Year > 2002"
  ) +
  theme_classic() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    text = element_text(size = 12),  # Adjust text size
    legend.position = "right"  # Position legend on the right
  )
