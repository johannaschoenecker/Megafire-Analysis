################################################################################
## This script tunes and trains a weighted random forest classification model
## distinguishing severely burned pixels which recovery their pre-fire coniferous
## vegetation type from those that do not return to a coniferous vegetation type
## within 20 years post-fire

## Code by Johanna Schönecker 
# 27th March 2025

# The following datasets are required to successfully run the script:
# 🔘 Dataframes in .csv format of pixel metrics for every megafire, with the following variables:
# returned,dist_unburned,elevation,slope,aspect,tri,tpi, ppt_1post, tdmean_1post, tmean_1post, vpdmin_1post,tmax_1post,
# vpdmax_1post,tmin_1post,ppt_1post_abs, tdmean_1post_abs, tmean_1post_abs, vpdmin_1post,tmax_1post_abs,
# vpdmax_1post_abs,tmin_1post_abs,years_since_fire, tree_planting


################################################################################

### Load required packages

library(ranger)
library(dplyr)
library(data.table)
library(caret)
library(tidyr)
library(pdp)
library(ggplot2)
library(stringr)
library(pdp)
library(vip)
library(ggthemes)
library(ggpubr)
library(cowplot)


# Directory containing the CSV files
directory <- "C:/Users/jscho/Documents/Megafires-1985-2023-Data/raster_df_mega_repeated_planting/"

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

data <- data %>%
  filter(rdnbr_class == 3)

data$years_since_fire <- data$fire_year - data$previous_fire_year
data$returned[!is.na(data$yrs_to_return) & data$yrs_to_return > 20] <- 0

# Replace NA values in years_since_fire with 100
data <- data %>%
  mutate(years_since_fire = ifelse(is.na(years_since_fire), 100, years_since_fire))


# Select only pixels that didn't reburn post- initial fire
data_nrep <- data %>%
  filter(reburn_20yrs == 0)

data_nrep_nplant <- data_nrep %>%
  filter(tree_planting == 0)

data <- data_nrep_nplant

# data_nrep_FS <- data_nrep %>%  
#   filter(NFS_bin == 1)
# 
# data_nrep_nFS <- data_nrep %>%  
#   filter(NFS_bin == 0)
# 
# data_nrep_FS_nplant <- data_nrep_FS %>%
#   filter(tree_planting == 0)
# 
# 
# data <- data_nrep_FS_nplant
# data <- data_nrep_nFS

# Select relevant columns

data <- data %>%
  dplyr::select(returned,dist_unburned,elevation,slope,aspect,tri,tpi, ppt_1post, tdmean_1post, tmean_1post, vpdmin_1post,tmax_1post,
                vpdmax_1post,tmin_1post,ppt_1post_abs, tdmean_1post_abs, tmean_1post_abs, vpdmin_1post,tmax_1post_abs,
                vpdmax_1post_abs,tmin_1post_abs,years_since_fire) %>%  #rdnbr_class
  drop_na()



# Convert rdnbr_class to a factor for classification
data$returned <- as.factor(data$returned)

# Sample 5% of the data if necessary (optional, for large datasets)
sampled_data <- data %>% sample_frac(1)

predictor_data <- as.data.table(sampled_data)

# Exclude any response or non-predictor columns
cols_to_exclude <- c("returned")
correlation_data <- predictor_data[, !..cols_to_exclude, with = FALSE]

# Compute correlation matrix
cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs")


cor_dist <- as.dist(1 - abs(cor_matrix))

clust <- hclust(cor_dist, method = "average")


plot(clust, main = "Variable Clustering by Absolute Correlation", cex = 0.7)
abline(h = 0.2, col = "red", lty = 2)  # Draw a cutoff line (adjust h as needed)

clusters <- cutree(clust, h = 0.2)  # Adjust h based on your cutoff
cluster_df <- data.frame(variable = names(clusters), cluster = clusters)

# Define the variables you want to KEEP
vars_to_keep <- c("dist_unburned","elevation","slope","aspect","tpi",
                  "tdmean_1post","tmean_1post",	
                  "vpdmin_1post","tmin_1post","ppt_1post_abs","tmean_1post_abs","vpdmax_1post_abs","years_since_fire")

# Subset only those columns
reduced_data <- correlation_data[, ..vars_to_keep]

# Add back the excluded columns like response or ID
reduced_data <- cbind(reduced_data, predictor_data[, ..cols_to_exclude])


# # Assuming your dataset is called `sampled_data`
# predictor_data <- as.data.table(sampled_data)
# 
# 
# # Step 1: Remove rdnbr_class and RF_pre_veg temporarily
# cols_to_exclude <- c("returned")
# correlation_data <- predictor_data[, !..cols_to_exclude, with = FALSE]
# 
# # Step 2: Calculate the correlation matrix (for columns remaining in correlation_data)
# cor_matrix <- cor(as.matrix(correlation_data), use = "pairwise.complete.obs")
# 
# # Step 3: Identify highly correlated indices
# highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)
# 
# # Step 4: Select only columns not in `highly_correlated`
# remaining_columns <- setdiff(seq_along(correlation_data), highly_correlated)
# reduced_data <- correlation_data[, ..remaining_columns]
# 
# # Step 5: Add back the excluded columns to the reduced_data
# reduced_data <- cbind(reduced_data, predictor_data[, ..cols_to_exclude])

# # Optional: Print removed features due to high correlation
# removed_features <- colnames(correlation_data)[highly_correlated]
# print("Removed features due to high correlation:")
# print(removed_features)


# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(reduced_data$returned, p = 0.7, list = FALSE)
train_data <- reduced_data[train_index, ]
test_data <- reduced_data[-train_index, ]

class_counts <- table(train_data$returned)
class_weights <- 1 / class_counts[train_data$returned]

# # Run the random forest classification model with class weights
# rf_model_class <- ranger(
#   formula = returned ~ .,
#   data = train_data,
#   splitrule = 'extratrees',
#   num.trees = 1000,
#   mtry = 7, #sqrt(ncol(train_data) - 1)
#   min.node.size = 7,
#   importance = 'impurity',
#   probability = TRUE,
#   num.threads = parallel::detectCores() - 1,
#   case.weights = class_weights  # Apply weights to handle imbalance
# )
# 
# 
# # Summary of the model
# print(rf_model_class)
# 
# saveRDS(rf_model_class, file = "C:/Users/jscho/Documents/Megafires-1985-2023/rf_model_class_returned_reburns_plantings.rds")

rf_model_class <- readRDS(file = "C:/Users/jscho/Documents/Megafires-1985-2023/rf_model_class_returned_reburns_plantings.rds")

# View variable importance
importance <- rf_model_class$variable.importance
print(importance)

# Optionally, inspect the variable importance visually
importance_df <- data.frame(
  Feature = names(importance),
  Importance = importance
)

importance_df <- importance_df %>%
  mutate(Category = case_when(
    Feature %in% c("elevation", "slope", "aspect","tpi","tri", "dist_unburned") ~ "topography",
    Feature == "RF_pre_veg" ~ "fuel",
    Feature == "years_since_fire" ~ "fire history",
    TRUE ~ "climate"  # All other features
  ))

as.factor(importance_df$group)


feature_labels <- c(
  "slope" = "Slope",
  "elevation" = "Elevation",
  "tpi" = "Topographic position index",
  "aspect" = "Aspect",
  "dist_unburned" = "Distance to seed source",
  "ppt_1post" = "Post-fire ppt anomaly",
  "vpdmin_1post" = "Post-fire min VPD anomaly",
  "tdmean_1post" = "Post-fire mean dew point T anomaly",
  "ppt_1post_abs" = "Post-fire ppt",
  "tmean_1post" = "Post-fire mean T anomaly",
  "tmean_1post_abs" = "Post-fire mean T",
  "vpdmax_1post_abs" = "Post-fire max VPD",
  "tmin_1post" = "Post-fire min T anomaly",
  "years_since_fire" = "Years since last fire"
  # Add all relevant features here
)

# Plot variable importances
plot_vIMP <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Category)) +
  geom_col(alpha = 0.85) +  # Use geom_col for bar plot
  coord_flip() +
  labs(title = "", x = "Features", y = "Importance") +
  theme_classic() +
  scale_fill_manual(values = c(
    "topography" = "#C37841",
    "fuel" = "#FAD7A1",
    "fire history" = "#22B1B9",
    "climate" = "#2D4249"
  )) +
  scale_x_discrete(labels = feature_labels) +  # Apply label mapping
  theme_classic()


print(plot_vIMP)



library(ggplot2)
library(ggtext)  # Needed for element_markdown

# Define category colors
feature_colors <- c(
  "topography" = "#C37841",
  "fuel" = "#FAD7A1",
  "fire history" = "#22B1B9",
  "climate" = "#2D4249"
)

# Create mapping of Feature → Label and Category
feature_label_map <- feature_labels[names(feature_labels) %in% importance_df$Feature]
category_map <- setNames(importance_df$Category, importance_df$Feature)

# Build custom label vector for axis
axis_labels <- sapply(importance_df$Feature, function(f) {
  color <- feature_colors[category_map[f]]
  label <- feature_label_map[[f]]
  if (!is.null(label)) {
    paste0("<span style='color:", color, "'>", label, "</span>")
  } else {
    f
  }
})

# Plot
plot_vIMP_dots <- ggplot(importance_df, aes(x = Importance, y = reorder(Feature, Importance), color = Category)) +
  geom_point(size = 4) +
  scale_color_manual(values = feature_colors) +
  labs(title = "", x = "Importance", y = "Features", color = "Category") +
  scale_y_discrete(labels = axis_labels) +
  theme_bw(base_size = 20) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.text.y = ggtext::element_markdown(size = 16)  # Enable colored text
  )


# Print the plot
print(plot_vIMP_dots)



#Get variable importance from the ranger model for only the top 8 variables
importance_df <- data.frame(
  Feature = names(rf_model_class$variable.importance),
  Importance = rf_model_class$variable.importance
)

# Select top 8 variables
top_vars <- importance_df %>%
  arrange(desc(Importance)) %>%
  head(8) %>%
  pull(Feature)

print(top_vars)  # Check selected top variables



# Predict on test data
predictions <- predict(rf_model_class, data = test_data)

# Convert probabilities to predicted classes
predicted_classes <- apply(predictions$predictions, 1, function(x) colnames(predictions$predictions)[which.max(x)])

# Now you can create a confusion matrix with test_data$rdnbr_class
confusion_matrix <- table(test_data$returned, predicted_classes)

print(confusion_matrix)


# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))



## New PDP calculations

library(iml)
library(dplyr)
library(ggplot2)

print("starting pdp plot making")

top_10_features <- importance_df %>%
  arrange(desc(Importance)) %>%
  slice(1:10)

# Single variable partial dependence plot
pdp_elevation <- partial(rf_model_class, pred.var = "elevation", train = train_data, which.class = "1", prob = TRUE) #grid.resolution = 20
print("pdp elev done")
pdp_df_elevation <- as.data.frame(pdp_elevation)
pdp_df_elevation$variable <- "elevation"
colnames(pdp_df_elevation)[2] <- "value"
colnames(pdp_df_elevation)[1] <- "indep"
fwrite(pdp_df_elevation, "C:/Users/jscho/Documents/Megafires-1985-2023/PDP_elevation.csv")

pdp_aspect <- partial(rf_model_class, pred.var = "aspect", train = train_data, which.class = "1", prob = TRUE)
print("pdp aspect done")
pdp_df_aspect <- as.data.frame(pdp_aspect)
pdp_df_aspect$variable <- "aspect"
colnames(pdp_df_aspect)[2] <- "value"
colnames(pdp_df_aspect)[1] <- "indep"
fwrite(pdp_df_aspect, "C:/Users/jscho/Documents/Megafires-1985-2023/PDP_aspect.csv")

pdp_slope <- partial(rf_model_class, pred.var = "slope", train = train_data, which.class = "1", prob = TRUE)
print("pdp slope done")
pdp_df_slope <- as.data.frame(pdp_slope)
pdp_df_slope$variable <- "slope"
colnames(pdp_df_slope)[2] <- "value"
colnames(pdp_df_slope)[1] <- "indep"
fwrite(pdp_df_slope, "C:/Users/jscho/Documents/Megafires-1985-2023/PDP_slope.csv")

pdp_tpi <- partial(rf_model_class, pred.var = "tpi", train = train_data, which.class = "1", prob = TRUE)
print("pdp tpi done")
pdp_df_tpi <- as.data.frame(pdp_tpi)
pdp_df_tpi$variable <- "tpi"
colnames(pdp_df_tpi)[2] <- "value"
colnames(pdp_df_tpi)[1] <- "indep"
fwrite(pdp_df_tpi, "C:/Users/jscho/Documents/Megafires-1985-2023/PDP_tpi.csv")

pdp_dist_unburned <- partial(rf_model_class, pred.var = "dist_unburned", train = train_data, which.class = "1", prob = TRUE)
print("pdp dist_unburned")
pdp_df_dist_unburned <- as.data.frame(pdp_dist_unburned)
pdp_df_dist_unburned$variable <- "dist_unburned"
colnames(pdp_df_dist_unburned)[2] <- "value"
colnames(pdp_df_dist_unburned)[1] <- "indep"
fwrite(pdp_df_dist_unburned, "C:/Users/jscho/Documents/Megafires-1985-2023/PDP_dist_unburned.csv")

pdp_tmean_1post <- partial(rf_model_class, pred.var = "tmean_1post_abs", train = train_data, which.class = "1", prob = TRUE)
print("pdp tmean_abs")
pdp_df_tmean_1post <- as.data.frame(pdp_tmean_1post)
pdp_df_tmean_1post$variable <- "tmean_1post"
colnames(pdp_df_tmean_1post)[2] <- "value"
colnames(pdp_df_tmean_1post)[1] <- "indep"
fwrite(pdp_df_tmean_1post, "C:/Users/jscho/Documents/Megafires-1985-2023/PDP_tmean.csv")

pdp_vpdmax_1post <- partial(rf_model_class, pred.var = "vpdmax_1post_abs", train = train_data, which.class = "1", prob = TRUE)
print("pdp vpdmax_1post")
pdp_df_vpdmax_1post <- as.data.frame(pdp_vpdmax_1post)
pdp_df_vpdmax_1post$variable <- "vpdmax_1post"
colnames(pdp_df_vpdmax_1post)[2] <- "value"
colnames(pdp_df_vpdmax_1post)[1] <- "indep"
fwrite(pdp_df_vpdmax_1post, "C:/Users/jscho/Documents/Megafires-1985-2023/PDP_vpdmax.csv")








pdp_df_master_new <- rbind(pdp_df_elevation,pdp_df_aspect, pdp_df_slope,pdp_df_tpi,pdp_df_dist_unburned, pdp_df_tmean_1post)


# Reorder and rename the 'variable' column
pdp_df_master_new$variable <- dplyr::recode(pdp_df_master_new$variable,
                                     'dist_unburned' = 'distance to seedsource',
                                     'elevation' = 'elevation',
                                     'aspect' = 'aspect',
                                     'tpi' = 'TPI',
                                     'slope' = 'slope',
                                     'tmean_1post' = 'post-FY mean T'
)


pdp_df_master_new$variable <- factor(pdp_df_master_new$variable)

fwrite(pdp_df_master_new, "C:/Users/jscho/Documents/Megafires-1985-2023/rf_model_class_returned_reburns_plantings_PDP.csv")

# Create the plot
pdp_years_new <- ggplot(pdp_df_master_new, aes(x = indep, y = value)) +
  geom_line(color = "blue") +
  labs(title = "", x = "", y = "Conifer recovery probability") +
  theme_bw() +
  theme(
    #axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    #axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  ) +
  facet_wrap(~ variable, nrow = 2, ncol = 3, scales = 'free')  # Facet by the reordered 'variable' column

pdp_years_new

saveRDS(pdp_years_new, "C:/Users/jscho/Documents/Megafires-1985-2023/pdp_plots_nreburns_nplantings.rds")


library(cowplot)

# Stack the plots vertically with labels
combined_plot <- plot_grid(
  plot_vIMP, pdp_years_new,
  ncol = 1,
  labels = c("A", "B"),
  label_size = 14,        # Adjust label size as needed
  align = "v",            # Align plots vertically
  axis = "lr"             # Align left and right axes
)

# Display the combined plot
print(combined_plot)


# Hyperparameter tuning

#Define the model training control
# set.seed(123)
# train_control <- trainControl(
#   method = "cv",               # Cross-validation
#   number = 5,                  # Number of folds
#   classProbs = TRUE,           # Enable probabilities
#   summaryFunction = twoClassSummary,  # For ROC-based performance metrics
#   verboseIter = TRUE           # Show progress
# )
# 
# # Define the grid of hyperparameters to search over
# grid <- expand.grid(
#   mtry = c(3, 5, 7, 9),              # Number of variables at each split
#   splitrule = c("gini", "extratrees"), # Split rule
#   min.node.size = c(1, 3, 5, 7)      # Minimum node size
# )
# 
# 
# # Rename levels of the target variable 'returned' to valid R names
# train_data$returned <- factor(train_data$returned,
#                               levels = c(0, 1),
#                               labels = c("class_0", "class_1"))
# # Apply caret's train function for hyperparameter tuning
# set.seed(123)
# rf_tuned <- train(
#   returned ~ .,
#   data = train_data,
#   method = "ranger",
#   metric = "ROC",
#   tuneGrid = grid,
#   trControl = train_control,
#   importance = 'impurity'  # Do not include 'probability' here
# )
# 
# # Print the best hyperparameters
# print(rf_tuned$bestTune)
# 
# # Plot results of the grid search
# plot(rf_tuned)
# 
# # Train the final tuned model using the best parameters
# best_model <- rf_tuned$finalModel




# train_data_sample <- train_data %>% sample_frac(0.1)
# 
# 
# 
# ### Hyperparameter tuning
# 
# # Load packages
# library(caret)
# library(ranger)
# library(dplyr)
# library(purrr)
# 
# # OPTIONAL: parallel backend (recommended for speed)
# library(doParallel)
# cl <- makeCluster(parallel::detectCores() - 1)
# registerDoParallel(cl)
# 
# # Step 0: Make sure target is a factor with valid names
# train_data_sample$returned <- factor(train_data_sample$returned,
#                                      levels = c(0, 1),
#                                      labels = c("class_0", "class_1"))
# 
# # Step 1: Define training control
# set.seed(123)
# train_control <- trainControl(
#   method = "cv",
#   number = 5,
#   classProbs = TRUE,
#   summaryFunction = twoClassSummary,
#   verboseIter = TRUE
# )
# 
# # Step 2: Define hyperparameter grid (excluding num.trees)
# grid <- expand.grid(
#   mtry = c(3, 5, 7, 9),
#   splitrule = c("gini", "extratrees"),
#   min.node.size = c(1, 3, 5, 7)
# )
# 
# # Step 3: Tune main hyperparameters with fixed num.trees (e.g., 500)
# set.seed(123)
# rf_prelim <- train(
#   returned ~ .,
#   data = train_data_sample,
#   method = "ranger",
#   metric = "ROC",
#   tuneGrid = grid,
#   trControl = train_control,
#   importance = "impurity",
#   num.trees = 500
# )
# 
# # Step 4: Extract best hyperparameters
# best_params <- rf_prelim$bestTune
# 
# # Step 5: Tune num.trees using best hyperparameters
# trees_to_try <- c(400, 500, 600, 700, 800, 900, 1000)
# rf_trees_results <- list()
# 
# for (ntree in trees_to_try) {
#   cat("Tuning number of trees:", ntree, "\n")
# 
#   set.seed(123)
#   model <- train(
#     returned ~ .,
#     data = train_data_sample,
#     method = "ranger",
#     metric = "ROC",
#     tuneGrid = best_params,
#     trControl = train_control,
#     importance = "impurity",
#     num.trees = ntree
#   )
# 
#   rf_trees_results[[as.character(ntree)]] <- model
# }
# 
# # Step 6: Combine results and find best performing model
# best_tree_model <- rf_trees_results %>%
#   map_df(~ .x$results %>% mutate(num.trees = .x$finalModel$num.trees), .id = "model_id") %>%
#   arrange(desc(ROC)) %>%
#   slice(1)
# 
# print("Best model based on ROC:")
# print(best_tree_model)
# 
# # Step 7: Stop parallel backend
# stopCluster(cl)
# registerDoSEQ()
# 
# 
