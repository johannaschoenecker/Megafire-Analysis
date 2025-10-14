# Load necessary libraries
library(ranger)
library(dplyr)
library(caret)
library(data.table)  # For fread()
library(pdp)
library(ggplot2)
library(tidyr)
library(tictoc)
library(ggcorrplot)
library(stringr)

patch_metrics_mega <- fread("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Dataframes/SN_patch_metrics_mega_rdnbr_1985_2020.csv")

patch_metrics_mega_edited <- patch_metrics_mega

patch_metrics_mega_edited$con_pre_fire <- patch_metrics_mega_edited$count_RF_pre_veg_7
patch_metrics_mega_edited$not_con_pre_fire <- patch_metrics_mega_edited$count_RF_pre_veg_1 + patch_metrics_mega_edited$count_RF_pre_veg_4 + patch_metrics_mega_edited$count_RF_pre_veg_5

# Read and bind the filtered files into one dataframe
data <- patch_metrics_mega_edited


#data <- data %>%
#  filter(rdnbr_class == 3)

# Select relevant columns
data <- data %>%
  dplyr::select(mean_rdnbr, mode_veg, n, mean_elevation, mean_tmean, mean_tmax, mean_tmin, mean_tdmean, mean_vpdmin, mean_vpdmax, mean_ppt, mean_tmean_1pre, 
                mean_tmax_1pre, mean_tmin_1pre, mean_tdmean_1pre, mean_vpdmin_1pre, mean_vpdmax_1pre, mean_ppt_1pre,mean_tmean_1pre_abs, mean_tmax_1pre_abs, mean_tmin_1pre_abs, 
                mean_tdmean_1pre_abs, mean_vpdmin_1pre_abs, mean_vpdmax_1pre_abs, mean_ppt_1pre_abs, mean_tmean_abs, 
                mean_tmax_abs, mean_tmin_abs, mean_tdmean_abs, mean_vpdmin_abs, mean_vpdmax_abs, mean_ppt_abs,  rdnbr_class,con_pre_fire,
                fire_year, contiguity)   # core_area, perimeter_area, perimeter, contiguity, OBJECTID  # con_pre_fire, not_con_pre_fire  #count_RF_post1_veg_1, count_RF_post1_veg_4, count_RF_post1_veg_5, count_RF_post1_veg_7, count_RF_post2_veg_1, count_RF_post2_veg_4, count_RF_post2_veg_5, count_RF_post2_veg_7,

# Remove any rows with missing values
data <- data %>% drop_na()


# Variable selection

# Set seed for reproducibility
set.seed(123)

# Convert rdnbr_class to a factor for classification
data$rdnbr_class <- as.factor(data$rdnbr_class)

# Convert rdnbr_class to a factor for classification
data$mode_veg <- as.factor(data$mode_veg)

# Sample proportion of the data if necessary
sampled_data <- data %>% sample_frac(1)

# Assuming the dataset is called `sampled_data`
predictor_data <- as.data.table(sampled_data)

# Step 1: Remove rdnbr_class and RF_pre_veg temporarily
cols_to_exclude <- c("contiguity","mean_elevation","rdnbr_class","mode_veg")
correlation_data <- predictor_data[, !..cols_to_exclude, with = FALSE]

# Step 2: Calculate the correlation matrix (for columns remaining in correlation_data)
cor_matrix <- cor(as.matrix(correlation_data), use = "pairwise.complete.obs")

# Step 3: Identify highly correlated indices
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)

# Step 4: Select only columns not in `highly_correlated`
remaining_columns <- setdiff(seq_along(correlation_data), highly_correlated)
reduced_data <- correlation_data[, ..remaining_columns]

# Step 5: Add back the excluded columns to the reduced_data
reduced_data <- cbind(reduced_data, predictor_data[, ..cols_to_exclude])

# Optional: Print removed features due to high correlation
removed_features <- colnames(correlation_data)[highly_correlated]
print("Removed features due to high correlation:")
print(removed_features)




# Sample data to subset

sampled_data <- reduced_data %>% sample_frac(1)

# Split data into train and test sets
train_index <- createDataPartition(sampled_data$contiguity, p = 0.7, list = FALSE)  # 70-30 split
train_data <- sampled_data[train_index, ]
test_data <- sampled_data[-train_index, ]

# Set up parallel processing if available
num_cores <- parallel::detectCores() - 1  # Use all but one core

tic("train RF")

# Train the random forest model
rf_model <- ranger(
  formula = contiguity ~ .,                      # Define rdnbr as the dependent variable
  data = train_data,                        # Training data
  num.trees = 100,                          # Number of trees
  mtry = floor(sqrt(ncol(train_data) - 1)), # Use sqrt of the number of predictors per split
  min.node.size = 10,                       # Minimum node size
  max.depth = 10,                           # Maximum depth of each tree
  importance = 'impurity',                  # Calculate feature importance based on impurity
  num.threads = num_cores                   # Set number of cores for parallel processing
)

toc()

# Output a summary of the model
print(rf_model)

# View variable importance
importance <- rf_model$variable.importance
print(importance)

# Make predictions on the test set
predictions <- predict(rf_model, data = test_data)$predictions

# Calculate performance metrics
actuals <- test_data$contiguity
rsq <- cor(predictions, actuals)^2  # R-squared
rmse <- sqrt(mean((predictions - actuals)^2))  # Root Mean Squared Error

# Print performance metrics
cat("R-squared:", rsq, "\n")
cat("RMSE:", rmse, "\n")

# Optionally, inspect the variable importance visually
importance_df <- data.frame(
  Feature = names(importance),
  Importance = importance
)

importance_df <- importance_df %>%
  mutate(group = case_when(
    Feature %in% c("mean_elevation") ~ "physical conditions",
    Feature %in% c("mode_veg","con_pre_fire") ~ "fuel",
    Feature %in% c("n","mean_rdnbr","rdnbr_class") ~ "fire",
    Feature == "years_since_fire" ~ "fire history",
    TRUE ~ "climate"  # All other features
  ))

as.factor(importance_df$group)

# # Plot variable importance (optional)
# plot_vIMP <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill=group)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(title = "", x = "Features", y = "Importance") +
#   theme_classic()+
#   scale_fill_manual(values = c("physical conditions" = "#C37841",
#                                #"fuel" = "#FAD7A1",
#                                "fire history" = "#22B1B9",
#                                "climate" = "#2D4249"))+
#   theme(
#     axis.title.x = element_text(face = "bold"),  # Bold x-axis label
#     axis.title.y = element_text(face = "bold")   # Bold y-axis label
#   )

# Filter to the top 8 features by Importance
top_10_features <- importance_df %>%
  arrange(desc(Importance)) %>%
  slice(1:10)

# Plot variable importance for top 8 features
plot_vIMP_contiguity <- ggplot(top_10_features, aes(x = reorder(Feature, Importance), y = Importance, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Contiguity", x = "Features", y = "Importance") +
  theme_classic() +
  scale_x_discrete(labels = c("n" = "patch size", "con_pre_fire" = "# coniferous pixels", "mean_ppt" = "mean FY precipitation anomaly",
                              "mean_tdmean" = "mean FY dew point temperature anomaly", "mean_rdnbr" = "mean RdNBR","mean_vpdmin_1pre" = "mean pre-FY min VPD anomaly",
                              "mean_vpdmin_abs" = "mean FY min VPD","mean_ppt_abs" = "mean FY precipitation","mean_tmin" = "mean FY min temperature anomaly", "mean_vpdmin" = "mean FY min VPD anomaly",
                              "mode_veg" = "mode vegetation", "rdnbr_class" = "RdNBR class","mean_ppt_1pre_abs" = "mean pre-FY precipitation", "mean_elevation" = "mean elevation")) +
  scale_fill_manual(values = c("physical conditions" = "#C37841",
                               "fuel" = "#FAD7A1",
                               "fire" = 'purple',
                               "fire history" = "#22B1B9",
                               "climate" = "#2D4249")) +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold")   # Bold y-axis label
  )


tic("calculate pdp")


# Single variable partial dependence plot
pdp_mean_rdnbr <- partial(rf_model, pred.var = "mean_rdnbr", train = train_data, grid.resolution = 40) #grid.resolution = 20
pdp_n <- partial(rf_model, pred.var = "n", train = train_data, grid.resolution = 40)
pdp_con_pre_fire <- partial(rf_model, pred.var = "con_pre_fire", train = train_data, grid.resolution = 40)
pdp_mean_ppt_1pre_abs <- partial(rf_model, pred.var = "mean_ppt_1pre_abs", train = train_data, grid.resolution = 40)
pdp_mean_ppt_abs <- partial(rf_model, pred.var = "mean_ppt_abs", train = train_data, grid.resolution = 40)
pdp_mean_elevation <- partial(rf_model, pred.var = "mean_elevation", train = train_data, grid.resolution = 40)
pdp_mean_tdmean <- partial(rf_model, pred.var = "mean_tdmean", train = train_data, grid.resolution = 40)
pdp_mean_ppt <- partial(rf_model, pred.var = "mean_ppt", train = train_data, grid.resolution = 40)


toc()

# Extract data from PDP object
pdp_df_mean_rdnbr <- as.data.frame(pdp_mean_rdnbr)
pdp_df_n <- as.data.frame(pdp_n)
pdp_df_con_pre_fire <- as.data.frame(pdp_con_pre_fire)
pdp_df_mean_ppt_1pre_abs <- as.data.frame(pdp_mean_ppt_1pre_abs)
pdp_df_mean_ppt_abs <- as.data.frame(pdp_mean_ppt_abs)
pdp_df_mean_elevation <- as.data.frame(pdp_mean_elevation)
pdp_df_mean_tdmean <- as.data.frame(pdp_mean_tdmean)
pdp_df_mean_ppt <- as.data.frame(pdp_mean_ppt)


pdp_df_mean_rdnbr$variable <- "mean_rdnbr"
colnames(pdp_df_mean_rdnbr)[2] <- "value"
colnames(pdp_df_mean_rdnbr)[1] <- "indep"

pdp_df_n$variable <- "n"
colnames(pdp_df_n)[2] <- "value"
colnames(pdp_df_n)[1] <- "indep"

pdp_df_con_pre_fire$variable <- "con_pre_fire"
colnames(pdp_df_con_pre_fire)[2] <- "value"
colnames(pdp_df_con_pre_fire)[1] <- "indep"

pdp_df_mean_ppt_1pre_abs$variable <- "mean_ppt_1pre_abs"
colnames(pdp_df_mean_ppt_1pre_abs)[2] <- "value"
colnames(pdp_df_mean_ppt_1pre_abs)[1] <- "indep"

pdp_df_mean_ppt_abs$variable <- "mean_ppt_abs"
colnames(pdp_df_mean_ppt_abs)[2] <- "value"
colnames(pdp_df_mean_ppt_abs)[1] <- "indep"

pdp_df_mean_elevation$variable <- "mean_elevation"
colnames(pdp_df_mean_elevation)[2] <- "value"
colnames(pdp_df_mean_elevation)[1] <- "indep"

pdp_df_mean_tdmean$variable <- "mean_tdmean"
colnames(pdp_df_mean_tdmean)[2] <- "value"
colnames(pdp_df_mean_tdmean)[1] <- "indep"

pdp_df_mean_ppt$variable <- "mean_ppt"
colnames(pdp_df_mean_ppt)[2] <- "value"
colnames(pdp_df_mean_ppt)[1] <- "indep"



pdp_df_master <- rbind(pdp_df_mean_rdnbr,pdp_df_n, pdp_df_con_pre_fire, pdp_df_mean_ppt_1pre_abs,pdp_df_mean_ppt_abs, pdp_df_mean_elevation, pdp_df_mean_tdmean, pdp_df_mean_ppt)


# Define custom labels as a named vector
custom_labels <- c("n" = "patch size",
                   "con_pre_fire" = "# coniferous pixels",
                   "mean_ppt" = "mean FY precipitation anomaly",
                   "mean_tdmean" = "mean FY dew point temperature anomaly",
                   "mean_rdnbr" = "mean RdNBR",
                   "mean_vpdmin_1pre" = "mean pre-FY min VPD anomaly",
                   "mean_vpdmin_abs" = "mean FY min VPD",
                   "mean_ppt_abs" = "mean FY precipitation",
                   "mean_tmin" = "mean FY min temperature anomaly",
                   "mean_vpdmin" = "mean FY min VPD anomaly",
                   "mode_veg" = "mode vegetation",
                   "rdnbr_class" = "RdNBR class",
                   "mean_ppt_1pre_abs" = "mean pre-FY precipitation",
                   "mean_elevation" = "mean elevation"
)

pdp_master_contiguity <- ggplot(pdp_df_master, aes(x = indep, y = value)) +
  geom_line(color = "blue") +
  labs(title = "", x = "", y = "Predicted contiguity") +
  #scale_y_continuous(c(0,1000))+
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),   # Remove minor gridlines
    strip.text = element_text(face = "bold")  # Make facet labels bold
  ) +
  facet_wrap(~ variable,nrow = 2, ncol = 4, scales = 'free',
             labeller = labeller(variable = custom_labels))  # Facet by the 'variable' column    , scales = 'free'

plot(pdp_master_contiguity)


##################### core_area  ############################################

# Read and bind the filtered files into one dataframe
data <- patch_metrics_mega_edited


#data <- data %>%
#  filter(rdnbr_class == 3)

# Select relevant columns
data <- data %>%
  dplyr::select(mean_rdnbr, mode_veg, n, mean_elevation, mean_tmean, mean_tmax, mean_tmin, mean_tdmean, mean_vpdmin, mean_vpdmax, mean_ppt, mean_tmean_1pre, 
                mean_tmax_1pre, mean_tmin_1pre, mean_tdmean_1pre, mean_vpdmin_1pre, mean_vpdmax_1pre, mean_ppt_1pre,mean_tmean_1pre_abs, mean_tmax_1pre_abs, mean_tmin_1pre_abs, 
                mean_tdmean_1pre_abs, mean_vpdmin_1pre_abs, mean_vpdmax_1pre_abs, mean_ppt_1pre_abs, mean_tmean_abs, 
                mean_tmax_abs, mean_tmin_abs, mean_tdmean_abs, mean_vpdmin_abs, mean_vpdmax_abs, mean_ppt_abs,  rdnbr_class,con_pre_fire,
                fire_year, core_area)   # core_area, perimeter_area, perimeter, contiguity, OBJECTID  # con_pre_fire, not_con_pre_fire  #count_RF_post1_veg_1, count_RF_post1_veg_4, count_RF_post1_veg_5, count_RF_post1_veg_7, count_RF_post2_veg_1, count_RF_post2_veg_4, count_RF_post2_veg_5, count_RF_post2_veg_7,

# Remove any rows with missing values
data <- data %>% drop_na()


# Variable selection

# Set seed for reproducibility
set.seed(123)

# Convert rdnbr_class to a factor for classification
data$rdnbr_class <- as.factor(data$rdnbr_class)

# Convert rdnbr_class to a factor for classification
data$mode_veg <- as.factor(data$mode_veg)

# Sample proportion of the data if necessary
sampled_data <- data %>% sample_frac(1)

# Assuming the dataset is called `sampled_data`
predictor_data <- as.data.table(sampled_data)

# Step 1: Remove rdnbr_class and RF_pre_veg temporarily
cols_to_exclude <- c("core_area","mean_elevation","rdnbr_class","mode_veg")
correlation_data <- predictor_data[, !..cols_to_exclude, with = FALSE]

# Step 2: Calculate the correlation matrix (for columns remaining in correlation_data)
cor_matrix <- cor(as.matrix(correlation_data), use = "pairwise.complete.obs")

# Step 3: Identify highly correlated indices
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)

# Step 4: Select only columns not in `highly_correlated`
remaining_columns <- setdiff(seq_along(correlation_data), highly_correlated)
reduced_data <- correlation_data[, ..remaining_columns]

# Step 5: Add back the excluded columns to the reduced_data
reduced_data <- cbind(reduced_data, predictor_data[, ..cols_to_exclude])

# Optional: Print removed features due to high correlation
removed_features <- colnames(correlation_data)[highly_correlated]
print("Removed features due to high correlation:")
print(removed_features)




# Sample data to subset

sampled_data <- reduced_data %>% sample_frac(1)

# Split data into train and test sets
train_index <- createDataPartition(sampled_data$core_area, p = 0.7, list = FALSE)  # 70-30 split
train_data <- sampled_data[train_index, ]
test_data <- sampled_data[-train_index, ]

# Set up parallel processing if available
num_cores <- parallel::detectCores() - 1  # Use all but one core

tic("train RF")

# Train the random forest model
rf_model <- ranger(
  formula = core_area ~ .,                      # Define rdnbr as the dependent variable
  data = train_data,                        # Training data
  num.trees = 100,                          # Number of trees
  mtry = floor(sqrt(ncol(train_data) - 1)), # Use sqrt of the number of predictors per split
  min.node.size = 10,                       # Minimum node size
  max.depth = 10,                           # Maximum depth of each tree
  importance = 'impurity',                  # Calculate feature importance based on impurity
  num.threads = num_cores                   # Set number of cores for parallel processing
)

toc()

# Output a summary of the model
print(rf_model)

# View variable importance
importance <- rf_model$variable.importance
print(importance)

# Make predictions on the test set
predictions <- predict(rf_model, data = test_data)$predictions

# Calculate performance metrics
actuals <- test_data$core_area
rsq <- cor(predictions, actuals)^2  # R-squared
rmse <- sqrt(mean((predictions - actuals)^2))  # Root Mean Squared Error

# Print performance metrics
cat("R-squared:", rsq, "\n")
cat("RMSE:", rmse, "\n")

# Optionally, inspect the variable importance visually
importance_df <- data.frame(
  Feature = names(importance),
  Importance = importance
)

importance_df <- importance_df %>%
  mutate(group = case_when(
    Feature %in% c("mean_elevation") ~ "physical conditions",
    Feature %in% c("mode_veg","con_pre_fire") ~ "fuel",
    Feature %in% c("n","mean_rdnbr","rdnbr_class") ~ "fire",
    Feature == "years_since_fire" ~ "fire history",
    TRUE ~ "climate"  # All other features
  ))

as.factor(importance_df$group)

# # Plot variable importance (optional)
# plot_vIMP <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill=group)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(title = "", x = "Features", y = "Importance") +
#   theme_classic()+
#   scale_fill_manual(values = c("physical conditions" = "#C37841",
#                                #"fuel" = "#FAD7A1",
#                                "fire history" = "#22B1B9",
#                                "climate" = "#2D4249"))+
#   theme(
#     axis.title.x = element_text(face = "bold"),  # Bold x-axis label
#     axis.title.y = element_text(face = "bold")   # Bold y-axis label
#   )

# Filter to the top 8 features by Importance
top_10_features <- importance_df %>%
  arrange(desc(Importance)) %>%
  slice(1:10)

# Plot variable importance for top 8 features
plot_vIMP_core_area <- ggplot(top_10_features, aes(x = reorder(Feature, Importance), y = Importance, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Core area", x = "Features", y = "Importance") +
  scale_x_discrete(labels = c("n" = "patch size", "con_pre_fire" = "# coniferous pixels", "mean_ppt" = "mean FY precipitation anomaly","mean_tdmean" = "mean FY dew point temperature anomaly",
                              "mean_rdnbr" = "mean RdNBR","mean_vpdmin_1pre" = "mean pre-FY min VPD anomaly","mean_vpdmin_abs" = "mean FY min VPD", "mean_ppt_abs" = "mean FY precipitation",
                              "mean_tmin" = "mean FY min temperature anomaly","mean_vpdmin" = "mean FY min VPD anomaly")) +
  theme_classic() +
  scale_fill_manual(values = c("physical conditions" = "#C37841",
                               "fuel" = "#FAD7A1",
                               "fire" = 'purple',
                               "fire history" = "#22B1B9",
                               "climate" = "#2D4249")) +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold")   # Bold y-axis label
  )



# Single variable partial dependence plot
pdp_mean_rdnbr <- partial(rf_model, pred.var = "mean_rdnbr", train = train_data, grid.resolution = 40) #grid.resolution = 20
pdp_n <- partial(rf_model, pred.var = "n", train = train_data, grid.resolution = 40)
pdp_con_pre_fire <- partial(rf_model, pred.var = "con_pre_fire", train = train_data, grid.resolution = 40)
pdp_mean_tdmean <- partial(rf_model, pred.var = "mean_tdmean", train = train_data, grid.resolution = 40)
pdp_mean_ppt <- partial(rf_model, pred.var = "mean_ppt", train = train_data, grid.resolution = 40)
pdp_mean_vpdmin_1pre <- partial(rf_model, pred.var = "mean_vpdmin_1pre", train = train_data, grid.resolution = 40)
pdp_mean_vpdmin_abs <- partial(rf_model, pred.var = "mean_vpdmin_abs", train = train_data, grid.resolution = 40)
pdp_mean_ppt_abs <- partial(rf_model, pred.var = "mean_ppt_abs", train = train_data, grid.resolution = 40)
pdp_mean_tmin <- partial(rf_model, pred.var = "mean_tmin", train = train_data, grid.resolution = 40)
pdp_mean_vpdmin <- partial(rf_model, pred.var = "mean_vpdmin", train = train_data, grid.resolution = 40)


# Extract data from PDP object
pdp_df_mean_rdnbr <- as.data.frame(pdp_mean_rdnbr)
pdp_df_n <- as.data.frame(pdp_n)
pdp_df_con_pre_fire <- as.data.frame(pdp_con_pre_fire)
pdp_df_mean_tdmean <- as.data.frame(pdp_mean_tdmean)
pdp_df_mean_ppt <- as.data.frame(pdp_mean_ppt)
pdp_df_mean_vpdmin_1pre <- as.data.frame(pdp_mean_vpdmin_1pre)
pdp_df_mean_vpdmin_abs <- as.data.frame(pdp_mean_vpdmin_abs)
pdp_df_mean_ppt_abs <- as.data.frame(pdp_mean_ppt_abs)
pdp_df_mean_tmin <- as.data.frame(pdp_mean_tmin)
pdp_df_mean_vpdmin <- as.data.frame(pdp_mean_vpdmin)



pdp_df_mean_rdnbr$variable <- "mean_rdnbr"
colnames(pdp_df_mean_rdnbr)[2] <- "value"
colnames(pdp_df_mean_rdnbr)[1] <- "indep"

pdp_df_n$variable <- "n"
colnames(pdp_df_n)[2] <- "value"
colnames(pdp_df_n)[1] <- "indep"

pdp_df_con_pre_fire$variable <- "con_pre_fire"
colnames(pdp_df_con_pre_fire)[2] <- "value"
colnames(pdp_df_con_pre_fire)[1] <- "indep"

pdp_df_mean_tdmean$variable <- "mean_tdmean"
colnames(pdp_df_mean_tdmean)[2] <- "value"
colnames(pdp_df_mean_tdmean)[1] <- "indep"

pdp_df_mean_ppt$variable <- "mean_ppt"
colnames(pdp_df_mean_ppt)[2] <- "value"
colnames(pdp_df_mean_ppt)[1] <- "indep"

pdp_df_mean_vpdmin_1pre$variable <- "mean_vpdmin_1pre"
colnames(pdp_df_mean_vpdmin_1pre)[2] <- "value"
colnames(pdp_df_mean_vpdmin_1pre)[1] <- "indep"

pdp_df_mean_vpdmin_abs$variable <- "mean_vpdmin_abs"
colnames(pdp_df_mean_vpdmin_abs)[2] <- "value"
colnames(pdp_df_mean_vpdmin_abs)[1] <- "indep"

pdp_df_mean_ppt_abs$variable <- "mean_ppt_abs"
colnames(pdp_df_mean_ppt_abs)[2] <- "value"
colnames(pdp_df_mean_ppt_abs)[1] <- "indep"

pdp_df_mean_tmin$variable <- "mean_tmin"
colnames(pdp_df_mean_tmin)[2] <- "value"
colnames(pdp_df_mean_tmin)[1] <- "indep"

pdp_df_mean_vpdmin$variable <- "mean_vpdmin"
colnames(pdp_df_mean_vpdmin)[2] <- "value"
colnames(pdp_df_mean_vpdmin)[1] <- "indep"

custom_labels <- c("n" = "patch size",
                   "con_pre_fire" = "# coniferous pixels",
                   "mean_ppt" = "mean FY precipitation anomaly",
                   "mean_tdmean" = "mean FY dew point temperature anomaly",
                   "mean_rdnbr" = "mean RdNBR",
                   "mean_vpdmin_1pre" = "mean pre-FY min VPD anomaly",
                   "mean_vpdmin_abs" = "mean FY min VPD",
                   "mean_ppt_abs" = "mean FY precipitation",
                   "mean_tmin" = "mean FY min temperature anomaly",
                   "mean_vpdmin" = "mean FY min VPD anomaly",
                   "mode_veg" = "mode vegetation",
                   "rdnbr_class" = "RdNBR class",
                   "mean_ppt_1pre_abs" = "mean pre-FY precipitation",
                   "mean_elevation" = "mean elevation"
)


pdp_df_master <- rbind(pdp_df_mean_rdnbr,pdp_df_n, pdp_df_con_pre_fire,
                       pdp_df_mean_tdmean,pdp_df_mean_ppt, pdp_df_mean_vpdmin_1pre, pdp_df_mean_vpdmin_abs, pdp_df_mean_ppt_abs,
                       pdp_df_mean_tmin, pdp_df_mean_vpdmin)


pdp_master_core_area <- ggplot(pdp_df_master, aes(x = indep, y = value)) +
  geom_line(color = "blue") +
  labs(title = "", x = "", y = "Predicted core area") +
  #scale_y_continuous(c(0,1000))+
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),   # Remove minor gridlines
    strip.text = element_text(face = "bold")  # Make facet labels bold
  ) +
  facet_wrap(~ variable,nrow = 2, ncol = 5, scales = 'free',
             labeller = labeller(variable = custom_labels))  # Facet by the 'variable' column    , scales = 'free'


plot(pdp_master_core_area)



##################### perimeter_area  ############################################

# Read and bind the filtered files into one dataframe
data <- patch_metrics_mega_edited


#data <- data %>%
#  filter(rdnbr_class == 3)

# Select relevant columns
data <- data %>%
  dplyr::select(mean_rdnbr, mode_veg, n, mean_elevation, mean_tmean, mean_tmax, mean_tmin, mean_tdmean, mean_vpdmin, mean_vpdmax, mean_ppt, mean_tmean_1pre, 
                mean_tmax_1pre, mean_tmin_1pre, mean_tdmean_1pre, mean_vpdmin_1pre, mean_vpdmax_1pre, mean_ppt_1pre,mean_tmean_1pre_abs, mean_tmax_1pre_abs, mean_tmin_1pre_abs, 
                mean_tdmean_1pre_abs, mean_vpdmin_1pre_abs, mean_vpdmax_1pre_abs, mean_ppt_1pre_abs, mean_tmean_abs, 
                mean_tmax_abs, mean_tmin_abs, mean_tdmean_abs, mean_vpdmin_abs, mean_vpdmax_abs, mean_ppt_abs,  rdnbr_class,con_pre_fire,
                fire_year, perimeter_area)   # core_area, perimeter_area, perimeter, contiguity, OBJECTID  # con_pre_fire, not_con_pre_fire  #count_RF_post1_veg_1, count_RF_post1_veg_4, count_RF_post1_veg_5, count_RF_post1_veg_7, count_RF_post2_veg_1, count_RF_post2_veg_4, count_RF_post2_veg_5, count_RF_post2_veg_7,

# Remove any rows with missing values
data <- data %>% drop_na()


# Variable selection

# Set seed for reproducibility
set.seed(123)

# Convert rdnbr_class to a factor for classification
data$rdnbr_class <- as.factor(data$rdnbr_class)

# Convert rdnbr_class to a factor for classification
data$mode_veg <- as.factor(data$mode_veg)

# Sample proportion of the data if necessary
sampled_data <- data %>% sample_frac(1)

# Assuming the dataset is called `sampled_data`
predictor_data <- as.data.table(sampled_data)

# Step 1: Remove rdnbr_class and RF_pre_veg temporarily
cols_to_exclude <- c("perimeter_area","mean_elevation","rdnbr_class","mode_veg")
correlation_data <- predictor_data[, !..cols_to_exclude, with = FALSE]

# Step 2: Calculate the correlation matrix (for columns remaining in correlation_data)
cor_matrix <- cor(as.matrix(correlation_data), use = "pairwise.complete.obs")

# Step 3: Identify highly correlated indices
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.8)

# Step 4: Select only columns not in `highly_correlated`
remaining_columns <- setdiff(seq_along(correlation_data), highly_correlated)
reduced_data <- correlation_data[, ..remaining_columns]

# Step 5: Add back the excluded columns to the reduced_data
reduced_data <- cbind(reduced_data, predictor_data[, ..cols_to_exclude])

# Optional: Print removed features due to high correlation
removed_features <- colnames(correlation_data)[highly_correlated]
print("Removed features due to high correlation:")
print(removed_features)




# Sample data to subset

sampled_data <- reduced_data %>% sample_frac(1)

# Split data into train and test sets
train_index <- createDataPartition(sampled_data$perimeter_area, p = 0.7, list = FALSE)  # 70-30 split
train_data <- sampled_data[train_index, ]
test_data <- sampled_data[-train_index, ]

# Set up parallel processing if available
num_cores <- parallel::detectCores() - 1  # Use all but one core

tic("train RF")

# Train the random forest model
rf_model <- ranger(
  formula = perimeter_area ~ .,                      # Define rdnbr as the dependent variable
  data = train_data,                        # Training data
  num.trees = 100,                          # Number of trees
  mtry = floor(sqrt(ncol(train_data) - 1)), # Use sqrt of the number of predictors per split
  min.node.size = 10,                       # Minimum node size
  max.depth = 10,                           # Maximum depth of each tree
  importance = 'impurity',                  # Calculate feature importance based on impurity
  num.threads = num_cores                   # Set number of cores for parallel processing
)

toc()

# Output a summary of the model
print(rf_model)

# View variable importance
importance <- rf_model$variable.importance
print(importance)

# Make predictions on the test set
predictions <- predict(rf_model, data = test_data)$predictions

# Calculate performance metrics
actuals <- test_data$perimeter_area
rsq <- cor(predictions, actuals)^2  # R-squared
rmse <- sqrt(mean((predictions - actuals)^2))  # Root Mean Squared Error

# Print performance metrics
cat("R-squared:", rsq, "\n")
cat("RMSE:", rmse, "\n")

# Optionally, inspect the variable importance visually
importance_df <- data.frame(
  Feature = names(importance),
  Importance = importance
)

importance_df <- importance_df %>%
  mutate(group = case_when(
    Feature %in% c("mean_elevation") ~ "physical conditions",
    Feature %in% c("mode_veg","con_pre_fire") ~ "fuel",
    Feature %in% c("n","mean_rdnbr","rdnbr_class") ~ "fire",
    Feature == "years_since_fire" ~ "fire history",
    TRUE ~ "climate"  # All other features
  ))

as.factor(importance_df$group)


top_10_features <- importance_df %>%
  arrange(desc(Importance)) %>%
  slice(1:10)

# Plot variable importance for top 8 features
plot_vIMP_perimeter_area <- ggplot(top_10_features, aes(x = reorder(Feature, Importance), y = Importance, fill = group)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Perimeter : area", x = "Features", y = "Importance") +
  scale_x_discrete(labels = c("n" = "patch size", "con_pre_fire" = "# coniferous pixels", "mean_rdnbr" = "mean RdNBR","mode_veg" = "mode vegetation type",
                              "rdnbr_class" = "RdNBR class","mean_ppt_1pre_abs" = "mean pre-FY precipitation","mean_elevation" = "mean elevation", "mean_ppt_abs" = "mean FY precipitation",
                              "mean_tdmean" = "mean FY dew point temperature")) +
  theme_classic() +
  scale_fill_manual(values = c("physical conditions" = "#C37841",
                               "fuel" = "#FAD7A1",
                               "fire" = "purple",
                               "fire history" = "#22B1B9",
                               "climate" = "#2D4249")) +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold"),   # Remove minor gridlines
    strip.text = element_text(face = "bold")  # Make facet labels bold
  ) +
  facet_wrap(~ variable,nrow = 2, ncol = 5, scales = 'free',
             labeller = labeller(variable = custom_labels))  # Facet by the 'variable' column    , scales = 'free'



# Single variable partial dependence plot
pdp_mean_rdnbr <- partial(rf_model, pred.var = "mean_rdnbr", train = train_data, grid.resolution = 40) #grid.resolution = 20
pdp_n <- partial(rf_model, pred.var = "n", train = train_data, grid.resolution = 40)
pdp_con_pre_fire <- partial(rf_model, pred.var = "con_pre_fire", train = train_data, grid.resolution = 40)
pdp_mean_tdmean <- partial(rf_model, pred.var = "mean_tdmean", train = train_data, grid.resolution = 40)
pdp_mean_ppt_1pre_abs <- partial(rf_model, pred.var = "mean_ppt_1pre_abs", train = train_data, grid.resolution = 40)
pdp_mean_elevation <- partial(rf_model, pred.var = "mean_elevation", train = train_data, grid.resolution = 40)
pdp_mean_ppt_abs <- partial(rf_model, pred.var = "mean_ppt_abs", train = train_data, grid.resolution = 40)
pdp_mean_vpdmax <- partial(rf_model, pred.var = "mean_vpdmax", train = train_data, grid.resolution = 40)


# Extract data from PDP object
pdp_df_mean_rdnbr <- as.data.frame(pdp_mean_rdnbr)
pdp_df_n <- as.data.frame(pdp_n)
pdp_df_con_pre_fire <- as.data.frame(pdp_con_pre_fire)
pdp_df_mean_tdmean <- as.data.frame(pdp_mean_tdmean)
pdp_df_mean_ppt_1pre_abs <- as.data.frame(pdp_mean_ppt_1pre_abs)
pdp_df_mean_elevation <- as.data.frame(pdp_mean_elevation)
pdp_df_mean_ppt_abs <- as.data.frame(pdp_mean_ppt_abs)
pdp_df_mean_vpdmax <- as.data.frame(pdp_mean_vpdmax)



pdp_df_mean_rdnbr$variable <- "mean_rdnbr"
colnames(pdp_df_mean_rdnbr)[2] <- "value"
colnames(pdp_df_mean_rdnbr)[1] <- "indep"

pdp_df_n$variable <- "n"
colnames(pdp_df_n)[2] <- "value"
colnames(pdp_df_n)[1] <- "indep"

pdp_df_con_pre_fire$variable <- "con_pre_fire"
colnames(pdp_df_con_pre_fire)[2] <- "value"
colnames(pdp_df_con_pre_fire)[1] <- "indep"

pdp_df_mean_tdmean$variable <- "mean_tdmean"
colnames(pdp_df_mean_tdmean)[2] <- "value"
colnames(pdp_df_mean_tdmean)[1] <- "indep"

pdp_df_mean_ppt_1pre_abs$variable <- "mean_ppt_1pre_abs"
colnames(pdp_df_mean_ppt_1pre_abs)[2] <- "value"
colnames(pdp_df_mean_ppt_1pre_abs)[1] <- "indep"

pdp_df_mean_elevation$variable <- "mean_elevation"
colnames(pdp_df_mean_elevation)[2] <- "value"
colnames(pdp_df_mean_elevation)[1] <- "indep"


pdp_df_mean_ppt_abs$variable <- "mean_ppt_abs"
colnames(pdp_df_mean_ppt_abs)[2] <- "value"
colnames(pdp_df_mean_ppt_abs)[1] <- "indep"

pdp_df_mean_vpdmax$variable <- "mean_vpdmax"
colnames(pdp_df_mean_vpdmax)[2] <- "value"
colnames(pdp_df_mean_vpdmax)[1] <- "indep"




pdp_df_master <- rbind(pdp_df_mean_rdnbr,pdp_df_n, pdp_df_con_pre_fire,
                       pdp_df_mean_tdmean,pdp_df_mean_ppt_1pre_abs, pdp_df_mean_elevation, pdp_df_mean_ppt_abs,
                       pdp_df_mean_vpdmax)


custom_labels = c("n" = "patch size", "con_pre_fire" = "# coniferous pixels", "mean_rdnbr" = "mean RdNBR","mode_veg" = "mode vegetation type",
           "rdnbr_class" = "RdNBR class","mean_ppt_1pre_abs" = "mean pre-FY precipitation","mean_elevation" = "mean elevation", "mean_ppt_abs" = "mean FY precipitation",
           "mean_tdmean" = "mean FY dew point temperature")


pdp_master_perimeter_area <- ggplot(pdp_df_master, aes(x = indep, y = value)) +
  geom_line(color = "blue") +
  labs(title = "", x = "", y = "Predicted perimeter : area") +
  #scale_y_continuous(c(0,1000))+
  theme_bw() +
  theme(
    axis.title.x = element_text(face = "bold"),  # Bold x-axis label
    axis.title.y = element_text(face = "bold"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),   # Remove minor gridlines
    strip.text = element_text(face = "bold")  # Make facet labels bold
  ) +
  facet_wrap(~ variable,nrow = 2, ncol = 5, scales = 'free',
             labeller = labeller(variable = custom_labels))  # Facet by the 'variable' column    , scales = 'free'

plot(pdp_master_perimeter_area)






###### Combining model outputs
library(cowplot)
plot_vIMP_contiguity <- plot_vIMP_contiguity +
  theme(legend.position = 'none')

plot_vIMP_core_area <- plot_vIMP_core_area +
  theme(legend.position = 'none')

plot_vIMP_perimeter_area <- plot_vIMP_perimeter_area +
  theme(legend.position = 'none')

final_plot <- plot_grid(plot_vIMP_contiguity, plot_vIMP_core_area, plot_vIMP_perimeter_area, ncol = 3, align = "h")

plot(pdp_master_contiguity)
