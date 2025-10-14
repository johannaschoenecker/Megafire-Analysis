# Load necessary libraries
library(ranger)
library(dplyr)
library(data.table)
library(caret)
library(tidyr)
library(pdp)
library(ggplot2)
library(stringr)
library(cowplot)


patch_metrics_mega <- fread("C:/Users/jscho/OneDrive - University of Cambridge/PhD/Analyses/Dataframes/SN_patch_metrics_mega_rdnbr_1985_2020.csv")

######## Core area ###########################


## average core area 

patch_metrics_annual_ca <- patch_metrics_mega %>%
  group_by(fire_year,rdnbr_class) %>%
  summarise(mn_core_area = mean(core_area), sd_core_area = sd(core_area))

patch_metrics_annual_ca_hs <- patch_metrics_annual_ca %>%
  filter(rdnbr_class %in% c(1,2,3))


# Create scatter plot

pl_ca <- ggplot(patch_metrics_annual_ca_hs, aes(x = fire_year, y = mn_core_area, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Mean core area",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))


# average core area by fire year and OBJECTID
patch_metrics_annual_ca <- patch_metrics_mega %>%
  group_by(OBJECTID,rdnbr_class) %>%
  summarise(mn_core_area = mean(core_area), sd_core_area = sd(core_area))


# Ensure each OBJECTID has only one fire_year in patch_metrics_mega
unique_fire_years <- patch_metrics_mega %>%
  select(OBJECTID, fire_year) %>%
  distinct(OBJECTID, .keep_all = TRUE)  # Keep only one unique row per OBJECTID

# Perform the left join to assign fire_year to patch_metrics_annual_prop_ca
patch_metrics_annual_ca <- patch_metrics_annual_ca %>%
  left_join(unique_fire_years, by = "OBJECTID")

patch_metrics_annual_ca_hs <- patch_metrics_annual_ca %>%
  filter(rdnbr_class %in% c(1,2,3))

pl_ca <- ggplot(patch_metrics_annual_ca_hs, aes(x = fire_year, y = mn_core_area, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Mean core area",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))



# cumulative core area

patch_metrics_annual_ca_sum <- patch_metrics_mega %>%
  group_by(fire_year,rdnbr_class) %>%
  summarise(sum_core_area = sum(core_area))


# Create scatter plot
ggplot(patch_metrics_annual_ca_sum, aes(x = fire_year, y = sum_core_area, fill = as.factor(rdnbr_class))) +
  geom_col(position = "dodge") +  # Use geom_col() for bar charts
  labs(
    title = "Stacked Bar Plot of Total Core Area Over Fire Years",
    x = "Fire Year",
    y = "Total Core Area",
    fill = "RDNBR Class"  # Legend title
  ) +
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  )



######## Contiguity ###########################
## average contiguity

patch_metrics_annual_cont <- patch_metrics_mega %>%
  group_by(fire_year,rdnbr_class) %>%
  summarise(mn_contiguity = mean(contiguity), sd_contiguity = sd(contiguity))

patch_metrics_annual_cont_hs <- patch_metrics_annual_cont %>%
  filter(rdnbr_class %in% c(1,2,3))


# Create scatter plot
ggplot(patch_metrics_annual_cont_hs, aes(x = fire_year, y = mn_contiguity, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Mean contiguity",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth()+
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  )


pl_cont <- ggplot(patch_metrics_annual_cont_hs, aes(x = fire_year, y = mn_contiguity, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Mean contiguity",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))



# average contiguity by fire year and OBJECTID
patch_metrics_annual_cont <- patch_metrics_mega %>%
  group_by(OBJECTID,rdnbr_class) %>%
  summarise(mn_contiguity = mean(contiguity), sd_contiguity = sd(contiguity))


# Ensure each OBJECTID has only one fire_year in patch_metrics_mega
unique_fire_years <- patch_metrics_mega %>%
  select(OBJECTID, fire_year) %>%
  distinct(OBJECTID, .keep_all = TRUE)  # Keep only one unique row per OBJECTID

# Perform the left join to assign fire_year to patch_metrics_annual_prop_ca
patch_metrics_annual_cont <- patch_metrics_annual_cont %>%
  left_join(unique_fire_years, by = "OBJECTID")

patch_metrics_annual_cont_hs <- patch_metrics_annual_cont %>%
  filter(rdnbr_class %in% c(1,2,3))

pl_cont <- ggplot(patch_metrics_annual_cont_hs, aes(x = fire_year, y = mn_contiguity, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Mean contiguity",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))

######## Perimeter : area ###########################
## average perimeter to area ratio

patch_metrics_annual_pa <- patch_metrics_mega %>%
  group_by(fire_year,rdnbr_class) %>%
  summarise(mn_pa = mean(perimeter_area), sd_pa = sd(perimeter_area))

patch_metrics_annual_pa_hs <- patch_metrics_annual_pa %>%
  filter(rdnbr_class %in% c(1,2,3))


# Create scatter plot

pl_pa <- ggplot(patch_metrics_annual_pa_hs, aes(x = fire_year, y = mn_pa, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Mean perimeter : area",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))

# average perimeter : area ratio by fire year and OBJECTID
patch_metrics_annual_pa <- patch_metrics_mega %>%
  group_by(OBJECTID,rdnbr_class) %>%
  summarise(mn_pa = mean(perimeter_area), sd_pa = sd(perimeter_area))


# Ensure each OBJECTID has only one fire_year in patch_metrics_mega
unique_fire_years <- patch_metrics_mega %>%
  select(OBJECTID, fire_year) %>%
  distinct(OBJECTID, .keep_all = TRUE)  # Keep only one unique row per OBJECTID

# Perform the left join to assign fire_year to patch_metrics_annual_prop_ca
patch_metrics_annual_pa <- patch_metrics_annual_pa %>%
  left_join(unique_fire_years, by = "OBJECTID")

patch_metrics_annual_pa_hs <- patch_metrics_annual_pa %>%
  filter(rdnbr_class %in% c(1,2,3))

pl_pa <- ggplot(patch_metrics_annual_pa_hs, aes(x = fire_year, y = mn_pa, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Mean perimeter : area",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))

######## Proportion core area within fire ###########################

# (1) Sum n of all patches within OBJECTID and rdnbr_class
# (2) Then sum core_area by OBJECTID and rdnbr_class
# Divide (2) by (1) 

patch_metrics_annual_prop_ca <- patch_metrics_mega %>%
  group_by(OBJECTID, rdnbr_class) %>%
  summarise(sum_n = sum(n), sum_core_area = sum(core_area))


patch_metrics_annual_prop_ca$prop_ca <- patch_metrics_annual_prop_ca$sum_core_area / patch_metrics_annual_prop_ca$sum_n

# Ensure each OBJECTID has only one fire_year in patch_metrics_mega
unique_fire_years <- patch_metrics_mega %>%
  select(OBJECTID, fire_year) %>%
  distinct(OBJECTID, .keep_all = TRUE)  # Keep only one unique row per OBJECTID

# Perform the left join to assign fire_year to patch_metrics_annual_prop_ca
patch_metrics_annual_prop_ca <- patch_metrics_annual_prop_ca %>%
  left_join(unique_fire_years, by = "OBJECTID")


# Create scatter plot
ggplot(patch_metrics_annual_prop_ca, aes(x = fire_year, y = prop_ca, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Proportion core area of total burned area",
    color = "RDNBR class"  # Legend title
  ) +
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  )


patch_metrics_annual_prop_ca_hs <- patch_metrics_annual_prop_ca %>%
  filter(rdnbr_class %in% c(1,2,3))




pl_prop_ca <- ggplot(patch_metrics_annual_prop_ca_hs, aes(x = fire_year, y = prop_ca, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Proportion core area of total burned area",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))



# Arrange four plots in a 2-row, 2-column layout
final_plot <- plot_grid(pl_ca, pl_cont, pl_pa, pl_prop_ca, 
                        ncol = 2,   # 2 columns
                        nrow = 2,   # 2 rows
                        align = "hv")  # Align both horizontally & vertically



######## Patch sizes and severities over time ##################################

patch_metrics_mega <- patch_metrics_mega %>%
  mutate(n_bins = cut(n,
                      breaks = c(1, 100, 1000,10000,1500000),
                      labels = c("small","medium","large","very large"),
                      include.lowest = TRUE))



patch_metrics_mega_frequencies <- patch_metrics_mega %>%
  group_by(fire_year, rdnbr_class, n_bins) %>%
  summarise(count_patches = n(),total_area = sum(n))



patch_metrics_mega_frequencies_hs <- patch_metrics_mega_frequencies %>%
  filter(rdnbr_class %in% c(1,2,3))




ggplot(patch_metrics_mega_frequencies_hs, aes(x = fire_year, y = count_patches, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  labs(
    title = "",
    x = "Fire year",
    y = "Number of patches in megafires",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))+
  facet_wrap(~ n_bins, nrow = 2, scales = 'free')  # Facet by n_bins, arranged in 2 rows





# Log-transform y axis and add statistics to number of patches in megafires

library(ggplot2)
library(dplyr)
library(purrr)


# 1️⃣ Compute R² and p-values for both log-transformed and raw values
regression_summary <- patch_metrics_mega_frequencies_hs %>%
  group_by(n_bins, rdnbr_class) %>%
  summarise(
    lm_model_log = list(lm(log10(count_patches + 1) ~ fire_year, data = cur_data())),  # Log-transformed model
    lm_model_raw = list(lm(count_patches ~ fire_year, data = cur_data())),             # Raw (non-log) model
    .groups = "drop"
  ) %>%
  mutate(
    # Extract R² and p-values for log-transformed model
    r_squared_log = map_dbl(lm_model_log, ~ summary(.x)$r.squared),
    p_value_log = map_dbl(lm_model_log, ~ summary(.x)$coefficients[2, 4]),
    
    # Extract R² and p-values for raw model
    r_squared_raw = map_dbl(lm_model_raw, ~ summary(.x)$r.squared),
    p_value_raw = map_dbl(lm_model_raw, ~ summary(.x)$coefficients[2, 4])
  ) %>%
  select(n_bins, rdnbr_class, r_squared_log, p_value_log, r_squared_raw, p_value_raw) %>%
  mutate(
    r_squared_log = round(r_squared_log, 2),
    p_value_log = signif(p_value_log, 2),
    r_squared_raw = round(r_squared_raw, 2),
    p_value_raw = signif(p_value_raw, 2)
  )

# Display the table
regression_summary


# 3️⃣ Plot with log-transformed y-axis
ggplot(patch_metrics_mega_frequencies_hs, aes(x = fire_year, y = count_patches, color = as.factor(rdnbr_class))) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +
  labs(
    title = "",
    x = "Fire year",
    y = "Log(Number of patches in megafires)",
    color = "RDNBR class"
  ) +
  scale_y_log10() +
  facet_wrap(~ n_bins, nrow = 2, scales = 'free') +
  theme_classic() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))



# Area covered by patches of each size in megafires
ggplot(patch_metrics_mega_frequencies_hs, aes(x = fire_year, y = total_area, color = as.factor(rdnbr_class))) +
  #geom_point(size = 3, alpha = 0.7) +  # Adjust point size and transparency
  geom_bar(stat='identity')+
  labs(
    title = "",
    x = "Fire year",
    y = "Total area in patches in megafires",
    color = "RDNBR class"  # Legend title
  ) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, aes(fill = after_scale(alpha(color, 0.3)))) +  # Linear regression + custom fill
  theme_classic() +  # Use a clean theme
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"  # Adjust legend position if needed
  ) +
  scale_color_manual(values = c("0" = "#D9DDDC", "1" = "#F6BE00", "2" = '#21918c', "3" = "#440154"))+
  facet_wrap(~ n_bins, nrow = 2, scales = 'free')  # Facet by n_bins, arranged in 2 rows



library(terra)
library(raster)


rdnbr_rast <- raster("C:/Users/jscho/Downloads/Fire_data_bundles_cTscEYjgVNWGwspibnbg/composite_data/MTBS_BSmosaics/2023/mtbs_CA_2023/mtbs_CA_2023.tif")
str(rdnbr_rast)
nbands(rdnbr_rast)
