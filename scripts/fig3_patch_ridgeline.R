# ============================================================================
# Figure 3 — Patch recovery ridgeline
# ============================================================================
# Density distribution of the proportion of each stand-replacing patch returned
# to conifer 20 yr post-fire, split by patch size category. Ridgeline plots
# show density; overlaid boxplots show median, Q1, Q3 per size class.
#
# Inputs: all_patch_summary (built by scripts/build_all_patch_summary.R)
# Output: Figures/fig3_patch_ridgeline.{pdf,svg}
# ============================================================================

source(here::here("scripts", "00_setup.R"))

# Load all patch data
all_patch_summary <- fread(here("Data", "all_patch_summary.csv"))

# --- Size bins and density per bin ------------------------------------------
patch_data <- all_patch_summary %>%
  mutate(size_bin = cut(area_ha,
                        breaks = c(0, 1, 5, 20, 100, Inf),
                        labels = c("<1 ha", "1-5 ha", "5-20 ha",
                                   "20-100 ha", ">100 ha"))) %>%
  filter(!is.na(size_bin) & !is.na(pct_returned_20))

# Sample sizes per bin — for the caption
n_per_bin <- patch_data %>%
  dplyr::count(size_bin, name = "n_patches")
print(n_per_bin)

# Density on 0-100 grid (constrains ridges to the response range)
ridge_data <- patch_data %>%
  group_by(size_bin) %>%
  group_modify(~ {
    d <- density(.x$pct_returned_20, from = 0, to = 100, bw = 3)
    data.frame(x = d$x, density = d$y)
  }) %>%
  ungroup()

# Boxplot data is just the patch-level values
box_data <- patch_data %>%
  dplyr::select(size_bin, pct_returned_20)

# --- Plot -------------------------------------------------------------------
p_recovery_ridges <- ggplot() +
  geom_ridgeline_gradient(
    data      = ridge_data,
    aes(x = x, y = size_bin, height = density, fill = x),
    scale     = 45, color = "white", linewidth = 0.3
  ) +
  scale_fill_gradient(
    low  = "#D2B48C",   # tan - low recovery
    high = "#0e4f12",   # conifer green - high recovery
    name = "% returned"
  ) +
  geom_boxplot(
    data     = box_data,
    aes(x = pct_returned_20, y = size_bin),
    width    = 0.15,
    outlier.shape = NA,
    fill     = "grey70",
    color    = "grey20",
    linewidth = 0.4,
    position = position_nudge(y = -0.12)
  ) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.8))) +
  labs(x = "% returned to conifer by year 20",
       y = "Patch size class") +
  theme_classic(base_size = 11) +
  theme(
    axis.title           = element_text(),
    legend.position      = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background    = element_rect(fill = alpha("white", 0.7), color = NA),
    legend.direction     = "horizontal",
    plot.caption         = element_text(color = "grey50", size = 9)
  )

print(p_recovery_ridges)

# --- Save -------------------------------------------------------------------
save_fig(p_recovery_ridges, "fig3_patch_ridgeline",
         width = 180, height = 120)
