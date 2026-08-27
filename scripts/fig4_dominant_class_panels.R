# ============================================================================
# Figure 4 — Dominant vegetation class at 20 years post-fire
# ============================================================================
# a) Frequency of stand-replacing patch dominant vegetation class 20 yr post-fire
# b) Patch sizes by dominant vegetation class 20 yr post-fire
# c) Proportion of patches by dominant vegetation type, across size classes
#
# Inputs: all_patch_summary (built by scripts/build_all_patch_summary.R)
# Output: Figures/fig4_dominant_class_panels.{pdf,svg}
# ============================================================================

source(here::here("scripts", "00_setup.R"))
pacman::p_load(rstatix, multcompView)
all_patch_summary <- fread(here("Data", "all_patch_summary.csv"))

# --- Shared factor levels so all panels use identical fills -----------------
all_levels_full <- sort(unique(
  all_patch_summary$dominant_class_20[!is.na(all_patch_summary$dominant_class_20)]
))

panel_theme <- theme_classic(base_size = 12) +
  theme(
    axis.title   = element_text(),
    plot.caption = element_text(color = "grey50", size = 9),
    plot.tag     = element_text()
  )

# --- Panel A: composition of dominant class ---------------------------------
pA <- all_patch_summary %>%
  filter(!is.na(dominant_class_20)) %>%
  mutate(dominant_class_20 = factor(dominant_class_20, levels = all_levels_full)) %>%
  mutate(dominant_class_20 = forcats::fct_infreq(dominant_class_20)) %>%
  ggplot(aes(x = dominant_class_20, fill = dominant_class_20)) +
  geom_bar(alpha = 0.85) +
  scale_fill_manual(values = veg_colors, labels = veg_labels,
                    breaks = names(veg_labels),
                    name = "Vegetation class", drop = FALSE) +
  labs(x = "Dominant class at year 20", y = "Number of patches") +
  panel_theme +
  theme(legend.position = "none",
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

# ---------------------------------------------------------------------------
# Statistical test for Panel B: does patch size differ by dominant class?
# ---------------------------------------------------------------------------
# area_ha is heavily right-skewed with unequal group sizes and variances
# (this is exactly why panel B is plotted on a log y-axis), so group
# differences can't be assessed with an ANOVA/t-test. Instead we use:
#   1) a Kruskal-Wallis test (non-parametric one-way ANOVA analogue) as the
#      omnibus test across all dominant classes, and
#   2) Dunn's test for pairwise post-hoc comparisons, with a
#      Benjamini-Hochberg correction across all pairwise tests.
#
# Result (n = 8531 patches across 8 dominant classes; Water never dominates):
#   Kruskal-Wallis: chi-sq(7) = 98.8, p = 1.9e-18 -> patch size differs
#   significantly across dominant classes overall, but the effect size is
#   small (eta2 = 0.011), i.e. dominant class explains only ~1% of the
#   variance in patch size - significance here is driven by the large n.
#   Post-hoc, only 4 of the 28 pairwise comparisons remain significant after
#   BH correction (p.adj < 0.05): Shrub-Sagebrush, Shrub-Conifer,
#   Sagebrush-Dense woodland, and Conifer-Dense woodland. All other pairs
#   (e.g. Conifer vs. Sagebrush, Shrub vs. Bare soil/Herbaceous/Open
#   woodland/Bare rock) are not distinguishable at this significance level.
#
# With 8 groups / 28 pairs, drawing every pairwise bracket would be
# unreadable, so results are summarised as a compact letter display (CLD):
# classes that share a letter above their box are NOT significantly
# different from each other.
# Full pairwise table: scripts/fig4_panelB_stats.R and
#   Data/figure_cache/fig4_panelB_dunn_test.csv
b_dat <- all_patch_summary %>%
  filter(!is.na(dominant_class_20)) %>%
  mutate(dominant_class_20 = factor(dominant_class_20, levels = all_levels_full))

# Same size-based ordering used for the boxplot x-axis, computed once so the
# CLD labels line up with the boxes they annotate
b_order <- b_dat %>%
  group_by(dominant_class_20) %>%
  summarise(med = median(area_ha), .groups = "drop") %>%
  arrange(desc(med)) %>%
  pull(dominant_class_20)

dunn_b <- b_dat %>%
  dunn_test(area_ha ~ dominant_class_20, p.adjust.method = "BH")

cld_b <- multcompLetters(
  setNames(dunn_b$p.adj, paste(dunn_b$group1, dunn_b$group2, sep = "-"))
)$Letters

cld_b_df <- b_dat %>%
  group_by(dominant_class_20) %>%
  summarise(y_pos = max(area_ha) * 1.4, .groups = "drop") %>%
  mutate(letter = cld_b[as.character(dominant_class_20)],
         dominant_class_20 = factor(dominant_class_20, levels = b_order))

# --- Panel B: patch sizes by dominant class ---------------------------------
pB <- b_dat %>%
  mutate(dominant_class_20 = factor(dominant_class_20, levels = b_order)) %>%
  ggplot(aes(x = dominant_class_20, y = area_ha, fill = dominant_class_20)) +
  geom_boxplot(alpha = 0.85, outlier.size = 0.6, key_glyph = "rect") +
  geom_text(data = cld_b_df,
            aes(x = dominant_class_20, y = y_pos, label = letter),
            inherit.aes = FALSE, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = veg_colors, labels = veg_labels,
                    name = "Vegetation class", drop = FALSE) +
  scale_y_log10(labels = scales::comma) +
  labs(x = "Dominant class at year 20", y = "Patch size (ha, log scale)") +
  panel_theme +
  theme(legend.position = "bottom",
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank()) +
  guides(fill = guide_legend())

# --- Panel C: composition across size classes -------------------------------
pC <- all_patch_summary %>%
  filter(!is.na(dominant_class_20)) %>%
  mutate(
    size_bin = cut(area_ha,
                   breaks = c(0, 1, 5, 20, 100, Inf),
                   labels = c("<1", "1-5", "5-20", "20-100", ">100")),
    dominant_class_20 = factor(dominant_class_20, levels = all_levels_full)
  ) %>%
  group_by(size_bin, dominant_class_20) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(size_bin) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = size_bin, y = prop, fill = dominant_class_20)) +
  geom_col(alpha = 0.85) +
  scale_fill_manual(values = veg_colors, labels = veg_labels,
                    breaks = names(veg_labels),
                    name = "Vegetation class", drop = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Patch size class (ha)", y = "Proportion of patches") +
  panel_theme +
  theme(legend.position = "none")

# --- Combine and save -------------------------------------------------------
p_dominant_panels <- pA + pB + pC +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13))

print(p_dominant_panels)

save_fig(p_dominant_panels, "fig4_dominant_class_panels",
         width = 340, height = 120)

# ---------------------------------------------------------------------------
# Alternative 2-panel version (a + c only)
# ---------------------------------------------------------------------------
# The patch-size difference in panel b is statistically significant only
# because of the very large n (eta2 = 0.011, "small" effect; see stats
# above) and just 4 of 28 pairwise comparisons survive correction - not a
# strong enough result to build a panel around. This version drops panel b
# and keeps the frequency (a) and size-class composition (c) panels.
#
# pA and pC's fill scales now share explicit `breaks = names(veg_labels)`, so
# patchwork treats them as the same legend and collects them into one,
# which plot_layout() centers under the full width of both panels rather
# than being confined under a single panel's column.
p_dominant_panels_ac <- (pA + pC) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13),
        legend.position = "bottom") &
  guides(fill = guide_legend(nrow = 2))

print(p_dominant_panels_ac)

save_fig(p_dominant_panels_ac, "fig4_dominant_class_panels_ac",
         width = 230, height = 130)
