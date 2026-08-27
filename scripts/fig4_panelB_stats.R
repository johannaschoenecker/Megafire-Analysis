# ============================================================================
# Significance testing for Figure 4, Panel B
# ============================================================================
# Panel B (scripts/fig4_dominant_class_panels.R) shows patch size (area_ha,
# log scale) grouped by dominant vegetation class at year 20 (dominant_class_20).
#
# area_ha is heavily right-skewed and groups have unequal n and unequal
# spread (hence the log-scale boxplot) -> use a non-parametric omnibus test
# (Kruskal-Wallis) followed by Dunn's post-hoc pairwise test with a
# Benjamini-Hochberg correction for multiple comparisons.
# ============================================================================

source(here::here("scripts", "00_setup.R"))
pacman::p_load(rstatix)

all_patch_summary <- fread(here("Data", "all_patch_summary.csv"))

dat <- all_patch_summary %>%
  filter(!is.na(dominant_class_20)) %>%
  mutate(dominant_class_20 = factor(dominant_class_20,
                                     levels = names(veg_labels),
                                     labels = veg_labels))

# --- Omnibus test: do patch sizes differ across dominant classes? -----------
kw <- dat %>% kruskal_test(area_ha ~ dominant_class_20)
print(kw)

kw_effect <- dat %>% kruskal_effsize(area_ha ~ dominant_class_20)
print(kw_effect)

# --- Post-hoc pairwise comparisons (Dunn's test, BH-adjusted) ---------------
dunn <- dat %>%
  dunn_test(area_ha ~ dominant_class_20, p.adjust.method = "BH") %>%
  arrange(p.adj)

print(dunn, n = Inf)

# Save results for the manuscript / supplement
out_dir <- here("Data", "figure_cache")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fwrite(dunn, file.path(out_dir, "fig4_panelB_dunn_test.csv"))

# --- Compact summary: which pairs are significant at alpha = 0.05 -----------
sig_pairs <- dunn %>% filter(p.adj < 0.05)
cat("\n", nrow(sig_pairs), "of", nrow(dunn),
    "pairwise comparisons significant at BH-adjusted p < 0.05\n")
print(sig_pairs %>% select(group1, group2, n1, n2, p, p.adj, p.adj.signif))
