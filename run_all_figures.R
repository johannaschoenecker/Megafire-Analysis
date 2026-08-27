# ============================================================================
# run_all_figures.R — source every figure script in order
# ============================================================================
# Assumes the upstream pipeline (scripts/01_build_pixel_data.R ...
# scripts/06_build_fire_table.R) has already been run at least once, or that
# the cached intermediates in Data/figure_cache/ and Data/ are present.
# Run from the project root (opened via the .Rproj file).
# ============================================================================

fig_scripts <- c(
  "fig1_overview_map.R",
  "fig2_pixel_recovery.R",
  "fig3_patch_ridgeline.R",
  "fig4_dominant_class_panels.R",
  "fig4_panelB_stats.R",
  "fig5_ale_importance.R",
  "fig6_recovery_mirror.R",
  "supp_landscape_metrics.R",
  "supp_ale_interaction.R",
  "supp_variable_importance.R"
)

for (f in fig_scripts) {
  message("=== Running scripts/", f, " ===")
  source(here::here("scripts", f))
}
