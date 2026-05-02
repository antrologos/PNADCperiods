# Regenerate the workflow diagram with updated text
# Run this script to create the updated fig-workflow-diagram.png

library(ggplot2)

# Paths
project_dir <- "d:/Dropbox/Artigos/mensalizacao_pnad"
fig_output_dir <- file.path(project_dir, "output/vignette/figures/annual-poverty-analysis")
pkg_fig_dir <- file.path(project_dir, "PNADCperiods/vignettes/figures/annual-poverty-analysis")

# Create output directories if needed
dir.create(fig_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pkg_fig_dir, recursive = TRUE, showWarnings = FALSE)

# Create workflow diagram
workflow_plot <- ggplot() +
  annotate("rect", xmin = 0, xmax = 3, ymin = 2.5, ymax = 3.5,
           fill = "#e0e0e0", color = "black") +
  annotate("text", x = 1.5, y = 3, label = "Quarterly PNADC\n(2015-2024)",
           size = 3.5, fontface = "bold") +

  annotate("rect", xmin = 4, xmax = 7, ymin = 2.5, ymax = 3.5,
           fill = "#bbdefb", color = "black") +
  annotate("text", x = 5.5, y = 3, label = "pnadc_identify_periods()\nCrosswalk",
           size = 3.5, fontface = "bold") +

  annotate("rect", xmin = 0, xmax = 3, ymin = 0.5, ymax = 1.5,
           fill = "#e0e0e0", color = "black") +
  annotate("text", x = 1.5, y = 1, label = "Annual PNADC\n(Income data)",
           size = 3.5, fontface = "bold") +

  annotate("rect", xmin = 4, xmax = 7, ymin = 0.5, ymax = 1.5,
           fill = "#c8e6c9", color = "black") +
  annotate("text", x = 5.5, y = 1, label = "pnadc_apply_periods()\n(merge + calibrate)",
           size = 3.5, fontface = "bold") +

  annotate("rect", xmin = 8, xmax = 11, ymin = 1.5, ymax = 2.5,
           fill = "#ffcdd2", color = "black") +
  annotate("text", x = 9.5, y = 2, label = "Monthly Poverty\nAnalysis",
           size = 3.5, fontface = "bold") +

  # Arrows
  annotate("segment", x = 3, xend = 4, y = 3, yend = 3,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 5.5, xend = 5.5, y = 2.5, yend = 1.5,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 3, xend = 4, y = 1, yend = 1,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 7, xend = 8, y = 1.5, yend = 1.8,
           arrow = arrow(length = unit(0.2, "cm"))) +

  xlim(-0.5, 11.5) + ylim(0, 4) +
  theme_void() +
  labs(title = "Data Workflow: Monthly Poverty Analysis with Annual PNADC") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 12))

# Save to output directory
ggsave(file.path(fig_output_dir, "fig-workflow-diagram.png"),
       plot = workflow_plot, width = 8, height = 4,
       dpi = 300, bg = "white")

# Copy to package vignettes folder
file.copy(file.path(fig_output_dir, "fig-workflow-diagram.png"),
          file.path(pkg_fig_dir, "fig-workflow-diagram.png"),
          overwrite = TRUE)

message("Workflow diagram regenerated successfully!")
message(sprintf("  Output: %s", file.path(fig_output_dir, "fig-workflow-diagram.png")))
message(sprintf("  Copied to: %s", file.path(pkg_fig_dir, "fig-workflow-diagram.png")))
