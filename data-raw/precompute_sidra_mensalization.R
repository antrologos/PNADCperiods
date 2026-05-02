# =============================================================================
# Precompute Script for SIDRA Mensalization Vignette
# =============================================================================
# This script generates all data and figures needed for the vignette.
# Run this before building the vignette.
#
# Outputs:
#   - output/vignette/sidra_mensalization/*.rds (cached data)
#   - output/vignette/sidra_mensalization/figures/*.png (figures)
#   - PNADCperiods/vignettes/figures/sidra-mensalization/*.png (copied figures)
#
# Author: PNADCperiods package
# =============================================================================

library(data.table)
library(ggplot2)

# Set working directory to package root
setwd("D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods")
devtools::load_all()

# Create output directories
output_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/sidra_mensalization"
figures_dir <- file.path(output_dir, "figures")
vignette_figures_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods/vignettes/figures/sidra-mensalization"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(vignette_figures_dir, recursive = TRUE, showWarnings = FALSE)

cat("=============================================================================\n")
cat("PRECOMPUTING DATA AND FIGURES FOR SIDRA MENSALIZATION VIGNETTE\n")
cat("=============================================================================\n\n")

# =============================================================================
# SECTION 1: Fetch and Cache SIDRA Data
# =============================================================================

cat("Step 1: Fetching SIDRA rolling quarters...\n")

# Fetch all series (for complete catalog)
rq_all <- fetch_sidra_rolling_quarters(verbose = TRUE, use_cache = TRUE)
saveRDS(rq_all, file.path(output_dir, "rolling_quarters_all.rds"))
cat("  Saved: rolling_quarters_all.rds\n")

# Fetch specific theme categories for examples
cat("\nStep 2: Fetching by theme category...\n")
rq_employment <- fetch_sidra_rolling_quarters(theme_category = "employment_type", verbose = FALSE, use_cache = TRUE)
rq_wages <- fetch_sidra_rolling_quarters(theme_category = "wage_mass", verbose = FALSE, use_cache = TRUE)
saveRDS(rq_employment, file.path(output_dir, "rolling_quarters_employment.rds"))
saveRDS(rq_wages, file.path(output_dir, "rolling_quarters_wages.rds"))
cat("  Saved: rolling_quarters_employment.rds, rolling_quarters_wages.rds\n")

# =============================================================================
# SECTION 2: Mensalize Series
# =============================================================================

cat("\nStep 3: Mensalizing all series...\n")

monthly_all <- mensalize_sidra_series(
  rolling_quarters = rq_all,
  starting_points = pnadc_series_starting_points,
  compute_derived = TRUE,
  verbose = TRUE
)
saveRDS(monthly_all, file.path(output_dir, "monthly_all.rds"))
cat("  Saved: monthly_all.rds\n")

# =============================================================================
# SECTION 3: Get Series Metadata
# =============================================================================

cat("\nStep 4: Extracting series metadata...\n")

metadata <- get_sidra_series_metadata()
saveRDS(metadata, file.path(output_dir, "series_metadata.rds"))
cat("  Saved: series_metadata.rds (", nrow(metadata), " series)\n")

# Create theme category summary table
category_summary <- metadata[, .N, by = theme_category][order(-N)]
saveRDS(category_summary, file.path(output_dir, "category_summary.rds"))

# =============================================================================
# SECTION 4: Generate Figures
# =============================================================================

cat("\nStep 5: Generating figures...\n")

# Set common theme
theme_vignette <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# -----------------------------------------------------------------------------
# Figure 1: Rolling Quarter Schematic
# -----------------------------------------------------------------------------
cat("  Generating Figure 1: Rolling Quarter Schematic...\n")

# Create data for schematic
rolling_schematic <- data.table(
  quarter = rep(c("Q1 2024", "Q2 2024", "Q3 2024"), each = 3),
  month = c("Jan", "Feb", "Mar", "Feb", "Mar", "Apr", "Mar", "Apr", "May"),
  month_num = c(1, 2, 3, 2, 3, 4, 3, 4, 5),
  position = rep(1:3, 3)
)

# Color by reference month
rolling_schematic[, ref_month := factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May"))]

fig1 <- ggplot(rolling_schematic, aes(x = quarter, y = position, fill = ref_month)) +
  geom_tile(color = "white", linewidth = 1.5, width = 0.8, height = 0.8) +
  geom_text(aes(label = month), color = "white", fontface = "bold", size = 4) +
  scale_fill_viridis_d(option = "C", name = "Reference Month") +
  scale_y_continuous(breaks = 1:3, labels = c("Month 1", "Month 2", "Month 3")) +
  labs(
    title = "Rolling Quarter Structure",
    subtitle = "Each 'quarter' contains interviews from three consecutive reference months",
    x = "Published Quarter",
    y = "Position in Quarter"
  ) +
  theme_vignette +
  theme(axis.text.y = element_text(face = "bold"))

ggsave(file.path(figures_dir, "fig1_rolling_schematic.png"), fig1,
       width = 8, height = 5, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# Figure 2: Data Flow Diagram
# -----------------------------------------------------------------------------
cat("  Generating Figure 2: Data Flow Diagram...\n")

# Create simple flow diagram data
flow_data <- data.table(
  step = 1:4,
  label = c(
    "SIDRA API\n(Rolling Quarters)",
    "fetch_sidra_\nrolling_quarters()",
    "mensalize_\nsidra_series()",
    "Monthly\nEstimates"
  ),
  x = c(1, 2, 3, 4),
  y = c(1, 1, 1, 1),
  type = c("data", "function", "function", "data")
)

fig2 <- ggplot(flow_data, aes(x = x, y = y)) +
  # Arrows
  geom_segment(aes(x = 1.3, xend = 1.7, y = 1, yend = 1),
               arrow = arrow(length = unit(0.3, "cm")), color = "gray50", linewidth = 1) +
  geom_segment(aes(x = 2.3, xend = 2.7, y = 1, yend = 1),
               arrow = arrow(length = unit(0.3, "cm")), color = "gray50", linewidth = 1) +
  geom_segment(aes(x = 3.3, xend = 3.7, y = 1, yend = 1),
               arrow = arrow(length = unit(0.3, "cm")), color = "gray50", linewidth = 1) +
  # Boxes
 geom_label(aes(label = label, fill = type),
             size = 3.5, fontface = "bold", label.padding = unit(0.5, "lines")) +
  scale_fill_manual(values = c("data" = "#E8F4F8", "function" = "#FFF3E0"),
                    guide = "none") +
  scale_x_continuous(limits = c(0.5, 4.5)) +
  scale_y_continuous(limits = c(0.5, 1.5)) +
  labs(
    title = "SIDRA Mensalization Data Flow",
    subtitle = "Three-step workflow: Fetch \u2192 Transform \u2192 Use"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(file.path(figures_dir, "fig2_data_flow.png"), fig2,
       width = 10, height = 4, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# Figure 3: Mensalization Process Visualization
# -----------------------------------------------------------------------------
cat("  Generating Figure 3: Mensalization Process...\n")

# Join rolling quarter data with mensalized data for comparison
# rq_all has: anomesfinaltrimmovel, mesnotrim, taxadesocup (original rolling quarter value)
# monthly_all has: anomesexato, m_taxadesocup (mensalized monthly value)
example_rq <- rq_all[anomesfinaltrimmovel >= 201901 & anomesfinaltrimmovel <= 201912,
                      .(anomesexato = anomesfinaltrimmovel, rolling = taxadesocup)]
example_monthly <- monthly_all[anomesexato >= 201901 & anomesexato <= 201912,
                                .(anomesexato, monthly = m_taxadesocup)]
example_data <- merge(example_rq, example_monthly, by = "anomesexato")

# Add proper date column
example_data[, date := as.Date(paste0(substr(anomesexato, 1, 4), "-",
                                       substr(anomesexato, 5, 6), "-01"))]

# Reshape for plotting
plot_data <- melt(example_data, id.vars = c("anomesexato", "date"),
                  variable.name = "type", value.name = "rate")
plot_data[, type := factor(type,
                           levels = c("rolling", "monthly"),
                           labels = c("Rolling Quarter (SIDRA)", "Monthly (Mensalized)"))]

fig3 <- ggplot(plot_data, aes(x = date, y = rate, color = type, linetype = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#2196F3", "#F44336"), name = "") +
  scale_linetype_manual(values = c("dashed", "solid"), name = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Mensalization in Action: Unemployment Rate (2019)",
    subtitle = "Converting smoothed rolling quarters to exact monthly estimates",
    x = "Month",
    y = "Unemployment Rate"
  ) +
  theme_vignette +
  theme(legend.position = "top")

ggsave(file.path(figures_dir, "fig3_mensalization_process.png"), fig3,
       width = 10, height = 6, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# Figure 4: Monthly vs Quarterly Unemployment Comparison
# -----------------------------------------------------------------------------
cat("  Generating Figure 4: Monthly vs Quarterly Comparison...\n")

# Join rolling quarter data with mensalized data for 2019-2023
comp_rq <- rq_all[anomesfinaltrimmovel >= 201901 & anomesfinaltrimmovel <= 202312,
                   .(anomesexato = anomesfinaltrimmovel, rolling = taxadesocup)]
comp_monthly <- monthly_all[anomesexato >= 201901 & anomesexato <= 202312,
                             .(anomesexato, monthly = m_taxadesocup)]
comparison_data <- merge(comp_rq, comp_monthly, by = "anomesexato")

comparison_data[, date := as.Date(paste0(substr(anomesexato, 1, 4), "-",
                                          substr(anomesexato, 5, 6), "-01"))]

# Reshape
comp_long <- melt(comparison_data, id.vars = c("anomesexato", "date"),
                  variable.name = "type", value.name = "rate")
comp_long[, type := factor(type,
                           levels = c("rolling", "monthly"),
                           labels = c("Rolling Quarter", "Monthly"))]

fig4 <- ggplot(comp_long, aes(x = date, y = rate, color = type)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  # Highlight COVID period
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-12-31"),
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.1) +
  annotate("text", x = as.Date("2020-07-01"), y = 16,
           label = "COVID-19", color = "red", fontface = "italic", size = 3) +
  scale_color_manual(values = c("Rolling Quarter" = "#666666", "Monthly" = "#E53935"),
                     name = "") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Unemployment Rate: Monthly vs Rolling Quarter (2019-2023)",
    subtitle = "Monthly estimates reveal dynamics hidden in smoothed quarterly data",
    x = NULL,
    y = "Unemployment Rate"
  ) +
  theme_vignette

ggsave(file.path(figures_dir, "fig4_monthly_vs_quarterly.png"), fig4,
       width = 12, height = 6, dpi = 150, bg = "white")

# -----------------------------------------------------------------------------
# Figure 5: COVID Case Study - Annotated Timeline
# -----------------------------------------------------------------------------
cat("  Generating Figure 5: COVID Case Study...\n")

# COVID period focus (monthly data only)
covid_data <- monthly_all[anomesexato >= 201901 & anomesexato <= 202206,
                           .(anomesexato, monthly = m_taxadesocup)]

covid_data[, date := as.Date(paste0(substr(anomesexato, 1, 4), "-",
                                     substr(anomesexato, 5, 6), "-01"))]

# Find key points
peak_row <- covid_data[which.max(monthly)]
pre_covid <- covid_data[anomesexato == 202002]
trough <- covid_data[anomesexato == 202112]

fig5 <- ggplot(covid_data, aes(x = date, y = monthly)) +
  geom_line(color = "#1976D2", linewidth = 1.2) +
  geom_point(color = "#1976D2", size = 2) +
  # Annotate key events
  geom_vline(xintercept = as.Date("2020-03-15"), linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = as.Date("2020-03-15"), y = 8,
           label = "Pandemic\nDeclared", hjust = 1.1, size = 3, color = "red") +
  # Peak annotation
  geom_point(data = peak_row, aes(x = date, y = monthly),
             color = "red", size = 4) +
  annotate("text", x = peak_row$date, y = peak_row$monthly + 0.8,
           label = paste0("Peak: ", round(peak_row$monthly, 1), "%\n(",
                          format(peak_row$date, "%b %Y"), ")"),
           size = 3, fontface = "bold") +
  # Recovery annotation
  geom_point(data = trough, aes(x = date, y = monthly),
             color = "darkgreen", size = 4) +
  annotate("text", x = trough$date, y = trough$monthly - 0.8,
           label = paste0("Recovery: ", round(trough$monthly, 1), "%"),
           size = 3, color = "darkgreen") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(7, 16)) +
  labs(
    title = "COVID-19 Impact on Brazilian Unemployment",
    subtitle = "Monthly estimates reveal the exact timing and magnitude of labor market shock",
    x = NULL,
    y = "Unemployment Rate"
  ) +
  theme_vignette

ggsave(file.path(figures_dir, "fig5_covid_case_study.png"), fig5,
       width = 12, height = 6, dpi = 150, bg = "white")

# =============================================================================
# SECTION 5: Copy Figures to Vignette Directory
# =============================================================================

cat("\nStep 6: Copying figures to vignette directory...\n")

figure_files <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
for (f in figure_files) {
  file.copy(f, vignette_figures_dir, overwrite = TRUE)
  cat("  Copied:", basename(f), "\n")
}

# =============================================================================
# SECTION 6: Generate Text Outputs for Vignette
# =============================================================================

cat("\nStep 7: Generating text outputs...\n")

# Quick start example output
quick_start_output <- capture.output({
  cat("# Mensalized data structure\n")
  cat("Dimensions:", nrow(monthly_all), "rows x", ncol(monthly_all), "columns\n")
  cat("\nDate range:", min(monthly_all$anomesexato), "to", max(monthly_all$anomesexato), "\n")
  cat("\nFirst 6 rows of key columns:\n")
  print(head(monthly_all[, .(anomesexato, m_popocup, m_taxadesocup)]))
})
writeLines(quick_start_output, file.path(output_dir, "quick_start_output.txt"))

# Metadata summary by theme_category
metadata_output <- capture.output({
  cat("# Available series by theme category\n\n")
  for (tc in category_summary$theme_category) {
    n <- category_summary[theme_category == tc, N]
    cat(sprintf("%-25s: %d series\n", tc, n))
  }
  cat("\nTotal:", sum(category_summary$N), "series\n")
})
writeLines(metadata_output, file.path(output_dir, "metadata_summary.txt"))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("PRECOMPUTATION COMPLETE\n")
cat("=============================================================================\n\n")

cat("Generated files:\n")
cat("  Data:\n")
for (f in list.files(output_dir, pattern = "\\.rds$")) {
  cat("    -", f, "\n")
}
cat("\n  Text outputs:\n")
for (f in list.files(output_dir, pattern = "\\.txt$")) {
  cat("    -", f, "\n")
}
cat("\n  Figures:\n")
for (f in list.files(figures_dir, pattern = "\\.png$")) {
  cat("    -", f, "\n")
}

cat("\nFigures copied to:", vignette_figures_dir, "\n")
cat("\nReady to build vignette!\n")
