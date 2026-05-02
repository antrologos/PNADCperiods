# Generate remaining figures for SIDRA vignette
library(data.table)
library(ggplot2)

# Paths
output_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/sidra_mensalization"
figures_dir <- file.path(output_dir, "figures")
vignette_figures_dir <- paste0(
  "D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods/vignettes/figures/",
  "sidra-mensalization"
)

dir.create(vignette_figures_dir, recursive = TRUE, showWarnings = FALSE)

# Load precomputed data
monthly_all <- readRDS(file.path(output_dir, "monthly_all.rds"))
rolling_quarters <- readRDS(file.path(output_dir, "rolling_quarters_all.rds"))

# Compute mesnotrim from anomesexato: month position in quarter (1, 2, or 3)
monthly_all[, mesnotrim := ((anomesexato %% 100 - 1) %% 3) + 1]

# Merge rolling quarter values with monthly data for comparison figures
# Rolling quarters have anomesfinaltrimmovel, monthly has anomesexato
rq_subset <- rolling_quarters[, .(anomesfinaltrimmovel, taxadesocup)]
setnames(rq_subset, "anomesfinaltrimmovel", "anomesexato")
monthly_all <- merge(monthly_all, rq_subset, by = "anomesexato", all.x = TRUE)

theme_vignette <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Figure 3: Mensalization Process
cat("Generating Figure 3...\n")
example_data <- monthly_all[anomesexato >= 201901 & anomesexato <= 201912,
                             .(anomesexato, mesnotrim,
                               rolling = taxadesocup,
                               monthly = m_taxadesocup)]
example_data[, date := as.Date(paste0(substr(anomesexato, 1, 4), "-",
                                       substr(anomesexato, 5, 6), "-01"))]

plot_data <- melt(example_data, id.vars = c("anomesexato", "date", "mesnotrim"),
                  variable.name = "type", value.name = "rate")
plot_data[, type := factor(type,
                           levels = c("rolling", "monthly"),
                           labels = c("Rolling Quarter (SIDRA)",
                                      "Monthly (Mensalized)"))]

fig3 <- ggplot(plot_data, aes(x = date, y = rate, color = type, linetype = type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#2196F3", "#F44336"), name = "") +
  scale_linetype_manual(values = c("dashed", "solid"), name = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Mensalization in Action: Unemployment Rate (2019)",
       subtitle = "Converting smoothed rolling quarters to exact monthly estimates",
       x = "Month", y = "Unemployment Rate") +
  theme_vignette + theme(legend.position = "top")

ggsave(file.path(figures_dir, "fig3_mensalization_process.png"), fig3,
       width = 10, height = 6, dpi = 150, bg = "white")

# Figure 4: Monthly vs Quarterly Comparison
cat("Generating Figure 4...\n")
comparison_data <- monthly_all[anomesexato >= 201901 & anomesexato <= 202312,
                                .(anomesexato,
                                  rolling = taxadesocup,
                                  monthly = m_taxadesocup)]
comparison_data[, date := as.Date(paste0(substr(anomesexato, 1, 4), "-",
                                          substr(anomesexato, 5, 6), "-01"))]

comp_long <- melt(comparison_data, id.vars = c("anomesexato", "date"),
                  variable.name = "type", value.name = "rate")
comp_long[, type := factor(type, levels = c("rolling", "monthly"),
                           labels = c("Rolling Quarter", "Monthly"))]

fig4 <- ggplot(comp_long, aes(x = date, y = rate, color = type)) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-12-31"),
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.1) +
  annotate("text", x = as.Date("2020-07-01"), y = 16, label = "COVID-19",
           color = "red", fontface = "italic", size = 3) +
  scale_color_manual(values = c("Rolling Quarter" = "#666666",
                                "Monthly" = "#E53935"), name = "") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Unemployment Rate: Monthly vs Rolling Quarter (2019-2023)",
       subtitle = "Monthly estimates reveal dynamics hidden in smoothed quarterly data",
       x = NULL, y = "Unemployment Rate") +
  theme_vignette

ggsave(file.path(figures_dir, "fig4_monthly_vs_quarterly.png"), fig4,
       width = 12, height = 6, dpi = 150, bg = "white")

# Figure 5: COVID Case Study
cat("Generating Figure 5...\n")
covid_data <- monthly_all[anomesexato >= 201901 & anomesexato <= 202206,
                           .(anomesexato, monthly = m_taxadesocup)]
covid_data[, date := as.Date(paste0(substr(anomesexato, 1, 4), "-",
                                     substr(anomesexato, 5, 6), "-01"))]

peak_row <- covid_data[which.max(monthly)]

fig5 <- ggplot(covid_data, aes(x = date, y = monthly)) +
  geom_line(color = "#1976D2", linewidth = 1.2) +
  geom_point(color = "#1976D2", size = 2) +
  geom_vline(xintercept = as.Date("2020-03-15"), linetype = "dashed",
             color = "red", alpha = 0.7) +
  annotate("text", x = as.Date("2020-03-15"), y = 8, label = "Pandemic\nDeclared",
           hjust = 1.1, size = 3, color = "red") +
  geom_point(data = peak_row, aes(x = date, y = monthly),
             color = "red", size = 4) +
  annotate("text", x = peak_row$date, y = peak_row$monthly + 0.8,
           label = paste0("Peak: ", round(peak_row$monthly, 1), "%\n(",
                          format(peak_row$date, "%b %Y"), ")"),
           size = 3, fontface = "bold") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(7, 16)) +
  labs(title = "COVID-19 Impact on Brazilian Unemployment",
       subtitle = "Monthly estimates reveal the exact timing and magnitude of labor market shock",
       x = NULL, y = "Unemployment Rate") +
  theme_vignette

ggsave(file.path(figures_dir, "fig5_covid_case_study.png"), fig5,
       width = 12, height = 6, dpi = 150, bg = "white")

# Copy all figures to vignette directory
cat("\nCopying figures to vignette directory...\n")
figure_files <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
for (f in figure_files) {
  file.copy(f, vignette_figures_dir, overwrite = TRUE)
  cat("  Copied:", basename(f), "\n")
}

# Generate text outputs
cat("\nGenerating text outputs...\n")
quick_start_output <- capture.output({
  cat("Dimensions:", nrow(monthly_all), "rows x", ncol(monthly_all), "columns\n")
  cat("Date range:", min(monthly_all$anomesexato), "to",
      max(monthly_all$anomesexato), "\n")
  cat("\nFirst 6 rows of key columns:\n")
  print(head(monthly_all[, .(anomesexato, mesnotrim, m_popocup, m_taxadesocup)]))
})
writeLines(quick_start_output, file.path(output_dir, "quick_start_output.txt"))

cat("\nDone! All figures generated.\n")
