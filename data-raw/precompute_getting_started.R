# =============================================================================
# Pre-compute Getting Started Vignette Outputs
# =============================================================================
# This script generates all data and figures needed for the getting-started
# vignette. It covers both workflows:
#   - Part A: Microdata mensalization (crosswalk + console output)
#   - Part B: SIDRA mensalization (fetch + mensalize + figure)
#
# Run this script to regenerate all vignette outputs.
#
# Outputs:
#   - output/vignette/getting_started_console_output.txt
#   - output/vignette/getting_started_crosswalk.rds
#   - output/vignette/getting_started_crosswalk_sample.rds
#   - output/vignette/getting_started_stats.rds
#   - output/vignette/getting_started_rolling_quarters.rds
#   - output/vignette/getting_started_monthly.rds
#   - PNADCperiods/vignettes/figures/getting-started/fig_monthly_vs_quarterly.png
# =============================================================================

library(data.table)
library(fst)
library(ggplot2)

# Load the PNADCperiods package
devtools::load_all("PNADCperiods")

# =============================================================================
# PATHS
# =============================================================================

data_dir <- "D:/Dropbox/Bancos_Dados/PNADC/Trimestral/Dados/"
output_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# STEP 0: CLEAR EXISTING OUTPUTS
# =============================================================================

cat("=== Clearing existing outputs ===\n\n")

existing_files <- list.files(output_dir, pattern = "getting_started", full.names = TRUE)
if (length(existing_files) > 0) {
  file.remove(existing_files)
  cat("Removed", length(existing_files), "existing files\n\n")
} else {
  cat("No existing files to remove\n\n")
}

# =============================================================================
# STEP 1: LOAD DATA WITH MINIMAL COLUMNS
# =============================================================================

cat("=== Loading PNADC data with minimal columns ===\n\n")

# List all quarterly files (2012-2025)
files <- list.files(data_dir, pattern = "pnadc_20.*\\.fst$", full.names = TRUE)
cat("Found", length(files), "quarterly files\n")

# ONLY load the columns required for period identification
cols_identification <- c(
  "Ano", "Trimestre", "UPA", "V1008", "V1014",
  "V2008", "V20081", "V20082", "V2009"
)

cat("Loading only required columns:",
    paste(cols_identification, collapse = ", "), "\n\n")

pnadc <- rbindlist(lapply(files, function(f) {
  cat("  Loading:", basename(f), "\n")
  read_fst(f, columns = cols_identification, as.data.table = TRUE)
}))

n_obs <- nrow(pnadc)
n_quarters <- uniqueN(pnadc[, .(Ano, Trimestre)])
year_min <- min(pnadc$Ano)
year_max <- max(pnadc$Ano)

cat("\nTotal observations:", format(n_obs, big.mark = ","), "\n")
cat("Unique quarters:", n_quarters, "\n")
cat("Period:", year_min, "to", year_max, "\n\n")

# =============================================================================
# STEP 2: RUN pnadc_identify_periods() AND CAPTURE OUTPUT
# =============================================================================

cat("=== Running pnadc_identify_periods() ===\n\n")

# Capture console output using sink() - works reliably on Windows
output_file <- file.path(output_dir, "getting_started_console_output.txt")
sink(output_file, split = TRUE)

crosswalk <- pnadc_identify_periods(pnadc, verbose = TRUE, store_date_bounds = TRUE)

sink()

cat("\nConsole output saved to:", output_file, "\n")

# =============================================================================
# STEP 3: EXTRACT STATISTICS FROM CROSSWALK
# =============================================================================

cat("\n=== Summary Statistics ===\n\n")

# Calculate determination rates directly from crosswalk columns
n_households <- nrow(crosswalk)
n_month_determined <- sum(crosswalk$determined_month, na.rm = TRUE)
n_fortnight_determined <- sum(crosswalk$determined_fortnight, na.rm = TRUE)
n_week_determined <- sum(crosswalk$determined_week, na.rm = TRUE)

rate_month <- n_month_determined / n_households
rate_fortnight <- n_fortnight_determined / n_households
rate_week <- n_week_determined / n_households

cat("Determination rates (household-quarter level):\n")
cat(sprintf("  Month:     %.2f%% (%s of %s)\n",
            rate_month * 100,
            format(n_month_determined, big.mark = ","),
            format(n_households, big.mark = ",")))
cat(sprintf("  Fortnight: %.2f%% (%s of %s)\n",
            rate_fortnight * 100,
            format(n_fortnight_determined, big.mark = ","),
            format(n_households, big.mark = ",")))
cat(sprintf("  Week:      %.2f%% (%s of %s)\n",
            rate_week * 100,
            format(n_week_determined, big.mark = ","),
            format(n_households, big.mark = ",")))

# Crosswalk structure
cat("\nCrosswalk structure:\n")
cat("  Rows:", format(nrow(crosswalk), big.mark = ","), "\n")
cat("  Columns:", ncol(crosswalk), "\n")
cat("  Key columns:", paste(key(crosswalk), collapse = ", "), "\n")

# =============================================================================
# STEP 4: SAVE CROSSWALK FOR VIGNETTE USE
# =============================================================================

cat("\n=== Saving crosswalk ===\n")

crosswalk_file <- file.path(output_dir, "getting_started_crosswalk.rds")
saveRDS(crosswalk, crosswalk_file)
cat("Crosswalk saved to:", crosswalk_file, "\n")
cat("  File size:", round(file.size(crosswalk_file) / 1024 / 1024, 1), "MB\n")

# Also save a small sample for quick examples
set.seed(42)
crosswalk_sample <- crosswalk[sample(.N, min(1000, .N))]
sample_file <- file.path(output_dir, "getting_started_crosswalk_sample.rds")
saveRDS(crosswalk_sample, sample_file)
cat("Sample crosswalk saved to:", sample_file, "\n")

# =============================================================================
# STEP 5: SAVE SUMMARY STATISTICS AS STRUCTURED DATA
# =============================================================================

cat("\n=== Saving summary statistics ===\n")

summary_stats <- list(
  # Data info
  n_observations = n_obs,
  n_quarters = n_quarters,
  year_min = year_min,
  year_max = year_max,

  # Crosswalk info
  n_households = n_households,

  # Determination counts
  n_month_determined = n_month_determined,
  n_fortnight_determined = n_fortnight_determined,
  n_week_determined = n_week_determined,

  # Determination rates
  rate_month = rate_month,
  rate_fortnight = rate_fortnight,
  rate_week = rate_week,

  # Timestamp
  generated_at = Sys.time()
)

stats_file <- file.path(output_dir, "getting_started_stats.rds")
saveRDS(summary_stats, stats_file)
cat("Summary statistics saved to:", stats_file, "\n")

# =============================================================================
# PART B: SIDRA MENSALIZATION
# =============================================================================
# The code below matches the vignette's Section 4 (SIDRA workflow) exactly.
# =============================================================================

cat("\n=== SIDRA Mensalization ===\n\n")

# --- VIGNETTE CODE: sidra-quickstart ---
# Step 1: Fetch rolling quarter data from SIDRA API
rolling_quarters <- fetch_sidra_rolling_quarters()

# Step 2: Convert to exact monthly estimates
monthly <- mensalize_sidra_series(rolling_quarters)

# Step 3: Use your monthly data
head(monthly[, .(anomesexato, m_popocup, m_taxadesocup)])
# --- END VIGNETTE CODE ---

cat("\n  Rolling quarters:", nrow(rolling_quarters), "rows x",
    ncol(rolling_quarters), "cols\n")
cat("  Monthly estimates:", nrow(monthly), "rows x",
    ncol(monthly), "cols\n")

# Save for verification
saveRDS(rolling_quarters,
        file.path(output_dir, "getting_started_rolling_quarters.rds"))
saveRDS(monthly,
        file.path(output_dir, "getting_started_monthly.rds"))

cat("  Saved: getting_started_rolling_quarters.rds\n")
cat("  Saved: getting_started_monthly.rds\n")

# =============================================================================
# STEP 6: GENERATE FIGURE FOR SIDRA SECTION
# =============================================================================
# The plotting code below matches the vignette's <details> block exactly.
# =============================================================================

cat("\n=== Generating figure ===\n\n")

figures_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods/vignettes/figures/getting-started"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# --- VIGNETTE CODE: sidra-plot ---
library(ggplot2)

# Prepare comparison data (merge rolling quarter and monthly estimates)
plot_data <- merge(
  rolling_quarters[, .(anomesexato = anomesfinaltrimmovel, rolling = taxadesocup)],
  monthly[, .(anomesexato, monthly = m_taxadesocup)],
  by = "anomesexato"
)[anomesexato >= 201901 & anomesexato <= 202312]

plot_data[, date := as.Date(paste0(substr(anomesexato, 1, 4), "-",
                                    substr(anomesexato, 5, 6), "-01"))]

# Reshape to long format
plot_long <- melt(plot_data, id.vars = c("anomesexato", "date"),
                  variable.name = "type", value.name = "rate")
plot_long[, type := factor(type,
                           levels = c("rolling", "monthly"),
                           labels = c("Rolling Quarter", "Monthly"))]

# Plot
fig <- ggplot(plot_long, aes(x = date, y = rate, color = type)) +
  geom_line(linewidth = 0.8) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-12-31"),
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.08) +
  scale_color_manual(values = c("Rolling Quarter" = "#888888",
                                "Monthly" = "#E53935"),
                     name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Unemployment Rate: Monthly vs Rolling Quarter (2019-2023)",
    subtitle = "Monthly estimates capture the true timing and magnitude of economic shocks",
    x = NULL,
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
# --- END VIGNETTE CODE ---

ggsave(file.path(figures_dir, "fig_monthly_vs_quarterly.png"), fig,
       width = 10, height = 5.5, dpi = 150, bg = "white")

cat("  Saved: fig_monthly_vs_quarterly.png\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("PRECOMPUTE COMPLETE\n")
cat(strrep("=", 70), "\n")
cat("\nGenerated files:\n")
cat("  1.", output_file, "\n")
cat("  2.", crosswalk_file, "\n")
cat("  3.", sample_file, "\n")
cat("  4.", stats_file, "\n")
cat("  5.", file.path(output_dir, "getting_started_rolling_quarters.rds"), "\n")
cat("  6.", file.path(output_dir, "getting_started_monthly.rds"), "\n")
cat("  7.", file.path(figures_dir, "fig_monthly_vs_quarterly.png"), "\n")
cat("\nKey results:\n")
cat(sprintf("  - Microdata: %s observations, %s quarters (%s-%s)\n",
            format(n_obs, big.mark = ","), n_quarters, year_min, year_max))
cat(sprintf("  - Households: %s\n", format(n_households, big.mark = ",")))
cat(sprintf("  - Month determination: %.2f%%\n", rate_month * 100))
cat(sprintf("  - Fortnight determination: %.2f%%\n", rate_fortnight * 100))
cat(sprintf("  - Week determination: %.2f%%\n", rate_week * 100))
cat(sprintf("  - SIDRA monthly series: %d rows x %d columns\n",
            nrow(monthly), ncol(monthly)))
cat("\n=== Done! ===\n")
