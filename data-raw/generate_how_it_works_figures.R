# =============================================================================
# Generate Figures and Pre-computed Data for how-it-works.Rmd Vignette
# =============================================================================
#
# This script produces methodological examples for the "How PNADCperiods Works"
# vignette. All figures and tables are computed from actual PNADC data.
#
# Contents:
#   1. Determination rate vs number of quarters stacked (the key figure)
#   2. Determination rates by quarter (table showing boundary effects)
#   3. Exception quarters detected (list)
#   4. Month/fortnight/week rate comparison (table)
#   5. Crosswalk sample (example output structure)
#
# =============================================================================

library(PNADCperiods)
library(data.table)
library(ggplot2)
library(fst)

# =============================================================================
# PATHS
# =============================================================================

pnadc_dir     <- "D:/Dropbox/Bancos_Dados/PNADC/Trimestral/Dados/"
processed_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/data/processed/"
fig_dir       <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/figures/how-it-works/"
table_dir     <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/tables/how-it-works/"
pkg_fig_dir   <- "D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods/vignettes/figures/how-it-works/"
test_results_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/test_results/summaries/"

# Create directories
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pkg_fig_dir, recursive = TRUE, showWarnings = FALSE)

# Figure settings
fig_width  <- 9
fig_height <- 5
fig_dpi    <- 150

data.table::setDTthreads(4)

# =============================================================================
# CACHE FILES
# =============================================================================

cumulative_cache <- paste0(processed_dir, "how_it_works_cumulative_det.fst")
quarterly_cache <- paste0(processed_dir, "how_it_works_quarterly_det.fst")
crosswalk_cache <- paste0(processed_dir, "how_it_works_crosswalk_sample.fst")

# =============================================================================
# HELPER: List and sort quarterly files
# =============================================================================

get_sorted_files <- function() {
  files <- list.files(pnadc_dir, pattern = "\\.fst$", full.names = TRUE)
  files <- files[!grepl("input", files)]

  file_info <- data.table(path = files, filename = basename(files))
  file_info[, year := as.integer(gsub(".*pnadc_([0-9]{4}).*", "\\1", filename))]
  file_info[, quarter := as.integer(gsub(".*-([0-9])q.*", "\\1", filename))]
  setorder(file_info, year, quarter)

  file_info
}

# =============================================================================
# HELPER: Load minimal columns for identification
# =============================================================================

load_minimal_pnadc <- function(filepath) {
  cols_needed <- c("Ano", "Trimestre", "UPA", "V1008", "V1014",
                   "V2008", "V20081", "V20082", "V2009")

  dt <- read_fst(filepath, as.data.table = TRUE)
  setnames(dt, names(dt), toupper(names(dt)))

  name_map <- c(
    "ANO" = "Ano", "TRIMESTRE" = "Trimestre",
    "UPA" = "UPA", "V1008" = "V1008", "V1014" = "V1014",
    "V2008" = "V2008", "V20081" = "V20081",
    "V20082" = "V20082", "V2009" = "V2009"
  )

  available <- intersect(toupper(cols_needed), names(dt))
  dt <- dt[, ..available]

  for (old_name in names(dt)) {
    if (old_name %in% names(name_map)) {
      setnames(dt, old_name, name_map[old_name])
    }
  }

  dt
}

# =============================================================================
# STEP 1: COMPUTE CUMULATIVE DETERMINATION RATES
# =============================================================================

cat("=== Step 1: Compute Cumulative Determination Rates ===\n")

if (file.exists(cumulative_cache)) {
  cat("Loading cached cumulative rates...\n")
  cumulative_det <- fst::read_fst(cumulative_cache, as.data.table = TRUE)
} else {
  file_info <- get_sorted_files()
  n_total <- nrow(file_info)

  cat("Found", n_total, "quarterly files\n")

  # Test points: 1, 2, 4, 8, 12, 20, 32, 40, and full
  test_points <- c(1, 2, 4, 8, 12, 20, 32, 40, n_total)
  test_points <- unique(pmin(test_points, n_total))

  cumulative_results <- list()

  for (n_quarters in test_points) {
    cat("Processing", n_quarters, "quarters...\n")

    # Load and stack files
    current_files <- file_info$path[1:n_quarters]
    pnadc_list <- lapply(current_files, load_minimal_pnadc)
    pnadc_temp <- rbindlist(pnadc_list, fill = TRUE)
    rm(pnadc_list); gc()

    # Run identification
    result <- pnadc_identify_periods(pnadc_temp, verbose = FALSE)

    # Compute rate
    det_rate <- mean(result$determined_month, na.rm = TRUE)

    cumulative_results[[length(cumulative_results) + 1]] <- data.table(
      n_quarters = n_quarters,
      n_observations = nrow(pnadc_temp),
      det_rate = det_rate
    )

    cat("  -> Determination rate:", sprintf("%.1f%%", det_rate * 100), "\n")

    rm(pnadc_temp, result); gc()
  }

  cumulative_det <- rbindlist(cumulative_results)
  fst::write_fst(cumulative_det, cumulative_cache)
}

cat("\nCumulative determination rates:\n")
print(cumulative_det)

# =============================================================================
# STEP 2: COMPUTE DETERMINATION RATES BY QUARTER (with full stacking)
# =============================================================================

cat("\n=== Step 2: Compute Determination Rates by Quarter ===\n")

if (file.exists(quarterly_cache)) {
  cat("Loading cached quarterly rates...\n")
  quarterly_det <- fst::read_fst(quarterly_cache, as.data.table = TRUE)
} else {
  file_info <- get_sorted_files()

  cat("Loading all", nrow(file_info), "files for full crosswalk...\n")

  pnadc_list <- lapply(file_info$path, function(f) {
    cat("  Loading:", basename(f), "\n")
    load_minimal_pnadc(f)
  })

  pnadc <- rbindlist(pnadc_list, fill = TRUE)
  rm(pnadc_list); gc()

  cat("Total observations:", format(nrow(pnadc), big.mark = ","), "\n")

  # Build full crosswalk
  cat("Building crosswalk with all period types...\n")
  crosswalk <- pnadc_identify_periods(pnadc, verbose = TRUE)

  # Compute rates by quarter
  quarterly_det <- crosswalk[, .(
    n_households = .N,
    det_month = mean(determined_month, na.rm = TRUE),
    det_fortnight = mean(determined_fortnight, na.rm = TRUE),
    det_week = mean(determined_week, na.rm = TRUE)
  ), by = .(Ano, Trimestre)]

  setorder(quarterly_det, Ano, Trimestre)

  # Save cache
  fst::write_fst(quarterly_det, quarterly_cache)

  # Also save a crosswalk sample
  crosswalk_sample <- crosswalk[Ano == 2023 & Trimestre == 2][1:20]
  fst::write_fst(crosswalk_sample, crosswalk_cache)

  rm(pnadc, crosswalk); gc()
}

cat("\nQuarterly determination rates (first 10):\n
")
print(head(quarterly_det, 10))

# =============================================================================
# STEP 3: GENERATE FIGURE - Determination Rate vs Quarters Stacked
# =============================================================================

cat("\n=== Step 3: Generate Figure ===\n")

# --- VIGNETTE CODE: plot-determination-rate ---
p1 <- ggplot(cumulative_det, aes(x = n_quarters, y = det_rate * 100)) +
  geom_line(color = "#2166ac", linewidth = 1.2) +
  geom_point(color = "#2166ac", size = 3.5) +
  geom_hline(yintercept = max(cumulative_det$det_rate) * 100,
             linetype = "dashed", color = "#666666", alpha = 0.7) +
  annotate("text",
           x = max(cumulative_det$n_quarters) * 0.85,
           y = max(cumulative_det$det_rate) * 100 + 1.2,
           label = paste0(sprintf("%.1f%%", max(cumulative_det$det_rate) * 100),
                         " (full history)"),
           color = "#666666", size = 3.5, hjust = 0.5) +
  annotate("text",
           x = 2,
           y = cumulative_det[n_quarters == 1, det_rate * 100] + 2,
           label = paste0(sprintf("%.0f%%", cumulative_det[n_quarters == 1, det_rate * 100]),
                         "\n(single quarter)"),
           color = "#2166ac", size = 3, hjust = 0, vjust = 0) +
  scale_x_continuous(
    breaks = c(1, 10, 20, 30, 40, 50),
    limits = c(0, max(cumulative_det$n_quarters) + 5)
  ) +
  scale_y_continuous(
    breaks = seq(65, 100, by = 5),
    limits = c(60, 100),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Monthly Determination Rate Improves with More Quarters Stacked",
    subtitle = paste0("Real PNADC data (2012-", max(quarterly_det$Ano),
                      "): Cross-quarter aggregation exploits rotating panel design"),
    x = "Number of Quarters Stacked",
    y = "Determination Rate"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "#666666", size = 11),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11),
    plot.margin = margin(10, 20, 10, 10)
  )
# --- END VIGNETTE CODE: plot-determination-rate ---

ggsave(file.path(fig_dir, "fig-determination-rate.png"),
       plot = p1, width = fig_width, height = fig_height,
       dpi = fig_dpi, bg = "white")
cat("Saved: fig-determination-rate.png\n")

# Copy to package
file.copy(file.path(fig_dir, "fig-determination-rate.png"),
          file.path(pkg_fig_dir, "fig-determination-rate.png"),
          overwrite = TRUE)
cat("Copied to:", pkg_fig_dir, "\n")

# =============================================================================
# STEP 4: SAVE TABLES FOR VIGNETTE
# =============================================================================

cat("\n=== Step 4: Save Tables ===\n")

# Table 1: Cumulative determination rates
fwrite(cumulative_det, file.path(table_dir, "cumulative_determination_rates.csv"))

# Table 2: Quarterly determination rates
fwrite(quarterly_det, file.path(table_dir, "quarterly_determination_rates.csv"))

# Table 3: Summary by period
period_summary <- quarterly_det[, .(
  period = fcase(
    Ano == min(Ano), "First year",
    Ano >= 2013 & Ano <= 2019, "2013-2019 (stable)",
    Ano >= 2020 & Ano <= 2021, "2020-2021 (COVID)",
    Ano >= 2022, "2022+"
  ),
  Ano, Trimestre, det_month, det_fortnight, det_week
)]

summary_by_period <- period_summary[, .(
  n_quarters = .N,
  mean_month = mean(det_month),
  min_month = min(det_month),
  max_month = max(det_month),
  mean_fortnight = mean(det_fortnight),
  mean_week = mean(det_week)
), by = period][order(-mean_month)]

fwrite(summary_by_period, file.path(table_dir, "summary_by_period.csv"))

# Table 4: Crosswalk sample structure
if (file.exists(crosswalk_cache)) {
  crosswalk_sample <- fst::read_fst(crosswalk_cache, as.data.table = TRUE)
  fwrite(crosswalk_sample, file.path(table_dir, "crosswalk_sample.csv"))
}

cat("Saved tables to:", table_dir, "\n")

# =============================================================================
# STEP 5: GENERATE EXPERIMENTAL STRATEGIES TABLE AND FIGURE
# =============================================================================

cat("\n=== Step 5: Generate Experimental Strategies Outputs ===\n")

# Load comprehensive test results
det_rates_master_file <- file.path(test_results_dir, "determination_rates_master.csv")

if (file.exists(det_rates_master_file)) {
  det_rates_master <- fread(det_rates_master_file)

  # Filter to relevant crosswalks (CW-FULL and experimental strategies)
  experimental_rates <- det_rates_master[crosswalk_id %in% c("CW-FULL",
    "EXP-P07", "EXP-P08", "EXP-P09", "EXP-P095",
    "EXP-U07", "EXP-U08", "EXP-U09", "EXP-U095",
    "EXP-B07", "EXP-B08", "EXP-B09", "EXP-B095")]

  # Create readable strategy labels
  experimental_rates[, strategy_label := fcase(
    crosswalk_id == "CW-FULL", "Strict baseline",
    strategy == "probabilistic", paste0("Probabilistic (conf=", conf_threshold, ")"),
    strategy == "upa_aggregation", paste0("UPA aggregation (", upa_threshold, ")"),
    strategy == "both", paste0("Both (conf=", conf_threshold, ")")
  )]

  # Create output table
  exp_table <- experimental_rates[, .(
    crosswalk_id,
    strategy = strategy_label,
    month_pct = round(det_month_overall * 100, 1),
    fortnight_pct = round(det_fortnight_overall * 100, 1),
    week_pct = round(det_week_overall * 100, 1),
    n_obs
  )]

  # Save experimental strategies table
  fwrite(exp_table, file.path(table_dir, "experimental_strategy_rates.csv"))
  cat("Saved: experimental_strategy_rates.csv\n")

  # Generate figure: Compare strict vs probabilistic strategies
  # --- VIGNETTE CODE: plot-experimental-strategies ---
  # Select key configurations for visualization
  exp_plot_data <- experimental_rates[crosswalk_id %in% c(
    "CW-FULL", "EXP-P08", "EXP-P09"
  )]

  exp_plot_data[, strategy_display := factor(strategy_label,
    levels = c("Strict baseline",
               "Probabilistic (conf=0.8)",
               "Probabilistic (conf=0.9)"),
    labels = c("Strict baseline",
               "Probabilistic (conf=0.8)",
               "Probabilistic (conf=0.9)")
  )]

  # Reshape to long format
  exp_plot_data_long <- melt(exp_plot_data,
    id.vars = "strategy_display",
    measure.vars = c("det_month_overall", "det_fortnight_overall", "det_week_overall"),
    variable.name = "period",
    value.name = "rate"
  )

  exp_plot_data_long[, period := factor(period,
    levels = c("det_month_overall", "det_fortnight_overall", "det_week_overall"),
    labels = c("Month", "Fortnight", "Week")
  )]

  exp_plot_data_long[, rate_pct := rate * 100]

  # Create bar chart
  p_exp <- ggplot(exp_plot_data_long,
                  aes(x = period, y = rate_pct, fill = strategy_display)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", rate_pct)),
              position = position_dodge(width = 0.7),
              vjust = -0.5, size = 3) +
    labs(
      title = "Determination Rates: Strict vs Probabilistic Strategies",
      subtitle = "8-quarter benchmark (2019-2020), 3.76M observations",
      x = NULL,
      y = "Determination Rate (%)",
      fill = "Strategy"
    ) +
    scale_y_continuous(
      limits = c(0, 105),
      breaks = seq(0, 100, by = 20),
      labels = function(x) paste0(x, "%")
    ) +
    scale_fill_manual(values = c(
      "Strict baseline" = "#D55E00",
      "Probabilistic (conf=0.8)" = "#0072B2",
      "Probabilistic (conf=0.9)" = "#009E73"
    )) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "#666666", size = 11),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(size = 11),
      plot.margin = margin(10, 20, 10, 10)
    )
  # --- END VIGNETTE CODE: plot-experimental-strategies ---

  # Save figure
  ggsave(file.path(fig_dir, "fig-experimental-strategies.png"),
         plot = p_exp, width = fig_width, height = fig_height,
         dpi = fig_dpi, bg = "white")
  cat("Saved: fig-experimental-strategies.png\n")

  # Copy to package vignettes folder
  file.copy(
    file.path(fig_dir, "fig-experimental-strategies.png"),
    file.path(pkg_fig_dir, "fig-experimental-strategies.png"),
    overwrite = TRUE
  )
  cat("Copied to:", pkg_fig_dir, "\n")

  # Print summary
  cat("\nExperimental Strategy Rates (8Q benchmark):\n")
  print(exp_table[order(-month_pct)])

} else {
  cat("WARNING: Comprehensive test results not found at:\n")
  cat("  ", det_rates_master_file, "\n")
  cat("Skipping experimental strategies outputs.\n")
}

# =============================================================================
# STEP 6: PRINT SUMMARY FOR VIGNETTE TEXT
# =============================================================================

cat("\n=== Summary Statistics for Vignette ===\n")

cat("\n-- Cumulative Rates (for 'Why Stacking Matters' section) --\n")
for (i in 1:nrow(cumulative_det)) {
  cat(sprintf("| %d quarters | %.1f%% |\n",
              cumulative_det$n_quarters[i],
              cumulative_det$det_rate[i] * 100))
}

cat("\n-- Overall Rates --\n")
cat("Month:     ", sprintf("%.1f%%", mean(quarterly_det$det_month) * 100), "\n")
cat("Fortnight: ", sprintf("%.1f%%", mean(quarterly_det$det_fortnight) * 100), "\n")
cat("Week:      ", sprintf("%.1f%%", mean(quarterly_det$det_week) * 100), "\n")

cat("\n-- By Period --\n")
print(summary_by_period)

cat("\n=== Done! ===\n")
