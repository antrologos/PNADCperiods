# =============================================================================
# Pre-compute results for download-and-prepare.Rmd vignette
# =============================================================================
#
# This script runs the same workflow shown in the vignette using real data.
# It validates that all code examples work correctly.
#
# Outputs:
#   - output/vignette/download_prepare_stats.rds
#
# =============================================================================

library(data.table)
library(fst)

# Load the PNADCperiods package
devtools::load_all("PNADCperiods")

# =============================================================================
# PATHS
# =============================================================================

data_dir <- "D:/Dropbox/Bancos_Dados/PNADC/Trimestral/Dados/"
output_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Precomputing results for download-and-prepare.Rmd ===\n\n")

# =============================================================================
# STEP 1: STACK DATA (matches vignette Step 4)
# =============================================================================

cat("=== Stacking quarterly data ===\n\n")

# --- VIGNETTE CODE: stack-data ---
# Columns needed for mensalization
cols_needed <- c(
  # Time and identifiers
  "Ano", "Trimestre", "UPA", "V1008", "V1014",
  # Birthday variables (for reference period algorithm)
  "V2008", "V20081", "V20082", "V2009",
  # Weight and stratification (for weight calibration)
  "V1028", "UF", "posest", "posest_sxi"
)

# Stack all quarters (2020-2024 matching vignette example)
files <- list.files(data_dir, pattern = "pnadc_202[0-4].*\\.fst$", full.names = TRUE)

pnadc_stacked <- rbindlist(lapply(files, function(f) {
  cat("Loading:", basename(f), "\n")
  read_fst(f, columns = cols_needed, as.data.table = TRUE)
}))

cat("Total observations:", format(nrow(pnadc_stacked), big.mark = ","), "\n")
# --- END VIGNETTE CODE ---

n_obs <- nrow(pnadc_stacked)
n_quarters <- uniqueN(pnadc_stacked[, .(Ano, Trimestre)])

cat("\nQuarters loaded:", n_quarters, "\n\n")

# =============================================================================
# STEP 2: BUILD CROSSWALK (matches vignette Step 5)
# =============================================================================

cat("=== Building crosswalk ===\n\n")

# --- VIGNETTE CODE: mensalize ---
# Step 1: Build crosswalk (identify reference periods)
crosswalk <- pnadc_identify_periods(pnadc_stacked, verbose = TRUE)

# Check determination rates
crosswalk[, .(
  month_rate = mean(determined_month),
  fortnight_rate = mean(determined_fortnight),
  week_rate = mean(determined_week)
)]
# --- END VIGNETTE CODE ---

# Extract rates for summary
det_rates <- list(
  month = crosswalk[, mean(determined_month, na.rm = TRUE)],
  fortnight = crosswalk[, mean(determined_fortnight, na.rm = TRUE)],
  week = crosswalk[, mean(determined_week, na.rm = TRUE)]
)

cat(sprintf("\nDetermination rates:\n"))
cat(sprintf("  Month:     %.2f%%\n", det_rates$month * 100))
cat(sprintf("  Fortnight: %.2f%%\n", det_rates$fortnight * 100))
cat(sprintf("  Week:      %.2f%%\n", det_rates$week * 100))

# =============================================================================
# STEP 3: APPLY CROSSWALK (matches vignette Step 5, without calibration)
# =============================================================================

cat("\n=== Applying crosswalk ===\n\n")

result <- pnadc_apply_periods(
  data = pnadc_stacked,
  crosswalk = crosswalk,
  weight_var = "V1028",
  anchor = "quarter",
  calibrate = FALSE,  # Skip calibration for speed in precompute
  verbose = TRUE
)

# --- VIGNETTE CODE: explore ---
# Distribution of reference months within quarters
month_dist <- result[, .N, by = ref_month_in_quarter][order(ref_month_in_quarter)]
# --- END VIGNETTE CODE ---

cat("\n=== Reference month distribution ===\n")
print(month_dist)

# =============================================================================
# STEP 4: SAVE SUMMARY STATISTICS
# =============================================================================

cat("\n=== Saving summary statistics ===\n")

summary_stats <- list(
  n_observations = n_obs,
  n_quarters = n_quarters,
  n_households = nrow(crosswalk),
  rate_month = det_rates$month,
  rate_fortnight = det_rates$fortnight,
  rate_week = det_rates$week,
  month_distribution = month_dist,
  generated_at = Sys.time()
)

stats_file <- file.path(output_dir, "download_prepare_stats.rds")
saveRDS(summary_stats, stats_file)
cat("Saved:", stats_file, "\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("PRECOMPUTE COMPLETE\n")
cat(strrep("=", 60), "\n")
cat(sprintf("\nKey results:\n"))
cat(sprintf("  - Observations: %s (%d quarters)\n",
            format(n_obs, big.mark = ","), n_quarters))
cat(sprintf("  - Households: %s\n", format(nrow(crosswalk), big.mark = ",")))
cat(sprintf("  - Month determination: %.2f%%\n", det_rates$month * 100))
cat(sprintf("  - Fortnight determination: %.2f%%\n", det_rates$fortnight * 100))
cat(sprintf("  - Week determination: %.2f%%\n", det_rates$week * 100))
cat("\n=== Done! ===\n")
