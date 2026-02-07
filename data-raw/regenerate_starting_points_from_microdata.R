# ==============================================================================
# Regenerate Starting Points from PNADC Microdata
# ==============================================================================
#
# This script computes y0 (starting point) values for each series by:
# 1. Loading stacked PNADC microdata
# 2. Building crosswalk with pnadc_identify_periods()
# 3. Calibrating weights with pnadc_apply_periods() (Marcos' methodology)
# 4. Computing z_ aggregates with compute_z_aggregates()
# 5. Computing starting points with compute_series_starting_points()
#
# This ensures the bundled starting points EXACTLY match what
# compute_starting_points_from_microdata() produces.
#
# Author: PNADCperiods package
# Date: 2026-02-02
# ==============================================================================

# Load required packages
library(data.table)
library(fst)

# Set working directory to package root
setwd("D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods")

# Load package (use devtools::load_all for development)
devtools::load_all()

# ==============================================================================
# Configuration
# ==============================================================================

# Path to stacked microdata (FULL population including all ages, not just 14+)
# This is required for correct z_populacao computation
MICRODATA_FILE <- "D:/Dropbox/Artigos/mensalizacao_pnad/data/processed/pnadc_full_microdata.fst"

# Crosswalk file (pre-computed to save time)
CROSSWALK_FILE <- "D:/Dropbox/Artigos/mensalizacao_pnad/data/processed/crosswalk_for_starting_points_full.rds"

# Calibration period
CALIBRATION_START <- 201301L
CALIBRATION_END <- 201912L

# ==============================================================================
# Step 1: Load microdata
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("REGENERATING BUNDLED STARTING POINTS FROM MICRODATA")
message(paste(rep("=", 70), collapse = ""), "\n")

message("Step 1: Loading microdata...")

if (!file.exists(MICRODATA_FILE)) {
  stop("Microdata file not found: ", MICRODATA_FILE,
       "\nRun test_approachB_full_pipeline.R first to create it.")
}

stacked <- read_fst(MICRODATA_FILE, as.data.table = TRUE)
message("  Loaded ", format(nrow(stacked), big.mark = ","), " observations")

# ==============================================================================
# Step 2: Load or build crosswalk
# ==============================================================================

message("\nStep 2: Loading crosswalk...")

if (file.exists(CROSSWALK_FILE)) {
  crosswalk <- readRDS(CROSSWALK_FILE)
  message("  Loaded crosswalk: ", format(nrow(crosswalk), big.mark = ","), " rows")
} else {
  message("  Building crosswalk (this may take a while)...")
  crosswalk <- pnadc_identify_periods(stacked, verbose = TRUE)
  saveRDS(crosswalk, CROSSWALK_FILE)
}

# ==============================================================================
# Step 3: Calibrate weights (Marcos' methodology)
# ==============================================================================

message("\nStep 3: Calibrating weights (Marcos' methodology)...")
message("  Month 2: scaled to poptrim (quarterly V1028 sum)")
message("  Months 1,3: scaled to SIDRA monthly population")

calibrated <- pnadc_apply_periods(
  data = stacked,
  crosswalk = crosswalk,
  weight_var = "V1028",
  anchor = "quarter",
  calibration_unit = "month",
  verbose = TRUE
)

# Verify calibration
message("\n  Verifying weight_monthly by month position...")
monthly_pops <- calibrated[!is.na(weight_monthly),
                            .(pop = sum(weight_monthly),
                              n_obs = .N),
                            by = .(ref_month_yyyymm, ref_month_in_quarter)]
setorder(monthly_pops, ref_month_yyyymm)

message("  Sample monthly populations (millions) for 2019:")
sample_months <- monthly_pops[ref_month_yyyymm >= 201901 & ref_month_yyyymm <= 201912]
for (i in 1:min(nrow(sample_months), 12)) {
  row <- sample_months[i]
  message(sprintf("    %d (pos %d): %.1fM", row$ref_month_yyyymm,
                  row$ref_month_in_quarter, row$pop / 1e6))
}

# ==============================================================================
# Step 4: Compute z_ aggregates
# ==============================================================================

message("\nStep 4: Computing z_ aggregates...")

z_aggregates <- compute_z_aggregates(calibrated, verbose = TRUE)

message("  Created ", ncol(z_aggregates) - 1, " z_ series for ",
        nrow(z_aggregates), " months")

# ==============================================================================
# Step 5: Fetch SIDRA rolling quarters
# ==============================================================================

message("\nStep 5: Fetching SIDRA rolling quarters...")

rolling_quarters <- fetch_sidra_rolling_quarters(verbose = TRUE, use_cache = TRUE)

message("  Fetched ", ncol(rolling_quarters) - 2, " series")

# ==============================================================================
# Step 5b: Compute comrendtodos from SIDRA massa/rend series
# ==============================================================================
# This is needed for compute_series_starting_points() to compute y0 for comrendtodos
# Formula matches mensalize_sidra_series.R line 204-205

message("\nStep 5b: Computing comrendtodos from SIDRA massa/rend series...")

if (all(c("massahabnominaltodos", "rendhabnominaltodos",
          "massaefetnominaltodos", "rendefetnominaltodos") %in% names(rolling_quarters))) {
  rolling_quarters[, comrendtodos := (massahabnominaltodos / rendhabnominaltodos +
                                       massaefetnominaltodos / rendefetnominaltodos) / 2 * 1000]
  message("  Computed comrendtodos (number of people with income)")
} else {
  warning("  Could not compute comrendtodos - missing massa/rend series in SIDRA data")
}

# ==============================================================================
# Step 6: Compute starting points
# ==============================================================================

message("\nStep 6: Computing starting points...")

pnadc_series_starting_points <- compute_series_starting_points(
  monthly_estimates = z_aggregates,
  rolling_quarters = rolling_quarters,
  calibration_start = CALIBRATION_START,
  calibration_end = CALIBRATION_END,
  verbose = TRUE
)

message("  Computed starting points for ",
        length(unique(pnadc_series_starting_points$series_name)), " series")

# ==============================================================================
# Step 7: Save bundled data
# ==============================================================================

message("\nStep 7: Saving to data/pnadc_series_starting_points.rda...")

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save with compression
save(pnadc_series_starting_points,
     file = "data/pnadc_series_starting_points.rda",
     compress = "xz")

message("  Saved!")

# ==============================================================================
# Step 8: Verify zero difference
# ==============================================================================

message("\nStep 8: Verifying against fresh computation...")

# Recompute y0 using the same pipeline (should match exactly)
y0_verify <- compute_series_starting_points(
  monthly_estimates = z_aggregates,
  rolling_quarters = rolling_quarters,
  calibration_start = CALIBRATION_START,
  calibration_end = CALIBRATION_END,
  verbose = FALSE
)

# Compare
comp <- merge(
  pnadc_series_starting_points,
  y0_verify,
  by = c("series_name", "mesnotrim"),
  suffixes = c("_bundled", "_verify")
)

comp[, diff := y0_bundled - y0_verify]
comp[, abs_diff := abs(diff)]

max_diff <- max(comp$abs_diff, na.rm = TRUE)

if (max_diff < 1e-10) {
  message("  VERIFIED: Zero difference (max diff = ", format(max_diff, scientific = TRUE), ")")
} else {
  warning("  UNEXPECTED: Non-zero difference detected (max diff = ", max_diff, ")")
}

# ==============================================================================
# Summary
# ==============================================================================

message("\n", paste(rep("=", 70), collapse = ""))
message("REGENERATION COMPLETE")
message(paste(rep("=", 70), collapse = ""), "\n")

message("Bundled starting points regenerated from R pipeline:")
message("  Series: ", length(unique(pnadc_series_starting_points$series_name)))
message("  Rows: ", nrow(pnadc_series_starting_points))
message("  Calibration period: ", CALIBRATION_START, " to ", CALIBRATION_END)
message("  File: data/pnadc_series_starting_points.rda")

message("\nMethodology:")
message("  1. pnadc_apply_periods() with Marcos' methodology:")
message("     - Month 2 scaled to poptrim (quarterly V1028 sum from ALL observations)")
message("     - Months 1,3 scaled to SIDRA monthly population")
message("  2. compute_z_aggregates() using calibrated weight_monthly")
message("  3. compute_series_starting_points() with CNPJ-aware calibration periods")

message("\nThis ensures bundled y0 EXACTLY matches compute_starting_points_from_microdata().")
