# ==============================================================================
# Generate Starting Points for SIDRA Series Mensalization
# ==============================================================================
#
# This script computes y0 (starting point) values for each series by:
# 1. Reading mensalized estimates from Marcos Hecksher's reference file
# 2. Computing cumulative variations from SIDRA rolling quarter data
# 3. Back-projecting: e0 = m - cum for calibration period (2013-2019)
# 4. Averaging e0 by mesnotrim (month position in quarter)
#
# The resulting starting points allow the mensalize_sidra_series() function
# to produce exact monthly estimates matching the reference methodology.
#
# Author: Generated for PNADCperiods package
# Reference: Hecksher, M. (2024). Mensalizacao da PNADC.
# ==============================================================================

library(data.table)
library(readxl)

# Source the package functions
source("R/sidra-series-metadata.R")
source("R/fetch-sidra-series.R")

# ==============================================================================
# Configuration
# ==============================================================================

# Path to Marcos's reference file with mensalized estimates
REFERENCE_FILE <- "D:/Dropbox/Artigos/mensalizacao_pnad/code/original_codes/PNADC_TrimMovel_e_Mensalizadas_202512.xls"

# Calibration period (as used in Marcos's methodology)
CALIBRATION_START <- 201301L
CALIBRATION_END <- 201912L

# ==============================================================================
# Step 1: Read reference mensalized series
# ==============================================================================

message("Reading reference mensalized series from Excel...")
ref_mensalizadas <- as.data.table(read_excel(
  REFERENCE_FILE,
  sheet = "Mensalizadas"
))

# Ensure anomesexato is integer
ref_mensalizadas[, anomesexato := as.integer(anomesexato)]

# Keep only m_* columns
m_cols <- grep("^m_", names(ref_mensalizadas), value = TRUE)
ref_mensalizadas <- ref_mensalizadas[, c("anomesexato", m_cols), with = FALSE]

message("  Found ", length(m_cols), " mensalized series")
message("  Date range: ", min(ref_mensalizadas$anomesexato), " to ",
        max(ref_mensalizadas$anomesexato))

# ==============================================================================
# Step 2: Fetch SIDRA rolling quarter data
# ==============================================================================

message("\nFetching rolling quarter series from SIDRA...")

# Get all series except price indices (which don't need mensalization)
meta <- get_sidra_series_metadata()
series_to_fetch <- meta[category != "price_index", series_name]

# Note: This may take several minutes on first run
rolling_quarters <- fetch_sidra_rolling_quarters(
  series = series_to_fetch,
  verbose = TRUE
)

message("  Fetched ", ncol(rolling_quarters) - 2, " series")

# ==============================================================================
# Step 3: Merge reference and SIDRA data
# ==============================================================================

# Align date columns: reference uses anomesexato, SIDRA uses anomesfinaltrimmovel
# But they represent the same thing (the end month of the rolling quarter)

message("\nMerging reference and SIDRA data...")

# Rename SIDRA date column to match
setnames(rolling_quarters, "anomesfinaltrimmovel", "anomesexato")

# The reference file starts at 201201 but SIDRA only has data from 201203
# So we need to align properly

dt <- merge(rolling_quarters, ref_mensalizadas, by = "anomesexato", all.x = TRUE)
setorder(dt, anomesexato)

message("  Merged data: ", nrow(dt), " rows")

# ==============================================================================
# Step 4: Compute starting points for each series
# ==============================================================================

message("\nComputing starting points...")

# Get series names that exist in both datasets
sidra_series <- setdiff(names(rolling_quarters), c("anomesexato", "mesnotrim"))
m_series <- gsub("^m_", "", m_cols)

# Find common series (SIDRA series that have mensalized reference values)
common_series <- intersect(sidra_series, m_series)
message("  Processing ", length(common_series), " common series")

results <- list()

for (v in common_series) {
  m_col <- paste0("m_", v)

  # Skip if reference values don't exist
  if (!m_col %in% names(dt) || all(is.na(dt[[m_col]]))) {
    message("  Skipping ", v, " (no reference values)")
    next
  }

  # Get rolling quarter and mensalized values
  rq <- dt[[v]]
  m_ref <- dt[[m_col]]
  mesnotrim <- dt$mesnotrim
  yyyymm <- dt$anomesexato

  # Calculate d3 (3x variation)
  d3 <- 3 * (rq - shift(rq, 1L))
  d3[1] <- NA_real_

  # Separate by month position
  d3m1 <- ifelse(mesnotrim == 1, d3, NA_real_)
  d3m2 <- ifelse(mesnotrim == 2, d3, NA_real_)
  d3m3 <- ifelse(mesnotrim == 3, d3, NA_real_)

  # Cumulative sums
  cum1 <- cumsum(ifelse(is.na(d3m1), 0, d3m1))
  cum2 <- cumsum(ifelse(is.na(d3m2), 0, d3m2))
  cum3 <- cumsum(ifelse(is.na(d3m3), 0, d3m3))

  # Combine by mesnotrim
  cum <- numeric(length(rq))
  cum[mesnotrim == 1] <- cum1[mesnotrim == 1]
  cum[mesnotrim == 2] <- cum2[mesnotrim == 2]
  cum[mesnotrim == 3] <- cum3[mesnotrim == 3]

  # Back-projection: e0 = m_ref - cum
  # For calibration period only
  in_calib <- yyyymm >= CALIBRATION_START & yyyymm <= CALIBRATION_END
  e0 <- ifelse(in_calib, m_ref - cum, NA_real_)

  # Average by mesnotrim
  for (pos in 1:3) {
    mask <- in_calib & mesnotrim == pos
    y0_val <- mean(e0[mask], na.rm = TRUE)

    if (!is.finite(y0_val)) {
      message("  Warning: ", v, " mesnotrim=", pos, " has no valid e0 values")
      y0_val <- 0
    }

    results[[length(results) + 1]] <- data.table(
      series_name = v,
      mesnotrim = as.integer(pos),
      y0 = y0_val
    )
  }
}

# Combine all results
pnadc_series_starting_points <- rbindlist(results)

message("\nGenerated starting points for ", length(unique(pnadc_series_starting_points$series_name)), " series")

# ==============================================================================
# Step 5: Validate starting points
# ==============================================================================

message("\nValidating starting points...")

# Test mensalization with the computed starting points on a few series
test_series <- c("popocup", "popdesocup", "pop14mais")
test_series <- intersect(test_series, unique(pnadc_series_starting_points$series_name))

if (length(test_series) > 0) {
  # Source the mensalization function
  source("R/mensalize-sidra-series.R")

  # Fetch test data
  test_rq <- fetch_sidra_rolling_quarters(series = test_series, verbose = FALSE)

  # Mensalize
  test_m <- mensalize_sidra_series(
    test_rq,
    starting_points = pnadc_series_starting_points,
    compute_derived = FALSE,
    verbose = FALSE
  )

  # Compare with reference for calibration period
  test_ref <- ref_mensalizadas[anomesexato >= CALIBRATION_START & anomesexato <= CALIBRATION_END]

  for (v in test_series) {
    m_col <- paste0("m_", v)
    if (m_col %in% names(test_m) && m_col %in% names(test_ref)) {
      comp <- merge(
        test_m[anomesexato >= CALIBRATION_START & anomesexato <= CALIBRATION_END,
               .(anomesexato, computed = get(m_col))],
        test_ref[, .(anomesexato, reference = get(m_col))],
        by = "anomesexato"
      )
      comp[, diff := computed - reference]

      rmse <- sqrt(mean(comp$diff^2, na.rm = TRUE))
      max_diff <- max(abs(comp$diff), na.rm = TRUE)
      mean_diff <- mean(comp$diff, na.rm = TRUE)

      message("  ", v, ": RMSE=", round(rmse, 2),
              ", max_diff=", round(max_diff, 2),
              ", mean_diff=", round(mean_diff, 2))
    }
  }
}

# ==============================================================================
# Step 6: Save starting points
# ==============================================================================

message("\nSaving starting points to data/pnadc_series_starting_points.rda...")

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Save
save(pnadc_series_starting_points, file = "data/pnadc_series_starting_points.rda",
     compress = "xz")

message("Done!")

# Print summary
message("\n=== Summary ===")
message("Series with starting points: ", length(unique(pnadc_series_starting_points$series_name)))
message("Total rows: ", nrow(pnadc_series_starting_points))
message("Calibration period: ", CALIBRATION_START, " to ", CALIBRATION_END)
