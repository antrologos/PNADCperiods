# Validate the move from apisidra to the aggregated-data API v3.
#
# Compares every series fetched through the v3 client against the last
# bundle produced through apisidra before the Cloudflare challenge started:
# the `rolling_quarters.qs2` asset of the `data-latest` release of
# PNADCperiods-dashboard, fetched 2026-09-14.
#
# Get the baseline with:
#   gh release download data-latest --repo antrologos/PNADCperiods-dashboard \
#     --pattern rolling_quarters.qs2 --dir <dir>
#
# Then run (never `Rscript -e` on Windows):
#   PNADC_VALIDATION_BASELINE=<dir>/rolling_quarters.qs2 \
#     Rscript data-raw/validate_sidra_v3_migration.R
#
# Exit status is non-zero when any overlapping value differs, so the script
# doubles as the gate that guards the migration commits.

baseline_path <- Sys.getenv("PNADC_VALIDATION_BASELINE")
if (!nzchar(baseline_path) || !file.exists(baseline_path)) {
  stop("Set PNADC_VALIDATION_BASELINE to a rolling_quarters.qs2 baseline")
}

report_dir <- Sys.getenv("PNADC_VALIDATION_OUTPUT", dirname(baseline_path))

suppressMessages(devtools::load_all(".", quiet = TRUE))

baseline <- qs2::qs_read(baseline_path)
data.table::setDT(baseline)

cat("Baseline:", baseline_path, "\n")
cat("  rows:", nrow(baseline), " cols:", ncol(baseline), "\n")
cat("  periods:", min(baseline$anomesfinaltrimmovel), "-",
    max(baseline$anomesfinaltrimmovel), "\n\n")

clear_sidra_cache()
fresh <- fetch_sidra_rolling_quarters(series = "all", use_cache = FALSE,
                                      verbose = TRUE)

if (is.null(fresh)) stop("v3 fetch returned NULL; nothing to validate")
data.table::setDT(fresh)

cat("\nFetched through v3:\n")
cat("  rows:", nrow(fresh), " cols:", ncol(fresh), "\n")
cat("  periods:", min(fresh$anomesfinaltrimmovel), "-",
    max(fresh$anomesfinaltrimmovel), "\n\n")

# ---- 1. coverage ---------------------------------------------------------

missing_cols <- setdiff(names(baseline), names(fresh))
extra_cols   <- setdiff(names(fresh), names(baseline))

cat("Columns missing from the v3 result:",
    if (length(missing_cols)) paste(missing_cols, collapse = ", ") else "none",
    "\n")
cat("Columns only in the v3 result:",
    if (length(extra_cols)) paste(extra_cols, collapse = ", ") else "none",
    "\n")

missing_periods <- setdiff(baseline$anomesfinaltrimmovel,
                           fresh$anomesfinaltrimmovel)
cat("Baseline periods absent from the v3 result:",
    if (length(missing_periods)) paste(missing_periods, collapse = ", ") else "none",
    "\n\n")

# ---- 2. value-by-value comparison ---------------------------------------

shared_cols <- intersect(names(baseline), names(fresh))
shared_cols <- setdiff(shared_cols, c("anomesfinaltrimmovel", "mesnotrim"))

overlap <- intersect(baseline$anomesfinaltrimmovel, fresh$anomesfinaltrimmovel)
old <- baseline[anomesfinaltrimmovel %in% overlap][order(anomesfinaltrimmovel)]
new <- fresh[anomesfinaltrimmovel %in% overlap][order(anomesfinaltrimmovel)]

meta <- get_sidra_series_metadata()
digits_of <- function(series_name) {
  path <- meta$api_path[match(series_name, meta$series_name)]
  if (is.na(path) || !grepl("/d/", path, fixed = TRUE)) return(NA_integer_)
  .parse_sidra_api_path(path)$digits
}

report <- data.table::rbindlist(lapply(shared_cols, function(column) {
  a <- old[[column]]
  b <- new[[column]]
  differing <- which(!(is.na(a) & is.na(b)) & (is.na(a) | is.na(b) | a != b))
  data.table::data.table(
    series          = column,
    d_modifier      = digits_of(column),
    n_compared      = length(a),
    identical       = identical(a, b),
    n_differing     = length(differing),
    max_abs_diff    = if (length(differing)) {
      max(abs(a[differing] - b[differing]), na.rm = TRUE)
    } else 0,
    first_mismatch  = if (length(differing)) {
      old$anomesfinaltrimmovel[differing[1]]
    } else NA_integer_
  )
}))

data.table::setorder(report, identical, -n_differing)

cat("Compared", nrow(report), "series over", length(overlap), "periods\n")
cat("Identical:", sum(report$identical), "/", nrow(report), "\n\n")

if (any(!report$identical)) {
  cat("Series that differ:\n")
  print(report[identical == FALSE])
} else {
  cat("Every overlapping value matches the apisidra baseline.\n")
}

report_path <- file.path(report_dir, "sidra_v3_validation_report.csv")
data.table::fwrite(report, report_path)
cat("\nReport written to", report_path, "\n")

# ---- 3. allxt guard ------------------------------------------------------

if ("contribuinteprev" %in% names(fresh)) {
  cat("\ncontribuinteprev: periods =", nrow(fresh),
      " duplicated =", anyDuplicated(fresh$anomesfinaltrimmovel), "\n")
}

# ---- 4. population, end to end ------------------------------------------

# fetch_monthly_population() reads the same table 6022 series that appears
# above as `populacao`. Once that column compares identical, the monthly
# figures follow, because the moving-quarter transformation and the boundary
# extrapolation are untouched by this migration. What is worth checking here
# is that the whole chain still runs and covers the expected span.

population <- fetch_monthly_population(verbose = FALSE)

if (is.null(population)) {
  cat("\nfetch_monthly_population returned NULL\n")
} else {
  cat("\nfetch_monthly_population:", nrow(population), "months,",
      min(population$ref_month_yyyymm), "-",
      max(population$ref_month_yyyymm), "\n")
  cat("  finite values:", sum(is.finite(population$m_populacao)), "/",
      nrow(population), "\n")
}

if (any(!report$identical)) {
  quit(status = 1L)
}
