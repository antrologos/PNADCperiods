# Tests for trailing-NA propagation in mensalize_sidra_series().
#
# Regression coverage for the "phantom row" pattern: when SIDRA publishes
# a price index (IPCA/INPC) for a month before PNADC publishes the rolling
# quarter ending in that same month, fetch_sidra_rolling_quarters() returns
# rows where only the price-index columns are filled and every PNADC column
# is NA. The mensalization must propagate NA for those positions; otherwise
# .compute_cumsum_by_mesnotrim() (which intentionally treats NA as 0 to
# accommodate late-starting series like CNPJ post-201510) and the fallback
# branch of .apply_final_adjustment() collude to produce numerically
# plausible but spurious mensalized values.

# Local synthetic generator (kept here to avoid refactoring helper-test-data.R;
# pattern mirrors make_rolling_quarter_dt in test-mensalize-sidra-series.R)
make_rq_with_trailing_na <- function(start_yyyymm = 201201L,
                                     n_months = 36L,
                                     series_name = "popocup",
                                     base_value = 100000,
                                     trend = 100,
                                     n_trailing_na = 1L) {
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L

  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  dt[, (series_name) := base_value + trend * (month_num - 1) +
       500 * sin(2 * pi * month_num / 12)]

  # Inject trailing NAs in the last n_trailing_na positions
  if (n_trailing_na > 0L) {
    dt[seq.int(n_months - n_trailing_na + 1L, n_months),
       (series_name) := NA_real_]
  }

  dt[, .SD, .SDcols = c("anomesfinaltrimmovel", "mesnotrim", series_name)]
}


# =============================================================================
# Single-series: trailing NA in rq must propagate as NA in m
# =============================================================================

test_that(".mensalize_single_series propagates trailing NA from rq to m", {
  dt <- make_rq_with_trailing_na(
    start_yyyymm = 201201L, n_months = 36L,
    series_name = "popocup", base_value = 90000, trend = 100,
    n_trailing_na = 1L
  )
  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(89500, 89800, 90100)
  )

  m <- PNADCperiods:::.mensalize_single_series(dt, "popocup", sp)

  # Last position has rq=NA; mensalized output must be NA.
  expect_true(is.na(m[length(m)]),
              label = "Last m must be NA when last rq is NA")

  # All positions where rq is non-NA must have non-NA m (with valid
  # starting points and 36 months, no first_any_data masking applies).
  rq_vec <- dt[["popocup"]]
  expect_true(all(!is.na(m[!is.na(rq_vec)])),
              label = "Non-trailing positions remain non-NA")
})


test_that(".mensalize_single_series propagates multiple trailing NAs", {
  dt <- make_rq_with_trailing_na(
    start_yyyymm = 201201L, n_months = 36L,
    series_name = "popocup", base_value = 90000, trend = 100,
    n_trailing_na = 3L
  )
  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(89500, 89800, 90100)
  )

  m <- PNADCperiods:::.mensalize_single_series(dt, "popocup", sp)

  # Last 3 positions have rq=NA; all 3 mensalized values must be NA.
  n <- length(m)
  expect_true(all(is.na(m[(n - 2L):n])),
              label = "Last 3 m values must be NA when last 3 rq are NA")
})


# =============================================================================
# Split-series: trailing NA in post-split rq must propagate as NA in m
# =============================================================================

test_that(".mensalize_split_series propagates trailing NA in post-split", {
  dt <- make_rq_with_trailing_na(
    start_yyyymm = 201201L, n_months = 60L,
    series_name = "subocuphoras", base_value = 5000, trend = 10,
    n_trailing_na = 1L
  )
  sp <- data.table::data.table(
    series_name = c(rep("subocuphoras_pre", 3), rep("subocuphoras", 3)),
    mesnotrim = c(1:3, 1:3),
    y0 = c(4800, 4900, 5000, 5400, 5500, 5600)
  )

  m <- PNADCperiods:::.mensalize_split_series(
    dt, "subocuphoras", sp, split_month = 201509L
  )

  # Last position is in post-split (201601 onward) with rq=NA;
  # mensalized must be NA.
  expect_true(is.na(m[length(m)]),
              label = "Last m must be NA when last post-split rq is NA")
})


# =============================================================================
# Full pipeline: derived series must be NA when primaries are NA on trailing
# =============================================================================

test_that("mensalize_sidra_series produces NA derived when primaries trail-NA", {
  # Build a multi-series RQ with trailing NA on both popocup and popdesocup
  # so that the derived popnaforca = popocup + popdesocup and rate
  # taxadesocup = popdesocup/popnaforca*100 receive NA inputs.
  start_yyyymm <- 201201L
  n_months <- 36L

  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  dt[, popocup := 90000 + 100 * (month_num - 1)]
  dt[, popdesocup := 10000 + 30 * (month_num - 1)]
  # Inject trailing NA in last position for both primaries
  dt[n_months, `:=`(popocup = NA_real_, popdesocup = NA_real_)]
  dt <- dt[, .(anomesfinaltrimmovel, mesnotrim, popocup, popdesocup)]

  sp <- data.table::data.table(
    series_name = c(rep("popocup", 3), rep("popdesocup", 3)),
    mesnotrim = rep(1:3, 2),
    y0 = c(89500, 89800, 90100, 9800, 9900, 10000)
  )

  out <- mensalize_sidra_series(dt, starting_points = sp,
                                 series = c("popocup", "popdesocup"),
                                 compute_derived = TRUE,
                                 verbose = FALSE)

  last_row <- out[anomesexato == max(anomesexato)]
  # Primaries must be NA
  expect_true(is.na(last_row$m_popocup),
              label = "m_popocup is NA on trailing")
  expect_true(is.na(last_row$m_popdesocup),
              label = "m_popdesocup is NA on trailing")
  # Derived rate: NA (or NaN — both are detected by is.na in R)
  if ("m_taxadesocup" %in% names(last_row)) {
    expect_true(is.na(last_row$m_taxadesocup),
                label = "m_taxadesocup is NA/NaN on trailing")
  }
})
