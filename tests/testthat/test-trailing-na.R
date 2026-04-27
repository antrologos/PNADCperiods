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

test_that("leading NA before first observed rq is reconstructed via y0 (regression guard)", {
  # The leading positions for which no rolling quarter ever existed
  # (e.g., 201201 = mesnotrim 1, 201202 = mesnotrim 2 — before the first
  # rolling quarter ending in 201203) MUST receive a non-NA mensalized
  # value derived from y0 + apply_final_adjustment lookahead. A previous
  # blanket fix `m[is.na(rq)] <- NA_real_` regressed this behaviour and
  # produced NA; this test guards against re-introducing that bug.
  start_yyyymm <- 201201L
  n_months <- 24L
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  # Series starts ONLY at month 3 (mesnotrim=3) — first 2 are NA, like
  # IPCA-only leading rows for 201201/201202.
  dt[, popocup := 90000 + 100 * (month_num - 1)]
  dt[1L:2L, popocup := NA_real_]
  dt <- dt[, .(anomesfinaltrimmovel, mesnotrim, popocup)]

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(89500, 89800, 90100)
  )

  m <- PNADCperiods:::.mensalize_single_series(dt, "popocup", sp)

  # Leading positions (1, 2) MUST be non-NA (reconstructed from y0).
  expect_false(is.na(m[1L]),
               label = "Leading position 1 (rq=NA) reconstructed from y0")
  expect_false(is.na(m[2L]),
               label = "Leading position 2 (rq=NA) reconstructed from y0")
  # And they must be reasonably close to y0 (sanity check; exact value
  # depends on apply_final_adjustment's lookahead computation).
  expect_true(abs(m[1L] - 89500) < 10000,
              label = "Leading m[1] is in the y0 ballpark")
  expect_true(abs(m[2L] - 89800) < 10000,
              label = "Leading m[2] is in the y0 ballpark")
})


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


# =============================================================================


# =============================================================================
# F1 Solution Tests - Corrected
# =============================================================================

test_that("F1: Pre-computed IPCA lag is available in result", {
  # Create synthetic rolling quarter data with IPCA from pre-PNADC era
  start_yyyymm <- 201101L
  n_months <- 24L
  
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  
  # IPCA from pre-PNADC
  dt[, ipca100dez1993 := 100 + 0.3 * (month_num - 1)]
  
  # PNADC series from 201201 onward
  dt[, massaefetnominaltodos := NA_real_]
  dt[3:n_months, massaefetnominaltodos := 1000 + 100 * (month_num - 3)]
  
  dt <- dt[, .(anomesfinaltrimmovel, mesnotrim, ipca100dez1993, massaefetnominaltodos)]
  
  sp <- data.table::data.table(
    series_name = "massaefetnominaltodos",
    mesnotrim = 1,
    y0 = 1000
  )
  
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = "massaefetnominaltodos",
    compute_derived = TRUE,
    verbose = FALSE
  )
  
  # Check that ipca100dez1993_lagged was copied to result
  expect_true("ipca100dez1993_lagged" %in% names(result),
              label = "ipca100dez1993_lagged column exists in result")
  
  # 201201 should have a lagged IPCA value (from 201112)
  row_201201 <- result[anomesexato == 201201L]
  expect_false(is.na(row_201201$ipca100dez1993_lagged),
               label = "ipca100dez1993_lagged is non-NA in 201201 (pre-PNADC available)")
})


test_that("F1: Deflated efet series is non-NA in 201201 with pre-PNADC IPCA lag", {
  # Test that the actual deflated series are computed correctly in 201201
  # when pre-PNADC IPCA lag is available (F1 solution)
  
  start_yyyymm <- 201101L
  n_months <- 24L
  
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  
  dt[, ipca100dez1993 := 100 + 0.3 * (month_num - 1)]
  
  # For deflated series, we need BOTH hab and efet nominal series
  dt[, massahabnominaltodos := NA_real_]
  dt[, massaefetnominaltodos := NA_real_]
  dt[3:n_months, `:=`(
    massahabnominaltodos = 1000 + 100 * (month_num - 3),
    massaefetnominaltodos = 1000 + 100 * (month_num - 3)
  )]
  
  dt <- dt[, .(anomesfinaltrimmovel, mesnotrim, ipca100dez1993, 
               massahabnominaltodos, massaefetnominaltodos)]
  
  sp <- data.table::data.table(
    series_name = c(rep("massahabnominaltodos", 3), rep("massaefetnominaltodos", 3)),
    mesnotrim = c(1:3, 1:3),
    y0 = c(1000, 1000, 1000, 1000, 1000, 1000)
  )
  
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = c("massahabnominaltodos", "massaefetnominaltodos"),
    compute_derived = TRUE,
    verbose = FALSE
  )
  
  row_201201 <- result[anomesexato == 201201L]
  
  # hab deflated (uses current IPCA, no lag)
  expect_false(is.na(row_201201$m_massahabtodosipcabr),
               label = "m_massahabtodosipcabr is non-NA in 201201 (hab, no lag needed)")
  
  # efet deflated (uses lagged IPCA - the F1 fix)
  # BEFORE F1, this would be NA
  # AFTER F1, this should be non-NA because lagged IPCA from pre-PNADC is available
  expect_false(is.na(row_201201$m_massaefettodosipcabr),
               label = "m_massaefettodosipcabr is non-NA in 201201 (efet, with F1 lag available)")
})
