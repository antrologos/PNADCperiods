# Tests for the pure-cumsum mensalization rewrite (v0.1.3+).
#
# Background: in 0.1.2 and earlier, `mensalize_sidra_series()` applied a final
# adjustment that re-anchored each (Jan, Feb, Mar)-style trio to its
# rolling-quarter mean. That adjustment couples the three mesnotrim sub-series
# via `rq_lead`, so when a new end-of-quarter rq is published, all three
# months in the trio shift by the same delta -- producing retroactive changes
# in already-published months. See investigation:
# C:/Users/antro/.claude/plans/diagnostico_marcos_toys_output.txt
#
# In v0.1.3, the algorithm stops at step 4 (`y = y0 + cum`) and returns three
# strictly independent month-position sub-series. Adding a new rolling
# quarter never alters previously computed monthly values.

# =============================================================================
# HELPERS
# =============================================================================

# Build a rolling_quarters data.table where rolling quarters are derived by
# definition from a known monthly series.
make_rq_from_monthly <- function(m_true, start_yyyymm = 201201L,
                                 series_name = "popocup") {
  n <- length(m_true)
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L

  yyyymm <- integer(n)
  mesnotrim <- integer(n)
  for (i in seq_len(n)) {
    yr <- start_year + (start_month + i - 2L) %/% 12L
    mo <- ((start_month + i - 2L) %% 12L) + 1L
    yyyymm[i] <- yr * 100L + mo
    mesnotrim[i] <- ((mo - 1L) %% 3L) + 1L
  }

  rq <- rep(NA_real_, n)
  for (t in 3:n) rq[t] <- mean(m_true[(t-2):t])

  dt <- data.table::data.table(
    anomesfinaltrimmovel = yyyymm,
    mesnotrim = as.integer(mesnotrim)
  )
  dt[, (series_name) := rq]
  dt
}


make_starting_points <- function(m_true_first_three, series_name = "popocup") {
  data.table::data.table(
    series_name = rep(series_name, 3),
    mesnotrim = 1:3,
    y0 = m_true_first_three
  )
}


# =============================================================================
# CORE INVARIANCE TESTS
# =============================================================================

test_that("pure cumsum recovers m_true exactly when y0 matches first 3 months", {
  m_true <- c(100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155)
  dt <- make_rq_from_monthly(m_true)
  sp <- make_starting_points(m_true[1:3])

  res <- mensalize_sidra_series(
    rolling_quarters = dt,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  expect_equal(res$m_popocup, m_true, tolerance = 1e-10)
})


test_that("pure cumsum is invariant when a new rolling quarter is appended", {
  # Critical regression test for the issue Marcos reported in 2026-04:
  # adding a new IBGE rolling quarter (e.g., the one ending in March)
  # must not alter previously computed monthly values for January and
  # February of the same trio.
  m_true <- c(100, 105, 110, 115, 120, 125, 130, 135, 140)

  dt_full <- make_rq_from_monthly(m_true)
  sp <- make_starting_points(m_true[1:3])

  res_A <- mensalize_sidra_series(dt_full[1:6], starting_points = sp,
                                  series = "popocup",
                                  compute_derived = FALSE, verbose = FALSE)
  res_B <- mensalize_sidra_series(dt_full, starting_points = sp,
                                  series = "popocup",
                                  compute_derived = FALSE, verbose = FALSE)

  m_A <- res_A$m_popocup
  m_B <- res_B[anomesexato %in% res_A$anomesexato, m_popocup]
  expect_equal(m_B, m_A, tolerance = 1e-12,
               label = "previously published months must not change when new rq is added")
})


test_that("invariance holds even when y0 is inconsistent with rq", {
  # When y0 does not match m_true[1:3], pure cumsum produces a series that
  # is m_true shifted by a constant per mesnotrim. Crucially, adding new
  # rolling quarters still does not retroactively shift earlier months.
  m_true <- c(100, 105, 110, 115, 120, 125, 130, 135, 140)
  dt_full <- make_rq_from_monthly(m_true)
  sp <- make_starting_points(c(95, 100, 105))  # offset by -5

  res_A <- mensalize_sidra_series(dt_full[1:6], starting_points = sp,
                                  series = "popocup",
                                  compute_derived = FALSE, verbose = FALSE)
  res_B <- mensalize_sidra_series(dt_full, starting_points = sp,
                                  series = "popocup",
                                  compute_derived = FALSE, verbose = FALSE)

  m_A <- res_A$m_popocup
  m_B <- res_B[anomesexato %in% res_A$anomesexato, m_popocup]
  expect_equal(m_B, m_A, tolerance = 1e-12,
               label = "invariance must hold under inconsistent y0")

  # And the offset is exactly -5 in every month
  expect_equal(res_B$m_popocup, m_true - 5, tolerance = 1e-10)
})


test_that("pure cumsum satisfies the d3 identity m[t]-m[t-3]=3*(rq[t]-rq[t-1])", {
  # This is the algebraic identity that the cumsum step is built on:
  # 3 * rq[t]   = m[t-2] + m[t-1] + m[t]
  # 3 * rq[t-1] = m[t-3] + m[t-2] + m[t-1]
  # subtracting: 3 * (rq[t] - rq[t-1]) = m[t] - m[t-3]
  m_true <- c(100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155)
  dt <- make_rq_from_monthly(m_true)
  sp <- make_starting_points(m_true[1:3])

  res <- mensalize_sidra_series(dt, starting_points = sp,
                                series = "popocup",
                                compute_derived = FALSE, verbose = FALSE)

  rq <- dt$popocup
  m  <- res$m_popocup

  for (t in 4:length(m)) {
    expect_equal(m[t] - m[t-3], 3 * (rq[t] - rq[t-1]), tolerance = 1e-10,
                 label = paste0("d3 identity at t=", t))
  }
})


test_that("trailing NA in rq still masks only the last (3rd-month) position", {
  # Mirrors the 2026-04-29 scenario: rq for the last month (mesnotrim=3) is
  # not yet published. Pure cumsum should produce values for the two earlier
  # months in the trio (mesnotrim=1 and 2) and NA only for the trailing one.
  m_true <- c(100, 105, 110, 115, 120, 125, 130, 135, 140)
  dt <- make_rq_from_monthly(m_true)
  sp <- make_starting_points(m_true[1:3])

  # Set last rq (mesnotrim=3) to NA, simulating "not yet published"
  dt[.N, popocup := NA_real_]

  res <- mensalize_sidra_series(dt, starting_points = sp,
                                series = "popocup",
                                compute_derived = FALSE, verbose = FALSE)

  # Earlier months (positions 1..8) should have valid values that match
  # what they would be if rq were complete (i.e., m_true).
  expect_equal(res$m_popocup[1:8], m_true[1:8], tolerance = 1e-10)
  # The trailing month (position 9, mesnotrim=3, rq=NA) is masked
  expect_true(is.na(res$m_popocup[9]))
})