# Tests for mensalize-sidra-series.R
# Covers the exported mensalize_sidra_series() function and its internal helpers:
#   .compute_cumsum_by_mesnotrim(), .extract_y0_vector(),
#   .apply_final_adjustment(), .mensalize_single_series(), .mensalize_split_series()
#
# All tests use synthetic mock data (no API calls).

# =============================================================================
# HELPER: generate proper YYYYMM sequence and mesnotrim
# =============================================================================

make_rolling_quarter_dt <- function(start_yyyymm = 201201L, n_months = 36L,
                                    series_name = "popocup",
                                    base_value = 100000, trend = 100) {

  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L

  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]

  # Deterministic series: base + linear trend + small seasonal component
  dt[, (series_name) := base_value + trend * (month_num - 1) +
       500 * sin(2 * pi * month_num / 12)]

  dt[, .SD, .SDcols = c("anomesfinaltrimmovel", "mesnotrim", series_name)]
}


# =============================================================================
# .compute_cumsum_by_mesnotrim() TESTS
# =============================================================================

test_that(".compute_cumsum_by_mesnotrim computes d3 correctly for simple case", {
  # 1. Setup: 6 months of constant rolling quarters (no change expected)
  rq <- c(300, 300, 300, 300, 300, 300)
  mesnotrim <- c(1L, 2L, 3L, 1L, 2L, 3L)

  # 2. Execute
  cum <- PNADCperiods:::.compute_cumsum_by_mesnotrim(rq, mesnotrim)

  # 3. Verify: d3 = 3*(RQ_t - RQ_{t-1}) = 0 for all interior,
  #    first of each mesnotrim position is set to NA (d3=0 for cumsum)

  #    All cumsums should be 0 since no variation exists
  expect_equal(cum[4], 0, label = "Cumsum for second pos1 should be 0 (no variation)")
  expect_equal(cum[5], 0, label = "Cumsum for second pos2 should be 0 (no variation)")
  expect_equal(cum[6], 0, label = "Cumsum for second pos3 should be 0 (no variation)")
})


test_that(".compute_cumsum_by_mesnotrim separates 3 mesnotrim positions", {
  # 1. Setup: Different step sizes at each month position
  #    RQ sequence: 100, 200, 300, 110, 210, 310, 120, 220, 320
  rq <- c(100, 200, 300, 110, 210, 310, 120, 220, 320)
  mesnotrim <- c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L)

  # 2. Execute
  cum <- PNADCperiods:::.compute_cumsum_by_mesnotrim(rq, mesnotrim)

  # 3. Verify: Each position accumulates independently
  #    For pos1: first d3 = NA (forced 0), d3[4] = 3*(110-300) = -570, d3[7] = 3*(120-310) = -570
  #    For pos2: first d3 = NA (forced 0), d3[5] = 3*(210-110) = 300, d3[8] = 3*(220-120) = 300
  #    For pos3: first d3 = NA (forced 0), d3[6] = 3*(310-210) = 300, d3[9] = 3*(320-220) = 300
  # Pos1 cumsums:
  expect_equal(cum[1], 0, label = "First pos1 cumsum is 0")
  # Pos2 cumsums:
  expect_equal(cum[2], 0, label = "First pos2 cumsum is 0")
  # Pos3 cumsums:
  expect_equal(cum[3], 0, label = "First pos3 cumsum is 0")

  # The second occurrence of each position should have the correct d3 accumulated
  # pos1: cum[4] = d3 at index 4 = 3*(110 - 300) = -570
  expect_equal(cum[4], -570, label = "Second pos1 d3 accumulated correctly")
  # pos2: cum[5] = d3 at index 5 = 3*(210 - 110) = 300
  expect_equal(cum[5], 300, label = "Second pos2 d3 accumulated correctly")
  # pos3: cum[6] = d3 at index 6 = 3*(310 - 210) = 300
  expect_equal(cum[6], 300, label = "Second pos3 d3 accumulated correctly")
})


test_that(".compute_cumsum_by_mesnotrim first obs of each position starts at 0", {
  # 1. Setup: 9 months with non-trivial values
  rq <- c(1000, 2000, 3000, 1100, 2100, 3100, 1200, 2200, 3200)
  mesnotrim <- c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L)

  # 2. Execute
  cum <- PNADCperiods:::.compute_cumsum_by_mesnotrim(rq, mesnotrim)

  # 3. Verify: First occurrence of each position = 0
  expect_equal(cum[1], 0, label = "First pos1 starts at 0")
  expect_equal(cum[2], 0, label = "First pos2 starts at 0")
  expect_equal(cum[3], 0, label = "First pos3 starts at 0")
})


test_that(".compute_cumsum_by_mesnotrim respects filter_mask", {
  # 1. Setup: 9 months but mask out the middle 3
  rq <- c(100, 200, 300, 110, 210, 310, 120, 220, 320)
  mesnotrim <- c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L)
  mask <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)

  # 2. Execute
  cum <- PNADCperiods:::.compute_cumsum_by_mesnotrim(rq, mesnotrim, filter_mask = mask)

  # 3. Verify: d3 at masked positions (4,5,6) treated as 0
  #    pos1: d3[1]=0, d3[4]=0(masked), d3[7] = 3*(120-310) = -570
  #    So cum[7] = -570 (only one unmasked step)
  #    Compare to unmasked: cum[7] would be -570 + (-570) = -1140
  cum_unmasked <- PNADCperiods:::.compute_cumsum_by_mesnotrim(rq, mesnotrim)
  expect_true(cum[7] != cum_unmasked[7],
              label = "Filtered cumsum differs from unfiltered")
})


test_that(".compute_cumsum_by_mesnotrim handles all-NA input", {
  # 1. Setup: All NA rolling quarter values
  rq <- rep(NA_real_, 9)
  mesnotrim <- c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L)

  # 2. Execute
  cum <- PNADCperiods:::.compute_cumsum_by_mesnotrim(rq, mesnotrim)

  # 3. Verify: All cumsums should be NA (no data to cumulate)
  expect_true(all(is.na(cum)),
              label = "All-NA input produces all-NA cumsum")
})


# =============================================================================
# .extract_y0_vector() TESTS
# =============================================================================

test_that(".extract_y0_vector returns 3 values for found series", {
  # 1. Setup: Starting points table with 3 positions for one series
  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(90000, 91000, 92000)
  )

  # 2. Execute
  y0 <- PNADCperiods:::.extract_y0_vector(sp, "popocup")

  # 3. Verify: 3-element vector with correct values

  expect_length(y0, 3)
  expect_equal(y0[1], 90000)
  expect_equal(y0[2], 91000)
  expect_equal(y0[3], 92000)
})


test_that(".extract_y0_vector returns 3 NAs for missing series", {
  # 1. Setup: Starting points table without the target series
  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(90000, 91000, 92000)
  )

  # 2. Execute
  y0 <- PNADCperiods:::.extract_y0_vector(sp, "popdesocup")

  # 3. Verify: 3 NAs when series not found
  expect_length(y0, 3)
  expect_true(all(is.na(y0)),
              label = "Missing series returns c(NA, NA, NA)")
})


test_that(".extract_y0_vector handles partial starting points", {
  # 1. Setup: Starting points with only 2 of 3 positions
  sp <- data.table::data.table(
    series_name = c("popocup", "popocup"),
    mesnotrim = c(1L, 3L),
    y0 = c(90000, 92000)
  )

  # 2. Execute
  y0 <- PNADCperiods:::.extract_y0_vector(sp, "popocup")

  # 3. Verify: Position 2 is NA, others filled
  expect_length(y0, 3)
  expect_equal(y0[1], 90000)
  expect_true(is.na(y0[2]), label = "Missing position 2 is NA")
  expect_equal(y0[3], 92000)
})


# =============================================================================
# .apply_final_adjustment() TESTS
# =============================================================================

test_that(".apply_final_adjustment preserves rolling quarter consistency", {
  # 1. Setup: 12 months where mean(m[t], m[t+1], m[t+2]) should == rq[t+2]
  #    Use known y values and rq values
  n <- 12
  mesnotrim <- rep(1:3, 4)
  # Create y values that differ from rq (final adjustment corrects them)
  y <- c(100, 200, 300, 110, 210, 310, 120, 220, 320, 130, 230, 330)
  # rq values: rolling quarter = average of 3 consecutive months
  # We want rq[3] = mean(m[1],m[2],m[3]), rq[4] = mean(m[2],m[3],m[4]), etc.
  # For testing, set rq = average of y with small shift
  rq <- c(NA, NA, mean(y[1:3]) + 5,
          mean(y[2:4]) + 3, mean(y[3:5]) + 2, mean(y[4:6]) + 1,
          mean(y[5:7]) + 4, mean(y[6:8]) + 2, mean(y[7:9]) + 3,
          mean(y[8:10]) + 1, mean(y[9:11]) + 5, mean(y[10:12]) + 2)

  # 2. Execute
  m <- PNADCperiods:::.apply_final_adjustment(y, rq, mesnotrim)

  # 3. Verify: For mesnotrim==3 positions where all neighbors are available,
  #    mean(m[i-2], m[i-1], m[i]) should equal rq[i]
  for (i in seq(3, 12, by = 3)) {
    if (i >= 3 && !is.na(rq[i])) {
      trio_mean <- mean(m[(i - 2):i])
      expect_equal(trio_mean, rq[i], tolerance = 1e-10,
                   label = paste0("Rolling quarter consistency at position ", i))
    }
  }
})


test_that(".apply_final_adjustment falls back to y at boundaries", {
  # 1. Setup: Short sequence where lead/lag values are missing
  y <- c(100, 200, 300)
  rq <- c(NA, NA, 200)
  mesnotrim <- c(1L, 2L, 3L)

  # 2. Execute
  m <- PNADCperiods:::.apply_final_adjustment(y, rq, mesnotrim)

  # 3. Verify: mesnotrim==1 cannot look ahead 2 positions beyond length
  #    (y_lead1 and y_lead2 will be NA for last elements),
  #    mesnotrim==1 at position 1: needs y_lead1[1], y_lead2[1] which exist
  #    mesnotrim==3 at position 3: needs y_lag1 and y_lag2 which exist
  expect_length(m, 3)
  # For this 3-element case, all trios are complete
  # pos1 (i=1): valid1 = needs y[1], y_lead1=y[2], y_lead2=y[3], rq_lead2=rq[3]
  # All available, so adjustment is applied
  expect_false(is.na(m[1]), label = "Position 1 gets adjusted value (complete trio)")
  expect_false(is.na(m[3]), label = "Position 3 gets adjusted value (complete trio)")
})


test_that(".apply_final_adjustment works with 12-month sequence", {
  # 1. Setup: 12 months with linearly increasing rq
  mesnotrim <- rep(1:3, 4)
  rq <- seq(1000, 1110, by = 10)  # 1000, 1010, ..., 1110
  # Set y to be rq itself (adjustment should be minimal when y is already close)
  y <- rq

  # 2. Execute
  m <- PNADCperiods:::.apply_final_adjustment(y, rq, mesnotrim)

  # 3. Verify: For complete trios at mesnotrim==3 positions,
  #    the rolling quarter relationship holds
  # At i=3: rq[3] should == mean(m[1], m[2], m[3])
  expect_equal(mean(m[1:3]), rq[3], tolerance = 1e-10,
               label = "First complete trio matches rq")
  # At i=6: rq[6] should == mean(m[4], m[5], m[6])
  expect_equal(mean(m[4:6]), rq[6], tolerance = 1e-10,
               label = "Second complete trio matches rq")
  # At i=9: rq[9] should == mean(m[7], m[8], m[9])
  expect_equal(mean(m[7:9]), rq[9], tolerance = 1e-10,
               label = "Third complete trio matches rq")
})


# =============================================================================
# .mensalize_single_series() TESTS
# =============================================================================

test_that(".mensalize_single_series produces standard output", {
  # 1. Setup: Build a data.table with 36 months of rolling quarter data
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 36L,
    series_name = "popocup", base_value = 90000, trend = 100
  )

  # Create starting points for this series
  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(89500, 89800, 90100)
  )

  # 2. Execute
  m_values <- PNADCperiods:::.mensalize_single_series(dt, "popocup", sp)

  # 3. Verify: Output is numeric vector with same length as input
  expect_length(m_values, nrow(dt))
  expect_type(m_values, "double")
  # Non-NA values should exist for most of the series (at least after first few)
  # With 36 months and valid starting points, all values should be non-NA
  # (cumsum produces 0 at first positions, y0 is non-NA, so y is non-NA everywhere)
  expect_true(sum(!is.na(m_values)) > nrow(dt) * 0.5,
              label = "Most values should be non-NA")
})


test_that(".mensalize_single_series returns all NA when series not in starting_points", {
  # 1. Setup: Data has series "popocup" but starting points are for a different series
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 12L,
    series_name = "popocup", base_value = 90000
  )

  sp <- data.table::data.table(
    series_name = rep("popdesocup", 3),
    mesnotrim = 1:3,
    y0 = c(10000, 10500, 11000)
  )

  # 2. Execute
  m_values <- PNADCperiods:::.mensalize_single_series(dt, "popocup", sp)

  # 3. Verify: y0 is c(NA,NA,NA), so y = NA + cum => all NA
  expect_true(all(is.na(m_values)),
              label = "Missing starting points produce all-NA output")
})


# =============================================================================
# .mensalize_split_series() TESTS
# =============================================================================

test_that(".mensalize_split_series handles spanning split correctly", {
  # 1. Setup: Series spanning from 201201 to 201612 (5 years, crosses 201509 split)
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 60L,
    series_name = "subocuphoras", base_value = 5000, trend = 10
  )

  # Starting points for pre-split and post-split
  sp <- data.table::data.table(
    series_name = c(rep("subocuphoras_pre", 3), rep("subocuphoras", 3)),
    mesnotrim = c(1:3, 1:3),
    y0 = c(4800, 4900, 5000, 5400, 5500, 5600)
  )

  # 2. Execute
  m_values <- PNADCperiods:::.mensalize_split_series(
    dt, "subocuphoras", sp, split_month = 201509L
  )

  # 3. Verify: Output covers full range
  expect_length(m_values, nrow(dt))
  expect_type(m_values, "double")

  # Pre-split values (up to and including 201509) should be mostly non-NA
  # With valid starting points, all values in each segment should be non-NA
  pre_idx <- which(dt$anomesfinaltrimmovel <= 201509L)
  expect_true(sum(!is.na(m_values[pre_idx])) > length(pre_idx) * 0.5,
              label = "Pre-split values mostly non-NA")

  # Post-split values (after 201509) should be mostly non-NA
  post_idx <- which(dt$anomesfinaltrimmovel > 201509L)
  expect_true(sum(!is.na(m_values[post_idx])) > length(post_idx) * 0.5,
              label = "Post-split values mostly non-NA")
})


test_that(".mensalize_split_series handles data entirely before split", {
  # 1. Setup: Series ending before the split point
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 36L,  # ends Dec 2014, before split
    series_name = "subocuphoras", base_value = 5000, trend = 10
  )

  sp <- data.table::data.table(
    series_name = c(rep("subocuphoras_pre", 3), rep("subocuphoras", 3)),
    mesnotrim = c(1:3, 1:3),
    y0 = c(4800, 4900, 5000, 5400, 5500, 5600)
  )

  # 2. Execute
  m_values <- PNADCperiods:::.mensalize_split_series(
    dt, "subocuphoras", sp, split_month = 201509L
  )

  # 3. Verify: Only pre-split logic is used
  expect_length(m_values, nrow(dt))
  # Should produce values using pre-split starting points
  expect_true(sum(!is.na(m_values)) > nrow(dt) * 0.5,
              label = "Before-split data fully processed")
})


test_that(".mensalize_split_series handles data entirely after split", {
  # 1. Setup: Series starting after the split point
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201601L, n_months = 24L,
    series_name = "subocuphoras", base_value = 5500, trend = 10
  )

  sp <- data.table::data.table(
    series_name = c(rep("subocuphoras_pre", 3), rep("subocuphoras", 3)),
    mesnotrim = c(1:3, 1:3),
    y0 = c(4800, 4900, 5000, 5400, 5500, 5600)
  )

  # 2. Execute
  m_values <- PNADCperiods:::.mensalize_split_series(
    dt, "subocuphoras", sp, split_month = 201509L
  )

  # 3. Verify: Only post-split logic is used
  expect_length(m_values, nrow(dt))
  expect_true(sum(!is.na(m_values)) > nrow(dt) * 0.5,
              label = "After-split data fully processed")
})


test_that(".mensalize_split_series resets cumsum at split boundary", {
  # 1. Setup: Data spanning the split with a jump in values
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201501L, n_months = 24L,  # Jan 2015 - Dec 2016
    series_name = "subocuphoras", base_value = 5000, trend = 10
  )
  # Introduce a level shift after the split to test independence
  post_rows <- dt$anomesfinaltrimmovel > 201509L
  data.table::set(dt, i = which(post_rows), j = "subocuphoras",
                  value = dt[["subocuphoras"]][post_rows] + 2000)

  sp <- data.table::data.table(
    series_name = c(rep("subocuphoras_pre", 3), rep("subocuphoras", 3)),
    mesnotrim = c(1:3, 1:3),
    y0 = c(4800, 4900, 5000, 7400, 7500, 7600)
  )

  # 2. Execute
  m_values <- PNADCperiods:::.mensalize_split_series(
    dt, "subocuphoras", sp, split_month = 201509L
  )

  # 3. Verify: Post-split values should reflect the level shift
  #    Pre-split values should be around 5000 level
  #    Post-split values should be around 7000+ level
  pre_mean <- mean(m_values[!post_rows], na.rm = TRUE)
  post_mean <- mean(m_values[post_rows], na.rm = TRUE)
  expect_true(post_mean > pre_mean + 1000,
              label = "Post-split reflects level shift (cumsums reset)")
})


# =============================================================================
# mensalize_sidra_series() EXPORTED FUNCTION TESTS
# =============================================================================

test_that("mensalize_sidra_series errors on missing anomesfinaltrimmovel", {
  # 1. Setup: data.table without the required column
  dt <- data.table::data.table(
    mesnotrim = c(1L, 2L, 3L),
    popocup = c(100, 200, 300)
  )

  # 2. Verify: Error about missing column
  expect_error(
    mensalize_sidra_series(dt, verbose = FALSE),
    "Missing required columns.*anomesfinaltrimmovel",
    label = "Missing anomesfinaltrimmovel raises error"
  )
})


test_that("mensalize_sidra_series errors on missing mesnotrim", {
  # 1. Setup: data.table without mesnotrim
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L),
    popocup = c(100, 200, 300)
  )

  # 2. Verify: Error about missing column
  expect_error(
    mensalize_sidra_series(dt, verbose = FALSE),
    "Missing required columns.*mesnotrim",
    label = "Missing mesnotrim raises error"
  )
})


test_that("mensalize_sidra_series auto-converts data.frame to data.table", {
  # 1. Setup: plain data.frame input with starting points
  n <- 36L
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = n, series_name = "popocup"
  )
  df <- as.data.frame(dt)  # convert to plain data.frame

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute: Should not error
  expect_no_error({
    result <- mensalize_sidra_series(
      df,
      starting_points = sp,
      series = "popocup",
      compute_derived = FALSE,
      verbose = FALSE
    )
  })

  # 3. Verify: Result is data.table with expected columns
  result <- mensalize_sidra_series(
    df,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )
  expect_s3_class(result, "data.table")
  expect_true("m_popocup" %in% names(result))
})


test_that("mensalize_sidra_series respects specific series selection", {
  # 1. Setup: Data with multiple series
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 36L, series_name = "popocup"
  )
  # Add additional series columns using data.table::set to avoid scoping issues
  data.table::set(dt, j = "popdesocup",
                  value = dt[["popocup"]] * 0.1 + rnorm(nrow(dt), 0, 50))
  data.table::set(dt, j = "popnaforca",
                  value = dt[["popocup"]] + dt[["popdesocup"]])

  sp <- data.table::data.table(
    series_name = c(rep("popocup", 3), rep("popdesocup", 3), rep("popnaforca", 3)),
    mesnotrim = rep(1:3, 3),
    y0 = c(99000, 99500, 100000, 9000, 9500, 10000, 108000, 109000, 110000)
  )

  # 2. Execute: Select only one series
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Only selected series in output
  expect_true("m_popocup" %in% names(result))
  expect_false("m_popdesocup" %in% names(result),
               label = "Non-selected series excluded")
  expect_false("m_popnaforca" %in% names(result),
               label = "Non-selected series excluded")
})


test_that("mensalize_sidra_series errors on invalid series name", {
  # 1. Setup
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 12L, series_name = "popocup"
  )

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Verify: Invalid series name triggers error
  expect_error(
    mensalize_sidra_series(
      dt,
      starting_points = sp,
      series = "nonexistent_series",
      verbose = FALSE
    ),
    "Series not found",
    label = "Invalid series name raises error"
  )
})


test_that("mensalize_sidra_series compute_derived=FALSE omits derived columns", {
  # 1. Setup: Provide two component series that normally produce a derived aggregate
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 36L, series_name = "popocup"
  )
  data.table::set(dt, j = "popdesocup", value = dt[["popocup"]] * 0.1)

  sp <- data.table::data.table(
    series_name = c(rep("popocup", 3), rep("popdesocup", 3)),
    mesnotrim = rep(1:3, 2),
    y0 = c(99000, 99500, 100000, 9000, 9500, 10000)
  )

  # 2. Execute: compute_derived = FALSE
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: No derived series like m_popnaforca (= popocup + popdesocup)
  expect_false("m_popnaforca" %in% names(result),
               label = "Derived aggregate not computed when compute_derived=FALSE")
  # Primary series should still be there
  expect_true("m_popocup" %in% names(result))
  expect_true("m_popdesocup" %in% names(result))
})


test_that("mensalize_sidra_series output has anomesexato as first column", {
  # 1. Setup
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 24L, series_name = "popocup"
  )

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: anomesexato is the first column
  expect_equal(names(result)[1], "anomesexato",
               label = "anomesexato is first column of output")
})


test_that("mensalize_sidra_series filters pre-PNADC rows", {
  # 1. Setup: Data starting before PNADC era (before 201201)
  # Build manually to include pre-2012 data
  dt_pre <- data.table::data.table(
    anomesfinaltrimmovel = c(201101L, 201102L, 201103L),
    mesnotrim = c(1L, 2L, 3L),
    popocup = c(80000, 80100, 80200)
  )
  dt_post <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 12L, series_name = "popocup"
  )
  dt <- data.table::rbindlist(list(dt_pre, dt_post))

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: No pre-2012 rows in output
  expect_true(min(result$anomesexato) >= 201201L,
              label = "Pre-PNADC rows (before 201201) filtered out")
  expect_equal(nrow(result), 12L,
               label = "Only PNADC-era rows remain")
})


test_that("mensalize_sidra_series accepts custom starting_points", {
  # 1. Setup
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 24L, series_name = "popocup"
  )

  # Custom starting points with specific values
  sp_custom <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(95000, 96000, 97000)
  )

  # 2. Execute: Should use our custom starting points without error
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp_custom,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Result produced without trying to load bundled data
  expect_s3_class(result, "data.table")
  expect_true("m_popocup" %in% names(result))
  expect_true(any(!is.na(result$m_popocup)),
              label = "Custom starting points produce non-NA values")
})


test_that("mensalize_sidra_series errors on wrong starting_points format", {
  # 1. Setup: Starting points missing required column
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 12L, series_name = "popocup"
  )

  # Missing 'y0' column
  sp_bad <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    value = c(99000, 99500, 100000)  # wrong column name
  )

  # 2. Verify: Error about missing columns
  expect_error(
    mensalize_sidra_series(dt, starting_points = sp_bad, verbose = FALSE),
    "Starting points missing columns.*y0",
    label = "Malformed starting_points raises error"
  )
})


test_that("mensalize_sidra_series passes through price index columns", {
  # 1. Setup: Data with price index column alongside a regular series
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 24L, series_name = "popocup"
  )
  # Add a price index column (these are not mensalized, just passed through)
  data.table::set(dt, j = "ipca100dez1993",
                  value = seq(100, by = 0.5, length.out = nrow(dt)))

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Price index column present (no m_ prefix, no mensalization)
  expect_true("ipca100dez1993" %in% names(result),
              label = "Price index column passed through")
  # Verify values are the same as input (no transformation)
  expect_equal(result$ipca100dez1993, dt$ipca100dez1993,
               label = "Price index values unchanged")
})


# =============================================================================
# ADDITIONAL PROPERTY-BASED AND EDGE CASE TESTS
# =============================================================================

test_that("mensalize_sidra_series excludes rate series from direct mensalization", {
  # 1. Setup: Data containing a rate series (should be excluded from mensalization)
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 24L, series_name = "popocup"
  )
  data.table::set(dt, j = "taxadesocup", value = runif(nrow(dt), 5, 15))

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute: Request "all" series (default)
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Rate series is filtered out, not directly mensalized
  expect_false("m_taxadesocup" %in% names(result),
               label = "Rate series not directly mensalized")
  expect_true("m_popocup" %in% names(result),
              label = "Population series is mensalized")
})


test_that("mensalize_sidra_series errors when no valid series columns exist", {
  # 1. Setup: Only metadata columns, no series data
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201201L, 201202L, 201203L),
    mesnotrim = c(1L, 2L, 3L)
  )

  # 2. Verify: Error about no series columns
  expect_error(
    mensalize_sidra_series(dt, verbose = FALSE),
    "No series columns found",
    label = "No series columns raises error"
  )
})


test_that(".apply_final_adjustment handles NA values in y gracefully", {
  # 1. Setup: y with some NA values interspersed
  y <- c(100, NA, 300, 110, 210, NA, 120, 220, 320)
  rq <- c(NA, NA, 200, 180, 220, 210, 190, 230, 220)
  mesnotrim <- c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L)

  # 2. Execute
  m <- PNADCperiods:::.apply_final_adjustment(y, rq, mesnotrim)

  # 3. Verify: Where y is NA, m should remain NA (no adjustment possible)
  expect_true(is.na(m[2]), label = "NA in y propagates to m")
  expect_true(is.na(m[6]), label = "NA in y propagates to m")

  # Where y is not NA and trio is incomplete due to NA neighbor, fallback = y
  # Position 1 (mesnotrim=1): needs y[1], y_lead1=y[2]=NA -> invalid, m=y
  expect_equal(m[1], y[1], label = "Boundary fallback to y when neighbor is NA")
})


test_that("mensalize_sidra_series output length matches PNADC-era input rows", {
  # 1. Setup: Exactly 12 months of PNADC-era data
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 12L, series_name = "popocup"
  )

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: One output row per input row
  expect_equal(nrow(result), 12L,
               label = "Output has same number of rows as PNADC-era input")
})


test_that(".compute_cumsum_by_mesnotrim produces correct d3 for linear rq", {
  # 1. Setup: Linearly increasing RQ
  #    rq = 100, 101, 102, 103, 104, ...
  #    d3 = 3*(rq[t] - rq[t-1]) = 3*1 = 3 for all t > first of position
  rq <- 100 + 0:11
  mesnotrim <- rep(1:3, 4)

  # 2. Execute
  cum <- PNADCperiods:::.compute_cumsum_by_mesnotrim(rq, mesnotrim)

  # 3. Verify: For position 1, second occurrence is index 4
  #    d3[4] = 3*(103-102) = 3, cum[4] = 3
  #    d3[7] = 3*(106-105) = 3, cum[7] = 6
  #    d3[10] = 3*(109-108) = 3, cum[10] = 9
  expect_equal(cum[1], 0, label = "First pos1 cum=0")
  expect_equal(cum[4], 3, label = "Second pos1 d3=3")
  expect_equal(cum[7], 6, label = "Third pos1 cum=6")
  expect_equal(cum[10], 9, label = "Fourth pos1 cum=9")

  # For position 2:
  #    d3[5] = 3*(104-103) = 3, cum[5] = 3
  #    d3[8] = 3*(107-106) = 3, cum[8] = 6
  expect_equal(cum[2], 0, label = "First pos2 cum=0")
  expect_equal(cum[5], 3, label = "Second pos2 d3=3")
  expect_equal(cum[8], 6, label = "Third pos2 cum=6")
})


test_that("mensalize_sidra_series excludes avg income series from direct mensalization", {
  # 1. Setup: Data with an average income column (should be derived, not mensalized)
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 24L, series_name = "popocup"
  )
  data.table::set(dt, j = "rendhabnominaltodos", value = runif(nrow(dt), 2000, 3000))

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Average income not directly mensalized
  expect_false("m_rendhabnominaltodos" %in% names(result),
               label = "Average income series not directly mensalized")
})


test_that("mensalize_sidra_series excludes residual series from direct mensalization", {
  # 1. Setup: Data with a residual series column
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 24L, series_name = "popocup"
  )
  data.table::set(dt, j = "popforadaforca", value = runif(nrow(dt), 50000, 60000))

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Residual series not directly mensalized
  expect_false("m_popforadaforca" %in% names(result),
               label = "Residual series not directly mensalized")
})


test_that("mensalize_sidra_series does not modify input data", {
  # 1. Setup
  dt <- make_rolling_quarter_dt(
    start_yyyymm = 201201L, n_months = 24L, series_name = "popocup"
  )
  dt_copy <- data.table::copy(dt)

  sp <- data.table::data.table(
    series_name = rep("popocup", 3),
    mesnotrim = 1:3,
    y0 = c(99000, 99500, 100000)
  )

  # 2. Execute
  result <- mensalize_sidra_series(
    dt,
    starting_points = sp,
    series = "popocup",
    compute_derived = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Original input unchanged
  expect_equal(names(dt), names(dt_copy),
               label = "Input columns unchanged")
  expect_equal(nrow(dt), nrow(dt_copy),
               label = "Input row count unchanged")
  expect_equal(dt$popocup, dt_copy$popocup,
               label = "Input values unchanged")
})
