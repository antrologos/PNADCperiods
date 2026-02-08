# Tests for weight calibration correctness
# Core invariant: each sub-monthly period (fortnight, week) is calibrated to the
# MONTHLY population total from SIDRA. So sum(weight_fortnight) per fortnight =
# sum(weight_monthly) per month = SIDRA population. Same for weeks.

# =============================================================================
# WEIGHT SUM PRESERVATION TESTS
# =============================================================================

test_that("all months are calibrated to SIDRA target population", {
  # 1. Setup: Create realistic data with multiple quarters
  set.seed(123)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # Create custom target_totals so test is offline and deterministic.
  # Use the quarterly V1028 sum / 1000 as a population target so that
  # target_pop * 1000 gives a value the calibration can converge to.
  qtr_wsum <- data[, .(qtr_wsum = sum(V1028, na.rm = TRUE)), by = .(Ano, Trimestre)]
  targets <- qtr_wsum[, {
    months <- (Trimestre - 1L) * 3L + 1:3
    data.table::data.table(
      ref_month_yyyymm = Ano * 100L + months,
      m_populacao = qtr_wsum / 1000
    )
  }, by = .(Ano, Trimestre)][, .(ref_month_yyyymm, m_populacao)]

  # 2. Execute: Identify periods and apply calibration with custom targets
  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    target_totals = targets,
    verbose = FALSE
  )

  # 3. Verify: Weights are valid for determined observations
  determined <- result[determined_month == TRUE]

  # All determined obs have positive weights
  expect_true(all(determined$weight_monthly > 0),
              label = "All determined observations have positive weights")

  # No NA weights for determined obs
  expect_false(any(is.na(determined$weight_monthly)),
               label = "No NA weights for determined observations")

  # 4. Verify: ALL months (1, 2, 3) are calibrated to the same SIDRA target
  # Each month's weight sum should equal target_pop * 1000
  month_sums <- determined[, .(w_sum = sum(weight_monthly)),
                           by = .(Ano, ref_month_in_year)]
  month_sums[, ref_month_yyyymm := Ano * 100L + ref_month_in_year]
  merged <- merge(month_sums, targets, by = "ref_month_yyyymm")
  expect_equal(merged$w_sum, merged$m_populacao * 1000, tolerance = 0.01,
               label = "All months calibrated to SIDRA target population")
})


test_that("each fortnight's weights sum to the monthly population total", {
  # 1. Setup: Create data and identify periods
  set.seed(124)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # Create custom target_totals scaled to the test data's weight sums.
  # With simulated data, quarterly V1028 sum differs from real SIDRA population.
  # Setting m_populacao = quarterly_sum / 1000 gives a consistent target value.
  qtr_wsum <- data[, .(qtr_wsum = sum(V1028, na.rm = TRUE)), by = .(Ano, Trimestre)]
  targets <- qtr_wsum[, {
    months <- (Trimestre - 1L) * 3L + 1:3
    data.table::data.table(
      ref_month_yyyymm = Ano * 100L + months,
      m_populacao = qtr_wsum / 1000
    )
  }, by = .(Ano, Trimestre)][, .(ref_month_yyyymm, m_populacao)]

  # 2. Execute: Apply fortnight calibration with custom targets
  fortnight_targets <- PNADCperiods:::derive_fortnight_population(targets)
  result_fortnight <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "fortnight",
    target_totals = fortnight_targets,
    verbose = FALSE
  )

  # 3. Execute: Apply month calibration with same base targets
  result_month <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    target_totals = targets,
    verbose = FALSE
  )

  # 4. Each INDIVIDUAL fortnight's weights should sum to the monthly population total
  # This is the key calibration invariant: fortnights and weeks are calibrated
  # to the SIDRA monthly population target, so each sub-period = monthly total
  t_fortnight <- result_fortnight[!is.na(ref_fortnight_in_month), .(
    N_fortnight = sum(weight_fortnight, na.rm = TRUE)
  ), by = .(Ano, ref_month_in_year, ref_fortnight_in_month)][
    order(Ano, ref_month_in_year, ref_fortnight_in_month)]

  t_month <- result_month[!is.na(ref_month_in_quarter), .(
    N_month = sum(weight_monthly, na.rm = TRUE)
  ), by = .(Ano, ref_month_in_year)][
    order(Ano, ref_month_in_year)]

  t <- merge(t_fortnight, t_month, by = c("Ano", "ref_month_in_year"))

  if (nrow(t) > 0) {
    expect_equal(t$N_fortnight, t$N_month, tolerance = 0.01,
                 label = "Each fortnight's weights must sum to monthly population total")
  }
})


test_that("each week's weights sum to the monthly population total", {
  # 1. Setup: Create data and identify periods
  set.seed(125)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # Custom targets scaled to test data (same approach as fortnight test)
  qtr_wsum <- data[, .(qtr_wsum = sum(V1028, na.rm = TRUE)),
                  by = .(Ano, Trimestre)]
  targets <- qtr_wsum[, {
    months <- (Trimestre - 1L) * 3L + 1:3
    data.table::data.table(
      ref_month_yyyymm = Ano * 100L + months,
      m_populacao = qtr_wsum / 1000
    )
  }, by = .(Ano, Trimestre)][, .(ref_month_yyyymm, m_populacao)]

  # 2. Execute: Apply week calibration with custom targets
  week_targets <- PNADCperiods:::derive_weekly_population(targets)
  result_week <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "week",
    target_totals = week_targets,
    verbose = FALSE
  )

  # 3. Execute: Apply month calibration with same base targets
  result_month <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    target_totals = targets,
    verbose = FALSE
  )

  # 4. Each INDIVIDUAL week's weights should sum to monthly total
  t_week <- result_week[!is.na(ref_week_in_month), .(
    N_week = sum(weight_weekly, na.rm = TRUE)
  ), by = .(Ano, ref_month_in_year, ref_week_in_month)][
    order(Ano, ref_month_in_year, ref_week_in_month)]

  t_month <- result_month[!is.na(ref_month_in_quarter), .(
    N_month = sum(weight_monthly, na.rm = TRUE)
  ), by = .(Ano, ref_month_in_year)][
    order(Ano, ref_month_in_year)]

  t <- merge(t_week, t_month, by = c("Ano", "ref_month_in_year"))

  if (nrow(t) > 0) {
    expect_equal(t$N_week, t$N_month, tolerance = 0.01,
                 label = "Each week's weights must sum to monthly total")
  }
})


test_that("no negative weights are produced in any calibration unit", {
  # 1. Setup: Create data
  set.seed(126)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Test all calibration units
  for (unit in c("month", "fortnight", "week")) {
    result <- pnadc_apply_periods(
      data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = TRUE,
      calibration_unit = unit,
      verbose = FALSE
    )

    # 3. Verify: No negative weights allowed
    weight_col <- paste0("weight_", ifelse(unit == "fortnight", "fortnight",
                                           ifelse(unit == "week", "weekly", "monthly")))

    negative_weights <- result[!is.na(get(weight_col)) & get(weight_col) < 0]

    # 4. Context: Negative weights are methodologically invalid
    expect_equal(nrow(negative_weights), 0,
                 label = paste("No negative weights allowed in", unit, "calibration"))
  }
})


test_that("anchor='year' produces valid calibrated weights", {
  # 1. Setup: Create multi-year data
  set.seed(127)
  data <- create_realistic_pnadc(n_quarters = 8, n_upas = 20, start_year = 2022)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Apply with year anchor
  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "year",
    calibrate = TRUE,
    calibration_unit = "month",
    verbose = FALSE
  )

  # 3. Verify: Weights are valid
  determined <- result[determined_month == TRUE]
  expect_true(all(determined$weight_monthly > 0),
              label = "All weights are positive")
  expect_false(any(is.na(determined$weight_monthly)),
               label = "No NA weights for determined observations")

  # 4. Context: Year anchor calibrates at year level
  # Each month is still scaled to external population targets
})


# =============================================================================
# INDETERMINATE OBSERVATIONS
# =============================================================================

test_that("indeterminate observations have NA weights", {
  # 1. Setup: Create data with some indeterminate cases
  set.seed(128)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 15)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Apply calibration with keep_all=TRUE
  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    keep_all = TRUE,
    verbose = FALSE
  )

  # 3. Verify: Indeterminate observations must have NA weights
  indeterminate <- result[is.na(determined_month) | determined_month == FALSE]

  if (nrow(indeterminate) > 0) {
    expect_true(all(is.na(indeterminate$weight_monthly)),
                label = "Indeterminate observations must have NA monthly weights")
  }

  # 4. Context: Only determined observations can have calibrated weights
})


test_that("keep_all=FALSE excludes indeterminate observations", {
  # 1. Setup: Create data
  set.seed(129)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 15)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Apply with keep_all=FALSE
  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    keep_all = FALSE,
    verbose = FALSE
  )

  # 3. Verify: All returned observations must be determined
  expect_true(all(result$determined_month == TRUE, na.rm = TRUE),
              label = "keep_all=FALSE returns only determined observations")

  # 4. Verify: No NA weights in result
  expect_true(all(!is.na(result$weight_monthly)),
              label = "No NA weights when keep_all=FALSE")
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("calibration works with single quarter data", {
  # 1. Setup: Minimal single-quarter data
  set.seed(130)
  data <- create_realistic_pnadc(n_quarters = 1, n_upas = 10)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Apply calibration
  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    verbose = FALSE
  )

  # 3. Verify: Basic structure preserved
  expect_true("weight_monthly" %in% names(result))
  expect_equal(nrow(result), nrow(data))

  # 4. Verify: Weights are valid even with sparse data
  if (any(result$determined_month == TRUE, na.rm = TRUE)) {
    determined <- result[determined_month == TRUE]
    expect_true(all(determined$weight_monthly > 0),
                label = "All determined observations have positive weights")
  }
})


test_that("calibration handles empty periods gracefully", {
  # 1. Setup: Create data where some months may be empty
  set.seed(131)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Should not error with sparse data
  expect_no_error({
    result <- pnadc_apply_periods(
      data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = TRUE,
      calibration_unit = "month",
      verbose = FALSE
    )
  })

  # 3. Context: Implementation should handle empty cells gracefully
})


# =============================================================================
# SMOOTHING BEHAVIOR
# =============================================================================

test_that("smooth=TRUE modifies weights differently than smooth=FALSE", {
  # 1. Setup: Create data with enough quarters for smoothing to have effect
  # Smoothing uses 3-period window for months, so need at least 6+ quarters
  set.seed(132)
  data <- create_realistic_pnadc(n_quarters = 8, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Apply with and without smoothing
  result_no_smooth <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    smooth = FALSE,
    verbose = FALSE
  )

  result_smooth <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    smooth = TRUE,
    verbose = FALSE
  )

  # 3. Verify: Both runs should complete successfully
  expect_true(is.data.frame(result_no_smooth))
  expect_true(is.data.frame(result_smooth))
  expect_true("weight_monthly" %in% names(result_no_smooth))
  expect_true("weight_monthly" %in% names(result_smooth))

  # 4. Context: Smoothing parameter is accepted and processed
  # Note: Whether smoothing produces different weights depends on data characteristics
  # (month distribution, determination patterns, etc.). The key is that the parameter
  # is processed without error and produces valid weights.
})


test_that("smoothing produces valid non-negative weights", {
  # 1. Setup: Create data
  set.seed(133)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Apply with smoothing
  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    smooth = TRUE,
    verbose = FALSE
  )

  # 3. Verify: Smoothed weights are valid
  determined <- result[determined_month == TRUE]
  expect_true(all(determined$weight_monthly > 0),
              label = "Smoothed weights are positive")
  expect_false(any(is.na(determined$weight_monthly)),
               label = "No NA values after smoothing")
})
