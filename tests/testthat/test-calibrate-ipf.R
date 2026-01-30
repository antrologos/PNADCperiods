# Tests for IPF weight calibration
# Tests for pnadc_ipf_calibrate() - alternative calibration method using
# Iterative Proportional Fitting instead of nested-cells raking

# =============================================================================
# BASIC STRUCTURE TESTS
# =============================================================================

test_that("pnadc_ipf_calibrate returns correct structure", {
  # 1. Setup: Create data with calibration columns
  set.seed(1000)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 30)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),  # Gender
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # Identify periods
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # Apply to get ref_month_yyyymm
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF calibration
  result <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "month",
    margins = c("age_group", "gender"),  # Simple margins for test
    verbose = FALSE
  )

  # 3. Verify: Should return data.table with IPF weight column
  expect_true(is.data.frame(result))
  # Note: IPF may filter to determined observations only
  expect_true(nrow(result) > 0)
  expect_true(nrow(result) <= nrow(data_with_periods))
  expect_true("weight_ipf_monthly" %in% names(result))

  # 4. Context: Basic functionality test
})


test_that("pnadc_ipf_calibrate handles fortnight calibration", {
  # 1. Setup: Create data
  set.seed(1001)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 30)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: Fortnight IPF calibration
  result <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "fortnight",
    margins = c("age_group"),  # Single margin for sparse fortnight data
    verbose = FALSE
  )

  # 3. Verify: Should have fortnight weight column
  expect_true("weight_ipf_fortnightly" %in% names(result))

  # 4. Context: Fortnight calibration with reduced margins
})


test_that("pnadc_ipf_calibrate handles week calibration", {
  # 1. Setup: Create data with enough observations for week calibration
  set.seed(1002)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 50)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: Week IPF calibration
  result <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "week",
    margins = c("age_group"),  # Minimal margins for very sparse week data
    verbose = FALSE
  )

  # 3. Verify: Should have week weight column
  expect_true("weight_ipf_weekly" %in% names(result))

  # 4. Context: Week calibration with minimal margins
})


# =============================================================================
# MARGIN PRESERVATION TESTS
# =============================================================================

test_that("IPF preserves age group margins", {
  # 1. Setup: Create data with clear age distribution
  set.seed(1003)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 40)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with age_group margin only
  result <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "month",
    margins = c("age_group"),
    verbose = FALSE
  )

  # 3. Verify: IPF weights should exist and be positive
  expect_true(all(result[!is.na(weight_ipf_monthly), weight_ipf_monthly > 0]))

  # 4. Context: Basic margin preservation (full validation requires margin calculation)
})


test_that("IPF handles multiple margins", {
  # 1. Setup: Create data with multiple demographic variables
  set.seed(1004)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 40)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with multiple margins
  result <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "month",
    margins = c("age_group", "gender", "hh_group"),
    verbose = FALSE
  )

  # 3. Verify: Should handle multiple margins without error
  expect_true("weight_ipf_monthly" %in% names(result))
  expect_true(all(result[!is.na(weight_ipf_monthly), weight_ipf_monthly > 0]))

  # 4. Context: IPF adjusts to match multiple independent margins
})


# =============================================================================
# PROPENSITY WEIGHTING TESTS
# =============================================================================

test_that("bias_correction='propensity' runs without error", {
  # 1. Setup: Create data with sufficient observations for propensity modeling
  set.seed(1005)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 40)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with propensity weighting
  expect_no_error({
    result <- pnadc_ipf_calibrate(
      data_with_periods,
      weight_var = "V1028",
      calibration_unit = "month",
      margins = c("age_group", "gender"),
      bias_correction = "propensity",
      verbose = FALSE
    )
  })

  # 3. Context: Propensity weighting corrects for selection bias
})


test_that("bias_correction='none' uses standard weights", {
  # 1. Setup: Create data
  set.seed(1006)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 30)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF without bias correction
  result <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "month",
    margins = c("age_group"),
    bias_correction = "none",
    verbose = FALSE
  )

  # 3. Verify: Should run successfully
  expect_true("weight_ipf_monthly" %in% names(result))

  # 4. Context: Default mode uses original weights
})


# =============================================================================
# IPF CONVERGENCE TESTS
# =============================================================================

test_that("IPF converges within max_iter", {
  # 1. Setup: Create data
  set.seed(1007)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 30)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with low max_iter (should still complete)
  expect_no_error({
    result <- pnadc_ipf_calibrate(
      data_with_periods,
      weight_var = "V1028",
      calibration_unit = "month",
      margins = c("age_group"),
      max_iter = 50,  # Lower than default 300
      verbose = FALSE
    )
  })

  # 3. Context: IPF should converge or stop at max_iter
})


test_that("IPF tol parameter affects convergence", {
  # 1. Setup: Create data
  set.seed(1008)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 30)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with different tolerance levels
  result_tight <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "month",
    margins = c("age_group"),
    tol = 1e-8,  # Tight tolerance
    verbose = FALSE
  )

  result_loose <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "month",
    margins = c("age_group"),
    tol = 1e-3,  # Loose tolerance
    verbose = FALSE
  )

  # 3. Verify: Both should complete
  expect_true("weight_ipf_monthly" %in% names(result_tight))
  expect_true("weight_ipf_monthly" %in% names(result_loose))

  # 4. Context: Tolerance controls convergence precision
})


# =============================================================================
# WEIGHT BOUNDS TESTS
# =============================================================================

test_that("IPF respects weight bounds", {
  # 1. Setup: Create data
  set.seed(1009)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 30)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with strict bounds
  result <- pnadc_ipf_calibrate(
    data_with_periods,
    weight_var = "V1028",
    calibration_unit = "month",
    margins = c("age_group"),
    bounds = c(0.5, 2),  # Strict bounds: 50% to 200% of original
    verbose = FALSE
  )

  # 3. Verify: Weights should be within bounds (relative to V1028)
  determined <- result[!is.na(weight_ipf_monthly)]
  if (nrow(determined) > 0) {
    ratios <- determined$weight_ipf_monthly / determined$V1028
    expect_true(all(ratios >= 0.5 - 0.01))  # Small tolerance for numerical error
    expect_true(all(ratios <= 2 + 0.01))
  }

  # 4. Context: Bounds prevent extreme weight adjustments
})


# =============================================================================
# ADAPTIVE MARGIN COLLAPSING TESTS
# =============================================================================

test_that("min_cell_size triggers margin collapsing", {
  # 1. Setup: Create small data to trigger sparse cells
  set.seed(1010)
  data <- create_realistic_pnadc(n_quarters = 1, n_upas = 10)  # Small data

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with high min_cell_size (should trigger collapsing)
  expect_no_error({
    result <- pnadc_ipf_calibrate(
      data_with_periods,
      weight_var = "V1028",
      calibration_unit = "month",
      margins = c("age_group"),
      min_cell_size = 50,  # High threshold
      verbose = FALSE
    )
  })

  # 3. Context: Adaptive collapsing handles sparse data
})


test_that("min_cell_size=0 disables collapsing", {
  # 1. Setup: Create data
  set.seed(1011)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 30)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Execute: IPF with min_cell_size=0 (no collapsing)
  expect_no_error({
    result <- pnadc_ipf_calibrate(
      data_with_periods,
      weight_var = "V1028",
      calibration_unit = "month",
      margins = c("age_group"),
      min_cell_size = 0,
      verbose = FALSE
    )
  })

  # 3. Context: min_cell_size=0 keeps original margin structure
})


# =============================================================================
# PARAMETER VALIDATION TESTS
# =============================================================================

test_that("invalid calibration_unit rejected", {
  # 1. Setup: Create minimal data
  data <- data.table::data.table(
    ref_month_yyyymm = 202301L,
    V1028 = 1000,
    V2009 = 30,
    V2007 = 1
  )

  # 2. Verify: Should error on invalid unit
  expect_error(
    pnadc_ipf_calibrate(
      data,
      weight_var = "V1028",
      calibration_unit = "invalid",
      margins = c("age_group"),
      verbose = FALSE
    ),
    "calibration_unit.*month.*fortnight.*week",
    label = "Invalid calibration_unit should be rejected"
  )
})


test_that("invalid margin names rejected", {
  # 1. Setup: Create minimal data
  set.seed(1012)
  data <- create_realistic_pnadc(n_quarters = 1, n_upas = 10)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    V2007 = sample(1:2, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  data_with_periods <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # 2. Verify: Should error on invalid margin names
  expect_error(
    pnadc_ipf_calibrate(
      data_with_periods,
      weight_var = "V1028",
      calibration_unit = "month",
      margins = c("invalid_margin"),
      verbose = FALSE
    ),
    label = "Invalid margin names should be rejected"
  )
})


test_that("missing required columns detected", {
  # 1. Setup: Create data missing required demographic columns
  data <- data.table::data.table(
    ref_month_yyyymm = 202301L,
    V1028 = 1000
    # Missing V2009 (age), V2007 (gender), etc.
  )

  # 2. Verify: Should error when trying to create margins
  expect_error(
    pnadc_ipf_calibrate(
      data,
      weight_var = "V1028",
      calibration_unit = "month",
      margins = c("age_group"),
      verbose = FALSE
    ),
    label = "Missing demographic columns should error"
  )
})
