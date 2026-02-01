# Integration tests for end-to-end pipelines
# These tests verify the complete workflow: identify → apply → calibrate

# =============================================================================
# BASIC END-TO-END PIPELINE TESTS
# =============================================================================

test_that("full pipeline works end-to-end with monthly calibration", {
  # 1. Setup: Create realistic multi-quarter data
  set.seed(900)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 30)

  # Add calibration columns
  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # 2. Execute: Full pipeline
  # Step 1: Identify periods
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # Step 2: Apply and calibrate
  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    smooth = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Basic structure
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(data))
  expect_true("weight_monthly" %in% names(result))
  expect_true("determined_month" %in% names(result))

  # 4. Verify: Weight sum preservation
  weight_check <- result[determined_month == TRUE, .(
    monthly_sum = sum(weight_monthly, na.rm = TRUE),
    v1028_sum = sum(V1028, na.rm = TRUE)
  ), by = .(Ano, Trimestre)]

  expect_equal(weight_check$monthly_sum, weight_check$v1028_sum, tolerance = 1e-6,
               label = "End-to-end pipeline preserves weight sums")

  # 5. Verify: Determination rates are reasonable
  # With 4 quarters, expect >70% month determination
  det_rate <- result[, mean(determined_month, na.rm = TRUE)]
  expect_true(det_rate > 0.70,
              label = paste0("Month determination rate (", round(det_rate, 3),
                             ") should be >70% with 4 quarters"))
})


test_that("full pipeline works with fortnight calibration", {
  # 1. Setup: Create data
  set.seed(901)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 25)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # 2. Execute: Full pipeline with fortnight
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "fortnight",
    smooth = FALSE,
    verbose = FALSE
  )

  # 3. Verify: Structure
  expect_true("weight_fortnight" %in% names(result))
  expect_true("determined_fortnight" %in% names(result))

  # 4. Verify: Fortnight weights sum to parent monthly totals
  weight_check <- result[determined_fortnight == TRUE, .(
    fortnight_sum = sum(weight_fortnight, na.rm = TRUE),
    v1028_sum = sum(V1028, na.rm = TRUE)
  ), by = .(Ano, Trimestre, ref_month_in_quarter)]

  expect_equal(weight_check$fortnight_sum, weight_check$v1028_sum, tolerance = 1e-6,
               label = "Fortnight pipeline preserves parent totals")
})


test_that("full pipeline with smoothing produces consistent results", {
  # 1. Setup: Create data with enough quarters for smoothing
  set.seed(902)
  data <- create_realistic_pnadc(n_quarters = 6, n_upas = 25)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # 2. Execute: Pipeline with smoothing
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    smooth = TRUE,
    verbose = FALSE
  )

  # 3. Verify: Still preserves total sums after smoothing
  weight_check <- result[determined_month == TRUE, .(
    monthly_sum = sum(weight_monthly, na.rm = TRUE),
    v1028_sum = sum(V1028, na.rm = TRUE)
  ), by = .(Ano, Trimestre)]

  expect_equal(weight_check$monthly_sum, weight_check$v1028_sum, tolerance = 1e-6,
               label = "Smoothed weights still preserve totals")

  # 4. Context: Smoothing redistributes but doesn't change totals
})


# =============================================================================
# EXPERIMENTAL STRATEGIES INTEGRATION
# =============================================================================

test_that("pipeline with experimental strategies maintains invariants", {
  # 1. Setup: Create data
  set.seed(903)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 25)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # 2. Execute: Strict identification
  crosswalk_strict <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 3. Execute: Experimental strategies
  crosswalk_exp <- pnadc_experimental_periods(
    crosswalk_strict,
    strategy = "both",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  # 4. Execute: Apply calibration with experimental crosswalk
  result <- pnadc_apply_periods(
    data, crosswalk_exp,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    verbose = FALSE
  )

  # 5. Verify: Weight sums still preserved
  weight_check <- result[determined_month == TRUE, .(
    monthly_sum = sum(weight_monthly, na.rm = TRUE),
    v1028_sum = sum(V1028, na.rm = TRUE)
  ), by = .(Ano, Trimestre)]

  expect_equal(weight_check$monthly_sum, weight_check$v1028_sum, tolerance = 1e-6,
               label = "Experimental strategies + calibration preserves weights")

  # 6. Verify: Experimental improved determination rate
  strict_rate <- crosswalk_strict[, mean(determined_month, na.rm = TRUE)]
  exp_rate <- crosswalk_exp[, mean(determined_month, na.rm = TRUE)]

  expect_true(exp_rate >= strict_rate,
              label = paste0("Experimental (", round(exp_rate, 3),
                             ") >= strict (", round(strict_rate, 3), ")"))
})


# =============================================================================
# MULTI-YEAR PIPELINE TESTS
# =============================================================================

test_that("pipeline works with multi-year data (year anchor)", {
  # 1. Setup: Create 2-year data (8 quarters)
  set.seed(904)
  data <- create_realistic_pnadc(n_quarters = 8, n_upas = 25, start_year = 2022)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # 2. Execute: Pipeline with year anchor
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "year",
    calibrate = TRUE,
    calibration_unit = "month",
    verbose = FALSE
  )

  # 3. Verify: Yearly totals preserved
  yearly_check <- result[determined_month == TRUE, .(
    monthly_sum = sum(weight_monthly, na.rm = TRUE),
    v1028_sum = sum(V1028, na.rm = TRUE)
  ), by = .(Ano)]

  expect_equal(yearly_check$monthly_sum, yearly_check$v1028_sum, tolerance = 1e-6,
               label = "Year anchor preserves yearly totals")

  # 4. Verify: High determination rate with 8 quarters
  det_rate <- result[, mean(determined_month, na.rm = TRUE)]
  expect_true(det_rate > 0.85,
              label = paste0("8-quarter determination (", round(det_rate, 3),
                             ") should be >85%"))
})


# =============================================================================
# ERROR HANDLING IN PIPELINE
# =============================================================================

test_that("pipeline handles missing weight columns gracefully", {
  # 1. Setup: Create data then remove V1028 to simulate missing weight column
  set.seed(905)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 10)
  data[, V1028 := NULL]  # Remove weight column to test error handling

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Verify: Should error when requesting calibration without weight columns
  expect_error(
    pnadc_apply_periods(
      data, crosswalk,
      weight_var = "V1028",  # Column doesn't exist anymore
      anchor = "quarter",
      calibrate = TRUE,
      calibration_unit = "month",
      verbose = FALSE
    ),
    "V1028|weight_var|missing",
    label = "Missing weight column should error"
  )
})


test_that("pipeline works without calibration", {
  # 1. Setup: Create data with V1028 column but won't calibrate
  set.seed(906)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 10)

  # Add V1028 column (required even when not calibrating)
  data[, V1028 := runif(.N, 500, 2000)]

  # 2. Execute: Pipeline without calibration
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  result <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,  # No calibration
    verbose = FALSE
  )

  # 3. Verify: Basic structure without calibrated weight columns
  expect_true(is.data.frame(result))
  expect_false("weight_monthly" %in% names(result))
  expect_true("determined_month" %in% names(result))
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("V1028" %in% names(result))  # Original weight column preserved
})


# =============================================================================
# KEEP_ALL PARAMETER INTEGRATION
# =============================================================================

test_that("pipeline with keep_all=FALSE filters correctly", {
  # 1. Setup: Create data
  set.seed(907)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 15)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # 2. Execute: Pipeline with keep_all=TRUE
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  result_all <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    keep_all = TRUE,
    verbose = FALSE
  )

  # 3. Execute: Pipeline with keep_all=FALSE
  result_filtered <- pnadc_apply_periods(
    data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    keep_all = FALSE,
    verbose = FALSE
  )

  # 4. Verify: keep_all=FALSE returns fewer rows
  expect_true(nrow(result_filtered) <= nrow(result_all))

  # 5. Verify: All rows in filtered result are determined
  expect_true(all(result_filtered$determined_month == TRUE, na.rm = TRUE))

  # 6. Verify: No NA weights in filtered result
  expect_true(all(!is.na(result_filtered$weight_monthly)))
})


# =============================================================================
# CONSISTENCY CHECKS
# =============================================================================

test_that("pipeline produces consistent results with same data", {
  # 1. Setup: Create data
  set.seed(908)
  data <- create_realistic_pnadc(n_quarters = 3, n_upas = 20)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # 2. Execute: Pipeline twice with same data
  crosswalk1 <- pnadc_identify_periods(data, verbose = FALSE)
  result1 <- pnadc_apply_periods(
    data, crosswalk1,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    verbose = FALSE
  )

  crosswalk2 <- pnadc_identify_periods(data, verbose = FALSE)
  result2 <- pnadc_apply_periods(
    data, crosswalk2,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    verbose = FALSE
  )

  # 3. Verify: Results should be identical
  expect_equal(result1[determined_month == TRUE]$weight_monthly,
               result2[determined_month == TRUE]$weight_monthly,
               tolerance = 1e-10,
               label = "Pipeline should produce consistent results")

  expect_equal(crosswalk1$determined_month,
               crosswalk2$determined_month,
               label = "Determination should be consistent")
})
