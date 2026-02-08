# Tests for pnadc_apply_periods() - apply crosswalk and optionally calibrate weights

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Note: Using shared test data generators from helper-test-data.R
# - create_realistic_pnadc() for test data generation
# - Real crosswalks from pnadc_identify_periods() instead of mocks

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("pnadc_apply_periods requires weight_var argument", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk, anchor = "quarter"),
    "weight_var"
  )
})

test_that("pnadc_apply_periods requires anchor argument", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk, weight_var = "V1028"),
    "anchor"
  )
})

test_that("pnadc_apply_periods validates anchor values", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk,
                        weight_var = "V1028",
                        anchor = "invalid"),
    "anchor"
  )
})

test_that("pnadc_apply_periods validates calibration_unit values", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  # match.arg error message format
  expect_error(
    pnadc_apply_periods(test_data, crosswalk,
                        weight_var = "V1028",
                        anchor = "quarter",
                        calibration_unit = "invalid"),
    "should be one of"
  )
})

test_that("pnadc_apply_periods validates weight_var exists", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  expect_error(
    pnadc_apply_periods(test_data, crosswalk,
                        weight_var = "NONEXISTENT",
                        anchor = "quarter",
                        calibrate = FALSE),
    "not found in data"
  )
})

test_that("pnadc_apply_periods validates crosswalk has required columns", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  # Crosswalk missing V1014 (required minimal key)
  bad_crosswalk <- data.frame(UPA = 1:3, something_else = 1:3)

  expect_error(
    pnadc_apply_periods(test_data, bad_crosswalk,
                        weight_var = "V1028",
                        anchor = "quarter",
                        calibrate = FALSE),
    "missing"  # More general pattern to match error
  )
})

# =============================================================================
# OUTPUT STRUCTURE TESTS (NO CALIBRATION)
# =============================================================================

test_that("pnadc_apply_periods without calibration returns correct structure", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  # Should be data.table
  expect_s3_class(result, "data.table")

  # Should have reference period columns from crosswalk (current implementation)
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_month_in_year" %in% names(result))
  expect_true("ref_fortnight_in_month" %in% names(result))
  expect_true("ref_fortnight_in_quarter" %in% names(result))
  expect_true("ref_week_in_month" %in% names(result))
  expect_true("ref_week_in_quarter" %in% names(result))

  # Should have determination flags
  expect_true("determined_month" %in% names(result))
  expect_true("determined_fortnight" %in% names(result))
  expect_true("determined_week" %in% names(result))

  # Should NOT have calibrated weight column when calibrate = FALSE
  expect_false("weight_monthly" %in% names(result))
})

test_that("pnadc_apply_periods preserves original data columns", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  test_data[, custom_column := "test_value"]

  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  expect_true("custom_column" %in% names(result))
  expect_true("Ano" %in% names(result))
  expect_true("Trimestre" %in% names(result))
  expect_true("V1028" %in% names(result))
})

# =============================================================================
# CALIBRATION TESTS
# =============================================================================

test_that("pnadc_apply_periods with calibration adds weight column", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    target_totals = NULL,  # Auto-derive from SIDRA
    verbose = FALSE
  )

  expect_true("weight_monthly" %in% names(result))
  expect_type(result$weight_monthly, "double")
})

test_that("pnadc_apply_periods respects calibration_unit parameter", {
  skip_if_not(requireNamespace("sidrar", quietly = TRUE),
              "sidrar package not available for fetching population targets")
  skip_if_offline()

  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Month calibration (with explicit targets)
  result_month <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    target_totals = NULL,  # Auto-derive from SIDRA
    verbose = FALSE
  )
  expect_true("weight_monthly" %in% names(result_month))

  # Fortnight calibration (let it derive targets from SIDRA)
  result_fortnight <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "fortnight",
    target_totals = NULL,  # Auto-derive from SIDRA
    verbose = FALSE
  )
  expect_true("weight_fortnight" %in% names(result_fortnight))

  # Week calibration (let it derive targets from SIDRA)
  result_week <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "week",
    target_totals = NULL,  # Auto-derive from SIDRA
    verbose = FALSE
  )
  expect_true("weight_weekly" %in% names(result_week))
})

# =============================================================================
# KEEP_ALL PARAMETER TESTS
# =============================================================================

test_that("pnadc_apply_periods keep_all = TRUE includes undetermined rows", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    target_totals = NULL,  # Auto-derive from SIDRA
    keep_all = TRUE,
    verbose = FALSE
  )

  # Should include all rows from original data
  expect_equal(nrow(result), nrow(test_data))

  # Undetermined rows should have NA weight
  undetermined <- result[is.na(ref_month_in_quarter)]
  if (nrow(undetermined) > 0) {
    expect_true(all(is.na(undetermined$weight_monthly)))
  }
})

test_that("pnadc_apply_periods keep_all = FALSE excludes undetermined rows", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    target_totals = NULL,  # Auto-derive from SIDRA
    keep_all = FALSE,
    verbose = FALSE
  )

  # Should only include determined rows
  expect_true(all(!is.na(result$ref_month_in_quarter)))
  expect_true(nrow(result) <= nrow(test_data))
})

# =============================================================================
# ANCHOR PARAMETER TESTS
# =============================================================================

test_that("pnadc_apply_periods accepts anchor = 'quarter'", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  expect_no_error(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = TRUE,
      target_totals = NULL,  # Auto-derive from SIDRA
      verbose = FALSE
    )
  )
})

test_that("pnadc_apply_periods accepts anchor = 'year'", {
  test_data <- create_realistic_pnadc(n_quarters = 4, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  expect_no_error(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "year",
      calibrate = TRUE,
      target_totals = NULL,  # Auto-derive from SIDRA
      verbose = FALSE
    )
  )
})

# =============================================================================
# VERBOSE OUTPUT TESTS
# =============================================================================

test_that("pnadc_apply_periods respects verbose parameter", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  # verbose = FALSE should produce no output
  expect_silent(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = FALSE,
      verbose = FALSE
    )
  )

  # verbose = TRUE should produce output
  expect_output(
    result <- pnadc_apply_periods(
      test_data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = FALSE,
      verbose = TRUE
    ),
    "Applying|crosswalk"
  )
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("pnadc_identify_periods rejects empty data", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  test_data <- test_data[0]  # Empty data

  # Empty data should error at identify_periods stage (validation rejects empty data)
  expect_error(
    pnadc_identify_periods(test_data, verbose = FALSE),
    "at least 1 rows"
  )
})

test_that("pnadc_apply_periods handles mismatched data keys", {
  # Create data and crosswalk with different UPA sets
  data1 <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)
  data2 <- create_realistic_pnadc(n_quarters = 1, n_upas = 5, seed = 999)

  # Create crosswalk from data1 (only UPAs 1-3)
  crosswalk <- pnadc_identify_periods(data1, verbose = FALSE)

  # Apply to data2 (has UPAs 1-5) - some rows won't match
  result <- pnadc_apply_periods(
    data2, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    keep_all = TRUE,
    verbose = FALSE
  )

  # Result should have same number of rows as data2 (keep_all=TRUE)
  expect_equal(nrow(result), nrow(data2))

  # But some rows may have NA for period columns if they don't match crosswalk
  expect_true(any(is.na(result$ref_month_in_quarter)))
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("pnadc_apply_periods works with real identify function output", {
  # Create data and run the identify function
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)
  crosswalk <- pnadc_identify_periods(test_data, verbose = FALSE)

  result <- pnadc_apply_periods(
    test_data, crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_fortnight_in_quarter" %in% names(result))
  expect_true("ref_week_in_quarter" %in% names(result))
})
