# Tests for validation utility functions

test_that("required_vars_ref_month returns expected columns", {
  vars <- required_vars_ref_month()
  expect_type(vars, "character")
  expect_true("Ano" %in% vars)
  expect_true("Trimestre" %in% vars)
  expect_true("UPA" %in% vars)
  expect_true("V2008" %in% vars)  # Birth day
  expect_true("V20081" %in% vars)  # Birth month
  expect_true("V20082" %in% vars)  # Birth year
  expect_true("V2009" %in% vars)   # Age
})

test_that("join_key_vars returns expected columns", {
  vars <- join_key_vars()
  expect_type(vars, "character")
  expect_true("Ano" %in% vars)
  expect_true("Trimestre" %in% vars)
  expect_true("UPA" %in% vars)
  expect_true("V1008" %in% vars)
  expect_true("V1014" %in% vars)
  # Note: V2003 (person number) is NOT in join_key_vars because
  # crosswalk is at household level (V1008), not person level
  expect_false("V2003" %in% vars)
})

test_that("required_vars_weights returns expected columns", {
  vars <- required_vars_weights()
  expect_type(vars, "character")
  expect_true("V1028" %in% vars)
  expect_true("UF" %in% vars)
  expect_true("posest" %in% vars)
  expect_true("posest_sxi" %in% vars)
})

test_that("validate_pnadc detects missing columns", {
  # Missing required columns
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  expect_error(validate_pnadc(bad_data), "missing_ref_month")
})

test_that("validate_pnadc accepts valid minimal data", {
  valid_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  # Should not error
 expect_silent(validate_pnadc(valid_data))
})

test_that("validate_pnadc detects invalid year", {
  bad_year_data <- data.frame(
    Ano = 2000,  # Too early (< 2012)
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_year_data), "invalid_years")
})

test_that("validate_pnadc detects invalid quarter", {
  bad_quarter_data <- data.frame(
    Ano = 2023,
    Trimestre = 5,  # Invalid quarter
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_quarter_data), "invalid_quarters")
})

test_that("validate_pnadc detects invalid birth day", {
  bad_day_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,     # Required column
    V1014 = 1,
    V2008 = 35,  # Invalid day
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_day_data), "invalid_birth_days")
})

test_that("validate_pnadc detects invalid birth month", {
  bad_month_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,     # Required column
    V1014 = 1,
    V2008 = 15,
    V20081 = 15,  # Invalid month
    V20082 = 1990,
    V2009 = 33
  )
  expect_error(validate_pnadc(bad_month_data), "invalid_birth_months")
})

test_that("validate_pnadc returns report when stop_on_error = FALSE", {
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  result <- validate_pnadc(bad_data, stop_on_error = FALSE)

  expect_type(result, "list")
  expect_false(result$valid)
  expect_true("missing_ref_month" %in% names(result$issues))
})

test_that("validate_pnadc reports valid data correctly", {
  valid_data <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )
  result <- validate_pnadc(valid_data, stop_on_error = FALSE)

  expect_type(result, "list")
  expect_true(result$valid)
  expect_equal(result$n_rows, 1)
})

test_that("validate_pnadc checks weight columns when requested", {
  # Valid ref month data but missing weight columns
  data_no_weights <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )

  # Should pass without check_weights
  expect_silent(validate_pnadc(data_no_weights, check_weights = FALSE))

  # Should fail with check_weights
  expect_error(validate_pnadc(data_no_weights, check_weights = TRUE), "missing_weights")
})

test_that("validate_monthly_totals accepts valid data", {
  valid_totals <- data.frame(
    ref_month_yyyymm = c(202301, 202302, 202303),
    m_populacao = c(200000, 200100, 200200)
  )
  # Should not error
  expect_silent(validate_monthly_totals(valid_totals))
})

test_that("validate_monthly_totals accepts anomesexato column", {
  valid_totals <- data.frame(
    anomesexato = c(202301, 202302, 202303),
    m_populacao = c(200000, 200100, 200200)
  )
  # Should not error
  expect_silent(validate_monthly_totals(valid_totals))
})

test_that("validate_monthly_totals detects missing date column", {
  bad_totals <- data.frame(
    m_populacao = c(200000, 200100, 200200)
  )
  expect_error(validate_monthly_totals(bad_totals), "missing_date")
})

test_that("validate_monthly_totals detects missing population column", {
  bad_totals <- data.frame(
    ref_month_yyyymm = c(202301, 202302, 202303)
  )
  expect_error(validate_monthly_totals(bad_totals), "missing_population")
})

test_that("ensure_data_table converts data.frame", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  result <- ensure_data_table(df)

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 3)
})

test_that("ensure_data_table returns data.table unchanged when copy = FALSE", {
  dt <- data.table::data.table(x = 1:3)
  result <- ensure_data_table(dt, copy = FALSE)

  # Should be the same object (not a copy)
  expect_identical(data.table::address(result), data.table::address(dt))
})

test_that("ensure_data_table returns copy when copy = TRUE", {
  dt <- data.table::data.table(x = 1:3)
  result <- ensure_data_table(dt, copy = TRUE)

  # Should be a different object
  expect_false(identical(data.table::address(result), data.table::address(dt)))
  expect_equal(result$x, dt$x)
})

# =============================================================================
# VALIDATE_PERIOD_INVARIANTS TESTS
# =============================================================================

test_that("validate_period_invariants accepts valid crosswalk", {
  # 1. Setup: Create valid crosswalk with proper nesting
  # Weeks 1-2 → Fortnight 1, Weeks 3-4 → Fortnight 2, etc.
  # Fortnights 1-2 → Month 1, Fortnights 3-4 → Month 2, Fortnights 5-6 → Month 3
  crosswalk <- data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = 1:10,
    V1008 = 1L,
    V2003 = 1L,
    ref_month_in_quarter = c(1L, 1L, 2L, 2L, 3L, 3L, NA, NA, NA, NA),
    ref_fortnight_in_quarter = c(1L, 1L, 3L, 3L, 5L, 5L, NA, NA, NA, NA),  # Match months
    ref_week_in_quarter = c(1L, 2L, 5L, 6L, 9L, 10L, NA, NA, NA, NA),      # Match fortnights
    determined_month = c(rep(TRUE, 6), rep(FALSE, 4)),
    determined_fortnight = c(rep(TRUE, 6), rep(FALSE, 4)),
    determined_week = c(rep(TRUE, 6), rep(FALSE, 4))
  )

  # 2. Verify: Should not error
  expect_no_error(
    PNADCperiods:::validate_period_invariants(crosswalk, context = "test")
  )
})

test_that("validate_period_invariants detects invalid month ranges", {
  # 1. Setup: Create crosswalk with invalid month (4, should be 1-3)
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(1L, 2L, 3L, 4L),  # 4 is invalid!
    ref_fortnight_in_quarter = c(1L, 3L, 5L, NA),
    ref_week_in_quarter = c(1L, 3L, 5L, NA),
    determined_month = c(TRUE, TRUE, TRUE, TRUE),
    determined_fortnight = c(TRUE, TRUE, TRUE, FALSE),
    determined_week = c(TRUE, TRUE, TRUE, FALSE)
  )

  # 2. Verify: Should error on invalid month value
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk),
    "ref_month_in_quarter must be 1-3",
    label = "Invalid month value (4) should be detected"
  )
})

test_that("validate_period_invariants detects invalid fortnight ranges", {
  # 1. Setup: Create crosswalk with invalid fortnight (7, should be 1-6)
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(1L, 2L, 3L),
    ref_fortnight_in_quarter = c(1L, 3L, 7L),  # 7 is invalid!
    ref_week_in_quarter = c(1L, 3L, 5L),
    determined_month = c(TRUE, TRUE, TRUE),
    determined_fortnight = c(TRUE, TRUE, TRUE),
    determined_week = c(TRUE, TRUE, TRUE)
  )

  # 2. Verify: Should error on invalid fortnight value
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk),
    "ref_fortnight_in_quarter must be 1-6",
    label = "Invalid fortnight value (7) should be detected"
  )
})

test_that("validate_period_invariants detects invalid week ranges", {
  # 1. Setup: Create crosswalk with invalid week (13, should be 1-12)
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(1L, 2L, 3L),
    ref_fortnight_in_quarter = c(1L, 3L, 5L),
    ref_week_in_quarter = c(1L, 5L, 13L),  # 13 is invalid!
    determined_month = c(TRUE, TRUE, TRUE),
    determined_fortnight = c(TRUE, TRUE, TRUE),
    determined_week = c(TRUE, TRUE, TRUE)
  )

  # 2. Verify: Should error on invalid week value
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk),
    "ref_week_in_quarter must be 1-12",
    label = "Invalid week value (13) should be detected"
  )
})

test_that("validate_period_invariants detects week without fortnight (nesting violation)", {
  # 1. Setup: Week determined but fortnight is NA (violates nesting)
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(1L, 2L),
    ref_fortnight_in_quarter = c(1L, NA),  # Fortnight NA
    ref_week_in_quarter = c(1L, 3L),       # But week determined!
    determined_month = c(TRUE, TRUE),
    determined_fortnight = c(TRUE, FALSE),  # Fortnight not determined
    determined_week = c(TRUE, TRUE)         # But week is determined - VIOLATION
  )

  # 2. Verify: Should error on nesting violation
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk),
    "Nesting violation.*week determined but fortnight is NA",
    label = "Week without fortnight violates nesting"
  )
})

test_that("validate_period_invariants detects fortnight without month (nesting violation)", {
  # 1. Setup: Fortnight determined but month is NA
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(1L, NA),       # Month NA
    ref_fortnight_in_quarter = c(1L, 3L),   # But fortnight determined!
    ref_week_in_quarter = c(1L, NA),
    determined_month = c(TRUE, FALSE),       # Month not determined
    determined_fortnight = c(TRUE, TRUE),    # But fortnight is - VIOLATION
    determined_week = c(TRUE, FALSE)
  )

  # 2. Verify: Should error on nesting violation
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk),
    "Nesting violation.*fortnight determined but month is NA",
    label = "Fortnight without month violates nesting"
  )
})

test_that("validate_period_invariants detects week-fortnight mismatch", {
  # 1. Setup: Week and fortnight inconsistent
  # Week 1-2 should be in fortnight 1, but marked as fortnight 3
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(1L),
    ref_fortnight_in_quarter = c(3L),  # Fortnight 3
    ref_week_in_quarter = c(1L),       # Week 1 - should be in fortnight 1!
    determined_month = c(TRUE),
    determined_fortnight = c(TRUE),
    determined_week = c(TRUE)
  )

  # 2. Verify: Should error on inconsistency
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk),
    "Week-fortnight inconsistency",
    label = "Week-fortnight mismatch should be detected"
  )
})

test_that("validate_period_invariants detects fortnight-month mismatch", {
  # 1. Setup: Fortnight and month inconsistent
  # Fortnights 1-2 should be in month 1, but marked as month 3
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(3L),       # Month 3
    ref_fortnight_in_quarter = c(1L),   # Fortnight 1 - should be in month 1!
    ref_week_in_quarter = c(1L),
    determined_month = c(TRUE),
    determined_fortnight = c(TRUE),
    determined_week = c(TRUE)
  )

  # 2. Verify: Should error on inconsistency
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk),
    "Fortnight-month inconsistency",
    label = "Fortnight-month mismatch should be detected"
  )
})

test_that("validate_period_invariants strict=FALSE allows warnings", {
  # 1. Setup: Create crosswalk with minor issue
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(1L, 2L, 4L),  # 4 is invalid
    ref_fortnight_in_quarter = c(1L, 3L, NA),
    ref_week_in_quarter = c(1L, 3L, NA),
    determined_month = c(TRUE, TRUE, TRUE),
    determined_fortnight = c(TRUE, TRUE, FALSE),
    determined_week = c(TRUE, TRUE, FALSE)
  )

  # 2. Verify: strict=FALSE should warn, not error
  expect_warning(
    PNADCperiods:::validate_period_invariants(crosswalk, strict = FALSE),
    "ref_month_in_quarter must be 1-3",
    label = "strict=FALSE should warn instead of error"
  )
})

test_that("validate_period_invariants provides context in error messages", {
  # 1. Setup: Invalid crosswalk
  crosswalk <- data.table::data.table(
    ref_month_in_quarter = c(4L),  # Invalid
    ref_fortnight_in_quarter = c(NA),
    ref_week_in_quarter = c(NA),
    determined_month = c(TRUE),
    determined_fortnight = c(FALSE),
    determined_week = c(FALSE)
  )

  # 2. Verify: Context appears in error message
  expect_error(
    PNADCperiods:::validate_period_invariants(crosswalk, context = "my_test"),
    "my_test.*ref_month_in_quarter",
    label = "Context should appear in error message"
  )
})

# =============================================================================
# VALIDATE_PNADC ADDITIONAL TESTS
# =============================================================================

test_that("validate_pnadc with stop_on_error=FALSE returns issues for invalid years", {
  # 1. Setup: Data with invalid year
  bad_data <- data.frame(
    Ano = 2000,  # Before 2012 (PNADC started 2012)
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )

  # 2. Execute: validate with stop_on_error=FALSE
  result <- validate_pnadc(bad_data, stop_on_error = FALSE)

  # 3. Verify: Should return list with issues
  expect_type(result, "list")
  expect_false(result$valid)
  expect_true("invalid_years" %in% names(result$issues))
})

test_that("validate_pnadc with stop_on_error=FALSE returns issues for invalid quarters", {
  # 1. Setup: Data with invalid quarter
  bad_data <- data.frame(
    Ano = 2023,
    Trimestre = 5,  # Invalid (should be 1-4)
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 33
  )

  # 2. Execute: validate with stop_on_error=FALSE
  result <- validate_pnadc(bad_data, stop_on_error = FALSE)

  # 3. Verify: Should return list with issues
  expect_false(result$valid)
  expect_true("invalid_quarters" %in% names(result$issues))
})

test_that("validate_pnadc detects unusual ages as warnings only", {
  # 1. Setup: Data with unusual age
  data_unusual_age <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = 150  # Unusual age (>130)
  )

  # 2. Execute: Should produce warning but still validate
  expect_warning(
    result <- validate_pnadc(data_unusual_age, stop_on_error = TRUE),
    "Unusual ages|warning_ages"
  )

  # 3. With stop_on_error=FALSE, should still return valid=TRUE (only warning)
  result <- suppressWarnings(validate_pnadc(data_unusual_age, stop_on_error = FALSE))
  expect_true(result$valid,
              label = "Unusual ages are warnings only, not validation failures")
})

test_that("validate_pnadc detects negative ages", {
  # 1. Setup: Data with negative age
  data_negative_age <- data.frame(
    Ano = 2023,
    Trimestre = 1,
    UPA = 1,
    V1008 = 1,
    V1014 = 1,
    V2008 = 15,
    V20081 = 6,
    V20082 = 1990,
    V2009 = -5  # Negative age
  )

  # 2. Execute: Should produce warning
  expect_warning(
    validate_pnadc(data_negative_age, stop_on_error = TRUE),
    "Unusual ages|warning_ages"
  )
})

test_that("validate_pnadc handles empty data gracefully", {
  # 1. Setup: Empty data.frame with correct columns
  empty_data <- data.frame(
    Ano = integer(0),
    Trimestre = integer(0),
    UPA = integer(0),
    V1008 = integer(0),
    V1014 = integer(0),
    V2008 = integer(0),
    V20081 = integer(0),
    V20082 = integer(0),
    V2009 = integer(0)
  )

  # 2. Verify: Should error on min.rows constraint
  # checkmate produces error about "at least 1 rows"
  expect_error(
    validate_pnadc(empty_data),
    "at least 1 rows|min.rows",
    label = "Empty data should fail min.rows check"
  )
})
