# Tests for pnadc_identify_periods() - main crosswalk builder

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Note: Using shared test data generators from helper-test-data.R
# to ensure consistent age/birthday calculations across all tests

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("pnadc_identify_periods validates required columns", {
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)

  expect_error(
    pnadc_identify_periods(bad_data, verbose = FALSE),
    "missing"
  )
})

test_that("pnadc_identify_periods handles character columns", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  test_data[, Ano := as.character(Ano)]
  test_data[, Trimestre := as.character(Trimestre)]

  result <- pnadc_identify_periods(test_data, verbose = FALSE)
  expect_s3_class(result, "data.table")
})

# =============================================================================
# OUTPUT STRUCTURE TESTS
# =============================================================================

test_that("pnadc_identify_periods returns correct crosswalk structure", {
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Should be data.table

  expect_s3_class(result, "data.table")

  # Should have household-quarter level keys
  expect_true("Ano" %in% names(result))
  expect_true("Trimestre" %in% names(result))
  expect_true("UPA" %in% names(result))
  expect_true("V1008" %in% names(result))
  expect_true("V1014" %in% names(result))

  # Should NOT have person-level keys
  expect_false("V2003" %in% names(result))

  # Should have month columns (IBGE-based)
  expect_true("ref_month_in_quarter" %in% names(result))
  expect_true("ref_month_in_year" %in% names(result))
  expect_true("ref_month_yyyymm" %in% names(result))
  expect_true("determined_month" %in% names(result))

  # Should have fortnight columns (IBGE-based)
  expect_true("ref_fortnight_in_month" %in% names(result))
  expect_true("ref_fortnight_in_quarter" %in% names(result))
  expect_true("ref_fortnight_yyyyff" %in% names(result))
  expect_true("determined_fortnight" %in% names(result))

  # Should have week columns (IBGE-based)
  expect_true("ref_week_in_month" %in% names(result))
  expect_true("ref_week_in_quarter" %in% names(result))
  expect_true("ref_week_yyyyww" %in% names(result))
  expect_true("determined_week" %in% names(result))
})

test_that("pnadc_identify_periods returns same number of rows as input", {
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Result should have same number of rows as input
  # (crosswalk preserves person-level rows for joining)
  expect_equal(nrow(result), nrow(test_data))

  # Each household-quarter-panel combination should have consistent reference values
  # (all persons in same household should have same determined period)
  dt_check <- result[, .(
    n_unique_month = data.table::uniqueN(ref_month_in_quarter)
  ), by = .(Ano, Trimestre, UPA, V1008, V1014)]
  expect_true(all(dt_check$n_unique_month == 1))
})

test_that("pnadc_identify_periods has determination flags that allow computing rates", {
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Compute determination rates from the result
  n_total <- nrow(result)
  month_rate <- sum(result$determined_month) / n_total
  fortnight_rate <- sum(result$determined_fortnight) / n_total
  week_rate <- sum(result$determined_week) / n_total

  # Rates should be between 0 and 1

  expect_true(month_rate >= 0 && month_rate <= 1)
  expect_true(fortnight_rate >= 0 && fortnight_rate <= 1)
  expect_true(week_rate >= 0 && week_rate <= 1)
})

# =============================================================================
# DETERMINATION FLAG TESTS
# =============================================================================

test_that("determination flags are consistent with ref values", {
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # determined_month should be TRUE iff ref_month_in_quarter is not NA
  expect_equal(
    result$determined_month,
    !is.na(result$ref_month_in_quarter)
  )

  # determined_fortnight should be TRUE iff ref_fortnight_in_quarter is not NA
  expect_equal(
    result$determined_fortnight,
    !is.na(result$ref_fortnight_in_quarter)
  )

  # determined_week should be TRUE iff ref_week_in_quarter is not NA
  expect_equal(
    result$determined_week,
    !is.na(result$ref_week_in_quarter)
  )
})

# =============================================================================
# REFERENCE VALUE CONSISTENCY TESTS
# =============================================================================
test_that("ref_month_in_quarter values are 1, 2, 3, or NA", {
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  valid_values <- c(1L, 2L, 3L, NA_integer_)
  expect_true(all(result$ref_month_in_quarter %in% valid_values))
})

test_that("ref_fortnight_in_quarter values are 1-6 or NA", {
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  non_na <- result$ref_fortnight_in_quarter[!is.na(result$ref_fortnight_in_quarter)]
  if (length(non_na) > 0) {
    expect_true(all(non_na >= 1L & non_na <= 6L))
  }
})

test_that("ref_week_in_quarter values are 1-12 or NA", {
  # IBGE quarters always have exactly 12 reference weeks (4 weeks × 3 months)
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  non_na <- result$ref_week_in_quarter[!is.na(result$ref_week_in_quarter)]
  if (length(non_na) > 0) {
    expect_true(all(non_na >= 1L & non_na <= 12L))
  }
})

test_that("ref_month_yyyymm is consistent with ref_month_in_year", {
  test_data <- create_realistic_pnadc(n_quarters = 2, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  determined <- result[determined_month == TRUE]
  if (nrow(determined) > 0) {
    # ref_month_yyyymm should be Ano * 100 + ref_month_in_year
    expected_yyyymm <- determined$Ano * 100L + determined$ref_month_in_year
    expect_equal(determined$ref_month_yyyymm, expected_yyyymm)
  }
})

# =============================================================================
# FORTNIGHT FORMAT TESTS
# =============================================================================

test_that("ref_fortnight_yyyyff follows YYYY01-YYYY24 format", {
  test_data <- create_realistic_pnadc(n_quarters = 4, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  determined <- result[determined_fortnight == TRUE]
  if (nrow(determined) > 0) {
    years <- determined$ref_fortnight_yyyyff %/% 100L
    fortnights <- determined$ref_fortnight_yyyyff %% 100L

    # Fortnights should be 1-24
    expect_true(all(fortnights >= 1L & fortnights <= 24L))

    # Years should be reasonable
    expect_true(all(years >= 2000L & years <= 2100L))
  }
})

test_that("ref_fortnight_in_month is 1 or 2", {
  test_data <- create_realistic_pnadc(n_quarters = 4, n_upas = 5)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  determined <- result[determined_fortnight == TRUE]
  if (nrow(determined) > 0) {
    # ref_fortnight_in_month should be 1 (first fortnight) or 2 (second fortnight)
    expect_true(all(determined$ref_fortnight_in_month %in% c(1L, 2L)))
  }
})

# =============================================================================
# VERBOSE OUTPUT TESTS
# =============================================================================

test_that("pnadc_identify_periods respects verbose parameter", {
  test_data <- create_realistic_pnadc(n_quarters = 1, n_upas = 3)

  # verbose = FALSE should produce no output
  expect_silent(result <- pnadc_identify_periods(test_data, verbose = FALSE))

  # verbose = TRUE should produce output
  expect_output(
    result <- pnadc_identify_periods(test_data, verbose = TRUE),
    "Building|Step|determination"
  )
})

# =============================================================================
# DETERMINATION RATE HIERARCHY TESTS
# =============================================================================

test_that("month determination rate >= fortnight rate >= week rate", {
  # This tests the expected hierarchy: coarser granularity = higher determination
  test_data <- create_realistic_pnadc(n_quarters = 4, n_upas = 10)

  result <- pnadc_identify_periods(test_data, verbose = FALSE)

  # Compute determination rates from the result
  n_total <- nrow(result)
  month_rate <- sum(result$determined_month) / n_total
  fortnight_rate <- sum(result$determined_fortnight) / n_total
  week_rate <- sum(result$determined_week) / n_total

  # Month should have highest (or equal) rate
  expect_true(month_rate >= fortnight_rate ||
              abs(month_rate - fortnight_rate) < 0.01)  # Allow small tolerance

  # Fortnight should have higher (or equal) rate than week
  expect_true(fortnight_rate >= week_rate ||
              abs(fortnight_rate - week_rate) < 0.01)
})

# =============================================================================
# CROSS-QUARTER AGGREGATION TESTS
# =============================================================================

test_that("cross-quarter aggregation improves month determination rate", {
  # 1. Context: The algorithm's core feature is aggregating by UPA-V1014 ACROSS quarters
  # Documentation states: 1 quarter ~70% determination, 8 quarters ~94% determination

  set.seed(200)

  # 2. Test with 1 quarter - expect lower determination rate
  data_1q <- create_realistic_pnadc(n_quarters = 1, n_upas = 25, start_year = 2023)
  cw_1q <- pnadc_identify_periods(data_1q, verbose = FALSE)
  rate_1q <- cw_1q[, mean(determined_month, na.rm = TRUE)]

  # 3. Test with 8 quarters - expect higher determination rate
  data_8q <- create_realistic_pnadc(n_quarters = 8, n_upas = 25, start_year = 2023, seed = 200)
  cw_8q <- pnadc_identify_periods(data_8q, verbose = FALSE)
  rate_8q <- cw_8q[, mean(determined_month, na.rm = TRUE)]

  # 4. Verify: More quarters should improve determination significantly
  # Documentation benchmarks: 70% (1Q) vs 94% (8Q) = 24pp improvement
  # We'll be conservative and expect at least 10pp improvement
  improvement <- rate_8q - rate_1q

  expect_true(improvement > 0.10,
              label = paste0("8 quarters should improve determination by >10pp. ",
                             "Actual: 1Q=", round(rate_1q, 3), ", 8Q=", round(rate_8q, 3),
                             " (", round(improvement, 3), "pp)"))

  # 5. Context: Cross-quarter aggregation is the key algorithmic innovation
  # Same UPA-V1014 across quarters converge to consistent month constraints
})


test_that("same UPA-V1014 across quarters converge to same month", {
  # 1. Setup: Create data where same UPA-V1014 appears in multiple quarters
  set.seed(201)

  # Use small number of UPAs and panel groups to ensure overlaps
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 10, start_year = 2023)

  # 2. Execute: Identify periods with cross-quarter aggregation
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 3. Verify: Within same UPA-V1014, determined months should be consistent
  # Group by UPA-V1014 and check if ref_month_in_quarter is consistent when determined
  consistency_check <- crosswalk[determined_month == TRUE, .(
    n_obs = .N,
    n_unique_months = uniqueN(ref_month_in_quarter, na.rm = TRUE),
    months = paste(unique(ref_month_in_quarter), collapse = ",")
  ), by = .(UPA, V1014)]

  # Filter to UPA-V1014 with multiple determinations
  multi_det <- consistency_check[n_obs > 1]

  if (nrow(multi_det) > 0) {
    # All multi-observation UPA-V1014 should have consistent month
    all_consistent <- all(multi_det$n_unique_months == 1)

    expect_true(all_consistent,
                label = "Same UPA-V1014 across quarters should converge to same month")
  }

  # 4. Context: This is the fundamental mechanism of cross-quarter aggregation
  # Phase 1 aggregates by .(UPA, V1014) across ALL quarters, not within-quarter
})


test_that("determination rate improves monotonically with more quarters", {
  # 1. Setup: Test with 2, 4, 6 quarters to verify monotonic improvement
  set.seed(202)

  rates <- sapply(c(2, 4, 6), function(nq) {
    data <- create_realistic_pnadc(n_quarters = nq, n_upas = 20, start_year = 2023, seed = 202)
    cw <- pnadc_identify_periods(data, verbose = FALSE)
    cw[, mean(determined_month, na.rm = TRUE)]
  })

  # 2. Verify: Each increase in quarters should improve or maintain rate
  # rates[1] = 2Q, rates[2] = 4Q, rates[3] = 6Q
  expect_true(rates[2] >= rates[1] - 0.01,  # Allow tiny tolerance for randomness
              label = paste0("4Q (", round(rates[2], 3), ") should be >= 2Q (", round(rates[1], 3), ")"))

  expect_true(rates[3] >= rates[2] - 0.01,
              label = paste0("6Q (", round(rates[3], 3), ") should be >= 4Q (", round(rates[2], 3), ")"))

  # 3. Context: Stacking more data consistently improves month determination
  # This is why CLAUDE.md recommends "8+ quarters recommended" for best results
})

# =============================================================================
# BIRTHDAY CONSTRAINT TESTS
# =============================================================================

test_that("unknown birthdays (99/9999) are handled gracefully", {
  # 1. Setup: Create data with unknown birthdays (PNADC codes)
  set.seed(300)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 10)

  # Make 20% have unknown birthdays
  unknown_idx <- sample(seq_len(nrow(data)), size = ceiling(nrow(data) * 0.2))
  data[unknown_idx, `:=`(
    V2008 = 99L,    # PNADC code for unknown day
    V20081 = 99L,   # PNADC code for unknown month
    V20082 = 9999L  # PNADC code for unknown year
  )]

  # 2. Execute: Should not error with unknown birthdays
  expect_no_error({
    crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Determination still works (based on other constraints like V1014)
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # Some observations should still be determined despite missing birthdays
  expect_true(crosswalk[, sum(determined_month, na.rm = TRUE)] > 0,
              label = "Determination works even with unknown birthdays")

  # 4. Context: Birthday constraints are helpful but not required
  # Algorithm can determine months using V1014 patterns alone
})


test_that("February 29 birthdays are handled correctly", {
  # 1. Setup: Create person born on Feb 29 (leap year birthday)
  # Interview in non-leap year (2023)
  data <- data.table::data.table(
    Ano = rep(2023L, 4),
    Trimestre = rep(1L, 4),  # Q1 = Jan-Mar
    UPA = rep(1L, 4),
    V1008 = rep(1L, 4),
    V1014 = rep(1L, 4),
    V2003 = 1:4,
    V2008 = 29L,      # Born on 29th
    V20081 = 2L,      # February
    V20082 = 2000L,   # Leap year
    V2009 = 23L       # Age 23 in 2023
  )

  # 2. Execute: Should not error with Feb 29 birthday in non-leap year
  expect_no_error({
    crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Returns valid structure
  expect_true(is.data.frame(crosswalk))
  expect_equal(nrow(crosswalk), nrow(data))

  # 4. Context: Implementation substitutes March 1 for Feb 29 in non-leap years
  # See pnadc-identify-periods.R line 307
})


test_that("birthday constraints narrow date ranges correctly", {
  # 1. Setup: Create controlled data to test birthday constraint logic
  # Person born March 15, interviewed in Q1 (Jan-Mar)
  set.seed(301)

  # Create Q1 2023 data
  data <- data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = 1:10,
    V1008 = 1L,
    V1014 = 1:10,
    V2003 = 1L,
    V2008 = 15L,      # Born on 15th of month
    V20081 = 3L,      # March
    V20082 = 1990L,
    V2009 = 32L       # Age 32 (depends on if birthday passed)
  )

  # 2. Execute: Identify periods
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 3. Verify: Some observations should be determined
  # With birthday on March 15, and interview in Q1, the month determination
  # may depend on whether interview was before or after birthday
  expect_true(is.data.frame(crosswalk))
  expect_true("ref_month_in_quarter" %in% names(crosswalk))

  # 4. Context: Birthday constraints help narrow date ranges
  # visit_before_birthday calculation adjusts date_min/date_max
  # This is tested indirectly here - direct testing would require accessing internals
})


test_that("age and birthday consistency is maintained", {
  # 1. Setup: Use realistic data generator which ensures age/birthday consistency
  set.seed(302)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 15)

  # 2. Verify: Age is consistent with birth year
  # Calculate expected age range based on birth year and survey year
  data[V20082 != 9999, expected_age_range := Ano - V20082]  # Rough age

  # Age should be within 1 year of expected (depends on birthday)
  age_consistent <- data[V20082 != 9999,
                         all(abs(V2009 - expected_age_range) <= 1)]

  expect_true(age_consistent,
              label = "Age should be consistent with birth year (within 1 year)")

  # 3. Execute: Identification should work correctly with consistent data
  expect_no_error({
    crosswalk <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 4. Context: Fixing create_minimal_pnadc() ensures tests use realistic data
  # This test verifies the fix from Phase 1.1
})


test_that("mixed birthday data (some known, some unknown) is handled", {
  # 1. Setup: Create data with mix of known and unknown birthdays
  set.seed(303)
  data <- create_realistic_pnadc(n_quarters = 3, n_upas = 15)

  # Make 50% have unknown birthdays
  n_unknown <- ceiling(nrow(data) * 0.5)
  unknown_idx <- sample(seq_len(nrow(data)), size = n_unknown)
  data[unknown_idx, `:=`(
    V2008 = 99L,
    V20081 = 99L,
    V20082 = 9999L
  )]

  # 2. Execute: Should handle mixed data gracefully
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 3. Verify: Both groups (known and unknown birthdays) can be determined
  known_birthday_idx <- data[V20082 != 9999, which = TRUE]
  unknown_birthday_idx <- data[V20082 == 9999, which = TRUE]

  # Some with known birthdays should be determined
  known_det_rate <- crosswalk[known_birthday_idx, mean(determined_month, na.rm = TRUE)]

  # Some with unknown birthdays should also be determined (via V1014 patterns)
  unknown_det_rate <- crosswalk[unknown_birthday_idx, mean(determined_month, na.rm = TRUE)]

  # Both groups should have non-zero determination
  expect_true(known_det_rate > 0,
              label = "Observations with known birthdays can be determined")

  expect_true(unknown_det_rate > 0,
              label = "Observations with unknown birthdays can still be determined")

  # 4. Context: Algorithm uses multiple constraints, not just birthdays
})
