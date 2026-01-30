# Tests for edge cases and boundary conditions
# Systematic coverage of unusual inputs and boundary transitions

# =============================================================================
# EMPTY AND MINIMAL DATA TESTS
# =============================================================================

test_that("identify_periods rejects empty data", {
  # 1. Setup: Create empty data.table with correct structure
  empty_data <- data.table::data.table(
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

  # 2. Verify: Should error on empty data (validation fails)
  expect_error(
    pnadc_identify_periods(empty_data, verbose = FALSE),
    "at least 1 rows",
    label = "Empty data should be rejected by validation"
  )

  # 3. Context: Input validation prevents processing of empty data
})


test_that("identify_periods handles single observation", {
  # 1. Setup: Create data with just one person
  single_obs <- data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = 1L,
    V1008 = 1L,
    V1014 = 1L,
    V2008 = 15L,
    V20081 = 6L,
    V20082 = 1990L,
    V2009 = 33L
  )

  # 2. Execute: Should not error
  expect_no_error({
    result <- pnadc_identify_periods(single_obs, verbose = FALSE)
  })

  # 3. Verify: Should return one row
  expect_equal(nrow(result), 1)
  expect_true("determined_month" %in% names(result))

  # 4. Context: Minimal valid input
})


test_that("apply_periods handles all NA weights gracefully", {
  # 1. Setup: Create data with NA weights
  data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  data[, V1028 := NA_real_]

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Execute: Apply without calibration (NA weights)
  expect_no_error({
    result <- pnadc_apply_periods(
      data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = FALSE,
      verbose = FALSE
    )
  })

  # 3. Context: Missing weight data should not crash
})


# =============================================================================
# YEAR AND QUARTER BOUNDARY TESTS
# =============================================================================

test_that("year boundary transition handled correctly", {
  # 1. Setup: Create data spanning year boundary (Q4 2023 + Q1 2024)
  set.seed(100)
  data_q4 <- create_realistic_pnadc(n_quarters = 1, n_upas = 10, start_year = 2023)
  data_q4[, Trimestre := 4L]

  data_q1 <- create_realistic_pnadc(n_quarters = 1, n_upas = 10, start_year = 2024)
  data_q1[, Trimestre := 1L]

  # Combine
  data <- rbind(data_q4, data_q1)

  # 2. Execute: Should handle year transition
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Both years should be present
  expect_true(2023L %in% result$Ano)
  expect_true(2024L %in% result$Ano)
  expect_true(4L %in% result$Trimestre)
  expect_true(1L %in% result$Trimestre)

  # 4. Context: Date calculations must handle year boundaries
})


test_that("December 31 to January 1 transition", {
  # 1. Setup: Create observation with interview on Dec 31
  data <- data.table::data.table(
    Ano = 2023L,
    Trimestre = 4L,
    UPA = 1L,
    V1008 = 1L,
    V1014 = 1L,
    V2008 = 31L,  # Day 31
    V20081 = 12L, # December
    V20082 = 1990L,
    V2009 = 33L
  )

  # 2. Execute: Should not error
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Should produce valid crosswalk
  expect_equal(nrow(result), 1)

  # 4. Context: Edge case for last day of year
})


# =============================================================================
# LEAP YEAR TESTS
# =============================================================================

test_that("February 29 birthday in non-leap year", {
  # 1. Setup: Person born Feb 29, interviewed in non-leap year (2023)
  data <- data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = 1L,
    V1008 = 1L,
    V1014 = 1L,
    V2008 = 29L,  # Day 29
    V20081 = 2L,  # February
    V20082 = 1992L,  # Leap year birth
    V2009 = 31L  # Age
  )

  # 2. Execute: Should handle gracefully (Feb 28 used as substitute)
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Should produce valid crosswalk
  expect_equal(nrow(result), 1)

  # 4. Context: Implementation uses Feb 28 in non-leap years
})


test_that("February 29 birthday in leap year", {
  # 1. Setup: Person born Feb 29, interviewed in leap year (2024)
  data <- data.table::data.table(
    Ano = 2024L,
    Trimestre = 1L,
    UPA = 1L,
    V1008 = 1L,
    V1014 = 1L,
    V2008 = 29L,
    V20081 = 2L,
    V20082 = 1992L,
    V2009 = 32L
  )

  # 2. Execute: Should handle correctly
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Valid crosswalk
  expect_equal(nrow(result), 1)

  # 4. Context: Feb 29 exists in leap years
})


# =============================================================================
# UNKNOWN BIRTHDAY CODES
# =============================================================================

test_that("all unknown birthdays (99/9999) handled", {
  # 1. Setup: Create data where all observations have unknown birthdays
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 10)
  data[, `:=`(
    V2008 = 99L,
    V20082 = 9999L
  )]

  # 2. Execute: Should not error
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Determination still works (based on other constraints)
  expect_true(nrow(result) > 0)
  expect_true("determined_month" %in% names(result))

  # 4. Context: Birthday constraints skipped when unknown
})


test_that("mixed known and unknown birthdays", {
  # 1. Setup: Half known, half unknown birthdays
  set.seed(200)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 10)

  n <- nrow(data)
  unknown_idx <- sample(seq_len(n), size = n %/% 2)
  data[unknown_idx, `:=`(
    V2008 = 99L,
    V20082 = 9999L
  )]

  # 2. Execute: Should not error
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Verify: Both types processed
  expect_equal(nrow(result), n)

  # 4. Context: Mixed data should work
})


# =============================================================================
# EXTREME AGE VALUES
# =============================================================================

test_that("very young ages handled (age 0)", {
  # 1. Setup: Create data with age 0 (infant)
  data <- data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = 1L,
    V1008 = 1L,
    V1014 = 1L,
    V2008 = 1L,
    V20081 = 1L,
    V20082 = 2023L,  # Born same year
    V2009 = 0L  # Age 0
  )

  # 2. Execute: Should not error
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Context: PNADC includes all ages
})


test_that("very old ages handled (age 100+)", {
  # 1. Setup: Create data with age 100+
  data <- data.table::data.table(
    Ano = 2023L,
    Trimestre = 1L,
    UPA = 1L,
    V1008 = 1L,
    V1014 = 1L,
    V2008 = 15L,
    V20081 = 6L,
    V20082 = 1920L,  # Very old
    V2009 = 103L  # Age 103
  )

  # 2. Execute: Should not error
  expect_no_error({
    result <- pnadc_identify_periods(data, verbose = FALSE)
  })

  # 3. Context: Centenarians exist in data
})


# =============================================================================
# SINGLE VS MULTI-QUARTER BEHAVIOR
# =============================================================================

test_that("single quarter produces lower determination rate", {
  # 1. Setup: Same UPAs across 1 quarter vs 4 quarters
  set.seed(300)
  data_1q <- create_realistic_pnadc(n_quarters = 1, n_upas = 20)
  data_4q <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  # 2. Execute: Identify periods
  cw_1q <- pnadc_identify_periods(data_1q, verbose = FALSE)
  cw_4q <- pnadc_identify_periods(data_4q, verbose = FALSE)

  # 3. Verify: More quarters should give higher determination rate
  rate_1q <- cw_1q[, mean(determined_month, na.rm = TRUE)]
  rate_4q <- cw_4q[, mean(determined_month, na.rm = TRUE)]

  expect_true(rate_4q > rate_1q,
              label = paste0("4Q (", round(rate_4q, 3),
                             ") should be > 1Q (", round(rate_1q, 3), ")"))

  # 4. Context: Cross-quarter aggregation improves determination
})


# =============================================================================
# ZERO WEIGHT TESTS
# =============================================================================

test_that("zero weights in input handled", {
  # 1. Setup: Create data with some zero weights
  set.seed(400)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 10)

  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  # Set some weights to zero
  zero_idx <- sample(1:nrow(data), size = 5)
  data[zero_idx, V1028 := 0]

  # 2. Execute: Apply with calibration
  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

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

  # 3. Verify: No negative weights produced
  expect_true(all(result$weight_monthly >= 0, na.rm = TRUE))

  # 4. Context: Zero weights edge case
})


# =============================================================================
# ALL PERIODS INDETERMINATE TEST
# =============================================================================

test_that("data with no determinations handled", {
  # 1. Setup: Create data that's very hard to determine
  # Single quarter, wide date ranges, no birthday info
  data <- data.table::data.table(
    Ano = rep(2023L, 10),
    Trimestre = rep(1L, 10),
    UPA = 1:10,  # All different UPAs
    V1008 = 1L,
    V1014 = 1:10,  # All different V1014
    V2008 = 99L,  # Unknown birthday
    V20081 = 99L,
    V20082 = 9999L,
    V2009 = 30L
  )

  # 2. Execute: Identify periods
  result <- pnadc_identify_periods(data, verbose = FALSE)

  # 3. Verify: Should return crosswalk (even if determination rate is low)
  expect_equal(nrow(result), 10)
  expect_true("determined_month" %in% names(result))

  # 4. Context: Low determination rate is valid outcome
})


# =============================================================================
# PARAMETER VALIDATION TESTS
# =============================================================================

test_that("invalid anchor parameter rejected", {
  # 1. Setup: Create data
  data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Verify: Invalid anchor should error
  expect_error(
    pnadc_apply_periods(
      data, crosswalk,
      weight_var = "V1028",
      anchor = "invalid_anchor",
      calibrate = TRUE,
      calibration_unit = "month",
      verbose = FALSE
    ),
    "anchor.*quarter.*year",
    label = "Invalid anchor parameter should be rejected"
  )
})


test_that("invalid calibration_unit rejected", {
  # 1. Setup: Create data
  data <- create_realistic_pnadc(n_quarters = 1, n_upas = 5)
  valid_ufs <- c(11:17, 21:29, 31:35, 41:43, 50:53)
  data[, `:=`(
    UF = sample(valid_ufs, .N, replace = TRUE),
    V1028 = runif(.N, 500, 2000),
    posest = sample(1:500, .N, replace = TRUE),
    posest_sxi = sample(100:999, .N, replace = TRUE)
  )]

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE)

  # 2. Verify: Invalid calibration_unit should error
  # Note: match.arg() throws error before custom message
  expect_error(
    pnadc_apply_periods(
      data, crosswalk,
      weight_var = "V1028",
      anchor = "quarter",
      calibrate = TRUE,
      calibration_unit = "invalid_unit",
      verbose = FALSE
    ),
    label = "Invalid calibration_unit should be rejected"
  )
})
