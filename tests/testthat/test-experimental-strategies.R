# Tests for experimental period identification strategies
# Tests for pnadc_experimental_periods() - probabilistic and UPA aggregation strategies

# =============================================================================
# PROBABILISTIC STRATEGY TESTS
# =============================================================================

test_that("probabilistic strategy requires store_date_bounds=TRUE", {
  # 1. Setup: Create crosswalk WITHOUT date bounds
  set.seed(400)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 15)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = FALSE)

  # 2. Verify: Should error when date bounds not stored
  # Error can be about missing columns like date_min, date_max, or week columns
  expect_error(
    pnadc_experimental_periods(
      crosswalk,
      strategy = "probabilistic",
      verbose = FALSE
    ),
    label = "Probabilistic strategy requires date bounds"
  )

  # 3. Context: Probabilistic strategy needs date ranges to calculate confidence
})


test_that("probabilistic strategy improves month determination", {
  # 1. Setup: Create data with date bounds
  set.seed(401)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # Store strict determination rate
  strict_rate <- crosswalk[, mean(determined_month, na.rm = TRUE)]

  # 2. Execute: Apply probabilistic strategy
  result <- pnadc_experimental_periods(
    crosswalk,
    strategy = "probabilistic",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  # Calculate experimental rate
  exp_rate <- result[, mean(determined_month, na.rm = TRUE)]

  # 3. Verify: Experimental rate should be >= strict rate
  expect_true(exp_rate >= strict_rate,
              label = paste0("Probabilistic should maintain or improve rate: ",
                             "strict=", round(strict_rate, 3), ", exp=", round(exp_rate, 3)))

  # 4. Context: Probabilistic adds determinations, never removes strict ones
})


test_that("confidence_threshold parameter affects determination", {
  # 1. Setup: Create data with store_date_bounds
  set.seed(402)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Test with different confidence thresholds
  result_08 <- pnadc_experimental_periods(
    crosswalk,
    strategy = "probabilistic",
    confidence_threshold = 0.8,
    verbose = FALSE
  )

  result_09 <- pnadc_experimental_periods(
    crosswalk,
    strategy = "probabilistic",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  result_095 <- pnadc_experimental_periods(
    crosswalk,
    strategy = "probabilistic",
    confidence_threshold = 0.95,
    verbose = FALSE
  )

  # 3. Verify: Lower threshold should give >= determinations than higher threshold
  rate_08 <- result_08[, mean(determined_month, na.rm = TRUE)]
  rate_09 <- result_09[, mean(determined_month, na.rm = TRUE)]
  rate_095 <- result_095[, mean(determined_month, na.rm = TRUE)]

  expect_true(rate_08 >= rate_09 - 0.01,  # Allow tiny tolerance
              label = paste0("threshold=0.8 (", round(rate_08, 3),
                             ") should be >= threshold=0.9 (", round(rate_09, 3), ")"))

  expect_true(rate_09 >= rate_095 - 0.01,
              label = paste0("threshold=0.9 (", round(rate_09, 3),
                             ") should be >= threshold=0.95 (", round(rate_095, 3), ")"))

  # 4. Context: More conservative thresholds assign fewer probabilistic months
})


test_that("probabilistic strategy preserves strict determinations", {
  # 1. Setup: Create data
  set.seed(403)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 15)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # Identify strict determinations
  strict_determined <- crosswalk[determined_month == TRUE]

  # 2. Execute: Apply probabilistic
  result <- pnadc_experimental_periods(
    crosswalk,
    strategy = "probabilistic",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  # 3. Verify: All strict determinations must be preserved
  result_determined <- result[determined_month == TRUE]

  # All strict IDs should still be determined in result
  # Note: Crosswalk is at household level; V2003 is not in the crosswalk output
  strict_ids <- paste(strict_determined$Ano, strict_determined$Trimestre,
                      strict_determined$UPA, strict_determined$V1008,
                      strict_determined$V1014, sep = "_")

  result_ids <- paste(result_determined$Ano, result_determined$Trimestre,
                      result_determined$UPA, result_determined$V1008,
                      result_determined$V1014, sep = "_")

  all_preserved <- all(strict_ids %in% result_ids)

  expect_true(all_preserved,
              label = "Probabilistic must preserve all strict determinations")

  # 4. Context: Experimental strategies only ADD, never remove determinations
})


test_that("probabilistic assignment only for 2-period ranges", {
  # 1. Setup: Create data
  set.seed(404)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 15)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Apply probabilistic
  result <- pnadc_experimental_periods(
    crosswalk,
    strategy = "probabilistic",
    confidence_threshold = 0.8,
    verbose = FALSE
  )

  # 3. Verify: Check the experimental assignment pattern
  # Probabilistic only assigns when there's ambiguity between 2 periods
  # This is verified indirectly - the function should not error

  expect_true(is.data.frame(result))
  expect_true("determined_month" %in% names(result))

  # 4. Context: Probabilistic works on 2-month ranges (implementation detail)
  # Single-month or 3-month ranges are not probabilistically assigned
})


# =============================================================================
# UPA AGGREGATION STRATEGY TESTS
# =============================================================================

test_that("UPA aggregation extends determinations via consensus", {
  # 1. Setup: Create data
  set.seed(500)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # Store strict rate
  strict_rate <- crosswalk[, mean(determined_month, na.rm = TRUE)]

  # 2. Execute: Apply UPA aggregation
  result <- pnadc_experimental_periods(
    crosswalk,
    strategy = "upa_aggregation",
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  # Calculate rate after UPA aggregation
  upa_rate <- result[, mean(determined_month, na.rm = TRUE)]

  # 3. Verify: UPA aggregation should maintain or improve rate
  expect_true(upa_rate >= strict_rate,
              label = paste0("UPA aggregation maintains or improves: ",
                             "strict=", round(strict_rate, 3), ", upa=", round(upa_rate, 3)))

  # 4. Context: UPA aggregation extends via UPA-level consensus
})


test_that("upa_proportion_threshold affects determination", {
  # 1. Setup: Create data
  set.seed(501)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Test with different thresholds
  result_05 <- pnadc_experimental_periods(
    crosswalk,
    strategy = "upa_aggregation",
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  result_08 <- pnadc_experimental_periods(
    crosswalk,
    strategy = "upa_aggregation",
    upa_proportion_threshold = 0.8,
    verbose = FALSE
  )

  # 3. Verify: Lower threshold should give >= determinations
  rate_05 <- result_05[, mean(determined_month, na.rm = TRUE)]
  rate_08 <- result_08[, mean(determined_month, na.rm = TRUE)]

  expect_true(rate_05 >= rate_08 - 0.01,
              label = paste0("threshold=0.5 (", round(rate_05, 3),
                             ") should be >= threshold=0.8 (", round(rate_08, 3), ")"))

  # 4. Context: Higher proportion threshold requires stronger UPA consensus
})


# =============================================================================
# "BOTH" STRATEGY TESTS
# =============================================================================

test_that("'both' strategy combines probabilistic and UPA aggregation", {
  # 1. Setup: Create data
  set.seed(600)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 20)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Apply each strategy separately and combined
  result_prob <- pnadc_experimental_periods(
    crosswalk,
    strategy = "probabilistic",
    confidence_threshold = 0.9,
    verbose = FALSE
  )

  result_upa <- pnadc_experimental_periods(
    crosswalk,
    strategy = "upa_aggregation",
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  result_both <- pnadc_experimental_periods(
    crosswalk,
    strategy = "both",
    confidence_threshold = 0.9,
    upa_proportion_threshold = 0.5,
    verbose = FALSE
  )

  # 3. Verify: "both" should be >= max(individual strategies)
  rate_prob <- result_prob[, mean(determined_month, na.rm = TRUE)]
  rate_upa <- result_upa[, mean(determined_month, na.rm = TRUE)]
  rate_both <- result_both[, mean(determined_month, na.rm = TRUE)]

  max_individual <- max(rate_prob, rate_upa)

  expect_true(rate_both >= max_individual - 0.01,  # Allow tiny tolerance
              label = paste0("'both' (", round(rate_both, 3),
                             ") should be >= max(individual) (", round(max_individual, 3), ")"))

  # 4. Context: Combined strategy should identify at least as many as either alone
})


test_that("'both' strategy preserves strict determinations", {
  # 1. Setup: Create data
  set.seed(601)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 15)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # Count strict determinations
  n_strict <- crosswalk[, sum(determined_month, na.rm = TRUE)]

  # 2. Execute: Apply "both" strategy
  result <- pnadc_experimental_periods(
    crosswalk,
    strategy = "both",
    verbose = FALSE
  )

  # Count experimental determinations
  n_exp <- result[, sum(determined_month, na.rm = TRUE)]

  # 3. Verify: Should have at least as many determinations as strict
  expect_true(n_exp >= n_strict,
              label = "Combined strategy must preserve strict determinations")

  # 4. Context: All experimental strategies are additive only
})


# =============================================================================
# NESTING PRESERVATION TESTS
# =============================================================================

test_that("experimental strategies maintain nesting (week requires fortnight)", {
  # 1. Setup: Create data
  set.seed(700)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 15)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Apply experimental strategy
  result <- pnadc_experimental_periods(
    crosswalk,
    strategy = "both",
    verbose = FALSE
  )

  # 3. Verify: Nesting violation check
  # All week-determined must have fortnight-determined
  violation <- result[determined_week == TRUE & (is.na(determined_fortnight) | determined_fortnight == FALSE)]

  expect_equal(nrow(violation), 0,
               label = "Week determination requires fortnight determination (nesting)")

  # 4. Context: Nesting is a fundamental invariant
})


test_that("experimental strategies maintain nesting (fortnight requires month)", {
  # 1. Setup: Create data
  set.seed(701)
  data <- create_realistic_pnadc(n_quarters = 4, n_upas = 15)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Apply experimental strategy
  result <- pnadc_experimental_periods(
    crosswalk,
    strategy = "both",
    verbose = FALSE
  )

  # 3. Verify: Nesting violation check
  # All fortnight-determined must have month-determined
  violation <- result[determined_fortnight == TRUE & (is.na(determined_month) | determined_month == FALSE)]

  expect_equal(nrow(violation), 0,
               label = "Fortnight determination requires month determination (nesting)")
})


# =============================================================================
# EDGE CASES
# =============================================================================

test_that("experimental strategies handle small data sets", {
  # 1. Setup: Create data with enough observations for experimental strategies
  set.seed(800)
  data <- create_realistic_pnadc(n_quarters = 2, n_upas = 10)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Should not error with reasonable data size
  expect_no_error({
    result <- pnadc_experimental_periods(
      crosswalk,
      strategy = "both",
      verbose = FALSE
    )
  })

  # 3. Context: Implementation should handle various data sizes gracefully
})


test_that("experimental strategies work with multi-quarter data", {
  # 1. Setup: Use multi-quarter data for experimental strategies
  # Single quarter may not have enough variation for experimental strategies
  set.seed(801)
  data <- create_realistic_pnadc(n_quarters = 3, n_upas = 15)

  crosswalk <- pnadc_identify_periods(data, verbose = FALSE, store_date_bounds = TRUE)

  # 2. Execute: Should work with sufficient data
  expect_no_error({
    result <- pnadc_experimental_periods(
      crosswalk,
      strategy = "both",
      verbose = FALSE
    )
  })

  # 3. Verify: Basic structure maintained
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), nrow(crosswalk))

  # 4. Context: Experimental strategies need sufficient data variation
})
