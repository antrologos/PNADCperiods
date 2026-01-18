# Tests for identify_reference_fortnight()

# =============================================================================
# INPUT VALIDATION TESTS
# =============================================================================

test_that("identify_reference_fortnight validates input", {
  # Missing required columns - should error
  bad_data <- data.frame(Ano = 2023, Trimestre = 1)
  expect_error(identify_reference_fortnight(bad_data))
})

test_that("identify_reference_fortnight returns correct structure", {
  # Create minimal valid data
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(1, 1),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(15, 20),
    V20081 = c(6, 3),
    V20082 = c(1990, 1985),
    V2009 = c(33, 38)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Check output columns exist
  expect_true("ref_fortnight" %in% names(result))
  expect_true("ref_fortnight_in_quarter" %in% names(result))
  expect_true("ref_fortnight_yyyyff" %in% names(result))

  # Check ref_fortnight_in_quarter values are valid (1-6)
  valid_values <- result$ref_fortnight_in_quarter[!is.na(result$ref_fortnight_in_quarter)]
  expect_true(all(valid_values %in% 1:6))

  # Output is a data.table

  expect_s3_class(result, "data.table")
})

# =============================================================================
# HELPER FUNCTION TESTS
# =============================================================================

test_that("date_to_fortnight_in_quarter calculates correctly", {
  # Q1: Jan, Feb, Mar -> positions 1-6
  # Jan 1-15 = position 1, Jan 16-31 = position 2
  # Feb 1-15 = position 3, Feb 16-28/29 = position 4
  # Mar 1-15 = position 5, Mar 16-31 = position 6

  # Early January (first fortnight)
  date1 <- as.Date("2024-01-10")
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date1, 1L), 1L)

  # Late January (second fortnight)
  date2 <- as.Date("2024-01-20")
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date2, 1L), 2L)

  # Early February (third fortnight)
  date3 <- as.Date("2024-02-10")
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date3, 1L), 3L)

  # Late March (sixth fortnight)
  date4 <- as.Date("2024-03-25")
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date4, 1L), 6L)

  # Boundary: exactly day 15 should be first half
  date5 <- as.Date("2024-01-15")
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date5, 1L), 1L)

  # Boundary: exactly day 16 should be second half
  date6 <- as.Date("2024-01-16")
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date6, 1L), 2L)
})

test_that("date_to_fortnight_in_quarter works for all quarters", {
  # Q2: Apr, May, Jun
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(as.Date("2024-04-05"), 2L), 1L)
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(as.Date("2024-06-20"), 2L), 6L)

  # Q3: Jul, Aug, Sep
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(as.Date("2024-07-10"), 3L), 1L)
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(as.Date("2024-09-25"), 3L), 6L)

  # Q4: Oct, Nov, Dec
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(as.Date("2024-10-01"), 4L), 1L)
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(as.Date("2024-12-31"), 4L), 6L)
})

test_that("date_to_yyyyff calculates correctly", {
  # January first fortnight: 202401
  expect_equal(PNADCperiods:::date_to_yyyyff(as.Date("2024-01-10")), 202401L)

  # January second fortnight: 202402
  expect_equal(PNADCperiods:::date_to_yyyyff(as.Date("2024-01-20")), 202402L)

  # June first fortnight: 202411 (month 6 -> (6-1)*2 + 1 = 11)
  expect_equal(PNADCperiods:::date_to_yyyyff(as.Date("2024-06-10")), 202411L)

  # December second fortnight: 202424
  expect_equal(PNADCperiods:::date_to_yyyyff(as.Date("2024-12-25")), 202424L)

  # Boundary tests
  expect_equal(PNADCperiods:::date_to_yyyyff(as.Date("2024-01-15")), 202401L)  # Day 15 = first half
  expect_equal(PNADCperiods:::date_to_yyyyff(as.Date("2024-01-16")), 202402L)  # Day 16 = second half
})

test_that("fortnight_in_quarter_to_yyyyff calculates correctly", {
  # Q1 position 1 = fortnight 1 of year
  expect_equal(PNADCperiods:::fortnight_in_quarter_to_yyyyff(2024L, 1L, 1L), 202401L)

  # Q1 position 6 = fortnight 6 of year
  expect_equal(PNADCperiods:::fortnight_in_quarter_to_yyyyff(2024L, 1L, 6L), 202406L)

  # Q2 position 1 = fortnight 7 of year
  expect_equal(PNADCperiods:::fortnight_in_quarter_to_yyyyff(2024L, 2L, 1L), 202407L)

  # Q3 position 3 = fortnight 15 of year (12 + 3 = 15)
  expect_equal(PNADCperiods:::fortnight_in_quarter_to_yyyyff(2024L, 3L, 3L), 202415L)

  # Q4 position 6 = fortnight 24 of year
  expect_equal(PNADCperiods:::fortnight_in_quarter_to_yyyyff(2024L, 4L, 6L), 202424L)
})

test_that("yyyyff_to_date calculates correctly", {
  # First fortnight of January: day 1
  expect_equal(PNADCperiods:::yyyyff_to_date(202401L), as.Date("2024-01-01"))

  # Second fortnight of January: day 16
  expect_equal(PNADCperiods:::yyyyff_to_date(202402L), as.Date("2024-01-16"))

  # Third fortnight = Feb 1-15
  expect_equal(PNADCperiods:::yyyyff_to_date(202403L), as.Date("2024-02-01"))

  # Fourth fortnight = Feb 16+
  expect_equal(PNADCperiods:::yyyyff_to_date(202404L), as.Date("2024-02-16"))

  # Last fortnight of year (Dec 16-31)
  expect_equal(PNADCperiods:::yyyyff_to_date(202424L), as.Date("2024-12-16"))
})

test_that("yyyyff_to_date and date_to_yyyyff are inverses", {
  # Round-trip: date -> yyyyff -> date should give first day of fortnight
  test_dates <- as.Date(c("2024-01-05", "2024-03-20", "2024-07-15", "2024-12-28"))

  for (i in seq_along(test_dates)) {
    d <- test_dates[i]
    yyyyff <- PNADCperiods:::date_to_yyyyff(d)
    back <- PNADCperiods:::yyyyff_to_date(yyyyff)

    # The reconstructed date should be the start of the fortnight (1st or 16th)
    expect_true(as.integer(format(back, "%d")) %in% c(1L, 16L))

    # The month should match
    expect_equal(as.integer(format(back, "%m")), as.integer(format(d, "%m")))

    # The year should match
    expect_equal(as.integer(format(back, "%Y")), as.integer(format(d, "%Y")))
  }
})

# =============================================================================
# VECTORIZATION TESTS
# =============================================================================

test_that("fortnight helper functions work with vectors", {
  dates <- as.Date(c("2024-01-10", "2024-02-20", "2024-03-05"))
  quarters <- c(1L, 1L, 1L)

  # date_to_fortnight_in_quarter
  positions <- PNADCperiods:::date_to_fortnight_in_quarter(dates, quarters)
  expect_equal(length(positions), 3L)
  expect_equal(positions, c(1L, 4L, 5L))  # Jan early=1, Feb late=4, Mar early=5

  # date_to_yyyyff
  yyyyffs <- PNADCperiods:::date_to_yyyyff(dates)
  expect_equal(length(yyyyffs), 3L)
  expect_equal(yyyyffs, c(202401L, 202404L, 202405L))

  # fortnight_in_quarter_to_yyyyff
  years <- c(2024L, 2024L, 2024L)
  pos <- c(1L, 3L, 6L)
  result <- PNADCperiods:::fortnight_in_quarter_to_yyyyff(years, quarters, pos)
  expect_equal(length(result), 3L)
  expect_equal(result, c(202401L, 202403L, 202406L))

  # yyyyff_to_date
  yyyyff_vec <- c(202401L, 202410L, 202424L)
  dates_back <- PNADCperiods:::yyyyff_to_date(yyyyff_vec)
  expect_equal(length(dates_back), 3L)
  expect_equal(dates_back, as.Date(c("2024-01-01", "2024-05-16", "2024-12-16")))
})

# =============================================================================
# NA HANDLING TESTS
# =============================================================================

test_that("identify_reference_fortnight handles NA values", {
  # Data with missing birthday info
  test_data <- data.frame(
    Ano = c(2023, 2023, 2023),
    Trimestre = c(1, 1, 1),
    UPA = c(1, 1, 2),
    V1008 = c(1, 1, 1),
    V1014 = c(1, 1, 1),
    V2003 = c(1, 2, 1),
    V2008 = c(15, 99, 20),      # 99 = unknown (converted to NA)
    V20081 = c(6, 99, 3),       # 99 = unknown (converted to NA)
    V20082 = c(1990, 9999, 1985), # 9999 = unknown (converted to NA)
    V2009 = c(33, NA, 38)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should complete without error
  expect_s3_class(result, "data.table")

  # Should have all output columns
  expect_true(all(c("ref_fortnight", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff") %in% names(result)))
})

test_that("identify_reference_fortnight handles all-NA groups", {
  # All persons in a UPA-panel have unknown birthdays
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(1, 1),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(99, 99),      # All unknown
    V20081 = c(99, 99),
    V20082 = c(9999, 9999),
    V2009 = c(NA, NA)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should complete without error (infinite values handled)
  expect_s3_class(result, "data.table")
})

# =============================================================================
# DETERMINATION RATE TESTS
# =============================================================================

test_that("identify_reference_fortnight stores determination rate attribute", {
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(1, 1),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(15, 20),
    V20081 = c(6, 3),
    V20082 = c(1990, 1985),
    V2009 = c(33, 38)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should have determination_rate attribute
  expect_true(!is.null(attr(result, "determination_rate")))

  # Attribute should be numeric between 0 and 1
  rate <- attr(result, "determination_rate")
  expect_true(is.numeric(rate))
  expect_true(rate >= 0 && rate <= 1)
})

# =============================================================================
# OUTPUT UNIQUENESS TESTS
# =============================================================================

test_that("identify_reference_fortnight returns unique rows per UPA-V1014", {
  # Multiple persons in same UPA-V1014
  test_data <- data.frame(
    Ano = rep(2023, 4),
    Trimestre = rep(1, 4),
    UPA = c(1, 1, 1, 2),
    V1008 = c(1, 1, 1, 1),
    V1014 = c(1, 1, 1, 1),
    V2003 = c(1, 2, 3, 1),
    V2008 = c(15, 20, 10, 5),
    V20081 = c(6, 3, 9, 2),
    V20082 = c(1990, 1985, 2000, 1995),
    V2009 = c(33, 38, 23, 28)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should have exactly 2 rows (one per UPA-V1014 combination)
  expect_equal(nrow(result), 2L)

  # Should be unique by UPA-V1014
  expect_equal(nrow(unique(result[, .(UPA, V1014)])), nrow(result))
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("identify_reference_fortnight handles year boundaries", {
  # Q4 data near end of year
  test_data <- data.frame(
    Ano = c(2023, 2023),
    Trimestre = c(4, 4),
    UPA = c(1, 1),
    V1008 = c(1, 1),
    V1014 = c(1, 1),
    V2003 = c(1, 2),
    V2008 = c(25, 10),
    V20081 = c(12, 11),     # Dec and Nov birthdays
    V20082 = c(1990, 1985),
    V2009 = c(33, 38)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Should complete without error
  expect_s3_class(result, "data.table")

  # ref_fortnight_yyyyff should be in valid range for Q4 (19-24)
  valid_values <- result$ref_fortnight_yyyyff[!is.na(result$ref_fortnight_yyyyff)]
  if (length(valid_values) > 0) {
    ff <- valid_values %% 100
    expect_true(all(ff >= 19 & ff <= 24))
  }
})

test_that("identify_reference_fortnight does NOT aggregate across quarters", {
  # Data spanning multiple quarters
  test_data <- data.frame(
    Ano = c(2023, 2023, 2023, 2023),
    Trimestre = c(1, 2, 3, 4),
    UPA = rep(1, 4),
    V1008 = rep(1, 4),
    V1014 = rep(1, 4),
    V2003 = rep(1, 4),
    V2008 = c(15, 15, 15, 15),
    V20081 = c(1, 4, 7, 10),  # Birthdays in each quarter
    V20082 = rep(1990, 4),
    V2009 = rep(33, 4)
  )

  result <- identify_reference_fortnight(test_data, verbose = FALSE)

  # Fortnights do NOT aggregate across quarters (unlike months)
  # Should have one row per household-quarter (4 quarters)
  expect_equal(nrow(result), 4L)
})

# =============================================================================
# OUT-OF-QUARTER VALIDATION TESTS (Bug fix verification)
# =============================================================================

test_that("date_to_fortnight_in_quarter returns NA for dates outside quarter", {
  # April date with Q1 should return NA (April is in Q2)
  date_q2_in_q1 <- as.Date("2024-04-15")
  expect_true(is.na(PNADCperiods:::date_to_fortnight_in_quarter(date_q2_in_q1, 1L)))

  # December date with Q1 should return NA
  date_q4_in_q1 <- as.Date("2024-12-25")
  expect_true(is.na(PNADCperiods:::date_to_fortnight_in_quarter(date_q4_in_q1, 1L)))

  # January date with Q2 should return NA
  date_q1_in_q2 <- as.Date("2024-01-15")
  expect_true(is.na(PNADCperiods:::date_to_fortnight_in_quarter(date_q1_in_q2, 2L)))

  # July date with Q4 should return NA
  date_q3_in_q4 <- as.Date("2024-07-20")
  expect_true(is.na(PNADCperiods:::date_to_fortnight_in_quarter(date_q3_in_q4, 4L)))
})

test_that("date_to_fortnight_in_quarter handles quarter boundaries correctly", {
  # Last day of Q1 (March 31) should be valid for Q1
  date_q1_end <- as.Date("2024-03-31")
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date_q1_end, 1L), 6L)

  # First day of Q2 (April 1) should be NA for Q1
  date_q2_start <- as.Date("2024-04-01")
  expect_true(is.na(PNADCperiods:::date_to_fortnight_in_quarter(date_q2_start, 1L)))

  # First day of Q2 should be valid for Q2
  expect_equal(PNADCperiods:::date_to_fortnight_in_quarter(date_q2_start, 2L), 1L)
})

test_that("date_to_fortnight_in_quarter handles vector with mixed in/out of quarter", {
  dates <- as.Date(c("2024-01-15", "2024-04-15", "2024-02-20", "2024-07-01"))
  quarters <- rep(1L, 4)

  positions <- PNADCperiods:::date_to_fortnight_in_quarter(dates, quarters)

  # Jan 15 in Q1 = position 1
  expect_equal(positions[1], 1L)

  # Apr 15 in Q1 = NA (outside quarter)
  expect_true(is.na(positions[2]))

  # Feb 20 in Q1 = position 4
  expect_equal(positions[3], 4L)

  # Jul 1 in Q1 = NA (outside quarter)
  expect_true(is.na(positions[4]))
})
