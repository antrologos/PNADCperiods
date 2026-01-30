# Tests for date utility functions

test_that("dow returns correct day of week", {
  # Sunday = 0
 expect_equal(dow(as.Date("2024-01-07")), 0)  # Sunday
  # Monday = 1
  expect_equal(dow(as.Date("2024-01-08")), 1)  # Monday
  # Saturday = 6
  expect_equal(dow(as.Date("2024-01-06")), 6)  # Saturday
})

test_that("is_leap_year identifies leap years correctly", {
  expect_true(is_leap_year(2024))
  expect_true(is_leap_year(2000))
  expect_false(is_leap_year(2023))
  expect_false(is_leap_year(1900))
})

test_that("yyyymm creates correct format", {
  expect_equal(yyyymm(2024, 1), 202401L)
  expect_equal(yyyymm(2024, 12), 202412L)
  expect_equal(yyyymm(2023, 6), 202306L)
})

test_that("first_valid_saturday calculates correctly", {
  # January 2024: starts on Monday (dow=1)
  # First Saturday is day 6
  # With min_days=4, need at least 4 days in week, so day 6 is OK (6 days in month)
  expect_equal(first_valid_saturday(2024, 1, min_days = 4), 6)

  # March 2024: starts on Friday (dow=5)
  # First Saturday is day 2 (only 2 days in March)
  # With min_days=4, need second Saturday (day 9)
  expect_equal(first_valid_saturday(2024, 3, min_days = 4), 9)

  # With min_days=3, first Saturday (day 2) might work
  # 2 days is less than 3, so still need second Saturday
  expect_equal(first_valid_saturday(2024, 3, min_days = 3), 9)
})

test_that("first_saturday_on_or_after works correctly", {
  # If date is Saturday, return same date
  expect_equal(first_saturday_on_or_after(as.Date("2024-01-06")), as.Date("2024-01-06"))

  # If date is Sunday, return next Saturday (6 days later)
  expect_equal(first_saturday_on_or_after(as.Date("2024-01-07")), as.Date("2024-01-13"))

  # If date is Monday, return Saturday (5 days later)
  expect_equal(first_saturday_on_or_after(as.Date("2024-01-08")), as.Date("2024-01-13"))
})

# =============================================================================
# DAY OF WEEK EDGE CASES
# =============================================================================

test_that("dow handles year boundaries correctly", {
  # Test year transitions
  expect_equal(dow(as.Date("2023-12-31")), 0)  # Sunday
  expect_equal(dow(as.Date("2024-01-01")), 1)  # Monday

  # Test epoch date (1970-01-01 = Thursday)
  expect_equal(dow(as.Date("1970-01-01")), 4)  # Thursday
})

test_that("dow works for all days of the week", {
  # Test a complete week in January 2024
  expect_equal(dow(as.Date("2024-01-07")), 0)  # Sunday
  expect_equal(dow(as.Date("2024-01-08")), 1)  # Monday
  expect_equal(dow(as.Date("2024-01-09")), 2)  # Tuesday
  expect_equal(dow(as.Date("2024-01-10")), 3)  # Wednesday
  expect_equal(dow(as.Date("2024-01-11")), 4)  # Thursday
  expect_equal(dow(as.Date("2024-01-12")), 5)  # Friday
  expect_equal(dow(as.Date("2024-01-06")), 6)  # Saturday
})

# =============================================================================
# LEAP YEAR EDGE CASES
# =============================================================================

test_that("is_leap_year handles century years correctly", {
  # Century years divisible by 400 are leap years
  expect_true(is_leap_year(2000))
  expect_true(is_leap_year(2400))

  # Century years not divisible by 400 are NOT leap years
  expect_false(is_leap_year(1900))
  expect_false(is_leap_year(2100))
  expect_false(is_leap_year(2200))
  expect_false(is_leap_year(2300))
})

test_that("is_leap_year handles standard leap year pattern", {
  # Regular leap years (divisible by 4, not century)
  expect_true(is_leap_year(2020))
  expect_true(is_leap_year(2024))
  expect_true(is_leap_year(2028))

  # Non-leap years
  expect_false(is_leap_year(2021))
  expect_false(is_leap_year(2022))
  expect_false(is_leap_year(2023))
})

# =============================================================================
# QUARTER HELPER FUNCTIONS
# =============================================================================

test_that("quarter_first_month returns correct first month", {
  expect_equal(PNADCperiods:::quarter_first_month(1), 1)   # Q1 starts in Jan
  expect_equal(PNADCperiods:::quarter_first_month(2), 4)   # Q2 starts in Apr
  expect_equal(PNADCperiods:::quarter_first_month(3), 7)   # Q3 starts in Jul
  expect_equal(PNADCperiods:::quarter_first_month(4), 10)  # Q4 starts in Oct
})

test_that("quarter_month_n returns correct n-th month of quarter", {
  # Q1 (Jan-Mar)
  expect_equal(PNADCperiods:::quarter_month_n(1, 1), 1)  # 1st month = Jan
  expect_equal(PNADCperiods:::quarter_month_n(1, 2), 2)  # 2nd month = Feb
  expect_equal(PNADCperiods:::quarter_month_n(1, 3), 3)  # 3rd month = Mar

  # Q4 (Oct-Dec)
  expect_equal(PNADCperiods:::quarter_month_n(4, 1), 10)  # 1st month = Oct
  expect_equal(PNADCperiods:::quarter_month_n(4, 2), 11)  # 2nd month = Nov
  expect_equal(PNADCperiods:::quarter_month_n(4, 3), 12)  # 3rd month = Dec
})

# =============================================================================
# YYYYMM FORMAT EDGE CASES
# =============================================================================

test_that("yyyymm handles edge cases", {
  # Year boundaries
  expect_equal(yyyymm(1999, 12), 199912L)
  expect_equal(yyyymm(2000, 1), 200001L)

  # Recent years
  expect_equal(yyyymm(2023, 1), 202301L)
  expect_equal(yyyymm(2024, 12), 202412L)
})

# =============================================================================
# ISO WEEK FUNCTIONS
# =============================================================================

test_that("iso_week_year handles year boundary edge cases", {
  # Dec 31, 2020 is week 53 of ISO year 2020
  # (2020 is a leap year starting on Wednesday, so has 53 weeks)
  result_20201231 <- PNADCperiods:::iso_week_year(as.Date("2020-12-31"))
  expect_equal(result_20201231$year, 2020)  # ISO year 2020
  expect_equal(result_20201231$week, 53)     # Week 53

  # Jan 1, 2021 is week 53 of ISO year 2020
  # (belongs to previous ISO year)
  result_20210101 <- PNADCperiods:::iso_week_year(as.Date("2021-01-01"))
  expect_equal(result_20210101$year, 2020)  # ISO year 2020
  expect_equal(result_20210101$week, 53)     # Week 53

  # Jan 4 is always in week 1 of the current year (ISO 8601 rule)
  result_20210104 <- PNADCperiods:::iso_week_year(as.Date("2021-01-04"))
  expect_equal(result_20210104$year, 2021)  # ISO year 2021
  expect_equal(result_20210104$week, 1)      # Week 1
})

test_that("iso_week extracts week number correctly", {
  # Mid-year date
  expect_equal(PNADCperiods:::iso_week(as.Date("2024-06-15")), 24)

  # Jan 4 is always week 1
  expect_equal(PNADCperiods:::iso_week(as.Date("2024-01-04")), 1)

  # Year boundary (Dec 31 might be week 52 or 53 or week 1 of next year)
  week_dec31 <- PNADCperiods:::iso_week(as.Date("2024-12-31"))
  expect_true(week_dec31 %in% c(1, 52, 53))
})

test_that("iso_year extracts ISO year correctly", {
  # Mid-year - ISO year equals calendar year
  expect_equal(PNADCperiods:::iso_year(as.Date("2024-06-15")), 2024)

  # Jan 1 might belong to previous ISO year
  iso_yr_jan1 <- PNADCperiods:::iso_year(as.Date("2024-01-01"))
  expect_true(iso_yr_jan1 %in% c(2023, 2024))

  # Dec 31 might belong to next ISO year
  iso_yr_dec31 <- PNADCperiods:::iso_year(as.Date("2024-12-31"))
  expect_true(iso_yr_dec31 %in% c(2024, 2025))
})

# =============================================================================
# FIRST SATURDAY EDGE CASES
# =============================================================================

test_that("first_saturday_on_or_after handles month boundaries", {
  # Last day of month
  expect_equal(
    first_saturday_on_or_after(as.Date("2024-01-31")),  # Wednesday
    as.Date("2024-02-03")  # Next Saturday in February
  )

  # Last day of December
  expect_equal(
    first_saturday_on_or_after(as.Date("2024-12-31")),  # Tuesday
    as.Date("2025-01-04")  # Next Saturday in January
  )
})

test_that("first_saturday_on_or_after handles Friday edge case", {
  # Friday should return next day (Saturday)
  expect_equal(
    first_saturday_on_or_after(as.Date("2024-01-05")),  # Friday
    as.Date("2024-01-06")  # Saturday
  )
})

test_that("first_valid_saturday handles leap year February", {
  # February 2024 (leap year) starts on Thursday (dow=4)
  # First Saturday is Feb 3 (day 3)
  # 3 days from start of month, so min_days=4 would skip to day 10
  expect_equal(first_valid_saturday(2024, 2, min_days = 4), 10)

  # February 2023 (non-leap year) starts on Wednesday (dow=3)
  # First Saturday is Feb 4 (day 4)
  # 4 days from start, exactly meets min_days=4
  expect_equal(first_valid_saturday(2023, 2, min_days = 4), 4)
})

test_that("first_valid_saturday handles month starting on Saturday", {
  # April 2023 starts on Saturday
  # First Saturday is day 1, which has only 1 day in month
  # With min_days=4, should skip to second Saturday (day 8)
  expect_equal(first_valid_saturday(2023, 4, min_days = 4), 8)
})

# =============================================================================
# MONTH POSITION FUNCTIONS (INTERNAL)
# =============================================================================

test_that("calculate_month_position_min handles quarter boundaries", {
  # These functions are internal but critical for period identification
  # Test with dates at start of quarters

  # Q1 2024: Jan 1 is a Monday
  # First valid Saturday with min_days=4 determines month position
  result <- PNADCperiods:::calculate_month_position_min(
    as.Date("2024-01-06"),  # First Saturday
    2024, 1
  )
  expect_equal(result, 1)  # Should be month 1

  # Later date in same quarter
  result <- PNADCperiods:::calculate_month_position_min(
    as.Date("2024-03-15"),
    2024, 1
  )
  expect_equal(result, 3)  # Should be month 3
})

test_that("calculate_month_position_max handles quarter boundaries", {
  # Test with dates at end of quarters

  # Early date in quarter
  result <- PNADCperiods:::calculate_month_position_max(
    as.Date("2024-01-15"),
    2024, 1
  )
  expect_equal(result, 1)  # Should be month 1

  # Late date in quarter
  result <- PNADCperiods:::calculate_month_position_max(
    as.Date("2024-03-28"),
    2024, 1
  )
  expect_equal(result, 3)  # Should be month 3
})
