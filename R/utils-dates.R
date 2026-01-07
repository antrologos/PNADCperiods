#' @title Date Utility Functions
#' @description Internal helper functions for date calculations, designed to
#'   match Stata's date handling conventions.
#' @name utils-dates
#' @keywords internal
NULL

#' Day of Week (Stata-compatible)
#'
#' Returns the day of week for a date, matching Stata's `dow()` function.
#'
#' @param date A Date object or vector of Dates
#' @return Integer vector: 0 = Sunday, 1 = Monday, ..., 6 = Saturday
#' @keywords internal
#' @noRd
dow <- function(date) {

  as.integer(format(date, "%w"))
}

#' First Day of Month
#'
#' Returns the first day of the month for a given date.
#'
#' @param date A Date object or vector of Dates
#' @return Date vector with day set to 1
#' @keywords internal
#' @noRd
first_of_month <- function(date) {
  as.Date(paste(format(date, "%Y-%m"), "01", sep = "-"))
}

#' Create Date from Year, Month, Day
#'
#' Safely creates Date objects, handling invalid dates (e.g., Feb 29 in
#' non-leap years) by returning NA.
#'
#' @param year Integer vector of years
#' @param month Integer vector of months (1-12)
#' @param day Integer vector of days (1-31)
#' @return Date vector (NA for invalid dates)
#' @keywords internal
#' @noRd
make_date <- function(year, month, day) {

  # Handle vectorized input
n <- max(length(year), length(month), length(day))
  year <- rep_len(year, n)
  month <- rep_len(month, n)
  day <- rep_len(day, n)

  # Create date strings
  date_str <- sprintf("%04d-%02d-%02d", year, month, day)

  # Parse with NA for invalid dates
  result <- as.Date(date_str, format = "%Y-%m-%d")

  # Validate - dates that parsed but are wrong (e.g., Feb 30 -> Mar 2)
  # should be NA
  valid_year <- as.integer(format(result, "%Y"))
  valid_month <- as.integer(format(result, "%m"))
  valid_day <- as.integer(format(result, "%d"))

  mismatch <- !is.na(result) & (valid_year != year | valid_month != month | valid_day != day)
  result[mismatch] <- NA

result
}

#' Handle February 29 Birthdays
#'
#' For leap year birthdays (Feb 29), returns March 1 in non-leap years.
#' This matches the Stata code's handling of these edge cases.
#'
#' @param birth_month Integer vector of birth months
#' @param birth_day Integer vector of birth days
#' @param year Integer vector of years to create dates for
#' @return Date vector of birthdays in the given year
#' @keywords internal
#' @noRd
make_birthday <- function(birth_month, birth_day, year) {
  n <- max(length(birth_month), length(birth_day), length(year))
  birth_month <- rep_len(birth_month, n)
  birth_day <- rep_len(birth_day, n)
  year <- rep_len(year, n)

  # First try to create the date normally
  result <- make_date(year, birth_month, birth_day)

  # For Feb 29 in non-leap years, use March 1
  feb29 <- birth_month == 2L & birth_day == 29L & is.na(result)
  result[feb29] <- make_date(year[feb29], 3L, 1L)

  result
}

#' Is Leap Year
#'
#' Check if a year is a leap year.
#'
#' @param year Integer vector of years
#' @return Logical vector
#' @keywords internal
#' @noRd
is_leap_year <- function(year) {
  (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

#' Get Quarter Months
#'
#' Returns the three months of a quarter.
#'
#' @param quarter Integer quarter (1-4)
#' @return Integer vector of length 3 with month numbers
#' @keywords internal
#' @noRd
quarter_months <- function(quarter) {
  (quarter - 1L) * 3L + 1L:3L
}

#' First Day of Quarter
#'
#' Returns the first day of a quarter.
#'
#' @param year Integer year
#' @param quarter Integer quarter (1-4)
#' @return Date
#' @keywords internal
#' @noRd
first_of_quarter <- function(year, quarter) {
  make_date(year, quarter_months(quarter)[1], 1L)
}

#' Create Year-Month Integer (YYYYMM format)
#'
#' Creates an integer in YYYYMM format from year and month.
#'
#' @param year Integer year
#' @param month Integer month (1-12)
#' @return Integer in YYYYMM format (e.g., 202301 for January 2023)
#' @keywords internal
#' @noRd
yyyymm <- function(year, month) {
  as.integer(year * 100L + month)
}

#' Extract Year-Month from Date
#'
#' Extracts YYYYMM integer from a Date.
#'
#' @param date Date vector
#' @return Integer vector in YYYYMM format
#' @keywords internal
#' @noRd
date_to_yyyymm <- function(date) {
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  yyyymm(year, month)
}

#' First Saturday of Month (with minimum days constraint)
#'
#' Calculates the day of the month for the first Saturday that has at least
#' `min_days` in the reference week within that month. This implements IBGE's
#' "Parada Tecnica" rule.
#'
#' The reference week is Saturday through Friday. For the first week of a month
#' to be valid, it must have enough days within that month.
#'
#' @param year Integer year
#' @param month Integer month (1-12)
#' @param min_days Integer minimum days required (default 4, use 3 for exception quarters)
#' @return Integer day of month for the first valid Saturday
#' @keywords internal
#' @noRd
first_valid_saturday <- function(year, month, min_days = 4L) {
  # Get first day of the month
  first_day <- make_date(year, month, 1L)

  # Day of week for first of month (0=Sun, 6=Sat)
  first_dow <- dow(first_day)

  # Calculate first Saturday
  # If first_dow = 0 (Sunday), first Saturday is day 7
  # If first_dow = 1 (Monday), first Saturday is day 6
  # ...
  # If first_dow = 6 (Saturday), first Saturday is day 1
  days_to_saturday <- (6L - first_dow) %% 7L
  first_saturday_day <- 1L + days_to_saturday

  # How many days of that week are in this month?
  # The week is Saturday (first_saturday_day) through Friday (first_saturday_day + 6)
  # Days in month from Saturday: min(first_saturday_day + 6, days_in_month) - first_saturday_day + 1
  # But we only care about days from the 1st: so it's first_saturday_day days

  # Actually, simpler: if first_saturday_day <= (7 - min_days + 1), there are enough days
  # With min_days = 4: first_saturday_day <= 4 means Saturday is day 1,2,3,4
  #   Day 1: 7 days in month, Day 2: 6 days, Day 3: 5 days, Day 4: 4 days (ok)
  # With min_days = 3: first_saturday_day <= 5 means Saturday is day 1,2,3,4,5
  #   Day 5: 3 days (ok)

  threshold <- 8L - min_days  # 4 for min_days=4, 5 for min_days=3

  # If first Saturday has enough days, use it; otherwise use second Saturday
  ifelse(first_saturday_day <= threshold, first_saturday_day, first_saturday_day + 7L)
}

#' First Saturday After or On a Date
#'
#' Finds the first Saturday that is on or after the given date.
#'
#' @param date Date vector
#' @return Date vector of Saturdays
#' @keywords internal
#' @noRd
first_saturday_on_or_after <- function(date) {
  dow_date <- dow(date)
  days_to_add <- (6L - dow_date) %% 7L
  date + days_to_add
}

#' Month Position in Quarter
#'
#' Returns which month within the quarter (1, 2, or 3) a date falls in.
#'
#' @param date Date vector
#' @return Integer vector (1, 2, or 3)
#' @keywords internal
#' @noRd
month_in_quarter <- function(date) {
  month <- as.integer(format(date, "%m"))
  ((month - 1L) %% 3L) + 1L
}

#' Quarter from Month
#'
#' Returns the quarter (1-4) for a given month.
#'
#' @param month Integer month (1-12)
#' @return Integer quarter (1-4)
#' @keywords internal
#' @noRd
month_to_quarter <- function(month) {
  ((month - 1L) %/% 3L) + 1L
}
