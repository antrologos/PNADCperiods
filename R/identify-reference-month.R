#' Identify Reference Month in PNADC Data
#'
#' Determines which month within each quarter corresponds to each survey
#' observation based on IBGE's "Parada Tecnica" (technical break) rules.
#'
#' @description
#' PNADC is a quarterly survey, but each interview actually refers to a specific
#' week within the quarter. This function identifies which month that week belongs
#' to, enabling monthly (instead of quarterly) time series analysis.
#'
#' The algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules (first Saturday with sufficient days in month)
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item UPA-panel level aggregation (everyone in same sampling unit interviewed together)
#' }
#'
#' @param data A data.frame or data.table with PNADC microdata. Required columns:
#'   \itemize{
#'     \item \code{Ano}: Survey year
#'     \item \code{Trimestre}: Quarter (1-4)
#'     \item \code{UPA}: Primary Sampling Unit
#'     \item \code{V1014}: Panel identifier
#'     \item \code{V2008}: Birth day (1-31)
#'     \item \code{V20081}: Birth month (1-12)
#'     \item \code{V20082}: Birth year
#'     \item \code{V2009}: Age
#'   }
#'   Optional but recommended for complete crosswalk:
#'   \itemize{
#'     \item \code{V1008}: Household sequence within UPA
#'     \item \code{V2003}: Person sequence within household
#'   }
#'
#' @param exception_quarters Character vector of quarters with relaxed timing rules,
#'   in format "YYYYtQ" (e.g., "2016t3"). If NULL (default), uses the built-in list
#'   of known exceptions: 2016t3, 2016t4, 2017t2, 2022t3, 2023t2.
#'
#' @return A data.table with the original key columns plus:
#'   \itemize{
#'     \item \code{ref_month}: Reference month as Date (first day of month, e.g., "2023-01-01")
#'     \item \code{ref_month_in_quarter}: Position in quarter (1, 2, 3) or NA if indeterminate
#'     \item \code{ref_month_yyyymm}: Integer YYYYMM format (e.g., 202301)
#'   }
#'
#' @details
#' The determination rate (proportion of observations with identified reference month)
#' is typically 85-90%. Observations that cannot be determined are marked with NA.
#'
#' The function processes data in the following steps:
#' \enumerate{
#'   \item Calculate first valid interview Saturday for each month in quarter
#'   \item For each person, calculate possible interview date range based on birthday constraints
#'   \item Convert date ranges to month-in-quarter positions
#'   \item Aggregate to UPA-panel level (all persons must agree)
#'   \item Handle exception quarters with relaxed timing rules
#' }
#'
#' @examples
#' \dontrun{
#' # Identify reference months
#' result <- identify_reference_month(pnadc_data)
#'
#' # Check determination rate
#' result[, .(
#'   total = .N,
#'   determined = sum(!is.na(ref_month_in_quarter)),
#'   rate = mean(!is.na(ref_month_in_quarter))
#' ), by = .(Ano, Trimestre)]
#' }
#'
#' @seealso \code{\link{get_exception_quarters}} for the list of exception quarters
#'
#' @export
identify_reference_month <- function(data, exception_quarters = NULL) {

  # Validate input
  validate_pnadc(data, check_weights = FALSE)

  # Convert to data.table (copy to avoid modifying original)
  dt <- ensure_data_table(data, copy = TRUE)

  # Use default exception quarters if not provided
  if (is.null(exception_quarters)) {
    exception_quarters <- get_exception_quarters()
  }

  # Create quarter identifier for exception matching
  dt[, quarter_id := paste0(Ano, "t", Trimestre)]

  # Step 1: Calculate first valid Saturday for each month of the quarter
  # Using standard rule (min_days = 4)
  dt[, `:=`(
    first_sat_m1 = first_valid_saturday(Ano, quarter_months(Trimestre)[1], min_days = 4L),
    first_sat_m2 = first_valid_saturday(Ano, quarter_months(Trimestre)[2], min_days = 4L),
    first_sat_m3 = first_valid_saturday(Ano, quarter_months(Trimestre)[3], min_days = 4L)
  )]

  # Step 2: Calculate birthday in survey year and first Saturday after birthday
  dt[, birthday := make_birthday(V20081, V2008, Ano)]
  dt[, first_sat_after_birthday := first_saturday_on_or_after(birthday)]

  # Determine if interview was before or after birthday
  # visitapreaniv: 0 = interviewed after/on birthday, 1 = interviewed before birthday
  dt[, visit_before_birthday := as.integer((Ano - V20082) - V2009)]

  # Step 3: Calculate interview date bounds
  # Get month numbers for the quarter
  dt[, `:=`(
    month1 = quarter_months(Trimestre)[1],
    month2 = quarter_months(Trimestre)[2],
    month3 = quarter_months(Trimestre)[3]
  )]

  # Minimum interview date: first Saturday of first month of quarter
  dt[, date_min := make_date(Ano, month1, first_sat_m1)]

  # Maximum interview date: first Saturday of third month + 21 days
  dt[, date_max := make_date(Ano, month3, first_sat_m3) + 21L]

  # Apply birthday constraints
  # If interviewed after birthday and birthday falls in interview window: start from birthday
  dt[visit_before_birthday == 0L &
       first_sat_after_birthday > date_min &
       first_sat_after_birthday <= date_max,
     date_min := first_sat_after_birthday]

  # If interviewed before birthday and birthday would fall in window: end before birthday
  dt[visit_before_birthday == 1L &
       (first_sat_after_birthday - 7L) < date_max &
       (first_sat_after_birthday - 7L) >= date_min,
     date_max := first_sat_after_birthday - 7L]

  # Step 4: Convert date bounds to month-in-quarter positions
  dt[, month_min_pos := calculate_month_position(date_min, Ano, Trimestre)]
  dt[, month_max_pos := calculate_month_position(date_max, Ano, Trimestre)]

  # Step 5: Aggregate to UPA-panel level
  # All persons in same (UPA, V1014, Ano, Trimestre) must have same reference month
  dt[, `:=`(
    upa_month_min = max(month_min_pos, na.rm = TRUE),
    upa_month_max = min(month_max_pos, na.rm = TRUE)
  ), by = .(Ano, Trimestre, UPA, V1014)]

  # Assign reference month: if min == max, that's the month; otherwise indeterminate
  dt[, ref_month_in_quarter := ifelse(
    upa_month_min == upa_month_max & upa_month_min >= 1L & upa_month_max <= 3L,
    upa_month_min,
    NA_integer_
  )]

  # Step 6: Handle exception quarters
  # Try relaxed rule (min_days = 3) for quarters that have issues
  dt <- apply_exception_rules(dt, exception_quarters)

  # Step 7: Calculate final reference month Date and YYYYMM
  dt[!is.na(ref_month_in_quarter), `:=`(
    ref_month = make_date(Ano, quarter_months(Trimestre)[ref_month_in_quarter], 1L),
    ref_month_yyyymm = yyyymm(Ano, quarter_months(Trimestre)[ref_month_in_quarter])
  )]

  dt[is.na(ref_month_in_quarter), `:=`(
    ref_month = as.Date(NA),
    ref_month_yyyymm = NA_integer_
  )]

  # Select output columns
  # Include all available join keys
  key_cols <- intersect(join_key_vars(), names(dt))
  output_cols <- c(key_cols, "ref_month", "ref_month_in_quarter", "ref_month_yyyymm")

  result <- dt[, ..output_cols]

  # Add class for nice printing
  class(result) <- c("pnadc_crosswalk", class(result))
  attr(result, "determination_rate") <- mean(!is.na(result$ref_month_in_quarter))

  result
}

#' Calculate Month Position in Quarter
#'
#' Converts a date to its month position within the quarter (1, 2, or 3).
#' Handles edge cases where dates fall in the first few days of a month.
#'
#' @param date Date vector
#' @param year Integer year vector
#' @param quarter Integer quarter vector (1-4)
#' @return Integer vector (1, 2, 3, or NA)
#' @keywords internal
#' @noRd
calculate_month_position <- function(date, year, quarter) {
  # Get the first month of the quarter
  first_month <- quarter_months(quarter)[1]

  # Extract month from date
  date_month <- as.integer(format(date, "%m"))
  date_day <- as.integer(format(date, "%d"))

  # Calculate position (1, 2, or 3)
  pos <- date_month - first_month + 1L

  # Adjustment for dates in first 3 days of month (they might belong to previous month's week)
  # If day <= 3 and position > 1, the interview might have been in the previous month
  pos <- ifelse(date_day <= 3L & pos > 1L, pos - 1L, pos)

  # Clamp to valid range
  pos <- ifelse(pos < 1L, 1L, ifelse(pos > 3L, 3L, pos))

  pos
}

#' Apply Exception Rules for Specific Quarters
#'
#' For quarters where the standard rule produces impossible results, apply
#' relaxed timing rules (min_days = 3 instead of 4).
#'
#' @param dt data.table with standard rule results
#' @param exception_quarters Character vector of exception quarters ("YYYYtQ")
#' @return data.table with exception rules applied
#' @keywords internal
#' @noRd
apply_exception_rules <- function(dt, exception_quarters) {

  # Identify observations in exception quarters that are currently indeterminate
  in_exception <- dt$quarter_id %in% exception_quarters
  currently_na <- is.na(dt$ref_month_in_quarter)
  needs_exception <- in_exception & currently_na

  if (!any(needs_exception)) {
    return(dt)
  }

  # For these observations, recalculate with relaxed rule
  exc_dt <- dt[needs_exception]

  # Recalculate first Saturdays with min_days = 3
  exc_dt[, `:=`(
    first_sat_m1_exc = first_valid_saturday(Ano, quarter_months(Trimestre)[1], min_days = 3L),
    first_sat_m2_exc = first_valid_saturday(Ano, quarter_months(Trimestre)[2], min_days = 3L),
    first_sat_m3_exc = first_valid_saturday(Ano, quarter_months(Trimestre)[3], min_days = 3L)
  )]

  # Recalculate date bounds
  exc_dt[, date_min_exc := make_date(Ano, month1, first_sat_m1_exc)]
  exc_dt[, date_max_exc := make_date(Ano, month3, first_sat_m3_exc) + 21L]

  # Apply birthday constraints (same logic as before)
  exc_dt[visit_before_birthday == 0L &
           first_sat_after_birthday > date_min_exc &
           first_sat_after_birthday <= date_max_exc,
         date_min_exc := first_sat_after_birthday]

  exc_dt[visit_before_birthday == 1L &
           (first_sat_after_birthday - 7L) < date_max_exc &
           (first_sat_after_birthday - 7L) >= date_min_exc,
         date_max_exc := first_sat_after_birthday - 7L]

  # Calculate positions with exception rule
  exc_dt[, month_min_pos_exc := calculate_month_position(date_min_exc, Ano, Trimestre)]
  exc_dt[, month_max_pos_exc := calculate_month_position(date_max_exc, Ano, Trimestre)]

  # Aggregate to UPA-panel level
  exc_dt[, `:=`(
    upa_month_min_exc = max(month_min_pos_exc, na.rm = TRUE),
    upa_month_max_exc = min(month_max_pos_exc, na.rm = TRUE)
  ), by = .(Ano, Trimestre, UPA, V1014)]

  # Determine if exception rule resolves the issue
  exc_dt[, ref_month_in_quarter_exc := ifelse(
    upa_month_min_exc == upa_month_max_exc &
      upa_month_min_exc >= 1L & upa_month_max_exc <= 3L,
    upa_month_min_exc,
    NA_integer_
  )]

  # Update original dt where exception rule works
  resolved <- !is.na(exc_dt$ref_month_in_quarter_exc)

  if (any(resolved)) {
    # Get the row indices in original dt that need updating
    # We need a way to match back - use a temporary ID
    dt[, .row_id := .I]
    exc_dt[, .row_id := dt[needs_exception, .row_id]]

    # Update only rows where exception resolved the issue
    resolved_rows <- exc_dt[resolved, .row_id]
    resolved_values <- exc_dt[resolved, ref_month_in_quarter_exc]

    dt[.row_id %in% resolved_rows,
       ref_month_in_quarter := resolved_values[match(.row_id, resolved_rows)]]

    dt[, .row_id := NULL]
  }

  dt
}

#' Get Exception Quarters
#'
#' Returns the list of quarters where IBGE used non-standard technical break timing.
#' In these quarters, the first reference week of a month only needs 3 days
#' (instead of the usual 4) within that month.
#'
#' @return Character vector of exception quarters in "YYYYtQ" format
#'
#' @details
#' The exception quarters are documented in IBGE's methodology notes:
#' \itemize{
#'   \item 2016t3: September 25 - October 1 technical break
#'   \item 2016t4: December 25-31 technical break
#'   \item 2017t2: June 25 - July 1 technical break
#'   \item 2022t3: September 25 - October 1 technical break
#'   \item 2023t2: June 25 - July 1 technical break
#' }
#'
#' @examples
#' get_exception_quarters()
#' # [1] "2016t3" "2016t4" "2017t2" "2022t3" "2023t2"
#'
#' @export
get_exception_quarters <- function() {
  c("2016t3", "2016t4", "2017t2", "2022t3", "2023t2")
}

#' Print Method for pnadc_crosswalk
#'
#' @param x A pnadc_crosswalk object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns x
#' @export
print.pnadc_crosswalk <- function(x, ...) {
  cat("PNADC Reference Month Crosswalk\n")
  cat("-------------------------------\n")
  cat("Observations:", format(nrow(x), big.mark = ","), "\n")

  det_rate <- attr(x, "determination_rate")
  if (!is.null(det_rate)) {
    cat("Determination rate:", sprintf("%.1f%%", det_rate * 100), "\n")
  }

  if (nrow(x) > 0 && "ref_month_yyyymm" %in% names(x)) {
    date_range <- range(x$ref_month_yyyymm, na.rm = TRUE)
    if (!any(is.na(date_range))) {
      cat("Date range:", date_range[1], "-", date_range[2], "\n")
    }
  }

  cat("\nJoin keys:", paste(intersect(join_key_vars(), names(x)), collapse = ", "), "\n")
  cat("Output columns:", paste(setdiff(names(x), join_key_vars()), collapse = ", "), "\n")

  invisible(x)
}
