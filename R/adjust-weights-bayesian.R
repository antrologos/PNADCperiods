#' Bayesian Weight Adjustment
#'
#' Adjusts individual survey weights to match smoothed monthly targets using
#' Bayesian posterior probabilities.
#'
#' @description
#' After smoothing monthly aggregates, individual weights need adjustment so that
#' when summed, they match the smoothed totals. This function uses Bayes' theorem:
#'
#' \deqn{P(status | category) = P(status) \times P(category | status) / P(category)}
#'
#' Where:
#' \itemize{
#'   \item status = employment situation (employed, unemployed, etc.)
#'   \item category = demographic/geographic cell (age x region x etc.)
#' }
#'
#' @param data A data.table with microdata and calibrated weights (output from
#'   \code{\link{calibrate_monthly_weights}}). Required columns:
#'   \itemize{
#'     \item \code{weight_calibrated}: Calibrated monthly weight
#'     \item \code{ref_month_yyyymm}: Reference month
#'     \item Employment variables: \code{VD4001}, \code{VD4002}, \code{VD4009}
#'     \item Demographics: \code{V2009}, \code{posest}, \code{posest_sxi}, \code{V2010}
#'   }
#'
#' @param monthly_targets A data.table with smoothed monthly targets (output from
#'   \code{\link{smooth_monthly_aggregates}}). Required columns:
#'   \itemize{
#'     \item \code{ref_month_yyyymm}: Month identifier
#'     \item \code{m_*}: Smoothed monthly estimates for each employment category
#'   }
#'
#' @param employment_detail Logical. If TRUE (default), use detailed employment
#'   categories including CNPJ distinction for employers and self-employed
#'   (requires \code{V4019} column for post-2015 data).
#'
#' @return A data.table with the input data plus:
#'   \itemize{
#'     \item \code{weight_monthly}: Final Bayesian-adjusted monthly weight
#'     \item \code{employment_status}: Employment situation code used in adjustment
#'   }
#'
#' @details
#' Employment status categories (sit):
#' \describe{
#'   \item{1}{Private employee with formal contract}
#'   \item{2}{Private employee without formal contract}
#'   \item{3}{Domestic worker with formal contract}
#'   \item{4}{Domestic worker without formal contract}
#'   \item{5}{Public employee with formal contract}
#'   \item{6}{Public employee without formal contract}
#'   \item{7}{Military/statutory employee}
#'   \item{8}{Employer}
#'   \item{9}{Self-employed}
#'   \item{10}{Unpaid family worker}
#'   \item{20}{Unemployed}
#'   \item{30}{Discouraged worker}
#'   \item{40}{Potential labor force (not discouraged)}
#'   \item{50}{Out of labor force}
#'   \item{60}{Children (0-13)}
#' }
#'
#' @examples
#' \dontrun{
#' # After calibrating weights and smoothing aggregates
#' final_data <- adjust_weights_bayesian(calibrated_data, smoothed_targets)
#' }
#'
#' @seealso \code{\link{calibrate_monthly_weights}}, \code{\link{smooth_monthly_aggregates}}
#'
#' @export
adjust_weights_bayesian <- function(data, monthly_targets, employment_detail = TRUE) {

  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_data_frame(monthly_targets, min.rows = 1)
  checkmate::assert_logical(employment_detail, len = 1)

  dt <- ensure_data_table(data, copy = TRUE)
  targets <- ensure_data_table(monthly_targets, copy = TRUE)

  # Step 1: Create employment status categories
  dt <- create_employment_status(dt, employment_detail)

  # Step 2: Create demographic category
  dt <- create_demographic_category(dt)

  # Step 3: Map employment status to target variables
  status_to_target <- get_status_target_mapping(employment_detail)

  # Step 4: Merge monthly targets
  target_cols <- grep("^m_", names(targets), value = TRUE)
  dt <- merge(dt, targets[, c("ref_month_yyyymm", target_cols), with = FALSE],
              by = "ref_month_yyyymm", all.x = TRUE)

  # Step 5: Calculate Bayesian adjustment
  dt <- calculate_bayesian_weights(dt, status_to_target)

  # Clean up temporary columns
  temp_cols <- c("cat", "prob_sit", "prob_cat_given_sit", "prob_cat",
                 "prob_sit_given_cat", "prob_sit_given_cat_intermed")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  # Remove target columns (they were only needed for calculation)
  dt[, (intersect(target_cols, names(dt))) := NULL]

  dt
}

#' Create Employment Status Categories
#'
#' Maps PNADC employment variables to status categories.
#'
#' @param dt data.table with PNADC variables
#' @param detailed Use detailed categories with CNPJ distinction
#' @return data.table with employment_status column
#' @keywords internal
#' @noRd
create_employment_status <- function(dt, detailed = TRUE) {

  # Initialize as NA
  dt[, employment_status := NA_integer_]

  # Children (age 0-13)
  dt[V2009 <= 13L, employment_status := 60L]

  # Employed persons (VD4002 == 1) - by position (VD4009)
  dt[VD4002 == 1L & VD4009 == 1L, employment_status := 1L]  # Private formal
  dt[VD4002 == 1L & VD4009 == 2L, employment_status := 2L]  # Private informal
  dt[VD4002 == 1L & VD4009 == 3L, employment_status := 3L]  # Domestic formal
  dt[VD4002 == 1L & VD4009 == 4L, employment_status := 4L]  # Domestic informal
  dt[VD4002 == 1L & VD4009 == 5L, employment_status := 5L]  # Public formal
  dt[VD4002 == 1L & VD4009 == 6L, employment_status := 6L]  # Public informal
  dt[VD4002 == 1L & VD4009 == 7L, employment_status := 7L]  # Military/statutory

  # Employer and self-employed - may split by CNPJ if detailed
  if (detailed && "V4019" %in% names(dt)) {
    # Employer with/without CNPJ
    dt[VD4002 == 1L & VD4009 == 8L & V4019 == 1L, employment_status := 81L]
    dt[VD4002 == 1L & VD4009 == 8L & V4019 == 2L, employment_status := 82L]
    dt[VD4002 == 1L & VD4009 == 8L & is.na(V4019), employment_status := 8L]

    # Self-employed with/without CNPJ
    dt[VD4002 == 1L & VD4009 == 9L & V4019 == 1L, employment_status := 91L]
    dt[VD4002 == 1L & VD4009 == 9L & V4019 == 2L, employment_status := 92L]
    dt[VD4002 == 1L & VD4009 == 9L & is.na(V4019), employment_status := 9L]
  } else {
    dt[VD4002 == 1L & VD4009 == 8L, employment_status := 8L]  # Employer
    dt[VD4002 == 1L & VD4009 == 9L, employment_status := 9L]  # Self-employed
  }

  # Family worker
  dt[VD4002 == 1L & VD4009 == 10L, employment_status := 10L]

  # Unemployed (VD4002 == 2)
  dt[VD4002 == 2L, employment_status := 20L]

  # Discouraged worker (VD4005 == 1)
  dt[VD4005 == 1L & is.na(employment_status), employment_status := 30L]

  # Potential labor force (VD4003 == 1, not discouraged)
  dt[VD4003 == 1L & VD4005 != 1L & is.na(employment_status), employment_status := 40L]

  # Out of labor force (everyone else age 14+)
  dt[V2009 >= 14L & is.na(employment_status), employment_status := 50L]

  dt
}

#' Create Demographic Category
#'
#' Creates a combined demographic/geographic category for Bayesian adjustment.
#'
#' @param dt data.table with demographic variables
#' @return data.table with cat column
#' @keywords internal
#' @noRd
create_demographic_category <- function(dt) {

  # Category based on posest, posest_sxi, and race (V2010)
  # This creates fine-grained cells for the Bayesian adjustment
  dt[, cat := posest * 10000L + (posest_sxi %% 1000L) * 10L + V2010]

  dt
}

#' Get Status to Target Variable Mapping
#'
#' Maps employment status codes to smoothed target variable names.
#'
#' @param detailed Use detailed categories
#' @return Named list mapping status codes to m_ variable names
#' @keywords internal
#' @noRd
get_status_target_mapping <- function(detailed = TRUE) {

  mapping <- list(
    "1" = "m_empregprivcomcart",
    "2" = "m_empregprivsemcart",
    "3" = "m_domesticocomcart",
    "4" = "m_domesticosemcart",
    "5" = "m_empregpublcomcart",
    "6" = "m_empregpublsemcart",
    "7" = "m_estatutmilitar",
    "8" = "m_empregador",
    "9" = "m_contapropria",
    "10" = "m_trabfamauxiliar",
    "20" = "m_popdesocup",
    "30" = "m_desalentado",
    "40" = "m_forcapotencial",
    "50" = "m_popforadaforca",
    "60" = "m_pop0a13"
  )

  if (detailed) {
    mapping[["81"]] <- "m_empregadorcomcnpj"
    mapping[["82"]] <- "m_empregadorsemcnpj"
    mapping[["91"]] <- "m_contapropriacomcnpj"
    mapping[["92"]] <- "m_contapropriasemcnpj"
  }

  mapping
}

#' Calculate Bayesian Weights
#'
#' Applies Bayes' theorem to adjust individual weights.
#'
#' @param dt data.table with employment_status, cat, and calibrated weights
#' @param status_mapping Status to target variable mapping
#' @return data.table with weight_monthly column
#' @keywords internal
#' @noRd
calculate_bayesian_weights <- function(dt, status_mapping) {

  # Get monthly population target
  if (!"m_populacao" %in% names(dt)) {
    warning("m_populacao not found in targets, using sum of calibrated weights")
    dt[, m_populacao := sum(weight_calibrated, na.rm = TRUE), by = ref_month_yyyymm]
  }

  # Calculate population by category (from intermediate weights)
  dt[, pop_cat := sum(weight_calibrated, na.rm = TRUE),
     by = .(ref_month_yyyymm, cat)]

  # Calculate population by status from intermediate weights
  dt[, pop_status_intermed := sum(weight_calibrated, na.rm = TRUE),
     by = .(ref_month_yyyymm, employment_status)]

  # Calculate population by category and status
  dt[, pop_cat_status := sum(weight_calibrated, na.rm = TRUE),
     by = .(ref_month_yyyymm, cat, employment_status)]

  # P(category | status) from intermediate
  dt[, prob_cat_given_sit_intermed := pop_cat_status / pop_status_intermed]
  dt[is.na(prob_cat_given_sit_intermed) | !is.finite(prob_cat_given_sit_intermed),
     prob_cat_given_sit_intermed := 0]

  # P(status | category) from intermediate
  dt[, prob_sit_given_cat_intermed := pop_cat_status / pop_cat]
  dt[is.na(prob_sit_given_cat_intermed) | !is.finite(prob_sit_given_cat_intermed),
     prob_sit_given_cat_intermed := 0]

  # Get target population for each status
  dt[, target_pop := NA_real_]
  for (status_code in names(status_mapping)) {
    target_var <- status_mapping[[status_code]]
    if (target_var %in% names(dt)) {
      # Target values are in thousands, multiply by 1000
      dt[employment_status == as.integer(status_code),
         target_pop := get(target_var) * 1000]
    }
  }

  # P(status) from targets
  dt[, prob_sit := target_pop / (m_populacao * 1000)]
  dt[is.na(prob_sit) | !is.finite(prob_sit), prob_sit := 0]

  # P(category) from intermediate
  dt[, prob_cat := pop_cat / sum(weight_calibrated, na.rm = TRUE),
     by = ref_month_yyyymm]

  # Bayesian posterior: P(status | category) = P(status) * P(cat | status) / P(cat)
  dt[, prob_sit_given_cat := prob_sit * prob_cat_given_sit_intermed / prob_cat]
  dt[is.na(prob_sit_given_cat) | !is.finite(prob_sit_given_cat),
     prob_sit_given_cat := prob_sit_given_cat_intermed]

  # Final weight adjustment
  # weight_monthly = weight_calibrated * (P(s|c) target / P(s|c) intermediate)
  dt[, weight_ratio := prob_sit_given_cat / prob_sit_given_cat_intermed]
  dt[is.na(weight_ratio) | !is.finite(weight_ratio) | weight_ratio <= 0,
     weight_ratio := 1]

  # Bound the ratio to prevent extreme adjustments
  dt[weight_ratio > 10, weight_ratio := 10]
  dt[weight_ratio < 0.1, weight_ratio := 0.1]

  dt[, weight_monthly := weight_calibrated * weight_ratio]

  # Clean up temporary columns
  temp_cols <- c("pop_cat", "pop_status_intermed", "pop_cat_status",
                 "prob_cat_given_sit_intermed", "prob_sit_given_cat_intermed",
                 "target_pop", "prob_sit", "prob_cat", "prob_sit_given_cat",
                 "weight_ratio")
  dt[, (intersect(temp_cols, names(dt))) := NULL]

  dt
}
