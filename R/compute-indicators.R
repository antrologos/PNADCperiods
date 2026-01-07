#' Compute Labor Force Indicators
#'
#' Calculates standard PNADC labor force indicators from weighted microdata.
#'
#' @description
#' Computes employment rates, unemployment rates, and other labor market
#' indicators using survey weights. Can compute indicators by any grouping
#' variable (typically month).
#'
#' @param data A data.frame or data.table with PNADC microdata and weights.
#'   Required columns depend on which indicators are requested.
#'
#' @param weight_var Character. Name of the weight variable to use.
#'   Default is "weight_monthly".
#'
#' @param by Character vector of grouping variables.
#'   Default is "ref_month_yyyymm" for monthly indicators.
#'
#' @param indicators Character vector of indicators to compute. Options:
#'   \itemize{
#'     \item "population": Total population, age 14+, labor force, etc.
#'     \item "employment": Employment by type (formal, informal, etc.)
#'     \item "sector": Employment by economic sector
#'     \item "rates": Unemployment rate, participation rate, etc
#'     \item "income": Income aggregates and averages
#'     \item "all": All indicators (default)
#'   }
#'
#' @return A data.table with requested indicators aggregated by the grouping variables.
#'
#' @details
#' Population indicators:
#' \itemize{
#'   \item \code{pop_total}: Total population
#'   \item \code{pop_14plus}: Population age 14+
#'   \item \code{pop_employed}: Employed persons
#'   \item \code{pop_unemployed}: Unemployed persons
#'   \item \code{pop_labor_force}: Labor force (employed + unemployed)
#'   \item \code{pop_out_of_lf}: Out of labor force
#' }
#'
#' Rate indicators:
#' \itemize{
#'   \item \code{rate_participation}: Labor force / pop 14+ (%)
#'   \item \code{rate_employment}: Employed / pop 14+ (%)
#'   \item \code{rate_unemployment}: Unemployed / labor force (%)
#' }
#'
#' @examples
#' \dontrun{
#' # Monthly indicators
#' indicators <- compute_labor_indicators(weighted_data)
#'
#' # Indicators by state
#' by_state <- compute_labor_indicators(weighted_data, by = c("ref_month_yyyymm", "UF"))
#' }
#'
#' @export
compute_labor_indicators <- function(data,
                                      weight_var = "weight_monthly",
                                      by = "ref_month_yyyymm",
                                      indicators = "all") {

  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(weight_var)
  checkmate::assert_character(by, min.len = 1)
  checkmate::assert_character(indicators, min.len = 1)

  dt <- ensure_data_table(data, copy = FALSE)

  # Check weight variable exists
  if (!weight_var %in% names(dt)) {
    stop("Weight variable '", weight_var, "' not found in data")
  }

  # Determine which indicator groups to compute
  all_groups <- c("population", "employment", "sector", "rates", "income")
  if ("all" %in% indicators) {
    indicator_groups <- all_groups
  } else {
    indicator_groups <- intersect(indicators, all_groups)
  }

  # Create indicator columns as needed
  dt <- create_indicator_columns(dt)

  # Initialize results
  results <- list()

  # Compute population indicators
  if ("population" %in% indicator_groups) {
    pop <- dt[, .(
      pop_total = sum(get(weight_var), na.rm = TRUE),
      pop_14plus = sum(get(weight_var) * (V2009 >= 14), na.rm = TRUE),
      pop_employed = sum(get(weight_var) * (VD4002 == 1), na.rm = TRUE),
      pop_unemployed = sum(get(weight_var) * (VD4002 == 2), na.rm = TRUE),
      pop_labor_force = sum(get(weight_var) * (VD4001 == 1), na.rm = TRUE),
      pop_out_of_lf = sum(get(weight_var) * (VD4001 == 2 & V2009 >= 14), na.rm = TRUE),
      pop_potential_lf = sum(get(weight_var) * (VD4003 == 1), na.rm = TRUE),
      pop_discouraged = sum(get(weight_var) * (VD4005 == 1), na.rm = TRUE)
    ), by = by]
    results$population <- pop
  }

  # Compute employment by type
  if ("employment" %in% indicator_groups) {
    emp <- dt[, .(
      emp_private_formal = sum(get(weight_var) * (VD4009 == 1), na.rm = TRUE),
      emp_private_informal = sum(get(weight_var) * (VD4009 == 2), na.rm = TRUE),
      emp_domestic_formal = sum(get(weight_var) * (VD4009 == 3), na.rm = TRUE),
      emp_domestic_informal = sum(get(weight_var) * (VD4009 == 4), na.rm = TRUE),
      emp_public_formal = sum(get(weight_var) * (VD4009 == 5), na.rm = TRUE),
      emp_public_informal = sum(get(weight_var) * (VD4009 == 6), na.rm = TRUE),
      emp_military = sum(get(weight_var) * (VD4009 == 7), na.rm = TRUE),
      emp_employer = sum(get(weight_var) * (VD4009 == 8), na.rm = TRUE),
      emp_self_employed = sum(get(weight_var) * (VD4009 == 9), na.rm = TRUE),
      emp_family_worker = sum(get(weight_var) * (VD4009 == 10), na.rm = TRUE),
      emp_with_ss = sum(get(weight_var) * (VD4012 == 1), na.rm = TRUE)
    ), by = by]
    results$employment <- emp
  }

  # Compute employment by sector
  if ("sector" %in% indicator_groups && "VD4010" %in% names(dt)) {
    sector <- dt[, .(
      sec_agriculture = sum(get(weight_var) * (VD4010 == 1), na.rm = TRUE),
      sec_manufacturing = sum(get(weight_var) * (VD4010 == 2), na.rm = TRUE),
      sec_construction = sum(get(weight_var) * (VD4010 == 3), na.rm = TRUE),
      sec_commerce = sum(get(weight_var) * (VD4010 == 4), na.rm = TRUE),
      sec_transport = sum(get(weight_var) * (VD4010 == 5), na.rm = TRUE),
      sec_food_accom = sum(get(weight_var) * (VD4010 == 6), na.rm = TRUE),
      sec_info_finance = sum(get(weight_var) * (VD4010 == 7), na.rm = TRUE),
      sec_public_admin = sum(get(weight_var) * (VD4010 == 8), na.rm = TRUE),
      sec_other_services = sum(get(weight_var) * (VD4010 %in% c(9, 10)), na.rm = TRUE),
      sec_domestic = sum(get(weight_var) * (VD4010 == 11), na.rm = TRUE)
    ), by = by]
    results$sector <- sector
  }

  # Compute income aggregates
  if ("income" %in% indicator_groups) {
    income_vars <- intersect(c("VD4016", "VD4017", "VD4019", "VD4020"), names(dt))
    if (length(income_vars) > 0) {
      income <- dt[, .(
        income_mass_habitual = if ("VD4019" %in% names(.SD))
          sum(get(weight_var) * VD4019, na.rm = TRUE) else NA_real_,
        income_mass_effective = if ("VD4020" %in% names(.SD))
          sum(get(weight_var) * VD4020, na.rm = TRUE) else NA_real_,
        income_earners = sum(get(weight_var) * (!is.na(VD4019) & VD4019 > 0), na.rm = TRUE)
      ), by = by]
      results$income <- income
    }
  }

  # Merge all results
  if (length(results) == 0) {
    warning("No indicators computed")
    return(NULL)
  }

  result <- Reduce(function(x, y) merge(x, y, by = by, all = TRUE), results)

  # Compute rates if requested
  if ("rates" %in% indicator_groups && "population" %in% indicator_groups) {
    result[, `:=`(
      rate_participation = 100 * pop_labor_force / pop_14plus,
      rate_employment = 100 * pop_employed / pop_14plus,
      rate_unemployment = 100 * pop_unemployed / pop_labor_force
    )]

    if ("pop_potential_lf" %in% names(result)) {
      result[, rate_extended_lf := 100 * (pop_labor_force + pop_potential_lf) / pop_14plus]
    }

    if ("employment" %in% indicator_groups && "emp_with_ss" %in% names(result)) {
      result[, rate_ss_contribution := 100 * emp_with_ss / pop_employed]
    }
  }

  result
}

#' Create Indicator Columns
#'
#' Creates binary indicator columns from PNADC categorical variables.
#'
#' @param dt data.table with PNADC variables
#' @return data.table with indicator columns added
#' @keywords internal
#' @noRd
create_indicator_columns <- function(dt) {

  # These are the core indicator columns used in the Stata code
  # We create them as 0/1 indicators for weighted summation

  # Population indicators
  if ("V2009" %in% names(dt)) {
    dt[, ind_pop14mais := as.integer(V2009 >= 14)]
  }

  # Employment indicators
  if ("VD4002" %in% names(dt)) {
    dt[, `:=`(
      ind_employed = as.integer(VD4002 == 1),
      ind_unemployed = as.integer(VD4002 == 2)
    )]
  }

  if ("VD4001" %in% names(dt)) {
    dt[, `:=`(
      ind_in_lf = as.integer(VD4001 == 1),
      ind_out_lf = as.integer(VD4001 == 2)
    )]
  }

  # Underemployment (VD4004 pre-2015, VD4004A post-2015)
  if ("VD4004A" %in% names(dt)) {
    dt[, ind_underemployed := as.integer(VD4004A == 1)]
  } else if ("VD4004" %in% names(dt)) {
    dt[, ind_underemployed := as.integer(VD4004 == 1)]
  }

  dt
}

#' Create PNADC Indicator Variables
#'
#' Creates the full set of binary indicator variables used in the mensalization
#' process. These correspond to the z_ variables in the Stata code.
#'
#' @param dt data.table with PNADC microdata
#' @return data.table with indicator columns (0/1 values)
#' @keywords internal
#' @noRd
create_all_indicator_vars <- function(dt) {

  # Population
  dt[, `:=`(
    populacao = 1L,
    pop14mais = as.integer(V2009 >= 14),
    popocup = as.integer(VD4002 == 1),
    popdesocup = as.integer(VD4002 == 2),
    popnaforca = as.integer(VD4001 == 1),
    popforadaforca = as.integer(VD4001 == 2 & V2009 >= 14)
  )]

  # Employment by type (VD4009)
  if ("VD4009" %in% names(dt)) {
    dt[, `:=`(
      empregprivcomcart = as.integer(VD4009 == 1),
      empregprivsemcart = as.integer(VD4009 == 2),
      domesticocomcart = as.integer(VD4009 == 3),
      domesticosemcart = as.integer(VD4009 == 4),
      empregpublcomcart = as.integer(VD4009 == 5),
      empregpublsemcart = as.integer(VD4009 == 6),
      estatutmilitar = as.integer(VD4009 == 7),
      empregador = as.integer(VD4009 == 8),
      contapropria = as.integer(VD4009 == 9),
      trabfamauxiliar = as.integer(VD4009 == 10)
    )]

    # Aggregates
    dt[, `:=`(
      empregado = as.integer(VD4009 %in% 1:7),
      empregpriv = as.integer(VD4009 %in% 1:2),
      domestico = as.integer(VD4009 %in% 3:4),
      empregpubl = as.integer(VD4009 %in% 5:7)
    )]
  }

  # CNPJ distinction for employers and self-employed
  if ("V4019" %in% names(dt)) {
    dt[, `:=`(
      empregadorcomcnpj = as.integer(VD4009 == 8 & V4019 == 1),
      empregadorsemcnpj = as.integer(VD4009 == 8 & V4019 == 2),
      contapropriacomcnpj = as.integer(VD4009 == 9 & V4019 == 1),
      contapropriasemcnpj = as.integer(VD4009 == 9 & V4019 == 2)
    )]
  }

  # Employment by sector (VD4010)
  if ("VD4010" %in% names(dt)) {
    dt[, `:=`(
      agropecuaria = as.integer(VD4010 == 1),
      industria = as.integer(VD4010 == 2),
      construcao = as.integer(VD4010 == 3),
      comercio = as.integer(VD4010 == 4),
      transporte = as.integer(VD4010 == 5),
      alojaliment = as.integer(VD4010 == 6),
      infcomfinimobadm = as.integer(VD4010 == 7),
      adminpublica = as.integer(VD4010 == 8),
      outroservico = as.integer(VD4010 %in% c(9, 10)),
      servicodomestico = as.integer(VD4010 == 11)
    )]
  }

  # Social security
  if ("VD4012" %in% names(dt)) {
    dt[, contribuinteprev := as.integer(VD4012 == 1)]
  }

  # Underemployment
  if ("VD4004A" %in% names(dt)) {
    dt[, subocuphoras := as.integer(VD4004A == 1)]
  } else if ("VD4004" %in% names(dt)) {
    dt[, subocuphoras := as.integer(VD4004 == 1)]
  }

  # Potential labor force and discouraged
  if ("VD4003" %in% names(dt)) {
    dt[, forcapotencial := as.integer(VD4003 == 1)]
  }
  if ("VD4005" %in% names(dt)) {
    dt[, desalentado := as.integer(VD4005 == 1)]
  }

  dt
}
