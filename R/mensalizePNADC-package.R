#' mensalizePNADC: Convert Quarterly PNADC Survey Data to Monthly Time Series
#'
#' @description
#' The mensalizePNADC package provides tools to convert Brazil's quarterly
#' PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) survey data
#' into monthly time series.
#'
#' The package offers two main capabilities:
#' \enumerate{
#'   \item \strong{Reference month identification}: Determines which month within
#'     each quarter each survey observation refers to, using IBGE's "Parada Tecnica"
#'     rules and respondent birthdates
#'   \item \strong{Monthly weight computation}: Adjusts survey weights for monthly
#'     (instead of quarterly) estimates using hierarchical calibration and
#'     Bayesian methods
#' }
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{mensalizePNADC}}}{Main function that processes stacked
#'     quarterly PNADC data and returns a crosswalk for joining with original data}
#'   \item{\code{\link{identify_reference_month}}}{Identifies which month each
#'     observation refers to (the core algorithm)}
#'   \item{\code{\link{calibrate_monthly_weights}}}{Redistributes quarterly
#'     weights to monthly using hierarchical calibration}
#'   \item{\code{\link{smooth_monthly_aggregates}}}{Removes quarterly artifacts
#'     from monthly series}
#'   \item{\code{\link{adjust_weights_bayesian}}}{Final Bayesian weight adjustment}
#'   \item{\code{\link{compute_labor_indicators}}}{Computes standard labor force
#'     indicators from weighted data}
#' }
#'
#' @section Utility Functions:
#' \describe{
#'   \item{\code{\link{validate_pnadc}}}{Validates input data has required columns}
#' }
#'
#' @section Background:
#' PNADC is a quarterly household survey conducted by IBGE (Brazilian Institute
#' of Geography and Statistics) that provides labor market and demographic
#' information. While the survey is published quarterly, each interview actually
#' refers to a specific week within the quarter.
#'
#' This package implements the methodology developed by Marcos Hecksher to
#' identify reference months and compute monthly-appropriate survey weights,
#' enabling monthly time series analysis of PNADC data.
#'
#' @section Methodology:
#' The reference month identification algorithm uses:
#' \itemize{
#'   \item IBGE's reference week timing rules ("Parada Tecnica")
#'   \item Respondent birthdates to constrain possible interview dates
#'   \item UPA-panel level aggregation (everyone interviewed together)
#' }
#'
#' The monthly weight computation uses:
#' \itemize{
#'   \item Hierarchical calibration across demographic/geographic cells
#'   \item Moving average smoothing to remove quarterly artifacts
#'   \item Bayesian posterior adjustment to match employment targets
#' }
#'
#' @references
#' IBGE Manual Basico da Entrevista PNADC (methodology on "Parada Tecnica")
#'
#' @author Marcos Hecksher \email{mdhecksher@@gmail.com}
#'
#' @docType package
#' @name mensalizePNADC-package
#' @aliases mensalizePNADC-package
#'
#' @import data.table
#' @importFrom checkmate assert_data_frame assert_int assert_logical assert_string assert_character
#' @importFrom stats median
"_PACKAGE"

# Prevent R CMD check notes about data.table syntax
utils::globalVariables(c(
  # PNADC variables
  "Ano", "Trimestre", "UPA", "V1008", "V1014", "V1016", "V2003",
  "V2007", "V2008", "V20081", "V20082", "V2009", "V2010",
  "V1028", "UF", "Estrato", "posest", "posest_sxi",
  "VD4001", "VD4002", "VD4003", "VD4004", "VD4004A", "VD4005",
  "VD4009", "VD4010", "VD4012",
  "VD4016", "VD4017", "VD4019", "VD4020",
  "V4019",
  # Computed variables
  "ref_month", "ref_month_in_quarter", "ref_month_yyyymm",
  "birthday", "first_sat_after_birthday",
  "visit_before_birthday", "month1", "month2", "month3",
  "first_sat_m1", "first_sat_m2", "first_sat_m3",
  "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
  "date_min", "date_max", "month_min_pos", "month_max_pos",
  "alt_date_min", "alt_date_max", "alt_month_min_pos", "alt_month_max_pos",
  "upa_month_min", "upa_month_max",
  "alt_upa_month_min", "alt_upa_month_max",
  "upa_month_min_final", "upa_month_max_final",
  "requires_exception", "requires_exc_m1", "requires_exc_m2", "requires_exc_m3",
  "trim_exc_m1", "trim_exc_m2", "trim_exc_m3",
  "celula1", "celula2", "celula3", "celula4",
  "weight_current", "weight_calibrated", "weight_monthly",
  "pop_quarter", "pop_month", "n_cells_quarter", "n_cells_month",
  "pop_cat", "pop_status_intermed", "pop_cat_status",
  "employment_status", "cat", "target_pop",
  "prob_sit", "prob_cat", "prob_cat_given_sit_intermed",
  "prob_sit_given_cat", "prob_sit_given_cat_intermed",
  "weight_ratio", "m_populacao",
  ".row_id", ".SD", ".N", ".I",
  # Indicator variables
  "populacao", "pop14mais", "popocup", "popdesocup", "popnaforca", "popforadaforca",
  "empregprivcomcart", "empregprivsemcart", "domesticocomcart", "domesticosemcart",
  "empregpublcomcart", "empregpublsemcart", "estatutmilitar",
  "empregador", "contapropria", "trabfamauxiliar",
  "empregadorcomcnpj", "empregadorsemcnpj", "contapropriacomcnpj", "contapropriasemcnpj",
  "agropecuaria", "industria", "construcao", "comercio", "transporte",
  "alojaliment", "infcomfinimobadm", "adminpublica", "outroservico", "servicodomestico",
  "contribuinteprev", "subocuphoras", "forcapotencial", "desalentado",
  "empregado", "empregpriv", "domestico", "empregpubl",
  "ind_pop14mais", "ind_employed", "ind_unemployed", "ind_in_lf", "ind_out_lf", "ind_underemployed"
))
