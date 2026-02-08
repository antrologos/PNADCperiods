#' PNADCperiods: Identify Reference Periods in Brazil's PNADC Survey Data
#'
#' The PNADCperiods package provides tools to identify the exact reference
#' period (month, fortnight, or week) of Brazil's quarterly official household
#' survey, PNADC (Pesquisa Nacional por Amostra de Domicilios Continua - IBGE),
#' allowing for analyzing the survey data at sub-quarterly temporal granularity.
#'
#' The package offers four main capabilities:
#' \enumerate{
#'   \item \strong{Reference period identification}: Determines which temporal
#'     period (month, fortnight, or week) within each quarter each survey
#'     observation refers to, using IBGE's "Parada Técnica" rules and
#'     respondent birthdates. The identification is \strong{nested by construction}:
#'     fortnights require months, and weeks require fortnights.
#'   \item \strong{Period-specific weight calibration}: Adjusts survey weights
#'     for sub-quarterly estimates using adaptive hierarchical rake weighting
#'     (4/2/1 cell levels for month/fortnight/week respectively)
#'   \item \strong{Experimental strategies}: Probabilistic assignment and UPA
#'     aggregation to boost fortnight/week determination rates for sensitivity
#'     analysis
#'   \item \strong{SIDRA series mensalization}: Converts IBGE's rolling quarterly
#'     (trimestre móvel) aggregate series to exact monthly estimates
#' }
#'
#' \strong{Determination rates (strict, full series 2012-2025):}
#' \itemize{
#'   \item Monthly: ~97\%, ~98\% with annual anchor
#'   \item Fortnight: ~9\%
#'   \item Week: ~3\%
#' }
#'
#' With smaller datasets, rates may differ (e.g., 8 quarters: ~94\% monthly).
#' Experimental strategies (probabilistic + UPA aggregation) further improve
#' these determination rates.
#'
#' The package is highly optimized for large datasets (~450,000 rows/sec).
#' Uses pre-computed lookup tables for 20x faster date creation.
#'
#' \strong{Note:} Strict fortnight and week determination rates are inherently low
#' because they cannot leverage cross-quarter aggregation like months can. Only
#' birthday constraints within a single quarter are available to narrow the
#' interview window.
#'
#' The main functions are:
#' \itemize{
#'   \item \code{\link{pnadc_identify_periods}}: Builds a crosswalk containing
#'     month, fortnight, and week reference periods with IBGE calendar-based dates
#'   \item \code{\link{pnadc_apply_periods}}: Applies the crosswalk to any
#'     PNADC dataset and optionally calibrates weights
#'   \item \code{\link{pnadc_experimental_periods}}: Applies experimental strategies
#'     (probabilistic, UPA aggregation) for improved fortnight/week determination
#'   \item \code{\link{get_sidra_series_metadata}}: Lists 86+ available PNADC
#'     rolling quarter series from SIDRA
#'   \item \code{\link{fetch_sidra_rolling_quarters}}: Downloads rolling quarterly
#'     data from IBGE SIDRA API
#'   \item \code{\link{mensalize_sidra_series}}: Converts rolling quarters to exact
#'     monthly estimates
#' }
#'
#' @references
#' IBGE Manual Básico da Entrevista PNADC (methodology on "Parada Técnica")
#'
#' @author Rogerio Barbosa \email{rogerio.barbosa@@iesp.uerj.br} (R package, dashboard, and website; Ceres-IESP/UERJ)
#' @author Marcos Hecksher \email{mdhecksher@@gmail.com} (mensalization methodology; Ipea)
#'
#' @import data.table
#' @importFrom lubridate ymd
#' @importFrom checkmate assert_data_frame assert_int assert_logical assert_string assert_character assert_choice
#' @importFrom stats lm predict
#' @importFrom utils head setTxtProgressBar txtProgressBar
#' @keywords internal
"_PACKAGE"

# OPTIMIZATION: Enable data.table's internal parallelization
# Uses all available cores for groupby, join, and := operations
.onLoad <- function(libname, pkgname) {
  data.table::setDTthreads(0L)  # 0 = use all available threads
}

# Prevent R CMD check notes about data.table syntax
utils::globalVariables(c(
  # PNADC variables
  "Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
  "V2008", "V20081", "V20082", "V2009",
  "V1028", "V1032", "UF", "posest", "posest_sxi",
  # Computed variables - reference month identification
  "ref_month", "ref_month_start", "ref_month_end", "ref_month_in_quarter", "ref_month_yyyymm", "ref_month_weeks",
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
  "requires_exc_fortnight", "trim_exc_fortnight",
  "requires_exc_week", "trim_exc_week",
  # Computed variables - reference fortnight identification
  "ref_fortnight", "ref_fortnight_start", "ref_fortnight_end", "ref_fortnight_in_quarter", "ref_fortnight_yyyyff",
  "fortnight_min_pos", "fortnight_max_pos", "fortnight_min_yyyyff", "fortnight_max_yyyyff",
  "alt_fortnight_min_pos", "alt_fortnight_max_pos",
  "hh_fortnight_min", "hh_fortnight_max",
  "alt_hh_fortnight_min", "alt_hh_fortnight_max",
  "determined_fortnight",
  # Computed variables - reference week identification
  "ref_week", "ref_week_start", "ref_week_end", "ref_week_in_quarter", "ref_week_yyyyww",
  "week_min_yyyyww", "week_max_yyyyww", "week_min_pos", "week_max_pos",
  "alt_week_min_yyyyww", "alt_week_max_yyyyww",
  "hh_week_min", "hh_week_max", "alt_hh_week_min", "alt_hh_week_max",
  # Sequential week values for year boundary comparisons
  "week_min_seq", "week_max_seq", "hh_week_min_seq", "hh_week_max_seq",
  "alt_week_min_seq", "alt_week_max_seq", "alt_hh_week_min_seq", "alt_hh_week_max_seq",
  "trim_has_exception",
  "determined_month", "determined_week",
  # Computed variables - weight calibration (monthly, fortnightly, and weekly)
  "celula1", "celula2", "celula3", "celula4",
  "weight_current", "weight_calibrated", "weight_monthly", "weight_fortnight", "weight_weekly",
  "pop_quarter", "pop_month", "pop_fortnight", "pop_week",
  "n_cells_quarter", "n_cells_month", "n_cells_fortnight", "n_cells_week",
  "m_populacao", "f_populacao", "z_populacao", "w_populacao",
  # Computed variables - SIDRA population fetch
  "Valor", "anomesexato", "anomesfinaltrimmovel", "populacao",
  # Computed variables - smooth aggregates
  "month_pos", "weight_smoothed", "pop_current",
  "row_num", "row_num2", "d_pop", "quarter_yyyyq",
  # Computed variables - monthly to weekly/fortnight targets
  "yyyyww", "yyyymm", "yyyyff", "n_days", "days_in_month", "ref_month_yyyymm",
  # data.table join prefix variables (i.* references right table columns)
  "i.month1", "i.month2", "i.month3",
  "i.first_sat_m1", "i.first_sat_m2", "i.first_sat_m3",
  "i.alt_sat_m1", "i.alt_sat_m2", "i.alt_sat_m3",
  "i.ref_month", "i.ref_month_start", "i.ref_month_end", "i.ref_month_in_quarter", "i.ref_month_yyyymm", "i.ref_month_weeks",
  "i.ref_fortnight", "i.ref_fortnight_start", "i.ref_fortnight_end", "i.ref_fortnight_in_quarter", "i.ref_fortnight_yyyyff",
  "i.ref_week", "i.ref_week_start", "i.ref_week_end", "i.ref_week_in_quarter", "i.ref_week_yyyyww",
  "i.determined_month", "i.determined_fortnight", "i.determined_week",
  "i.m_populacao", "i.f_populacao", "i.w_populacao",
  # data.table special symbols and output column reference
  ".SD", ".N", ".I", "..output_cols", ".",
  # Calibration variables
  "v1032", "v2009", "uf", "..xw_cols",
  "n_months_in_year", "pop_year", "n_years_in_month", "year",
  # Unified calibration variables
  "anchor_year", "target_pop", "i.target_pop",
  "n_cells_anchor", "n_cells_period", "pop_anchor", "pop_period",
  # Optimized calibration internal variables
  ".anchor_key", ".is_determined", "i.pop_orig",
  "ref_week_iso_yyyyww", "ref_week_yyyyww",
  # Smoothing variables
  "period_pos", "i.pos", "cell_pop", "pop_smoothed", "pop_lag", "pop_lead",
  "smooth_factor", "i.smooth_factor", "pop_orig", "pop_new", "final_factor",
  "i.final_factor",
  # Month-conditional refinement variables
  "month_num_temp", "last_day_temp", "month_start_yyyyww", "month_end_yyyyww",
  # Nested fortnight/week identification variables (Phase 2 & 3)
  "fortnight_lower", "fortnight_upper",
  "fortnight_month", "fortnight_half", "fortnight_month_num",
  "fortnight_start_day", "fortnight_end_day",
  "fortnight_start_yyyyww", "fortnight_end_yyyyww",
  # Experimental period identification variables (restructured nested strategies)
  "ref_month_exp", "ref_month_exp_confidence",
  "ref_fortnight_exp", "ref_fortnight_exp_confidence",
  "ref_week_exp", "ref_week_exp_confidence",
  "hh_date_min", "hh_date_max", "fortnight_range", "date_midpoint",
  "midpoint_day", "boundary_distance",
  "ref_fortnight_likely", "ref_fortnight_confidence",
  "ref_week_likely", "ref_week_confidence",
  "ref_fortnight_upa", "ref_week_upa",
  "consensus_fortnight", "consensus_week", "n_unique", "n_households",
  "i.hh_date_min", "i.hh_date_max", "i.hh_fortnight_min", "i.hh_fortnight_max",
  "i.fortnight_range", "i.consensus_fortnight", "i.consensus_week",
  "needs_reagg",
  # Probabilistic strategy variables
  "hh_week_min_yyyyww", "hh_week_max_yyyyww",
  "hh_week_min_seq", "hh_week_max_seq", "week_range",
  "midpoint_week_yyyyww", "week_boundary_distance",
  "upa_date_min", "upa_date_max",
  "upa_month_min_pos", "upa_month_max_pos", "month_range",
  "month1_start", "month2_start", "month3_start",
  "boundary_date", "days_before_boundary", "days_after_boundary", "total_days",
  "month_identified", "effective_month",
  "hh_date_midpoint", "boundary_day_15",
  "days_in_first_fortnight", "days_in_second_fortnight", "total_interval_days",
  "fortnight_identified", "effective_fortnight",
  "fortnight_start_date", "fortnight_end_date",
  "week_date_midpoint", "week_boundary_date",
  "days_in_first_week", "days_in_second_week", "total_week_interval",
  # UPA aggregation strategy variables
  "n_total", "n_strict", "n_narrow", "consensus_month", "prop_strict",
  "has_narrow_month_range", "has_narrow_fortnight_range", "has_narrow_week_range",
  "prop_narrow", "likely_month", "likely_fortnight", "likely_week",
  "days_before", "days_after", "confidence",
  "upa_fortnight_min", "upa_fortnight_max", "upa_fortnight_range",
  "upa_week_min", "upa_week_max", "upa_week_range",
  "upa_week_min_seq", "upa_week_max_seq",
  "week_min", "week_max", "week_min_seq", "week_max_seq",
  "midpoint_week_seq",
  # data.table join prefix variables for experimental strategies
  "i.consensus_month", "i.likely_month", "i.likely_fortnight", "i.likely_week",
  "i.confidence", "i.month_identified", "i.effective_month",
  "i.fortnight_identified", "i.effective_fortnight",
  "i.upa_date_min", "i.upa_date_max", "i.upa_month_min_pos", "i.upa_month_max_pos",
  "i.month_range", "upa_month_min", "upa_month_max", "upa_month_range",
  # Combine crosswalks and derived columns
  "probabilistic_assignment",
  "i.ref_month_exp", "i.ref_month_exp_confidence",
  "i.ref_fortnight_exp", "i.ref_fortnight_exp_confidence",
  "i.ref_week_exp", "i.ref_week_exp_confidence",
  # IBGE calendar variables (quarter/month/fortnight/week bounds)
  "ibge_month_start_int", "ibge_first_sat_day", "i.ibge_month_start_int", "i.ibge_n_weeks",
  "quarter_end", "alt_quarter_end",
  "temp_month", "temp_month_in_q", "temp_fortnight_in_month",
  "ref_fortnight_weeks",
  "alt_week_min_pos", "alt_week_max_pos",
  "hh_week_min_pos", "hh_week_max_pos",
  "fortnight_boundary",
  # UPA aggregation week nesting validation variables
  "effective_fortnight_for_week", "week_lower_bound", "week_upper_bound",
  # Exception detection variables (experimental strategy)
  "alt_sat_m1", "alt_sat_m2", "alt_sat_m3",
  "alt_date_min", "alt_date_max", "alt_month_min_pos", "alt_month_max_pos",
  "upa_month_min_final", "upa_month_max_final",
  "month_min_pos_final", "month_max_pos_final",
  "midpoint_in_tech_stop", "tech_stop_type_month", "adjusted_midpoint",
  "month1_min_days", "month2_min_days", "month3_min_days",
  # Variables from pnadc_identify_periods and identify_reference_week
  "date_min_sat", "date_max_sat", "alt_date_min_sat", "alt_date_max_sat",
  "fortnight_min_in_month", "fortnight_max_in_month", "fortnight1_end",
  "alt_fortnight_min_in_month", "alt_fortnight_max_in_month",
  "week_month_in_quarter", "week_month_num", "week_fortnight_in_month",
  "fortnight_first_week_in_month", "fortnight_last_week_in_month",
  "fortnight_first_week_in_quarter", "fortnight_last_week_in_quarter",
  "alt_hh_week_min_pos", "alt_hh_week_max_pos",
  "in_tech_stop_range", "in_technical_stop_range", "tech_stop_type",
  "ref_week_temp", "hh_consensus_week", "fallback_week",
  "week_month_in_qtr", "week_in_month", "ibge_month_start_date",
  "i.trim_exc_m1", "i.trim_exc_m2", "i.trim_exc_m3",
  # Variables from pnadc_identify_periods week refinement
  "temp_month_num",
  # ============================================================================
  # SIDRA Series Mensalization Variables
  # ============================================================================
  # Metadata columns (hierarchical structure)
  "series_name", "api_path", "table_id", "variable_id",
  "classification_id", "classification_value",
  "theme", "theme_category", "subcategory",
  "description_pt", "description_en", "description",
  "unit", "unit_label_pt", "unit_label_en",
  "is_derived", "requires_deflation",
  # Mensalization algorithm variables
  "mesnotrim", "anomesfinaltrimmovel", "anomesexato",
  "d3", "cum", "cum1", "cum2", "cum3",
  "d3m1", "d3m2", "d3m3",
  "e0", "y0", "y",
  # SIDRA API result columns
  "Valor", "Trimestre Movel", "Trimestre.Movel",
  # Deflation variables
  "ipca_index", "deflator",
  # Starting points data
  "mesnotrim_1", "mesnotrim_2", "mesnotrim_3",
  # ============================================================================
  # Mensalization Derived Series Variables
  # ============================================================================
  "m_contribuinteprev", "m_desalentado", "m_domestico", "m_domesticocomcart",
  "m_domesticosemcart", "m_empregado", "m_empregpriv", "m_empregprivcomcart",
  "m_empregprivsemcart", "m_empregpubl", "m_empregpublcomcart",
  "m_empregpublsemcart", "m_estatutmilitar", "m_forcaampliada",
  "m_forcapotencial", "m_niveldesocup_calc", "m_nivelocup_calc",
  "m_perccontribprev_calc", "m_percdesalento_calc", "m_pop14mais",
  "m_popdesocup", "m_popnaforca", "m_popocup", "m_subocuphoras",
  "m_taxacombdesopot_calc", "m_taxacombdesosub_calc", "m_taxacompsubutlz_calc",
  "m_taxadesocup_calc", "m_taxapartic_calc", "m_taxasubocuphoras_calc",
  # ============================================================================
  # Probability/Aggregation Strategy Variables (pnadc-experimental-periods.R)
  # ============================================================================
  "..cols_to_keep",
  ".ppc_f_current", ".ppc_f_factor", ".ppc_f_target", ".ppc_fortnight",
  ".ppc_m_current", ".ppc_m_factor", ".ppc_m_target", ".ppc_month",
  ".ppc_q_current", ".ppc_q_factor", ".ppc_q_target", ".ppc_quarter",
  "days_within_fortnight_1", "days_within_fortnight_2",
  "days_within_month_1_min1_max2", "days_within_month_2_min1_max2",
  "days_within_month_2_min2_max3", "days_within_month_3_min2_max3",
  "days_within_week_1", "days_within_week_2", "days_within_week_3",
  "days_within_week_4",
  "determined_aggreg_fortnight", "determined_aggreg_month",
  "determined_aggreg_week", "determined_probable_fortnight",
  "determined_probable_month", "determined_probable_week",
  "fortnight_1_end", "fortnight_1_start", "fortnight_2_end", "fortnight_2_start",
  "fortnight_max_hh", "fortnight_min_hh", "fortnight_pos", "fortnight_prob_filter",
  "i.alt_date_max", "i.alt_date_min", "i.date_max", "i.date_min",
  "i.f_total_orig", "i.fortnight_1_end", "i.fortnight_1_start",
  "i.fortnight_2_end", "i.fortnight_2_start", "i.is_leap", "i.m_total_orig",
  "i.q_total_orig",
  "i.week_1_start", "i.week_1_end", "i.week_2_start", "i.week_2_end",
  "i.week_3_start", "i.week_3_end", "i.week_4_start", "i.week_4_end",
  "i.week_5_start", "i.week_5_end", "i.week_6_start", "i.week_6_end",
  "i.week_7_start", "i.week_7_end", "i.week_8_start", "i.week_8_end",
  "i.week_9_start", "i.week_9_end", "i.week_10_start", "i.week_10_end",
  "i.week_11_start", "i.week_11_end", "i.week_12_start", "i.week_12_end",
  "ibge_month_start", "is_leap",
  "month_max_upa", "month_min_upa", "month_prob_filter",
  "n_fortnight", "n_month", "n_week",
  "prob_month_min1_max2", "prob_month_min2_max3",
  "prob_ref_fortnight_in_month", "prob_ref_fortnight_in_month_max",
  "prob_ref_fortnight_in_month_min",
  "prob_ref_month_in_quarter", "prob_ref_month_in_quarter_max",
  "prob_ref_month_in_quarter_min",
  "prob_ref_week_in_month", "prob_ref_week_in_month_max",
  "prob_ref_week_in_month_min",
  "prob_week_1_2", "prob_week_3_4",
  "prop_fortnight", "prop_fortnight_1", "prop_fortnight_2",
  "prop_month", "prop_month_1_min1_max2", "prop_month_2_min1_max2",
  "prop_month_2_min2_max3", "prop_month_3_min2_max3",
  "prop_week", "prop_week_1", "prop_week_2", "prop_week_3", "prop_week_4",
  "ref_fortnight_in_month", "ref_fortnight_in_month_aggreg",
  "ref_fortnight_in_month_aggreg_confidence",
  "ref_month_in_quarter_aggreg", "ref_month_in_quarter_aggreg_confidence",
  "ref_month_in_year",
  "ref_week_in_month", "ref_week_in_month_aggreg",
  "ref_week_in_month_aggreg_confidence",
  "week_1_start", "week_1_end", "week_2_start", "week_2_end",
  "week_3_start", "week_3_end", "week_4_start", "week_4_end",
  "week_5_start", "week_8_end", "week_9_start", "week_12_end",
  "week_max_hh", "week_min_hh", "week_pos", "week_prob_filter"
))
