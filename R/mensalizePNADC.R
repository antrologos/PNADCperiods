#' Mensalize PNADC Quarterly Survey Data
#'
#' Main function to convert quarterly PNADC survey data to monthly time series.
#' Returns a crosswalk data.frame for joining with original data, adding
#' reference month information and optionally monthly survey weights.
#'
#' @description
#' This function processes stacked quarterly PNADC microdata to:
#' \enumerate{
#'   \item Identify which month within each quarter each observation refers to
#'   \item Optionally compute monthly survey weights (if \code{compute_weights = TRUE})
#' }
#'
#' The output is a crosswalk table that can be joined with original (unstacked)
#' PNADC data files to add monthly time information.
#'
#' @param data A data.frame or data.table with stacked quarterly PNADC microdata.
#'
#'   For reference month identification only (\code{compute_weights = FALSE}),
#'   minimum required columns are:
#'   \itemize{
#'     \item \code{Ano}, \code{Trimestre}: Year and quarter
#'     \item \code{UPA}, \code{V1014}: Primary sampling unit and panel
#'     \item \code{V2008}, \code{V20081}, \code{V20082}: Birth day, month, year
#'     \item \code{V2009}: Age
#'   }
#'
#'   For monthly weight computation (\code{compute_weights = TRUE}), additional
#'   columns are required. See Details.
#'
#' @param compute_weights Logical. If TRUE, compute monthly survey weights in
#'   addition to identifying reference months. Default is FALSE.
#'
#' @param monthly_totals A data.frame with monthly population totals for calibration.
#'   Required if \code{compute_weights = TRUE}. Must contain:
#'   \itemize{
#'     \item \code{ref_month_yyyymm} or \code{anomesexato}: Month in YYYYMM format
#'     \item \code{m_populacao}: Monthly population in thousands
#'   }
#'
#' @param output Character. What to return:
#'   \itemize{
#'     \item \code{"crosswalk"} (default): Minimal crosswalk for joining
#'     \item \code{"microdata"}: Full microdata with all computed columns
#'     \item \code{"aggregates"}: Monthly aggregated indicators
#'   }
#'
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return Depends on \code{output} parameter:
#'
#'   If \code{output = "crosswalk"} (default):
#'   A data.table with join keys and new time variables:
#'   \itemize
#'     \item Join keys: \code{Ano}, \code{Trimestre}, \code{UPA}, \code{V1008}, \code{V1014}, \code{V2003}
#'     \item \code{ref_month}: Reference month as Date
#'     \item \code{ref_month_in_quarter}: Position in quarter (1, 2, 3) or NA
#'     \item \code{ref_month_yyyymm}: Integer YYYYMM format
#'     \item \code{weight_monthly}: Monthly weight (if \code{compute_weights = TRUE})
#'   }
#'
#'   If \code{output = "microdata"}:
#'   Full input data with all computed columns added.
#'
#'   If \code{output = "aggregates"}:
#'   Monthly aggregated labor indicators.
#'
#' @details
#' ## Reference Month Identification
#'
#' The algorithm determines which month each survey response refers to based on:
#' \itemize{
#'   \item IBGE's "Parada Tecnica" rules for reference week timing
#'   \item Respondent birthdates (constrains possible interview dates)
#'   \item UPA-panel grouping (everyone interviewed together)
#' }
#'
#' Typically 85-90% of observations can be assigned a definite reference month.
#'
#' ## Monthly Weight Computation
#'
#' When \code{compute_weights = TRUE}, the function:
#' \enumerate{
#'   \item Redistributes quarterly weights to months using hierarchical calibration
#'   \item Smooths monthly aggregates to remove quarterly artifacts
#'   \item Applies Bayesian adjustment to match individual weights to targets
#' }
#'
#' Additional required columns for weight computation:
#' \itemize{
#'   \item Survey design: \code{V1028}, \code{UF}, \code{Estrato}, \code{posest}, \code{posest_sxi}
#'   \item Demographics: \code{V2007}, \code{V2010}
#'   \item Labor force: \code{VD4001}, \code{VD4002}, \code{VD4003}, \code{VD4005}
#'   \item Employment: \code{VD4009}, \code{VD4010}, \code{VD4012}
#'   \item Income: \code{VD4016}, \code{VD4017}, \code{VD4019}, \code{VD4020}
#'   \item Optional: \code{V4019} (CNPJ, for detailed employer/self-employed categories)
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage: identify reference months only
#' library(mensalizePNADC)
#'
#' # Load stacked quarterly data (minimum columns)
#' pnadc <- data.table::fread("pnadc_stacked.csv",
#'   select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#'              "V2008", "V20081", "V20082", "V2009"))
#'
#' # Get crosswalk
#' crosswalk <- mensalizePNADC(pnadc)
#'
#' # Join with original quarterly file
#' original <- haven::read_dta("PNADC_2023T1.dta")
#' monthly <- dplyr::left_join(original, crosswalk,
#'   by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"))
#'
#'
#' # Full pipeline with monthly weights
#' pnadc_full <- haven::read_dta("PNADCtrimestralempilhada.dta")
#' monthly_pop <- haven::read_dta("pnadc_series_mensalizadas.dta")
#'
#' result <- mensalizePNADC(pnadc_full,
#'   compute_weights = TRUE,
#'   monthly_totals = monthly_pop)
#' }
#'
#' @seealso
#' \code{\link{identify_reference_month}} for just reference month identification
#' \code{\link{calibrate_monthly_weights}} for weight calibration details
#' \code{\link{compute_labor_indicators}} for computing indicators from results
#'
#' @export
mensalizePNADC <- function(data,
                            compute_weights = FALSE,
                            monthly_totals = NULL,
                            output = c("crosswalk", "microdata", "aggregates"),
                            verbose = TRUE) {

  # Validate arguments
  output <- match.arg(output)
  checkmate::assert_logical(compute_weights, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  if (compute_weights && is.null(monthly_totals)) {
    stop("monthly_totals is required when compute_weights = TRUE")
  }

  # Step 1: Identify reference months
  if (verbose) message("Step 1/", ifelse(compute_weights, "4", "1"),
                        ": Identifying reference months...")

  crosswalk <- identify_reference_month(data)

  det_rate <- attr(crosswalk, "determination_rate")
  if (verbose) {
    message("  Determination rate: ", sprintf("%.1f%%", det_rate * 100))
  }

  if (!compute_weights) {
    if (output == "crosswalk") {
      return(crosswalk)
    } else if (output == "microdata") {
      # Merge crosswalk with original data
      dt <- ensure_data_table(data, copy = TRUE)
      key_cols <- intersect(join_key_vars(), names(dt))
      result <- merge(dt, crosswalk, by = key_cols, all.x = TRUE)
      return(result)
    } else {
      stop("output = 'aggregates' requires compute_weights = TRUE")
    }
  }

  # Steps 2-4: Full weight computation pipeline

  # Merge reference months with full data
  if (verbose) message("Step 2/4: Calibrating monthly weights...")

  dt <- ensure_data_table(data, copy = TRUE)
  key_cols <- intersect(join_key_vars(), names(dt))
  dt <- merge(dt, crosswalk, by = key_cols, all.x = TRUE)

  # Calibrate weights
  dt <- calibrate_monthly_weights(dt, monthly_totals)

  if (verbose) message("Step 3/4: Smoothing monthly aggregates...")

  # Create indicator variables and aggregate
  dt <- create_all_indicator_vars(dt)

  indicator_vars <- c(
    "populacao", "pop14mais", "popocup", "popdesocup", "popnaforca", "popforadaforca",
    "empregprivcomcart", "empregprivsemcart", "domesticocomcart", "domesticosemcart",
    "empregpublcomcart", "empregpublsemcart", "estatutmilitar",
    "empregador", "contapropria", "trabfamauxiliar",
    "agropecuaria", "industria", "construcao", "comercio", "transporte",
    "alojaliment", "infcomfinimobadm", "adminpublica", "outroservico", "servicodomestico",
    "contribuinteprev", "subocuphoras", "forcapotencial", "desalentado"
  )

  # Add CNPJ variants if available
  if ("V4019" %in% names(dt)) {
    indicator_vars <- c(indicator_vars,
                        "empregadorcomcnpj", "empregadorsemcnpj",
                        "contapropriacomcnpj", "contapropriasemcnpj")
  }

  # Aggregate monthly
  aggregates <- dt[!is.na(ref_month_in_quarter), {
    result <- list(n_obs = .N)
    for (v in intersect(indicator_vars, names(.SD))) {
      result[[paste0("z_", v)]] <- sum(get(v) * weight_calibrated, na.rm = TRUE)
    }
    result
  }, by = ref_month_yyyymm]

  # Smooth aggregates
  smoothed <- smooth_monthly_aggregates(aggregates)

  if (verbose) message("Step 4/4: Applying Bayesian weight adjustment...")

  # Bayesian adjustment
  dt <- adjust_weights_bayesian(dt, smoothed)

  if (verbose) {
    n_with_weights <- sum(!is.na(dt$weight_monthly))
    message("  Observations with monthly weights: ",
            format(n_with_weights, big.mark = ","))
  }

  # Return based on output type
  if (output == "crosswalk") {
    # Minimal crosswalk with weights
    output_cols <- c(key_cols, "ref_month", "ref_month_in_quarter",
                     "ref_month_yyyymm", "weight_monthly")
    result <- dt[, ..output_cols]
    class(result) <- c("pnadc_crosswalk", class(result))
    attr(result, "determination_rate") <- det_rate
    return(result)
  } else if (output == "microdata") {
    return(dt)
  } else {
    # Return smoothed aggregates
    return(smoothed)
  }
}
