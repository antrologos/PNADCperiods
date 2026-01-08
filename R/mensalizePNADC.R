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
#'   addition to identifying reference months. Default is FALSE. Requires the
#'   \code{sidrar} package to fetch population data from IBGE SIDRA API.
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
#'   \itemize{
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
#'   Monthly aggregated indicators.
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
#' ## Cross-Quarter Aggregation (Important!)
#'
#' **For optimal determination rates (~95%), input data should be stacked across
#' multiple quarters** (ideally 2+ years). The algorithm leverages PNADC's rotating
#' panel design where the same UPA-V1014 is interviewed in the same relative week
#' across all quarterly visits.
#'
#' \itemize{
#'   \item **Per-quarter processing**: ~65-75% determination rate
#'   \item **Multi-quarter stacked**: ~95% determination rate
#' }
#'
#' The cross-quarter aggregation dramatically improves accuracy by combining
#' birthday constraints from multiple interview rounds.
#'
#' ## Monthly Weight Computation
#'
#' When \code{compute_weights = TRUE}, the function:
#' \enumerate{
#'   \item Fetches monthly population from IBGE SIDRA API (table 6022)
#'   \item Redistributes quarterly weights to months using hierarchical rake weighting
#'   \item Smooths monthly aggregates to remove quarterly artifacts
#' }
#'
#' The resulting \code{weight_monthly} is the **final** weight for general-purpose
#' monthly analysis. No Bayesian adjustment is applied.
#'
#' For theme-specific calibration to match IBGE SIDRA series (e.g., unemployment
#' rate, employment levels), use \code{\link{calibrate_to_sidra}} separately.
#'
#' Additional required columns for weight computation:
#' \itemize{
#'   \item Survey design: \code{V1028}, \code{V1008}, \code{V2003}, \code{UF}, \code{posest}, \code{posest_sxi}
#' }
#'
#' @section Dependencies:
#' When \code{compute_weights = TRUE}, the \code{sidrar} package is required
#' to fetch population data. Install with: \code{install.packages("sidrar")}
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
#' # With monthly weights for general analysis
#' # (requires sidrar package and internet connection)
#' pnadc_full <- haven::read_dta("PNADCtrimestralempilhada.dta")
#'
#' result <- mensalizePNADC(pnadc_full, compute_weights = TRUE)
#'
#' # Use weight_monthly for any monthly aggregate
#' result[, .(pop = sum(weight_monthly)), by = ref_month_yyyymm]
#' }
#'
#' @seealso
#' \code{\link{identify_reference_month}} for just reference month identification
#' \code{\link{calibrate_monthly_weights}} for weight calibration details
#' \code{\link{calibrate_to_sidra}} for theme-specific Bayesian calibration
#' \code{\link{compute_labor_indicators}} for computing indicators from results
#'
#' @export
mensalizePNADC <- function(data,
                            compute_weights = FALSE,
                            output = c("crosswalk", "microdata", "aggregates"),
                            verbose = TRUE) {

  # Validate arguments
  output <- match.arg(output)
  checkmate::assert_logical(compute_weights, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Determine number of steps for progress messages
  n_steps <- if (compute_weights) 4L else 1L

  # Step 1: Identify reference months
  if (verbose) message("Step 1/", n_steps, ": Identifying reference months...")

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

  # Steps 2-4: Weight computation pipeline (without Bayesian adjustment)

  # Step 2: Fetch monthly population from SIDRA
  if (verbose) message("Step 2/", n_steps, ": Fetching monthly population from SIDRA...")

  monthly_totals <- fetch_monthly_population(verbose = verbose)

  # Step 3: Calibrate weights using rake weighting
  if (verbose) message("Step 3/", n_steps, ": Calibrating monthly weights (rake weighting)...")

  dt <- ensure_data_table(data, copy = TRUE)

  # Ensure consistent types for join keys (PNADC data often has character columns)
  if (is.character(dt$Ano)) dt[, Ano := as.integer(Ano)]
  if (is.character(dt$Trimestre)) dt[, Trimestre := as.integer(Trimestre)]

  key_cols <- intersect(join_key_vars(), names(dt))
  dt <- merge(dt, crosswalk, by = key_cols, all.x = TRUE)

  # Calibrate weights using hierarchical rake weighting
  dt <- calibrate_monthly_weights(dt, monthly_totals)

  # Step 4: Smooth monthly aggregates
  if (verbose) message("Step 4/", n_steps, ": Smoothing monthly aggregates...")

  # Aggregate and smooth to get final monthly weights
  # The smoothing step adjusts weights so monthly series don't show artificial quarterly patterns
  dt <- smooth_calibrated_weights(dt)

  # Rename to final output name
  if ("weight_smoothed" %in% names(dt)) {
    data.table::setnames(dt, "weight_smoothed", "weight_monthly")
  } else if ("weight_calibrated" %in% names(dt)) {
    data.table::setnames(dt, "weight_calibrated", "weight_monthly")
  }

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
    output_cols <- intersect(output_cols, names(dt))
    result <- dt[, ..output_cols]
    class(result) <- c("pnadc_crosswalk", class(result))
    attr(result, "determination_rate") <- det_rate
    return(result)
  } else if (output == "microdata") {
    return(dt)
  } else {
    # Return monthly aggregates
    aggregates <- compute_monthly_aggregates(dt)
    return(aggregates)
  }
}

#' Smooth Calibrated Weights
#'
#' Apply smoothing to calibrated weights to remove artificial quarterly patterns.
#'
#' @param dt data.table with weight_calibrated column
#' @return data.table with weight_smoothed column added
#' @keywords internal
#' @noRd
smooth_calibrated_weights <- function(dt) {
  # Aggregate to monthly totals
  monthly_totals <- dt[!is.na(ref_month_in_quarter), .(
    pop_calibrated = sum(weight_calibrated, na.rm = TRUE)
  ), by = ref_month_yyyymm]

  # Apply smoothing to remove quarterly artifacts
  smoothed <- smooth_monthly_aggregates(monthly_totals)

  if ("m_populacao" %in% names(smoothed)) {
    # Merge smoothed totals back
    dt <- merge(dt, smoothed[, .(ref_month_yyyymm, m_populacao)],
                by = "ref_month_yyyymm", all.x = TRUE)

    # Compute monthly weight totals for calibration
    dt[, pop_month := sum(weight_calibrated, na.rm = TRUE), by = ref_month_yyyymm]

    # Scale individual weights to match smoothed monthly totals
    # m_populacao is in thousands
    dt[!is.na(m_populacao) & pop_month > 0,
       weight_smoothed := weight_calibrated * (m_populacao * 1000 / pop_month)]

    # For months without smoothed values, keep calibrated weight
    dt[is.na(weight_smoothed), weight_smoothed := weight_calibrated]

    # Clean up
    dt[, c("m_populacao", "pop_month") := NULL]
  } else {
    # If smoothing failed, just use calibrated weights
    dt[, weight_smoothed := weight_calibrated]
  }

  dt
}

#' Compute Monthly Aggregates
#'
#' Compute monthly aggregated indicators from weighted microdata.
#'
#' @param dt data.table with weight_monthly column
#' @return data.table with monthly aggregates
#' @keywords internal
#' @noRd
compute_monthly_aggregates <- function(dt) {
  dt[!is.na(ref_month_in_quarter), .(
    n_obs = .N,
    population = sum(weight_monthly, na.rm = TRUE)
  ), by = ref_month_yyyymm]
}
