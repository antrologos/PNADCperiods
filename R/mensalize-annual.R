#' Mensalize Annual PNADC Data
#'
#' Merges annual PNADC data with a mensalization crosswalk and optionally
#' calibrates the annual weights (V1032) to monthly population totals.
#'
#' @description
#' This function:
#' \enumerate{
#'   \item Merges annual PNADC data with a crosswalk to add reference month information
#'   \item Optionally calibrates V1032 weights using hierarchical rake weighting
#'   \item Final calibration to SIDRA monthly population totals
#' }
#'
#' @param annual_data data.frame/data.table with annual PNADC data (stacked or single year).
#'   Required columns:
#'   \itemize{
#'     \item Join keys: \code{ano}, \code{trimestre}, \code{upa}, \code{v1008}, \code{v1014}, \code{v2003}
#'     \item Weight: \code{v1032}
#'     \item Demographics (for calibration): \code{v2009} (age), \code{uf}, \code{posest}, \code{posest_sxi}
#'   }
#' @param crosswalk Output from \code{mensalizePNADC()} with reference month information.
#' @param calibrate_weights Logical. If TRUE (default), calibrate V1032 weights to
#'   monthly SIDRA population totals using hierarchical rake weighting.
#'   If FALSE, just merge crosswalk without calibration.
#' @param n_cells Integer (1-4). Number of hierarchical cell levels for raking.
#'   Default is 4 (full hierarchy: age, posest_sxi, UF, posest).
#' @param keep_all Logical. Keep observations without determined reference month? Default TRUE.
#' @param verbose Logical. Print progress? Default TRUE.
#'
#' @return data.table with annual data merged with crosswalk plus:
#'   \itemize{
#'     \item \code{ref_month}, \code{ref_month_in_quarter}, \code{ref_month_yyyymm}: From crosswalk
#'     \item \code{weight_monthly}: Calibrated monthly weight (if calibrate_weights = TRUE)
#'   }
#'
#' @details
#' ## Why Not Use Quarterly Crosswalk Weights?
#'
#' The \code{weight_monthly} from the quarterly crosswalk is calibrated assuming all 5
#' panel visits are present in each quarter. Annual data has only 1 visit per observation,
#' so those weights are inappropriate.
#'
#' This function uses the annual weight \code{V1032} as the starting point and applies:
#' \enumerate{
#'   \item Hierarchical rake weighting within demographic cells (redistributing yearly
#'     totals to months)
#'   \item Final calibration to SIDRA monthly population totals
#' }
#'
#' @seealso \code{\link{mensalizePNADC}} for creating the crosswalk
#'
#' @examples
#' \dontrun{
#' # Step 1: Create crosswalk from quarterly data
#' crosswalk <- mensalizePNADC(quarterly_data, compute_weights = FALSE)
#'
#' # Step 2: Load and merge annual data with calibration
#' d <- mensalize_annual_pnadc(
#'   annual_data = annual_data,
#'   crosswalk = crosswalk,
#'   calibrate_weights = TRUE
#' )
#'
#' # Use the calibrated weight
#' d[, .(pop = sum(weight_monthly)), by = ref_month_yyyymm]
#' }
#'
#' @export
mensalize_annual_pnadc <- function(annual_data,
                                    crosswalk,
                                    calibrate_weights = TRUE,
                                    n_cells = 4L,
                                    keep_all = TRUE,
                                    verbose = TRUE) {

  checkmate::assert_logical(calibrate_weights, len = 1)
  checkmate::assert_int(n_cells, lower = 1L, upper = 4L)
  checkmate::assert_logical(keep_all, len = 1)
  checkmate::assert_logical(verbose, len = 1)

  # Convert to data.table
  dt <- ensure_data_table(annual_data, copy = TRUE)
  xw <- ensure_data_table(crosswalk, copy = TRUE)

  # Standardize column names to lowercase
  data.table::setnames(dt, names(dt), tolower(names(dt)))
  data.table::setnames(xw, names(xw), tolower(names(xw)))

  # Define join keys
  join_keys <- c("ano", "trimestre", "upa", "v1008", "v1014", "v2003")

  # Ensure matching column types
  for (k in join_keys) {
    if (k %in% names(dt) && k %in% names(xw)) {
      dt[[k]] <- as.numeric(dt[[k]])
      xw[[k]] <- as.numeric(xw[[k]])
    }
  }

  # Set keys for efficient merge
  data.table::setkeyv(dt, join_keys)
  data.table::setkeyv(xw, join_keys)

  # Select only reference month columns from crosswalk (NOT weight_monthly)
  xw_cols <- c(join_keys, "ref_month", "ref_month_in_quarter", "ref_month_yyyymm")
  xw_cols <- intersect(xw_cols, names(xw))

  # Merge
  if (verbose) message("Merging crosswalk with annual data...")
  dt <- merge(dt, xw[, ..xw_cols], by = join_keys, all.x = TRUE)

  match_rate <- mean(!is.na(dt$ref_month_in_quarter))
  if (verbose) {
    message(sprintf("Match rate: %.1f%%", 100 * match_rate))
  }

  if (!calibrate_weights) {
    return(dt)
  }

  # Calibrate annual weights to monthly population totals
  if (verbose) message("Calibrating weights...")
  dt <- calibrate_annual_weights(dt, n_cells = n_cells, keep_all = keep_all, verbose = verbose)

  dt
}
