#' Starting Points for SIDRA Series Mensalization
#'
#' Pre-computed starting point values (y0) for mensalizing IBGE's rolling
#' quarterly series into exact monthly estimates.
#'
#' @name pnadc_series_starting_points
#' @docType data
#' @usage data(pnadc_series_starting_points)
#'
#' @format A data.table with 159 rows and 3 columns:
#' \describe{
#'   \item{series_name}{Character. Name of the SIDRA series (53 series)}
#'   \item{mesnotrim}{Integer. Month position in quarter (1, 2, or 3)}
#'   \item{y0}{Numeric. Starting point value for this series and position}
#' }
#'
#' @details
#' These starting points were computed from PNADC microdata using the full
#' R package pipeline, ensuring consistency with
#' \code{\link{compute_starting_points_from_microdata}}:
#' \enumerate{
#'   \item Weight calibration via \code{\link{pnadc_apply_periods}}: month 2
#'     scaled to poptrim (quarterly V1028 sum from ALL observations), months 1
#'     and 3 scaled to SIDRA monthly population
#'   \item z_ aggregates computed via \code{\link{compute_z_aggregates}} using
#'     calibrated \code{weight_monthly}
#'   \item Starting points computed via \code{\link{compute_series_starting_points}}
#'     with CNPJ-aware calibration periods
#' }
#'
#' The calibration period (2013-2019) was chosen because:
#' \itemize{
#'   \item It includes stable pre-pandemic data
#'   \item IBGE methodology was consistent during this period
#'   \item Sufficient observations for reliable estimates
#' }
#'
#' CNPJ series (empregadorcomcnpj, empregadorsemcnpj, contapropriacomcnpj,
#' contapropriasemcnpj) use calibration period 2016-2019 with cumulative sum
#' starting from October 2015 due to V4019 variable availability.
#'
#' @section Methodology Consistency:
#' The bundled starting points are generated using the same pipeline as
#' \code{\link{compute_starting_points_from_microdata}}, ensuring that users
#' who compute custom starting points will get consistent results.
#'
#' @section When to Use Custom Starting Points:
#' The bundled starting points are suitable for most users. Consider computing
#' custom starting points with \code{\link{compute_starting_points_from_microdata}} if:
#' \itemize{
#'   \item IBGE makes major methodological changes to the PNADC
#'   \item You need series not included in the bundled data
#'   \item You want to use a different calibration period
#'   \item You are working with updated or different microdata
#' }
#'
#' @source Computed using \code{data-raw/regenerate_starting_points_from_microdata.R}
#'
#' @seealso
#' \code{\link{mensalize_sidra_series}} which uses this data by default
#' \code{\link{compute_series_starting_points}} for custom calibration
#'
#' @examples
#' \dontrun{
#' # View bundled starting points
#' data(pnadc_series_starting_points)
#' head(pnadc_series_starting_points)
#'
#' # See which series are available
#' unique(pnadc_series_starting_points$series_name)
#' }
#'
"pnadc_series_starting_points"
