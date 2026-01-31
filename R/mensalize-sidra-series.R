#' Mensalize SIDRA Rolling Quarter Series
#'
#' Functions to convert IBGE's rolling quarterly (trimestre movel) series into
#' exact monthly estimates using the methodology from Hecksher (2024).
#'
#' @name mensalize-sidra-series
#' @keywords internal
NULL

#' Convert Rolling Quarters to Exact Monthly Series
#'
#' Transforms SIDRA rolling quarterly averages into exact monthly values using
#' the mathematical relationship between consecutive rolling quarters.
#'
#' @param rolling_quarters data.table from \code{\link{fetch_sidra_rolling_quarters}}
#'   containing rolling quarter series with columns \code{anomesfinaltrimmovel},
#'   \code{mesnotrim}, and series value columns.
#' @param starting_points Optional data.table with precomputed starting points
#'   (y0 values). If NULL (default), uses bundled \code{pnadc_series_starting_points}.
#'   See Details for format.
#' @param series Character vector of series names to mensalize, or "all" (default)
#'   for all series in the input data (except price indices).
#' @param compute_derived Logical. Compute derived series (rates, aggregates)?
#'   Default TRUE.
#' @param verbose Logical. Print progress messages? Default TRUE.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{anomesexato}{Integer. YYYYMM exact month}
#'     \item{m_<series>}{Numeric. Mensalized value for each series}
#'   }
#'
#' @details
#' The algorithm exploits the mathematical property of rolling quarterly averages:
#'
#' \deqn{RQ_t - RQ_{t-1} = (Month_t - Month_{t-3}) / 3}
#'
#' This means exact 3-month variations can be extracted from consecutive rolling
#' quarters. By accumulating these variations separately for each month-position
#' (1, 2, or 3), we build cumulative variation series. The only unknown is the
#' starting level for Jan, Feb, and Mar 2012.
#'
#' **Starting points** are estimated by:
#' 1. Computing monthly estimates from calibrated microdata (z_ variables)
#' 2. Calculating cumulative variations from SIDRA (cum_ variables)
#' 3. Backprojecting: e0 = z_ - cum_ over calibration period (2013-2019)
#' 4. Averaging e0 by month position to get y0_ for each position
#'
#' **Final adjustment** ensures the average of 3 consecutive mensalized values
#' equals the original rolling quarter value.
#'
#' @section Starting Points Format:
#' If providing custom starting points, the data.table must have columns:
#' \itemize{
#'   \item \code{series_name}: Character. Series name matching rolling_quarters columns
#'   \item \code{mesnotrim}: Integer (1, 2, or 3). Month position in quarter
#'   \item \code{y0}: Numeric. Starting point value
#' }
#'
#' @section Mathematical Foundation:
#' The mensalization algorithm proceeds in steps:
#' \enumerate{
#'   \item Calculate d3 = 3 * (RQ_t - RQ_t-1)
#'   \item Separate d3 by month position: d3m1, d3m2, d3m3
#'   \item Cumulate separately: cum1, cum2, cum3
#'   \item Apply starting points: y = y0 + cum
#'   \item Final adjustment for rolling quarter consistency
#' }
#'
#' @examples
#' \dontrun{
#' # Fetch rolling quarters and mensalize
#' rq <- fetch_sidra_rolling_quarters(category = "population")
#' monthly <- mensalize_sidra_series(rq)
#'
#' # View unemployment rate over time
#' monthly[, .(anomesexato, m_popocup, m_popdesocup)]
#' }
#'
#' @seealso
#' \code{\link{fetch_sidra_rolling_quarters}} to obtain input data
#' \code{\link{compute_series_starting_points}} for custom calibration
#'
#' @references
#' Hecksher, Marcos (2024). "Mensalizacao da PNADC: Metodologia e Resultados."
#' Apresentacao MEGE-UCAM, abril 2024.
#'
#' @export
mensalize_sidra_series <- function(rolling_quarters,
                                    starting_points = NULL,
                                    series = "all",
                                    compute_derived = TRUE,
                                    verbose = TRUE) {

  # Input validation
  if (!inherits(rolling_quarters, "data.table")) {
    rolling_quarters <- data.table::as.data.table(rolling_quarters)
  }

  required_cols <- c("anomesfinaltrimmovel", "mesnotrim")
  missing <- setdiff(required_cols, names(rolling_quarters))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  # Get series columns (exclude metadata columns)
  meta_cols <- c("anomesfinaltrimmovel", "mesnotrim")
  all_series <- setdiff(names(rolling_quarters), meta_cols)

  # Filter out price indices (they don't need mensalization - already monthly)
  price_indices <- c("ipca100dez1993", "ipcavarmensal",
                     "inpc100dez1993", "inpcvarmensal")
  all_series <- setdiff(all_series, price_indices)

  if (length(all_series) == 0) {
    stop("No series columns found in rolling_quarters")
  }

  # Select series to process
  if (!identical(series, "all")) {
    invalid <- setdiff(series, all_series)
    if (length(invalid) > 0) {
      stop("Series not found in input: ", paste(invalid, collapse = ", "))
    }
    all_series <- series
  }

  # Load bundled starting points if not provided
  if (is.null(starting_points)) {
    # Try to load from package data
    if (exists("pnadc_series_starting_points", envir = asNamespace("PNADCperiods"))) {
      starting_points <- get("pnadc_series_starting_points",
                             envir = asNamespace("PNADCperiods"))
      if (verbose) message("Using bundled starting points")
    } else {
      # Starting points not available - use zeros with warning
      warning("Bundled starting points not found. Using zeros. ",
              "Results will have incorrect levels but correct variations.")
      starting_points <- data.table::data.table(
        series_name = rep(all_series, each = 3),
        mesnotrim = rep(1:3, length(all_series)),
        y0 = 0
      )
    }
  }

  # Validate starting points format
  if (!inherits(starting_points, "data.table")) {
    starting_points <- data.table::as.data.table(starting_points)
  }

  sp_required <- c("series_name", "mesnotrim", "y0")
  sp_missing <- setdiff(sp_required, names(starting_points))
  if (length(sp_missing) > 0) {
    stop("Starting points missing columns: ", paste(sp_missing, collapse = ", "))
  }

  if (verbose) {
    message("Mensalizing ", length(all_series), " series...")
  }

  # Make a copy to avoid modifying input
  dt <- data.table::copy(rolling_quarters)
  data.table::setorder(dt, anomesfinaltrimmovel)

  # Initialize result with time columns
  result <- data.table::data.table(
    anomesexato = dt$anomesfinaltrimmovel  # Same as end month of rolling quarter
  )

  # Process each series
  for (v in all_series) {
    if (verbose && length(all_series) > 5) {
      message("  Processing: ", v)
    }

    m_values <- .mensalize_single_series(
      dt = dt,
      series_name = v,
      starting_points = starting_points
    )

    # Add to result with m_ prefix
    result[, paste0("m_", v) := m_values]
  }

  # Compute derived series if requested
  if (compute_derived) {
    if (verbose) message("Computing derived series...")
    result <- .compute_derived_series(result, verbose = verbose)
  }

  if (verbose) {
    message("Mensalization complete: ", nrow(result), " months (",
            min(result$anomesexato), " to ", max(result$anomesexato), ")")
  }

  result
}


#' Mensalize a Single Series
#'
#' Internal function implementing the core mensalization algorithm for one series.
#'
#' @param dt data.table with rolling quarter data
#' @param series_name Name of the series column
#' @param starting_points data.table with y0 values
#' @return Numeric vector of mensalized values
#' @keywords internal
#' @noRd
.mensalize_single_series <- function(dt, series_name, starting_points) {

  n <- nrow(dt)
  rq <- dt[[series_name]]
  mesnotrim <- dt$mesnotrim

  # Step 1: Calculate d3 = 3 × (RQ_t - RQ_{t-1})
  d3 <- 3 * (rq - data.table::shift(rq, 1L))
  d3[1] <- NA_real_

  # Step 2: Separate by month position
  d3m1 <- ifelse(mesnotrim == 1, d3, NA_real_)
  d3m2 <- ifelse(mesnotrim == 2, d3, NA_real_)
  d3m3 <- ifelse(mesnotrim == 3, d3, NA_real_)

  # Step 3: Cumulative sum (ignoring NAs)
  cum1 <- cumsum(ifelse(is.na(d3m1), 0, d3m1))
  cum2 <- cumsum(ifelse(is.na(d3m2), 0, d3m2))
  cum3 <- cumsum(ifelse(is.na(d3m3), 0, d3m3))

  # Step 4: Combine based on mesnotrim
  cum <- numeric(n)
  cum[mesnotrim == 1] <- cum1[mesnotrim == 1]
  cum[mesnotrim == 2] <- cum2[mesnotrim == 2]
  cum[mesnotrim == 3] <- cum3[mesnotrim == 3]

  # Step 5: Get starting points (y0 for each mesnotrim position)
  # Use local variable to avoid data.table scoping issues
  target_series <- series_name
  sp <- starting_points[get("series_name") == target_series]

  if (nrow(sp) == 0) {
    # No starting points for this series - use zeros
    y0 <- c(0, 0, 0)
  } else {
    y0 <- numeric(3)
    for (pos in 1:3) {
      val <- sp[mesnotrim == pos, y0]
      y0[pos] <- if (length(val) > 0) val[1] else 0
    }
  }

  # Step 6: Apply starting points: y = y0[mesnotrim] + cum
  y <- y0[mesnotrim] + cum

  # Step 7: Final adjustment for rolling quarter consistency
  # This ensures that the average of 3 consecutive months equals the rolling quarter
  #
  # The rolling quarter at position k covers exact months at positions k-2, k-1, k
  # (where position k has mesnotrim==3).
  #
  # For each trio of consecutive rows (mesnotrim 1,2,3):
  # - All three should average to the rolling quarter value at the mesnotrim==3 position
  # - Formula: m = y + rq - avg(y) where rq is from the mesnotrim==3 position

  # Shift: positive n with type="lead" shifts values UP (future -> current)
  # e.g., y_lead1[i] = y[i+1]
  y_lead1 <- data.table::shift(y, 1L, type = "lead")
  y_lead2 <- data.table::shift(y, 2L, type = "lead")
  y_lag1 <- data.table::shift(y, 1L, type = "lag")
  y_lag2 <- data.table::shift(y, 2L, type = "lag")

  # Get rolling quarter value from the mesnotrim==3 position for each trio
  rq_lead1 <- data.table::shift(rq, 1L, type = "lead")
  rq_lead2 <- data.table::shift(rq, 2L, type = "lead")

  # Vectorized computation
  m <- numeric(n)

  # For mesnotrim==1: use rq at n+2, avg of y at n, n+1, n+2
  idx1 <- which(mesnotrim == 1)
  for (i in idx1) {
    if (i + 2 <= n && !anyNA(c(y[i], y_lead1[i], y_lead2[i], rq_lead2[i]))) {
      avg_y <- (y[i] + y_lead1[i] + y_lead2[i]) / 3
      m[i] <- y[i] + rq_lead2[i] - avg_y
    } else {
      m[i] <- y[i]
    }
  }

  # For mesnotrim==2: use rq at n+1, avg of y at n-1, n, n+1
  idx2 <- which(mesnotrim == 2)
  for (i in idx2) {
    if (i > 1 && i < n && !anyNA(c(y_lag1[i], y[i], y_lead1[i], rq_lead1[i]))) {
      avg_y <- (y_lag1[i] + y[i] + y_lead1[i]) / 3
      m[i] <- y[i] + rq_lead1[i] - avg_y
    } else {
      m[i] <- y[i]
    }
  }

  # For mesnotrim==3: use rq at n, avg of y at n-2, n-1, n
  idx3 <- which(mesnotrim == 3)
  for (i in idx3) {
    if (i > 2 && !anyNA(c(y_lag2[i], y_lag1[i], y[i], rq[i]))) {
      avg_y <- (y_lag2[i] + y_lag1[i] + y[i]) / 3
      m[i] <- y[i] + rq[i] - avg_y
    } else {
      m[i] <- y[i]
    }
  }

  m
}


#' Compute Derived Series (Rates and Aggregates)
#'
#' Internal function to compute rates and aggregates from primary mensalized series.
#'
#' @param dt data.table with mensalized primary series
#' @param verbose Logical. Print progress?
#' @return data.table with additional derived columns
#' @keywords internal
#' @noRd
.compute_derived_series <- function(dt, verbose = FALSE) {

  # Check which primary series are available
  has_col <- function(x) x %in% names(dt)

  # ============================================================================
  # Aggregates (sums of components)
  # ============================================================================

  # Labor force = employed + unemployed
  if (has_col("m_popocup") && has_col("m_popdesocup")) {
    dt[, m_popnaforca := m_popocup + m_popdesocup]
  }

  # Private sector employees (total)
  if (has_col("m_empregprivcomcart") && has_col("m_empregprivsemcart")) {
    dt[, m_empregpriv := m_empregprivcomcart + m_empregprivsemcart]
  }

  # Domestic workers (total)
  if (has_col("m_domesticocomcart") && has_col("m_domesticosemcart")) {
    dt[, m_domestico := m_domesticocomcart + m_domesticosemcart]
  }

  # Public sector employees (total)
  if (has_col("m_empregpublcomcart") && has_col("m_empregpublsemcart") &&
      has_col("m_estatutmilitar")) {
    dt[, m_empregpubl := m_empregpublcomcart + m_empregpublsemcart + m_estatutmilitar]
  }

  # All employees (total)
  if (has_col("m_empregpriv") && has_col("m_domestico") && has_col("m_empregpubl")) {
    dt[, m_empregado := m_empregpriv + m_domestico + m_empregpubl]
  }

  # Extended labor force
  if (has_col("m_popnaforca") && has_col("m_forcapotencial")) {
    dt[, m_forcaampliada := m_popnaforca + m_forcapotencial]
  }

  # ============================================================================
  # Rates (percentages)
  # ============================================================================

  # Participation rate = (employed + unemployed) / pop14+
  if (has_col("m_popocup") && has_col("m_popdesocup") && has_col("m_pop14mais")) {
    dt[, m_taxapartic_calc := round((m_popocup + m_popdesocup) / m_pop14mais * 100, 1)]
  }

  # Employment rate = employed / pop14+
  if (has_col("m_popocup") && has_col("m_pop14mais")) {
    dt[, m_nivelocup_calc := round(m_popocup / m_pop14mais * 100, 1)]
  }

  # Unemployment level = unemployed / pop14+
  if (has_col("m_popdesocup") && has_col("m_pop14mais")) {
    dt[, m_niveldesocup_calc := round(m_popdesocup / m_pop14mais * 100, 1)]
  }

  # Unemployment rate = unemployed / labor force
  if (has_col("m_popdesocup") && has_col("m_popnaforca")) {
    dt[, m_taxadesocup_calc := round(m_popdesocup / m_popnaforca * 100, 1)]
  }

  # Social security contribution rate
  if (has_col("m_contribuinteprev") && has_col("m_popocup")) {
    dt[, m_perccontribprev_calc := round(m_contribuinteprev / m_popocup * 100, 1)]
  }

  # Combined unemployment + underemployment rate
  if (has_col("m_subocuphoras") && has_col("m_popdesocup") && has_col("m_popnaforca")) {
    dt[, m_taxacombdesosub_calc := round((m_subocuphoras + m_popdesocup) / m_popnaforca * 100, 1)]
  }

  # Combined unemployment + potential labor force rate
  if (has_col("m_popdesocup") && has_col("m_forcapotencial") && has_col("m_forcaampliada")) {
    dt[, m_taxacombdesopot_calc := round((m_popdesocup + m_forcapotencial) / m_forcaampliada * 100, 1)]
  }

  # Composite underutilization rate
  if (has_col("m_subocuphoras") && has_col("m_popdesocup") &&
      has_col("m_forcapotencial") && has_col("m_forcaampliada")) {
    dt[, m_taxacompsubutlz_calc := round(
      (m_subocuphoras + m_popdesocup + m_forcapotencial) / m_forcaampliada * 100, 1
    )]
  }

  # Underemployment rate
  if (has_col("m_subocuphoras") && has_col("m_popocup")) {
    dt[, m_taxasubocuphoras_calc := round(m_subocuphoras / m_popocup * 100, 1)]
  }

  # Discouraged worker percentage
  if (has_col("m_desalentado") && has_col("m_popnaforca")) {
    dt[, m_percdesalento_calc := round(m_desalentado / (m_popnaforca + m_desalentado) * 100, 1)]
  }

  dt
}


#' Compute Starting Points from Microdata
#'
#' For advanced users who want to compute custom starting points using their
#' own calibrated microdata estimates.
#'
#' @param monthly_estimates data.table with columns:
#'   \itemize{
#'     \item \code{anomesexato}: YYYYMM exact month
#'     \item \code{z_<series>}: Monthly estimates from calibrated microdata
#'   }
#' @param rolling_quarters data.table from \code{fetch_sidra_rolling_quarters}
#' @param calibration_start Integer. Start of calibration period (YYYYMM). Default 201301.
#' @param calibration_end Integer. End of calibration period (YYYYMM). Default 201912.
#' @param scale_factor Numeric. Scale factor for z_ values (usually 1000). Default 1000.
#' @param verbose Logical. Print progress? Default TRUE.
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{series_name}{Character. Series name}
#'     \item{mesnotrim}{Integer. Month position (1, 2, or 3)}
#'     \item{y0}{Numeric. Starting point value}
#'   }
#'
#' @details
#' The starting points (y0) are computed by:
#' 1. Calculating cumulative variations from SIDRA rolling quarters
#' 2. Computing backprojection: e0 = z / scale_factor - cum
#' 3. Averaging e0 by mesnotrim over the calibration period
#'
#' @examples
#' \dontrun{
#' # After calibrating microdata with pnadc_apply_periods():
#' # monthly_est <- compute_monthly_estimates(calibrated_data)
#' # rq <- fetch_sidra_rolling_quarters()
#' # y0 <- compute_series_starting_points(monthly_est, rq)
#' # monthly <- mensalize_sidra_series(rq, starting_points = y0)
#' }
#'
#' @export
compute_series_starting_points <- function(monthly_estimates,
                                            rolling_quarters,
                                            calibration_start = 201301L,
                                            calibration_end = 201912L,
                                            scale_factor = 1000,
                                            verbose = TRUE) {

  # Ensure data.tables
  if (!inherits(monthly_estimates, "data.table")) {
    monthly_estimates <- data.table::as.data.table(monthly_estimates)
  }
  if (!inherits(rolling_quarters, "data.table")) {
    rolling_quarters <- data.table::as.data.table(rolling_quarters)
  }

  # Find z_ series in monthly estimates
  z_cols <- grep("^z_", names(monthly_estimates), value = TRUE)
  if (length(z_cols) == 0) {
    stop("No z_ columns found in monthly_estimates")
  }

  series_names <- sub("^z_", "", z_cols)

  if (verbose) {
    message("Computing starting points for ", length(series_names), " series")
    message("Calibration period: ", calibration_start, " to ", calibration_end)
  }

  # Prepare rolling quarters
  rq <- data.table::copy(rolling_quarters)
  data.table::setorder(rq, anomesfinaltrimmovel)

  # Add mesnotrim if missing
  if (!"mesnotrim" %in% names(rq)) {
    rq[, mesnotrim := ((anomesfinaltrimmovel %% 100L - 1L) %% 3L) + 1L]
  }

  # Merge with monthly estimates
  me <- data.table::copy(monthly_estimates)
  data.table::setnames(me, "anomesexato", "anomesfinaltrimmovel", skip_absent = TRUE)

  if (!"anomesfinaltrimmovel" %in% names(me)) {
    stop("monthly_estimates must have 'anomesexato' or 'anomesfinaltrimmovel' column")
  }

  dt <- merge(rq, me, by = "anomesfinaltrimmovel", all.x = TRUE)
  data.table::setorder(dt, anomesfinaltrimmovel)

  # Compute starting points for each series
  results <- list()

  for (v in series_names) {
    z_col <- paste0("z_", v)

    if (!v %in% names(dt)) {
      if (verbose) message("  Skipping ", v, " (not in rolling_quarters)")
      next
    }
    if (!z_col %in% names(dt)) {
      if (verbose) message("  Skipping ", v, " (no z_ column)")
      next
    }

    if (verbose) message("  Processing: ", v)

    rq_values <- dt[[v]]
    z_values <- dt[[z_col]]
    mesnotrim <- dt$mesnotrim

    # Calculate cumulative variations
    d3 <- 3 * (rq_values - data.table::shift(rq_values, 1L))
    d3[1] <- NA_real_

    d3m1 <- ifelse(mesnotrim == 1, d3, NA_real_)
    d3m2 <- ifelse(mesnotrim == 2, d3, NA_real_)
    d3m3 <- ifelse(mesnotrim == 3, d3, NA_real_)

    cum1 <- cumsum(ifelse(is.na(d3m1), 0, d3m1))
    cum2 <- cumsum(ifelse(is.na(d3m2), 0, d3m2))
    cum3 <- cumsum(ifelse(is.na(d3m3), 0, d3m3))

    cum <- numeric(length(rq_values))
    cum[mesnotrim == 1] <- cum1[mesnotrim == 1]
    cum[mesnotrim == 2] <- cum2[mesnotrim == 2]
    cum[mesnotrim == 3] <- cum3[mesnotrim == 3]

    # Backprojection error
    e0 <- z_values / scale_factor - cum

    # Filter to calibration period
    yyyymm <- dt$anomesfinaltrimmovel
    in_calibration <- yyyymm >= calibration_start & yyyymm <= calibration_end

    # Average by mesnotrim
    for (pos in 1:3) {
      mask <- in_calibration & mesnotrim == pos
      y0_val <- mean(e0[mask], na.rm = TRUE)

      if (!is.finite(y0_val)) y0_val <- 0

      results[[length(results) + 1]] <- data.table::data.table(
        series_name = v,
        mesnotrim = pos,
        y0 = y0_val
      )
    }
  }

  result <- data.table::rbindlist(results)

  if (verbose) {
    message("Computed starting points for ", length(unique(result$series_name)), " series")
  }

  result
}
