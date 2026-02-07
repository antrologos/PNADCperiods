#' Mensalize SIDRA Rolling Quarter Series
#'
#' Functions to convert IBGE's rolling quarterly (trimestre movel) series into
#' exact monthly estimates.
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

  # Filter out rate series - they are DERIVED from mensalized components
  # (e.g., m_taxadesocup = m_popdesocup / m_popnaforca * 100)
  # Mensalizing rates directly is WRONG - Marcos derives them post-mensalization
  rate_series <- c("taxapartic", "nivelocup", "niveldesocup", "taxadesocup",
                   "perccontribprev", "taxasubocuphoras", "taxacombdesosub",
                   "taxacombdesopot", "taxacompsubutlz", "percdesalento")
  all_series <- setdiff(all_series, rate_series)

  # Filter out average income series - they are DERIVED from mensalized components
  # Marcos: m_rendhabnominaltodos = m_massahabnominaltodos / m_comrendtodos * 1000
  # Mensalizing ratios directly is mathematically WRONG
  # Note: Both nominal and real (INPC-deflated) averages are derived
  avg_income_series <- c("rendhabnominaltodos", "rendefetnominaltodos",
                         "rendhabrealtodos", "rendefetrealtodos",
                         "rendhabrealprinc", "rendefetrealprinc")
  all_series <- setdiff(all_series, avg_income_series)

  # Filter out residual series - they are COMPUTED from mensalized components
  # Residual series are computed from mensalized components (see derived series logic)
  # to ensure subcategories sum exactly to parent totals
  residual_series <- c("popforadaforca", "empregadorsemcnpj",
                       "contapropriasemcnpj", "trabfamauxiliar")
  all_series <- setdiff(all_series, residual_series)

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
    # With LazyData: true, data is accessible via getExportedValue or directly
    starting_points <- tryCatch({
      # Try to get from package namespace (works with LazyData: true)
      getExportedValue("PNADCperiods", "pnadc_series_starting_points")
    }, error = function(e) {
      # Fallback: try to load via data()
      tryCatch({
        env <- new.env()
        data("pnadc_series_starting_points", package = "PNADCperiods", envir = env)
        env$pnadc_series_starting_points
      }, error = function(e2) NULL)
    })

    if (!is.null(starting_points)) {
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

  # Compute comrendtodos (number of people with income) from SIDRA data
  # This is needed to derive average income series (rendhabnominaltodos, rendefetnominaltodos)
  # Formula: comrendtodos = (massa/rend + massa/rend)/2 * 1000
  # The SIDRA series are: massa in millions, rend in reais, so result is in thousands
  if ("massahabnominaltodos" %in% names(dt) && "rendhabnominaltodos" %in% names(dt) &&
      "massaefetnominaltodos" %in% names(dt) && "rendefetnominaltodos" %in% names(dt)) {
    # Note: massa is in millions, rend is in reais
    # massa/rend gives millions of people, multiply by 1000 to get thousands
    # IMPORTANT: Guard against division by zero - only compute where denominators are positive
    dt[rendhabnominaltodos > 0 & rendefetnominaltodos > 0,
       comrendtodos := (massahabnominaltodos / rendhabnominaltodos +
                        massaefetnominaltodos / rendefetnominaltodos) / 2 * 1000]
    if (verbose) message("Computed comrendtodos from SIDRA massa/rend series")
    # Add comrendtodos to the series list for mensalization
    # (it's derived from SIDRA but needs to be mensalized like other population counts)
    if (!"comrendtodos" %in% all_series) {
      all_series <- c(all_series, "comrendtodos")
    }
  }

  # PNADC started in January 2012; first rolling quarter is March 2012 (201203)

  # Filter to PNADC-era data only. Price indices (IPCA) go back to 1979 but
  # mensalization is only valid for PNADC series. Without this filter, pre-2012
  # rows would just output the y0 starting points cyclically (no actual data).
  pnadc_start <- .PNADC_DATES$PNADC_START
  if (min(dt$anomesfinaltrimmovel) < pnadc_start) {
    n_pre_pnadc <- sum(dt$anomesfinaltrimmovel < pnadc_start)
    if (verbose) {
      message("Filtering out ", n_pre_pnadc, " pre-PNADC rows (before ", pnadc_start, ")")
    }
    dt <- dt[anomesfinaltrimmovel >= pnadc_start]
  }

  data.table::setorder(dt, anomesfinaltrimmovel)

  # Initialize result with time columns
  result <- data.table::data.table(
    anomesexato = dt$anomesfinaltrimmovel  # Same as end month of rolling quarter
  )

  # Define split-calibration series (require two separate mensalizations stitched together)
  # subocuphoras: VD4004 pre-201509, VD4004A post-201509 (split calibration)
  split_series <- c("subocuphoras")

  # Process each series
  for (v in all_series) {
    if (verbose && length(all_series) > 5) {
      message("  Processing: ", v)
    }

    # Check if this series needs split calibration
    if (v %in% split_series) {
      # Use split mensalization (two separate processes stitched at VD4004 split)
      if (verbose) message("    (using split calibration for ", v, ")")
      m_values <- .mensalize_split_series(
        dt = dt,
        series_name = v,
        starting_points = starting_points,
        split_month = .PNADC_DATES$VD4004_SPLIT
      )
    } else {
      # Standard single mensalization
      m_values <- .mensalize_single_series(
        dt = dt,
        series_name = v,
        starting_points = starting_points
      )
    }

    # Add to result with m_ prefix
    result[, paste0("m_", v) := m_values]
  }

  # Add price indices (pass through - already monthly, no mensalization needed)
  price_indices <- c("ipca100dez1993", "ipcavarmensal",
                     "inpc100dez1993", "inpcvarmensal")
  for (pi in price_indices) {
    if (pi %in% names(dt)) {
      result[, (pi) := dt[[pi]]]
    }
  }

  # Note: comrendtodos is now mensalized (added to all_series above)
  # and will be available as m_comrendtodos for deriving average income series

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


# =============================================================================
# MENSALIZATION HELPER FUNCTIONS
# =============================================================================

#' Compute Cumulative Sum by Month Position
#'
#' Internal helper that computes d3 differences and cumsum separated by mesnotrim.
#'
#' @param rq Numeric vector of rolling quarter values
#' @param mesnotrim Integer vector of month positions (1, 2, or 3)
#' @param filter_mask Logical vector. If provided, NA out d3 where FALSE before cumsum.
#' @return Numeric vector of cumulative sums aligned by mesnotrim
#' @keywords internal
#' @noRd
.compute_cumsum_by_mesnotrim <- function(rq, mesnotrim, filter_mask = NULL) {
  n <- length(rq)

  # Step 1: Calculate d3 = 3 × (RQ_t - RQ_{t-1})
  d3 <- 3 * (rq - data.table::shift(rq, 1L))

  # Set first observation of EACH mesnotrim position to NA

  # This ensures each position's cumsum starts at 0, avoiding pre-PNADC contamination
  # for the first few months (201201, 201202, 201203)
  first_pos1 <- which(mesnotrim == 1)[1]
  first_pos2 <- which(mesnotrim == 2)[1]
  first_pos3 <- which(mesnotrim == 3)[1]
  first_positions <- stats::na.omit(c(first_pos1, first_pos2, first_pos3))
  d3[first_positions] <- NA_real_

  # Apply filter mask if provided (for split series)
  if (!is.null(filter_mask)) {
    d3[!filter_mask] <- NA_real_
  }

  # Step 2: Separate by month position
  d3m1 <- ifelse(mesnotrim == 1, d3, NA_real_)
  d3m2 <- ifelse(mesnotrim == 2, d3, NA_real_)
  d3m3 <- ifelse(mesnotrim == 3, d3, NA_real_)

  # Track where we have actual data vs NA from ORIGINAL rq (not d3)
  # This is used to restore NA for series that start later (e.g., CNPJ from 201510)
  rq_has_data <- !is.na(rq)
  has_data1 <- ifelse(mesnotrim == 1, rq_has_data, FALSE)
  has_data2 <- ifelse(mesnotrim == 2, rq_has_data, FALSE)
  has_data3 <- ifelse(mesnotrim == 3, rq_has_data, FALSE)

  # Step 3: Cumulative sum (treating NAs as 0 for accumulation)
  cum1 <- cumsum(ifelse(is.na(d3m1), 0, d3m1))
  cum2 <- cumsum(ifelse(is.na(d3m2), 0, d3m2))
  cum3 <- cumsum(ifelse(is.na(d3m3), 0, d3m3))

  # For series that start later (e.g., CNPJ from 201510), we want NA before first data
  # Find the first index where the series has ANY data
  first_any_data <- which(rq_has_data)[1]

  # If no data at all, all positions are invalid
  if (is.na(first_any_data)) {
    cum1[] <- NA_real_
    cum2[] <- NA_real_
    cum3[] <- NA_real_
  } else {
    # Series start window: first_any_data and 2 positions before (for the 3 mesnotrims)
    # E.g., if first data is at 201512 (idx 48), then 201510 (idx 46) and 201511 (idx 47)
    # should also be valid since they have y0 starting points
    series_start <- max(1L, first_any_data - 2L)

    # Mark all positions at or after series_start as valid for their respective mesnotrim
    valid_range <- seq_len(n) >= series_start

    # For core series with early data, also mark structural first positions
    has_early_data <- any(rq_has_data[1:min(6, n)])
    if (has_early_data) {
      valid_range[first_pos1] <- TRUE
      valid_range[first_pos2] <- TRUE
      valid_range[first_pos3] <- TRUE
    }

    # Apply validity: positions in valid_range with matching mesnotrim get cum, others NA
    first_valid1 <- valid_range & (mesnotrim == 1 | cummax(has_data1))
    first_valid2 <- valid_range & (mesnotrim == 2 | cummax(has_data2))
    first_valid3 <- valid_range & (mesnotrim == 3 | cummax(has_data3))

    # Simpler approach: just mark positions at or after series_start as valid
    first_valid1 <- valid_range | as.logical(cummax(has_data1))
    first_valid2 <- valid_range | as.logical(cummax(has_data2))
    first_valid3 <- valid_range | as.logical(cummax(has_data3))

    cum1[!first_valid1] <- NA_real_
    cum2[!first_valid2] <- NA_real_
    cum3[!first_valid3] <- NA_real_
  }

  # Step 4: Combine based on mesnotrim
  cum <- rep(NA_real_, n)
  cum[mesnotrim == 1] <- cum1[mesnotrim == 1]
  cum[mesnotrim == 2] <- cum2[mesnotrim == 2]
  cum[mesnotrim == 3] <- cum3[mesnotrim == 3]

  cum
}


#' Extract y0 Starting Points Vector
#'
#' Internal helper that extracts y0 values for a series from starting_points.
#'
#' @param starting_points data.table with series_name, mesnotrim, y0 columns
#' @param target_series Character. Series name to extract.
#' @return Numeric vector of length 3 (y0 for mesnotrim 1, 2, 3)
#' @keywords internal
#' @noRd
.extract_y0_vector <- function(starting_points, target_series) {
  sp <- starting_points[get("series_name") == target_series]

  # Return NA for series without calibrated starting points
  # This ensures uncalibrated series show NA instead of 0
  if (nrow(sp) == 0) {
    return(c(NA_real_, NA_real_, NA_real_))
  }

  y0 <- rep(NA_real_, 3)
  for (pos in 1:3) {
    val <- sp[mesnotrim == pos, y0]
    y0[pos] <- if (length(val) > 0) val[1] else NA_real_
  }

  y0
}


#' Apply Final Rolling Quarter Adjustment
#'
#' Internal helper that applies the final adjustment to ensure rolling quarter consistency.
#' For each trio of consecutive months, the average equals the rolling quarter value.
#'
#' Uses fully vectorized operations for performance - no for loops.
#'
#' @param y Numeric vector of y values (y0 + cumsum)
#' @param rq Numeric vector of rolling quarter values
#' @param mesnotrim Integer vector of month positions (1, 2, or 3)
#' @return Numeric vector of final mensalized values
#' @keywords internal
#' @noRd
.apply_final_adjustment <- function(y, rq, mesnotrim) {
  n <- length(y)

  # Shift values for computing averages
  y_lead1 <- data.table::shift(y, 1L, type = "lead")
  y_lead2 <- data.table::shift(y, 2L, type = "lead")
  y_lag1 <- data.table::shift(y, 1L, type = "lag")
  y_lag2 <- data.table::shift(y, 2L, type = "lag")

  # Get rolling quarter value from the mesnotrim==3 position for each trio
  rq_lead1 <- data.table::shift(rq, 1L, type = "lead")
  rq_lead2 <- data.table::shift(rq, 2L, type = "lead")

  # Initialize result with fallback values (y itself)
  m <- y

  # Vectorized computation for mesnotrim==1
  # avg_y = (y + y_lead1 + y_lead2) / 3, m = y + rq_lead2 - avg_y
  valid1 <- mesnotrim == 1L & !is.na(y) & !is.na(y_lead1) & !is.na(y_lead2) & !is.na(rq_lead2)
  avg_y1 <- (y + y_lead1 + y_lead2) / 3
  m[valid1] <- y[valid1] + rq_lead2[valid1] - avg_y1[valid1]

  # Vectorized computation for mesnotrim==2
  # avg_y = (y_lag1 + y + y_lead1) / 3, m = y + rq_lead1 - avg_y
  valid2 <- mesnotrim == 2L & !is.na(y_lag1) & !is.na(y) & !is.na(y_lead1) & !is.na(rq_lead1)
  avg_y2 <- (y_lag1 + y + y_lead1) / 3
  m[valid2] <- y[valid2] + rq_lead1[valid2] - avg_y2[valid2]

  # Vectorized computation for mesnotrim==3
  # avg_y = (y_lag2 + y_lag1 + y) / 3, m = y + rq - avg_y
  valid3 <- mesnotrim == 3L & !is.na(y_lag2) & !is.na(y_lag1) & !is.na(y) & !is.na(rq)
  avg_y3 <- (y_lag2 + y_lag1 + y) / 3
  m[valid3] <- y[valid3] + rq[valid3] - avg_y3[valid3]

  m
}


# =============================================================================
# MENSALIZATION FUNCTIONS
# =============================================================================

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

  rq <- dt[[series_name]]
  mesnotrim <- dt$mesnotrim

  # Step 1-4: Compute cumsum by month position
  cum <- .compute_cumsum_by_mesnotrim(rq, mesnotrim)

  # Step 5: Get starting points
  y0 <- .extract_y0_vector(starting_points, series_name)

  # Step 6: Apply starting points
  y <- y0[mesnotrim] + cum

  # Step 7: Final adjustment for rolling quarter consistency
  .apply_final_adjustment(y, rq, mesnotrim)
}


#' Mensalize a Split-Calibration Series
#'
#' Internal function for series that require split calibration (like subocuphoras).
#' Performs TWO separate mensalizations and stitches them together at the split point.
#'
#' The key insight is that each period must be processed INDEPENDENTLY to avoid
#' cross-contamination at the boundary. The final adjustment uses lead/lag operations
#' that would otherwise cross the 201509/201510 boundary.
#'
#' @param dt data.table with rolling quarter data
#' @param series_name Name of the series column (e.g., "subocuphoras")
#' @param starting_points data.table with y0 values (must include both "series" and "series_pre")
#' @param split_month Integer. The last month of the pre-split period.
#'   Default NULL uses .PNADC_DATES$VD4004_SPLIT (201509).
#' @return Numeric vector of mensalized values
#' @keywords internal
#' @noRd
.mensalize_split_series <- function(dt, series_name, starting_points, split_month = NULL) {

  # Resolve default from centralized constants

  if (is.null(split_month)) {
    split_month <- .PNADC_DATES$VD4004_SPLIT
  }

  rq <- dt[[series_name]]
  mesnotrim <- dt$mesnotrim
  yyyymm <- dt$anomesfinaltrimmovel
  n <- length(rq)

  # Initialize result vector
  m_result <- numeric(n)

  # ============================================================================
  # PART 1: Process PRE-SPLIT period independently (up to and including split_month)
  # ============================================================================

  pre_idx <- which(yyyymm <= split_month)
  if (length(pre_idx) > 0) {
    # Extract subset for pre-split period
    rq_pre <- rq[pre_idx]
    mesnotrim_pre <- mesnotrim[pre_idx]

    # Compute cumsum for pre-split period only
    cum_pre <- .compute_cumsum_by_mesnotrim(rq_pre, mesnotrim_pre)

    # Get starting points for pre-split
    y0_pre <- .extract_y0_vector(starting_points, paste0(series_name, "_pre"))
    y_pre <- y0_pre[mesnotrim_pre] + cum_pre

    # Apply final adjustment ONLY to pre-split data (no boundary crossing)
    m_pre <- .apply_final_adjustment(y_pre, rq_pre, mesnotrim_pre)

    # Store results
    m_result[pre_idx] <- m_pre
  }

  # ============================================================================
  # PART 2: Process POST-SPLIT period independently (after split_month)
  # ============================================================================

  post_idx <- which(yyyymm > split_month)
  if (length(post_idx) > 0) {
    # Extract subset for post-split period
    rq_post <- rq[post_idx]
    mesnotrim_post <- mesnotrim[post_idx]

    # Compute cumsum for post-split period only (cumsum starts fresh from 201510)
    cum_post <- .compute_cumsum_by_mesnotrim(rq_post, mesnotrim_post)

    # Get starting points for post-split
    y0_post <- .extract_y0_vector(starting_points, series_name)
    y_post <- y0_post[mesnotrim_post] + cum_post

    # Apply final adjustment ONLY to post-split data (no boundary crossing)
    m_post <- .apply_final_adjustment(y_post, rq_post, mesnotrim_post)

    # Store results
    m_result[post_idx] <- m_post
  }

  m_result
}


#' Compute Derived Series (Rates and Aggregates)
#'
#' Internal function to compute rates and aggregates from primary mensalized series.
#'
#' @section Why Derived Series Are Computed Here (Not From Starting Points):
#'
#' Some series have two possible computation paths:
#' \enumerate{
#'   \item **Direct mensalization**: Use z_ aggregates from microdata for starting
#'     points, then mensalize the SIDRA rolling quarter series
#'   \item **Derived computation**: Compute from other mensalized series (sums, ratios)
#' }
#'
#' For certain series, we use path 2 (derived) because:
#'
#' \itemize{
#'   \item **Rate series** (taxadesocup, percdesalento, etc.): Computed as ratios
#'     of mensalized populations. This ensures mathematical consistency -
#'     rate = mensalized_numerator / mensalized_denominator
#'   \item **Aggregate sums** (popnaforca = popocup + popdesocup): Ensures parts
#'     sum exactly to totals after mensalization
#'   \item **Residual series** (empregadorsemcnpj = empregador - empregadorcomcnpj):
#'     Ensures subcategories sum to parent totals (accounting identity)
#' }
#'
#' The alternative (direct mensalization) would require separate z_ aggregates
#' for each rate/aggregate, and the resulting mensalized values wouldn't have
#' guaranteed mathematical relationships with their components.
#'
#' @param dt data.table with mensalized primary series
#' @param verbose Logical. Print progress?
#' @return data.table with additional derived columns
#' @keywords internal
#' @noRd
.compute_derived_series <- function(dt, verbose = FALSE) {

  # Helper to check column existence (with m_ prefix)
  has_col <- function(x) paste0("m_", x) %in% names(dt)
  has_col_raw <- function(x) x %in% names(dt)

  # ============================================================================
  # PHASE 1: AGGREGATES (sums of components)
  # Must be computed first as some rates depend on these aggregates
  # ============================================================================
  for (spec in .DERIVED_SERIES_SPEC$aggregates) {
    if (all(sapply(spec$components, has_col))) {
      cols <- paste0("m_", spec$components)
      dt[, paste0("m_", spec$name) := Reduce(`+`, .SD), .SDcols = cols]
      if (verbose) message("    Computed aggregate: m_", spec$name)
    }
  }

  # ============================================================================
  # PHASE 2: AVERAGE INCOME (massa / comrendtodos * 1000)
  # ============================================================================
  for (spec in .DERIVED_SERIES_SPEC$average_income) {
    if (has_col(spec$numerator) && has_col(spec$denominator)) {
      num_col <- paste0("m_", spec$numerator)
      denom_col <- paste0("m_", spec$denominator)
      out_col <- paste0("m_", spec$name)
      dt[get(denom_col) > 0, (out_col) := round(get(num_col) / get(denom_col) * spec$multiplier, spec$decimals)]
      if (verbose) message("    Computed average income: m_", spec$name)
    }
  }

  # ============================================================================
  # PHASE 3: RESIDUALS (parent - sum of subtracted components)
  # Ensures accounting identities hold
  # ============================================================================
  for (spec in .DERIVED_SERIES_SPEC$residuals) {
    if (has_col(spec$parent) && all(sapply(spec$subtract, has_col))) {
      parent_col <- paste0("m_", spec$parent)
      subtract_cols <- paste0("m_", spec$subtract)
      out_col <- paste0("m_", spec$name)
      dt[, (out_col) := get(parent_col) - Reduce(`+`, .SD), .SDcols = subtract_cols]
      if (verbose) message("    Computed residual: m_", spec$name)
    }
  }

  # ============================================================================
  # PHASE 4: RATES (numerator / denominator * 100)
  # ============================================================================
  for (spec in .DERIVED_SERIES_SPEC$rates) {
    # Build list of numerator components (may be single or multiple)
    num_components <- if (is.list(spec$numerator)) spec$numerator else list(spec$numerator)
    num_is_sum <- length(num_components) > 1 || (is.list(spec$numerator) && length(spec$numerator) > 1)

    # Check if all required columns exist
    all_num_exist <- all(sapply(unlist(num_components), has_col))
    denom_components <- if (is.list(spec$denominator)) spec$denominator else list(spec$denominator)
    all_denom_exist <- all(sapply(unlist(denom_components), has_col))

    if (all_num_exist && all_denom_exist) {
      # Compute numerator (may be sum of multiple components)
      num_cols <- paste0("m_", unlist(num_components))
      denom_cols <- paste0("m_", unlist(denom_components))
      out_col <- paste0("m_", spec$name)
      decimals <- spec$decimals

      # For rates with compound numerator or denominator
      if (length(num_cols) == 1 && length(denom_cols) == 1) {
        dt[, (out_col) := round(get(num_cols) / get(denom_cols) * 100, decimals)]
      } else {
        # Build expression for compound numerator/denominator
        num_sum <- rowSums(dt[, num_cols, with = FALSE], na.rm = TRUE)
        denom_sum <- rowSums(dt[, denom_cols, with = FALSE], na.rm = TRUE)
        dt[, (out_col) := round(num_sum / denom_sum * 100, decimals)]
      }
      if (verbose) message("    Computed rate: m_", spec$name)
    }
  }

  # ============================================================================
  # PHASE 5: DEFLATED SERIES (nominal * deflator)
  # Uses IPCA index with different lag for hab vs efet series
  # ============================================================================
  if (has_col_raw("ipca100dez1993")) {
    latest_ipca <- dt[anomesexato == max(anomesexato), ipca100dez1993][1]

    if (!is.na(latest_ipca) && latest_ipca > 0) {
      # Pre-compute deflators
      dt[, .deflator_hab := latest_ipca / ipca100dez1993]
      dt[, .ipca_lagged := data.table::shift(ipca100dez1993, n = 1L, type = "lag")]
      dt[, .deflator_efet := latest_ipca / .ipca_lagged]

      for (spec in .DERIVED_SERIES_SPEC$deflated) {
        if (has_col(spec$source)) {
          source_col <- paste0("m_", spec$source)
          out_col <- paste0("m_", spec$name)
          deflator_col <- if (spec$use_lagged_ipca) ".deflator_efet" else ".deflator_hab"
          dt[, (out_col) := round(get(source_col) * get(deflator_col), spec$decimals)]
          if (verbose) message("    Computed deflated: m_", spec$name)
        }
      }

      # Cleanup temporary columns
      dt[, c(".deflator_hab", ".deflator_efet", ".ipca_lagged") := NULL]
    }
  }

  dt
}


# =============================================================================
# DERIVED SERIES SPECIFICATION (metadata-driven)
# =============================================================================
# This declarative structure defines all derived series and their computation.
# Processing order: aggregates -> average_income -> residuals -> rates -> deflated
# =============================================================================

.DERIVED_SERIES_SPEC <- list(

  # ==========================================================================
  # AGGREGATES: output = sum of components
  # Order matters: some aggregates depend on others (e.g., empregado needs empregpriv)
  # ==========================================================================
  aggregates = list(
    list(name = "popnaforca",
         components = c("popocup", "popdesocup"),
         description = "Labor force = employed + unemployed"),

    list(name = "empregpriv",
         components = c("empregprivcomcart", "empregprivsemcart"),
         description = "Private sector employees (total)"),

    list(name = "domestico",
         components = c("domesticocomcart", "domesticosemcart"),
         description = "Domestic workers (total)"),

    list(name = "empregpubl",
         components = c("empregpublcomcart", "empregpublsemcart", "estatutmilitar"),
         description = "Public sector employees (total)"),

    list(name = "empregado",
         components = c("empregpriv", "domestico", "empregpubl"),
         description = "All employees (total) - depends on prior aggregates"),

    list(name = "forcaampliada",
         components = c("popnaforca", "forcapotencial"),
         description = "Extended labor force - depends on popnaforca aggregate")
  ),

  # ==========================================================================
  # AVERAGE INCOME: output = numerator / denominator * multiplier
  # Formula: m_rend = m_massa / m_comrendtodos * 1000
  # ==========================================================================
  average_income = list(
    list(name = "rendhabnominaltodos",
         numerator = "massahabnominaltodos",
         denominator = "comrendtodos",
         multiplier = 1000,
         decimals = 0,
         description = "Average habitual income (nominal)"),

    list(name = "rendefetnominaltodos",
         numerator = "massaefetnominaltodos",
         denominator = "comrendtodos",
         multiplier = 1000,
         decimals = 0,
         description = "Average effective income (nominal)"),

    # INPC-deflated average income series
    # These use the mensalized real (INPC-deflated) mass series
    list(name = "rendhabrealtodos",
         numerator = "massahabrealtodos",
         denominator = "comrendtodos",
         multiplier = 1000,
         decimals = 1,
         description = "Average habitual income (INPC-deflated)"),

    list(name = "rendefetrealtodos",
         numerator = "massaefetrealtodos",
         denominator = "comrendtodos",
         multiplier = 1000,
         decimals = 1,
         description = "Average effective income (INPC-deflated)")
  ),

  # ==========================================================================
  # RESIDUALS: output = parent - sum(subtract)
  # Computed as residuals to ensure accounting identities
  # ==========================================================================
  residuals = list(
    list(name = "popforadaforca",
         parent = "pop14mais",
         subtract = c("popocup", "popdesocup"),
         description = "Population outside labor force"),

    list(name = "empregadorsemcnpj",
         parent = "empregador",
         subtract = c("empregadorcomcnpj"),
         description = "Employers without CNPJ"),

    list(name = "contapropriasemcnpj",
         parent = "contapropria",
         subtract = c("contapropriacomcnpj"),
         description = "Self-employed without CNPJ"),

    list(name = "trabfamauxiliar",
         parent = "popocup",
         subtract = c("empregprivcomcart", "empregprivsemcart",
                      "domesticocomcart", "domesticosemcart",
                      "empregpublcomcart", "empregpublsemcart",
                      "estatutmilitar", "empregador", "contapropria"),
         description = "Unpaid family workers (residual)")
  ),

  # ==========================================================================
  # RATES: output = (sum of numerator) / (sum of denominator) * 100
  # All rates use decimals = 1
  # ==========================================================================
  rates = list(
    list(name = "taxapartic",
         numerator = c("popocup", "popdesocup"),
         denominator = c("pop14mais"),
         decimals = 1,
         description = "Participation rate"),

    list(name = "nivelocup",
         numerator = c("popocup"),
         denominator = c("pop14mais"),
         decimals = 1,
         description = "Employment rate"),

    list(name = "niveldesocup",
         numerator = c("popdesocup"),
         denominator = c("pop14mais"),
         decimals = 1,
         description = "Unemployment level"),

    list(name = "taxadesocup",
         numerator = c("popdesocup"),
         denominator = c("popnaforca"),
         decimals = 1,
         description = "Unemployment rate"),

    list(name = "perccontribprev",
         numerator = c("contribuinteprev"),
         denominator = c("popocup"),
         decimals = 1,
         description = "Social security contribution rate"),

    list(name = "taxacombdesosub",
         numerator = c("subocuphoras", "popdesocup"),
         denominator = c("popnaforca"),
         decimals = 1,
         description = "Combined unemployment + underemployment rate"),

    list(name = "taxacombdesopot",
         numerator = c("popdesocup", "forcapotencial"),
         denominator = c("forcaampliada"),
         decimals = 1,
         description = "Combined unemployment + potential labor force rate"),

    list(name = "taxacompsubutlz",
         numerator = c("subocuphoras", "popdesocup", "forcapotencial"),
         denominator = c("forcaampliada"),
         decimals = 1,
         description = "Composite underutilization rate"),

    list(name = "taxasubocuphoras",
         numerator = c("subocuphoras"),
         denominator = c("popocup"),
         decimals = 1,
         description = "Underemployment rate"),

    list(name = "percdesalento",
         numerator = c("desalentado"),
         denominator = c("popnaforca", "desalentado"),
         decimals = 1,
         description = "Discouraged worker percentage")
  ),

  # ==========================================================================
  # DEFLATED: output = source * deflator (rounded)
  # "hab" series use current IPCA, "efet" series use lagged IPCA
  # ==========================================================================
  deflated = list(
    list(name = "massahabtodosipcabr",
         source = "massahabnominaltodos",
         use_lagged_ipca = FALSE,
         decimals = 0,
         description = "Real habitual income mass (IPCA-deflated)"),

    list(name = "rendhabtodosipcabr",
         source = "rendhabnominaltodos",
         use_lagged_ipca = FALSE,
         decimals = 0,
         description = "Real average habitual income (IPCA-deflated)"),

    list(name = "massaefettodosipcabr",
         source = "massaefetnominaltodos",
         use_lagged_ipca = TRUE,
         decimals = 0,
         description = "Real effective income mass (IPCA-deflated, lagged)"),

    list(name = "rendefettodosipcabr",
         source = "rendefetnominaltodos",
         use_lagged_ipca = TRUE,
         decimals = 0,
         description = "Real average effective income (IPCA-deflated, lagged)")
  )
)


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
#' @param calibration_start Integer. Start of calibration period (YYYYMM).
#'   Default NULL uses .PNADC_DATES$DEFAULT_CALIB_START (201301).
#'   Note: CNPJ series automatically use CNPJ_CALIB_START (201601) regardless.
#' @param calibration_end Integer. End of calibration period (YYYYMM).
#'   Default NULL uses .PNADC_DATES$DEFAULT_CALIB_END (201912).
#' @param scale_factor Numeric. Scale factor for z_ values (usually 1000). Default 1000.
#' @param use_series_specific_periods Logical. If TRUE (default), use series-specific
#'   calibration periods for CNPJ series (201601-201912) and cumsum starting dates
#'   (201510). Set to FALSE to use uniform calibration for all series.
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
#' @section Series-Specific Handling:
#' When \code{use_series_specific_periods = TRUE}, the following series receive
#' special handling for series-specific data availability:
#' \describe{
#'   \item{CNPJ series}{empregadorcomcnpj, empregadorsemcnpj, contapropriacomcnpj,
#'     contapropriasemcnpj use calibration period 201601-201912 and cumsum starts
#'     from 201510 (when V4019 became available)}
#' }
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
                                            calibration_start = NULL,
                                            calibration_end = NULL,
                                            scale_factor = 1000,
                                            use_series_specific_periods = TRUE,
                                            verbose = TRUE) {

  # Resolve defaults from centralized constants
  if (is.null(calibration_start)) {
    calibration_start <- .PNADC_DATES$DEFAULT_CALIB_START
  }
  if (is.null(calibration_end)) {
    calibration_end <- .PNADC_DATES$DEFAULT_CALIB_END
  }

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

  # Define series-specific calibration periods
  # CNPJ series: V4019 only available from 201510, use calibration 201601-201912

  cnpj_series <- c("empregadorcomcnpj", "empregadorsemcnpj",
                   "contapropriacomcnpj", "contapropriasemcnpj")

  # subocuphoras requires SPLIT CALIBRATION
  # The VD4004 -> VD4004A transition at 201510 requires TWO separate mensalizations:
  # - Post-201509: cumsum from 201510, calibration 201601-201912 (same as CNPJ)
  # - Pre-201509: cumsum from start, calibration 201301-201412
  # Then the two are stitched together at 201509
  split_series <- c("subocuphoras")

  if (verbose) {
    message("Computing starting points for ", length(series_names), " series")
    message("Calibration period: ", calibration_start, " to ", calibration_end)
    if (use_series_specific_periods) {
      message("  (CNPJ series will use 201601-201912 with cumsum from 201510)")
      message("  (subocuphoras uses split calibration: post-201509 + pre-201509)")
    }
  }

  # Prepare rolling quarters
  rq <- data.table::copy(rolling_quarters)
  data.table::setorder(rq, anomesfinaltrimmovel)

  # Add mesnotrim if missing (use .get_mesnotrim() for consistency)
  if (!"mesnotrim" %in% names(rq)) {
    rq[, mesnotrim := .get_mesnotrim(anomesfinaltrimmovel %% 100L)]
  }

  # Merge with monthly estimates
  me <- data.table::copy(monthly_estimates)
  data.table::setnames(me, "anomesexato", "anomesfinaltrimmovel", skip_absent = TRUE)

  if (!"anomesfinaltrimmovel" %in% names(me)) {
    stop("monthly_estimates must have 'anomesexato' or 'anomesfinaltrimmovel' column")
  }

  dt <- merge(rq, me, by = "anomesfinaltrimmovel", all.x = TRUE)
  data.table::setorder(dt, anomesfinaltrimmovel)

  # ============================================================================
  # INPC deflation for rhrp* series (hourly wage)
  # ============================================================================
  # SIDRA's rhrp* series are REAL (INPC-deflated to latest period)
  # Our z_rhrp* values are NOMINAL (computed from microdata income/hours)
  # We need to deflate z_rhrp* by INPC to match SIDRA's real values
  # Formula: real = nominal * (latest_inpc / current_inpc)
  # ============================================================================

  rhrp_series <- grep("^z_rhrp", names(dt), value = TRUE)
  if (length(rhrp_series) > 0 && "inpc100dez1993" %in% names(dt)) {
    if (verbose) message("  Deflating ", length(rhrp_series), " rhrp series by INPC...")

    latest_inpc <- dt[anomesfinaltrimmovel == max(anomesfinaltrimmovel), inpc100dez1993][1]

    if (!is.na(latest_inpc) && latest_inpc > 0) {
      for (z_col in rhrp_series) {
        # Deflate: real = nominal * (latest_inpc / current_inpc)
        dt[, (z_col) := get(z_col) * latest_inpc / inpc100dez1993]
      }
    } else {
      warning("INPC values not available, rhrp* series will use nominal values")
    }
  }

  # ============================================================================
  # INPC deflation for real income series (rendhabrealtodos, massahabrealtodos)
  # ============================================================================
  # SIDRA's *real* series are INPC-deflated
  # We need z_ values that match them (deflated nominal values)

  # For rendhabrealtodos etc., we need to deflate the nominal income values
  # These don't have z_ columns directly, but we have z_massahabnominaltodos
  # and z_comrendtodos to compute them

  # Compute real income mass from nominal
  if ("z_massahabnominaltodos" %in% names(dt) && "inpc100dez1993" %in% names(dt)) {
    latest_inpc <- dt[anomesfinaltrimmovel == max(anomesfinaltrimmovel), inpc100dez1993][1]

    if (!is.na(latest_inpc) && latest_inpc > 0) {
      if (verbose) message("  Computing INPC-deflated income series...")

      # Real habitual income mass (INPC)
      dt[, z_massahabrealtodos := z_massahabnominaltodos * latest_inpc / inpc100dez1993]
    }
  }

  if ("z_massaefetnominaltodos" %in% names(dt) && "inpc100dez1993" %in% names(dt)) {
    latest_inpc <- dt[anomesfinaltrimmovel == max(anomesfinaltrimmovel), inpc100dez1993][1]

    if (!is.na(latest_inpc) && latest_inpc > 0) {
      # Real effective income mass (INPC) - use lagged INPC like SIDRA
      inpc_lagged <- data.table::shift(dt$inpc100dez1993, n = 1L, type = "lag")
      dt[, z_massaefetrealtodos := z_massaefetnominaltodos * latest_inpc / inpc_lagged]
    }
  }

  # Update series_names to include newly computed deflated z_ columns
  # (rhrp* and real income series computed above)
  all_z_cols <- grep("^z_", names(dt), value = TRUE)
  series_names <- sub("^z_", "", all_z_cols)

  if (verbose) {
    message("  Total series to process: ", length(series_names))
  }

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
    yyyymm <- dt$anomesfinaltrimmovel

    # Determine series-specific calibration period
    # CNPJ series: use 201601-201912 with cumsum from 201510 (V4019 only available from 201510)
    is_cnpj_series <- use_series_specific_periods && v %in% cnpj_series
    is_split_series <- use_series_specific_periods && v %in% split_series

    # For subocuphoras, use the same post-VD4004_SPLIT parameters as CNPJ (handled below)
    series_cal_start <- if (is_cnpj_series || is_split_series) .PNADC_DATES$CNPJ_CALIB_START else calibration_start
    series_cal_end <- calibration_end
    cumsum_start <- if (is_cnpj_series || is_split_series) .PNADC_DATES$V4019_AVAILABLE else NA_integer_

    # Calculate cumulative variations using shared helper
    # For CNPJ and split series, cumsum should start from 201510
    filter_mask <- if (!is.na(cumsum_start)) (yyyymm >= cumsum_start) else NULL
    cum <- .compute_cumsum_by_mesnotrim(rq_values, mesnotrim, filter_mask = filter_mask)

    # Backprojection error
    e0 <- z_values / scale_factor - cum

    # Filter to calibration period (series-specific)
    in_calibration <- yyyymm >= series_cal_start & yyyymm <= series_cal_end

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

    # ========================================================================
    # SPLIT CALIBRATION for subocuphoras (and similar series)
    # Compute ADDITIONAL starting points for pre-VD4004_SPLIT data
    # (split calibration for pre-VD4004_SPLIT period)
    # ========================================================================
    if (is_split_series) {
      split_month <- .PNADC_DATES$VD4004_SPLIT
      if (verbose) message("    Computing pre-", split_month, " starting points for split calibration...")

      # For pre-split: cumsum only for data <= split_month, calibration period before split
      cum_pre <- .compute_cumsum_by_mesnotrim(rq_values, mesnotrim,
                                              filter_mask = (yyyymm <= split_month))

      # Backprojection for pre-split calibration period
      e0_pre <- z_values / scale_factor - cum_pre

      # Pre-split calibration period: DEFAULT_CALIB_START to PRESPLIT_CALIB_END
      in_calib_pre <- yyyymm >= .PNADC_DATES$DEFAULT_CALIB_START & yyyymm <= .PNADC_DATES$PRESPLIT_CALIB_END

      for (pos in 1:3) {
        mask <- in_calib_pre & mesnotrim == pos
        y0_val <- mean(e0_pre[mask], na.rm = TRUE)

        if (!is.finite(y0_val)) y0_val <- 0

        # Store with "_pre" suffix to distinguish from post-201509 y0
        results[[length(results) + 1]] <- data.table::data.table(
          series_name = paste0(v, "_pre"),
          mesnotrim = pos,
          y0 = y0_val
        )
      }
    }
  }

  result <- data.table::rbindlist(results)

  if (verbose) {
    message("Computed starting points for ", length(unique(result$series_name)), " series")
  }

  result
}


# ==============================================================================
# Starting Points Computation from Microdata
# ==============================================================================

#' Compute z_ Aggregates from Monthly Microdata
#'
#' Computes monthly z_ aggregates from PNADC microdata using calibrated monthly
#' weights, with options for different population scaling approaches.
#'
#' @param calibrated_data PNADC microdata output from \code{pnadc_apply_periods()}
#'   with \code{calibrate = TRUE}. Must include \code{weight_monthly},
#'   \code{ref_month_yyyymm}, and \code{ref_month_in_quarter}.
#' @param verbose Print progress messages.
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{anomesexato}{Integer YYYYMM month}
#'     \item{z_<series>}{Numeric weighted aggregates for each series}
#'   }
#'
#' @details
#' This function creates z_ indicator variables and aggregates them using the
#' calibrated \code{weight_monthly} from \code{pnadc_apply_periods()}.
#'
#' The \code{pnadc_apply_periods()} function implements the calibration
#' methodology as follows:
#' \itemize{
#'   \item Month 2: Scaled to poptrim (quarterly V1028 sum from ALL observations)
#'   \item Months 1,3: Scaled to SIDRA monthly population
#' }
#'
#' This function simply aggregates the indicators using the already-calibrated weights.
#'
#' @examples
#' \dontrun{
#' # Step 1: Build crosswalk
#' crosswalk <- pnadc_identify_periods(stacked_data)
#'
#' # Step 2: Calibrate weights
#' calibrated <- pnadc_apply_periods(stacked_data, crosswalk,
#'                                    weight_var = "V1028",
#'                                    calibration_unit = "month")
#'
#' # Step 3: Compute z_ aggregates using calibrated weights
#' z_agg <- compute_z_aggregates(calibrated)
#'
#' # Step 4: Compute starting points
#' rq <- fetch_sidra_rolling_quarters()
#' y0 <- compute_series_starting_points(z_agg, rq)
#' }
#'
#' @seealso
#' \code{\link{pnadc_apply_periods}} for the calibration step
#' \code{\link{compute_series_starting_points}} for the y0 computation
#'
#' @export
compute_z_aggregates <- function(calibrated_data, verbose = TRUE) {

  # Validate input: must be output from pnadc_apply_periods with calibrate = TRUE
  required_cols <- c("weight_monthly", "ref_month_yyyymm", "ref_month_in_quarter")
  missing_cols <- setdiff(required_cols, names(calibrated_data))
  if (length(missing_cols) > 0) {
    stop("Input must be output from pnadc_apply_periods() with calibrate = TRUE. ",
         "Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Ensure data.table
  if (!inherits(calibrated_data, "data.table")) {
    calibrated_data <- data.table::as.data.table(calibrated_data)
  }
  dt <- data.table::copy(calibrated_data)

  # Filter to observations with valid weights (determined months)
  dt <- dt[!is.na(weight_monthly) & weight_monthly > 0]
  if (nrow(dt) == 0) {
    stop("No observations with valid weight_monthly")
  }

  if (verbose) {
    message("Computing z_ aggregates using calibrated weights")
    message("  Observations: ", format(nrow(dt), big.mark = ","))
  }

  # ============================================================================
  # Step 1: Create anomesexato from ref_month_yyyymm
  # ============================================================================

  dt[, anomesexato := ref_month_yyyymm]

  # ============================================================================
  # Step 2: Create indicator variables
  # ============================================================================

  if (verbose) message("  Creating indicator variables...")

  # Convert character VD variables to integer (PNADC stores them as "01", "02", etc.)
  vd_vars <- c("VD4001", "VD4002", "VD4003", "VD4004", "VD4004A", "VD4005",
               "VD4009", "VD4010", "VD4012", "V4019")
  for (vv in vd_vars) {
    if (vv %in% names(dt) && is.character(dt[[vv]])) {
      data.table::set(dt, j = vv, value = as.integer(dt[[vv]]))
    }
  }

  # Population indicators
  dt[, z_populacao := 1L]
  dt[, z_pop14mais := as.integer(V2009 >= 14)]

  # Labor force status
  if ("VD4001" %in% names(dt)) {
    dt[, z_popforadaforca := as.integer(VD4001 == 2L)]
  }
  if ("VD4002" %in% names(dt)) {
    dt[, z_popocup := as.integer(VD4002 == 1L)]
    dt[, z_popdesocup := as.integer(VD4002 == 2L)]
  }

  # Employment type (VD4009)
  if ("VD4009" %in% names(dt)) {
    dt[, `:=`(
      z_empregprivcomcart = as.integer(VD4009 == 1L),
      z_empregprivsemcart = as.integer(VD4009 == 2L),
      z_domesticocomcart = as.integer(VD4009 == 3L),
      z_domesticosemcart = as.integer(VD4009 == 4L),
      z_empregpublcomcart = as.integer(VD4009 == 5L),
      z_empregpublsemcart = as.integer(VD4009 == 6L),
      z_estatutmilitar = as.integer(VD4009 == 7L),
      z_empregador = as.integer(VD4009 == 8L),
      z_contapropria = as.integer(VD4009 == 9L),
      z_trabfamauxiliar = as.integer(VD4009 == 10L)
    )]

    # CNPJ indicators - use %in% for NA-safe comparison (NA-safe)
    # V4019 only available from ~201510, so these will be 0/FALSE for earlier periods
    if ("V4019" %in% names(dt)) {
      dt[, z_empregadorcomcnpj := as.integer(VD4009 %in% 8L & V4019 %in% 1L)]
      dt[, z_empregadorsemcnpj := as.integer(VD4009 %in% 8L & V4019 %in% 2L)]
      dt[, z_contapropriacomcnpj := as.integer(VD4009 %in% 9L & V4019 %in% 1L)]
      dt[, z_contapropriasemcnpj := as.integer(VD4009 %in% 9L & V4019 %in% 2L)]
    }
  }

  # Sectors (VD4010)
  if ("VD4010" %in% names(dt)) {
    dt[, `:=`(
      z_agropecuaria = as.integer(VD4010 == 1L),
      z_industria = as.integer(VD4010 == 2L),
      z_construcao = as.integer(VD4010 == 3L),
      z_comercio = as.integer(VD4010 == 4L),
      z_transporte = as.integer(VD4010 == 5L),
      z_alojaliment = as.integer(VD4010 == 6L),
      z_infcomfinimobadm = as.integer(VD4010 == 7L),
      z_adminpublica = as.integer(VD4010 %in% c(8L, 9L)),
      z_outroservico = as.integer(VD4010 == 10L),
      z_servicodomestico = as.integer(VD4010 == 11L)
    )]
  }

  # Other indicators
  if ("VD4012" %in% names(dt)) {
    dt[, z_contribuinteprev := as.integer(VD4012 == 1L)]
  }
  if ("VD4003" %in% names(dt)) {
    dt[, z_forcapotencial := as.integer(VD4003 == 1L)]
  }
  if ("VD4005" %in% names(dt)) {
    dt[, z_desalentado := as.integer(VD4005 == 1L)]
  }

  # Suboccupation (variable changed name over time: VD4004 -> VD4004A at 201510)
  # VD4004 used for anomesexato <= VD4004_SPLIT, VD4004A otherwise
  if ("VD4004A" %in% names(dt) || "VD4004" %in% names(dt)) {
    split_month <- .PNADC_DATES$VD4004_SPLIT
    dt[, z_subocuphoras := 0L]
    if ("VD4004A" %in% names(dt)) {
      dt[anomesexato > split_month, z_subocuphoras := as.integer(VD4004A == 1L)]
    }
    if ("VD4004" %in% names(dt)) {
      dt[anomesexato <= split_month, z_subocuphoras := as.integer(VD4004 == 1L)]
    }
  }

  # ==========================================================================
  # AGGREGATE SERIES (sums of subcategories)
  # These can be computed directly from microdata rather than as post-hoc sums
  # ==========================================================================

  # Labor force (popnaforca = popocup + popdesocup)
  if ("VD4002" %in% names(dt)) {
    dt[, z_popnaforca := as.integer(VD4002 %in% c(1L, 2L))]
  }

  # Aggregate employment types
  if ("VD4009" %in% names(dt)) {
    dt[, z_empregpriv := as.integer(VD4009 %in% c(1L, 2L))]           # Private employees
    dt[, z_domestico := as.integer(VD4009 %in% c(3L, 4L))]            # Domestic workers
    dt[, z_empregpubl := as.integer(VD4009 %in% c(5L, 6L, 7L))]       # Public sector
    dt[, z_empregado := as.integer(VD4009 %in% 1:7)]                   # All employees
  }

  # Extended labor force (forcaampliada = popnaforca + forcapotencial)
  if ("VD4002" %in% names(dt) && "VD4003" %in% names(dt)) {
    dt[, z_forcaampliada := as.integer(VD4002 %in% c(1L, 2L) | VD4003 == 1L)]
  }

  # ==========================================================================
  # INCOME SERIES
  # These are income VALUES (not indicators) - aggregated as sum(value * weight)
  # ==========================================================================

  # Habitual income mass from all jobs (VD4019)
  if ("VD4019" %in% names(dt)) {
    # Convert to numeric if needed
    if (is.character(dt$VD4019)) {
      dt[, VD4019 := as.numeric(VD4019)]
    }
    # z_massahabnominaltodos is the income VALUE itself
    # When aggregated: sum(VD4019 * weight) = total income mass
    dt[, z_massahabnominaltodos := fifelse(is.na(VD4019), 0, VD4019)]

    # Count of people with income (comrendtodos)
    dt[, z_comrendtodos := as.integer(!is.na(VD4019) & VD4019 > 0)]
  }

  # Effective income mass from all jobs (VD4020)
  if ("VD4020" %in% names(dt)) {
    if (is.character(dt$VD4020)) {
      dt[, VD4020 := as.numeric(VD4020)]
    }
    dt[, z_massaefetnominaltodos := fifelse(is.na(VD4020), 0, VD4020)]
  }

  # ==========================================================================
  # HOURS WORKED AND INCOME BY CATEGORY (for rhrp* hourly wage series)
  # ==========================================================================
  # rhrp = Rendimento habitual real por hora efetivamente trabalhada
  # Formula: total_habitual_income / total_effective_hours (then INPC deflated)
  # We need to mensalize numerator and denominator separately, then compute ratio
  #
  # Variables:
  # - VD4016: Habitual income from main job (numerator)
  # - VD4035: Effective hours worked per week - all jobs (denominator)
  # ==========================================================================

  # Convert hours and income variables to numeric
  if ("VD4016" %in% names(dt)) {
    if (is.character(dt$VD4016)) dt[, VD4016 := as.numeric(VD4016)]
    dt[, VD4016 := fifelse(is.na(VD4016), 0, VD4016)]
  }
  if ("VD4017" %in% names(dt)) {
    if (is.character(dt$VD4017)) dt[, VD4017 := as.numeric(VD4017)]
    dt[, VD4017 := fifelse(is.na(VD4017), 0, VD4017)]
  }
  if ("VD4031" %in% names(dt)) {
    if (is.character(dt$VD4031)) dt[, VD4031 := as.numeric(VD4031)]
    dt[, VD4031 := fifelse(is.na(VD4031), 0, VD4031)]
  }
  if ("VD4035" %in% names(dt)) {
    if (is.character(dt$VD4035)) dt[, VD4035 := as.numeric(VD4035)]
    dt[, VD4035 := fifelse(is.na(VD4035), 0, VD4035)]
  }

  # Income and hours by employment type (VD4009) - for rhrp by position
  if ("VD4009" %in% names(dt) && "VD4016" %in% names(dt) && "VD4035" %in% names(dt)) {
    if (verbose) message("  Creating income/hours aggregates by employment type...")

    # Total income from main job (VD4016) by employment type
    dt[, z_income_hab_empregado := VD4016 * as.integer(VD4009 %in% 1:7)]
    dt[, z_income_hab_empregpriv := VD4016 * as.integer(VD4009 %in% c(1L, 2L))]
    dt[, z_income_hab_empregprivcomcart := VD4016 * as.integer(VD4009 == 1L)]
    dt[, z_income_hab_empregprivsemcart := VD4016 * as.integer(VD4009 == 2L)]
    dt[, z_income_hab_domestico := VD4016 * as.integer(VD4009 %in% c(3L, 4L))]
    dt[, z_income_hab_domesticocomcart := VD4016 * as.integer(VD4009 == 3L)]
    dt[, z_income_hab_domesticosemcart := VD4016 * as.integer(VD4009 == 4L)]
    dt[, z_income_hab_empregpubl := VD4016 * as.integer(VD4009 %in% c(5L, 6L, 7L))]
    dt[, z_income_hab_empregpublcomcart := VD4016 * as.integer(VD4009 == 5L)]
    dt[, z_income_hab_empregpublsemcart := VD4016 * as.integer(VD4009 == 6L)]
    dt[, z_income_hab_estatutmilitar := VD4016 * as.integer(VD4009 == 7L)]
    dt[, z_income_hab_empregador := VD4016 * as.integer(VD4009 == 8L)]
    dt[, z_income_hab_contapropria := VD4016 * as.integer(VD4009 == 9L)]

    # CNPJ variants (only available from 201510)
    if ("V4019" %in% names(dt)) {
      dt[, z_income_hab_empregadorcomcnpj := VD4016 * as.integer(VD4009 %in% 8L & V4019 %in% 1L)]
      dt[, z_income_hab_empregadorsemcnpj := VD4016 * as.integer(VD4009 %in% 8L & V4019 %in% 2L)]
      dt[, z_income_hab_contapropriacomcnpj := VD4016 * as.integer(VD4009 %in% 9L & V4019 %in% 1L)]
      dt[, z_income_hab_contapropriasemcnpj := VD4016 * as.integer(VD4009 %in% 9L & V4019 %in% 2L)]
    }

    # Effective hours worked (VD4035) by employment type
    dt[, z_hours_efet_empregado := VD4035 * as.integer(VD4009 %in% 1:7)]
    dt[, z_hours_efet_empregpriv := VD4035 * as.integer(VD4009 %in% c(1L, 2L))]
    dt[, z_hours_efet_empregprivcomcart := VD4035 * as.integer(VD4009 == 1L)]
    dt[, z_hours_efet_empregprivsemcart := VD4035 * as.integer(VD4009 == 2L)]
    dt[, z_hours_efet_domestico := VD4035 * as.integer(VD4009 %in% c(3L, 4L))]
    dt[, z_hours_efet_domesticocomcart := VD4035 * as.integer(VD4009 == 3L)]
    dt[, z_hours_efet_domesticosemcart := VD4035 * as.integer(VD4009 == 4L)]
    dt[, z_hours_efet_empregpubl := VD4035 * as.integer(VD4009 %in% c(5L, 6L, 7L))]
    dt[, z_hours_efet_empregpublcomcart := VD4035 * as.integer(VD4009 == 5L)]
    dt[, z_hours_efet_empregpublsemcart := VD4035 * as.integer(VD4009 == 6L)]
    dt[, z_hours_efet_estatutmilitar := VD4035 * as.integer(VD4009 == 7L)]
    dt[, z_hours_efet_empregador := VD4035 * as.integer(VD4009 == 8L)]
    dt[, z_hours_efet_contapropria := VD4035 * as.integer(VD4009 == 9L)]

    if ("V4019" %in% names(dt)) {
      dt[, z_hours_efet_empregadorcomcnpj := VD4035 * as.integer(VD4009 %in% 8L & V4019 %in% 1L)]
      dt[, z_hours_efet_empregadorsemcnpj := VD4035 * as.integer(VD4009 %in% 8L & V4019 %in% 2L)]
      dt[, z_hours_efet_contapropriacomcnpj := VD4035 * as.integer(VD4009 %in% 9L & V4019 %in% 1L)]
      dt[, z_hours_efet_contapropriasemcnpj := VD4035 * as.integer(VD4009 %in% 9L & V4019 %in% 2L)]
    }
  }

  # Income and hours by sector (VD4010) - for rhrp by sector
  if ("VD4010" %in% names(dt) && "VD4016" %in% names(dt) && "VD4035" %in% names(dt)) {
    if (verbose) message("  Creating income/hours aggregates by sector...")

    # Income by sector
    dt[, z_income_hab_agropecuaria := VD4016 * as.integer(VD4010 == 1L)]
    dt[, z_income_hab_industria := VD4016 * as.integer(VD4010 == 2L)]
    dt[, z_income_hab_construcao := VD4016 * as.integer(VD4010 == 3L)]
    dt[, z_income_hab_comercio := VD4016 * as.integer(VD4010 == 4L)]
    dt[, z_income_hab_transporte := VD4016 * as.integer(VD4010 == 5L)]
    dt[, z_income_hab_alojaliment := VD4016 * as.integer(VD4010 == 6L)]
    dt[, z_income_hab_infcomfinimobadm := VD4016 * as.integer(VD4010 == 7L)]
    dt[, z_income_hab_adminpublica := VD4016 * as.integer(VD4010 %in% c(8L, 9L))]
    dt[, z_income_hab_outroservico := VD4016 * as.integer(VD4010 == 10L)]
    dt[, z_income_hab_servicodomestico := VD4016 * as.integer(VD4010 == 11L)]

    # Hours by sector
    dt[, z_hours_efet_agropecuaria := VD4035 * as.integer(VD4010 == 1L)]
    dt[, z_hours_efet_industria := VD4035 * as.integer(VD4010 == 2L)]
    dt[, z_hours_efet_construcao := VD4035 * as.integer(VD4010 == 3L)]
    dt[, z_hours_efet_comercio := VD4035 * as.integer(VD4010 == 4L)]
    dt[, z_hours_efet_transporte := VD4035 * as.integer(VD4010 == 5L)]
    dt[, z_hours_efet_alojaliment := VD4035 * as.integer(VD4010 == 6L)]
    dt[, z_hours_efet_infcomfinimobadm := VD4035 * as.integer(VD4010 == 7L)]
    dt[, z_hours_efet_adminpublica := VD4035 * as.integer(VD4010 %in% c(8L, 9L))]
    dt[, z_hours_efet_outroservico := VD4035 * as.integer(VD4010 == 10L)]
    dt[, z_hours_efet_servicodomestico := VD4035 * as.integer(VD4010 == 11L)]
  }

  # ============================================================================
  # Step 3: Aggregate by month using weight_monthly
  # ============================================================================

  if (verbose) message("  Aggregating by month...")

  # Get z_ columns
  z_cols <- grep("^z_", names(dt), value = TRUE)

  if (length(z_cols) == 0) {
    stop("No z_ indicator columns created. Check variable availability.")
  }

  # Aggregate: sum(indicator * weight_monthly) by month
  result <- dt[, lapply(.SD, function(x) sum(x * weight_monthly, na.rm = TRUE)),
               by = anomesexato, .SDcols = z_cols]

  data.table::setorder(result, anomesexato)

  # ============================================================================
  # Step 4: Scale income series to match SIDRA units
  # ============================================================================
  # SIDRA reports income mass in MILLIONS of reais, but our z_ aggregation
  # produces raw reais. We scale by 1/1000 here so that the standard
  # scale_factor = 1000 in compute_series_starting_points() works correctly.
  #
  # After this scaling:
  # - z_populacao is in raw (SIDRA in thousands, so z/1000 = SIDRA)
  # - z_massahabnominaltodos is in thousands (SIDRA in millions, so z/1000 = SIDRA)
  # Both use scale_factor = 1000 for y0 computation.

  if (verbose) message("  Scaling income series to SIDRA units...")

  # Scale income mass series (raw reais -> thousands, so /1000 gives SIDRA millions)
  income_mass_series <- c("z_massahabnominaltodos", "z_massaefetnominaltodos")
  for (col in income_mass_series) {
    if (col %in% names(result)) {
      result[, (col) := get(col) / 1000]
    }
  }

  # Scale income by category series (for rhrp computation)
  # These are also in raw reais, need same scaling
  income_by_cat_series <- grep("^z_income_hab_", names(result), value = TRUE)
  for (col in income_by_cat_series) {
    result[, (col) := get(col) / 1000]
  }

  # Note: z_hours_efet_* series stay in raw hours (no scaling needed)

  # ============================================================================
  # Step 5: Compute rhrp (hourly wage) series from income/hours
  # ============================================================================
  # rhrp = rendimento habitual por hora efetivamente trabalhada
  # Formula: rhrp = (total_income / total_hours) * conversion_factor
  # Conversion: income is monthly, hours are weekly -> divide by 4.33 (weeks/month)
  # The z_income_hab_* are in thousands (scaled above), z_hours_efet_* are raw
  # Result: rhrp in reais per hour
  #
  # Note: These z_rhrp_* values are NOMINAL. SIDRA's rhrp series are REAL
  # (INPC-deflated). The y0 computation will need to deflate by INPC.
  # ============================================================================

  if (verbose) message("  Computing hourly wage (rhrp) series...")

  # Helper to compute rhrp safely (avoid division by zero)
  compute_rhrp <- function(income_col, hours_col) {
    if (income_col %in% names(result) && hours_col %in% names(result)) {
      income <- result[[income_col]]
      hours <- result[[hours_col]]
      # Convert: income (thousands/month) / hours (hours/week) / 4.33 (weeks/month)
      # The 1000 factor converts back from thousands
      rhrp <- fifelse(hours > 0, income * 1000 / (hours * 4.33), NA_real_)
      return(rhrp)
    }
    return(NULL)
  }

  # rhrp by employment type
  employment_types <- c("empregado", "empregpriv", "empregprivcomcart", "empregprivsemcart",
                        "domestico", "domesticocomcart", "domesticosemcart",
                        "empregpubl", "empregpublcomcart", "empregpublsemcart", "estatutmilitar",
                        "empregador", "contapropria")

  for (et in employment_types) {
    income_col <- paste0("z_income_hab_", et)
    hours_col <- paste0("z_hours_efet_", et)
    rhrp <- compute_rhrp(income_col, hours_col)
    if (!is.null(rhrp)) {
      result[, paste0("z_rhrp", et) := rhrp]
    }
  }

  # rhrp by employment type (CNPJ variants)
  cnpj_types <- c("empregadorcomcnpj", "empregadorsemcnpj",
                  "contapropriacomcnpj", "contapropriasemcnpj")

  for (et in cnpj_types) {
    income_col <- paste0("z_income_hab_", et)
    hours_col <- paste0("z_hours_efet_", et)
    rhrp <- compute_rhrp(income_col, hours_col)
    if (!is.null(rhrp)) {
      result[, paste0("z_rhrp", et) := rhrp]
    }
  }

  # rhrp by sector
  sectors <- c("agropecuaria", "industria", "construcao", "comercio", "transporte",
               "alojaliment", "infcomfinimobadm", "adminpublica", "outroservico",
               "servicodomestico")

  for (sec in sectors) {
    income_col <- paste0("z_income_hab_", sec)
    hours_col <- paste0("z_hours_efet_", sec)
    rhrp <- compute_rhrp(income_col, hours_col)
    if (!is.null(rhrp)) {
      result[, paste0("z_rhrp", sec) := rhrp]
    }
  }

  # ============================================================================
  # NOTE: Derived series (rates, per-capita income) are NOT computed here
  # ============================================================================
  # Rate series (taxadesocup, taxapartic, etc.) should NOT have starting points.
  # Methodology: mensalize COMPONENT series (popdesocup, popnaforca),
  # then DERIVE rates: m_taxadesocup = m_popdesocup / m_popnaforca * 100
  #
  # Per-capita income series (rendhabnominaltodos, rendefetnominaltodos) are
  # available directly from SIDRA and are mensalized with their own y0 values.
  # They are NOT computed as z_massa / z_comrend here.

  # Count final z_ columns
  z_final <- grep("^z_", names(result), value = TRUE)

  if (verbose) {
    message("  Created ", length(z_final), " z_ series for ",
            nrow(result), " months (",
            min(result$anomesexato), " to ", max(result$anomesexato), ")")
  }

  result
}


#' Compute Starting Points from Raw PNADC Microdata
#'
#' Complete workflow to compute y0 starting points from raw PNADC microdata.
#' This is a convenience wrapper that combines period identification, weight
#' calibration, z_ aggregation, and starting point computation.
#'
#' @param data Stacked PNADC microdata (multiple quarters). Must contain variables
#'   for period identification (see \code{\link{pnadc_identify_periods}}).
#' @param calibration_start Integer. Start of calibration period (YYYYMM).
#'   Default NULL uses .PNADC_DATES$DEFAULT_CALIB_START (201301).
#' @param calibration_end Integer. End of calibration period (YYYYMM).
#'   Default NULL uses .PNADC_DATES$DEFAULT_CALIB_END (201912).
#' @param verbose Print progress messages.
#'
#' @return data.table with columns:
#'   \describe{
#'     \item{series_name}{Character. Series name}
#'     \item{mesnotrim}{Integer. Month position (1, 2, or 3)}
#'     \item{y0}{Numeric. Starting point value}
#'   }
#'
#' @details
#' This function performs the complete workflow:
#' \enumerate{
#'   \item Build crosswalk via \code{pnadc_identify_periods()}
#'   \item Calibrate weights via \code{pnadc_apply_periods()}
#'   \item Compute z_ aggregates via \code{compute_z_aggregates()}
#'   \item Fetch SIDRA rolling quarters
#'   \item Compute starting points via \code{compute_series_starting_points()}
#' }
#'
#' @section Weight Calibration:
#' Weights are calibrated as follows:
#' \itemize{
#'   \item Month 2: Scaled to poptrim (quarterly V1028 sum from ALL observations)
#'   \item Months 1,3: Scaled to SIDRA monthly population
#' }
#'
#' @examples
#' \dontrun{
#' # Load stacked PNADC data
#' stacked <- fst::read_fst("pnadc_stacked.fst", as.data.table = TRUE)
#'
#' # Compute starting points
#' y0 <- compute_starting_points_from_microdata(stacked)
#'
#' # Compare with bundled values
#' bundled <- pnadc_series_starting_points
#' comparison <- merge(y0, bundled, by = c("series_name", "mesnotrim"))
#' }
#'
#' @seealso
#' \code{\link{pnadc_apply_periods}} for the weight calibration step
#' \code{\link{compute_z_aggregates}} for the z_ aggregation step
#' \code{\link{compute_series_starting_points}} for the y0 computation
#' \code{\link{pnadc_identify_periods}} for period identification
#'
#' @export
compute_starting_points_from_microdata <- function(data,
                                                    calibration_start = NULL,
                                                    calibration_end = NULL,
                                                    verbose = TRUE) {

  # Resolve defaults from centralized constants
  if (is.null(calibration_start)) {
    calibration_start <- .PNADC_DATES$DEFAULT_CALIB_START
  }
  if (is.null(calibration_end)) {
    calibration_end <- .PNADC_DATES$DEFAULT_CALIB_END
  }

  if (verbose) {
    message("\n", paste(rep("=", 60), collapse = ""))
    message("Computing Starting Points from Microdata")
    message(paste(rep("=", 60), collapse = ""))
    message("Calibration period: ", calibration_start, " to ", calibration_end)
    message("")
  }

  # Step 1: Build crosswalk
  if (verbose) message("Step 1: Building crosswalk...")
  crosswalk <- pnadc_identify_periods(data, verbose = verbose)

  # Step 2: Calibrate weights (month 2 -> poptrim, months 1,3 -> SIDRA)
  if (verbose) message("\nStep 2: Calibrating weights...")
  calibrated <- pnadc_apply_periods(
    data = data,
    crosswalk = crosswalk,
    weight_var = "V1028",
    calibration_unit = "month",
    verbose = verbose
  )

  # Step 3: Compute z_ aggregates using calibrated weights
  if (verbose) message("\nStep 3: Computing z_ aggregates...")
  z_aggregates <- compute_z_aggregates(calibrated, verbose = verbose)

  # Step 4: Fetch SIDRA rolling quarters
  # exclude_derived=TRUE skips rate series (they're computed from population ratios)
  if (verbose) message("\nStep 4: Fetching SIDRA rolling quarters...")
  rolling_quarters <- fetch_sidra_rolling_quarters(
    verbose = verbose,
    use_cache = TRUE,
    exclude_derived = TRUE
  )

  # Step 5: Compute starting points
  if (verbose) message("\nStep 5: Computing starting points...")
  y0 <- compute_series_starting_points(
    monthly_estimates = z_aggregates,
    rolling_quarters = rolling_quarters,
    calibration_start = calibration_start,
    calibration_end = calibration_end,
    verbose = verbose
  )

  if (verbose) {
    message("\n", paste(rep("=", 60), collapse = ""))
    message("Complete: ", length(unique(y0$series_name)), " series x 3 positions = ",
            nrow(y0), " starting points")
    message(paste(rep("=", 60), collapse = ""))
  }

  y0
}
