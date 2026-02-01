#' Fetch SIDRA Rolling Quarter Series
#'
#' Functions to download PNADC labor market indicators from IBGE's SIDRA API.
#' These series are published as rolling quarterly averages (trimestre movel).
#'
#' @name fetch-sidra-series
#' @keywords internal
NULL

# ============================================================================
# CACHING INFRASTRUCTURE FOR SIDRA SERIES
# ============================================================================

# Package-level cache environment for SIDRA series data
# Separate from population cache to allow independent management
.sidra_series_cache <- new.env(parent = emptyenv())

#' Clear All SIDRA Caches
#'
#' Clears all cached SIDRA data (both rolling quarter series and population).
#' Use this if you need to force a fresh download, for example after IBGE
#' updates their data.
#'
#' @return Invisibly returns TRUE if any cache was cleared, FALSE if all empty.
#'
#' @examples
#' \dontrun{
#' clear_sidra_cache()
#' series <- fetch_sidra_rolling_quarters()  # Will fetch fresh from API
#' pop <- fetch_monthly_population()          # Will also fetch fresh
#' }
#'
#' @export
clear_sidra_cache <- function() {
  had_series_cache <- FALSE
  had_pop_cache <- FALSE

  # Clear series cache
  cache_names <- ls(envir = .sidra_series_cache)
  if (length(cache_names) > 0) {
    rm(list = cache_names, envir = .sidra_series_cache)
    had_series_cache <- TRUE
  }

  # Clear population cache
  pop_cache_names <- ls(envir = .sidra_population_cache)
  if (length(pop_cache_names) > 0) {
    rm(list = pop_cache_names, envir = .sidra_population_cache)
    had_pop_cache <- TRUE
  }

  invisible(had_series_cache || had_pop_cache)
}

#' Check if SIDRA Series Cache Exists
#'
#' @param cache_name Name of the cached object to check
#' @return Logical. TRUE if cache exists, FALSE otherwise.
#' @keywords internal
#' @noRd
.is_series_cache_valid <- function(cache_name = "rolling_quarters") {
  data_name <- paste0(cache_name, "_data")
  time_name <- paste0(cache_name, "_time")

  exists(data_name, envir = .sidra_series_cache) &&
    exists(time_name, envir = .sidra_series_cache)
}

#' Get Cache Timestamp
#'
#' @param cache_name Name of the cached object
#' @return POSIXct timestamp or NULL if not cached.
#' @keywords internal
#' @noRd
.get_cache_time <- function(cache_name = "rolling_quarters") {
  time_name <- paste0(cache_name, "_time")
  if (exists(time_name, envir = .sidra_series_cache)) {
    get(time_name, envir = .sidra_series_cache)
  } else {
    NULL
  }
}


#' Fetch Rolling Quarter Series from SIDRA
#'
#' Downloads PNADC labor market indicators from IBGE's SIDRA API. These series
#' are published as rolling quarterly averages (trimestre movel), with 12
#' observations per year.
#'
#' @param series Character vector of series names to fetch, or "all" (default)
#'   for all available series. Use \code{get_sidra_series_metadata()$series_name}
#'   to see available names.
#' @param category Character vector of categories to filter by. Valid options:
#'   "rate", "population", "employment", "sector", "income_nominal",
#'   "income_real", "underutilization", "price_index". Use NULL for no filter.
#' @param use_cache Logical. Use cached data if available? Default FALSE.
#'   When TRUE, shows the date when data was cached (may be outdated).
#'   Use \code{\link{clear_sidra_cache}} to force fresh download.
#' @param verbose Logical. Print progress messages? Default TRUE.
#' @param retry_failed Logical. Retry failed series downloads? Default TRUE.
#' @param max_retries Integer. Maximum retry attempts per series. Default 3.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{anomesfinaltrimmovel}{Integer. YYYYMM of rolling quarter end month}
#'     \item{mesnotrim}{Integer. Month position in quarter (1, 2, or 3)}
#'     \item{<series_name>}{Numeric. One column per requested series}
#'   }
#'
#' @details
#' Rolling quarters are labeled by their ending month:
#' \itemize{
#'   \item 201201 = Nov 2011 - Jan 2012 (mesnotrim = 1)
#'   \item 201202 = Dec 2011 - Feb 2012 (mesnotrim = 2)
#'   \item 201203 = Jan - Mar 2012 (mesnotrim = 3)
#'   \item 201204 = Feb - Apr 2012 (mesnotrim = 1)
#'   \item etc.
#' }
#'
#' The \code{mesnotrim} column indicates the month's position within its rolling
#' quarter, which is essential for the mensalization algorithm.
#'
#' @section Rate Limiting:
#' SIDRA API may have rate limits. The function includes automatic retry logic
#' with exponential backoff for failed requests.
#'
#' @examples
#' \dontrun{
#' # Fetch all series (may take several minutes on first call)
#' rq <- fetch_sidra_rolling_quarters()
#'
#' # Fetch only population series (faster)
#' rq_pop <- fetch_sidra_rolling_quarters(category = "population")
#'
#' # Fetch specific series
#' rq <- fetch_sidra_rolling_quarters(
#'   series = c("taxadesocup", "popocup", "popdesocup")
#' )
#' }
#'
#' @seealso
#' \code{\link{get_sidra_series_metadata}} for available series names and metadata
#' \code{\link{get_sidra_series_metadata}} for series details
#' \code{\link{mensalize_sidra_series}} to convert to exact months
#'
#' @export
fetch_sidra_rolling_quarters <- function(series = "all",
                                          category = NULL,
                                          use_cache = FALSE,
                                          verbose = TRUE,
                                          retry_failed = TRUE,
                                          max_retries = 3) {

  # Get metadata for requested series
  meta <- get_sidra_series_metadata(series = series, category = category)

  if (nrow(meta) == 0) {
    stop("No series found matching the specified criteria")
  }

  series_names <- meta$series_name

  # Check cache for complete dataset
  if (use_cache && .is_series_cache_valid("rolling_quarters")) {
    cached_data <- get("rolling_quarters_data", envir = .sidra_series_cache)

    # Check if cached data contains all requested series
    cached_series <- setdiff(names(cached_data),
                              c("anomesfinaltrimmovel", "mesnotrim"))
    if (all(series_names %in% cached_series)) {
      # Get cache timestamp
      cache_time <- .get_cache_time("rolling_quarters")
      cache_date <- format(cache_time, "%Y-%m-%d %H:%M")

      if (verbose) {
        message("Using cached data from ", cache_date)
        message("  (Data may be outdated. Use use_cache=FALSE or clear_sidra_cache() to refresh)")
      }

      # Return only requested columns
      cols_to_keep <- c("anomesfinaltrimmovel", "mesnotrim", series_names)
      result <- data.table::copy(cached_data[, ..cols_to_keep])

      if (verbose) {
        message("  ", length(series_names), " series, ",
                nrow(result), " rolling quarters (",
                min(result$anomesfinaltrimmovel), " to ",
                max(result$anomesfinaltrimmovel), ")")
      }
      return(result)
    }
  }

  # Check for sidrar package
  if (!requireNamespace("sidrar", quietly = TRUE)) {
    stop(
      "Package 'sidrar' is required for fetching series from SIDRA.\n",
      "Install with: install.packages('sidrar')",
      call. = FALSE
    )
  }

  if (verbose) {
    message("Fetching ", nrow(meta), " series from SIDRA API...")
    message("This may take a few minutes on first run.")
  }

  # Initialize result with time index
  result <- NULL
  failed_series <- character(0)

  # Progress tracking
  n_series <- nrow(meta)

  for (i in seq_len(n_series)) {
    series_name <- meta$series_name[i]
    api_path <- meta$api_path[i]

    if (verbose) {
      pct <- round(100 * i / n_series)
      message(sprintf("  [%3d%%] Fetching %s...", pct, series_name))
    }

    # Fetch with retry logic
    series_data <- NULL
    for (attempt in seq_len(max_retries)) {
      series_data <- tryCatch({
        suppressMessages(sidrar::get_sidra(api = api_path))
      }, error = function(e) {
        if (attempt < max_retries && retry_failed) {
          # Exponential backoff
          Sys.sleep(2^attempt)
          NULL
        } else {
          warning("Failed to fetch ", series_name, ": ", conditionMessage(e))
          NULL
        }
      })

      if (!is.null(series_data)) break
    }

    if (is.null(series_data)) {
      failed_series <- c(failed_series, series_name)
      next
    }

    # Process the response
    dt <- .process_sidra_response(series_data, series_name)

    if (is.null(dt)) {
      failed_series <- c(failed_series, series_name)
      next
    }

    # Merge with result
    if (is.null(result)) {
      result <- dt
    } else {
      result <- merge(result, dt, by = "anomesfinaltrimmovel", all = TRUE)
    }
  }

  if (is.null(result)) {
    stop("Failed to fetch any series from SIDRA")
  }

  # Add mesnotrim column (month position in quarter)
  result[, mesnotrim := .get_mesnotrim(anomesfinaltrimmovel %% 100L)]

  # Reorder columns
  data.table::setcolorder(result, c("anomesfinaltrimmovel", "mesnotrim"))

  # Sort by time
  data.table::setorder(result, anomesfinaltrimmovel)

  # Report failures
  if (length(failed_series) > 0) {
    warning("Failed to fetch ", length(failed_series), " series: ",
            paste(failed_series, collapse = ", "))
  }

  # Always cache the result after fetching (use_cache only controls reading)
  assign("rolling_quarters_data", data.table::copy(result),
         envir = .sidra_series_cache)
  assign("rolling_quarters_time", Sys.time(),
         envir = .sidra_series_cache)

  if (verbose) {
    n_success <- length(series_names) - length(failed_series)
    message("Successfully fetched ", n_success, "/", length(series_names),
            " series")
    message("  Rolling quarters: ", min(result$anomesfinaltrimmovel),
            " to ", max(result$anomesfinaltrimmovel),
            " (", nrow(result), " observations)")
  }

  result
}


#' Process SIDRA API Response
#'
#' Internal function to extract the series values from a SIDRA API response.
#'
#' @param raw Raw data.frame from sidrar::get_sidra()
#' @param series_name Name to give the value column
#' @return data.table with anomesfinaltrimmovel and series value columns
#' @keywords internal
#' @noRd
.process_sidra_response <- function(raw, series_name) {

  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  dt <- data.table::as.data.table(raw)

  # Find the period code column (handles various SIDRA naming patterns)
  # For PNADC tables, this is typically "Trimestre Movel (Codigo)"
  code_col <- NULL

  # Try various patterns
  patterns <- c(
    "Trimestre.*M.*vel.*C.*digo",
    "trimestre.*m.*vel.*c.*digo",
    "M.*vel.*C.*digo",
    "Trimestre.*C.*digo"
  )

  for (pattern in patterns) {
    matches <- grep(pattern, names(dt), value = TRUE, ignore.case = TRUE)
    if (length(matches) > 0) {
      code_col <- matches[1]
      break
    }
  }

  # For price indices, the period might be different
  if (is.null(code_col)) {
    # Try "Mes (Codigo)" for monthly series like IPCA
    matches <- grep("M.*s.*C.*digo|Month.*Code", names(dt),
                    value = TRUE, ignore.case = TRUE)
    if (length(matches) > 0) {
      code_col <- matches[1]
    }
  }

  if (is.null(code_col)) {
    warning("Could not find period code column for series: ", series_name)
    return(NULL)
  }

  # Extract values
  result <- dt[, .(
    anomesfinaltrimmovel = as.integer(get(code_col)),
    value = as.numeric(Valor)
  )]

  # Remove NA codes
  result <- result[!is.na(anomesfinaltrimmovel)]

  # Rename value column to series name
  data.table::setnames(result, "value", series_name)

  result
}


#' Fetch IPCA Index for Deflation
#'
#' Fetches the IPCA price index from SIDRA for deflating nominal income series.
#' This is a convenience function that extracts just the IPCA series.
#'
#' @param use_cache Logical. Use cached data? Default TRUE.
#' @param verbose Logical. Print progress? Default TRUE.
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{anomesexato}{Integer. YYYYMM exact month}
#'     \item{ipca_index}{Numeric. IPCA index (base Dec 1993 = 100)}
#'   }
#'
#' @keywords internal
#' @noRd
.fetch_ipca_index <- function(use_cache = TRUE, verbose = TRUE) {

  # Check cache
  if (use_cache && .is_series_cache_valid("ipca", 24)) {
    return(data.table::copy(get("ipca_data", envir = .sidra_series_cache)))
  }

  if (!requireNamespace("sidrar", quietly = TRUE)) {
    stop("Package 'sidrar' required. Install with: install.packages('sidrar')")
  }

  if (verbose) message("Fetching IPCA index from SIDRA...")

  raw <- tryCatch({
    suppressMessages(sidrar::get_sidra(
      api = "/t/1737/n1/all/v/2266/p/all/d/v2266%2013"
    ))
  }, error = function(e) {
    stop("Failed to fetch IPCA: ", conditionMessage(e))
  })

  dt <- data.table::as.data.table(raw)

  # Find month code column
  code_col <- grep("M.*s.*C.*digo", names(dt), value = TRUE, ignore.case = TRUE)
  if (length(code_col) == 0) {
    stop("Could not find month code column in IPCA response")
  }

  result <- dt[, .(
    anomesexato = as.integer(get(code_col[1])),
    ipca_index = as.numeric(Valor)
  )]

  result <- result[!is.na(anomesexato)]
  data.table::setorder(result, anomesexato)

  # Cache
  if (use_cache) {
    assign("ipca_data", data.table::copy(result), envir = .sidra_series_cache)
    assign("ipca_time", Sys.time(), envir = .sidra_series_cache)
  }

  result
}
