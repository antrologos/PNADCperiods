#' Fetch SIDRA Rolling Quarter Series
#'
#' Functions to download PNADC labor market indicators from IBGE's SIDRA API.
#' These series are published as rolling quarterly averages (trimestre movel).
#'
#' @name fetch-sidra-series
#' @keywords internal
NULL

# ============================================================================
# UNIFIED CACHING INFRASTRUCTURE FOR SIDRA DATA
# ============================================================================
#
# All SIDRA-fetched data (rolling quarters, population) is cached in a single
# environment with namespaced keys:
#   - rolling_quarters_data, rolling_quarters_time
#   - population_data, population_time
#
# This simplifies cache management and ensures consistent behavior.
# ============================================================================

# Unified package-level cache environment for ALL SIDRA data
.sidra_cache <- new.env(parent = emptyenv())

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
  cache_names <- ls(envir = .sidra_cache)
  had_cache <- length(cache_names) > 0

  if (had_cache) {
    rm(list = cache_names, envir = .sidra_cache)
  }

  invisible(had_cache)
}

#' Check if SIDRA Cache Entry is Valid
#'
#' @param cache_name Name of the cached object (e.g., "rolling_quarters", "population")
#' @param max_age_hours Maximum cache age in hours. NULL for no expiration (default).
#' @return Logical. TRUE if cache exists and is not expired, FALSE otherwise.
#' @keywords internal
#' @noRd
.is_cache_valid <- function(cache_name, max_age_hours = NULL) {
  data_name <- paste0(cache_name, "_data")
  time_name <- paste0(cache_name, "_time")

  if (!exists(data_name, envir = .sidra_cache) ||
      !exists(time_name, envir = .sidra_cache)) {
    return(FALSE)
  }

  # Check expiration if max_age_hours is specified

if (!is.null(max_age_hours)) {
    cache_time <- get(time_name, envir = .sidra_cache)
    age_hours <- as.numeric(difftime(Sys.time(), cache_time, units = "hours"))
    if (age_hours >= max_age_hours) {
      return(FALSE)
    }
  }

  TRUE
}

#' Get Cache Entry
#'
#' @param cache_name Name of the cached object
#' @return The cached data, or NULL if not found.
#' @keywords internal
#' @noRd
.get_cache <- function(cache_name) {
  data_name <- paste0(cache_name, "_data")
  if (exists(data_name, envir = .sidra_cache)) {
    data.table::copy(get(data_name, envir = .sidra_cache))
  } else {
    NULL
  }
}

#' Set Cache Entry
#'
#' @param cache_name Name of the cached object
#' @param data Data to cache
#' @keywords internal
#' @noRd
.set_cache <- function(cache_name, data) {
  data_name <- paste0(cache_name, "_data")
  time_name <- paste0(cache_name, "_time")
  assign(data_name, data.table::copy(data), envir = .sidra_cache)
  assign(time_name, Sys.time(), envir = .sidra_cache)
}

#' Get Cache Timestamp
#'
#' @param cache_name Name of the cached object
#' @return POSIXct timestamp or NULL if not cached.
#' @keywords internal
#' @noRd
.get_cache_time <- function(cache_name) {
  time_name <- paste0(cache_name, "_time")
  if (exists(time_name, envir = .sidra_cache)) {
    get(time_name, envir = .sidra_cache)
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
#' @param theme Character vector of themes to filter by. Valid options:
#'   "labor_market", "earnings", "demographics", "social_protection", "prices".
#'   Use NULL for no filter.
#' @param theme_category Character vector of theme categories to filter by.
#'   Use NULL for no filter.
#' @param subcategory Character vector of subcategories to filter by.
#'   Use NULL for no filter.
#' @param exclude_derived Logical. If TRUE, exclude series marked as derived
#'   (is_derived = TRUE in metadata). Default FALSE for backward compatibility.
#'   Derived series (rates) are computed from other series during mensalization,
#'   so excluding them saves API calls when fetching for mensalization.
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
#' # Fetch only labor market series
#' rq_labor <- fetch_sidra_rolling_quarters(theme = "labor_market")
#'
#' # Fetch only unemployment data
#' rq_unemp <- fetch_sidra_rolling_quarters(theme = "labor_market",
#'                                           theme_category = "unemployment")
#'
#' # Fetch specific series
#' rq <- fetch_sidra_rolling_quarters(
#'   series = c("taxadesocup", "popocup", "popdesocup")
#' )
#' }
#'
#' @seealso
#' \code{\link{get_sidra_series_metadata}} for available series names and metadata
#' \code{\link{mensalize_sidra_series}} to convert to exact months
#'
#' @export
fetch_sidra_rolling_quarters <- function(series = "all",
                                          theme = NULL,
                                          theme_category = NULL,
                                          subcategory = NULL,
                                          exclude_derived = FALSE,
                                          use_cache = FALSE,
                                          verbose = TRUE,
                                          retry_failed = TRUE,
                                          max_retries = 3) {

  # Get metadata for requested series
  meta <- get_sidra_series_metadata(
    series = series,
    theme = theme,
    theme_category = theme_category,
    subcategory = subcategory
  )

  # Optionally exclude derived series (rates computed from other series)
  if (exclude_derived && nrow(meta) > 0) {
    meta <- meta[is_derived == FALSE]
  }

  if (nrow(meta) == 0) {
    stop("No series found matching the specified criteria")
  }

  series_names <- meta$series_name

  # Check cache for complete dataset
  if (use_cache && .is_cache_valid("rolling_quarters")) {
    cached_data <- .get_cache("rolling_quarters")

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
  .set_cache("rolling_quarters", result)

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
