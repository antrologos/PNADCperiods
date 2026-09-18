# Internal client for IBGE's "Dados Agregados" API v3.
#
# The legacy host apisidra.ibge.gov.br has been behind a Cloudflare browser
# challenge since 2026-09, answering HTTP 403 to most programmatic requests.
# The aggregated-data API v3 serves the same tables and is not challenged.
#
# The series metadata table stores legacy apisidra paths, so this file
# translates them at request time and reshapes the v3 `view=flat` payload
# into the data.frame layout the rest of the package already consumes:
# character columns named by the Portuguese labels, plus a numeric `Valor`.

.SIDRA_V3_BASE <- "https://servicodados.ibge.gov.br/api/v3/agregados"

# SIDRA's "allxt" selects every category except the totalising one, a
# shorthand v3 does not have. Classification 12027 (table 3918) has exactly
# two categories: 31295 "Total" (level 0) and 99157 (level 1), so allxt is
# equivalent to [99157]. Listing it explicitly keeps a total row from
# sneaking in and duplicating periods.
.SIDRA_V3_ALLXT <- c("12027" = "99157")

# Markers SIDRA uses in place of a number.
.SIDRA_NA_STRINGS <- c("..", "...", "-", "X", "")


#' Split a legacy apisidra path into its components
#'
#' @param api_path Character(1), e.g. "/t/6318/n1/all/v/1641/p/all/c629/32386".
#' @return List with table, variable, periods, locality, classification,
#'   category and digits.
#' @keywords internal
#' @noRd
.parse_sidra_api_path <- function(api_path) {

  if (!is.character(api_path) || length(api_path) != 1L || is.na(api_path)) {
    stop("api_path must be a single non-NA string", call. = FALSE)
  }

  parts <- strsplit(sub("^/+", "", api_path), "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]

  spec <- list(
    table          = NA_character_,
    variable       = NA_character_,
    periods        = NA_character_,
    locality       = NA_character_,
    classification = NA_character_,
    category       = NA_character_,
    digits         = NA_integer_
  )

  i <- 1L
  while (i <= length(parts)) {
    key <- parts[i]
    nxt <- if (i < length(parts)) parts[i + 1L] else NA_character_

    if (is.na(nxt)) {
      stop("Malformed SIDRA api path (dangling '", key, "'): ", api_path,
           call. = FALSE)
    }

    if (identical(key, "t")) {
      spec$table <- nxt
    } else if (identical(key, "v")) {
      spec$variable <- nxt
    } else if (identical(key, "p")) {
      spec$periods <- nxt
    } else if (grepl("^n[0-9]+$", key)) {
      spec$locality <- paste0(toupper(key), "[", nxt, "]")
    } else if (identical(key, "d")) {
      # "/d/v4099%201" asks apisidra for 1 decimal place. v3 has no
      # equivalent, so the rounding is reproduced client-side.
      spec$digits <- suppressWarnings(
        as.integer(sub(".* ", "", gsub("%20", " ", nxt, fixed = TRUE)))
      )
    } else if (grepl("^c[0-9]+$", key)) {
      spec$classification <- sub("^c", "", key)
      spec$category <- nxt
    } else {
      stop("Unsupported SIDRA api path token '", key, "' in: ", api_path,
           call. = FALSE)
    }

    i <- i + 2L
  }

  if (is.na(spec$table) || is.na(spec$variable)) {
    stop("Malformed SIDRA api path (missing /t/ or /v/): ", api_path,
         call. = FALSE)
  }

  if (is.na(spec$periods))  spec$periods  <- "all"
  if (is.na(spec$locality)) spec$locality <- "N1[all]"

  spec
}


#' Percent-encode the square brackets of a v3 selector
#'
#' v3 accepts both literal and encoded brackets; encoding keeps the request
#' valid across libcurl builds that reject unescaped brackets in a query.
#'
#' @keywords internal
#' @noRd
.sidra_v3_brackets <- function(x) {
  gsub("]", "%5D", gsub("[", "%5B", x, fixed = TRUE), fixed = TRUE)
}


#' Build the v3 request URL for a legacy apisidra path
#'
#' @keywords internal
#' @noRd
.sidra_v3_url <- function(api_path) {
  .sidra_v3_url_from_spec(.parse_sidra_api_path(api_path))
}

.sidra_v3_url_from_spec <- function(spec) {

  query <- paste0("localidades=", .sidra_v3_brackets(spec$locality))

  if (!is.na(spec$classification)) {
    category <- spec$category

    if (identical(category, "allxt")) {
      category <- unname(.SIDRA_V3_ALLXT[spec$classification])
      if (is.na(category)) {
        stop("No v3 translation for 'allxt' on classification c",
             spec$classification,
             ". Add it to .SIDRA_V3_ALLXT after checking the table metadata.",
             call. = FALSE)
      }
    }

    query <- paste0(
      query, "&classificacao=", spec$classification,
      .sidra_v3_brackets(paste0("[", category, "]"))
    )
  }

  base <- getOption("PNADCperiods.sidra_base_url", .SIDRA_V3_BASE)

  paste0(base, "/", spec$table,
         "/periodos/", spec$periods,
         "/variaveis/", spec$variable,
         "?", query, "&view=flat")
}


#' Perform one HTTP request (seam kept separate so tests can replace it)
#'
#' @keywords internal
#' @noRd
.sidra_v3_curl_fetch <- function(url, handle) {
  curl::curl_fetch_memory(url, handle = handle)
}


#' Fetch the raw JSON body of a v3 request
#'
#' @return Character(1) holding the UTF-8 response body.
#' @keywords internal
#' @noRd
.sidra_v3_get_json <- function(url, timeout_seconds = 60) {

  handle <- curl::new_handle(
    timeout         = timeout_seconds,
    connecttimeout  = min(timeout_seconds, 20),
    useragent       = paste0("PNADCperiods/",
                             utils::packageVersion("PNADCperiods"),
                             " (https://github.com/antrologos/PNADCperiods)"),
    accept_encoding = "gzip"
  )
  curl::handle_setheaders(handle, Accept = "application/json")

  response <- .sidra_v3_curl_fetch(url, handle)

  if (!identical(as.integer(response$status_code), 200L)) {
    stop("SIDRA API returned HTTP ", response$status_code, " for ", url,
         call. = FALSE)
  }

  body <- rawToChar(response$content)
  Encoding(body) <- "UTF-8"
  body
}


#' Reshape a v3 `view=flat` payload into the legacy data.frame layout
#'
#' The first element of a flat payload carries the Portuguese column labels,
#' exactly as apisidra did. They become the column names and the row is
#' dropped, leaving character columns plus a numeric `Valor`.
#'
#' @param json_text Character(1), the response body.
#' @param digits Integer or NA; decimal places requested by a /d/ modifier.
#' @keywords internal
#' @noRd
.sidra_flat_to_data_frame <- function(json_text, digits = NA_integer_) {

  flat <- jsonlite::fromJSON(json_text, simplifyDataFrame = TRUE)

  if (!is.data.frame(flat) || nrow(flat) < 1L) {
    stop("Unexpected SIDRA API payload (not a flat table)", call. = FALSE)
  }

  header <- vapply(flat, function(column) as.character(column[[1L]]),
                   character(1))

  df <- flat[-1L, , drop = FALSE]
  names(df) <- unname(header)
  rownames(df) <- NULL

  if (!"Valor" %in% names(df)) {
    stop("Unexpected SIDRA API payload (no 'Valor' column)", call. = FALSE)
  }

  for (column in names(df)) df[[column]] <- as.character(df[[column]])

  values <- df[["Valor"]]
  values[values %in% .SIDRA_NA_STRINGS] <- NA_character_
  values <- suppressWarnings(as.numeric(values))
  if (!is.na(digits)) values <- round(values, digits)
  df[["Valor"]] <- values

  df
}


#' Fetch one SIDRA series from the aggregated-data API v3
#'
#' Drop-in replacement for the legacy per-series request: it takes the same
#' apisidra path stored in the series metadata and returns the same shape.
#' Signals an error on transport failure, a non-200 status, a malformed path
#' or an unexpected payload, so callers can keep handling failure in one
#' place.
#'
#' @param api_path Character(1), a legacy apisidra path.
#' @keywords internal
#' @noRd
.get_sidra_v3 <- function(api_path,
                          timeout_seconds = getOption(
                            "PNADCperiods.sidra_timeout", 60)) {

  spec <- .parse_sidra_api_path(api_path)
  body <- .sidra_v3_get_json(.sidra_v3_url_from_spec(spec),
                             timeout_seconds = timeout_seconds)

  .sidra_flat_to_data_frame(body, digits = spec$digits)
}
