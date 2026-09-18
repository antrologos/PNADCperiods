# Tests for the internal client for the IBGE aggregated-data API v3.
#
# The v3 service replaces apisidra.ibge.gov.br, which sits behind a
# Cloudflare challenge since 2026-09. The adapter must reproduce, column
# for column, the data.frame shape the package consumed before, so that
# .process_sidra_response() and fetch_monthly_population() keep working
# unchanged.

# ---- path parser ---------------------------------------------------------

test_that(".parse_sidra_api_path reads table, variable and period", {
  spec <- .parse_sidra_api_path("/t/6390/n1/all/v/5929/p/all")

  expect_equal(spec$table, "6390")
  expect_equal(spec$variable, "5929")
  expect_equal(spec$periods, "all")
  expect_equal(spec$locality, "N1[all]")
  expect_true(is.na(spec$classification))
  expect_true(is.na(spec$digits))
})

test_that(".parse_sidra_api_path reads the classification and category", {
  spec <- .parse_sidra_api_path("/t/6318/n1/all/v/1641/p/all/c629/32386")

  expect_equal(spec$classification, "629")
  expect_equal(spec$category, "32386")
})

test_that(".parse_sidra_api_path extracts the decimal count from /d/", {
  expect_equal(
    .parse_sidra_api_path("/t/6381/n1/all/v/4099/p/all/d/v4099%201")$digits,
    1L
  )
  expect_equal(
    .parse_sidra_api_path("/t/1737/n1/all/v/2266/p/all/d/v2266%2013")$digits,
    13L
  )
})

test_that(".parse_sidra_api_path errors on an unsupported token", {
  expect_error(
    .parse_sidra_api_path("/t/6318/n1/all/v/1641/p/all/f/a"),
    "Unsupported"
  )
  expect_error(.parse_sidra_api_path("/t/6318/n1/all/v"), "Malformed")
  expect_error(.parse_sidra_api_path("/n1/all/p/all"), "Malformed")
})

# ---- URL builder ---------------------------------------------------------

test_that(".sidra_v3_url builds the expected request for a plain path", {
  expect_equal(
    .sidra_v3_url("/t/6390/n1/all/v/5929/p/all"),
    paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6390",
           "/periodos/all/variaveis/5929",
           "?localidades=N1%5Ball%5D&view=flat")
  )
})

test_that(".sidra_v3_url appends the classificacao query parameter", {
  expect_equal(
    .sidra_v3_url("/t/6318/n1/all/v/1641/p/all/c629/32386"),
    paste0("https://servicodados.ibge.gov.br/api/v3/agregados/6318",
           "/periodos/all/variaveis/1641",
           "?localidades=N1%5Ball%5D&classificacao=629%5B32386%5D&view=flat")
  )
})

test_that(".sidra_v3_url leaves the /d/ modifier out of the URL", {
  url <- .sidra_v3_url("/t/6381/n1/all/v/4099/p/all/d/v4099%201")

  expect_false(grepl("/d/", url, fixed = TRUE))
  expect_false(grepl("4099%201", url, fixed = TRUE))
  expect_true(grepl("variaveis/4099", url, fixed = TRUE))
})

test_that(".sidra_v3_url resolves allxt to the single non-total category", {
  url <- .sidra_v3_url("/t/3918/n1/all/v/4090/p/all/c12027/allxt")

  expect_true(grepl("classificacao=12027%5B99157%5D", url, fixed = TRUE))
  expect_false(grepl("allxt", url, fixed = TRUE))
})

test_that(".sidra_v3_url errors on allxt for an unmapped classification", {
  expect_error(
    .sidra_v3_url("/t/6318/n1/all/v/1641/p/all/c629/allxt"),
    "allxt"
  )
})

test_that(".sidra_v3_url translates every api_path in the metadata table", {
  meta <- get_sidra_series_metadata()
  expect_gte(nrow(meta), 80)

  for (i in seq_len(nrow(meta))) {
    path <- meta$api_path[i]
    spec <- .parse_sidra_api_path(path)

    # The parsed ids must agree with the denormalised metadata columns.
    expect_equal(as.integer(spec$table), meta$table_id[i],
                 info = paste("table_id mismatch for", meta$series_name[i]))
    expect_equal(as.integer(spec$variable), meta$variable_id[i],
                 info = paste("variable_id mismatch for", meta$series_name[i]))

    url <- .sidra_v3_url(path)
    expect_true(startsWith(url, .SIDRA_V3_BASE),
                info = paste("bad base for", meta$series_name[i]))
    expect_true(endsWith(url, "&view=flat"),
                info = paste("missing view=flat for", meta$series_name[i]))
  }
})

# ---- flat JSON converter -------------------------------------------------

test_that(".sidra_flat_to_data_frame reproduces the sidrar column contract", {
  df <- .sidra_flat_to_data_frame(fixture_v3_flat_rq)

  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "data.table"))
  expect_equal(nrow(df), 3L)

  expect_true("Valor" %in% names(df))
  expect_true("Trimestre Móvel (Código)" %in% names(df))
  expect_true(
    paste0("Condição em relação à força de ",
           "trabalho e condição de ocupação (Código)") %in% names(df)
  )

  expect_type(df$Valor, "double")
  non_value <- setdiff(names(df), "Valor")
  expect_true(all(vapply(df[non_value], is.character, logical(1))))

  expect_equal(df[["Trimestre Móvel (Código)"]],
               c("202605", "202606", "202607"))
})

test_that(".sidra_flat_to_data_frame handles the monthly period label", {
  df <- .sidra_flat_to_data_frame(fixture_v3_flat_month)

  expect_true("Mês (Código)" %in% names(df))
  expect_false("Trimestre Móvel (Código)" %in% names(df))
})

test_that(".sidra_flat_to_data_frame maps SIDRA missing markers to NA", {
  df <- .sidra_flat_to_data_frame(fixture_v3_flat_na)

  expect_true(all(is.na(df$Valor)))
  expect_type(df$Valor, "double")
})

test_that(".sidra_flat_to_data_frame applies the /d/ rounding", {
  rounded <- .sidra_flat_to_data_frame(fixture_v3_flat_month, digits = 1L)
  plain   <- .sidra_flat_to_data_frame(fixture_v3_flat_month)

  expect_equal(rounded$Valor, round(plain$Valor, 1L))
})

test_that(".sidra_flat_to_data_frame returns zero rows for a header-only payload", {
  df <- .sidra_flat_to_data_frame(fixture_v3_flat_header_only)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_true("Valor" %in% names(df))
})

test_that(".sidra_flat_to_data_frame errors when the payload is not a flat table", {
  expect_error(.sidra_flat_to_data_frame(fixture_v3_error_500), "payload")
  expect_error(.sidra_flat_to_data_frame("[]"), "payload")
})

# ---- contract with the existing (unchanged) consumers --------------------

test_that(".sidra_flat_to_data_frame output is accepted by .process_sidra_response", {
  df <- .sidra_flat_to_data_frame(fixture_v3_flat_rq)
  dt <- .process_sidra_response(df, "popnaforca")

  expect_s3_class(dt, "data.table")
  expect_equal(names(dt), c("anomesfinaltrimmovel", "popnaforca"))
  expect_type(dt$anomesfinaltrimmovel, "integer")
  expect_equal(dt$anomesfinaltrimmovel, c(202605L, 202606L, 202607L))
  expect_true(all(is.finite(dt$popnaforca)))
})

test_that("the monthly payload also survives .process_sidra_response", {
  df <- .sidra_flat_to_data_frame(fixture_v3_flat_month)
  dt <- .process_sidra_response(df, "ipcavarmensal")

  expect_s3_class(dt, "data.table")
  expect_equal(dt$anomesfinaltrimmovel, c(202606L, 202607L))
})

test_that("the population payload satisfies the population column heuristic", {
  df <- .sidra_flat_to_data_frame(fixture_v3_flat_pop)

  # Same grep as R/fetch-sidra-population.R
  code_col <- grep("Trimestre.*vel.*digo|trimestre.*vel.*digo",
                   names(df), value = TRUE, ignore.case = TRUE)

  expect_length(code_col, 1L)
  expect_false(anyNA(as.integer(df[[code_col[1]]])))
  expect_true(all(is.finite(df$Valor)))
})

# ---- fetcher -------------------------------------------------------------

test_that(".get_sidra_v3 propagates a transport error", {
  testthat::local_mocked_bindings(
    .sidra_v3_get_json = function(...) stop("simulated network error")
  )

  expect_error(.get_sidra_v3("/t/6022/n1/all/v/606/p/all"),
               "simulated network error")
})

test_that(".get_sidra_v3 errors on a non-200 response", {
  testthat::local_mocked_bindings(
    .sidra_v3_curl_fetch = function(...) {
      list(status_code = 500L, content = charToRaw(fixture_v3_error_500))
    }
  )

  expect_error(.get_sidra_v3("/t/6022/n1/all/v/606/p/all"), "HTTP 500")
})

test_that(".get_sidra_v3 rounds according to the /d/ modifier", {
  testthat::local_mocked_bindings(
    .sidra_v3_get_json = function(...) fixture_v3_flat_month
  )

  df <- .get_sidra_v3("/t/1737/n1/all/v/63/p/all/d/v63%201")
  expect_equal(df$Valor, round(df$Valor, 1L))
})

# ---- live integration ----------------------------------------------------

test_that(".get_sidra_v3 returns live data for a rolling-quarter series", {
  skip_on_cran()
  skip_if_offline()

  df <- tryCatch(
    .get_sidra_v3("/t/6381/n1/all/v/4099/p/all/d/v4099%201"),
    error = function(e) skip(paste("SIDRA v3 unavailable:", conditionMessage(e)))
  )

  expect_s3_class(df, "data.frame")
  expect_gt(nrow(df), 100)
  expect_true("Valor" %in% names(df))
})

test_that(".get_sidra_v3 returns one row per period for contribuinteprev", {
  skip_on_cran()
  skip_if_offline()

  df <- tryCatch(
    .get_sidra_v3("/t/3918/n1/all/v/4090/p/all/c12027/allxt"),
    error = function(e) skip(paste("SIDRA v3 unavailable:", conditionMessage(e)))
  )

  code_col <- grep("Trimestre.*vel.*digo", names(df),
                   value = TRUE, ignore.case = TRUE)[1]

  # An allxt mistranslated to [all] would return the total row too, and
  # duplicated periods would silently corrupt the merge of all 90 series.
  expect_false(anyDuplicated(df[[code_col]]) > 0)
})
