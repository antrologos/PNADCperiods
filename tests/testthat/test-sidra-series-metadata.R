# Tests for sidra-series-metadata.R
# Note: All tests use the metadata function's own output (no external data needed)

# =============================================================================
# DEFAULT CALL: STRUCTURE AND COMPLETENESS
# =============================================================================

test_that("default call returns 86+ rows data.table with all required columns", {
  # 1. Setup: Call with defaults
  meta <- get_sidra_series_metadata()

  # 2. Verify: Structure

  expect_s3_class(meta, "data.table")
  expect_gte(nrow(meta), 86)

  # 3. Verify: All documented columns are present
  required_cols <- c(
    "series_name", "api_path", "table_id", "variable_id",
    "classification_id", "classification_value",
    "theme", "theme_category", "subcategory",
    "description_pt", "description_en", "description",
    "unit", "unit_label_pt", "unit_label_en",
    "is_derived", "requires_deflation"
  )
  for (col in required_cols) {
    expect_true(col %in% names(meta),
                label = paste("Column", col, "must be present"))
  }
})

test_that("no duplicate series_name values", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: All series names are unique
  expect_equal(length(unique(meta$series_name)), nrow(meta),
               label = "series_name values must be unique")

  # 3. Context: Duplicates would cause ambiguous lookups
  dup_names <- meta$series_name[duplicated(meta$series_name)]
  expect_equal(length(dup_names), 0,
               label = paste("Duplicated names:", paste(dup_names, collapse = ", ")))
})

test_that("all api_path values start with /t/", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: SIDRA API paths follow the /t/{table_id}/... pattern
  expect_true(all(grepl("^/t/", meta$api_path)),
              label = "All api_path values must start with /t/")

  # 3. Context: Malformed paths will cause SIDRA API failures
  bad_paths <- meta$api_path[!grepl("^/t/", meta$api_path)]
  expect_equal(length(bad_paths), 0,
               label = paste("Invalid paths:", paste(bad_paths, collapse = "; ")))
})

test_that("all table_id and variable_id are positive integers", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: table_id
  expect_type(meta$table_id, "integer")
  expect_true(all(meta$table_id > 0L),
              label = "All table_id must be positive integers")

  # 3. Verify: variable_id
  expect_type(meta$variable_id, "integer")
  expect_true(all(meta$variable_id > 0L),
              label = "All variable_id must be positive integers")
})

# =============================================================================
# THEME FILTERING
# =============================================================================

test_that("theme filtering: labor_market returns a subset", {
  # 1. Setup
  all_meta <- get_sidra_series_metadata()
  labor <- get_sidra_series_metadata(theme = "labor_market")

  # 2. Verify: Filtered result is a proper subset
  expect_s3_class(labor, "data.table")
  expect_gt(nrow(labor), 0)
  expect_lt(nrow(labor), nrow(all_meta))

  # 3. Verify: All returned rows have the correct theme
  expect_true(all(labor$theme == "labor_market"),
              label = "All rows must have theme == 'labor_market'")
})

test_that("invalid theme raises error with informative message", {
  # 1. Execute + Verify: Invalid theme name
  expect_error(
    get_sidra_series_metadata(theme = "nonexistent_theme"),
    "Invalid theme.*nonexistent_theme"
  )

  # 2. Context: Check that valid themes are listed in the error
  expect_error(
    get_sidra_series_metadata(theme = "fake"),
    "labor_market"
  )
})

test_that("multiple themes can be specified", {
  # 1. Setup
  combined <- get_sidra_series_metadata(theme = c("labor_market", "earnings"))

  # 2. Verify: Contains rows from both themes
  expect_true("labor_market" %in% combined$theme)
  expect_true("earnings" %in% combined$theme)

  # 3. Verify: No rows from other themes
  expect_true(all(combined$theme %in% c("labor_market", "earnings")),
              label = "Only requested themes should appear")
})

# =============================================================================
# THEME_CATEGORY FILTERING
# =============================================================================

test_that("theme_category filtering works", {
  # 1. Setup
  unemployment <- get_sidra_series_metadata(theme_category = "unemployment")

  # 2. Verify: Returns non-empty subset
  expect_gt(nrow(unemployment), 0)
  expect_true(all(unemployment$theme_category == "unemployment"))

  # 3. Context: taxadesocup should be in unemployment category
  expect_true("taxadesocup" %in% unemployment$series_name)
})

test_that("theme_category with no matches returns zero rows", {
  # 1. Setup: Use a category that does not exist
  result <- get_sidra_series_metadata(theme_category = "nonexistent_category")

  # 2. Verify: Returns valid data.table with zero rows
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
})

# =============================================================================
# SUBCATEGORY FILTERING
# =============================================================================

test_that("subcategory filtering includes NA subcategory rows", {
  # 1. Setup: economic_sector has subcategory = NA; filter for "levels"
  #    should return rows with subcategory == "levels" OR subcategory == NA
  result <- get_sidra_series_metadata(
    theme = "labor_market",
    subcategory = "levels"
  )

  # 2. Verify: Contains explicit "levels" rows
  expect_true("levels" %in% result$subcategory)

  # 3. Verify: Also includes NA subcategory rows (economic_sector, etc.)
  expect_true(any(is.na(result$subcategory)),
              label = "NA subcategory rows should be included when filtering by subcategory")
})

test_that("subcategory filtering returns correct subset", {
  # 1. Setup: Get only "rates" subcategory from labor_market
  rates <- get_sidra_series_metadata(theme = "labor_market", subcategory = "rates")

  # 2. Verify: All non-NA subcategories are "rates"
  non_na_subcats <- rates$subcategory[!is.na(rates$subcategory)]
  expect_true(all(non_na_subcats == "rates"),
              label = "Non-NA subcategories should all be 'rates'")
})

# =============================================================================
# SPECIFIC SERIES SELECTION
# =============================================================================

test_that("specific series selection: taxadesocup returns exactly 1 row", {
  # 1. Setup
  result <- get_sidra_series_metadata(series = "taxadesocup")

  # 2. Verify: Exactly one row
  expect_equal(nrow(result), 1)
  expect_equal(result$series_name, "taxadesocup")

  # 3. Verify: Known metadata values
  expect_equal(result$table_id, 6381L)
  expect_equal(result$variable_id, 4099L)
  expect_equal(result$theme, "labor_market")
  expect_equal(result$theme_category, "unemployment")
  expect_equal(result$unit, "percent")
})

test_that("multiple specific series selection works", {
  # 1. Setup
  result <- get_sidra_series_metadata(series = c("taxadesocup", "popocup", "populacao"))

  # 2. Verify: Exactly 3 rows, one per requested series
  expect_equal(nrow(result), 3)
  expect_true(all(c("taxadesocup", "popocup", "populacao") %in% result$series_name))
})

test_that("invalid series name raises error", {
  # 1. Execute + Verify: Unknown series name
  expect_error(
    get_sidra_series_metadata(series = "this_series_does_not_exist"),
    "Unknown series.*this_series_does_not_exist"
  )

  # 2. Verify: Partially valid set still errors on the invalid entry
  expect_error(
    get_sidra_series_metadata(series = c("taxadesocup", "fake_series")),
    "Unknown series.*fake_series"
  )
})

# =============================================================================
# COMBINED FILTERS
# =============================================================================

test_that("combined theme + theme_category filters work correctly", {
  # 1. Setup: Filter labor_market -> participation
  result <- get_sidra_series_metadata(
    theme = "labor_market",
    theme_category = "participation"
  )

  # 2. Verify: All rows match both criteria
  expect_true(all(result$theme == "labor_market"))
  expect_true(all(result$theme_category == "participation"))

  # 3. Verify: Known series are present
  expect_true("taxapartic" %in% result$series_name)
  expect_true("popocup" %in% result$series_name)
  expect_true("popdesocup" %in% result$series_name)
})

test_that("combined theme + subcategory + series filters work", {
  # 1. Setup: Narrow filter that should return 1 row
  result <- get_sidra_series_metadata(
    theme = "labor_market",
    subcategory = "rates",
    series = "taxadesocup"
  )

  # 2. Verify
  expect_equal(nrow(result), 1)
  expect_equal(result$series_name, "taxadesocup")
})

# =============================================================================
# UNIT CONSISTENCY
# =============================================================================

test_that("population/level series have unit = thousands", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: Known population-level series use "thousands"
  pop_series <- c("popocup", "popdesocup", "popnaforca", "popforadaforca",
                   "populacao", "pop14mais")
  pop_meta <- meta[series_name %in% pop_series]
  expect_true(all(pop_meta$unit == "thousands"),
              label = "All population/level series must use unit = 'thousands'")
})

test_that("rate series have unit = percent", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: Known rate series use "percent"
  rate_series <- c("taxadesocup", "taxapartic", "nivelocup", "niveldesocup",
                    "taxacombdesosub", "taxacompsubutlz", "perccontribprev")
  rate_meta <- meta[series_name %in% rate_series]
  expect_true(all(rate_meta$unit == "percent"),
              label = "All rate series must use unit = 'percent'")
})

test_that("income series have unit = currency or currency_millions", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: Earnings series use "currency"
  earnings_avg <- c("rendhabnominaltodos", "rendhabrealtodos",
                     "rendhabrealprinc", "rendefetrealprinc")
  earn_meta <- meta[series_name %in% earnings_avg]
  expect_true(all(earn_meta$unit == "currency"),
              label = "Average earnings series must use unit = 'currency'")

  # 3. Verify: Wage mass series use "currency_millions"
  mass_series <- c("massahabnominaltodos", "massahabrealtodos",
                    "massaefetnominaltodos", "massaefetrealtodos")
  mass_meta <- meta[series_name %in% mass_series]
  expect_true(all(mass_meta$unit == "currency_millions"),
              label = "Wage mass series must use unit = 'currency_millions'")
})

# =============================================================================
# IS_DERIVED CONSISTENCY
# =============================================================================

test_that("is_derived is TRUE for rate/percentage series", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: Known rate series (which are derived from ratios) are marked is_derived
  derived_series <- c("taxadesocup", "taxapartic", "nivelocup", "niveldesocup",
                       "taxacombdesosub", "taxacombdesopot", "taxacompsubutlz",
                       "taxasubocuphoras", "percdesalento", "perccontribprev")
  derived_meta <- meta[series_name %in% derived_series]
  expect_true(all(derived_meta$is_derived),
              label = "Rate/percentage series should have is_derived = TRUE")

  # 3. Verify: Population level series are NOT derived
  level_series <- c("popocup", "popdesocup", "popnaforca", "populacao")
  level_meta <- meta[series_name %in% level_series]
  expect_true(all(!level_meta$is_derived),
              label = "Population level series should have is_derived = FALSE")
})

# =============================================================================
# LANGUAGE SWITCHING
# =============================================================================

test_that("lang = 'en' switches description column to English", {
  # 1. Setup
  meta_pt <- get_sidra_series_metadata(series = "taxadesocup", lang = "pt")
  meta_en <- get_sidra_series_metadata(series = "taxadesocup", lang = "en")

  # 2. Verify: description column differs by language
  expect_equal(meta_pt$description, meta_pt$description_pt)
  expect_equal(meta_en$description, meta_en$description_en)

  # 3. Verify: English and Portuguese descriptions are different
  expect_false(identical(meta_pt$description, meta_en$description),
               label = "PT and EN descriptions should differ")

  # 4. Context: Both description_pt and description_en columns should always be present
  expect_true("description_pt" %in% names(meta_en))
  expect_true("description_en" %in% names(meta_pt))
})

test_that("lang = 'pt' is the default", {
  # 1. Setup
  meta_default <- get_sidra_series_metadata(series = "popocup")
  meta_pt <- get_sidra_series_metadata(series = "popocup", lang = "pt")

  # 2. Verify: Default description matches Portuguese
  expect_equal(meta_default$description, meta_pt$description_pt)
})

# =============================================================================
# .get_mesnotrim INTERNAL FUNCTION
# =============================================================================

test_that(".get_mesnotrim maps quarter-starting months to position 1", {
  # 1. Setup + Verify: Months 1, 4, 7, 10 are the first month of their quarter
  expect_equal(PNADCperiods:::.get_mesnotrim(1), 1)
  expect_equal(PNADCperiods:::.get_mesnotrim(4), 1)
  expect_equal(PNADCperiods:::.get_mesnotrim(7), 1)
  expect_equal(PNADCperiods:::.get_mesnotrim(10), 1)
})

test_that(".get_mesnotrim maps second months to position 2", {
  # 1. Setup + Verify: Months 2, 5, 8, 11
  expect_equal(PNADCperiods:::.get_mesnotrim(2), 2)
  expect_equal(PNADCperiods:::.get_mesnotrim(5), 2)
  expect_equal(PNADCperiods:::.get_mesnotrim(8), 2)
  expect_equal(PNADCperiods:::.get_mesnotrim(11), 2)
})

test_that(".get_mesnotrim maps quarter-ending months to position 3", {
  # 1. Setup + Verify: Months 3, 6, 9, 12
  expect_equal(PNADCperiods:::.get_mesnotrim(3), 3)
  expect_equal(PNADCperiods:::.get_mesnotrim(6), 3)
  expect_equal(PNADCperiods:::.get_mesnotrim(9), 3)
  expect_equal(PNADCperiods:::.get_mesnotrim(12), 3)
})

test_that(".get_mesnotrim is vectorized", {
  # 1. Setup: Pass all 12 months at once
  months <- 1:12
  expected <- rep(1:3, 4)
  result <- PNADCperiods:::.get_mesnotrim(months)

  # 2. Verify: Vectorized output matches individual calls
  expect_equal(result, expected)
  expect_length(result, 12)
})

# =============================================================================
# .PNADC_DATES INTERNAL CONSTANTS
# =============================================================================

test_that(".PNADC_DATES contains expected constants with correct values", {
  # 1. Setup
  dates <- PNADCperiods:::.PNADC_DATES

  # 2. Verify: Is a named list
  expect_type(dates, "list")
  expect_true(length(dates) > 0)

  # 3. Verify: Specific constant values
  expect_equal(dates$PNADC_START, 201201L,
               label = "PNADC started in January 2012")
  expect_equal(dates$VD4004_SPLIT, 201509L,
               label = "VD4004 split occurred in September 2015")
  expect_equal(dates$V4019_AVAILABLE, 201510L,
               label = "V4019 available from October 2015")
  expect_equal(dates$DEFAULT_CALIB_START, 201301L,
               label = "Default calibration starts January 2013")
  expect_equal(dates$DEFAULT_CALIB_END, 201912L,
               label = "Default calibration ends December 2019 (pre-COVID)")
  expect_equal(dates$CNPJ_CALIB_START, 201601L,
               label = "CNPJ calibration starts January 2016")
  expect_equal(dates$PRESPLIT_CALIB_END, 201412L,
               label = "Pre-split calibration ends December 2014")
})

test_that(".PNADC_DATES values are all integer scalars", {
  # 1. Setup
  dates <- PNADCperiods:::.PNADC_DATES

  # 2. Verify: Each element is a single integer
  for (nm in names(dates)) {
    expect_type(dates[[nm]], "integer")
    expect_length(dates[[nm]], 1)
  }
})

# =============================================================================
# KEY SERIES METADATA SPOT-CHECKS
# =============================================================================

test_that("taxadesocup has correct api_path and table_id", {
  # 1. Setup
  result <- get_sidra_series_metadata(series = "taxadesocup")

  # 2. Verify: Known correct values from SIDRA
  expect_equal(result$table_id, 6381L)
  expect_equal(result$variable_id, 4099L)
  expect_equal(result$api_path, "/t/6381/n1/all/v/4099/p/all/d/v4099%201")
  expect_equal(result$unit, "percent")
  expect_true(result$is_derived)
  expect_false(result$requires_deflation)
})

test_that("populacao series has correct metadata", {
  # 1. Setup
  result <- get_sidra_series_metadata(series = "populacao")

  # 2. Verify
  expect_equal(result$table_id, 6022L)
  expect_equal(result$variable_id, 606L)
  expect_equal(result$theme, "demographics")
  expect_equal(result$unit, "thousands")
  expect_false(result$is_derived)
  expect_false(result$requires_deflation)
})

test_that("rendhabrealtodos earnings series has correct metadata", {
  # 1. Setup
  result <- get_sidra_series_metadata(series = "rendhabrealtodos")

  # 2. Verify
  expect_equal(result$table_id, 6390L)
  expect_equal(result$theme, "earnings")
  expect_equal(result$unit, "currency")
  expect_false(result$is_derived)
  expect_true(result$requires_deflation)
})

# =============================================================================
# API_PATH VALIDITY FOR NON-DERIVED SERIES
# =============================================================================

test_that("all non-derived series have valid api_path structure", {
  # 1. Setup
  meta <- get_sidra_series_metadata()
  non_derived <- meta[is_derived == FALSE]

  # 2. Verify: api_path matches /t/{table_id}/... pattern
  for (i in seq_len(nrow(non_derived))) {
    row <- non_derived[i]
    expected_prefix <- paste0("/t/", row$table_id, "/")
    expect_true(
      startsWith(row$api_path, expected_prefix),
      label = paste(row$series_name, "api_path should start with", expected_prefix)
    )
  }
})

test_that("api_path contains the variable_id for all series", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: Each api_path references its variable_id in the /v/ segment
  for (i in seq_len(nrow(meta))) {
    row <- meta[i]
    var_pattern <- paste0("/v/", row$variable_id)
    expect_true(
      grepl(var_pattern, row$api_path, fixed = TRUE),
      label = paste(row$series_name, "api_path should contain", var_pattern)
    )
  }
})

# =============================================================================
# REQUIRES_DEFLATION CONSISTENCY
# =============================================================================

test_that("requires_deflation is TRUE only for real earnings series", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: Series requiring deflation are in the earnings theme
  deflated <- meta[requires_deflation == TRUE]
  expect_true(all(deflated$theme == "earnings"),
              label = "Only earnings series should require deflation")

  # 3. Verify: Non-earnings series never require deflation
  non_earnings <- meta[theme != "earnings"]
  expect_true(all(non_earnings$requires_deflation == FALSE),
              label = "Non-earnings series should not require deflation")
})

test_that("nominal earnings series do not require deflation", {
  # 1. Setup
  nominal_series <- c("rendhabnominaltodos", "rendefetnominaltodos",
                       "massahabnominaltodos", "massaefetnominaltodos")
  nominal_meta <- get_sidra_series_metadata(series = nominal_series)

  # 2. Verify: Nominal series do not require deflation
  expect_true(all(nominal_meta$requires_deflation == FALSE),
              label = "Nominal earnings series should not require deflation")
})

test_that("real earnings series require deflation", {
  # 1. Setup
  real_series <- c("rendhabrealtodos", "rendhabrealprinc",
                    "rendefetrealprinc", "massahabrealtodos", "massaefetrealtodos")
  real_meta <- get_sidra_series_metadata(series = real_series)

  # 2. Verify: Real series require deflation
  expect_true(all(real_meta$requires_deflation == TRUE),
              label = "Real earnings series should require deflation")
})

# =============================================================================
# THEME COVERAGE
# =============================================================================

test_that("all five themes are represented in the metadata", {
  # 1. Setup
  meta <- get_sidra_series_metadata()
  themes <- unique(meta$theme)

  # 2. Verify: All expected themes exist
  expected_themes <- c("labor_market", "earnings", "demographics",
                        "social_protection", "prices")
  for (th in expected_themes) {
    expect_true(th %in% themes,
                label = paste("Theme", th, "should be represented"))
  }

  # 3. Verify: No unexpected themes
  expect_true(all(themes %in% expected_themes),
              label = "No unexpected themes should exist")
})

test_that("prices theme includes IPCA and INPC series", {
  # 1. Setup
  prices <- get_sidra_series_metadata(theme = "prices")

  # 2. Verify: Known price index series
  expect_true("ipca100dez1993" %in% prices$series_name)
  expect_true("ipcavarmensal" %in% prices$series_name)
  expect_true("inpc100dez1993" %in% prices$series_name)
  expect_true("inpcvarmensal" %in% prices$series_name)

  # 3. Verify: Price index units
  ipca_idx <- prices[series_name == "ipca100dez1993"]
  expect_equal(ipca_idx$unit, "index")
  ipca_var <- prices[series_name == "ipcavarmensal"]
  expect_equal(ipca_var$unit, "percent")
})

# =============================================================================
# CLASSIFICATION COLUMNS
# =============================================================================

test_that("classification_id and classification_value are consistent", {
  # 1. Setup
  meta <- get_sidra_series_metadata()

  # 2. Verify: When classification_id is NA, classification_value is also NA
  na_class_id <- meta[is.na(classification_id)]
  expect_true(all(is.na(na_class_id$classification_value)),
              label = "classification_value should be NA when classification_id is NA")

  # 3. Verify: When classification_id is not NA, classification_value is not NA
  non_na_class_id <- meta[!is.na(classification_id)]
  expect_true(all(!is.na(non_na_class_id$classification_value)),
              label = "classification_value should not be NA when classification_id is set")
})

test_that("api_path contains classification when classification_id is set", {
  # 1. Setup
  meta <- get_sidra_series_metadata()
  classified <- meta[!is.na(classification_id)]

  # 2. Verify: api_path references the classification
  for (i in seq_len(nrow(classified))) {
    row <- classified[i]
    expect_true(
      grepl(row$classification_id, row$api_path, fixed = TRUE),
      label = paste(row$series_name, "api_path should contain", row$classification_id)
    )
    expect_true(
      grepl(row$classification_value, row$api_path, fixed = TRUE),
      label = paste(row$series_name, "api_path should contain value", row$classification_value)
    )
  }
})
