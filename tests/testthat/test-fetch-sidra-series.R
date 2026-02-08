# Tests for fetch-sidra-series.R
# Covers: SIDRA cache infrastructure, .process_sidra_response(), metadata filtering,
# and integration tests for fetch_sidra_rolling_quarters()

# =============================================================================
# CACHE INFRASTRUCTURE TESTS (all offline)
# =============================================================================

test_that(".set_cache stores data and timestamp", {
  # 1. Setup: Clear cache to ensure clean state
  PNADCperiods:::clear_sidra_cache()

  # 2. Execute: Store a data.table in cache
  test_dt <- data.table::data.table(
    anomesfinaltrimmovel = c(202301L, 202302L, 202303L),
    popocup = c(100.5, 101.2, 102.0)
  )
  PNADCperiods:::.set_cache("test_series", test_dt)

  # 3. Verify: Data and timestamp exist in cache environment
  cache_env <- PNADCperiods:::.sidra_cache
  expect_true(exists("test_series_data", envir = cache_env))
  expect_true(exists("test_series_time", envir = cache_env))

  # Stored data should match original
  stored <- get("test_series_data", envir = cache_env)
  expect_equal(nrow(stored), 3)
  expect_equal(stored$popocup, c(100.5, 101.2, 102.0))

  # Timestamp should be recent (within last 5 seconds)
  stored_time <- get("test_series_time", envir = cache_env)
  expect_s3_class(stored_time, "POSIXct")
  expect_true(difftime(Sys.time(), stored_time, units = "secs") < 5)

  # 4. Cleanup
  PNADCperiods:::clear_sidra_cache()
})


test_that(".get_cache returns a copy, not a reference, of cached data", {
  # 1. Setup: Clear cache and store data
  PNADCperiods:::clear_sidra_cache()

  original_dt <- data.table::data.table(
    anomesfinaltrimmovel = c(202301L, 202302L),
    valor = c(50.0, 60.0)
  )
  PNADCperiods:::.set_cache("copy_test", original_dt)

  # 2. Execute: Retrieve the cached data
  retrieved <- PNADCperiods:::.get_cache("copy_test")

  # 3. Verify: Modify the retrieved copy and check original is unchanged
  expect_s3_class(retrieved, "data.table")
  retrieved[, valor := valor * 100]

  # Get a fresh copy from cache to verify original is untouched
  fresh_copy <- PNADCperiods:::.get_cache("copy_test")
  expect_equal(fresh_copy$valor, c(50.0, 60.0))

  # Also verify the internal env data is unchanged
  cache_env <- PNADCperiods:::.sidra_cache
  internal <- get("copy_test_data", envir = cache_env)
  expect_equal(internal$valor, c(50.0, 60.0))

  # 4. Cleanup
  PNADCperiods:::clear_sidra_cache()
})


test_that(".get_cache returns NULL when cache is empty", {
  # 1. Setup: Clear cache
  PNADCperiods:::clear_sidra_cache()

  # 2. Execute: Try to retrieve non-existent cache entry
  result <- PNADCperiods:::.get_cache("nonexistent_cache_key")

  # 3. Verify: Should return NULL

  expect_null(result)
})


test_that(".is_cache_valid returns TRUE for fresh cache", {
  # 1. Setup: Clear cache and store fresh data
  PNADCperiods:::clear_sidra_cache()

  test_dt <- data.table::data.table(x = 1:3)
  PNADCperiods:::.set_cache("validity_test", test_dt)

  # 2. Execute & Verify: Fresh cache with no expiration should be valid
  expect_true(PNADCperiods:::.is_cache_valid("validity_test"))

  # Also valid with generous max_age_hours
  expect_true(PNADCperiods:::.is_cache_valid("validity_test", max_age_hours = 24))

  # 3. Cleanup
  PNADCperiods:::clear_sidra_cache()
})


test_that(".is_cache_valid returns FALSE for empty cache", {
  # 1. Setup: Clear cache
  PNADCperiods:::clear_sidra_cache()

  # 2. Execute & Verify: Non-existent entry should not be valid
  expect_false(PNADCperiods:::.is_cache_valid("does_not_exist"))

  # Also FALSE with explicit max_age_hours
  expect_false(PNADCperiods:::.is_cache_valid("does_not_exist", max_age_hours = 24))
})


test_that(".is_cache_valid respects max_age_hours for expired cache", {
  # 1. Setup: Clear cache and store data with a backdated timestamp
  PNADCperiods:::clear_sidra_cache()

  test_dt <- data.table::data.table(x = 1:3)
  PNADCperiods:::.set_cache("expiry_test", test_dt)

  # Manually backdate the timestamp by 3 hours
  cache_env <- PNADCperiods:::.sidra_cache
  assign("expiry_test_time", Sys.time() - 3 * 3600, envir = cache_env)

  # 2. Execute & Verify: Should be invalid if max_age is 1 hour
  expect_false(PNADCperiods:::.is_cache_valid("expiry_test", max_age_hours = 1))

  # But should still be valid if max_age is 24 hours
  expect_true(PNADCperiods:::.is_cache_valid("expiry_test", max_age_hours = 24))

  # And valid with NULL max_age (no expiration)
  expect_true(PNADCperiods:::.is_cache_valid("expiry_test", max_age_hours = NULL))

  # 3. Cleanup
  PNADCperiods:::clear_sidra_cache()
})


test_that(".get_cache_time returns POSIXct timestamp after .set_cache", {
  # 1. Setup: Clear cache and store data
  PNADCperiods:::clear_sidra_cache()

  before <- Sys.time()
  test_dt <- data.table::data.table(x = 1:3)
  PNADCperiods:::.set_cache("time_test", test_dt)
  after <- Sys.time()

  # 2. Execute: Retrieve the cache time
  cache_time <- PNADCperiods:::.get_cache_time("time_test")

  # 3. Verify: Should be a POSIXct between before and after
  expect_s3_class(cache_time, "POSIXct")
  expect_true(cache_time >= before)
  expect_true(cache_time <= after)

  # 4. Cleanup
  PNADCperiods:::clear_sidra_cache()
})


test_that(".get_cache_time returns NULL when not cached", {
  # 1. Setup: Clear cache
  PNADCperiods:::clear_sidra_cache()

  # 2. Execute & Verify: No timestamp for non-existent entry
  result <- PNADCperiods:::.get_cache_time("never_cached")
  expect_null(result)
})


test_that("clear_sidra_cache returns TRUE when cache had data", {
  # 1. Setup: Store something in the cache
  PNADCperiods:::clear_sidra_cache()

  test_dt <- data.table::data.table(x = 1:3)
  PNADCperiods:::.set_cache("clearable", test_dt)

  # 2. Execute: Clear the cache
  result <- PNADCperiods:::clear_sidra_cache()

  # 3. Verify: Should return TRUE (had data to clear)
  expect_true(result)

  # And cache should actually be empty now
  cache_env <- PNADCperiods:::.sidra_cache
  expect_equal(length(ls(envir = cache_env)), 0)
})


test_that("clear_sidra_cache returns FALSE when already empty", {
  # 1. Setup: Ensure cache is empty
  PNADCperiods:::clear_sidra_cache()

  # 2. Execute: Clear again
  result <- PNADCperiods:::clear_sidra_cache()

  # 3. Verify: Should return FALSE (nothing to clear)
  expect_false(result)
})


# =============================================================================
# .process_sidra_response() TESTS (all offline, mock data.frames)
# =============================================================================

test_that(".process_sidra_response extracts values from standard response", {
  # 1. Setup: Create a mock SIDRA API response with typical column names
  mock_response <- data.frame(
    `Trimestre Movel (Codigo)` = c("202301", "202302", "202303"),
    `Trimestre Movel` = c("Jan-Fev-Mar 2023", "Fev-Mar-Abr 2023", "Mar-Abr-Mai 2023"),
    `Valor` = c("100.5", "101.2", "102.0"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # 2. Execute: Process the response
  result <- PNADCperiods:::.process_sidra_response(mock_response, "test_series")

  # 3. Verify: Should produce a data.table with correct structure
  expect_s3_class(result, "data.table")
  expect_true("anomesfinaltrimmovel" %in% names(result))
  expect_true("test_series" %in% names(result))
  expect_equal(nrow(result), 3)

  # Values should be correctly extracted
  expect_equal(result$anomesfinaltrimmovel, c(202301L, 202302L, 202303L))
  expect_equal(result$test_series, c(100.5, 101.2, 102.0))
})


test_that(".process_sidra_response returns NULL with warning for missing code column", {
  # 1. Setup: Create response without the period code column
  mock_response <- data.frame(
    `Some Other Column` = c("A", "B", "C"),
    `Valor` = c("100.5", "101.2", "102.0"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # 2. Execute & Verify: Should return NULL and produce a warning
  expect_warning(
    result <- PNADCperiods:::.process_sidra_response(mock_response, "bad_series"),
    "Could not find period code column"
  )
  expect_null(result)
})


test_that(".process_sidra_response returns NULL for empty response", {
  # 1. Setup: Create an empty data.frame
  mock_empty <- data.frame(
    `Trimestre Movel (Codigo)` = character(0),
    `Valor` = character(0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # 2. Execute: Process empty response
  result <- PNADCperiods:::.process_sidra_response(mock_empty, "empty_series")

  # 3. Verify: Should return NULL (0 rows)
  expect_null(result)
})


test_that(".process_sidra_response returns NULL for NULL input", {
  # 1. Execute & Verify: NULL input should return NULL
  result <- PNADCperiods:::.process_sidra_response(NULL, "null_series")
  expect_null(result)
})


test_that(".process_sidra_response handles NA codes by removing them", {
  # 1. Setup: Response with some NA period codes
  mock_response <- data.frame(
    `Trimestre Movel (Codigo)` = c("202301", NA, "202303"),
    `Valor` = c("100.5", "101.2", "102.0"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # 2. Execute: Process the response
  result <- PNADCperiods:::.process_sidra_response(mock_response, "na_test")

  # 3. Verify: Should have 2 rows (NA code row removed)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  expect_equal(result$anomesfinaltrimmovel, c(202301L, 202303L))
})


# =============================================================================
# METADATA FILTERING TESTS (offline)
# =============================================================================

test_that("theme parameter filters series correctly", {
  # 1. Execute: Get labor_market series
  labor_meta <- get_sidra_series_metadata(theme = "labor_market")

  # 2. Verify: All returned series should have labor_market theme
  expect_true(nrow(labor_meta) > 0)
  expect_true(all(labor_meta$theme == "labor_market"))

  # Cross-check: earnings theme should be different
  earnings_meta <- get_sidra_series_metadata(theme = "earnings")
  expect_true(nrow(earnings_meta) > 0)
  expect_true(all(earnings_meta$theme == "earnings"))

  # No overlap in series names between themes
  expect_length(intersect(labor_meta$series_name, earnings_meta$series_name), 0)
})


test_that("exclude_derived logic correctly filters derived series from metadata", {
  # 1. Execute: Get all metadata and identify derived series
  all_meta <- get_sidra_series_metadata()
  derived_names <- all_meta[is_derived == TRUE, series_name]
  non_derived_names <- all_meta[is_derived == FALSE, series_name]

  # 2. Verify: There should be both derived and non-derived series
  expect_true(length(derived_names) > 0)
  expect_true(length(non_derived_names) > 0)

  # Verify that filtering works as expected for fetch_sidra_rolling_quarters
  # (we test the metadata filtering logic, not the full API call)
  filtered_meta <- all_meta[is_derived == FALSE]
  expect_equal(nrow(filtered_meta), length(non_derived_names))

  # None of the derived series should remain
  expect_false(any(filtered_meta$series_name %in% derived_names))

  # Known derived series: taxadesocup, taxapartic, etc. should be marked
  expect_true("taxadesocup" %in% derived_names)
  expect_true("taxapartic" %in% derived_names)

  # Known non-derived series: popocup, popdesocup should not be marked
  expect_true("popocup" %in% non_derived_names)
  expect_true("popdesocup" %in% non_derived_names)
})


test_that("get_sidra_series_metadata returns expected columns and types", {
  # 1. Execute: Get full metadata
  meta <- get_sidra_series_metadata()

  # 2. Verify: Required columns exist
  expected_cols <- c("series_name", "api_path", "table_id", "variable_id",
                     "theme", "theme_category", "description",
                     "unit", "is_derived", "requires_deflation")
  for (col in expected_cols) {
    expect_true(col %in% names(meta), info = paste("Missing column:", col))
  }

  # Types should be correct
  expect_type(meta$series_name, "character")
  expect_type(meta$api_path, "character")
  expect_type(meta$table_id, "integer")
  expect_type(meta$is_derived, "logical")

  # Should have a reasonable number of series (86+ as documented)
  expect_true(nrow(meta) >= 80)

  # No duplicate series names
  expect_equal(length(unique(meta$series_name)), nrow(meta))
})


# =============================================================================
# .get_mesnotrim() HELPER TESTS
# =============================================================================

test_that(".get_mesnotrim returns correct month position in quarter", {
  # Position 1: Jan, Apr, Jul, Oct
  expect_equal(PNADCperiods:::.get_mesnotrim(1), 1)
  expect_equal(PNADCperiods:::.get_mesnotrim(4), 1)
  expect_equal(PNADCperiods:::.get_mesnotrim(7), 1)
  expect_equal(PNADCperiods:::.get_mesnotrim(10), 1)

  # Position 2: Feb, May, Aug, Nov
  expect_equal(PNADCperiods:::.get_mesnotrim(2), 2)
  expect_equal(PNADCperiods:::.get_mesnotrim(5), 2)
  expect_equal(PNADCperiods:::.get_mesnotrim(8), 2)
  expect_equal(PNADCperiods:::.get_mesnotrim(11), 2)

  # Position 3: Mar, Jun, Sep, Dec
  expect_equal(PNADCperiods:::.get_mesnotrim(3), 3)
  expect_equal(PNADCperiods:::.get_mesnotrim(6), 3)
  expect_equal(PNADCperiods:::.get_mesnotrim(9), 3)
  expect_equal(PNADCperiods:::.get_mesnotrim(12), 3)
})


# =============================================================================
# INTEGRATION TEST (requires internet + sidrar)
# =============================================================================

test_that("fetch_sidra_rolling_quarters fetches real data from API", {
  skip_if_not(requireNamespace("sidrar", quietly = TRUE),
              "sidrar package not available")
  skip_if_offline()

  # 1. Setup: Clear cache to ensure fresh fetch
  clear_sidra_cache()

  # 2. Execute: Fetch a single small series to minimize API load
  result <- tryCatch(
    fetch_sidra_rolling_quarters(
      series = "taxadesocup",
      use_cache = FALSE,
      verbose = FALSE,
      max_retries = 2
    ),
    error = function(e) {
      skip(paste("SIDRA API unavailable:", conditionMessage(e)))
    }
  )

  # 3. Verify: Should be a data.table with expected structure
  expect_s3_class(result, "data.table")
  expect_true("anomesfinaltrimmovel" %in% names(result))
  expect_true("mesnotrim" %in% names(result))
  expect_true("taxadesocup" %in% names(result))

  # Should have many observations (PNADC starts 2012)
  expect_true(nrow(result) > 100)

  # anomesfinaltrimmovel should be valid YYYYMM integers
  expect_true(all(result$anomesfinaltrimmovel >= 201201L))
  expect_true(all(result$anomesfinaltrimmovel %% 100L >= 1))
  expect_true(all(result$anomesfinaltrimmovel %% 100L <= 12))

  # mesnotrim should be 1, 2, or 3
  expect_true(all(result$mesnotrim %in% c(1L, 2L, 3L)))

  # taxadesocup should be a reasonable unemployment rate (0-50%)
  valid_values <- result$taxadesocup[!is.na(result$taxadesocup)]
  expect_true(length(valid_values) > 0)
  expect_true(all(valid_values > 0 & valid_values < 50))

  # Data should be sorted by time
  expect_equal(result$anomesfinaltrimmovel,
               sort(result$anomesfinaltrimmovel))

  # 4. Cleanup
  clear_sidra_cache()
})
