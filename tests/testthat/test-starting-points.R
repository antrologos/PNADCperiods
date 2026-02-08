# Tests for starting points functions and bundled dataset
# Covers: pnadc_series_starting_points (bundled data),
#          compute_z_aggregates(), compute_series_starting_points()

# =============================================================================
# BUNDLED DATASET TESTS (pnadc_series_starting_points)
# =============================================================================

test_that("bundled dataset loads successfully via data()", {
  data("pnadc_series_starting_points", package = "PNADCperiods")
  expect_s3_class(pnadc_series_starting_points, "data.table")
  expect_true(nrow(pnadc_series_starting_points) > 0,
              label = "Dataset has rows")
})

test_that("bundled dataset has exactly 3 columns: series_name, mesnotrim, y0", {
  data("pnadc_series_starting_points", package = "PNADCperiods")
  expect_equal(ncol(pnadc_series_starting_points), 3)
  expect_named(pnadc_series_starting_points,
               c("series_name", "mesnotrim", "y0"),
               ignore.order = FALSE)
})

test_that("mesnotrim only contains values 1, 2, 3", {
  data("pnadc_series_starting_points", package = "PNADCperiods")
  expect_true(all(pnadc_series_starting_points$mesnotrim %in% c(1L, 2L, 3L)),
              label = "All mesnotrim values are 1, 2, or 3")
  # Confirm all three positions are present

  expect_setequal(unique(pnadc_series_starting_points$mesnotrim), c(1L, 2L, 3L))
})

test_that("no NA in series_name or mesnotrim", {
  data("pnadc_series_starting_points", package = "PNADCperiods")
  expect_false(any(is.na(pnadc_series_starting_points$series_name)),
               label = "No NA in series_name")
  expect_false(any(is.na(pnadc_series_starting_points$mesnotrim)),
               label = "No NA in mesnotrim")
})

test_that("all y0 values are finite (no NA, NaN, Inf)", {
  data("pnadc_series_starting_points", package = "PNADCperiods")
  expect_true(all(is.finite(pnadc_series_starting_points$y0)),
              label = "All y0 values are finite")
})

test_that("key labor market series are present in bundled dataset", {
  data("pnadc_series_starting_points", package = "PNADCperiods")
  available <- unique(pnadc_series_starting_points$series_name)

  key_series <- c("popocup", "popdesocup", "popnaforca", "pop14mais")
  for (s in key_series) {
    expect_true(s %in% available,
                label = paste0("Series '", s, "' is present"))
  }
})

test_that("each series has exactly 3 rows (one per mesnotrim)", {
  data("pnadc_series_starting_points", package = "PNADCperiods")
  counts <- pnadc_series_starting_points[, .N, by = series_name]

  expect_true(all(counts$N == 3L),
              label = "Every series has exactly 3 rows")

  # Verify each series has all three positions
  for (s in unique(pnadc_series_starting_points$series_name)) {
    positions <- pnadc_series_starting_points[series_name == s, mesnotrim]
    expect_setequal(positions, c(1L, 2L, 3L))
  }
})

# =============================================================================
# compute_z_aggregates() TESTS
# =============================================================================

# Helper: create mock calibrated microdata for compute_z_aggregates()
create_mock_calibrated <- function(n_per_month = 10L,
                                    months = c(202301L, 202302L, 202303L),
                                    seed = 100L) {
  set.seed(seed)
  n <- n_per_month * length(months)

  dt <- data.table::data.table(
    weight_monthly = rep(1000, n),
    ref_month_yyyymm = rep(months, each = n_per_month),
    ref_month_in_quarter = rep(1:3, each = n_per_month),
    Ano = 2023L,
    Trimestre = 1L,
    V2009 = sample(c(5L, 20L, 35L, 50L, 70L), n, replace = TRUE),
    VD4001 = sample(c(1L, 2L), n, replace = TRUE),
    VD4002 = sample(c(1L, 2L, NA_integer_), n, replace = TRUE, prob = c(0.45, 0.15, 0.40)),
    VD4009 = sample(c(1L:10L, NA_integer_), n, replace = TRUE),
    VD4010 = sample(c(1L:11L, NA_integer_), n, replace = TRUE),
    V4019 = sample(c(1L, 2L, NA_integer_), n, replace = TRUE),
    VD4004 = sample(c(1L, 2L, NA_integer_), n, replace = TRUE),
    VD4004A = sample(c(1L, 2L, NA_integer_), n, replace = TRUE),
    VD4019 = round(runif(n, 1000, 5000), 2),
    VD4020 = round(runif(n, 900, 4500), 2),
    VD4016 = round(runif(n, 500, 3000), 2),
    VD4017 = round(runif(n, 500, 3000), 2),
    VD4031 = round(runif(n, 20, 44), 0),
    VD4035 = round(runif(n, 20, 50), 0)
  )
  dt
}


test_that("compute_z_aggregates errors when weight_monthly is missing", {
  dt <- data.table::data.table(
    ref_month_yyyymm = c(202301L, 202302L),
    ref_month_in_quarter = c(1L, 2L),
    V2009 = c(30L, 40L)
  )
  expect_error(
    compute_z_aggregates(dt, verbose = FALSE),
    "weight_monthly"
  )
})

test_that("compute_z_aggregates computes correct z_popocup and z_popdesocup", {
  # Construct a deterministic example:
  # 4 obs in one month, weights all 1000
  # VD4002: 1, 1, 2, 2  -> 2 occupied, 2 unemployed
  dt <- data.table::data.table(
    weight_monthly = rep(1000, 4),
    ref_month_yyyymm = rep(202301L, 4),
    ref_month_in_quarter = rep(1L, 4),
    Ano = 2023L,
    Trimestre = 1L,
    V2009 = c(30L, 35L, 25L, 40L),
    VD4001 = c(1L, 1L, 1L, 1L),
    VD4002 = c(1L, 1L, 2L, 2L)
  )

  result <- compute_z_aggregates(dt, verbose = FALSE)

  # z_popocup = sum(indicator * weight): 2 obs with VD4002==1, weight=1000 each
  expect_equal(result$z_popocup, 2 * 1000)
  # z_popdesocup = sum(indicator * weight): 2 obs with VD4002==2
  expect_equal(result$z_popdesocup, 2 * 1000)
})

test_that("z_populacao equals 1 for every observation (total pop = sum of weights)", {
  # 6 obs across 2 months, various weights
  dt <- data.table::data.table(
    weight_monthly = c(500, 800, 1200, 600, 900, 1100),
    ref_month_yyyymm = c(rep(202301L, 3), rep(202302L, 3)),
    ref_month_in_quarter = c(rep(1L, 3), rep(2L, 3)),
    Ano = 2023L,
    Trimestre = 1L,
    V2009 = c(30L, 35L, 25L, 40L, 50L, 20L)
  )

  result <- compute_z_aggregates(dt, verbose = FALSE)
  data.table::setorder(result, anomesexato)

  # z_populacao should be sum of weights per month
  # Month 1: 500 + 800 + 1200 = 2500
  # Month 2: 600 + 900 + 1100 = 2600
  expect_equal(result[anomesexato == 202301L, z_populacao], 2500)
  expect_equal(result[anomesexato == 202302L, z_populacao], 2600)
})

test_that("compute_z_aggregates groups correctly by month across multiple months", {
  dt <- create_mock_calibrated(n_per_month = 20L)
  result <- compute_z_aggregates(dt, verbose = FALSE)

  # Should have one row per month
  expect_equal(nrow(result), 3)
  expect_true(all(c(202301L, 202302L, 202303L) %in% result$anomesexato))

  # All z_ columns should be present and numeric
  z_cols <- grep("^z_", names(result), value = TRUE)
  expect_true(length(z_cols) > 0, label = "z_ columns exist")
  for (col in z_cols) {
    expect_true(is.numeric(result[[col]]),
                label = paste0(col, " is numeric"))
  }
})

test_that("compute_z_aggregates converts character VD variables to integer", {
  # VD variables stored as character in some PNADC files ("01", "02")
  dt <- data.table::data.table(
    weight_monthly = rep(1000, 4),
    ref_month_yyyymm = rep(202301L, 4),
    ref_month_in_quarter = rep(1L, 4),
    Ano = 2023L,
    Trimestre = 1L,
    V2009 = c(30L, 35L, 25L, 40L),
    VD4001 = c("01", "01", "02", "02"),
    VD4002 = c("01", "01", "02", "02")
  )

  # Should not error, and should produce correct results
  result <- compute_z_aggregates(dt, verbose = FALSE)
  expect_equal(result$z_popocup, 2 * 1000)
  expect_equal(result$z_popdesocup, 2 * 1000)
})

# =============================================================================
# compute_series_starting_points() TESTS
# =============================================================================

# Helper: create paired mock data for compute_series_starting_points()
# Returns a list with $monthly_estimates and $rolling_quarters
create_mock_for_starting_points <- function(n_months = 36L,
                                             start_yyyymm = 201301L,
                                             base_rq = 100,
                                             scale_factor = 1000,
                                             seed = 200L) {
  set.seed(seed)

  # Generate YYYYMM sequence
  months_seq <- generate_yyyymm_seq(start_yyyymm, n_months)
  mesnotrim_seq <- ((months_seq %% 100L - 1L) %% 3L) + 1L

  # Rolling quarters: base value with slight trend
  rq_values <- base_rq * (1 + 0.001 * seq_len(n_months))

  rolling_quarters <- data.table::data.table(
    anomesfinaltrimmovel = months_seq,
    mesnotrim = mesnotrim_seq,
    popocup = rq_values,
    popdesocup = rq_values * 0.1,
    pop14mais = rq_values * 1.2
  )

  # Monthly estimates: z_ columns = scale_factor * rq + noise
  # (so that z/scale_factor is close to rq, giving small y0)
  monthly_estimates <- data.table::data.table(
    anomesexato = months_seq,
    z_popocup = rq_values * scale_factor + rnorm(n_months, 0, 50),
    z_popdesocup = rq_values * 0.1 * scale_factor + rnorm(n_months, 0, 5),
    z_pop14mais = rq_values * 1.2 * scale_factor + rnorm(n_months, 0, 60)
  )

  list(
    monthly_estimates = monthly_estimates,
    rolling_quarters = rolling_quarters
  )
}


test_that("compute_series_starting_points returns correct columns", {
  mock <- create_mock_for_starting_points()
  result <- compute_series_starting_points(
    mock$monthly_estimates,
    mock$rolling_quarters,
    calibration_start = 201301L,
    calibration_end = 201912L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_named(result, c("series_name", "mesnotrim", "y0"), ignore.order = FALSE)
})

test_that("compute_series_starting_points returns 3 rows per series", {
  mock <- create_mock_for_starting_points()
  result <- compute_series_starting_points(
    mock$monthly_estimates,
    mock$rolling_quarters,
    calibration_start = 201301L,
    calibration_end = 201912L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  counts <- result[, .N, by = series_name]
  expect_true(all(counts$N == 3L),
              label = "Each series has exactly 3 rows")

  # Each series should have all 3 mesnotrim values
  for (s in unique(result$series_name)) {
    positions <- result[series_name == s, mesnotrim]
    expect_setequal(positions, c(1L, 2L, 3L))
  }
})

test_that("custom calibration_start/end are respected", {
  # Create data spanning a wide range
  mock <- create_mock_for_starting_points(n_months = 120L, start_yyyymm = 201201L)

  # Use narrow calibration period
  result_narrow <- compute_series_starting_points(
    mock$monthly_estimates,
    mock$rolling_quarters,
    calibration_start = 201601L,
    calibration_end = 201712L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  # Use wider calibration period
  result_wide <- compute_series_starting_points(
    mock$monthly_estimates,
    mock$rolling_quarters,
    calibration_start = 201301L,
    calibration_end = 201912L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  # Both should produce valid results
  expect_true(all(is.finite(result_narrow$y0)))
  expect_true(all(is.finite(result_wide$y0)))

  # y0 values should differ because calibration periods differ
  merged <- merge(result_narrow, result_wide,
                  by = c("series_name", "mesnotrim"),
                  suffixes = c("_narrow", "_wide"))
  # Not all y0 pairs should be identical (noise + different averaging window)
  expect_false(all(merged$y0_narrow == merged$y0_wide),
               label = "Different calibration periods produce different y0")
})

test_that("use_series_specific_periods=FALSE uses uniform calibration for all series", {
  # Create data with a "CNPJ-like" series name to verify it does NOT get special treatment
  months_seq <- generate_yyyymm_seq(201201L, 120)
  mesnotrim_seq <- ((months_seq %% 100L - 1L) %% 3L) + 1L

  rq_vals <- 100 * (1 + 0.001 * seq_along(months_seq))

  rq <- data.table::data.table(
    anomesfinaltrimmovel = months_seq,
    mesnotrim = mesnotrim_seq,
    popocup = rq_vals,
    empregadorcomcnpj = rq_vals * 0.05
  )

  me <- data.table::data.table(
    anomesexato = months_seq,
    z_popocup = rq_vals * 1000 + rnorm(120, 0, 10),
    z_empregadorcomcnpj = rq_vals * 0.05 * 1000 + rnorm(120, 0, 1)
  )

  result <- compute_series_starting_points(
    me, rq,
    calibration_start = 201301L,
    calibration_end = 201912L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  # Both series should be present with 3 rows each
  expect_true("popocup" %in% result$series_name)
  expect_true("empregadorcomcnpj" %in% result$series_name)
  expect_equal(nrow(result[series_name == "empregadorcomcnpj"]), 3)
})

test_that("scale_factor=1 produces correct y0 (no scaling applied)", {
  mock <- create_mock_for_starting_points(scale_factor = 1)

  # Rebuild monthly estimates with scale_factor=1 (z_ values close to rq)
  months_seq <- mock$rolling_quarters$anomesfinaltrimmovel
  rq_popocup <- mock$rolling_quarters$popocup

  me <- data.table::data.table(
    anomesexato = months_seq,
    z_popocup = rq_popocup + rnorm(length(months_seq), 0, 0.1)
  )

  result <- compute_series_starting_points(
    me,
    mock$rolling_quarters,
    calibration_start = 201301L,
    calibration_end = 201912L,
    scale_factor = 1,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  # With scale_factor=1 and z ~ rq, y0 should be close to the base rq value
  # (y0 is the starting level, not zero; ~100 for base_rq=100)
  expect_true(all(is.finite(result$y0)))
  # The three y0 values per mesnotrim should be consistent with each other
  y0_vals <- result[series_name == "popocup", y0]
  expect_true(sd(y0_vals) < 5,
              label = "y0 values consistent across mesnotrim positions")
})

test_that("mathematical property: backprojection e0 averaged gives y0", {
  # The defining formula:
  #   e0 = z / scale_factor - cumsum_by_mesnotrim(rq)
  #   y0[pos] = mean(e0[mesnotrim == pos & in_calibration])
  #
  # We verify this by constructing data where we can compute e0 manually.

  set.seed(42)
  n <- 12L  # 12 months = 4 quarters
  months_seq <- generate_yyyymm_seq(201301L, n)
  mesnotrim_seq <- ((months_seq %% 100L - 1L) %% 3L) + 1L

  rq_vals <- rep(100, n)  # Constant rolling quarter
  z_vals <- rep(100000, n)  # Constant z values (scale 1000)
  scale_factor <- 1000

  rq <- data.table::data.table(
    anomesfinaltrimmovel = months_seq,
    mesnotrim = mesnotrim_seq,
    popocup = rq_vals
  )
  me <- data.table::data.table(
    anomesexato = months_seq,
    z_popocup = z_vals
  )

  result <- compute_series_starting_points(
    me, rq,
    calibration_start = 201301L,
    calibration_end = 201312L,
    scale_factor = scale_factor,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  # With constant rq, d3 = 3*(rq - lag(rq)) = 0, so cumsum = 0 for all periods
  # e0 = z/scale_factor - cum = 100000/1000 - 0 = 100
  # y0 should be 100 for all positions
  expect_equal(result[series_name == "popocup", y0],
               rep(100, 3),
               tolerance = 0.001)
})

test_that("compute_series_starting_points errors when no z_ columns present", {
  months_seq <- generate_yyyymm_seq(201301L, 12)
  mesnotrim_seq <- ((months_seq %% 100L - 1L) %% 3L) + 1L

  rq <- data.table::data.table(
    anomesfinaltrimmovel = months_seq,
    mesnotrim = mesnotrim_seq,
    popocup = rep(100, 12)
  )
  # monthly_estimates with NO z_ columns
  me <- data.table::data.table(
    anomesexato = months_seq,
    popocup = rep(100000, 12)
  )

  expect_error(
    compute_series_starting_points(me, rq, verbose = FALSE),
    "No z_ columns"
  )
})

test_that("compute_series_starting_points skips series not in rolling_quarters", {
  months_seq <- generate_yyyymm_seq(201301L, 12)
  mesnotrim_seq <- ((months_seq %% 100L - 1L) %% 3L) + 1L

  rq <- data.table::data.table(
    anomesfinaltrimmovel = months_seq,
    mesnotrim = mesnotrim_seq,
    popocup = rep(100, 12)
    # NOTE: no "popdesocup" column here
  )
  me <- data.table::data.table(
    anomesexato = months_seq,
    z_popocup = rep(100000, 12),
    z_popdesocup = rep(10000, 12)  # z_ column exists but series not in rq
  )

  result <- compute_series_starting_points(
    me, rq,
    calibration_start = 201301L,
    calibration_end = 201312L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  # Only popocup should appear (popdesocup skipped because not in rq)
  expect_true("popocup" %in% result$series_name)
  expect_false("popdesocup" %in% result$series_name)
})

test_that("compute_series_starting_points handles anomesexato column name in monthly_estimates", {
  # The function renames "anomesexato" to "anomesfinaltrimmovel" internally
  months_seq <- generate_yyyymm_seq(201301L, 12)
  mesnotrim_seq <- ((months_seq %% 100L - 1L) %% 3L) + 1L

  rq <- data.table::data.table(
    anomesfinaltrimmovel = months_seq,
    mesnotrim = mesnotrim_seq,
    popocup = rep(100, 12)
  )
  me <- data.table::data.table(
    anomesexato = months_seq,  # Using anomesexato, not anomesfinaltrimmovel
    z_popocup = rep(100000, 12)
  )

  # Should not error
  result <- compute_series_starting_points(
    me, rq,
    calibration_start = 201301L,
    calibration_end = 201312L,
    scale_factor = 1000,
    verbose = FALSE
  )
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("non-finite y0 values are replaced with 0", {
  # If all z_ values are NA for a calibration period, e0 would be NaN
  # The function should replace non-finite y0 with 0
  months_seq <- generate_yyyymm_seq(201301L, 12)
  mesnotrim_seq <- ((months_seq %% 100L - 1L) %% 3L) + 1L

  rq <- data.table::data.table(
    anomesfinaltrimmovel = months_seq,
    mesnotrim = mesnotrim_seq,
    popocup = rep(100, 12)
  )

  # z_ values are all NA
  me <- data.table::data.table(
    anomesexato = months_seq,
    z_popocup = rep(NA_real_, 12)
  )

  result <- compute_series_starting_points(
    me, rq,
    calibration_start = 201301L,
    calibration_end = 201312L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  # All y0 should be 0 (non-finite replaced)
  expect_true(all(result$y0 == 0),
              label = "Non-finite y0 replaced with 0")
})

# =============================================================================
# INTEGRATION: compute_z_aggregates() -> compute_series_starting_points()
# =============================================================================

test_that("z_aggregates output is compatible with compute_series_starting_points input", {
  # Build calibrated mock data -> compute z_aggregates -> feed into starting points
  dt <- create_mock_calibrated(n_per_month = 30L,
                                months = generate_yyyymm_seq(201301L, 12))
  z_agg <- compute_z_aggregates(dt, verbose = FALSE)

  # Verify z_aggregates has anomesexato column

  expect_true("anomesexato" %in% names(z_agg))

  # Create matching rolling quarter data with same series
  z_cols <- grep("^z_", names(z_agg), value = TRUE)
  series_names <- sub("^z_", "", z_cols)

  rq <- data.table::data.table(
    anomesfinaltrimmovel = z_agg$anomesexato,
    mesnotrim = ((z_agg$anomesexato %% 100L - 1L) %% 3L) + 1L
  )
  # Add matching series columns with plausible rolling quarter values
  for (s in series_names) {
    z_col <- paste0("z_", s)
    if (z_col %in% names(z_agg)) {
      rq[, (s) := z_agg[[z_col]] / 1000]
    }
  }

  # Should not error
  result <- compute_series_starting_points(
    z_agg, rq,
    calibration_start = 201301L,
    calibration_end = 201312L,
    scale_factor = 1000,
    use_series_specific_periods = FALSE,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0, label = "Starting points computed from z_aggregates")
  expect_true(all(is.finite(result$y0)),
              label = "All y0 from pipeline are finite")
})

test_that("compute_z_aggregates filters out zero and NA weights", {
  dt <- data.table::data.table(
    weight_monthly = c(1000, 0, NA_real_, 1000),
    ref_month_yyyymm = rep(202301L, 4),
    ref_month_in_quarter = rep(1L, 4),
    Ano = 2023L,
    Trimestre = 1L,
    V2009 = c(30L, 35L, 25L, 40L)
  )

  result <- compute_z_aggregates(dt, verbose = FALSE)

  # z_populacao should only count obs with valid weight (2 obs with weight=1000)
  expect_equal(result$z_populacao, 2 * 1000)
})

test_that("compute_z_aggregates errors when all weights are zero/NA", {
  dt <- data.table::data.table(
    weight_monthly = c(0, NA_real_, 0),
    ref_month_yyyymm = rep(202301L, 3),
    ref_month_in_quarter = rep(1L, 3),
    Ano = 2023L,
    Trimestre = 1L,
    V2009 = c(30L, 35L, 25L)
  )

  expect_error(
    compute_z_aggregates(dt, verbose = FALSE),
    "No observations with valid weight_monthly"
  )
})

test_that("compute_z_aggregates produces z_pop14mais only for age >= 14", {
  dt <- data.table::data.table(
    weight_monthly = rep(1000, 6),
    ref_month_yyyymm = rep(202301L, 6),
    ref_month_in_quarter = rep(1L, 6),
    Ano = 2023L,
    Trimestre = 1L,
    V2009 = c(5L, 10L, 13L, 14L, 30L, 65L)  # 3 under 14, 3 at/above 14
  )

  result <- compute_z_aggregates(dt, verbose = FALSE)

  # z_pop14mais: only ages 14, 30, 65 -> 3 obs * 1000 weight
  expect_equal(result$z_pop14mais, 3 * 1000)
  # z_populacao: all 6 obs
  expect_equal(result$z_populacao, 6 * 1000)
})
