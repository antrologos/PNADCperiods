# Test NA propagation in derived series - EXTENDED WITH REAL DATA

# Helper to load real rolling quarters
load_real_rolling_quarters <- function() {
  testthat::skip_if_not_installed("qs2")
  data_path <- "d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard/data/rolling_quarters.qs2"
  if (!file.exists(data_path)) {
    skip("rolling_quarters.qs2 not found")
  }
  getNamespace("qs2")$qs_read(data_path)
}

# D1: Basic aggregates
test_that("D1: popnaforca = popocup + popdesocup - synthetic data", {
  dt <- data.table::data.table(
    anomesexato = 202301:202310,
    m_popocup = c(100, 105, 110, 108, 112, 115, 118, 120, 122, 125),
    m_popdesocup = c(10, 12, 11, 13, 9, 8, 10, 11, 12, 10)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  expect_equal(result$m_popnaforca, dt$m_popocup + dt$m_popdesocup)
})

test_that("D1.NA: popnaforca NA when any component NA", {
  dt <- data.table::data.table(
    anomesexato = c(202301, 202302, 202303, 202304),
    m_popocup = c(100, NA, 110, 108),
    m_popdesocup = c(10, 12, NA, 13)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  # Row 2: popocup NA -> aggregate NA
  expect_true(is.na(result$m_popnaforca[2]))
  # Row 3: popdesocup NA -> aggregate NA
  expect_true(is.na(result$m_popnaforca[3]))
  # Row 1: both valid -> aggregate valid
  expect_false(is.na(result$m_popnaforca[1]))
})

# D2: Rates
test_that("D2: taxadesocup rate computation", {
  dt <- data.table::data.table(
    anomesexato = 202301:202310,
    m_popocup = c(100, 105, 110, 108, 112, 115, 118, 120, 122, 125),
    m_popdesocup = c(10, 12, 11, 13, 9, 8, 10, 11, 12, 10)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  popnaforca <- dt$m_popocup + dt$m_popdesocup
  expected <- round(dt$m_popdesocup / popnaforca * 100, 1)
  expect_equal(result$m_taxadesocup, expected)
})

test_that("D2.NA: taxadesocup NA when denominator NA", {
  dt <- data.table::data.table(
    anomesexato = c(202301, 202302, 202303),
    m_popocup = c(100, 105, 110),
    m_popdesocup = c(10, NA, 11)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  # Row 2: popdesocup NA -> popnaforca NA -> taxadesocup NA
  expect_true(is.na(result$m_popnaforca[2]))
  expect_true(is.na(result$m_taxadesocup[2]) || is.nan(result$m_taxadesocup[2]))
})

# D3: Residuals
test_that("D3: residual computation", {
  dt <- data.table::data.table(
    anomesexato = 202301:202310,
    m_pop14mais = c(170, 172, 174, 176, 178, 180, 182, 184, 186, 188),
    m_popocup = c(100, 102, 104, 106, 108, 110, 112, 114, 116, 118),
    m_popdesocup = c(12, 13, 11, 14, 10, 9, 11, 12, 13, 10)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  expected <- dt$m_pop14mais - dt$m_popocup - dt$m_popdesocup
  expect_equal(result$m_popforadaforca, expected)
})

test_that("D3.NA: residual NA when any subtracted component NA", {
  dt <- data.table::data.table(
    anomesexato = c(202301, 202302, 202303),
    m_pop14mais = c(170, 172, 174),
    m_popocup = c(100, NA, 104),
    m_popdesocup = c(12, 13, NA)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  # Row 2: popocup NA -> residual NA
  expect_true(is.na(result$m_popforadaforca[2]))
  # Row 3: popdesocup NA -> residual NA
  expect_true(is.na(result$m_popforadaforca[3]))
})

# D4: Average income
test_that("D4: average income computation", {
  dt <- data.table::data.table(
    anomesexato = 202301:202310,
    m_massahabnominaltodos = c(250e6, 260e6, 255e6, 265e6, 270e6,
                               275e6, 280e6, 285e6, 290e6, 295e6),
    m_comrendtodos = c(90e3, 91e3, 92e3, 93e3, 94e3,
                       95e3, 96e3, 97e3, 98e3, 99e3)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  expected <- round(dt$m_massahabnominaltodos / dt$m_comrendtodos * 1000, 0)
  expect_equal(result$m_rendhabnominaltodos, expected)
})

test_that("D4.NA: average income NA when denominator NA", {
  dt <- data.table::data.table(
    anomesexato = c(202301, 202302, 202303),
    m_massahabnominaltodos = c(250e6, 260e6, 255e6),
    m_comrendtodos = c(90e3, NA, 92e3)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  # Row 2: denominator NA -> average income NA
  expect_true(is.na(result$m_rendhabnominaltodos[2]))
  # Row 1,3: denominator valid -> average income valid
  expect_false(is.na(result$m_rendhabnominaltodos[1]))
  expect_false(is.na(result$m_rendhabnominaltodos[3]))
})

# D5: Deflated series
test_that("D5: deflated series with IPCA", {
  ipca_values <- seq(5000, 5450, by = 50)
  dt <- data.table::data.table(
    anomesexato = 202301:202310,
    m_massahabnominaltodos = c(250e6, 260e6, 255e6, 265e6, 270e6,
                               275e6, 280e6, 285e6, 290e6, 295e6),
    m_comrendtodos = c(90e3, 91e3, 92e3, 93e3, 94e3,
                       95e3, 96e3, 97e3, 98e3, 99e3),
    ipca100dez1993 = ipca_values
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  expect_true("m_massahabtodosipcabr" %in% names(result))
  # All values should be non-NA since all sources are valid
  expect_true(sum(!is.na(result$m_massahabtodosipcabr)) == 10)
})

test_that("D5.NA: deflated series NA when nominal source NA", {
  ipca_values <- seq(5000, 5450, by = 50)
  dt <- data.table::data.table(
    anomesexato = c(202301, 202302, 202303),
    m_massahabnominaltodos = c(250e6, NA, 255e6),
    m_comrendtodos = c(90e3, 91e3, 92e3),
    ipca100dez1993 = c(5000, 5050, 5100)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))
  # Row 2: nominal source NA -> deflated NA
  expect_true(is.na(result$m_massahabtodosipcabr[2]))
  # Row 1,3: nominal source valid -> deflated valid
  expect_false(is.na(result$m_massahabtodosipcabr[1]))
  expect_false(is.na(result$m_massahabtodosipcabr[3]))
})

# D1-D5 Summary: comprehensive edge case
test_that("D1-D5: complex dependency chain with partial NA", {
  dt <- data.table::data.table(
    anomesexato = c(202301, 202302, 202303, 202304, 202305),
    m_popocup = c(100, 105, NA, 108, 112),
    m_popdesocup = c(10, NA, 11, 13, 9),
    m_pop14mais = c(170, 172, 174, 176, 178),
    m_massahabnominaltodos = c(250e6, 260e6, 255e6, 265e6, 270e6),
    m_comrendtodos = c(90e3, 91e3, NA, 93e3, 94e3),
    ipca100dez1993 = c(5000, 5050, 5100, 5150, 5200)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  # Row 1: all valid
  expect_false(is.na(result$m_popnaforca[1]))
  expect_false(is.na(result$m_taxadesocup[1]))
  expect_false(is.na(result$m_popforadaforca[1]))
  expect_false(is.na(result$m_rendhabnominaltodos[1]))

  # Row 2: popdesocup NA -> affects popnaforca, taxadesocup
  expect_true(is.na(result$m_popnaforca[2]))
  expect_true(is.na(result$m_taxadesocup[2]) || is.nan(result$m_taxadesocup[2]))
  # comrendtodos valid -> average income valid
  expect_false(is.na(result$m_rendhabnominaltodos[2]))

  # Row 3: popocup NA -> affects popnaforca
  #        comrendtodos NA -> affects average income
  expect_true(is.na(result$m_popnaforca[3]))
  expect_true(is.na(result$m_rendhabnominaltodos[3]))

  # Row 4: all valid
  expect_false(is.na(result$m_popnaforca[4]))
  expect_false(is.na(result$m_taxadesocup[4]))

  # Row 5: all valid
  expect_false(is.na(result$m_popnaforca[5]))
  expect_false(is.na(result$m_taxadesocup[5]))
})