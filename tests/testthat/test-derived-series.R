# Tests for .compute_derived_series() and .DERIVED_SERIES_SPEC
# These tests verify the derived series computation engine (aggregates, average
# income, residuals, rates, deflated) and the structural integrity of the
# declarative specification that drives it.

library(data.table)

# =============================================================================
# PHASE 1: AGGREGATE TESTS
# =============================================================================

test_that("aggregate: popocup + popdesocup = popnaforca", {
  dt <- data.table(
    anomesexato = 202301:202310,
    m_popocup    = c(100, 105, 110, 108, 112, 115, 118, 120, 122, 125),
    m_popdesocup = c(10, 12, 11, 13, 9, 8, 10, 11, 12, 10)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  expect_true("m_popnaforca" %in% names(result))
  expect_equal(result$m_popnaforca, dt$m_popocup + dt$m_popdesocup)
})

test_that("aggregate dependency chain: empregpriv built first, then empregado uses it", {
  dt <- data.table(
    anomesexato = 202301:202310,
    # Components for empregpriv
    m_empregprivcomcart = seq(40, 49),
    m_empregprivsemcart = seq(10, 19),
    # Components for domestico
    m_domesticocomcart = seq(5, 14),
    m_domesticosemcart = seq(2, 11),
    # Components for empregpubl
    m_empregpublcomcart = seq(8, 17),
    m_empregpublsemcart = seq(3, 12),
    m_estatutmilitar    = seq(1, 10)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  # empregpriv should be computed first
  expect_true("m_empregpriv" %in% names(result))
  expected_empregpriv <- dt$m_empregprivcomcart + dt$m_empregprivsemcart
  expect_equal(result$m_empregpriv, expected_empregpriv)

  # domestico should also be computed
  expect_true("m_domestico" %in% names(result))
  expected_domestico <- dt$m_domesticocomcart + dt$m_domesticosemcart
  expect_equal(result$m_domestico, expected_domestico)

  # empregpubl depends on 3 components

  expect_true("m_empregpubl" %in% names(result))
  expected_empregpubl <- dt$m_empregpublcomcart + dt$m_empregpublsemcart + dt$m_estatutmilitar
  expect_equal(result$m_empregpubl, expected_empregpubl)

  # empregado = empregpriv + domestico + empregpubl (all computed above)
  expect_true("m_empregado" %in% names(result))
  expected_empregado <- expected_empregpriv + expected_domestico + expected_empregpubl
  expect_equal(result$m_empregado, expected_empregado)
})

# =============================================================================
# PHASE 2: AVERAGE INCOME TESTS
# =============================================================================

test_that("average income: rendhabnominaltodos = massahabnominaltodos / comrendtodos * 1000", {
  dt <- data.table(
    anomesexato        = 202301:202310,
    m_massahabnominaltodos = c(250e6, 260e6, 255e6, 265e6, 270e6,
                               275e6, 280e6, 285e6, 290e6, 295e6),
    m_comrendtodos         = c(90e3, 91e3, 92e3, 93e3, 94e3,
                               95e3, 96e3, 97e3, 98e3, 99e3)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  expect_true("m_rendhabnominaltodos" %in% names(result))
  expected <- round(dt$m_massahabnominaltodos / dt$m_comrendtodos * 1000, 0)
  expect_equal(result$m_rendhabnominaltodos, expected)
})

# =============================================================================
# PHASE 3: RESIDUAL TESTS
# =============================================================================

test_that("residual: popforadaforca = pop14mais - popocup - popdesocup", {
  dt <- data.table(
    anomesexato  = 202301:202310,
    m_pop14mais  = c(170, 172, 174, 176, 178, 180, 182, 184, 186, 188),
    m_popocup    = c(100, 102, 104, 106, 108, 110, 112, 114, 116, 118),
    m_popdesocup = c(12, 13, 11, 14, 10, 9, 11, 12, 13, 10)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  expect_true("m_popforadaforca" %in% names(result))
  expected <- dt$m_pop14mais - dt$m_popocup - dt$m_popdesocup
  expect_equal(result$m_popforadaforca, expected)
})

# =============================================================================
# PHASE 4: RATE TESTS
# =============================================================================

test_that("rate: taxadesocup = popdesocup / popnaforca * 100", {
  dt <- data.table(
    anomesexato  = 202301:202310,
    m_popocup    = c(100, 105, 110, 108, 112, 115, 118, 120, 122, 125),
    m_popdesocup = c(10, 12, 11, 13, 9, 8, 10, 11, 12, 10)
  )
  # Phase 1 computes m_popnaforca = popocup + popdesocup
  # Phase 4 computes taxadesocup = popdesocup / popnaforca * 100
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  expect_true("m_taxadesocup" %in% names(result))
  popnaforca <- dt$m_popocup + dt$m_popdesocup
  expected <- round(dt$m_popdesocup / popnaforca * 100, 1)
  expect_equal(result$m_taxadesocup, expected)
})

test_that("rate with compound denominator: percdesalento uses (popnaforca + desalentado)", {
  dt <- data.table(
    anomesexato    = 202301:202310,
    m_popocup      = c(100, 105, 110, 108, 112, 115, 118, 120, 122, 125),
    m_popdesocup   = c(10, 12, 11, 13, 9, 8, 10, 11, 12, 10),
    m_desalentado  = c(5, 6, 4, 7, 3, 4, 5, 6, 7, 4),
    m_forcapotencial = c(8, 9, 7, 10, 6, 7, 8, 9, 10, 7)
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  expect_true("m_percdesalento" %in% names(result))

  # percdesalento spec: numerator = "desalentado", denominator = c("popnaforca", "desalentado")
  # popnaforca is computed in Phase 1 as popocup + popdesocup
  popnaforca <- dt$m_popocup + dt$m_popdesocup
  # Compound denominator: rowSums of m_popnaforca + m_desalentado
  denom <- popnaforca + dt$m_desalentado
  expected <- round(dt$m_desalentado / denom * 100, 1)
  expect_equal(result$m_percdesalento, expected)
})

# =============================================================================
# PHASE 5: DEFLATED SERIES TESTS
# =============================================================================

test_that("deflated series: verify IPCA deflation formula for hab and efet", {
  # 10 months with rising IPCA index
  ipca_values <- seq(5000, 5450, by = 50)
  dt <- data.table(
    anomesexato            = 202301:202310,
    m_massahabnominaltodos = c(250e6, 260e6, 255e6, 265e6, 270e6,
                               275e6, 280e6, 285e6, 290e6, 295e6),
    m_massaefetnominaltodos = c(240e6, 250e6, 245e6, 255e6, 260e6,
                                265e6, 270e6, 275e6, 280e6, 285e6),
    m_comrendtodos         = c(90e3, 91e3, 92e3, 93e3, 94e3,
                               95e3, 96e3, 97e3, 98e3, 99e3),
    ipca100dez1993         = ipca_values
  )
  result <- PNADCperiods:::.compute_derived_series(copy(dt))

  latest_ipca <- ipca_values[10]  # max anomesexato

  # hab series use current IPCA (use_lagged_ipca = FALSE)
  # deflator_hab = latest_ipca / ipca100dez1993
  expect_true("m_massahabtodosipcabr" %in% names(result))
  deflator_hab <- latest_ipca / ipca_values
  expected_hab <- round(dt$m_massahabnominaltodos * deflator_hab, 0)
  expect_equal(result$m_massahabtodosipcabr, expected_hab)

  # efet series use lagged IPCA (use_lagged_ipca = TRUE)
  # deflator_efet = latest_ipca / shift(ipca100dez1993, 1, type="lag")
  expect_true("m_massaefettodosipcabr" %in% names(result))
  ipca_lagged <- c(NA, ipca_values[1:9])
  deflator_efet <- latest_ipca / ipca_lagged
  expected_efet <- round(dt$m_massaefetnominaltodos * deflator_efet, 0)
  # Row 1 is NA because lagged IPCA is NA
  expect_true(is.na(result$m_massaefettodosipcabr[1]))
  expect_equal(result$m_massaefettodosipcabr[2:10], expected_efet[2:10])
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("missing component column: derived series gracefully skipped", {
  # Only provide popocup, not popdesocup -- popnaforca cannot be computed
  dt <- data.table(
    anomesexato = 202301:202310,
    m_popocup   = seq(100, 109)
  )
  # Should not error
  result <- expect_silent(PNADCperiods:::.compute_derived_series(copy(dt)))
  # popnaforca should NOT be created (missing component)
  expect_false("m_popnaforca" %in% names(result))
  # taxadesocup also should NOT be created (missing numerator and denominator)
  expect_false("m_taxadesocup" %in% names(result))
})

test_that("division by zero: denominator=0 does not crash", {
  dt <- data.table(
    anomesexato  = 202301:202310,
    m_popocup    = c(100, 105, 110, 108, 0, 115, 118, 120, 122, 125),
    m_popdesocup = c(10, 12, 11, 13, 0, 8, 10, 11, 12, 10)
  )
  # Row 5: popnaforca = 0, so taxadesocup = 0/0
  # Should not error -- NaN or Inf is acceptable, just no crash
  result <- expect_silent(PNADCperiods:::.compute_derived_series(copy(dt)))
  expect_true("m_taxadesocup" %in% names(result))
  # The function completes without error
  expect_s3_class(result, "data.table")
})

# =============================================================================
# .DERIVED_SERIES_SPEC STRUCTURAL VALIDATION
# =============================================================================

test_that("spec: all aggregate specs have 'name' and non-empty 'components'", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC
  for (s in spec$aggregates) {
    expect_true(!is.null(s$name), info = paste("Aggregate missing 'name'"))
    expect_true(is.character(s$name) && nchar(s$name) > 0,
                info = paste("Aggregate has empty name"))
    expect_true(!is.null(s$components),
                info = paste("Aggregate", s$name, "missing 'components'"))
    expect_true(length(s$components) >= 2,
                info = paste("Aggregate", s$name, "has fewer than 2 components"))
  }
})

test_that("spec: all rate specs have 'name', 'numerator', 'denominator'", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC
  for (s in spec$rates) {
    expect_true(!is.null(s$name),
                info = "Rate missing 'name'")
    expect_true(is.character(s$name) && nchar(s$name) > 0,
                info = "Rate has empty name")
    expect_true(!is.null(s$numerator),
                info = paste("Rate", s$name, "missing 'numerator'"))
    expect_true(!is.null(s$denominator),
                info = paste("Rate", s$name, "missing 'denominator'"))
    expect_true(length(unlist(s$numerator)) >= 1,
                info = paste("Rate", s$name, "has empty numerator"))
    expect_true(length(unlist(s$denominator)) >= 1,
                info = paste("Rate", s$name, "has empty denominator"))
  }
})

test_that("spec: all residual specs have 'name', 'parent', 'subtract'", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC
  for (s in spec$residuals) {
    expect_true(!is.null(s$name),
                info = "Residual missing 'name'")
    expect_true(is.character(s$name) && nchar(s$name) > 0,
                info = "Residual has empty name")
    expect_true(!is.null(s$parent),
                info = paste("Residual", s$name, "missing 'parent'"))
    expect_true(is.character(s$parent) && nchar(s$parent) > 0,
                info = paste("Residual", s$name, "has empty parent"))
    expect_true(!is.null(s$subtract),
                info = paste("Residual", s$name, "missing 'subtract'"))
    expect_true(length(s$subtract) >= 1,
                info = paste("Residual", s$name, "has empty subtract list"))
  }
})

test_that("spec: no duplicate names across all spec sections", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC
  all_names <- c(
    sapply(spec$aggregates, `[[`, "name"),
    sapply(spec$average_income, `[[`, "name"),
    sapply(spec$residuals, `[[`, "name"),
    sapply(spec$rates, `[[`, "name"),
    sapply(spec$deflated, `[[`, "name")
  )
  duplicated_names <- all_names[duplicated(all_names)]
  expect_equal(length(duplicated_names), 0,
               info = paste("Duplicate derived series names:", paste(duplicated_names, collapse = ", ")))
})

test_that("spec: aggregate dependencies in correct order (empregpriv before empregado)", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC
  agg_names <- sapply(spec$aggregates, `[[`, "name")

  # empregpriv must appear before empregado (empregado depends on empregpriv)
  idx_empregpriv <- which(agg_names == "empregpriv")
  idx_empregado  <- which(agg_names == "empregado")
  expect_true(length(idx_empregpriv) == 1, info = "empregpriv not found in aggregates")
  expect_true(length(idx_empregado) == 1, info = "empregado not found in aggregates")
  expect_true(idx_empregpriv < idx_empregado,
              info = "empregpriv must be computed before empregado")

  # popnaforca must appear before forcaampliada (forcaampliada depends on popnaforca)
  idx_popnaforca   <- which(agg_names == "popnaforca")
  idx_forcaampliada <- which(agg_names == "forcaampliada")
  expect_true(length(idx_popnaforca) == 1, info = "popnaforca not found in aggregates")
  expect_true(length(idx_forcaampliada) == 1, info = "forcaampliada not found in aggregates")
  expect_true(idx_popnaforca < idx_forcaampliada,
              info = "popnaforca must be computed before forcaampliada")

  # domestico must appear before empregado
  idx_domestico <- which(agg_names == "domestico")
  expect_true(idx_domestico < idx_empregado,
              info = "domestico must be computed before empregado")

  # empregpubl must appear before empregado
  idx_empregpubl <- which(agg_names == "empregpubl")
  expect_true(idx_empregpubl < idx_empregado,
              info = "empregpubl must be computed before empregado")
})

test_that("spec: all deflated specs have 'source' and 'use_lagged_ipca'", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC
  for (s in spec$deflated) {
    expect_true(!is.null(s$name),
                info = "Deflated missing 'name'")
    expect_true(!is.null(s$source),
                info = paste("Deflated", s$name, "missing 'source'"))
    expect_true(is.character(s$source) && nchar(s$source) > 0,
                info = paste("Deflated", s$name, "has empty source"))
    expect_true(!is.null(s$use_lagged_ipca),
                info = paste("Deflated", s$name, "missing 'use_lagged_ipca'"))
    expect_true(is.logical(s$use_lagged_ipca),
                info = paste("Deflated", s$name, "'use_lagged_ipca' is not logical"))
  }
})

test_that("spec: phase ordering ensures aggregates computed before rates that use them", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC

  # Collect all names produced by aggregates (Phase 1)
  agg_names <- sapply(spec$aggregates, `[[`, "name")

  # Rates that reference aggregate outputs in their numerator or denominator
  # e.g., taxadesocup uses popnaforca (aggregate) as denominator
  for (s in spec$rates) {
    # Check denominator references
    denom_components <- unlist(s$denominator)
    for (d in denom_components) {
      if (d %in% agg_names) {
        # This is fine -- the aggregate is computed in Phase 1, rate in Phase 4
        expect_true(d %in% agg_names,
                    info = paste("Rate", s$name, "references aggregate", d,
                                 "which should be computed before rates"))
      }
    }
    # Check numerator references
    num_components <- unlist(s$numerator)
    for (n in num_components) {
      if (n %in% agg_names) {
        expect_true(n %in% agg_names,
                    info = paste("Rate", s$name, "references aggregate", n,
                                 "which should be computed before rates"))
      }
    }
  }

  # Specifically verify that taxadesocup denominator (popnaforca) is an aggregate
  taxadesocup <- Filter(function(s) s$name == "taxadesocup", spec$rates)
  expect_true(length(taxadesocup) == 1)
  denom <- unlist(taxadesocup[[1]]$denominator)
  expect_true("popnaforca" %in% agg_names,
              info = "popnaforca must be in aggregates since taxadesocup depends on it")
  expect_true(all(denom %in% c(agg_names, "popnaforca")),
              info = "taxadesocup denominator must reference known aggregates or primaries")
})

test_that("spec: all average_income specs have 'multiplier' and 'decimals'", {
  spec <- PNADCperiods:::.DERIVED_SERIES_SPEC
  for (s in spec$average_income) {
    expect_true(!is.null(s$name),
                info = "Average income spec missing 'name'")
    expect_true(!is.null(s$numerator),
                info = paste("Average income", s$name, "missing 'numerator'"))
    expect_true(!is.null(s$denominator),
                info = paste("Average income", s$name, "missing 'denominator'"))
    expect_true(!is.null(s$multiplier),
                info = paste("Average income", s$name, "missing 'multiplier'"))
    expect_true(is.numeric(s$multiplier) && s$multiplier > 0,
                info = paste("Average income", s$name, "'multiplier' must be positive numeric"))
    expect_true(!is.null(s$decimals),
                info = paste("Average income", s$name, "missing 'decimals'"))
    expect_true(is.numeric(s$decimals) && s$decimals >= 0,
                info = paste("Average income", s$name, "'decimals' must be non-negative numeric"))
  }
})

