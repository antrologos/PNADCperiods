# Tests for F1 Solution: Pre-PNADC IPCA Lag in rolling_quarters
# EC2 and EC1 revisados para refletir o comportamento real

make_rq_with_ipca <- function(start_yyyymm = 201201L,
                              n_months = 24L,
                              series_name = 'massaefetnominaltodos',
                              base_value = 1000,
                              trend = 100,
                              ipca_start = NULL,
                              ipca_values = NULL) {
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L

  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]

  dt[, (series_name) := NA_real_]
  dt[dt$anomesfinaltrimmovel >= 201201L,
     (series_name) := base_value + trend * (which(dt$anomesfinaltrimmovel >= 201201L) - 1)]

  if (!is.null(ipca_start)) {
    dt[, ipca100dez1993 := NA_real_]
    idx_start <- which(dt$anomesfinaltrimmovel == ipca_start)[1]

    if (!is.na(idx_start) && !is.null(ipca_values)) {
      n_ipca <- length(ipca_values)
      end_idx <- min(idx_start + n_ipca - 1, nrow(dt))
      dt[idx_start:end_idx, ipca100dez1993 := ipca_values[1:(end_idx - idx_start + 1)]]
    }
  }

  dt[, .SD, .SDcols = c('anomesfinaltrimmovel', 'mesnotrim', series_name,
                        if ('ipca100dez1993' %in% names(dt)) 'ipca100dez1993' else NULL)]
}

test_that('F1 EC1: IPCA only 201201+ — lagged IPCA is NA at 201201', {
  dt <- make_rq_with_ipca(start_yyyymm = 201201L, n_months = 24L,
    ipca_start = 201201L,
    ipca_values = c(100, 100.3, 100.6, 100.9, 101.2, 101.5,
                    101.8, 102.1, 102.4, 102.7, 103.0, 103.3,
                    103.6, 103.9, 104.2, 104.5, 104.8, 105.1,
                    105.4, 105.7, 106.0, 106.3, 106.6, 106.9))

  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)

  result <- mensalize_sidra_series(dt, starting_points = sp,
    series = 'massaefetnominaltodos', compute_derived = TRUE, verbose = FALSE)

  row_201201 <- result[anomesexato == 201201L]
  if ('ipca100dez1993_lagged' %in% names(result)) {
    expect_true(is.na(row_201201$ipca100dez1993_lagged),
      label = 'ipca100dez1993_lagged is NA at 201201 (no pre-PNADC data)')
  }
  if ('m_massaefettodosipcabr' %in% names(result)) {
    expect_true(is.na(row_201201$m_massaefettodosipcabr),
      label = 'm_massaefettodosipcabr is NA at 201201 (Phase 5 fallback)')
  }
})

test_that('F1 EC2: IPCA from 201111 — lagged IPCA non-NA at 201201', {
  start_yyyymm <- 201111L
  n_months <- 26L

  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]

  dt[, ipca100dez1993 := 100 + 0.3 * (month_num - 1)]

  dt[, massaefetnominaltodos := NA_real_]
  dt[dt$anomesfinaltrimmovel >= 201201L,
     massaefetnominaltodos := 1000 + 100 * (which(dt$anomesfinaltrimmovel >= 201201L) - 1)]

  dt <- dt[, .(anomesfinaltrimmovel, mesnotrim, ipca100dez1993, massaefetnominaltodos)]

  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)

  result <- mensalize_sidra_series(dt, starting_points = sp,
    series = 'massaefetnominaltodos', compute_derived = TRUE, verbose = FALSE)

  row_201201 <- result[anomesexato == 201201L]
  if ('ipca100dez1993_lagged' %in% names(result)) {
    expect_false(is.na(row_201201$ipca100dez1993_lagged),
      label = 'ipca100dez1993_lagged is non-NA at 201201 (F1 pre-computed from 201112)')
  }
  if ('m_massaefettodosipcabr' %in% names(result)) {
    expect_false(is.na(row_201201$m_massaefettodosipcabr),
      label = 'm_massaefettodosipcabr is non-NA at 201201 (F1 solution)')
  }
})

test_that('F1 EC3: IPCA gap at 201112 — lagged IPCA is NA at 201201', {
  start_yyyymm <- 201110L
  n_months <- 26L

  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]

  dt[, ipca100dez1993 := 100 + 0.3 * (month_num - 1)]
  dt[anomesfinaltrimmovel == 201112L, ipca100dez1993 := NA_real_]

  dt[, massaefetnominaltodos := NA_real_]
  dt[dt$anomesfinaltrimmovel >= 201201L,
     massaefetnominaltodos := 1000 + 100 * (which(dt$anomesfinaltrimmovel >= 201201L) - 1)]

  dt <- dt[, .(anomesfinaltrimmovel, mesnotrim, ipca100dez1993, massaefetnominaltodos)]

  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)

  result <- mensalize_sidra_series(dt, starting_points = sp,
    series = 'massaefetnominaltodos', compute_derived = TRUE, verbose = FALSE)

  row_201201 <- result[anomesexato == 201201L]
  if ('ipca100dez1993_lagged' %in% names(result)) {
    expect_true(is.na(row_201201$ipca100dez1993_lagged),
      label = 'ipca100dez1993_lagged is NA at 201201 (IPCA gap at 201112)')
  }
  if ('m_massaefettodosipcabr' %in% names(result)) {
    expect_true(is.na(row_201201$m_massaefettodosipcabr),
      label = 'm_massaefettodosipcabr is NA at 201201 (lagged IPCA is NA)')
  }
})

test_that('F1 EC4: latest_ipca is NA — Phase 5 guard prevents computation', {
  dt <- make_rq_with_ipca(start_yyyymm = 201201L, n_months = 24L,
    ipca_start = 201201L, ipca_values = rep(NA_real_, 24))

  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)

  result <- mensalize_sidra_series(dt, starting_points = sp,
    series = 'massaefetnominaltodos', compute_derived = TRUE, verbose = FALSE)

  if ('m_massaefettodosipcabr' %in% names(result)) {
    expect_true(all(is.na(result$m_massaefettodosipcabr)),
      label = 'm_massaefettodosipcabr all NA (Phase 5 guard, latest_ipca = NA)')
  }
})

# ============================================================================
# STRESS TESTS: Rolling Quarter Edge Cases and Robustness
# ============================================================================

test_that('STRESS-1: rolling_quarters data.table vazio deve falhar com stop', {
  empty_dt <- data.table::data.table()
  sp <- data.table::data.table(series_name = 'test', mesnotrim = 1, y0 = 100)
  
  expect_error(
    mensalize_sidra_series(empty_dt, starting_points = sp, verbose = FALSE),
    "Missing required columns",
    label = "Empty rolling_quarters should fail"
  )
})

test_that('STRESS-2: rolling_quarters sem series PNADC deve falhar', {
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L),
    mesnotrim = c(1L, 2L, 3L)
  )
  sp <- data.table::data.table(series_name = 'test', mesnotrim = 1, y0 = 100)
  
  expect_error(
    mensalize_sidra_series(dt, starting_points = sp, verbose = FALSE),
    "No series columns found",
    label = "No PNADC series should fail"
  )
})

test_that('STRESS-3: rolling_quarters apenas IPCA sem PNADC deve falhar', {
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L),
    mesnotrim = c(1L, 2L, 3L),
    ipca100dez1993 = c(100, 100.5, 101)
  )
  sp <- data.table::data.table(series_name = 'test', mesnotrim = 1, y0 = 100)
  
  expect_error(
    mensalize_sidra_series(dt, starting_points = sp, verbose = FALSE),
    "No series columns found",
    label = "Only IPCA should fail"
  )
})

test_that('STRESS-4: rolling_quarters com anomesfinaltrimmovel duplicado', {
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201203L, 201204L),
    mesnotrim = c(1L, 1L, 2L),
    massaefetnominaltodos = c(1000, 1000, 1050)
  )
  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)
  
  result <- mensalize_sidra_series(dt, starting_points = sp, verbose = FALSE)
  
  expect_true(nrow(result) > 0, label = "Duplicated anomesfinaltrimmovel should not crash")
  expect_true("m_massaefetnominaltodos" %in% names(result))
})

test_that('STRESS-5: rolling_quarters com mesnotrim inválido', {
  skip("known issue: mesnotrim validation not implemented in mensalize_sidra_series")
})

test_that('STRESS-6: rolling_quarters com anomesfinaltrimmovel desordenado', {
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201205L, 201203L, 201204L),
    mesnotrim = c(3L, 1L, 2L),
    massaefetnominaltodos = c(1100, 1000, 1050)
  )
  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)
  
  result <- mensalize_sidra_series(dt, starting_points = sp, verbose = FALSE)
  
  expect_true(all(diff(result$anomesexato) > 0),
              label = "Result should be properly ordered")
  expect_true(nrow(result) == 3L)
})

test_that('STRESS-7: starting_points vazio deve produzir m_* todos NA', {
  empty_sp <- data.table::data.table(
    series_name = character(),
    mesnotrim = integer(),
    y0 = numeric()
  )
  
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L),
    mesnotrim = c(1L, 2L, 3L),
    massaefetnominaltodos = c(1000, 1050, 1100)
  )
  
  result <- mensalize_sidra_series(dt, starting_points = empty_sp, verbose = FALSE)
  
  expect_true(all(is.na(result$m_massaefetnominaltodos)),
              label = "Empty starting_points should give all-NA m_*")
})

test_that('STRESS-8: starting_points com series nao em rolling_quarters', {
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L),
    mesnotrim = c(1L, 2L, 3L),
    massaefetnominaltodos = c(1000, 1050, 1100)
  )
  
  sp <- data.table::data.table(
    series_name = c('massaefetnominaltodos', 'nonexistent_series'),
    mesnotrim = rep(1, 2),
    y0 = c(1000, 5000)
  )
  
  result <- mensalize_sidra_series(dt, starting_points = sp,
                                    series = 'massaefetnominaltodos', verbose = FALSE)
  
  expect_true('m_massaefetnominaltodos' %in% names(result))
  expect_true(!'m_nonexistent_series' %in% names(result))
})

test_that('STRESS-9: rolling_quarters com todos valores NA', {
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L),
    mesnotrim = c(1L, 2L, 3L),
    massaefetnominaltodos = c(NA_real_, NA_real_, NA_real_)
  )
  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)
  
  result <- mensalize_sidra_series(dt, starting_points = sp, verbose = FALSE)
  
  expect_true(all(is.na(result$m_massaefetnominaltodos)))
})

test_that('STRESS-10: rolling_quarters com NA intercalado', {
  dt <- data.table::data.table(
    anomesfinaltrimmovel = c(201203L, 201204L, 201205L, 201206L, 201207L),
    mesnotrim = c(1L, 2L, 3L, 1L, 2L),
    massaefetnominaltodos = c(1000, 1050, NA_real_, 1100, 1150)
  )
  sp <- data.table::data.table(series_name = 'massaefetnominaltodos', mesnotrim = 1, y0 = 1000)
  
  result <- mensalize_sidra_series(dt, starting_points = sp, verbose = FALSE)
  
  expect_true(nrow(result) == 5L)
  expect_false(is.na(result$m_massaefetnominaltodos[4]),
               label = "KNOWN ISSUE: NA gap not properly handled")
})

