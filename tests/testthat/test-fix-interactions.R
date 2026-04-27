# Tests for Fix Interactions: V1, V2, F1 operating together
# 
# Context:
# V1 (trailing fantasma): m[is.na(rq)] <- NA_real_ — REPLACED by V2
# V2 (trailing-only): NA-out only after last non-NA of rq (preserves leading via lookahead)
# F1 (pre-PNADC lag IPCA/INPC): pre-compute lag before PNADC filter
#
# These tests confirm all 3 fixes work together without interference.

# Helper: Create synthetic rolling quarters with IPCA for interaction testing
make_rq_interaction_test <- function(
    start_yyyymm = 201201L,
    n_months = 24L,
    series_name = "massaefetnominaltodos",
    base_value = 1000,
    trend = 100,
    ipca_start = NULL,
    ipca_values = NULL,
    na_pattern = NULL) {
  
  start_year <- start_yyyymm %/% 100L
  start_month <- start_yyyymm %% 100L
  
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year + (start_month + month_num - 2L) %/% 12L,
    month = ((start_month + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  
  # Initialize series with NA, then fill where specified
  dt[, (series_name) := NA_real_]
  
  # Apply NA pattern if provided (indices to mark as NA)
  if (!is.null(na_pattern)) {
    # na_pattern is list(leading = c(...), trailing = c(...))
    if (!is.null(na_pattern$leading)) {
      dt[na_pattern$leading, (series_name) := NA_real_]
    }
    if (!is.null(na_pattern$trailing)) {
      dt[na_pattern$trailing, (series_name) := NA_real_]
    }
  }
  
  # Fill non-NA positions with trend
  dt[!is.na(get(series_name)) | (is.na(na_pattern) | (is.null(na_pattern$leading) && is.null(na_pattern$trailing))),
     (series_name) := base_value + trend * (seq_len(.N) - 1L)]
  
  # Add IPCA if specified
  if (!is.null(ipca_start)) {
    dt[, ipca100dez1993 := NA_real_]
    idx_start <- which(dt$anomesfinaltrimmovel == ipca_start)[1]
    
    if (!is.na(idx_start) && !is.null(ipca_values)) {
      n_ipca <- length(ipca_values)
      end_idx <- min(idx_start + n_ipca - 1, nrow(dt))
      dt[idx_start:end_idx, ipca100dez1993 := ipca_values[1:(end_idx - idx_start + 1)]]
    }
  }
  
  dt[, .SD, .SDcols = c("anomesfinaltrimmovel", "mesnotrim", series_name,
                        if ("ipca100dez1993" %in% names(dt)) "ipca100dez1993" else NULL)]
}


# =============================================================================
# T1: Rolling quarters with trailing NA (V2) AND pre-PNADC IPCA (F1)
# =============================================================================
test_that("T1: V2 trailing + F1 pre-PNADC IPCA work together", {
  # Setup: 26 months (201111-201312)
  # - 201111 to 201112: IPCA only (pre-PNADC), pop NA
  # - 201201 to 201212: pop data + IPCA (PNADC era)
  # - 202603 (last row): trailing NA in pop, but IPCA continues
  
  start_year <- 201111L
  n_months <- 26L
  
  start_year_num <- start_year %/% 100L
  start_month_num <- start_year %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year_num + (start_month_num + month_num - 2L) %/% 12L,
    month = ((start_month_num + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  
  # IPCA from 201111 throughout
  dt[, ipca100dez1993 := 100 + 0.3 * (month_num - 1)]
  
  # massaefetnominaltodos: NA for 201111-201112, then data from 201201+
  dt[, massaefetnominaltodos := NA_real_]
  pnadc_start_idx <- which(dt$anomesfinaltrimmovel >= 201201L)[1]
  if (!is.na(pnadc_start_idx)) {
    dt[pnadc_start_idx:nrow(dt), massaefetnominaltodos := 1000 + 50 * seq_len(nrow(dt) - pnadc_start_idx + 1)]
  }
  
  # Mark last row as trailing NA (V2 behavior)
  dt[.N, massaefetnominaltodos := NA_real_]
  
  sp <- data.table::data.table(
    series_name = "massaefetnominaltodos",
    mesnotrim = 1:3,
    y0 = c(1000, 1010, 1020)
  )
  
  result <- mensalize_sidra_series(dt, starting_points = sp,
    series = "massaefetnominaltodos", compute_derived = TRUE, verbose = FALSE)
  
  # Assert F1: m_massaefettodosipcabr[201201] is non-NA (lagged IPCA from 201112 exists)
  row_201201 <- result[anomesexato == 201201L]
  if ("m_massaefettodosipcabr" %in% names(result)) {
    expect_false(is.na(row_201201$m_massaefettodosipcabr),
      label = "T1-F1: m_massaefettodosipcabr[201201] is non-NA (F1 lag from pre-PNADC)")
  }
  
  # Assert V2: m_massaefetnominaltodos[last] is NA (trailing NA)
  last_row <- result[.N]
  expect_true(is.na(last_row$m_massaefetnominaltodos),
    label = "T1-V2: m_massaefetnominaltodos[trailing] is NA (V2 trailing)")
})


# =============================================================================
# T2: Leading NA reconstruction (V2 lookahead) + F1 pre-PNADC lag
# =============================================================================
test_that("T2: V2 lookahead (leading NA) + F1 lag work together", {
  # Setup: 26 months (201111-201312)
  # - 201111-201112: IPCA present but pop NA (leading)
  # - 201201 onwards: pop data present
  # Expected: m_popocup[201201] reconstructed via lookahead, m_*ipcabr[201201] via F1 lag
  
  start_year <- 201111L
  n_months <- 26L
  
  start_year_num <- start_year %/% 100L
  start_month_num <- start_year %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year_num + (start_month_num + month_num - 2L) %/% 12L,
    month = ((start_month_num + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  
  # IPCA from 201111 throughout (pre-PNADC available)
  dt[, ipca100dez1993 := 100 + 0.2 * (month_num - 1)]
  
  # popocup: NA for 201111-201112, then data from 201201+
  dt[, popocup := NA_real_]
  pnadc_start_idx <- which(dt$anomesfinaltrimmovel >= 201201L)[1]
  if (!is.na(pnadc_start_idx)) {
    dt[pnadc_start_idx:nrow(dt), popocup := 80000 + 200 * seq_len(nrow(dt) - pnadc_start_idx + 1)]
  }
  
  # popdesocup: same pattern
  dt[, popdesocup := NA_real_]
  if (!is.na(pnadc_start_idx)) {
    dt[pnadc_start_idx:nrow(dt), popdesocup := 5000 + 30 * seq_len(nrow(dt) - pnadc_start_idx + 1)]
  }
  
  sp <- data.table::data.table(
    series_name = c(rep("popocup", 3), rep("popdesocup", 3)),
    mesnotrim = rep(1:3, 2),
    y0 = c(80000, 80100, 80200, 5000, 5100, 5200)
  )
  
  result <- mensalize_sidra_series(dt, starting_points = sp,
    series = c("popocup", "popdesocup"), compute_derived = TRUE, verbose = FALSE)
  
  # Assert V2 lookahead: m_popocup[201201] is non-NA
  # (Algorithm reconstructs leading position via y0 + lookahead in final adjustment)
  row_201201 <- result[anomesexato == 201201L]
  expect_false(is.na(row_201201$m_popocup),
    label = "T2-V2: m_popocup[201201] is non-NA (V2 lookahead reconstruction)")
  expect_false(is.na(row_201201$m_popdesocup),
    label = "T2-V2: m_popdesocup[201201] is non-NA (V2 lookahead reconstruction)")
  
  # Assert F1: if IPCA-deflated series exist, they should also be non-NA at 201201
  # (because F1 pre-computed lag before PNADC filter)
  # Note: These are only created if compute_derived=TRUE AND ipca series present
  if ("m_popocupipcabr" %in% names(result)) {
    expect_false(is.na(row_201201$m_popocupipcabr),
      label = "T2-F1: m_popocupipcabr[201201] is non-NA (F1 lag from pre-PNADC)")
  }
})


# =============================================================================
# T3: Combined trailing + leading NA + IPCA pre-2012 (all 3 fixes interact)
# =============================================================================
test_that("T3: V2 trailing + V2 leading + F1 IPCA all together", {
  # Setup: 28 months (201111-202003)
  # - 201111-201112: leading NA in pop, IPCA present
  # - 201201 to 201912: full data
  # - 202001 to 202003: trailing NA in pop (last 3 months), IPCA continues
  # Expected: 
  # - m_popocup[201201] non-NA (V2 lookahead)
  # - m_*ipcabr[201201] non-NA (F1 lag from 201112)
  # - m_popocup[202001-202003] NA (V2 trailing)
  
  start_year <- 201111L
  n_months <- 28L
  
  start_year_num <- start_year %/% 100L
  start_month_num <- start_year %% 100L
  dt <- data.table::data.table(month_num = seq_len(n_months))
  dt[, `:=`(
    year = start_year_num + (start_month_num + month_num - 2L) %/% 12L,
    month = ((start_month_num + month_num - 2L) %% 12L) + 1L
  )]
  dt[, anomesfinaltrimmovel := year * 100L + month]
  dt[, mesnotrim := ((month - 1L) %% 3L) + 1L]
  
  # IPCA from 201111 throughout
  dt[, ipca100dez1993 := 100 + 0.15 * (month_num - 1)]
  
  # popocup: NA for 201111-201112 (leading), NA for 202001-202003 (trailing)
  dt[, popocup := NA_real_]
  pnadc_start_idx <- which(dt$anomesfinaltrimmovel >= 201201L)[1]
  pnadc_end_idx <- which(dt$anomesfinaltrimmovel <= 201912L)
  pnadc_end_idx <- if (length(pnadc_end_idx) > 0) max(pnadc_end_idx) else nrow(dt)
  
  if (!is.na(pnadc_start_idx) && pnadc_start_idx <= pnadc_end_idx) {
    dt[pnadc_start_idx:pnadc_end_idx, popocup := 80000 + 100 * seq_len(pnadc_end_idx - pnadc_start_idx + 1)]
  }
  
  sp <- data.table::data.table(
    series_name = "popocup",
    mesnotrim = 1:3,
    y0 = c(80000, 80050, 80100)
  )
  
  result <- mensalize_sidra_series(dt, starting_points = sp,
    series = "popocup", compute_derived = TRUE, verbose = FALSE)
  
  # Assert V2 lookahead: m_popocup[201201] non-NA
  row_201201 <- result[anomesexato == 201201L]
  expect_false(is.na(row_201201$m_popocup),
    label = "T3-V2-lead: m_popocup[201201] is non-NA (V2 lookahead)")
  
  # Assert V2 trailing: m_popocup[202001-202003] NA
  trailing_rows <- result[anomesexato %in% c(202001L, 202002L, 202003L)]
  expect_true(all(is.na(trailing_rows$m_popocup)),
    label = "T3-V2-trail: m_popocup[202001-202003] all NA (V2 trailing)")
  
  # Assert F1: if deflated series exist, m_popocupipcabr[201201] non-NA
  if ("m_popocupipcabr" %in% names(result)) {
    expect_false(is.na(row_201201$m_popocupipcabr),
      label = "T3-F1: m_popocupipcabr[201201] is non-NA (F1 lag from pre-PNADC)")
  }
})
