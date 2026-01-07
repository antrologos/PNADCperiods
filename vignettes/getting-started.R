## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----installation-------------------------------------------------------------
# # Install from GitHub (when available)
# # devtools::install_github("PLACEHOLDER/mensalizePNADC")
# 
# # Or install locally
# # devtools::install("path/to/mensalizePNADC")

## ----load-data----------------------------------------------------------------
# library(mensalizePNADC)
# library(data.table)
# 
# # Load your stacked quarterly PNADC data
# pnadc <- fread("pnadc_stacked.csv",
#   select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
#              "V2008", "V20081", "V20082", "V2009"))

## ----get-crosswalk------------------------------------------------------------
# crosswalk <- mensalizePNADC(pnadc)
# 
# print(crosswalk)
# # PNADC Reference Month Crosswalk
# # -------------------------------
# # Observations: 1,234,567
# # Determination rate: 87.3%
# # Date range: 201201 - 202312

## ----join-data----------------------------------------------------------------
# # Load an original quarterly file
# library(haven)
# original <- read_dta("PNADC_2023T1.dta")
# 
# # Join to add monthly information
# monthly_data <- merge(original, crosswalk,
#   by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"),
#   all.x = TRUE)
# 
# # Now you have:
# # - ref_month: Reference month as Date
# # - ref_month_in_quarter: Position in quarter (1, 2, 3, or NA)
# # - ref_month_yyyymm: Integer YYYYMM format

## ----use-monthly--------------------------------------------------------------
# # Filter to a specific month
# jan_2023 <- monthly_data[ref_month_yyyymm == 202301]
# 
# # Group by month
# by_month <- monthly_data[, .(
#   n_obs = .N,
#   mean_age = mean(V2009, na.rm = TRUE)
# ), by = ref_month_yyyymm]

## ----monthly-weights----------------------------------------------------------
# # Load full data with all required variables
# pnadc_full <- read_dta("PNADCtrimestralempilhada.dta")
# 
# # Load monthly population totals
# monthly_pop <- read_dta("pnadc_series_mensalizadas.dta")
# 
# # Run full mensalization
# result <- mensalizePNADC(pnadc_full,
#   compute_weights = TRUE,
#   monthly_totals = monthly_pop,
#   verbose = TRUE)
# 
# # Use weight_monthly for estimates
# unemployment <- result[, .(
#   unemployed = sum(weight_monthly * (VD4002 == 2), na.rm = TRUE),
#   labor_force = sum(weight_monthly * (VD4001 == 1), na.rm = TRUE)
# ), by = ref_month_yyyymm]
# 
# unemployment[, rate := 100 * unemployed / labor_force]

## ----exceptions---------------------------------------------------------------
# get_exception_quarters()
# # [1] "2016t3" "2016t4" "2017t2" "2022t3" "2023t2"

## ----modular------------------------------------------------------------------
# # Step 1: Just identify reference months
# months <- identify_reference_month(pnadc)
# 
# # Step 2: Check determination rate by quarter
# months[, .(
#   total = .N,
#   determined = sum(!is.na(ref_month_in_quarter)),
#   rate = mean(!is.na(ref_month_in_quarter))
# ), by = .(Ano, Trimestre)]
# 
# # Step 3: Calibrate weights (if needed)
# calibrated <- calibrate_monthly_weights(merged_data, monthly_pop)
# 
# # Step 4: Compute indicators
# indicators <- compute_labor_indicators(calibrated)

