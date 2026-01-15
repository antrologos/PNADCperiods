## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  echo = TRUE,
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 10,
  fig.height = 6
)

## ----packages-----------------------------------------------------------------
# # Core packages
# library(mensalizePNADC)
# library(data.table)
# 
# # Survey analysis
# library(survey)       # For svydesign and linearization-based variance
# library(srvyr)        # Tidyverse-friendly wrapper for survey
# 
# # Visualization
# library(ggplot2)
# library(scales)

## ----load-data----------------------------------------------------------------
# # Load stacked PNADC data with ALL required variables
# # This needs: Ano, Trimestre, UPA, V1008, V1014, V2003, V2008, V20081, V20082, V2009
# #             V1028, UF, posest, posest_sxi (for weight calibration)
# #             Estrato (for Strategy 1)
# #             V1028001-V1028200 (for Strategy 2)
# #             Plus any analysis variables (VD4001, VD4002, V2007, etc.)
# 
# pnadc <- fread("path/to/your/pnadc_full_stacked.csv")
# 
# # Apply mensalization with weight computation
# pnadc <- mensalizePNADC(pnadc, compute_weights = TRUE, keep_all = TRUE)
# 
# # Filter to determined observations (those with monthly weights)
# pnadc_monthly <- pnadc[!is.na(weight_monthly)]

## ----design-linearization-----------------------------------------------------
# # Create survey design object for a specific month
# # Let's analyze January 2020 (ref_month_yyyymm = 202001)
# 
# pnadc_jan2020 <- pnadc_monthly[ref_month_yyyymm == 202001]
# 
# # Set up the survey design
# design_jan2020 <- svydesign(
#   ids = ~UPA,                    # PSU/cluster identifier
#   strata = ~Estrato,             # Stratification variable
#   weights = ~weight_monthly,     # Monthly-adjusted weight
#   data = pnadc_jan2020,
#   nest = TRUE                    # UPAs are nested within strata
# )
# 
# # Check the design
# summary(design_jan2020)

## ----estimates-linearization--------------------------------------------------
# # Labor force participation rate (VD4001 == 1 means in labor force)
# # Filter to working-age population (14+) first
# design_14plus <- subset(design_jan2020, V2009 >= 14)
# 
# participation <- svymean(~I(VD4001 == 1), design_14plus, na.rm = TRUE)
# print(participation)
# 
# # Get confidence interval
# confint(participation, level = 0.95)
# 
# # Participation by sex (V2007: 1 = Male, 2 = Female)
# participation_by_sex <- svyby(
#   ~I(VD4001 == 1),
#   by = ~V2007,
#   design = design_14plus,
#   FUN = svymean,
#   na.rm = TRUE
# )
# print(participation_by_sex)

## ----time-series-linearization------------------------------------------------
# # Function to compute participation rate for one month
# compute_participation <- function(month_yyyymm, data) {
# 
#   month_data <- data[ref_month_yyyymm == month_yyyymm & V2009 >= 14]
# 
#   if (nrow(month_data) == 0) return(NULL)
# 
#   # Create survey design
#   design <- svydesign(
#     ids = ~UPA,
#     strata = ~Estrato,
#     weights = ~weight_monthly,
#     data = month_data,
#     nest = TRUE
#   )
# 
#   # Compute overall participation
#   result <- svymean(~I(VD4001 == 1), design, na.rm = TRUE)
# 
#   data.table(
#     ref_month_yyyymm = month_yyyymm,
#     participation = as.numeric(result),
#     se = as.numeric(SE(result))
#   )
# }
# 
# # Get all unique months
# months <- sort(unique(pnadc_monthly$ref_month_yyyymm))
# 
# # Compute for all months (this may take a few minutes)
# results_list <- lapply(months, compute_participation, data = pnadc_monthly)
# monthly_participation <- rbindlist(results_list)
# 
# # Add confidence intervals
# monthly_participation[, `:=`(
#   ci_lower = participation - 1.96 * se,
#   ci_upper = participation + 1.96 * se,
#   period = as.Date(paste0(ref_month_yyyymm %/% 100, "-",
#                           ref_month_yyyymm %% 100, "-15"))
# )]

## ----create-rep-weights-------------------------------------------------------
# # Calculate the adjustment ratio
# pnadc_monthly[, weight_ratio := weight_monthly / V1028]
# 
# # Get names of replication weight columns
# rep_weight_cols <- paste0("V1028", sprintf("%03d", 1:200))
# 
# # Create adjusted replication weight columns
# for (col in rep_weight_cols) {
#   new_col <- paste0(col, "_monthly")
#   pnadc_monthly[, (new_col) := get(col) * weight_ratio]
# }
# 
# # Verify the adjustment (the ratio should be constant within each observation)
# cat("Ratio range:", range(pnadc_monthly$weight_ratio, na.rm = TRUE), "\n")

