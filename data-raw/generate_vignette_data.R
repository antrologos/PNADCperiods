# =============================================================================
# Generate Pre-computed Data and Figures for the Applied Examples Vignette
# =============================================================================
#
# This script produces:
#   1. Aggregated time series (saved to data/processed/)
#   2. PNG figures for the vignette (saved to output/vignette/figures/)
#
# IMPORTANT: Code blocks marked with "# VIGNETTE CODE:" comments are EXACTLY
# what appears in the applied-examples.Rmd vignette. Any changes here must
# be mirrored there, and vice versa.
#
# Run code/check_vignette_sync.R to verify synchronization before release.
#
# CACHING STRATEGY (multi-level):
#   Level 1: If all aggregate series exist and are up-to-date → load them,
#            skip directly to figure generation (fastest)
#   Level 2: If stacked microdata cache exists → load it, regenerate aggregates
#   Level 3: If nothing cached → run full stacking loop from quarterly files
#
# To force regeneration, delete the relevant cache files in data/processed/
# =============================================================================

library(PNADCperiods)
library(data.table)
library(ggplot2)
library(scales)
library(fst)

# =============================================================================
# PATHS
# =============================================================================

pnadc_dir     <- "D:/Dropbox/Bancos_Dados/PNADC/Trimestral/Dados/"
processed_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/data/processed/"
fig_dir       <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/figures/applied-examples/"
table_dir     <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/tables/"
pkg_fig_dir   <- "D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods/vignettes/figures/applied-examples/"

# Create directories if needed
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pkg_fig_dir, recursive = TRUE, showWarnings = FALSE)

# Standard figure settings
fig_width  <- 10
fig_height <- 6
fig_dpi    <- 150

# Set threads
data.table::setDTthreads(4)

# =============================================================================
# CACHE FILES
# =============================================================================

# Microdata cache (stacked quarterly files with derived variables)
microdata_cache <- paste0(processed_dir, "pnadc_vignette_microdata.fst")

# Aggregate series cache files
aggregate_files <- c(
  quarterly_total  = paste0(processed_dir, "series_quarterly_total.fst"),
  quarterly_gender = paste0(processed_dir, "series_quarterly_gender.fst"),
  quarterly_race   = paste0(processed_dir, "series_quarterly_race.fst"),
  monthly_total    = paste0(processed_dir, "series_monthly_total.fst"),
  monthly_gender   = paste0(processed_dir, "series_monthly_gender.fst"),
  monthly_race     = paste0(processed_dir, "series_monthly_race.fst"),
  monthly_mw_exact = paste0(processed_dir, "series_monthly_mw_exact.fst"),
  mw_history       = paste0(processed_dir, "mw_history.fst"),
  # Sub-monthly analysis files
  fortnight_covid         = paste0(processed_dir, "series_fortnight_covid.fst"),
  weekly_carnival         = paste0(processed_dir, "series_weekly_carnival.fst"),
  weekly_carnival_detail  = paste0(processed_dir, "series_weekly_carnival_detail.fst")
)

# =============================================================================
# CACHE VALIDATION FUNCTIONS
# =============================================================================

check_aggregates_exist <- function() {
  all(file.exists(aggregate_files))
}

check_aggregates_uptodate <- function() {
  # Aggregates are up-to-date if they exist and are newer than microdata cache
  if (!check_aggregates_exist()) return(FALSE)
  if (!file.exists(microdata_cache)) return(TRUE)  # No microdata to compare

  microdata_mtime <- file.info(microdata_cache)$mtime
  aggregate_mtimes <- sapply(aggregate_files, function(f) file.info(f)$mtime)

  # All aggregates must be newer than microdata
  all(aggregate_mtimes >= microdata_mtime)
}

check_microdata_exists <- function() {
  file.exists(microdata_cache)
}

check_microdata_uptodate <- function() {
  # Microdata is up-to-date if it exists and is newer than any raw quarterly file
  if (!check_microdata_exists()) return(FALSE)

  # List raw quarterly files
  raw_files <- list.files(pnadc_dir, pattern = "\\.fst$", full.names = TRUE)
  raw_files <- raw_files[!grepl("input", raw_files)]

  if (length(raw_files) == 0) return(TRUE)  # No raw files to compare

  microdata_mtime <- file.info(microdata_cache)$mtime
  raw_mtimes <- sapply(raw_files, function(f) file.info(f)$mtime)

  # Microdata must be newer than all raw files
  all(microdata_mtime >= raw_mtimes)
}

# =============================================================================
# DETERMINE WHAT NEEDS TO BE DONE
# =============================================================================

cat("=== Checking Cache Status ===\n")

aggregates_exist <- check_aggregates_exist()
aggregates_uptodate <- check_aggregates_uptodate()
microdata_exists <- check_microdata_exists()
microdata_uptodate <- check_microdata_uptodate()
need_aggregates <- !aggregates_exist || !aggregates_uptodate

cat("  Microdata cache exists:", microdata_exists, "\n")
cat("  Microdata cache up-to-date:", microdata_uptodate, "\n")
cat("  Aggregate files exist:", aggregates_exist, "\n")
cat("  Aggregate files up-to-date:", aggregates_uptodate, "\n")

# Determine processing path
if (aggregates_exist && aggregates_uptodate) {
  processing_mode <- "load_aggregates"
  cat("\n-> Using cached aggregates (skipping to figure generation)\n")
} else if (microdata_exists && microdata_uptodate) {
  processing_mode <- "load_microdata"
  cat("\n-> Using cached microdata (regenerating aggregates)\n")
} else {
  processing_mode <- "full_rebuild"
  cat("\n-> Full rebuild from quarterly files\n")
}

# =============================================================================
# LEVEL 1: LOAD CACHED AGGREGATES (fastest path)
# =============================================================================

if (processing_mode == "load_aggregates") {

  cat("\n=== Loading Cached Aggregates ===\n")

  quarterly_total  <- fst::read_fst(aggregate_files["quarterly_total"], as.data.table = TRUE)
  quarterly_gender <- fst::read_fst(aggregate_files["quarterly_gender"], as.data.table = TRUE)
  quarterly_race   <- fst::read_fst(aggregate_files["quarterly_race"], as.data.table = TRUE)
  monthly_total    <- fst::read_fst(aggregate_files["monthly_total"], as.data.table = TRUE)
  monthly_gender   <- fst::read_fst(aggregate_files["monthly_gender"], as.data.table = TRUE)
  monthly_race     <- fst::read_fst(aggregate_files["monthly_race"], as.data.table = TRUE)
  monthly_mw_exact <- fst::read_fst(aggregate_files["monthly_mw_exact"], as.data.table = TRUE)
  mw_history       <- fst::read_fst(aggregate_files["mw_history"], as.data.table = TRUE)

  # Load sub-monthly series if available
  fortnight_covid <- if (file.exists(aggregate_files["fortnight_covid"])) {
    fst::read_fst(aggregate_files["fortnight_covid"], as.data.table = TRUE)
  } else NULL
  weekly_carnival <- if (file.exists(aggregate_files["weekly_carnival"])) {
    fst::read_fst(aggregate_files["weekly_carnival"], as.data.table = TRUE)
  } else NULL
  weekly_carnival_detail <- if (file.exists(aggregate_files["weekly_carnival_detail"])) {
    fst::read_fst(aggregate_files["weekly_carnival_detail"], as.data.table = TRUE)
  } else NULL

  # Ensure period is Date type
  for (dt in list(quarterly_total, quarterly_gender, quarterly_race,
                  monthly_total, monthly_gender, monthly_race, monthly_mw_exact)) {
    if ("period" %in% names(dt) && !inherits(dt$period, "Date")) {
      dt[, period := as.Date(period)]
    }
  }

  cat("Loaded all aggregate series.\n")
  cat("  Quarterly total:", nrow(quarterly_total), "rows\n")
  cat("  Monthly total:", nrow(monthly_total), "rows\n")
  cat("  Monthly MW exact:", nrow(monthly_mw_exact), "rows\n")

  # Recreate mw_adjustment_months from cached mw_history
  mw_history[, mw_change := mw_value != shift(mw_value, fill = mw_value[1])]
  mw_adjustment_months <- mw_history[mw_change == TRUE, yyyymm]

}

# =============================================================================
# LEVEL 2 & 3: LOAD OR BUILD MICRODATA
# =============================================================================

if (processing_mode %in% c("load_microdata", "full_rebuild")) {

  # ---------------------------------------------------------------------------
  # STEP 1: LOAD PNADC DATA
  # ---------------------------------------------------------------------------

  cat("\n=== Step 1: Load PNADC Data ===\n")

  if (processing_mode == "load_microdata") {

    cat("Loading cached microdata from:", basename(microdata_cache), "\n")
    pnadc <- fst::read_fst(microdata_cache, as.data.table = TRUE)
    cat("Loaded", format(nrow(pnadc), big.mark = ","), "observations\n")
    cat("Period:", min(pnadc$Ano), "Q", min(pnadc[Ano == min(Ano), Trimestre]),
        "to", max(pnadc$Ano), "Q", max(pnadc[Ano == max(Ano), Trimestre]), "\n")

  } else {
    # Full rebuild from quarterly files

    cat("Loading from quarterly files...\n")

    # List all quarterly data files
    files <- list.files(pnadc_dir, pattern = "\\.fst$", full.names = TRUE)
    files <- files[!grepl("input", files)]

    cat("Found", length(files), "quarterly files\n")

    # Columns needed for analysis
    cols_needed <- c(
      # Identifiers and time
      "Ano", "Trimestre", "UF",
      # Mensalization columns
      "UPA", "Estrato", "V1008", "V1014",
      "V2008", "V20081", "V20082", "V2009",
      # Weight columns
      "V1028", "posest", "posest_sxi",
      # Labor market variables
      "VD4001",   # PEA (economically active)
      "VD4002",   # Employment condition
      "VD4009",   # Position in occupation
      "VD4012",   # Social security contribution
      # Income variables
      "VD4016",   # Habitual income - main job (refers to CURRENT month)
      "VD4017",   # Effective income - main job (refers to PREVIOUS month)
      "VD4019",   # Habitual income - all jobs
      "VD4020",   # Effective income - all jobs
      # Hours worked
      "VD4035",   # Effective hours worked - all jobs (in reference week)
      # Demographics
      "V2007",    # Sex
      "V2010"     # Race
    )

    # Load all files
    pnadc_list <- lapply(files, function(f) {
      cat("Loading:", basename(f), "\n")
      dt <- read_fst(f, as.data.table = TRUE)

      # Standardize column names to uppercase
      setnames(dt, names(dt), toupper(names(dt)))

      # Select available columns
      available <- intersect(toupper(cols_needed), names(dt))
      dt <- dt[, ..available]

      # Rename to expected case
      name_map <- c(
        "ANO" = "Ano", "TRIMESTRE" = "Trimestre", "UF" = "UF",
        "UPA" = "UPA", "ESTRATO" = "Estrato", "V1008" = "V1008", "V1014" = "V1014",
        "V2008" = "V2008", "V20081" = "V20081", "V20082" = "V20082",
        "V2009" = "V2009", "V1028" = "V1028", "POSEST" = "posest", "POSEST_SXI" = "posest_sxi",
        "VD4001" = "VD4001", "VD4002" = "VD4002", "VD4009" = "VD4009", "VD4012" = "VD4012",
        "VD4016" = "VD4016", "VD4017" = "VD4017", "VD4019" = "VD4019", "VD4020" = "VD4020",
        "VD4035" = "VD4035", "V2007" = "V2007", "V2010" = "V2010"
      )

      for (old_name in names(dt)) {
        if (old_name %in% names(name_map)) {
          setnames(dt, old_name, name_map[old_name])
        }
      }

      dt
    })

    pnadc <- rbindlist(pnadc_list, fill = TRUE)
    rm(pnadc_list); gc()

    cat("\nTotal observations:", format(nrow(pnadc), big.mark = ","), "\n")
    cat("Period:", min(pnadc$Ano), "Q", min(pnadc[Ano == min(Ano), Trimestre]),
        "to", max(pnadc$Ano), "Q", max(pnadc[Ano == max(Ano), Trimestre]), "\n")

    # -------------------------------------------------------------------------
    # STEP 2: CREATE LABOR MARKET VARIABLES
    # -------------------------------------------------------------------------

    cat("\n=== Step 2: Ensure Numeric Types ===\n")

    # Ensure numeric types
    numeric_cols <- c("V2009", "V1028", "VD4001", "VD4002", "VD4009", "VD4012",
                      "VD4016", "VD4017", "VD4019", "VD4020", "VD4035", "V2007", "V2010")
    for (col in numeric_cols) {
      if (col %in% names(pnadc) && !is.numeric(pnadc[[col]])) {
        pnadc[, (col) := as.numeric(get(col))]
      }
    }

    cat("Total observations:", format(nrow(pnadc), big.mark = ","), "\n")

    # Save microdata cache
    cat("\nSaving cached microdata to:", basename(microdata_cache), "\n")
    fst::write_fst(pnadc, microdata_cache, compress = 50)
    cat("Microdata cache saved.\n")
  }

  # ---------------------------------------------------------------------------
  # STEP 3: APPLY MENSALIZATION
  # ---------------------------------------------------------------------------

  cat("\n=== Step 3: Apply Mensalization ===\n")

  # Build crosswalk (identify reference periods)
  # IMPORTANT: store_date_bounds = TRUE is required for experimental strategies
  crosswalk <- pnadc_identify_periods(pnadc, verbose = TRUE, store_date_bounds = TRUE)

  # Check determination rate
  det_rate <- crosswalk[, mean(determined_month, na.rm = TRUE)]
  cat("\nMonth determination rate:", sprintf("%.1f%%", det_rate * 100), "\n")

  # Keep a clean copy for sub-monthly calibration later (Section 4)
  pnadc_clean <- copy(pnadc)

  # Apply crosswalk and calibrate weights
  result <- pnadc_apply_periods(
    pnadc,
    crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    verbose = TRUE
  )

  # pnadc_apply_periods() returns the input data with crosswalk merged in
  # No separate merge needed - just use result as pnadc
  pnadc <- result

  cat("Observations with determined month:",
      format(sum(!is.na(pnadc$ref_month_in_quarter)), big.mark = ","),
      sprintf("(%.1f%%)", mean(!is.na(pnadc$ref_month_in_quarter)) * 100), "\n")

  # ===========================================================================
  # STEP 3b: APPLY EXPERIMENTAL STRATEGIES FOR SUB-MONTHLY ANALYSIS
  # ===========================================================================

  cat("\n=== Step 3b: Apply Experimental Strategies for Sub-Monthly Periods ===\n")

  # Ensure join key types match between data and crosswalk
  # The crosswalk may have different types than pnadc for some columns
  join_cols <- c("UPA", "V1008", "V1014")
  for (col in join_cols) {
    if (col %in% names(pnadc) && col %in% names(crosswalk)) {
      if (is.character(pnadc[[col]]) && is.integer(crosswalk[[col]])) {
        cat(sprintf("Converting %s to integer to match crosswalk...\n", col))
        pnadc[, (col) := as.integer(get(col))]
        pnadc_clean[, (col) := as.integer(get(col))]
      } else if (is.integer(pnadc[[col]]) && is.character(crosswalk[[col]])) {
        cat(sprintf("Converting crosswalk$%s to integer to match pnadc...\n", col))
        crosswalk[, (col) := as.integer(get(col))]
      }
    }
  }

  # Apply experimental strategies for improved fortnight/week determination

  # This uses probabilistic assignment and UPA aggregation to boost
  # the 8.9% strict fortnight rate and 3.3% strict week rate
  #
  # Thresholds (user requirements for vignette):
  # - confidence_threshold = 0.85 (85%): For probabilistic assignment, require
  #   >=85% of the interview window to fall within a single period
  # - upa_proportion_threshold = 0.80 (80%): For UPA aggregation, require
  #   >=80% of strictly identified observations in a UPA to have the same period
  #
  # Expected rates with these thresholds: fortnight ~12-13%, week ~5-6%
  crosswalk_experimental <- pnadc_experimental_periods(
    crosswalk,
    strategy = "both",  # probabilistic + UPA aggregation
    confidence_threshold = 0.85,
    upa_proportion_threshold = 0.80,
    verbose = TRUE
  )

  # Verify nesting is preserved (fortnights require months, weeks require fortnights)
  if (sum(crosswalk_experimental$determined_fortnight &
          !crosswalk_experimental$determined_month, na.rm = TRUE) > 0) {
    stop("Nesting violation: fortnights determined without months")
  }
  if (sum(crosswalk_experimental$determined_week &
          !crosswalk_experimental$determined_fortnight, na.rm = TRUE) > 0) {
    stop("Nesting violation: weeks determined without fortnights")
  }
  cat("\U2713 Nesting validation: PASSED\n")

  # Calculate experimental rates from the crosswalk
  n_total <- nrow(crosswalk_experimental)
  n_fortnight_exp <- sum(!is.na(crosswalk_experimental$ref_fortnight_in_quarter))
  n_week_exp <- sum(!is.na(crosswalk_experimental$ref_week_in_quarter))
  cat("\nExperimental determination counts:\n")
  cat("  Fortnights:", format(n_fortnight_exp, big.mark = ","),
      sprintf("(%.1f%%)\n", 100 * n_fortnight_exp / n_total))
  cat("  Weeks:", format(n_week_exp, big.mark = ","),
      sprintf("(%.1f%%)\n", 100 * n_week_exp / n_total))

  # Validate against expected ranges for conf=0.85, upa=0.80
  expected_fortnight_range <- c(10.0, 13.5)
  expected_week_range <- c(5.0, 7.0)

  # Calculate actual rates from counts (more reliable than attributes)
  actual_fn_rate <- 100 * n_fortnight_exp / n_total
  actual_wk_rate <- 100 * n_week_exp / n_total

  if (!is.na(actual_fn_rate) &&
      (actual_fn_rate < expected_fortnight_range[1] ||
       actual_fn_rate > expected_fortnight_range[2])) {
    warning(sprintf("Fortnight rate %.1f%% outside expected [%.1f%%, %.1f%%]",
                    actual_fn_rate,
                    expected_fortnight_range[1],
                    expected_fortnight_range[2]))
  } else {
    cat(sprintf("✓ Fortnight rate %.1f%% within expected range\n", actual_fn_rate))
  }

  if (!is.na(actual_wk_rate) &&
      (actual_wk_rate < expected_week_range[1] ||
       actual_wk_rate > expected_week_range[2])) {
    warning(sprintf("Week rate %.1f%% outside expected [%.1f%%, %.1f%%]",
                    actual_wk_rate,
                    expected_week_range[1],
                    expected_week_range[2]))
  } else {
    cat(sprintf("✓ Week rate %.1f%% within expected range\n", actual_wk_rate))
  }

  # ===========================================================================
  # STEP 3c: CALIBRATE FORTNIGHT WEIGHTS (clean approach matching vignette)
  # ===========================================================================
  # Fortnight and weekly analysis require calibrated weights to account for
  # the non-random selection into sub-monthly determination.
  # Using pnadc_clean (copy before monthly pnadc_apply_periods) avoids column
  # conflicts from duplicate merges.

  cat("\n=== Step 3c: Calibrate Fortnight Weights ===\n")

  # --- VIGNETTE CODE: submonthly-workflow (fortnight) ---
  pnadc_fortnight <- pnadc_apply_periods(
    pnadc_clean,
    crosswalk_experimental,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "fortnight",
    verbose = TRUE
  )
  # --- END VIGNETTE CODE ---

  # Filter to working-age population (matching Step 2)
  pnadc_fortnight <- pnadc_fortnight[V2009 >= 14]

  cat("Fortnight calibration complete.\n")
  cat("Observations with weight_fortnight:",
      format(sum(!is.na(pnadc_fortnight$weight_fortnight)), big.mark = ","),
      sprintf("(%.1f%%)\n", 100 * mean(!is.na(pnadc_fortnight$weight_fortnight))))

  # ===========================================================================
  # STEP 3d: CALIBRATE WEEKLY WEIGHTS
  # ===========================================================================

  cat("\n=== Step 3d: Calibrate Weekly Weights ===\n")

  # --- VIGNETTE CODE: submonthly-workflow (weekly) ---
  pnadc_weekly <- pnadc_apply_periods(
    copy(pnadc_clean),
    crosswalk_experimental,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "week",
    smooth = FALSE,  # No smoothing for weekly (too sparse)
    verbose = TRUE
  )
  # --- END VIGNETTE CODE ---

  # Filter to working-age population (matching Step 2)
  pnadc_weekly <- pnadc_weekly[V2009 >= 14]

  cat("Weekly calibration complete.\n")
  cat("Observations with weight_weekly:",
      format(sum(!is.na(pnadc_weekly$weight_weekly)), big.mark = ","),
      sprintf("(%.1f%%)\n", 100 * mean(!is.na(pnadc_weekly$weight_weekly))))

  # ===========================================================================
  # STEP 3e: FILTER AND CREATE LABOR MARKET VARIABLES
  # ===========================================================================
  # Filter to working-age population (14+) AFTER crosswalk and calibration.
  # The crosswalk was built from ALL observations (including children) for
  # better determination rates (~97% vs ~95% with 14+ only).

  cat("\n=== Step 3e: Filter to Working-Age Population and Create Variables ===\n")

  # --- VIGNETTE CODE: create-variables ---
  pnadc <- pnadc[V2009 >= 14]

  pnadc[, `:=`(
    # PEA: Economically Active Population (VD4001 == 1)
    pea = fifelse(VD4001 == 1, 1L, 0L),
    # Employed (VD4002 == 1)
    employed = fifelse(VD4002 == 1, 1L, 0L),
    # Unemployed (in labor force but not employed)
    unemployed = fifelse(VD4002 == 2, 1L, 0L)
  )]

  # Formality indicator (among employed)
  pnadc[, formal := fifelse(
    VD4009 %in% c(1L, 3L, 5L, 7L), 1L,
    fifelse(VD4009 %in% c(8L, 9L) & VD4012 == 1L, 1L, 0L)
  )]
  pnadc[VD4002 != 1, formal := NA_integer_]

  # Sex categories
  pnadc[, sexo := fifelse(V2007 == 1, "Men", "Women")]

  # Race categories
  pnadc[, raca := fcase(
    V2010 == 1, "White",
    V2010 == 2, "Black",
    V2010 == 4, "Brown",
    V2010 %in% c(3, 5), "Other",
    default = NA_character_
  )]
  # --- END VIGNETTE CODE: create-variables ---

  cat("Working-age population (14+):", format(nrow(pnadc), big.mark = ","), "\n")

  # ===========================================================================
  # STEP 4: COMPUTE AGGREGATED SERIES
  # ===========================================================================

  cat("\n=== Step 4: Compute Aggregated Series ===\n")

  # ---------------------------------------------------------------------------
  # QUARTERLY SERIES (using original V1028 weights)
  # ---------------------------------------------------------------------------

  quarterly_total <- pnadc[, .(
    pia = sum(V1028, na.rm = TRUE),
    pea = sum(pea * V1028, na.rm = TRUE),
    employed = sum(employed * V1028, na.rm = TRUE),
    unemployed = sum(unemployed * V1028, na.rm = TRUE),
    formal = sum(formal * V1028, na.rm = TRUE),

    unemployment_rate = sum(unemployed * V1028, na.rm = TRUE) /
                        sum(pea * V1028, na.rm = TRUE),
    participation_rate = sum(pea * V1028, na.rm = TRUE) /
                         sum(V1028, na.rm = TRUE),
    formalization_rate = sum(formal * V1028, na.rm = TRUE) /
                         sum(employed * V1028, na.rm = TRUE),
    employment_level = sum(employed * V1028, na.rm = TRUE) /
                       sum(V1028, na.rm = TRUE)
  ), by = .(Ano, Trimestre)]

  quarterly_total[, `:=`(
    period = as.Date(paste0(Ano, "-", (Trimestre - 1) * 3 + 2, "-15")),
    frequency = "Quarterly"
  )]

  # By gender
  quarterly_gender <- pnadc[!is.na(sexo), .(
    participation_rate = sum(pea * V1028, na.rm = TRUE) / sum(V1028, na.rm = TRUE),
    unemployment_rate = sum(unemployed * V1028, na.rm = TRUE) / sum(pea * V1028, na.rm = TRUE)
  ), by = .(Ano, Trimestre, sexo)]

  quarterly_gender[, `:=`(
    period = as.Date(paste0(Ano, "-", (Trimestre - 1) * 3 + 2, "-15")),
    frequency = "Quarterly"
  )]

  # By race
  quarterly_race <- pnadc[!is.na(raca) & raca %in% c("White", "Black", "Brown"), .(
    employment_level = sum(employed * V1028, na.rm = TRUE) / sum(V1028, na.rm = TRUE),
    unemployment_rate = sum(unemployed * V1028, na.rm = TRUE) / sum(pea * V1028, na.rm = TRUE)
  ), by = .(Ano, Trimestre, raca)]

  quarterly_race[, `:=`(
    period = as.Date(paste0(Ano, "-", (Trimestre - 1) * 3 + 2, "-15")),
    frequency = "Quarterly"
  )]

  # ---------------------------------------------------------------------------
  # MONTHLY SERIES (using weight_monthly)
  # ---------------------------------------------------------------------------

  pnadc_monthly <- pnadc[!is.na(weight_monthly)]

  monthly_total <- pnadc_monthly[, .(
    pia = sum(weight_monthly, na.rm = TRUE),
    pea = sum(pea * weight_monthly, na.rm = TRUE),
    employed = sum(employed * weight_monthly, na.rm = TRUE),
    unemployed = sum(unemployed * weight_monthly, na.rm = TRUE),
    formal = sum(formal * weight_monthly, na.rm = TRUE),

    unemployment_rate = sum(unemployed * weight_monthly, na.rm = TRUE) /
                        sum(pea * weight_monthly, na.rm = TRUE),
    participation_rate = sum(pea * weight_monthly, na.rm = TRUE) /
                         sum(weight_monthly, na.rm = TRUE),
    formalization_rate = sum(formal * weight_monthly, na.rm = TRUE) /
                         sum(employed * weight_monthly, na.rm = TRUE),
    employment_level = sum(employed * weight_monthly, na.rm = TRUE) /
                       sum(weight_monthly, na.rm = TRUE)
  ), by = ref_month_yyyymm]

  monthly_total[, `:=`(
    period = as.Date(paste0(ref_month_yyyymm %/% 100, "-",
                            ref_month_yyyymm %% 100, "-15")),
    frequency = "Monthly"
  )]

  # By gender
  monthly_gender <- pnadc_monthly[!is.na(sexo), .(
    participation_rate = sum(pea * weight_monthly, na.rm = TRUE) /
                         sum(weight_monthly, na.rm = TRUE),
    unemployment_rate = sum(unemployed * weight_monthly, na.rm = TRUE) /
                        sum(pea * weight_monthly, na.rm = TRUE)
  ), by = .(ref_month_yyyymm, sexo)]

  monthly_gender[, `:=`(
    period = as.Date(paste0(ref_month_yyyymm %/% 100, "-",
                            ref_month_yyyymm %% 100, "-15")),
    frequency = "Monthly"
  )]

  # By race
  monthly_race <- pnadc_monthly[!is.na(raca) & raca %in% c("White", "Black", "Brown"), .(
    employment_level = sum(employed * weight_monthly, na.rm = TRUE) /
                       sum(weight_monthly, na.rm = TRUE),
    unemployment_rate = sum(unemployed * weight_monthly, na.rm = TRUE) /
                        sum(pea * weight_monthly, na.rm = TRUE)
  ), by = .(ref_month_yyyymm, raca)]

  monthly_race[, `:=`(
    period = as.Date(paste0(ref_month_yyyymm %/% 100, "-",
                            ref_month_yyyymm %% 100, "-15")),
    frequency = "Monthly"
  )]

  cat("Quarterly observations:", nrow(quarterly_total), "\n")
  cat("Monthly observations:", nrow(monthly_total), "\n")

  # ===========================================================================
  # STEP 5: MINIMUM WAGE ANALYSIS DATA
  # ===========================================================================

  cat("\n=== Step 5: Minimum Wage Analysis (Exact Values) ===\n")

  # Historical minimum wage data for Brazil - month by month
  # Source: IPEADATA series 1739471028
  mw_history <- data.table(
    yyyymm = c(
      201201:201212,  # 2012: R$ 622
      201301:201312,  # 2013: R$ 678
      201401:201412,  # 2014: R$ 724
      201501:201512,  # 2015: R$ 788
      201601:201612,  # 2016: R$ 880
      201701:201712,  # 2017: R$ 937
      201801:201812,  # 2018: R$ 954
      201901:201912,  # 2019: R$ 998
      202001:202012,  # 2020: R$ 1,039 (Jan), R$ 1,045 (Feb-Dec)
      202101:202112,  # 2021: R$ 1,100
      202201:202212,  # 2022: R$ 1,212
      202301:202312,  # 2023: R$ 1,302 (Jan-Apr), R$ 1,320 (May-Dec)
      202401:202412,  # 2024: R$ 1,412
      202501:202512   # 2025: R$ 1,518
    ),
    mw_value = c(
      rep(622, 12),
      rep(678, 12),
      rep(724, 12),
      rep(788, 12),
      rep(880, 12),
      rep(937, 12),
      rep(954, 12),
      rep(998, 12),
      c(1039, rep(1045, 11)),
      rep(1100, 12),
      rep(1212, 12),
      c(rep(1302, 4), rep(1320, 8)),
      rep(1412, 12),
      rep(1518, 12)
    )
  )

  # Filter: Formal private sector employees age >= 18 with positive income
  mw_subset <- pnadc_monthly[
    VD4009 == 1 &                            # Formal private sector
    V2009 >= 18 &                            # Age 18+
    !is.na(VD4016) & VD4016 > 0 &            # Has habitual income
    !is.na(VD4017) & VD4017 > 0              # Has effective income
  ]

  cat("Formal private sector workers (age >= 18):", format(nrow(mw_subset), big.mark = ","), "\n")

  # Get unique MW values
  mw_values <- unique(mw_history$mw_value)
  cat("Unique MW values to track:", length(mw_values), "\n")

  # Compute proportion earning EXACTLY each MW value per month
  monthly_mw_exact <- rbindlist(lapply(mw_values, function(mw) {
    mw_subset[, .(
      mw_value = mw,
      pct_habitual = sum((VD4016 == mw) * weight_monthly, na.rm = TRUE) /
                     sum(weight_monthly, na.rm = TRUE),
      pct_effective = sum((VD4017 == mw) * weight_monthly, na.rm = TRUE) /
                      sum(weight_monthly, na.rm = TRUE),
      n_workers = sum(weight_monthly, na.rm = TRUE)
    ), by = .(ref_month_yyyymm)]
  }))

  # Add period column and order
  monthly_mw_exact[, period := as.Date(paste0(
    ref_month_yyyymm %/% 100, "-",
    ref_month_yyyymm %% 100, "-15"
  ))]
  setorder(monthly_mw_exact, period, mw_value)

  # Mark MW adjustment months
  mw_history[, mw_change := mw_value != shift(mw_value, fill = mw_value[1])]
  mw_adjustment_months <- mw_history[mw_change == TRUE, yyyymm]

  cat("Exact MW tracking data rows:", nrow(monthly_mw_exact), "\n")
  cat("MW adjustment events:", length(mw_adjustment_months), "\n")

  # ===========================================================================
  # STEP 5b: SUB-MONTHLY ANALYSIS - FORTNIGHT COVID EXAMPLE
  # ===========================================================================
  # VIGNETTE CODE: fortnight-aggregate

  cat("\n=== Step 5b: Fortnight COVID Analysis (2020 Q1-Q2, using Experimental Strategies) ===\n")

  # pnadc_fortnight was created in Step 3c via:
  #   pnadc_apply_periods(pnadc_clean, crosswalk_experimental, calibration_unit = "fortnight")
  # It contains weight_fortnight calibrated to population totals.

  # --- VIGNETTE CODE: fortnight-aggregate ---
  pnadc_fn_covid <- pnadc_fortnight[
    !is.na(weight_fortnight) & Ano == 2020 & Trimestre %in% 1:2 & V2009 >= 14
  ]

  fortnight_covid <- pnadc_fn_covid[, .(
    participation_rate = sum((VD4001 == 1) * weight_fortnight, na.rm = TRUE) /
                         sum(weight_fortnight, na.rm = TRUE),
    n_obs = .N
  ), by = .(ref_fortnight_yyyyff)]

  fortnight_covid[, period := as.Date(paste0(
    ref_fortnight_yyyyff %/% 100, "-",
    ((ref_fortnight_yyyyff %% 100) - 1L) %/% 2L + 1L, "-",
    fifelse(ref_fortnight_yyyyff %% 2L == 1L, "1", "16")
  ))]
  # --- END VIGNETTE CODE: fortnight-aggregate ---

  setorder(fortnight_covid, period)

  cat("Observations with determined fortnight (2020 Q1-Q2):",
      format(nrow(pnadc_fn_covid), big.mark = ","), "\n")
  cat("Fortnight COVID series rows:", nrow(fortnight_covid), "\n")
  if (nrow(fortnight_covid) > 0) {
    cat("Period range:",
        format(min(fortnight_covid$period), "%Y-%m-%d"), "to",
        format(max(fortnight_covid$period), "%Y-%m-%d"), "\n")
  }

  # END VIGNETTE CODE: fortnight-aggregate

  # ===========================================================================
  # STEP 5c: SUB-MONTHLY ANALYSIS - WEEKLY CARNIVAL EXAMPLE
  # ===========================================================================
  # VIGNETTE CODE: carnival-aggregate

  cat("\n=== Step 5c: Weekly Carnival Analysis (2019, 2022-2025 Q1) ===\n")

  # pnadc_weekly was created in Step 3d via:
  #   pnadc_apply_periods(copy(pnadc_clean), crosswalk_experimental, calibration_unit = "week")
  # It contains weight_weekly calibrated to population totals.

  # --- VIGNETTE CODE: carnival-aggregate ---
  carnival_years <- c(2019L, 2022L, 2023L, 2024L, 2025L)
  pnadc_wk <- pnadc_weekly[
    !is.na(weight_weekly) & Trimestre == 1L & Ano %in% carnival_years &
    V2009 >= 14 & VD4002 == 1 & !is.na(VD4035)
  ]

  carnival_weeks <- data.table(
    Ano = carnival_years,
    carnival_iso_week = c(10L, 9L, 8L, 7L, 10L)
  )

  pnadc_wk[, iso_week := ref_week_yyyyww %% 100L]
  pnadc_wk <- merge(pnadc_wk, carnival_weeks, by = "Ano", all.x = TRUE)
  pnadc_wk[, is_carnival_week := (iso_week == carnival_iso_week)]
  pnadc_wk[, is_carnival_state := UF %in% c(29L, 33L, 26L)]

  weekly_carnival <- pnadc_wk[, .(
    avg_hours_worked = sum(VD4035 * weight_weekly, na.rm = TRUE) /
                       sum(weight_weekly, na.rm = TRUE),
    n_obs = .N
  ), by = .(is_carnival_week, is_carnival_state)]
  # --- END VIGNETTE CODE: carnival-aggregate ---

  cat("Carnival aggregation rows:", nrow(weekly_carnival), "\n")

  # Also compute detailed per-week series for time series figure
  weekly_carnival_detail <- pnadc_wk[, .(
    avg_hours_worked = sum(VD4035 * weight_weekly, na.rm = TRUE) /
                       sum(weight_weekly, na.rm = TRUE),
    total_weight = sum(weight_weekly, na.rm = TRUE),
    n_obs = .N
  ), by = .(Ano, ref_week_yyyyww, iso_week, is_carnival_week, is_carnival_state)]

  weekly_carnival_detail[, region := fifelse(is_carnival_state, "Carnival States (BA/RJ/PE)", "Other States")]
  setorder(weekly_carnival_detail, Ano, ref_week_yyyyww)

  cat("Weekly Carnival detail rows:", nrow(weekly_carnival_detail), "\n")
  cat("Years:", paste(unique(weekly_carnival_detail$Ano), collapse = ", "), "\n")

  # END VIGNETTE CODE: carnival-aggregate

  # ===========================================================================
  # STEP 6: SAVE AGGREGATED DATA
  # ===========================================================================

  cat("\n=== Step 6: Save Aggregated Data ===\n")

  # Save to processed directory (fst format for speed)
  fst::write_fst(quarterly_total, paste0(processed_dir, "series_quarterly_total.fst"))
  fst::write_fst(quarterly_gender, paste0(processed_dir, "series_quarterly_gender.fst"))
  fst::write_fst(quarterly_race, paste0(processed_dir, "series_quarterly_race.fst"))
  fst::write_fst(monthly_total, paste0(processed_dir, "series_monthly_total.fst"))
  fst::write_fst(monthly_gender, paste0(processed_dir, "series_monthly_gender.fst"))
  fst::write_fst(monthly_race, paste0(processed_dir, "series_monthly_race.fst"))
  fst::write_fst(monthly_mw_exact, paste0(processed_dir, "series_monthly_mw_exact.fst"))
  fst::write_fst(mw_history, paste0(processed_dir, "mw_history.fst"))

  # Save sub-monthly series if available
  if (!is.null(fortnight_covid)) {
    fst::write_fst(fortnight_covid, paste0(processed_dir, "series_fortnight_covid.fst"))
    cat("Saved: series_fortnight_covid.fst\n")
  }
  if (!is.null(weekly_carnival)) {
    fst::write_fst(weekly_carnival, paste0(processed_dir, "series_weekly_carnival.fst"))
    fst::write_fst(weekly_carnival_detail, paste0(processed_dir, "series_weekly_carnival_detail.fst"))
    cat("Saved: series_weekly_carnival.fst + detail\n")
  }

  cat("Saved data to:", processed_dir, "\n")

  # Store observation count before cleanup (needed for metadata)
  n_obs <- nrow(pnadc)

  # Clean up microdata to free memory
  rm(pnadc, pnadc_clean, pnadc_fortnight, pnadc_weekly, pnadc_monthly,
     pnadc_fn_covid, pnadc_wk, mw_subset, result,
     crosswalk, crosswalk_experimental)
  gc()

}  # End of processing_mode block (load_microdata or full_rebuild)

# =============================================================================
# STEP 7: GENERATE FIGURES FOR VIGNETTE
# =============================================================================

cat("\n=== Step 7: Generate Vignette Figures ===\n")

# -----------------------------------------------------------------------------
# FIGURE 1: COVID-19 Unemployment (fig-covid-unemployment.png)
# -----------------------------------------------------------------------------
# VIGNETTE CODE: covid-unemployment
# Filter to COVID period (Oct 2019 - Jan 2022)
covid_quarterly <- quarterly_total[period >= "2019-10-01" & period <= "2022-01-01"]
covid_monthly <- monthly_total[period >= "2019-10-01" & period <= "2022-01-01"]

# Find the unemployment peaks
peak_quarterly <- covid_quarterly[which.max(unemployment_rate)]
peak_monthly <- covid_monthly[which.max(unemployment_rate)]

# Create the comparison plot
ggplot() +
  # Quarterly as step function (emphasizes moving-average nature)
  geom_step(data = covid_quarterly,
            aes(x = period, y = unemployment_rate),
            color = "steelblue", linewidth = 1.2, direction = "mid") +
  geom_point(data = covid_quarterly,
             aes(x = period, y = unemployment_rate),
             color = "steelblue", size = 3) +

  # Monthly as line
  geom_line(data = covid_monthly,
            aes(x = period, y = unemployment_rate),
            color = "darkred", linewidth = 0.9) +
  geom_point(data = covid_monthly,
             aes(x = period, y = unemployment_rate),
             color = "darkred", size = 1.5) +

  # Annotate monthly peak
  annotate("segment",
           x = peak_monthly$period, xend = peak_monthly$period,
           y = peak_monthly$unemployment_rate + 0.005,
           yend = peak_monthly$unemployment_rate + 0.02,
           arrow = arrow(length = unit(0.2, "cm")), color = "darkred") +
  annotate("text",
           x = peak_monthly$period,
           y = peak_monthly$unemployment_rate + 0.022,
           label = paste0("Monthly peak: ", format(peak_monthly$period, "%b %Y"), "\n",
                         sprintf("%.1f%%", peak_monthly$unemployment_rate * 100)),
           size = 3.2, color = "darkred", hjust = 0.5) +

  # Annotate quarterly peak
  annotate("segment",
           x = peak_quarterly$period + 30, xend = peak_quarterly$period + 30,
           y = peak_quarterly$unemployment_rate - 0.005,
           yend = peak_quarterly$unemployment_rate - 0.02,
           arrow = arrow(length = unit(0.2, "cm")), color = "steelblue") +
  annotate("text",
           x = peak_quarterly$period + 30,
           y = peak_quarterly$unemployment_rate - 0.025,
           label = paste0("Quarterly peak: ", format(peak_quarterly$period, "%b %Y"), "\n",
                         sprintf("%.1f%%", peak_quarterly$unemployment_rate * 100)),
           size = 3.2, color = "steelblue", hjust = 0.5) +

  # Scales and labels
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     limits = c(0.10, 0.17)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
  labs(
    title = "COVID-19 Unemployment Spike: Monthly vs Quarterly",
    subtitle = "Monthly data (red) shows the true peak; quarterly (blue) averages it away",
    x = NULL, y = "Unemployment Rate",
    caption = "Source: PNADC/IBGE. Quarterly shown as step function to emphasize moving-average nature."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
# END VIGNETTE CODE: covid-unemployment

ggsave(file.path(fig_dir, "fig-covid-unemployment.png"),
       width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
cat("Saved: fig-covid-unemployment.png\n")

# -----------------------------------------------------------------------------
# FIGURE 2: 2014-2017 Recession (fig-recession-detail.png)
# -----------------------------------------------------------------------------
# VIGNETTE CODE: recession-detail
# Filter to recession period (Jun 2014 - Jun 2017)
recession_quarterly <- quarterly_total[period >= "2014-06-01" & period <= "2017-06-01"]
recession_monthly <- monthly_total[period >= "2014-06-01" & period <= "2017-06-01"]

# Find the peak
peak_recession <- recession_monthly[which.max(unemployment_rate)]

# Calculate month-over-month changes
setorder(recession_monthly, period)
recession_monthly[, change := unemployment_rate - shift(unemployment_rate)]

# Identify months with large jumps (> 1 percentage point)
big_jumps <- recession_monthly[!is.na(change) & change > 0.01]

# Create the plot
ggplot() +
  # Quarterly
  geom_step(data = recession_quarterly,
            aes(x = period, y = unemployment_rate),
            color = "steelblue", linewidth = 1, direction = "mid", alpha = 0.7) +
  geom_point(data = recession_quarterly,
             aes(x = period, y = unemployment_rate),
             color = "steelblue", size = 2.5) +

  # Monthly
  geom_line(data = recession_monthly,
            aes(x = period, y = unemployment_rate),
            color = "darkred", linewidth = 0.8) +
  geom_point(data = recession_monthly,
             aes(x = period, y = unemployment_rate),
             color = "darkred", size = 1.2) +

  # Highlight big monthly jumps
  geom_point(data = big_jumps,
             aes(x = period, y = unemployment_rate),
             color = "darkred", size = 3, shape = 21, fill = "yellow", stroke = 1.5) +

  # Mark the peak
  annotate("point", x = peak_recession$period, y = peak_recession$unemployment_rate,
           color = "darkred", size = 4, shape = 18) +
  annotate("text",
           x = peak_recession$period + 45, y = peak_recession$unemployment_rate,
           label = paste0("Peak: ", format(peak_recession$period, "%b %Y"), "\n",
                         sprintf("%.1f%%", peak_recession$unemployment_rate * 100)),
           size = 3, hjust = 0, color = "darkred") +

  # Scales and labels
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(0.06, 0.14, 0.02)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title = "2014-2017 Recession: Month-by-Month Unemployment Rise",
    subtitle = "Yellow circles: months with >1 p.p. unemployment increase. Monthly data (red) vs quarterly (blue).",
    x = NULL, y = "Unemployment Rate",
    caption = "Source: PNADC/IBGE. Monthly data reveals sudden jumps hidden in quarterly averages."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
# END VIGNETTE CODE: recession-detail

ggsave(file.path(fig_dir, "fig-recession-detail.png"),
       width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
cat("Saved: fig-recession-detail.png\n")

# -----------------------------------------------------------------------------
# FIGURES 3-5: REMOVED FROM VIGNETTE (kept for archival purposes)
# Gender Gap, Racial Inequality, and Formalization Paradox examples were
# removed from the vignette. Code preserved below but commented out.
# To re-enable, uncomment the relevant sections.
# -----------------------------------------------------------------------------

# # -----------------------------------------------------------------------------
# # FIGURE 3: Gender Gap During COVID (fig-gender-gap.png) - REMOVED
# # -----------------------------------------------------------------------------
# # VIGNETTE CODE: gender-gap
# # [Code removed from active generation - see git history for original]

# # -----------------------------------------------------------------------------
# # FIGURE 4: Racial Inequality (fig-racial-gap.png) - REMOVED
# # -----------------------------------------------------------------------------
# # VIGNETTE CODE: racial-gap
# # [Code removed from active generation - see git history for original]

# # -----------------------------------------------------------------------------
# # FIGURE 5: Formalization Paradox (fig-formalization.png) - REMOVED
# # -----------------------------------------------------------------------------
# # VIGNETTE CODE: formalization
# # [Code removed from active generation - see git history for original]

# -----------------------------------------------------------------------------
# SETUP: Create shared objects for all MW figures
# (These are created once and used by all MW figures below)
# -----------------------------------------------------------------------------
# Create long format for plotting
mw_long <- melt(monthly_mw_exact,
                id.vars = c("ref_month_yyyymm", "period", "mw_value", "n_workers"),
                measure.vars = c("pct_habitual", "pct_effective"),
                variable.name = "income_type",
                value.name = "pct_at_mw")

# Add readable labels
mw_long[, income_label := fifelse(income_type == "pct_habitual",
                                   "Habitual Income (current month)",
                                   "Effective Income (previous month)")]

# Get adjustment dates for reference lines
adj_dates <- as.Date(paste0(mw_adjustment_months %/% 100, "-",
                             mw_adjustment_months %% 100, "-15"))

# -----------------------------------------------------------------------------
# FIGURE 3: MW 2013 Transition (fig-mw-2013-transition.png)
# This is the first MW example in the vignette (simple case)
# -----------------------------------------------------------------------------
# VIGNETTE CODE: mw-2013-transition
# Create long format for plotting (needed for all MW examples)
mw_long <- melt(monthly_mw_exact,
                id.vars = c("ref_month_yyyymm", "period", "mw_value", "n_workers"),
                measure.vars = c("pct_habitual", "pct_effective"),
                variable.name = "income_type",
                value.name = "pct_at_mw")

# Add readable labels
mw_long[, income_label := fifelse(income_type == "pct_habitual",
                                   "Habitual Income (current month)",
                                   "Effective Income (previous month)")]

# Get adjustment dates for reference lines
adj_dates <- as.Date(paste0(mw_adjustment_months %/% 100, "-",
                             mw_adjustment_months %% 100, "-15"))

# Filter to the first transition period (Oct 2012 - Apr 2013)
mw_2013 <- mw_long[period >= "2012-10-15" & period <= "2013-04-15" &
                   mw_value %in% c(622, 678)]

# Create the plot
ggplot(mw_2013,
       aes(x = period, y = pct_at_mw, color = factor(mw_value), linetype = factor(mw_value))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +

  # Mark the adjustment date (January 2013)
  geom_vline(xintercept = as.Date("2013-01-15"),
             linetype = "dashed", linewidth = 0.8, color = "gray40") +

  # Facet by income type
  facet_wrap(~ income_label, ncol = 2) +

  # Scales and labels
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  scale_color_manual(values = c("622" = "#b2182b", "678" = "#2166ac"),
                     labels = c("622" = "R$ 622 (old)", "678" = "R$ 678 (new)"),
                     name = "MW Value") +
  scale_linetype_manual(values = c("622" = "solid", "678" = "solid"), guide = "none") +
  labs(
    title = "MW Transition: January 2013 (R$ 622 -> R$ 678)",
    subtitle = "Habitual income shows transition in January; Effective income shows same pattern in February",
    x = NULL, y = "Share Earning Exact MW",
    caption = "Vertical line = MW adjustment. Habitual income refers to current month; effective to previous month."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )
# END VIGNETTE CODE: mw-2013-transition

ggsave(file.path(fig_dir, "fig-mw-2013-transition.png"),
       width = 10, height = 5, dpi = fig_dpi, bg = "white")
cat("Saved: fig-mw-2013-transition.png\n")

# -----------------------------------------------------------------------------
# FIGURE 8: MW 2020 Double Adjustment (fig-mw-2020-double.png)
# -----------------------------------------------------------------------------
# VIGNETTE CODE: mw-2020-double
# Filter to 2020 double adjustment period (Jul 2019 - Jul 2020)
mw_2020 <- mw_long[period >= "2019-07-15" & period <= "2020-07-15" &
                   mw_value %in% c(998, 1039, 1045)]

# Get adjustment dates in this period
adj_2020 <- adj_dates[adj_dates >= "2019-07-15" & adj_dates <= "2020-07-15"]

# Create the plot
ggplot(mw_2020,
       aes(x = period, y = pct_at_mw, color = factor(mw_value), linetype = factor(mw_value))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +

  # Mark adjustment months
  geom_vline(xintercept = adj_2020, linetype = "dashed", linewidth = 0.8, color = "gray40") +

  # Facet by income type
  facet_wrap(~ income_label, ncol = 2) +

  # Scales and labels
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
  scale_color_manual(values = c("998" = "#b2182b", "1039" = "#7b3294", "1045" = "#2166ac"),
                     labels = c("998" = "R$ 998 (2019)", "1039" = "R$ 1,039 (Jan 2020)",
                               "1045" = "R$ 1,045 (Feb 2020+)"),
                     name = "MW Value") +
  scale_linetype_manual(values = c("998" = "solid", "1039" = "solid", "1045" = "solid"),
                        guide = "none") +
  labs(
    title = "The 2020 Double Adjustment: A Demanding Test",
    subtitle = "R$ 998 -> R$ 1,039 (Jan) -> R$ 1,045 (Feb). Three values, two transitions, one-month lag preserved.",
    x = NULL, y = "Share Earning Exact MW",
    caption = "Vertical lines = MW adjustments. The short-lived R$ 1,039 value (one month only) creates a sharp test."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )
# END VIGNETTE CODE: mw-2020-double

ggsave(file.path(fig_dir, "fig-mw-2020-double.png"),
       width = 10, height = 5, dpi = fig_dpi, bg = "white")
cat("Saved: fig-mw-2020-double.png\n")

# -----------------------------------------------------------------------------
# FIGURE 5: MW Big Picture (fig-mw-big-picture.png)
# This is the last MW example in the vignette (full series)
# -----------------------------------------------------------------------------
# VIGNETTE CODE: mw-big-picture
# Create color palette (blue to red gradient)
mw_vals <- sort(unique(monthly_mw_exact$mw_value))
n_mw <- length(mw_vals)
palette_fn <- colorRampPalette(c("#2166ac", "#67a9cf", "#d1e5f0",
                                  "#fddbc7", "#ef8a62", "#b2182b"))
mw_colors <- palette_fn(n_mw)
names(mw_colors) <- as.character(mw_vals)

# Filter to MW values with meaningful presence (>0.1% at some point)
mw_long[, max_pct := max(pct_at_mw), by = mw_value]
mw_long_filtered <- mw_long[max_pct > 0.001]

# Create the plot
ggplot(mw_long_filtered,
       aes(x = period, y = pct_at_mw, color = factor(mw_value), group = mw_value)) +
  geom_line(linewidth = 0.6, alpha = 0.8) +

  # Mark MW adjustment months
  geom_vline(xintercept = adj_dates, linetype = "dashed", alpha = 0.3, color = "gray40") +

  # Facet by income type
  facet_wrap(~ income_label, ncol = 1) +

  # Scales and labels
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = mw_colors, name = "MW Value (R$)") +
  labs(
    title = "Share of Formal Workers Earning Each Exact MW Value",
    subtitle = "Each line = one MW value. Vertical lines = MW adjustments. Pattern shifted 1 month between panels.",
    x = NULL, y = "Share of Workers",
    caption = "Source: PNADC/IBGE. Formal private sector employees age 18+. All calculations use weight_monthly."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  guides(color = guide_legend(ncol = 2))
# END VIGNETTE CODE: mw-big-picture

ggsave(file.path(fig_dir, "fig-mw-big-picture.png"),
       width = 12, height = 7, dpi = fig_dpi, bg = "white")
cat("Saved: fig-mw-big-picture.png\n")

# -----------------------------------------------------------------------------
# FIGURE 6: COVID Fortnight Comparison (fig-covid-fortnight-comparison.png)
# -----------------------------------------------------------------------------
# VIGNETTE CODE: covid-fortnight

if (!is.null(fortnight_covid) && nrow(fortnight_covid) > 0) {
  cat("\nGenerating COVID fortnight figure...\n")

  # --- VIGNETTE CODE: covid-fortnight ---
  # Monthly comparison data
  covid_monthly_part <- monthly_total[
    period >= "2020-01-01" & period <= "2020-06-30",
    .(period, participation_rate)
  ]

  cat("Fortnight data points (n >= 300):", nrow(fortnight_covid[n_obs >= 300]), "\n")

  # Create plot showing participation rate at both resolutions
  p_covid_fn <- ggplot() +
    # Monthly as reference line
    geom_line(data = covid_monthly_part,
              aes(x = period, y = participation_rate),
              color = "darkred", linewidth = 1, alpha = 0.7) +
    geom_point(data = covid_monthly_part,
               aes(x = period, y = participation_rate),
               color = "darkred", size = 3) +

    # Fortnightly
    geom_point(data = fortnight_covid[n_obs >= 300],
               aes(x = period, y = participation_rate, size = n_obs),
               color = "#ff7f0e", alpha = 0.8) +
    scale_size_continuous(range = c(2, 5), guide = "none") +

    # Mark March fortnight boundary (lockdown transition)
    geom_vline(xintercept = as.Date("2020-03-16"),
               linetype = "dotted", color = "gray40", linewidth = 0.8) +
    annotate("text", x = as.Date("2020-03-16"), y = 0.63,
             label = "Mar 16\n(Lockdowns begin)",
             size = 2.8, hjust = -0.1, color = "gray30") +

    # Scales and labels
    scale_y_continuous(labels = percent_format(accuracy = 0.1),
                       limits = c(0.52, 0.64)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
    labs(
      title = "COVID-19 Labor Force Participation: Monthly vs Fortnightly",
      subtitle = "Fortnightly points (orange) show discontinuity; point size = sample size (experimental strategies)",
      x = NULL, y = "Labor Force Participation Rate",
      caption = paste0(
        "Monthly (red line): ~350,000 obs/month | Fortnightly (orange points): experimental strategies boost coverage\n",
        "Source: PNADC/IBGE. Uses pnadc_experimental_periods(conf=0.85, upa=0.80) for ~12-13% fortnight rate. Only fortnights with n >= 300 shown."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(file.path(fig_dir, "fig-covid-fortnight-comparison.png"),
         plot = p_covid_fn,
         width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  cat("Saved: fig-covid-fortnight-comparison.png\n")

} else {
  cat("Skipping COVID fortnight figure (no data)\n")
}
# END VIGNETTE CODE: covid-fortnight

# -----------------------------------------------------------------------------
# FIGURE 7: Carnival Weekly DiD (fig-carnival-weekly-did.png)
# -----------------------------------------------------------------------------
# VIGNETTE CODE: carnival-weekly

if (!is.null(weekly_carnival) && nrow(weekly_carnival) > 0) {
  cat("\nGenerating Carnival weekly figure...\n")

  # weekly_carnival is already the pooled 2x2 summary (matching vignette carnival-aggregate)
  # --- VIGNETTE CODE: carnival-weekly ---
  p_carnival <- ggplot(weekly_carnival,
         aes(x = fifelse(is_carnival_week, "Carnival Week", "Other Weeks"),
             y = avg_hours_worked,
             fill = fifelse(is_carnival_state, "Carnival States (BA/RJ/PE)", "Other States"))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +

    # Add value labels
    geom_text(aes(label = sprintf("%.1f h", avg_hours_worked)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +

    scale_y_continuous(limits = c(0, 45),
                       expand = c(0, 0)) +
    scale_fill_manual(values = c("Carnival States (BA/RJ/PE)" = "#ff7f0e",
                                 "Other States" = "#1f77b4")) +
    labs(
      title = "Average Hours Worked by Week Type and Region",
      subtitle = "Pooled data from Q1 2019, 2022-2025 (Carnival years, experimental strategies)",
      x = NULL, y = "Average Hours Worked (per week)",
      fill = "Region",
      caption = paste0(
        "Uses pnadc_experimental_periods(strategy='both', conf=0.85, upa=0.80) for ~5-6% weekly rate. Pooled across 5 years.\n",
        "Carnival states: Bahia, Rio de Janeiro, Pernambuco. Source: PNADC/IBGE."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  ggsave(file.path(fig_dir, "fig-carnival-weekly-did.png"),
         plot = p_carnival,
         width = 8, height = 6, dpi = fig_dpi, bg = "white")
  cat("Saved: fig-carnival-weekly-did.png\n")

  # Also create time series plot for Carnival weeks - hours worked
  # Uses weekly_carnival_detail (per-week data, not the pooled summary)
  if (!is.null(weekly_carnival_detail) && nrow(weekly_carnival_detail) > 0) {
  p_carnival_ts <- ggplot(weekly_carnival_detail[is_carnival_state == TRUE],
         aes(x = iso_week, y = avg_hours_worked, color = factor(Ano))) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +

    # Highlight Carnival weeks
    geom_point(data = weekly_carnival_detail[is_carnival_state == TRUE & is_carnival_week == TRUE],
               aes(x = iso_week, y = avg_hours_worked),
               shape = 21, fill = "yellow", size = 4, stroke = 1.5) +

    scale_y_continuous(limits = c(30, 45)) +
    scale_x_continuous(breaks = seq(1, 14, 2)) +
    scale_color_viridis_d(option = "plasma", end = 0.9) +
    labs(
      title = "Weekly Average Hours Worked in Carnival States (Q1)",
      subtitle = "Yellow circles = Carnival week. Each line = one year (experimental strategies).",
      x = "ISO Week Number", y = "Average Hours Worked",
      color = "Year",
      caption = "Carnival states: BA, RJ, PE. Experimental strategies (conf=0.85, upa=0.80) ~5-6% weekly rate."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )

  ggsave(file.path(fig_dir, "fig-carnival-weekly-timeseries.png"),
         plot = p_carnival_ts,
         width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  cat("Saved: fig-carnival-weekly-timeseries.png\n")
  }  # end weekly_carnival_detail null guard

} else {
  cat("Skipping Carnival weekly figures (no data)\n")
}
# END VIGNETTE CODE: carnival-weekly

# =============================================================================
# STEP 8: COPY FIGURES TO PACKAGE
# =============================================================================

cat("\n=== Step 8: Copy Figures to Package ===\n")

# Copy all PNG files to package vignettes folder
fig_files <- list.files(fig_dir, pattern = "\\.png$", full.names = TRUE)
file.copy(fig_files, pkg_fig_dir, overwrite = TRUE)
cat("Copied", length(fig_files), "figures to:", pkg_fig_dir, "\n")

# =============================================================================
# STEP 9: SAVE METADATA
# =============================================================================

cat("\n=== Step 9: Save Metadata ===\n")

# Helper function to format period as "YYYY-QN"
format_period_quarter <- function(date) {
  yr <- as.integer(format(date, "%Y"))
  mo <- as.integer(format(date, "%m"))
  qtr <- ceiling(mo / 3)
  paste0(yr, "-Q", qtr)
}

# Save metadata for documentation
# When using cached aggregates, some values are not available
if (processing_mode == "load_aggregates") {
  # Load previous metadata if available, or create minimal version
  old_metadata_file <- file.path(table_dir, "vignette_metadata.rds")
  if (file.exists(old_metadata_file)) {
    metadata <- readRDS(old_metadata_file)
    metadata$regenerated_date <- Sys.Date()
    metadata$figures <- basename(fig_files)
    cat("Updated existing metadata with new figure list\n")
  } else {
    # Infer period from monthly_total
    metadata <- list(
      determination_rate = NA,
      n_observations = NA,
      period_start = format_period_quarter(min(monthly_total$period)),
      period_end = format_period_quarter(max(monthly_total$period)),
      generated_date = Sys.Date(),
      figures = basename(fig_files)
    )
    cat("Created minimal metadata (cached aggregates mode)\n")
  }
} else {
  # Use n_obs stored before rm() and derive period from monthly_total
  metadata <- list(
    determination_rate = det_rate,
    n_observations = n_obs,
    period_start = format_period_quarter(min(monthly_total$period)),
    period_end = format_period_quarter(max(monthly_total$period)),
    generated_date = Sys.Date(),
    figures = basename(fig_files)
  )
  cat("Saved full metadata\n")
}

saveRDS(metadata, file.path(table_dir, "vignette_metadata.rds"))

cat("\n=== Done! ===\n")
cat("Data saved to:", processed_dir, "\n")
cat("Figures saved to:", fig_dir, "\n")
cat("Figures copied to:", pkg_fig_dir, "\n")
