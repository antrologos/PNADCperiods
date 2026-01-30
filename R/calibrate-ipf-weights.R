# ==============================================================================
# IPF Weight Calibration for PNADC Data
# ==============================================================================
#
# Alternative calibration using Iterative Proportional Fitting (IPF) instead of
# the nested-cells raking approach in pnadc_apply_periods(). This function
# adjusts weights to match multiple independent margins simultaneously.
#
# CRITICAL DESIGN: Target proportions are derived from the FULL quarterly sample
# (all observations, not just determined ones). This corrects for selection bias
# in the month/fortnight/week determined sample, ensuring temporal consistency
# where sub-period weighted averages match parent period results.
#
# Margins used:
#   - Age groups (9 categories)
#   - Gender (2 categories)
#   - Household size (4 categories)
#   - Post-stratum (posest)
#   - Rural/Urban (optional, if V1022 present)
#
# Final weights are scaled to match SIDRA total population for each period.
#
# ==============================================================================


#' IPF Weight Calibration for PNADC Data
#'
#' Alternative weight calibration using Iterative Proportional Fitting (IPF).
#' Unlike the nested-cells raking in \code{pnadc_apply_periods()}, this function
#' adjusts weights to simultaneously match multiple independent margins:
#' age groups, gender, household size, posest, and optionally rural/urban.
#'
#' @param data A data.table with period assignments from \code{pnadc_apply_periods()}
#'   or \code{pnadc_identify_periods()}. Must contain the reference period columns
#'   (e.g., \code{ref_month_yyyymm}) and demographic variables.
#' @param weight_var Character. Name of the original weight column, either
#'   \code{"V1028"} (quarterly) or \code{"V1032"} (annual). Default: \code{"V1028"}.
#' @param calibration_unit Character. Time unit for calibration: \code{"month"},
#'   \code{"fortnight"}, or \code{"week"}. Default: \code{"month"}.
#' @param margins Character vector of margin names to use. Available margins:
#'   \code{"age_group"} (9 age categories), \code{"gender"} (male/female),
#'   \code{"hh_group"} (household size: 1, 2, 3-4, 5+), \code{"posest"}
#'   (IBGE post-stratum), \code{"rural_urban"} (requires V1022).
#'   Default: \code{c("age_group", "gender", "hh_group", "posest")}.
#'   Use fewer/simpler margins for sparse data (fortnight, week).
#' @param target_totals Optional data.table with external population totals from
#'   \code{fetch_monthly_population()}. If provided, final weights are scaled to
#'   match these totals. If NULL, weights preserve the sample's weighted totals.
#' @param bounds Numeric vector of length 2. Weight adjustment bounds as ratios
#'   of the original weight, e.g., \code{c(0.1, 10)} allows weights to be reduced
#'   to 10\% or increased to 1000\% of the original. Default: \code{c(0.1, 10)}.
#' @param max_iter Integer. Maximum number of IPF iterations. Default: 300.
#' @param tol Numeric. Convergence tolerance - iteration stops when the maximum
#'   relative change in weights is below this value. Default: 1e-6.
#' @param bias_correction Character. Method for correcting selection bias in the
#'   determined sample. Options: \code{"none"} (default, use standard weights),
#'   \code{"propensity"} (use inverse probability weighting based on propensity
#'   scores for being in the determined sample).
#' @param method Character. IPF method to use. Options: \code{"margin"} (default,
#'   iterate over independent margins), \code{"cell"} (iterate over hierarchical
#'   cells like nested raking, preserving anchor-period structure).
#' @param min_cell_size Integer. Minimum cell size threshold for adaptive margin
#'   collapsing. If any margin has cells smaller than this, the margin categories
#'   will be collapsed (e.g., 9 age groups -> 4). Default: 30. Set to 0 to disable.
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return The input data.table with a new weight column added:
#'   \code{weight_ipf_monthly}, \code{weight_ipf_fortnightly}, or
#'   \code{weight_ipf_weekly}, depending on \code{calibration_unit}.
#'
#' @details
#' The IPF algorithm iteratively adjusts weights to match target proportions for
#' each margin. \strong{Critically, target proportions are derived from the FULL
#' quarterly sample} (all observations, not just those with determined reference
#' periods). This corrects for selection bias in the month/fortnight/week
#' determined sample.
#'
#' The algorithm flow:
#' \enumerate{
#'   \item Create margin variables on ALL observations
#'   \item (Optional) Compute propensity scores and IPW for bias correction
#'   \item Calculate target proportions from FULL quarterly sample (unbiased)
#'   \item (Optional) Adaptively collapse sparse margins
#'   \item Filter to determined observations only
#'   \item Run IPF iterations (margin-based or cell-based)
#'   \item Apply weight bounds
#'   \item Scale to SIDRA population totals (each period to its containing month)
#' }
#'
#' Available margins: age_group (9 categories from V2009), gender (V2007),
#' hh_group (household size: 1, 2, 3-4, 5+), posest (IBGE post-stratum),
#' rural_urban (V1022, if available).
#'
#' For sparse data (fortnight/week), use fewer margins to ensure convergence.
#'
#' @section Bias Correction:
#' The \code{bias_correction = "propensity"} option models the probability of an
#' observation being in the determined sample using logistic regression with
#' age group, gender, posest (post-stratum), household size group, and rural/urban
#' as predictors. Inverse probability weights (IPW) are then used as initial
#' weights for IPF, directly addressing selection bias.
#'
#' @section Cell-Based IPF:
#' The \code{method = "cell"} option runs IPF within hierarchical cells (similar
#' to nested raking) rather than matching independent margins. This preserves
#' anchor-period totals within cells and often produces better temporal consistency.
#'
#' @section Adaptive Margins:
#' When \code{min_cell_size > 0}, margins with sparse cells are automatically
#' collapsed to prevent extreme weight adjustments. For example, 9 age groups
#' may be collapsed to 4 broader categories for fortnight/week calibration.
#'
#' @examples
#' \dontrun{
#' library(PNADCperiods)
#'
#' # Standard workflow
#' crosswalk <- pnadc_identify_periods(pnadc_stacked)
#' result <- pnadc_apply_periods(pnadc_2020, crosswalk,
#'                                weight_var = "V1028",
#'                                calibrate = FALSE)
#'
#' # Apply IPF calibration
#' sidra_pop <- fetch_monthly_population(2020, 2020)
#' result_ipf <- pnadc_ipf_calibrate(result,
#'                                    weight_var = "V1028",
#'                                    calibration_unit = "fortnight",
#'                                    target_totals = sidra_pop)
#'
#' # Compare with standard calibration
#' result_std <- pnadc_apply_periods(pnadc_2020, crosswalk,
#'                                    calibrate = TRUE,
#'                                    calibration_unit = "fortnight")
#' }
#'
#' @export
pnadc_ipf_calibrate <- function(data,
                                 weight_var = "V1028",
                                 calibration_unit = "month",
                                 margins = c("age_group", "gender", "hh_group", "posest"),
                                 target_totals = NULL,
                                 bounds = c(0.1, 10),
                                 max_iter = 300L,
                                 tol = 1e-6,
                                 bias_correction = c("none", "propensity"),
                                 method = c("margin", "cell"),
                                 min_cell_size = 30L,
                                 verbose = TRUE) {

  # Match arguments for new parameters
  bias_correction <- match.arg(bias_correction)
  method <- match.arg(method)

  # --- Determine period column ---
  period_col <- switch(calibration_unit,
    "month"     = "ref_month_yyyymm",
    "fortnight" = "ref_fortnight_yyyyff",
    "week"      = "ref_week_yyyyww",
    stop("calibration_unit must be 'month', 'fortnight', or 'week'")
  )

  # Check period column exists
  if (!period_col %in% names(data)) {
    stop(sprintf("Period column '%s' not found. Run pnadc_apply_periods() first.", period_col))
  }

  # Get original weights
  if (!weight_var %in% names(data)) {
    stop(sprintf("Weight column '%s' not found in data.", weight_var))
  }

  # Determine column for filtering
  det_col <- switch(calibration_unit,
    "month"     = "determined_month",
    "fortnight" = "determined_fortnight",
    "week"      = "determined_week"
  )

  # --- Validate margins ---
  valid_margins <- c("age_group", "gender", "hh_group", "posest", "rural_urban")
  invalid <- setdiff(margins, valid_margins)
  if (length(invalid) > 0) {
    stop(sprintf("Invalid margin(s): %s. Valid margins: %s",
                 paste(invalid, collapse = ", "),
                 paste(valid_margins, collapse = ", ")))
  }

  # Check for rural_urban feasibility
  if ("rural_urban" %in% margins && !"V1022" %in% names(data)) {
    warning("Margin 'rural_urban' requested but V1022 not found - skipping")
    margins <- setdiff(margins, "rural_urban")
  }

  if (verbose) {
    cat(sprintf("Margins: %s\n", paste(margins, collapse = ", ")))
  }

  # Track created columns for cleanup
  created_cols <- c("w", ".w0")

  # ===========================================================================
  # STEP 1: Create margin variables on FULL data (before filtering)
  # ===========================================================================
  if (verbose) cat("Creating margin variables on full data...\n")

  n_total <- nrow(data)

  # Age groups (9 categories: 0-4, 5-9, 10-14, 15-19, 20-29, 30-39, 40-49, 50-64, 65+)
  if ("age_group" %in% margins) {
    data[, age_group := data.table::fcase(
      V2009 <= 4L,  1L,
      V2009 <= 9L,  2L,
      V2009 <= 14L, 3L,
      V2009 <= 19L, 4L,
      V2009 <= 29L, 5L,
      V2009 <= 39L, 6L,
      V2009 <= 49L, 7L,
      V2009 <= 64L, 8L,
      default = 9L
    )]
    created_cols <- c(created_cols, "age_group")
  }

  # Gender (V2007: 1=male, 2=female)
  if ("gender" %in% margins) {
    data[, gender := V2007]
    created_cols <- c(created_cols, "gender")
  }

  # Household size (count persons per household) - computed on FULL data
  if ("hh_group" %in% margins) {
    hh_info <- data[, .(hh_n = .N), by = .(Ano, Trimestre, UPA, V1008)]
    data[hh_info, hh_size := i.hh_n, on = .(Ano, Trimestre, UPA, V1008)]
    data[, hh_group := data.table::fcase(
      hh_size == 1L, 1L,
      hh_size == 2L, 2L,
      hh_size <= 4L, 3L,
      default = 4L
    )]
    created_cols <- c(created_cols, "hh_size", "hh_group")
  }

  # Rural/Urban (requires V1022)
  if ("rural_urban" %in% margins) {
    data[, rural_urban := data.table::fifelse(V1022 == 1L, 1L, 2L)]
    created_cols <- c(created_cols, "rural_urban")
  }

  # ===========================================================================
  # STEP 1b: Propensity Score Pre-Weighting (if requested)
  # ===========================================================================
  use_ipw <- FALSE
  if (bias_correction == "propensity") {
    if (verbose) cat("Computing propensity scores for selection bias correction...\n")

    # Create auxiliary variables for propensity model if not already created for margins
    # Household size groups (if not already created)
    if (!"hh_group" %in% names(data) && all(c("Ano", "Trimestre", "UPA", "V1008") %in% names(data))) {
      hh_info <- data[, .(hh_n = .N), by = .(Ano, Trimestre, UPA, V1008)]
      data[hh_info, .hh_size := i.hh_n, on = .(Ano, Trimestre, UPA, V1008)]
      data[, .hh_group := data.table::fcase(
        .hh_size == 1L, 1L,
        .hh_size == 2L, 2L,
        .hh_size <= 4L, 3L,
        default = 4L
      )]
      created_cols <- c(created_cols, ".hh_size", ".hh_group")
    }

    # Rural/Urban (if not already created)
    if (!"rural_urban" %in% names(data) && "V1022" %in% names(data)) {
      data[, .rural_urban := data.table::fifelse(V1022 == 1L, 1L, 2L)]
      created_cols <- c(created_cols, ".rural_urban")
    }

    # Build propensity model formula
    # Use available variables: age_group, gender, posest, hh_group, rural_urban
    model_terms <- character(0)
    if ("age_group" %in% names(data)) model_terms <- c(model_terms, "factor(age_group)")
    if ("gender" %in% names(data)) model_terms <- c(model_terms, "factor(gender)")
    if ("posest" %in% names(data)) model_terms <- c(model_terms, "factor(posest)")
    # Use .hh_group or hh_group depending on which exists
    if (".hh_group" %in% names(data)) {
      model_terms <- c(model_terms, "factor(.hh_group)")
    } else if ("hh_group" %in% names(data)) {
      model_terms <- c(model_terms, "factor(hh_group)")
    }
    # Use .rural_urban or rural_urban depending on which exists
    if (".rural_urban" %in% names(data)) {
      model_terms <- c(model_terms, "factor(.rural_urban)")
    } else if ("rural_urban" %in% names(data)) {
      model_terms <- c(model_terms, "factor(rural_urban)")
    }

    if (length(model_terms) == 0) {
      warning("No covariates available for propensity model - skipping bias correction")
    } else {
      # Create determination indicator
      data[, .det := get(det_col)]
      created_cols <- c(created_cols, ".det")

      # Fit propensity model by quarter
      tryCatch({
        # Build model formula
        model_formula <- as.formula(paste(".det ~", paste(model_terms, collapse = " + ")))

        # Fit separate model per quarter for better fit
        data[, .prop_score := {
          model <- glm(model_formula, family = binomial(link = "logit"),
                       weights = get(weight_var), data = .SD)
          predict(model, type = "response")
        }, by = .(Ano, Trimestre)]

        # Handle edge cases FIRST (before computing IPW)
        data[is.na(.prop_score) | .prop_score < 0.05, .prop_score := 0.05]
        data[.prop_score > 0.95, .prop_score := 0.95]

        # Calculate stabilized IPW: w_ipw = V1028 * P(det) / P(det|X)
        data[, .det_rate := mean(.det, na.rm = TRUE), by = .(Ano, Trimestre)]
        data[, .w_ipw := get(weight_var) * .det_rate / .prop_score]

        # Truncate extreme IPW (1st and 99th percentile by quarter)
        data[, .w_ipw := {
          q01 <- quantile(.w_ipw, 0.01, na.rm = TRUE)
          q99 <- quantile(.w_ipw, 0.99, na.rm = TRUE)
          pmin(pmax(.w_ipw, q01), q99)
        }, by = .(Ano, Trimestre)]

        use_ipw <- TRUE
        created_cols <- c(created_cols, ".prop_score", ".det_rate", ".w_ipw")

        if (verbose) {
          cat(sprintf("  Propensity score range: [%.3f, %.3f]\n",
                      min(data$.prop_score, na.rm = TRUE),
                      max(data$.prop_score, na.rm = TRUE)))
          valid_ipw <- data[!is.na(.w_ipw) & get(weight_var) > 0]
          if (nrow(valid_ipw) > 0) {
            ipw_ratio <- valid_ipw$.w_ipw / valid_ipw[[weight_var]]
            cat(sprintf("  IPW adjustment range: [%.2f, %.2f]\n",
                        min(ipw_ratio, na.rm = TRUE),
                        max(ipw_ratio, na.rm = TRUE)))
          }
        }
      }, error = function(e) {
        warning(sprintf("Propensity model failed: %s - using standard weights", e$message))
      })
    }
  }

  # ===========================================================================
  # STEP 2: Calculate TARGET PROPORTIONS from FULL quarterly sample
  # This is the CRITICAL FIX - use ALL data to get unbiased proportions
  # ===========================================================================
  if (verbose) cat("Calculating target proportions from FULL quarterly sample (unbiased)...\n")

  # Get original weights from full data
  w_full <- data[[weight_var]]

  # Calculate proportions by QUARTER (Ano, Trimestre) from FULL data
  # This ensures proportions reflect the true population composition
  target_props <- lapply(margins, function(m) {
    # Group by quarter AND margin variable to get quarterly proportions
    props <- data[, .(weighted_n = sum(get(weight_var))), by = c("Ano", "Trimestre", m)]
    props[, quarter_total := sum(weighted_n), by = .(Ano, Trimestre)]
    props[, target_prop := weighted_n / quarter_total]
    props
  })
  names(target_props) <- margins

  # ===========================================================================
  # STEP 2b: Adaptive Margin Collapsing (for sparse data)
  # ===========================================================================
  if (min_cell_size > 0 && calibration_unit %in% c("fortnight", "week")) {
    if (verbose) cat("Checking margin cell sizes for adaptive collapsing...\n")

    margins_to_collapse <- character(0)
    margins_to_remove <- character(0)

    for (m in margins) {
      # Check cell sizes in the determined sample by quarter
      cell_sizes <- data[get(det_col) == TRUE, .N, by = c("Ano", "Trimestre", m)]
      min_size <- min(cell_sizes$N, na.rm = TRUE)

      if (min_size < min_cell_size) {
        if (verbose) {
          cat(sprintf("  Margin '%s': min cell size = %d (below threshold %d)\n",
                      m, min_size, min_cell_size))
        }

        if (m == "age_group") {
          # Collapse 9 age groups -> 4 broader categories
          data[, age_group := data.table::fcase(
            age_group <= 3L, 1L,   # 0-14
            age_group <= 5L, 2L,   # 15-29
            age_group <= 8L, 3L,   # 30-64
            default = 4L            # 65+
          )]

          # Recalculate target proportions with collapsed margin
          target_props[["age_group"]] <- data[, .(weighted_n = sum(get(weight_var))),
                                              by = c("Ano", "Trimestre", "age_group")]
          target_props[["age_group"]][, quarter_total := sum(weighted_n),
                                      by = .(Ano, Trimestre)]
          target_props[["age_group"]][, target_prop := weighted_n / quarter_total]

          if (verbose) cat("    -> Collapsed age_group: 9 -> 4 categories\n")

        } else if (m == "hh_group") {
          # Collapse 4 hh groups -> 2
          data[, hh_group := data.table::fcase(
            hh_group <= 2L, 1L,   # 1-2 persons
            default = 2L           # 3+ persons
          )]

          # Recalculate target proportions
          target_props[["hh_group"]] <- data[, .(weighted_n = sum(get(weight_var))),
                                             by = c("Ano", "Trimestre", "hh_group")]
          target_props[["hh_group"]][, quarter_total := sum(weighted_n),
                                     by = .(Ano, Trimestre)]
          target_props[["hh_group"]][, target_prop := weighted_n / quarter_total]

          if (verbose) cat("    -> Collapsed hh_group: 4 -> 2 categories\n")

        } else if (m == "posest") {
          # posest is too granular for sparse data - remove it
          margins_to_remove <- c(margins_to_remove, "posest")
          target_props[["posest"]] <- NULL
          if (verbose) cat("    -> Removed posest margin (too sparse)\n")

        } else if (m == "rural_urban") {
          # Check if still too sparse after being binary - if so, remove
          margins_to_remove <- c(margins_to_remove, "rural_urban")
          target_props[["rural_urban"]] <- NULL
          if (verbose) cat("    -> Removed rural_urban margin (too sparse)\n")
        }
      }
    }

    # Remove marked margins
    if (length(margins_to_remove) > 0) {
      margins <- setdiff(margins, margins_to_remove)
    }

    if (verbose && length(margins) > 0) {
      cat(sprintf("  Final margins: %s\n", paste(margins, collapse = ", ")))
    }
  }

  # ===========================================================================
  # STEP 3: Filter to determined observations only
  # ===========================================================================
  if (det_col %in% names(data)) {
    data <- data[get(det_col) == TRUE]
    n_det <- nrow(data)
    if (verbose) {
      cat(sprintf("Working with %s determined observations (%.1f%% of %s total)\n",
                  format(n_det, big.mark = ","),
                  100 * n_det / n_total,
                  format(n_total, big.mark = ",")))
    }
  } else {
    n_det <- n_total
  }

  # Extract original weights AFTER filtering (for the determined observations)
  # Use IPW-adjusted weights if propensity correction was applied
  if (use_ipw && ".w_ipw" %in% names(data)) {
    w0 <- data$.w_ipw
    if (verbose) cat("  Using IPW-adjusted weights as initial weights\n")
  } else {
    w0 <- data[[weight_var]]
  }

  # Store original weights in data for reliable access later
  # (store the BASE weights V1028, not the IPW-adjusted ones, for bound calculations)
  data[, .w0 := data[[weight_var]]]

  # Check for zero/missing original weights
  n_zero_weights <- sum(w0 <= 0 | is.na(w0))
  if (n_zero_weights > 0 && verbose) {
    cat(sprintf("  Warning: %d observations have zero/missing original weights\n", n_zero_weights))
  }

  # ===========================================================================
  # STEP 4: IPF iterations on determined observations
  # Uses quarterly proportions as targets (from full sample)
  # ===========================================================================
  if (verbose) {
    method_desc <- if (method == "cell") "cell-based" else "margin-based"
    cat(sprintf("Running %s IPF (max %d iterations, tol=%.0e)...\n",
                method_desc, max_iter, tol))
  }

  w <- copy(w0)
  data[, w := w]  # Add working weight column

  converged <- FALSE
  final_iter <- max_iter

  if (method == "cell") {
    # =========================================================================
    # CELL-BASED IPF: Preserve anchor-period structure within hierarchical cells
    # Similar to nested raking approach
    # =========================================================================

    # Create hierarchical cells (using same structure as nested raking)
    # celula1: Age groups (4 categories for compatibility)
    data[, ipf_celula1 := data.table::fcase(
      V2009 <= 13L, 0L,
      V2009 <= 29L, 1L,
      V2009 <= 59L, 2L,
      default = 3L
    )]
    created_cols <- c(created_cols, "ipf_celula1")

    # celula2: Post-stratum group + age (if available)
    if ("posest_sxi" %in% names(data)) {
      # posest_sxi may be character - convert to integer for arithmetic
      data[, ipf_celula2 := as.integer(posest_sxi) %/% 100L + 10L * ipf_celula1]
    } else {
      data[, ipf_celula2 := ipf_celula1]
    }
    created_cols <- c(created_cols, "ipf_celula2")

    # Determine number of cell levels based on calibration unit (like nested raking)
    n_cell_levels <- switch(calibration_unit,
                            "month" = 2L,
                            "fortnight" = 2L,
                            "week" = 1L)

    # Store period column as a proper name for joins
    data[, ipf_period := get(period_col)]
    created_cols <- c(created_cols, "ipf_period")

    for (iter in seq_len(max_iter)) {
      w_old <- copy(w)

      # Iterate over cell levels (coarse to fine)
      for (level in seq_len(n_cell_levels)) {
        cell_col <- paste0("ipf_celula", level)

        # Calculate anchor-period totals (quarterly totals per cell)
        anchor_totals <- data[, .(anchor_total = sum(.w0)),
                              by = c("Ano", "Trimestre", cell_col)]

        # Calculate current period totals per cell
        period_totals <- data[, .(period_total = sum(w)),
                              by = c("Ano", "Trimestre", cell_col, "ipf_period")]

        # Count periods per anchor-cell
        period_totals[, n_periods := .N, by = c("Ano", "Trimestre", cell_col)]

        # Target: each period gets proportional share of anchor total
        # (equal share if we assume uniform distribution within quarter)
        period_totals[anchor_totals, target := i.anchor_total / n_periods,
                      on = c("Ano", "Trimestre", cell_col)]

        # Adjustment factor
        period_totals[, factor := target / period_total]
        period_totals[is.na(factor) | is.infinite(factor), factor := 1]

        # Cap extreme adjustments
        period_totals[, factor := pmin(pmax(factor, 0.5), 2.0)]

        # Apply adjustment to weights using explicit join
        data[period_totals, w := w * i.factor,
             on = c("Ano", "Trimestre", cell_col, "ipf_period")]
      }

      w <- data$w

      # Apply bounds (ratio relative to original weight)
      w0_safe <- pmax(data$.w0, 1e-10)
      ratio <- w / w0_safe
      ratio <- pmax(bounds[1], pmin(bounds[2], ratio))

      # For observations with zero original weights, keep them at zero
      w <- data$.w0 * ratio
      w[data$.w0 <= 0 | is.na(data$.w0)] <- 0
      data.table::set(data, j = "w", value = w)

      # Check convergence
      max_change <- max(abs(w - w_old) / pmax(w_old, 1e-10))

      if (max_change < tol) {
        converged <- TRUE
        final_iter <- iter
        if (verbose) cat(sprintf("  Converged at iteration %d (max change: %.2e)\n", iter, max_change))
        break
      }

      if (verbose && iter %% 10 == 0) {
        cat(sprintf("  Iteration %d: max change = %.2e\n", iter, max_change))
      }
    }

  } else {
    # =========================================================================
    # MARGIN-BASED IPF: Original approach - iterate over independent margins
    # =========================================================================

    for (iter in seq_len(max_iter)) {
      w_old <- copy(w)

      # Iterate through each margin
      for (m in margins) {
        # Current proportions by QUARTER (to match target_props structure)
        current <- data[, .(current_total = sum(w)), by = c("Ano", "Trimestre", m)]
        current[, quarter_total := sum(current_total), by = .(Ano, Trimestre)]
        current[, current_prop := current_total / quarter_total]

        # Merge target proportions (from full quarterly sample)
        current[target_props[[m]], target_prop := i.target_prop, on = c("Ano", "Trimestre", m)]

        # Adjustment factor = target_prop / current_prop
        current[, factor := target_prop / current_prop]
        current[is.na(factor) | is.infinite(factor), factor := 1]

        # Apply adjustment to weights (join back on Ano, Trimestre, margin)
        data[current, w := w * i.factor, on = c("Ano", "Trimestre", m)]
      }

      w <- data$w

      # Apply bounds (ratio relative to original weight)
      # Handle zero/NA original weights to avoid Inf/NaN
      w0_safe <- pmax(data$.w0, 1e-10)
      ratio <- w / w0_safe

      ratio <- pmax(bounds[1], pmin(bounds[2], ratio))

      # For observations with zero original weights, keep them at zero
      w <- data$.w0 * ratio
      w[data$.w0 <= 0 | is.na(data$.w0)] <- 0
      # Use set() to avoid NSE issues - data[, w := w] is a no-op!
      data.table::set(data, j = "w", value = w)

      # Check convergence
      max_change <- max(abs(w - w_old) / pmax(w_old, 1e-10))

      if (max_change < tol) {
        converged <- TRUE
        final_iter <- iter
        if (verbose) cat(sprintf("  Converged at iteration %d (max change: %.2e)\n", iter, max_change))
        break
      }

      if (verbose && iter %% 10 == 0) {
        cat(sprintf("  Iteration %d: max change = %.2e\n", iter, max_change))
      }
    }
  }

  if (!converged && verbose) {
    cat(sprintf("  Warning: Did not converge after %d iterations (max change: %.2e)\n",
                max_iter, max_change))
  }

  # ===========================================================================
  # STEP 5: Final scaling to SIDRA population (if provided)
  # Each period scales to its containing month's SIDRA population
  # ===========================================================================
  if (!is.null(target_totals)) {
    if (verbose) cat("Scaling to SIDRA population totals...\n")

    # Standardize target_totals column name
    tt <- data.table::copy(target_totals)
    if ("anomesexato" %in% names(tt) && !"ref_month_yyyymm" %in% names(tt)) {
      tt[, ref_month_yyyymm := anomesexato]
    }

    if (!"m_populacao" %in% names(tt)) {
      if (verbose) cat("  Warning: m_populacao not found in target_totals - skipping scaling\n")
    } else {
      # Calculate current population sum per period
      period_sums <- data[, .(current_pop = sum(w)), by = period_col]

      # Map each period to its corresponding month for SIDRA lookup
      if (calibration_unit == "month") {
        # Direct: period_col IS ref_month_yyyymm
        period_sums[tt, target_pop := i.m_populacao * 1000,
                    on = c(setNames("ref_month_yyyymm", period_col))]

      } else if (calibration_unit == "fortnight") {
        # ref_fortnight_yyyyff format: YYYYFF where FF = 01-24 (24 fortnights/year)
        # Fortnight 01-02 → Month 1, Fortnight 03-04 → Month 2, etc.
        period_sums[, fortnight_num := get(period_col) %% 100L]
        period_sums[, month_num := (fortnight_num + 1L) %/% 2L]
        period_sums[, year := get(period_col) %/% 100L]
        period_sums[, ref_month_yyyymm := year * 100L + month_num]

        # Join with SIDRA monthly population
        period_sums[tt, target_pop := i.m_populacao * 1000, on = "ref_month_yyyymm"]

        # Clean up temp columns
        period_sums[, c("fortnight_num", "month_num", "year", "ref_month_yyyymm") := NULL]

      } else if (calibration_unit == "week") {
        # ref_week_yyyyww format: YYYYWW where WW = 01-48 (48 weeks/year, 4 per month)
        # Week 01-04 → Month 1, Week 05-08 → Month 2, etc.
        period_sums[, week_num := get(period_col) %% 100L]
        period_sums[, month_num := (week_num - 1L) %/% 4L + 1L]
        period_sums[, year := get(period_col) %/% 100L]
        period_sums[, ref_month_yyyymm := year * 100L + month_num]

        # Join with SIDRA monthly population
        period_sums[tt, target_pop := i.m_populacao * 1000, on = "ref_month_yyyymm"]

        # Clean up temp columns
        period_sums[, c("week_num", "month_num", "year", "ref_month_yyyymm") := NULL]
      }

      # Calculate and apply scale factor
      if ("target_pop" %in% names(period_sums)) {
        period_sums[, scale_factor := target_pop / current_pop]
        period_sums[is.na(scale_factor) | is.infinite(scale_factor), scale_factor := 1]

        # Apply scaling to weights
        data[period_sums, w := w * i.scale_factor, on = period_col]
        w <- data$w

        if (verbose) {
          n_matched <- sum(!is.na(period_sums$target_pop))
          n_total_periods <- nrow(period_sums)
          mean_scale <- mean(period_sums$scale_factor, na.rm = TRUE)
          cat(sprintf("  Matched %d/%d periods to SIDRA totals\n", n_matched, n_total_periods))
          cat(sprintf("  Mean scaling factor: %.4f\n", mean_scale))
        }
      } else {
        if (verbose) cat("  Warning: Could not compute target_pop\n")
      }
    }
  }

  # --- Output ---
  out_col <- switch(calibration_unit,
    "month"     = "weight_ipf_monthly",
    "fortnight" = "weight_ipf_fortnightly",
    "week"      = "weight_ipf_weekly"
  )

  # Use set() to ensure the w vector is assigned correctly
  data.table::set(data, j = out_col, value = w)

  # Compute final statistics before cleanup
  if (verbose) {
    cat(sprintf("Done. Created column: %s\n", out_col))

    # Summary statistics using stored original weights
    final_weights <- data[[out_col]]
    w0_for_stats <- data$.w0
    valid_idx <- w0_for_stats > 0 & !is.na(w0_for_stats)
    ratio_final <- final_weights[valid_idx] / w0_for_stats[valid_idx]

    cat(sprintf("  Weight ratio stats: min=%.2f, median=%.2f, max=%.2f\n",
                min(ratio_final, na.rm = TRUE),
                median(ratio_final, na.rm = TRUE),
                max(ratio_final, na.rm = TRUE)))
    cat(sprintf("  Weights at bounds: %.1f%% at lower, %.1f%% at upper\n",
                100 * mean(ratio_final <= bounds[1] + 0.01, na.rm = TRUE),
                100 * mean(ratio_final >= bounds[2] - 0.01, na.rm = TRUE)))
  }

  # Clean up temporary columns
  cols_to_remove <- intersect(created_cols, names(data))
  if (length(cols_to_remove) > 0) {
    data[, (cols_to_remove) := NULL]
  }

  data
}
