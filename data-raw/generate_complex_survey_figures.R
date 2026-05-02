# =============================================================================
# Generate Pre-computed Data and Figures for the Complex Survey Design Vignette
# =============================================================================
#
# This script produces:
#   1. Survey estimates with standard errors (saved to data/processed/)
#   2. PNG figures for the vignette (saved to output/vignette/figures/)
#
# IMPORTANT: Code blocks marked with "# --- VIGNETTE CODE: chunk-name ---"
# comments should match what appears in the complex-survey-design.Rmd vignette.
#
# STRATEGY:
#   - Load and stack all quarterly PNADC files
#   - Apply mensalization using pnadc_identify_periods() + pnadc_apply_periods()
#   - Compute survey estimates with proper complex design
#
# =============================================================================

library(PNADCperiods)
library(data.table)
library(ggplot2)
library(scales)
library(fst)
library(survey)

# =============================================================================
# PATHS
# =============================================================================

# Input paths - quarterly PNADC data
pnadc_dir     <- "D:/Dropbox/Bancos_Dados/PNADC/Trimestral/Dados/"

# Output paths
processed_dir <- "D:/Dropbox/Artigos/mensalizacao_pnad/data/processed/"
fig_dir       <- "D:/Dropbox/Artigos/mensalizacao_pnad/output/vignette/figures/complex-survey-design/"
pkg_fig_dir   <- "D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods/vignettes/figures/complex-survey-design/"

# Create directories if needed
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
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

# Microdata cache with survey design variables
survey_microdata_cache <- paste0(processed_dir, "pnadc_survey_microdata.fst")

# Survey estimates cache
survey_estimates_cache <- paste0(processed_dir, "survey_gender_estimates.fst")

# =============================================================================
# STEP 1: LOAD OR BUILD MICRODATA WITH SURVEY DESIGN VARIABLES
# =============================================================================

cat("=== Step 1: Load PNADC Data with Survey Design Variables ===\n")

if (file.exists(survey_microdata_cache)) {
  cat("Loading cached microdata from:", basename(survey_microdata_cache), "\n")
  pnadc <- fst::read_fst(survey_microdata_cache, as.data.table = TRUE)
  cat("Loaded", format(nrow(pnadc), big.mark = ","), "observations\n")
} else {
  cat("Building microdata from quarterly files...\n")

  # ---------------------------------------------------------------------------
  # STEP 1a: Load and stack quarterly files (all years available)
  # ---------------------------------------------------------------------------

  # List all quarterly data files
  files <- list.files(pnadc_dir, pattern = "^pnadc_\\d{4}-\\dq\\.fst$", full.names = TRUE)
  cat("Found", length(files), "quarterly files\n")

  # Columns needed for mensalization + survey analysis
  cols_needed <- c(
    # Mensalization keys
    "Ano", "Trimestre", "UPA", "V1008", "V1014",
    # Birthday variables (for identification)
    "V2008", "V20081", "V20082", "V2009",
    # Calibration variables
    "V1028", "UF", "posest", "posest_sxi",
    # Survey design variables
    "Estrato",
    # Labor market and demographic variables
    "VD4001",   # Labor force participation
    "VD4002",   # Occupation status
    "V2007"     # Sex
  )

  # Load and stack all quarterly files
  pnadc_list <- lapply(files, function(f) {
    cat("  Loading:", basename(f), "\n")
    dt <- fst::read_fst(f, as.data.table = TRUE, columns = cols_needed)
    dt
  })

  pnadc <- rbindlist(pnadc_list, fill = TRUE)
  rm(pnadc_list); gc()

  cat("Stacked data:", format(nrow(pnadc), big.mark = ","), "observations\n")

  # Check for required columns
  cat("\nColumn check:\n")
  cat("  Has Estrato:", "Estrato" %in% names(pnadc), "\n")
  cat("  Has VD4001:", "VD4001" %in% names(pnadc), "\n")
  cat("  Has V2007:", "V2007" %in% names(pnadc), "\n")

  if (!"Estrato" %in% names(pnadc)) {
    stop("Estrato column not found. Check quarterly files.")
  }

  # ---------------------------------------------------------------------------
  # STEP 1d: Apply mensalization using new two-step API
  # ---------------------------------------------------------------------------
  cat("\n=== Applying Mensalization ===\n")

  # Convert key columns back to proper types for mensalization
  pnadc[, Ano := as.integer(Ano)]
  pnadc[, Trimestre := as.integer(Trimestre)]
  pnadc[, V2009 := as.integer(V2009)]

  # --- VIGNETTE CODE: load-data ---
  # Step 1: Build crosswalk (identify reference periods)
  crosswalk <- pnadc_identify_periods(pnadc, verbose = TRUE)

  # Step 2: Apply crosswalk and calibrate weights
  # Note: pnadc_apply_periods merges the crosswalk and adds all period columns
  pnadc <- pnadc_apply_periods(
    pnadc,
    crosswalk,
    weight_var = "V1028",
    anchor = "quarter",
    calibrate = TRUE,
    calibration_unit = "month",
    verbose = TRUE
  )
  # --- END VIGNETTE CODE: load-data ---

  # Clean up crosswalk to free memory
  rm(crosswalk); gc()

  # ---------------------------------------------------------------------------
  # STEP 1e: Create derived variables
  # ---------------------------------------------------------------------------
  cat("\nCreating derived variables...\n")

  # Filter to working-age population
  pnadc[, V2009 := as.integer(V2009)]
  pnadc <- pnadc[V2009 >= 14]

  # Create labor force indicator
  pnadc[, VD4001 := as.integer(VD4001)]
  pnadc[, in_labor_force := fifelse(VD4001 == 1, 1L, 0L)]

  # Create sex labels
  pnadc[, V2007 := as.integer(V2007)]
  pnadc[, sex := fifelse(V2007 == 1, "Men", "Women")]

  cat("Working-age population (14+):", format(nrow(pnadc), big.mark = ","), "\n")

  # Save cache
  cat("\nSaving cached microdata to:", basename(survey_microdata_cache), "\n")
  fst::write_fst(pnadc, survey_microdata_cache, compress = 50)
}

# =============================================================================
# STEP 2: FILTER TO DETERMINED OBSERVATIONS
# =============================================================================

cat("\n=== Step 2: Filter to Determined Observations ===\n")

pnadc_monthly <- pnadc[!is.na(weight_monthly)]
cat("Observations with determined month:", format(nrow(pnadc_monthly), big.mark = ","), "\n")

# =============================================================================
# STEP 3: COMPUTE SURVEY ESTIMATES WITH STANDARD ERRORS
# =============================================================================

cat("\n=== Step 3: Compute Survey Estimates by Month ===\n")

# Set survey options
options(survey.lonely.psu = "adjust")

# Get unique months
months <- sort(unique(pnadc_monthly$ref_month_yyyymm))
cat("Months to process:", length(months), "\n")

# Function to compute participation rate by sex for one month
compute_participation_by_sex <- function(month_yyyymm, data) {

  month_data <- data[ref_month_yyyymm == month_yyyymm]

  if (nrow(month_data) < 100) return(NULL)

  # Check for singleton strata issues
  upa_per_strata <- month_data[, .(n_upa = uniqueN(UPA)), by = Estrato]

  # If too many singleton strata, fall back to simpler design
  singleton_pct <- mean(upa_per_strata$n_upa == 1)

  tryCatch({
    if (singleton_pct > 0.3) {
      # Use simpler design without strata if too many singletons
      design <- svydesign(
        ids = ~UPA,
        weights = ~weight_monthly,
        data = month_data,
        nest = FALSE
      )
    } else {
      # Full design with strata
      design <- svydesign(
        ids = ~UPA,
        strata = ~Estrato,
        weights = ~weight_monthly,
        data = month_data,
        nest = TRUE
      )
    }

    # Compute participation by sex
    result <- svyby(
      ~in_labor_force,
      by = ~sex,
      design = design,
      FUN = svymean,
      na.rm = TRUE,
      vartype = c("se", "ci")
    )

    setDT(result)
    result[, ref_month_yyyymm := month_yyyymm]

    result
  }, error = function(e) {
    cat("Error processing month", month_yyyymm, ":", e$message, "\n")
    NULL
  })
}

# Check if estimates are cached
if (file.exists(survey_estimates_cache)) {
  cat("Loading cached survey estimates...\n")
  results_by_sex <- fst::read_fst(survey_estimates_cache, as.data.table = TRUE)
} else {
  cat("Computing survey estimates (this may take a while)...\n")

  # Process months with progress indicator
  results_list <- vector("list", length(months))

  for (i in seq_along(months)) {
    if (i %% 12 == 0) cat("Processing month", i, "of", length(months), "\n")
    results_list[[i]] <- compute_participation_by_sex(months[i], pnadc_monthly)
  }

  results_by_sex <- rbindlist(results_list[!sapply(results_list, is.null)])

  # Save cache
  fst::write_fst(results_by_sex, survey_estimates_cache)
  cat("Saved survey estimates to cache\n")
}

# Standardize column names
if ("in_labor_force" %in% names(results_by_sex)) {
  setnames(results_by_sex, "in_labor_force", "participation")
}

# Add period column
results_by_sex[, period := as.Date(paste0(
  ref_month_yyyymm %/% 100, "-",
  ref_month_yyyymm %% 100, "-15"
))]

cat("Survey estimates computed for", nrow(results_by_sex), "month-sex combinations\n")

# =============================================================================
# STEP 4: COMPUTE GENDER GAP
# =============================================================================

cat("\n=== Step 4: Compute Gender Gap ===\n")

# --- VIGNETTE CODE: compute-gap ---
# Reshape to wide format to compute gap
results_wide <- dcast(results_by_sex, ref_month_yyyymm + period ~ sex,
                      value.var = c("participation", "se"))

# Compute gap and its SE
# SE of difference: SE(gap) = sqrt(SE_men^2 + SE_women^2)
results_wide[, `:=`(
  gap = participation_Men - participation_Women,
  gap_se = sqrt(se_Men^2 + se_Women^2)
)]

results_wide[, `:=`(
  gap_ci_lower = gap - 1.96 * gap_se,
  gap_ci_upper = gap + 1.96 * gap_se
)]
# --- END VIGNETTE CODE: compute-gap ---

cat("Gender gap computed for", nrow(results_wide), "months\n")

# =============================================================================
# STEP 5: GENERATE FIGURES
# =============================================================================

cat("\n=== Step 5: Generate Figures ===\n")

# -----------------------------------------------------------------------------
# FIGURE 1: Gender Gap Time Series with Confidence Bands
# (fig-gender-participation-ci.png)
# -----------------------------------------------------------------------------

# --- VIGNETTE CODE: plot-gender-gap ---
# Reshape back to long for plotting
results_long <- results_by_sex[!is.na(participation)]

# Ensure proper CI columns exist
if (!"ci_l" %in% names(results_long)) {
  results_long[, ci_l := participation - 1.96 * se]
  results_long[, ci_u := participation + 1.96 * se]
}

p1 <- ggplot(results_long, aes(x = period, y = participation, color = sex, fill = sex)) +

  # Confidence bands
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u), alpha = 0.2, color = NA) +

  # Point estimates
  geom_line(linewidth = 0.8) +

  # Highlight COVID period
  annotate("rect",
           xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf,
           fill = "gray80", alpha = 0.3) +
  annotate("text",
           x = as.Date("2021-01-01"), y = 0.85,
           label = "COVID-19", fontface = "italic", size = 3, color = "gray40") +

  # Scales and labels
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.45, 0.85),
    breaks = seq(0.45, 0.85, 0.05)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Men" = "#0074D9", "Women" = "#FF4136")) +
  scale_fill_manual(values = c("Men" = "#0074D9", "Women" = "#FF4136")) +

  labs(
    title = "Labor Force Participation by Sex: Monthly Series with 95% Confidence Intervals",
    subtitle = "Brazil, 2012-2025. Shaded regions show 95% confidence bands from complex survey design.",
    x = NULL,
    y = "Labor Force Participation Rate",
    color = "Sex",
    fill = "Sex",
    caption = "Source: PNADC/IBGE. Monthly weights from PNADCperiods. Variance estimated via Taylor linearization."
  ) +

  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
# --- END VIGNETTE CODE: plot-gender-gap ---

ggsave(file.path(fig_dir, "fig-gender-participation-ci.png"), p1,
       width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
cat("Saved: fig-gender-participation-ci.png\n")

# -----------------------------------------------------------------------------
# FIGURE 2: Gender Gap (Men - Women) Over Time
# (fig-gender-gap-difference.png)
# -----------------------------------------------------------------------------

# --- VIGNETTE CODE: plot-gap ---
# Pre-COVID average for reference line
pre_covid_avg <- results_wide[period < "2020-03-01", mean(gap, na.rm = TRUE)]

p2 <- ggplot(results_wide[!is.na(gap)], aes(x = period, y = gap)) +

  # Confidence band
  geom_ribbon(aes(ymin = gap_ci_lower, ymax = gap_ci_upper),
              fill = "#7b3294", alpha = 0.3) +

  # Point estimate
  geom_line(color = "#7b3294", linewidth = 0.9) +

  # Reference line at pre-COVID average
  geom_hline(
    yintercept = pre_covid_avg,
    linetype = "dashed", color = "gray50"
  ) +
  annotate("text", x = min(results_wide$period, na.rm = TRUE) + 180,
           y = pre_covid_avg + 0.008,
           label = paste0("Pre-COVID avg: ", sprintf("%.1f", pre_covid_avg * 100), " p.p."),
           size = 3, color = "gray40", hjust = 0) +

  # Highlight COVID period
  annotate("rect",
           xmin = as.Date("2020-03-01"), xmax = as.Date("2021-12-31"),
           ymin = -Inf, ymax = Inf,
           fill = "gray80", alpha = 0.3) +

  # Scales and labels
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0.15, 0.30, 0.025)
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +

  labs(
    title = "Gender Gap in Labor Force Participation (Men - Women)",
    subtitle = "Monthly series with 95% CI. Dashed line = pre-COVID average. Gap widened during pandemic.",
    x = NULL,
    y = "Participation Gap (percentage points)",
    caption = "Source: PNADC/IBGE. Monthly weights from PNADCperiods."
  ) +

  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
# --- END VIGNETTE CODE: plot-gap ---

ggsave(file.path(fig_dir, "fig-gender-gap-difference.png"), p2,
       width = fig_width, height = 5, dpi = fig_dpi, bg = "white")
cat("Saved: fig-gender-gap-difference.png\n")

# -----------------------------------------------------------------------------
# FIGURE 3: Why Standard Errors Matter - Comparing with and without CI
# (fig-why-se-matter.png)
# -----------------------------------------------------------------------------

# --- VIGNETTE CODE: plot-why-se-matter ---
# Subset to COVID period for focused visualization
p3_data <- results_by_sex[period >= "2019-01-01" & period <= "2022-06-01"]

p3 <- ggplot(p3_data, aes(x = period, y = participation, color = sex)) +

  # Just the lines (no CI)
  geom_line(linewidth = 0.9) +

  # Add confidence bands with low alpha
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u, fill = sex),
              alpha = 0.25, color = NA) +

  # Scales and labels
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.45, 0.80)
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_color_manual(values = c("Men" = "#0074D9", "Women" = "#FF4136")) +
  scale_fill_manual(values = c("Men" = "#0074D9", "Women" = "#FF4136")) +

  # Add annotations for interpretation
  annotate("segment",
           x = as.Date("2020-06-15"), xend = as.Date("2020-06-15"),
           y = 0.52, yend = 0.62,
           arrow = arrow(ends = "both", length = unit(0.1, "cm")),
           color = "gray30") +
  annotate("text",
           x = as.Date("2020-06-15"), y = 0.50,
           label = "Is this drop\nstatistically\nsignificant?",
           size = 2.8, color = "gray30") +

  labs(
    title = "Why Confidence Intervals Matter: COVID-19 Labor Market Shock",
    subtitle = "Confidence bands (shaded) show the precision of estimates. Overlapping bands suggest uncertain differences.",
    x = NULL,
    y = "Labor Force Participation Rate",
    color = "Sex",
    fill = "Sex",
    caption = "Source: PNADC/IBGE. Confidence bands show +/- 1.96 SE from complex survey design."
  ) +

  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
# --- END VIGNETTE CODE: plot-why-se-matter ---

ggsave(file.path(fig_dir, "fig-why-se-matter.png"), p3,
       width = fig_width, height = 5.5, dpi = fig_dpi, bg = "white")
cat("Saved: fig-why-se-matter.png\n")

# =============================================================================
# STEP 6: COPY FIGURES TO PACKAGE
# =============================================================================

cat("\n=== Step 6: Copy Figures to Package ===\n")

# Copy all PNG files to package vignettes folder
fig_files <- list.files(fig_dir, pattern = "\\.png$", full.names = TRUE)
file.copy(fig_files, pkg_fig_dir, overwrite = TRUE)
cat("Copied", length(fig_files), "figures to:", pkg_fig_dir, "\n")

# =============================================================================
# STEP 7: SUMMARY
# =============================================================================

cat("\n=== Summary ===\n")
cat("Figures generated:\n")
for (f in basename(fig_files)) {
  cat("  -", f, "\n")
}
cat("\nOutput directory:", fig_dir, "\n")
cat("Package directory:", pkg_fig_dir, "\n")
cat("\n=== Done! ===\n")
