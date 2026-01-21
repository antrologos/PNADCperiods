# PNADCperiods - Architecture Documentation

This document describes the implemented architecture of the PNADCperiods package (v2.1.0).

## Package Overview

**Package name:** `PNADCperiods`
**Purpose:** Identify reference periods (months, fortnights, weeks) in Brazil's PNADC survey data and optionally calibrate weights for sub-quarterly analysis.

## API Design

### Two Main Functions

| Function | Purpose |
|----------|---------|
| `pnadc_identify_periods()` | Build crosswalk with nested month/fortnight/week identification |
| `pnadc_apply_periods()` | Apply crosswalk to data + optional weight calibration |

### Supporting Functions (Exported)

| Function | Purpose |
|----------|---------|
| `identify_reference_month()` | Standalone month identification |
| `identify_reference_fortnight()` | Standalone fortnight identification |
| `identify_reference_week()` | Standalone week identification |
| `fetch_monthly_population()` | Fetch population from SIDRA API |
| `validate_pnadc()` | Input validation |
| `pnadc_experimental_periods()` | Experimental probabilistic period assignment |
| `combine_period_crosswalks()` | Merge strict and experimental crosswalks |
| `calibrate_monthly_weights()` | Legacy monthly calibration (use `pnadc_apply_periods()` instead) |

## Nested Identification Strategy

The package uses a **nested identification hierarchy**:

```
Month → Fortnight → Week
```

**Key principle:** A finer period can only be determined if its parent period is determined.

| Phase | Period | Requires | Aggregation Level |
|-------|--------|----------|-------------------|
| 1 | Month | - | UPA-V1014 across ALL quarters |
| 2 | Fortnight | Month determined | Household within quarter |
| 3 | Week | Fortnight determined | Household within quarter |

This nesting ensures logical consistency: you cannot know which week without knowing which fortnight, and you cannot know which fortnight without knowing which month.

## Crosswalk Structure

The crosswalk is a `data.table` at household-quarter level:

| Column | Type | Description |
|--------|------|-------------|
| `Ano` | integer | Survey year |
| `Trimestre` | integer | Quarter (1-4) |
| `UPA` | integer | Primary sampling unit |
| `V1008` | integer | Household sequence |
| `V1014` | integer | Panel group (1-8) |
| `ref_month` | Date | Reference month (1st of month) |
| `ref_month_in_quarter` | integer | Position in quarter (1, 2, 3) or NA |
| `ref_month_yyyymm` | integer | YYYYMM format |
| `determined_month` | logical | TRUE if month was determined |
| `ref_fortnight` | Date | Reference fortnight (1st or 16th) |
| `ref_fortnight_in_quarter` | integer | Position in quarter (1-6) or NA |
| `ref_fortnight_yyyyff` | integer | YYYYFF format (1-24 per year) |
| `determined_fortnight` | logical | TRUE if fortnight was determined |
| `ref_week` | Date | Reference week (Monday) |
| `ref_week_in_quarter` | integer | Position in quarter (1-14) or NA |
| `ref_week_yyyyww` | integer | ISO YYYYWW format |
| `determined_week` | logical | TRUE if week was determined |

**Join keys:** `Ano`, `Trimestre`, `UPA`, `V1008`, `V1014`

## Determination Rates

| Period | Rate | Reason |
|--------|------|--------|
| Month | ~97% | Aggregates at UPA-V1014 level across ALL quarters (panel design) |
| Fortnight | ~7% | Nested under month; household-level within quarter only |
| Week | ~1.5% | Nested under fortnight; household-level within quarter only |

**Key insight:** Only month identification benefits from stacking data across quarters. Fortnights and weeks are determined solely from birthday constraints within a single quarter, AND require their parent period to be determined first.

## File Structure

```
R/
├── pnadc-identify-periods.R        # Main: pnadc_identify_periods() - nested 4-phase algorithm
├── pnadc-apply-periods.R           # Main: pnadc_apply_periods() - includes internal smoothing
├── identify-reference-month.R      # Core month algorithm with dynamic exceptions
├── identify-reference-fortnight.R  # Core fortnight algorithm (household-level aggregation)
├── identify-reference-week.R       # Core week algorithm (household-level aggregation)
├── experimental-period-identification.R  # pnadc_experimental_periods() - probabilistic strategies
├── calibrate-weights.R             # Legacy calibration + annual weight calibration
├── fetch-sidra-population.R        # SIDRA API for population data
├── utils-dates.R                   # Fast date utilities (lookup tables, ISO week functions)
├── utils-validation.R              # Input validation utilities
└── PNADCperiods-package.R          # Package docs + globalVariables
```

## Algorithm Details

### Phase 1: Month Identification (High Rate)

1. Calculate valid interview Saturdays using IBGE "Parada Técnica" rules
2. Apply birthday constraints to narrow date range per person
3. Convert dates to month positions (1, 2, 3 in quarter)
4. **Aggregate at UPA-V1014 level ACROSS ALL QUARTERS** (key optimization)
5. Dynamic exception detection for edge cases (relaxed Saturday rules)
6. Determine: if `min_position == max_position` → determined

### Phase 2: Fortnight Identification (Nested, Low Rate)

**Precondition:** Only processes observations with `determined_month = TRUE`

1. Constrain fortnight search to within the identified month:
   - Month 1 → fortnights 1-2
   - Month 2 → fortnights 3-4
   - Month 3 → fortnights 5-6
2. Apply birthday constraints within constrained range
3. **Aggregate at household level `(Ano, Trimestre, UPA, V1008)` WITHIN QUARTER**
4. Dynamic exception detection
5. Determine: if `min_position == max_position` → determined

### Phase 3: Week Identification (Nested, Low Rate)

**Precondition:** Only processes observations with `determined_fortnight = TRUE`

1. Constrain week search to within the identified fortnight
2. Apply birthday constraints within constrained range
3. **Aggregate at household level `(Ano, Trimestre, UPA, V1008)` WITHIN QUARTER**
4. Dynamic exception detection
5. Determine: if `min_position == max_position` → determined

### Phase 4: Build Crosswalk

Combines results from all phases into the final crosswalk data.table.

## Experimental Strategies

The `pnadc_experimental_periods()` function provides three strategies for improving determination rates beyond the strict algorithm:

| Strategy | Description |
|----------|-------------|
| `probabilistic` | For 2-period ranges, assigns based on which period contains most of the date interval. Works at UPA-V1014 level for months, household level for fortnights/weeks. |
| `upa_aggregation` | Extends strictly identified periods to other observations in same UPA-V1014 when a consensus exists. |
| `both` | Sequentially applies probabilistic first, then UPA aggregation. Guarantees identification rate >= max of individual strategies. |

**All strategies enforce proper nesting:** experimental fortnights require identified months (strict or experimental), experimental weeks require identified fortnights (strict or experimental).

### Strategy Details

- **Probabilistic:** Calculates confidence as the proportion of the date interval falling within the assigned period. Only assigns when confidence >= threshold (default 0.9).
- **UPA Aggregation:** Requires a proportion >= threshold (default 0.9) of observations in the UPA-V1014 group to have strict identification before extending to others.
- **Both:** The two strategies operate independently (UPA aggregation considers only strict identifications), so the result is the union of both strategies.

Output columns: `ref_month_exp`, `ref_fortnight_exp`, `ref_week_exp` (positions) with corresponding `*_confidence` columns. The `probabilistic_assignment` flag indicates which observations were assigned experimentally vs strictly.

## Calibration Pipeline

When `pnadc_apply_periods(..., calibrate = TRUE)`:

1. Join data with crosswalk
2. Fetch population targets from SIDRA (or use provided targets)
3. Create hierarchical calibration cells
4. Iterative reweighting within anchor period (quarter or year)
5. Calibrate to external population totals
6. Apply smoothing (period-specific)

### Period-Specific Calibration Settings

| Period | Cell Levels | Smoothing | Notes |
|--------|-------------|-----------|-------|
| Month | 4 (full hierarchy) | 3-period rolling mean | Full demographic/geographic cells |
| Fortnight | 2 (age + region) | 7-period rolling mean | Simplified due to sparse data |
| Week | 1 (age only) | None | Minimal cells due to very sparse data |

### Calibration Cell Hierarchy

1. **celula1:** Age groups (0-13, 14-29, 30-59, 60+)
2. **celula2:** Post-stratum group + age
3. **celula3:** State (UF) + celula2
4. **celula4:** Post-stratum (posest) + celula2

### Anchor Types

- `anchor = "quarter"`: Preserve quarterly totals, redistribute to sub-periods
- `anchor = "year"`: Preserve annual totals (for annual PNADC data with V1032 weights)

## Performance Optimizations

The package is optimized for large datasets (~450,000 rows/sec):

1. **Lookup tables:** Pre-computed date tables for 20x faster date creation vs `ISOdate()`
2. **Integer arithmetic:** ISO week calculations use pure integer math (300x faster than `data.table::isoweek()`)
3. **data.table threading:** Enabled via `setDTthreads(0)` at package load
4. **Vectorized operations:** All calculations use vectorized data.table syntax
5. **Efficient joins:** data.table binary joins instead of `merge()`

## Dependencies

**Required:**
- `data.table` (>= 1.14.0) - Core data manipulation
- `checkmate` (>= 2.0.0) - Input validation

**Suggested:**
- `sidrar` - SIDRA API access for population data
- `testthat` - Testing
- `knitr`, `rmarkdown` - Vignettes
- `pkgdown` - Website

## Workflow Examples

### Standard Monthly Analysis
```r
# Build crosswalk from stacked multi-year data (maximizes determination rate)
crosswalk <- pnadc_identify_periods(pnadc_stacked)

# Apply to specific quarter with calibration
result <- pnadc_apply_periods(pnadc_2023, crosswalk,
                              weight_var = "V1028",
                              anchor = "quarter")
```

### Annual Data
```r
result <- pnadc_apply_periods(pnadc_annual, crosswalk,
                              weight_var = "V1032",
                              anchor = "year")
```

### Crosswalk Only (No Calibration)
```r
result <- pnadc_apply_periods(pnadc_2023, crosswalk,
                              calibrate = FALSE)
```

### Experimental Period Assignment
```r
# Build standard crosswalk
crosswalk <- pnadc_identify_periods(pnadc_data)

# Apply experimental strategies for additional assignments
crosswalk_exp <- pnadc_experimental_periods(
  crosswalk,
  pnadc_data,
  strategy = "probabilistic",
  confidence_threshold = 0.9
)
```

## Required PNADC Variables

### For Period Identification
- `Ano` - Survey year
- `Trimestre` - Quarter (1-4)
- `UPA` - Primary sampling unit
- `V1008` - Household sequence number
- `V1014` - Panel group (1-8)
- `V2008` - Birth day (1-31, 99=unknown)
- `V20081` - Birth month (1-12, 99=unknown)
- `V20082` - Birth year
- `V2009` - Age

### For Weight Calibration (additional)
- `V1028` - Quarterly weight (for quarterly data)
- `V1032` - Annual weight (for annual data)
- `UF` - State code
- `posest` - Post-stratum
- `posest_sxi` - Post-stratum group
