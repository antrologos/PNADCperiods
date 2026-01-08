# mensalizePNADC

Convert Brazil's quarterly PNADC survey data into monthly time series.

## Overview

The `mensalizePNADC` package identifies reference months in Brazil's quarterly PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) survey data and optionally computes monthly survey weights. This enables monthly (instead of quarterly) labor market analysis using PNADC microdata.

**Key Features:**

- **High accuracy**: ~96% determination rate for 2013-2019 data
- **Fast processing**: ~2.3 minutes for 15.8 million rows
- **Minimal dependencies**: Only requires `data.table` and `checkmate`
- **Flexible output**: Returns a crosswalk for easy joins with original data

## Installation

```r
# Install from GitHub
devtools::install_github("PLACEHOLDER/mensalizePNADC")

# Or install from local source
devtools::install("path/to/mensalizePNADC")
```

## Quick Start

```r
library(mensalizePNADC)
library(data.table)

# Load stacked quarterly PNADC data (minimum required columns)
pnadc <- fread("pnadc_stacked.csv",
  select = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003",
             "V2008", "V20081", "V20082", "V2009"))

# Get reference month crosswalk
crosswalk <- mensalizePNADC(pnadc)
# Step 1/1: Identifying reference months...
#   Determination rate: 96.3%

# Join with original data
result <- merge(original_data, crosswalk,
  by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"))

# Now use ref_month_yyyymm for monthly analysis
result[, .(n = .N), by = ref_month_yyyymm]
```

## How It Works

The algorithm identifies which month within each quarter each survey observation refers to using:

1. **IBGE's "Parada Tecnica" rules**: Interview weeks end on Saturdays, with specific rules about how many days must fall within each month

2. **Birthday constraints**: Respondent birthdates and ages constrain possible interview dates

3. **Cross-quarter aggregation**: PNADC is a rotating panel survey where the same UPA-V1014 is always interviewed in the same relative month across quarters. This dramatically improves determination rates.

## Important: Use Stacked Data for Best Results

The mensalization algorithm achieves **~95% determination rate** when processing **stacked multi-quarter data**. If you process quarters individually, you'll only get ~65-75% determination.

| Processing Mode | Determination Rate |
|-----------------|-------------------|
| Per-quarter (single quarter) | ~65-75% |
| Stacked (multi-quarter) | **~95%** |

This is by design: PNADC uses a rotating panel where households are interviewed in the same relative week each quarter. The algorithm combines birthday constraints from all quarters to narrow down the interview date.

**Recommended**: Stack at least 2 years of quarterly data before calling `mensalizePNADC()`.

## Output Variables

| Variable | Type | Description |
|----------|------|-------------|
| `ref_month` | Date | Reference month (first day, e.g., "2023-01-01") |
| `ref_month_in_quarter` | Integer | Position in quarter: 1, 2, 3, or NA |
| `ref_month_yyyymm` | Integer | YYYYMM format (e.g., 202301) |
| `weight_monthly` | Numeric | Monthly weight (if `compute_weights = TRUE`) |

## Required Input Variables

| Variable | Description |
|----------|-------------|
| `Ano` | Survey year |
| `Trimestre` | Quarter (1-4) |
| `UPA` | Primary Sampling Unit |
| `V1014` | Panel identifier |
| `V1008` | Household identifier |
| `V2003` | Person identifier |
| `V2008` | Birth day (1-31, or 99 for unknown) |
| `V20081` | Birth month (1-12, or 99 for unknown) |
| `V20082` | Birth year (or 9999 for unknown) |
| `V2009` | Age |

## Functions

| Function | Description |
|----------|-------------|
| `mensalizePNADC()` | Main function: identify months + optional weights |
| `identify_reference_month()` | Just reference month identification |
| `calibrate_monthly_weights()` | Rake weighting for monthly weights |
| `calibrate_to_sidra()` | Optional Bayesian calibration to match SIDRA |
| `validate_pnadc()` | Input data validation |
| `get_exception_quarters()` | List quarters with non-standard timing |

## Performance

| Dataset | Rows | Time |
|---------|------|------|
| 1 quarter | ~570K | ~5 sec |
| 1 year | ~2.3M | ~20 sec |
| 7 years (2013-2019) | 15.8M | 2.3 min |

## Determination Rates

| Period | Rate | Notes |
|--------|------|-------|
| 2013-2019 | 96-99% | Best results |
| 2012 | ~85% | Panel rotation effects |
| 2020+ | Variable | Pandemic sample changes |

## Monthly Weights (Optional)

For monthly aggregate estimates, compute monthly-appropriate survey weights:

```r
result <- mensalizePNADC(pnadc_full,
  compute_weights = TRUE,
  monthly_totals = monthly_pop)

# Use weight_monthly for estimates
result[, .(pop = sum(weight_monthly)), by = ref_month_yyyymm]
```

## SIDRA Calibration (Optional)

To match IBGE's official SIDRA series exactly:

```r
# Calibrate for unemployment analysis
calibrated <- calibrate_to_sidra(result, theme = "unemployment")
```

Available themes: `"unemployment"`, `"employment"`, `"labor_force"`, `"income"`

## Authors

- **Marcos Hecksher** - Original methodology and Stata implementation
- **Rogerio Barbosa** - R package co-author

## References

- [IBGE PNADC Documentation](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
- Hecksher, M. (2024). Mensalização da PNADC. Working paper.

## License
MIT
