# mensalizePNADC

<!-- badges: start -->
[![pkgdown](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

Convert Brazil's quarterly PNADC survey data into monthly time series.

📖 **[Full Documentation](https://antrologos.github.io/mensalizePNADC/)** — Detailed algorithm explanation with examples and diagrams

## Overview

The `mensalizePNADC` package identifies reference months in Brazil's quarterly PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) survey data and optionally computes monthly survey weights. This enables monthly (instead of quarterly) labor market analysis using PNADC microdata.

**Key Features:**

- **High accuracy**: **97.0%** determination rate on 28.4 million observations (2012-2025) — **identical to Stata**
- **Dynamic exception detection**: Automatically detects quarters needing relaxed timing rules
- **Fast processing**: **~1 minute** for 28.4 million rows (basic mode) — ~450,000 rows/sec
- **Minimal dependencies**: Requires `data.table` and `checkmate`; `sidrar` needed only for weight calibration
- **Flexible output**: Returns a crosswalk for easy joins with original data

## Installation

```r
# Install from GitHub
devtools::install_github("antrologos/mensalizePNADC")
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
#   Determination rate: 97.0%

# Join with original data
result <- merge(original_data, crosswalk,
  by = c("Ano", "Trimestre", "UPA", "V1008", "V1014", "V2003"))

# Now use ref_month_yyyymm for monthly analysis
result[, .(n = .N), by = ref_month_yyyymm]
```

## Important: Use Stacked Data for Best Results

The mensalization algorithm achieves **97.0% determination rate** when processing **stacked multi-quarter data**. If you process quarters individually, you'll only get ~65-75% determination.

| Processing Mode | Determination Rate |
|-----------------|-------------------|
| Per-quarter (single quarter) | 65-75% |
| Stacked (multi-quarter) | **97.0%** |

This is by design: PNADC uses a rotating panel where households are interviewed in the same relative month position each quarter. The algorithm combines birthday constraints from all quarters to narrow down which month the household belongs to.

**Recommended**: Stack at least 2 years of quarterly data before calling `mensalizePNADC()`.

## Required Input Variables

**Minimum (reference month identification only):**

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

**Additional (for `compute_weights = TRUE`):**

| Variable | Description |
|----------|-------------|
| `V1028` | Original quarterly survey weight |
| `UF` | State code |
| `posest` | Post-stratification cell |
| `posest_sxi` | Post-stratification group |

## Output Variables

| Variable | Type | Description |
|----------|------|-------------|
| `ref_month` | Date | Reference month (first day, e.g., "2023-01-01") |
| `ref_month_in_quarter` | Integer | Position in quarter: 1, 2, 3, or NA |
| `ref_month_yyyymm` | Integer | YYYYMM format (e.g., 202301) |
| `weight_monthly` | Numeric | Monthly weight (if `compute_weights = TRUE`); NA for indeterminate observations |

## Functions

| Function | Description |
|----------|-------------|
| `mensalizePNADC()` | Main function: identify months + optional weights |
| `identify_reference_month()` | Just reference month identification |
| `calibrate_monthly_weights()` | Rake weighting for monthly weights |
| `smooth_monthly_aggregates()` | Remove quarterly artifacts from series |
| `calibrate_to_sidra()` | Optional Bayesian calibration to match SIDRA |
| `fetch_monthly_population()` | Fetch population from SIDRA API |
| `validate_pnadc()` | Input data validation |

## Performance

- **97.0% determination rate** on 28.4 million observations (2012-2025)
- **~1 minute** for basic processing (28.4M rows) — ~450,000 rows/sec
- **~5 minutes** with weight calibration — ~95,000 rows/sec
- **Identical results** to the original Stata implementation

## Documentation

- 📖 **[Online Documentation](https://antrologos.github.io/mensalizePNADC/)** — Full pkgdown site with function reference
- 📖 **[Getting Started Vignette](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html)** — Detailed algorithm explanation with diagrams
- After installing: `vignette("getting-started", package = "mensalizePNADC")`

## Authors

- **Marcos Hecksher** - Original methodology and Stata implementation
- **Rogerio Barbosa** - R package maintainer

## References

- [IBGE PNADC Documentation](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
- Hecksher, M. (2024). Mensalizacao da PNADC. Working paper.

## License

MIT
