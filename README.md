# mensalizePNADC

<!-- badges: start -->
[![R-CMD-check](https://github.com/antrologos/mensalizePNADC/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/antrologos/mensalizePNADC/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/antrologos/mensalizePNADC/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/gh/antrologos/mensalizePNADC/branch/master/graph/badge.svg)](https://codecov.io/gh/antrologos/mensalizePNADC)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

Convert Brazil's quarterly PNADC survey data into monthly time series.

## Overview

The `mensalizePNADC` package identifies reference months in Brazil's quarterly PNADC (Pesquisa Nacional por Amostra de Domicilios Continua) microdata and optionally computes monthly survey weights.

**Why mensalization?** PNADC quarterly statistics are actually moving averages of three months. Mensalization recovers the specific month each observation refers to, enabling true monthly labor market analysis.

- **97.0% determination rate** on 28.4M observations (2012-2025)
- **Validated methodology** based on Hecksher (2024)
- **Fast**: ~1 minute for 28M rows

## Key Functions

| Function | Description |
|----------|-------------|
| `mensalizePNADC()` | Main function: identify reference months + optional weight calibration |
| `identify_reference_month()` | Reference month identification only |
| `mensalize_annual_pnadc()` | Process annual PNADC data with mensalization crosswalk |
| `calibrate_monthly_weights()` | Hierarchical rake weighting for monthly weights |
| `fetch_monthly_population()` | Fetch population totals from IBGE SIDRA API |
| `smooth_monthly_aggregates()` | Remove quarterly artifacts from monthly series |
| `validate_pnadc()` | Validate input data has required columns |

## Installation

```r
# Install from GitHub
devtools::install_github("antrologos/mensalizePNADC")
```

## Quick Example

```r
library(mensalizePNADC)
library(data.table)

# Load stacked PNADC data (2+ years recommended for best determination rate)
pnadc <- fread("pnadc_stacked.csv")

# Identify reference months only
result <- mensalizePNADC(pnadc)

# With monthly weight calibration (requires sidrar package)
result <- mensalizePNADC(pnadc, compute_weights = TRUE)

# Use in analysis
result[, .(n = .N), by = ref_month_yyyymm]
```

**Key parameters:**
- `compute_weights = TRUE`: Compute calibrated monthly weights (default: FALSE)
- `output`: Return type - `"crosswalk"` (default), `"microdata"`, or `"aggregates"`

**Output columns:**
- `ref_month`: Reference month as Date
- `ref_month_in_quarter`: Position in quarter (1, 2, 3) or NA if undetermined
- `ref_month_yyyymm`: Month in YYYYMM integer format
- `weight_monthly`: Calibrated monthly weight (if `compute_weights = TRUE`)

**Required input columns:**
- Reference month: `Ano`, `Trimestre`, `UPA`, `V1014`, `V2008`, `V20081`, `V20082`, `V2009`
- Join keys: `V1008`, `V2003`
- For weights: add `V1028`, `UF`, `posest`, `posest_sxi`

For complete examples, see the [Get Started guide](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html).

## Documentation

| Guide | Description |
|-------|-------------|
| [Get Started](https://antrologos.github.io/mensalizePNADC/articles/getting-started.html) | Installation and first steps |
| [Download Data](https://antrologos.github.io/mensalizePNADC/articles/download-and-prepare.html) | Prepare PNADC microdata |
| [Applied Examples](https://antrologos.github.io/mensalizePNADC/articles/applied-examples.html) | COVID, recession, minimum wage validation |
| [How It Works](https://antrologos.github.io/mensalizePNADC/articles/how-it-works.html) | Algorithm details |
| [Reference](https://antrologos.github.io/mensalizePNADC/reference/index.html) | Function documentation |

## Authors

- **Marcos Hecksher** - Original methodology
- **Rogerio Barbosa** - R package maintainer

## References

- [IBGE PNADC Documentation](https://www.ibge.gov.br/estatisticas/sociais/trabalho/9171-pesquisa-nacional-por-amostra-de-domicilios-continua-mensal.html)
- Hecksher, M. (2024). Mensalizacao da PNADC. Working paper.

## License

MIT
