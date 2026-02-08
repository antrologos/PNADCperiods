# PNADCperiods

<!-- badges: start -->
[![R-CMD-check](https://github.com/antrologos/PNADCperiods/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/antrologos/PNADCperiods/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/antrologos/PNADCperiods/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/antrologos/PNADCperiods/actions/workflows/pkgdown.yaml)
[![codecov](https://codecov.io/gh/antrologos/PNADCperiods/branch/master/graph/badge.svg)](https://codecov.io/gh/antrologos/PNADCperiods)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Convert Brazil's quarterly PNADC survey data into sub-quarterly time series (monthly, fortnightly, weekly) and mensalize IBGE SIDRA aggregate series.

## Installation

```r
devtools::install_github("antrologos/PNADCperiods")
```

## Main Features

- **Microdata mensalization**: Identify reference months, fortnights, and weeks in PNADC microdata
- **SIDRA mensalization**: Convert 86+ rolling quarterly IBGE series to exact monthly values
- **Weight calibration**: Hierarchical raking to IBGE population totals
- **~97% monthly determination** with full data stacking; experimental strategies improve fortnight/week rates further

## Basic Usage

### Microdata Mensalization

```r
library(PNADCperiods)

# Build crosswalk identifying reference periods
crosswalk <- pnadc_identify_periods(pnadc_stacked)

# Apply to data with weight calibration
result <- pnadc_apply_periods(pnadc, crosswalk, weight_var = "V1028", anchor = "quarter")
```

### SIDRA Series Mensalization

```r
# Fetch rolling quarterly data from SIDRA
rolling <- fetch_sidra_rolling_quarters(theme = "labor_market")

# Convert to exact monthly series
monthly <- mensalize_sidra_series(rolling)
```

## Key Functions

### Microdata

| Function | Description |
|----------|-------------|
| `pnadc_identify_periods()` | Build crosswalk: identify months/fortnights/weeks |
| `pnadc_apply_periods()` | Apply crosswalk + calibrate weights |
| `pnadc_experimental_periods()` | Boost rates with probabilistic strategies |
| `validate_pnadc()` | Validate input columns |

### SIDRA Series

| Function | Description |
|----------|-------------|
| `get_sidra_series_metadata()` | List 86+ available PNADC series |
| `fetch_sidra_rolling_quarters()` | Download rolling quarter data from SIDRA |
| `mensalize_sidra_series()` | Convert rolling quarters to exact monthly |
| `fetch_monthly_population()` | Fetch population totals from SIDRA |

## Documentation

- [Getting Started](https://antrologos.github.io/PNADCperiods/articles/getting-started.html)
- [SIDRA Mensalization](https://antrologos.github.io/PNADCperiods/articles/sidra-mensalization.html)
- [How It Works](https://antrologos.github.io/PNADCperiods/articles/how-it-works.html)
- [Applied Examples](https://antrologos.github.io/PNADCperiods/articles/applied-examples.html)
- [Full Reference](https://antrologos.github.io/PNADCperiods/reference/index.html)

## Authors

- **Rogerio J. Barbosa** (Ceres-IESP/UERJ) -- R package, dashboard, and website
- **Marcos Hecksher** (Ipea) -- Mensalization methodology

## Credits

Original PNADC data is collected by the Brazilian Institute of Geography and Statistics (IBGE). The {PNADCperiods} package is developed at the [Center for the Study of Wealth and Social Stratification (Ceres - IESP/UERJ)](https://ceres-iesp.uerj.br/) and the Institute for Applied Economic Research (Ipea), Brazil.

**Citation:**

> Barbosa, Rogerio J; Hecksher, Marcos. (2026). PNADCperiods: Identify Reference Periods in Brazil's PNADC Survey Data. R package version v0.1.0. https://github.com/antrologos/PNADCperiods

```r
citation("PNADCperiods")
```

## Getting Help

- [GitHub Issues](https://github.com/antrologos/PNADCperiods/issues) for bug reports and questions

## License

MIT
