# PNADCperiods 0.1.2

## CRAN policy compliance (response to Brian Ripley, 2026-04-26)

* Internet-using functions now fail gracefully with an informative
  `message()` (no `warning()`, no `stop()`) and return `NULL` invisibly
  when the SIDRA API is unreachable or returns an unexpected schema.
  Affects `fetch_monthly_population()`, `fetch_sidra_rolling_quarters()`,
  and `pnadc_apply_periods()` (which falls back to returning the data
  with the crosswalk applied but no calibrated weights when SIDRA is
  unavailable).
* New mocked-API tests (`testthat::local_mocked_bindings`) verify the
  graceful-failure behaviour offline.
* Test suite no longer depends on the SIDRA API: 22 calibration and
  pipeline tests now use locally-constructed `target_totals` mocks
  instead of fetching live SIDRA population data.

## Bug fixes — mensalize_sidra_series()

* Trailing `NA` in the rolling-quarter input (e.g., when IBGE has
  published IPCA/INPC for a month but not yet the corresponding PNADC
  rolling quarter) no longer produces phantom mensalized values that
  silently echoed the value of the same `mesnotrim` position three
  months earlier. The mask now zeros out only trailing positions after
  the last observed `rq`, leaving leading positions intact.
* IPCA lag is pre-computed before the PNADC filter is applied, removing
  contamination of starting points by future-IPCA observations.
* INPC lag is pre-computed in `compute_series_starting_points()`, with
  test coverage for derived-series propagation through aggregates.

## Documentation

* README, vignettes and pkgdown navbar updated to recommend
  `install.packages("PNADCperiods")` now that the package is on CRAN.

# PNADCperiods 0.1.1

## CRAN resubmission

* Expanded all acronyms in DESCRIPTION (IBGE, SIDRA, UPA, API)
* Added Hecksher (2020) reference with URL
* Replaced `\dontrun{}` with `\donttest{}` for API examples;
  created runnable examples for `validate_pnadc()` and
  `get_sidra_series_metadata()`
* Fixed missing `anchor` parameter in `compute_starting_points_from_microdata()`
* Removed `.onLoad` that globally set `data.table` thread count
* Added `skip_on_cran()` to all API-dependent tests
* Added `fst` to Suggests

# PNADCperiods 0.1.0

## Initial CRAN release

### Microdata period identification
* `pnadc_identify_periods()`: Build crosswalk mapping survey observations to
  months, fortnights, and weeks using IBGE's technical break rules and
  respondent birthdates.
* `pnadc_apply_periods()`: Apply crosswalk and calibrate weights to SIDRA
  population totals with adaptive hierarchical cell definitions.
* `pnadc_experimental_periods()`: Probabilistic assignment and UPA aggregation
  strategies for improved fortnight/week determination rates.
* `validate_pnadc()`: Input validation for required PNADC variables.

### SIDRA mensalization module
* `get_sidra_series_metadata()`: Metadata for 86+ PNADC rolling quarter series
  available from the IBGE SIDRA API.
* `fetch_sidra_rolling_quarters()`: Download rolling quarter data from SIDRA.
* `mensalize_sidra_series()`: Convert rolling quarter series into exact monthly
  estimates via matrix inversion, without requiring microdata access.
* `fetch_monthly_population()`: Fetch monthly population totals from SIDRA.
* `compute_starting_points_from_microdata()`: Compute y0 starting points for
  the mensalization algorithm from survey microdata.
