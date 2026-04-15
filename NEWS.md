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
