## Resubmission addressing CRAN policy notice (Brian Ripley, 2026-04-26)

This release responds to a CRAN policy notice pointing out that the
SIDRA-API-dependent tests were giving check warnings and errors when the
upstream service was momentarily unreachable, in violation of:

> 'Packages which use Internet resources should fail gracefully with an
> informative message if the resource is not available or has changed
> (and not give a check warning nor error).'

In this version I have:

* Replaced every `stop()` and `warning()` triggered by SIDRA-API
  unreachability with `message()` plus `return(invisible(NULL))`.
  Affected functions: `fetch_monthly_population()`,
  `fetch_sidra_rolling_quarters()`, and the `target_totals = NULL`
  branch of `pnadc_apply_periods()` (which now returns the data with
  the crosswalk applied but uncalibrated weights, instead of erroring).
* Added two new offline tests using `testthat::local_mocked_bindings()`
  that explicitly verify the graceful-failure path:
  `test-fetch-sidra-population.R` and `test-fetch-sidra-series.R`.
* Removed the implicit SIDRA dependency from 22 calibration and
  integration tests by injecting locally-constructed `target_totals`
  mocks. These tests now run offline on CRAN regardless of API
  availability.

The package also includes bug fixes for `mensalize_sidra_series()`
where trailing `NA`s in the rolling-quarter input previously produced
phantom mensalized values; see NEWS.md.

## R CMD check results

0 errors | 0 warnings | 1 note (about CRAN resubmission)

## Test environments

* Local: Windows 11 (x86_64), R 4.5.0
* GitHub Actions: ubuntu-latest (R-devel, R-release, R-oldrel-1),
  windows-latest (R-release), macOS-latest (R-release)
* win-builder: R-devel, R-release

## Package notes

* All vignettes use `eval = FALSE` for code chunks because they require
  access to PNADC microdata files (~9 GB) that cannot be bundled with the
  package. Pre-computed figures are included via markdown image references.

* Functions that access the IBGE SIDRA API (`fetch_sidra_rolling_quarters()`,
  `fetch_monthly_population()`, `mensalize_sidra_series()`) use
  `\donttest{}` in examples and now fail gracefully (informative
  `message()`, return `NULL` invisibly) when the API is unreachable.
  Tests that would still hit the live API are wrapped in
  `testthat::skip_on_cran()` and `testthat::skip_if_offline()`.

* Functions that require large local microdata files
  (`pnadc_identify_periods()`, `pnadc_apply_periods()`,
  `pnadc_experimental_periods()`, `compute_starting_points_from_microdata()`)
  use `\dontrun{}` in examples because these require ~9 GB of survey
  microdata that cannot be distributed with the package.

## Downstream dependencies

There are currently no downstream dependencies for this package.
