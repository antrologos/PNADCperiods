## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 (x86_64), R 4.5.0
* GitHub Actions: macOS-latest (R release), windows-latest (R release),
  ubuntu-latest (R devel, release, oldrel-1)
* win-builder: R-devel, R-release

## Package notes

* This is a new submission.

* All vignettes use `eval = FALSE` for code chunks because they require
  access to PNADC microdata files (~9 GB) that cannot be bundled with the
  package. Pre-computed figures are included via markdown image references
  so that vignettes display meaningful output despite non-evaluated code.

* Functions that access the IBGE SIDRA API (`fetch_sidra_rolling_quarters()`,
  `fetch_monthly_population()`, `mensalize_sidra_series()`) use
  `\donttest{}` in examples. All tests involving internet access are
  wrapped in `testthat::skip_on_cran()`.

## Downstream dependencies

There are currently no downstream dependencies for this package.
