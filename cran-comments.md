## Resubmission

This is a resubmission. In this version I have:

* Expanded all acronyms in the Description field on first use: PNADC
  (Pesquisa Nacional por Amostra de Domicilios Continua), IBGE (Instituto
  Brasileiro de Geografia e Estatistica), UPA (Primary Sampling Unit),
  SIDRA (Sistema IBGE de Recuperacao Automatica), and API (Application
  Programming Interface).

* Added a proper reference in the Description field with URL for the
  methodology: Hecksher (2020)
  <https://repositorio.ipea.gov.br/handle/11058/9859>.

* Replaced \dontrun{} with \donttest{} for all examples that access
  external APIs (SIDRA). These examples fetch a small subset of series
  and complete in under 5 seconds. Only examples requiring large local
  microdata files (~9 GB) that cannot be distributed retain \dontrun{}.

* Removed commented-out code lines from examples.

* Created runnable examples (no wrapper) for `validate_pnadc()` and
  `get_sidra_series_metadata()` using minimal synthetic data.

## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE is about this being a new submission to CRAN.

## Test environments

* Local: Windows 11 (x86_64), R 4.5.0
* GitHub Actions: ubuntu-latest (R-devel, R-release, R-oldrel-1),
  windows-latest (R-release), macOS-latest (R-release)
* win-builder: R-devel, R-release

## Package notes

* All vignettes use `eval = FALSE` for code chunks because they require
  access to PNADC microdata files (~9 GB) that cannot be bundled with the
  package. Pre-computed figures are included via markdown image references
  so that vignettes display meaningful output despite non-evaluated code.

* Functions that access the IBGE SIDRA API (`fetch_sidra_rolling_quarters()`,
  `fetch_monthly_population()`, `mensalize_sidra_series()`) use
  `\donttest{}` in examples. All tests involving internet access are
  wrapped in both `testthat::skip_on_cran()` and
  `testthat::skip_if_offline()`.

* Functions that require large local microdata files
  (`pnadc_identify_periods()`, `pnadc_apply_periods()`,
  `pnadc_experimental_periods()`, `compute_starting_points_from_microdata()`)
  use `\dontrun{}` in examples because these require ~9 GB of survey
  microdata that cannot be distributed with the package.

## Downstream dependencies

There are currently no downstream dependencies for this package.
