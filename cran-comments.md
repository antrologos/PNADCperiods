## Summary

This release does two things: it restores access to IBGE's SIDRA data,
which broke upstream in September 2026, and it removes a final adjustment
step from the mensalization algorithm. Details below.

### 1. SIDRA access moved to IBGE's aggregated-data API v3

Between 14 and 16 September 2026, IBGE placed the host this package
queried, `apisidra.ibge.gov.br`, behind a Cloudflare browser challenge.
It now answers HTTP 403 to most programmatic requests: ten identical
requests measured on 2026-09-18 returned three successes and seven
challenges, independent of the request headers. Every data-fetching
function in version 0.1.2 is therefore unusable in practice.

The two affected functions, `fetch_sidra_rolling_quarters()` and
`fetch_monthly_population()`, now query IBGE's aggregated-data API v3 at
`servicodados.ibge.gov.br/api/v3/agregados`, which serves the same
aggregates and is not challenged. The public interface is unchanged, and
so is the graceful-failure behaviour introduced in 0.1.2: an informative
`message()` and `NULL` returned invisibly, never a `warning()` or a
`stop()`.

All 90 series were validated against the last dataset retrieved through
the old host on 2026-09-14: every value over the 569 shared periods is
identical.

### 2. Dependency change: `sidrar` replaced by `curl` and `jsonlite`

`sidrar` was removed from Imports. Its 0.5.1 release added a fallback to
the same v3 service, but that fallback rejects whole-series period
selections (`p/all`) and decimal modifiers (`/d/`), which every request
this package makes uses; it would fail for all 90 series.

The replacement reduces the dependency tree rather than growing it:
`sidrar` brought in `magrittr`, `httr`, `rjson`, `rvest`, `stringr` and
`xml2`, whereas `jsonlite` and `curl` have no R dependencies of their
own.

### 3. Breaking change in `mensalize_sidra_series()`

The function no longer re-anchors each month-position trio to its
rolling-quarter mean as a final step; it now stops at the cumulative
sum. This keeps previously published months stable across IBGE releases,
at the cost of a small drift between the trio average and the official
rolling-quarter value. The internal helper implementing the legacy step
is preserved and still unit-tested, so historical analyses can be
reproduced. See NEWS.md.

## R CMD check results

0 errors | 0 warnings | 0 notes

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
  `\donttest{}` in examples and fail gracefully (informative `message()`,
  return `NULL` invisibly) when the API is unreachable. Tests that would
  still hit the live API are wrapped in `testthat::skip_on_cran()` and
  `testthat::skip_if_offline()`.

* Functions that require large local microdata files
  (`pnadc_identify_periods()`, `pnadc_apply_periods()`,
  `pnadc_experimental_periods()`, `compute_starting_points_from_microdata()`)
  use `\dontrun{}` in examples because these require ~9 GB of survey
  microdata that cannot be distributed with the package.

## Downstream dependencies

There are currently no downstream dependencies for this package.
