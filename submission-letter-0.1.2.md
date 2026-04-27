Subject: PNADCperiods 0.1.2 — fixes Internet-resource handling per CRAN policy

Dear CRAN maintainers,

Thank you for the notice from Prof. Brian Ripley dated 2026-04-26
concerning the check failures of PNADCperiods on the
r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc,
r-release-linux-x86_64, and M1mac platforms.

The 22 test failures all stemmed from a single root cause: when the
SIDRA API (apisidra.ibge.gov.br) was unreachable during your check
runs, our internet-using functions threw `stop()` (or `warning()`)
errors instead of failing gracefully — in clear violation of the
policy:

  "Packages which use Internet resources should fail gracefully with
  an informative message if the resource is not available or has
  changed (and not give a check warning nor error)."

I have addressed this in PNADCperiods 0.1.2 with the following changes:

1. `fetch_monthly_population()` (R/fetch-sidra-population.R) and
   `fetch_sidra_rolling_quarters()` (R/fetch-sidra-series.R) now emit
   only `message()` (no `stop()`, no `warning()`) when the SIDRA API
   times out or returns an unexpected schema, and return `NULL`
   invisibly. The graceful-failure contract is documented in the
   `@return` Roxygen tags and a dedicated `@section Internet Resource
   Behaviour:` note.

2. `pnadc_apply_periods()` (R/pnadc-apply-periods.R) — which calls
   `fetch_monthly_population()` internally when `target_totals = NULL`
   — now detects the `NULL` return, emits an informative message, and
   returns the data with the crosswalk applied but without calibrated
   weights, instead of erroring.

3. Two new tests using `testthat::local_mocked_bindings()` verify the
   graceful path offline: see test-fetch-sidra-population.R and
   test-fetch-sidra-series.R.

4. The 22 previously-failing calibration and pipeline tests no longer
   touch the SIDRA API: they now build `target_totals` from local
   helpers (`create_mock_pop_targets()`), so they run reliably offline
   regardless of upstream availability.

Local R CMD check --as-cran on the 0.1.2 tarball reports 0 errors,
0 warnings, 1 note (the standard "CRAN resubmission" note).

I have also taken the opportunity to fix an unrelated bug in
`mensalize_sidra_series()` where trailing NA in the rolling-quarter
input could produce phantom values that silently echoed the value of
the same `mesnotrim` position three months earlier. NEWS.md documents
both the policy fix and the bug fix.

Please let me know if any further changes are required.

Best regards,
Rogerio Barbosa
rogerio.barbosa@iesp.uerj.br
