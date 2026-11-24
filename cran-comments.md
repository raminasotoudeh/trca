## Submission

This is a new submission. trca implements Topological Relational Class
Analysis (tRCA), a method for recovering positions, construals, and the
relational topology among them from attitudinal survey data. It accompanies
Sotoudeh (2026), published in American Behavioral Scientist,
<doi:10.1177/00027642261497123>.

## Test environments

* local macOS Sonoma 14.7.7 (aarch64), R 4.3.1,
  `R CMD check --as-cran --no-manual` (2026-11-24), with network access,
  all suggested packages installed, and Pandoc available
* win-builder R-release and R-devel: uploaded 2026-11-24; results pending
* macOS builder R-devel: upload endpoint returned HTTP 502; not confirmed
* GitHub Actions: pending successful repository push and workflow results

## R CMD check results

Latest local result: 0 errors | 0 warnings | 2 notes

* New submission.
* Unable to verify current time.

All 192 tests passed, with no failures, warnings, or skips. Examples and
vignette checks passed. The local R version is older than current release;
external checks on current R and a PDF-manual check remain pending.
The earlier missing graphlayouts and Pandoc issues have been resolved.

This file is not yet ready for submission. Replace pending external checks
with actual results and review any findings before submitting to CRAN.

## Notes for the reviewer

* The package has no compiled code.
* Functions that use random numbers (k-means, permutation and reference
  draws) take an optional `seed` argument. They call `set.seed()` only when
  a seed is supplied; otherwise they draw from the session's random number
  stream like any other R function.
* Messages go through `message()`; plotting functions return ggplot objects
  instead of printing. ggplot2 and the other plotting dependencies are in
  Suggests and are checked with `requireNamespace()`.
* All examples run in well under 5 seconds; none are wrapped in
  `\dontrun{}`.
* The vignette uses the `knitr::knitr` engine with the markdown package and
  does not require pandoc.
