## Submission

This is a new submission. trca implements Topological Relational Class
Analysis (tRCA), a method for recovering positions, construals, and the
relational topology among them from attitudinal survey data. It accompanies
a forthcoming paper by the maintainer.

## Test environments

* local macOS 26.5 (aarch64), R 4.6.1 (previously recorded check)
* local macOS Sonoma 14.7.7 (aarch64), R 4.3.1, `R CMD check --no-manual`
  (2026-11-10; optional graphlayouts unavailable; incoming checks not run)
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel,
  release, oldrel-1) <!-- TODO: pending the first push to https://github.com/raminasotoudeh/trca and verified workflow results -->
* win-builder (devel and release) <!-- TODO: not submitted; record results after successful uploads -->
* macOS builder (release) <!-- TODO: not submitted; record results after a successful upload -->

## R CMD check results

Previously recorded local `--as-cran` result: 0 errors | 0 warnings | 1 note

* This is a new release.

Additional local check on 2026-11-10 (R 4.3.1, `--no-manual`, without
`--as-cran`): 0 errors | 0 warnings | 1 note. The note is for the optional
suggested package graphlayouts, which was unavailable in the checking
environment. All 192 tests passed; examples and the vignette passed.
Network restrictions prevented a fresh CRAN incoming check in this session.

External check results are still pending; this file is not yet ready for
submission.

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
