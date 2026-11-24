# Release checklist for trca 0.1.0

Status updated on 2026-11-24.

## Latest preparation

The published article citation is now included in the package and its
documentation. Its title, author, journal, year, and DOI were checked against
the local SAGE proof (ABS1497123-2.pdf, article opening page) and the
publication URL supplied by the maintainer. No volume or issue was assumed.

The package installs, its README regenerates, and `citation("trca")` returns
the published article plus a separate software citation. A fresh source
tarball with the vignette was built on 2026-11-24.

The fresh local `R CMD check --no-manual` passed with 0 errors, 0 warnings,
and 1 note for the unavailable optional package graphlayouts. The full
`--as-cran` check was attempted but aborted when the dependency check could
not reach CRAN. Remote incoming checks had been disabled for that attempt;
it must not be reported as a completed CRAN check. R 4.3.1 is the locally
available version; final submission checks must use current R.

Command-line access to api.github.com also failed. No connected browser
upload tool is available in this session. GitHub upload, CI, external builder
checks, CRAN submission, and the release archive remain pending.

Prepared tarball and check logs are kept in `release-artifacts/` (excluded
from package builds and Git). This is a release candidate, not a CRAN release.

## Repository

- [x] Create https://github.com/raminasotoudeh/trca (public).
- [x] Initialize this package directory as a standalone Git repository on `main`.
- [x] Configure `origin` and add GitHub installation instructions to README.Rmd and README.md.
- [x] Add package and issue-tracker URLs to DESCRIPTION.
- [ ] Push the initial commit. Command-line DNS/network access was unavailable in the preparation session; the browser upload was cancelled without committing an incomplete package. The remote repository remains empty.

From this directory in a network-enabled terminal:

```sh
git push -u origin main
```

If GitHub authentication fails, run `gh auth login --hostname github.com`
and then `gh auth setup-git`, and retry the push. The existing account
could not be verified from the restricted preparation session.

## Checks and CRAN submission

- [x] Build the source package with its vignette.
- [x] Local `R CMD check --no-manual`: R 4.3.1, macOS Sonoma 14.7.7, arm64;
  0 errors, 0 warnings, 1 note (optional graphlayouts unavailable).
  All 192 tests, examples, and the vignette passed. This check preceded the
  documentation and repository-metadata changes; no R code changed.
- [ ] Observe the five jobs in `.github/workflows/R-CMD-check.yaml` after
  the push. Manual reruns are available through `workflow_dispatch`.
- [ ] Submit the same source tarball to win-builder release and devel.
- [ ] Submit the source tarball to the macOS builder.
- [ ] Read all returned logs, resolve any issues, and replace all three
  TODO entries in cran-comments.md with actual results and R versions.
- [ ] Run a final `R CMD check --as-cran` with network access and all
  suggested packages available, then submit to CRAN.

The external builder uploads have **not** been submitted. The browser file
picker did not attach the tarball, and command-line network access was
unavailable. Do not report the pending checks as passing.

With devtools installed, these commands run the external checks from the
package directory:

```r
devtools::check_win_release(pkg = ".")
devtools::check_win_devel(pkg = ".")
devtools::check_mac_release(pkg = ".")
```

The check services send their results to the maintainer address in
DESCRIPTION. After reviewing their results and finalizing cran-comments.md:

```r
devtools::check(pkg = ".", cran = TRUE)
devtools::submit_cran(pkg = ".")
```

## Paper citation

- [x] Once the paper is published, verify the journal and DOI, then update
  `inst/CITATION`, `README.Rmd`, generated `README.md`, and
  `vignettes/trca.Rmd`. Add its DOI to DESCRIPTION when appropriate.
  Keep the paper citation separate from the software citation.

Published paper: Sotoudeh (2026), American Behavioral Scientist,
https://doi.org/10.1177/00027642261497123.

## Release archive

- [ ] After CRAN acceptance, tag the exact accepted source as `v0.1.0` and
  publish a GitHub release.
- [ ] Archive that release on Zenodo, verifying author, ORCID, version,
  MIT license, and the files before publishing the record.
- [ ] Add the assigned version DOI to the software citation and the
  paper's code-availability statement. Never guess or prefill a DOI.

No CRAN submission, release tag, or Zenodo record has been created.
