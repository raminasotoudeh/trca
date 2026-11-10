# Release checklist for trca 0.1.0

Status recorded on 2026-11-10.

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

- [ ] Once the paper is published, verify the journal and DOI, then update
  `inst/CITATION`, `README.Rmd`, generated `README.md`, and
  `vignettes/trca.Rmd`. Add its DOI to DESCRIPTION when appropriate.
  Keep the paper citation separate from the software citation.

No publication DOI has been verified. The existing forthcoming citation is
retained.

## Release archive

- [ ] After CRAN acceptance, tag the exact accepted source as `v0.1.0` and
  publish a GitHub release.
- [ ] Archive that release on Zenodo, verifying author, ORCID, version,
  MIT license, and the files before publishing the record.
- [ ] Add the assigned version DOI to the software citation and the
  paper's code-availability statement. Never guess or prefill a DOI.

No CRAN submission, release tag, or Zenodo record has been created.
