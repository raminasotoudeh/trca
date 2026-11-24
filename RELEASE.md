# Release checklist for trca 0.1.0

Status updated on 2026-11-24.

## Current status

The release candidate is built and locally checked. It has not been submitted
to CRAN. GitHub's public API reported an empty repository and no workflow runs
on 2026-11-24. The local push failed because this session could not access
usable GitHub credentials. The maintainer was asked to push from Terminal.

The journal citation has been updated throughout the package. Title, author,
journal, year, and DOI were verified against the local SAGE proof
(ABS1497123-2.pdf, article opening page) and the publication URL supplied by
the maintainer. No volume or issue was assumed. The method citation remains
separate from the software citation.

## Checks

- [x] Build source tarball with vignette.
- [x] Install package and validate `citation("trca")`.
- [x] Regenerate README from README.Rmd.
- [x] Install the optional graphlayouts dependency in the temporary check library.
- [x] Complete network-enabled `R CMD check --as-cran --no-manual` with all
  suggested packages and Pandoc available: R 4.3.1, macOS Sonoma 14.7.7,
  aarch64; 0 errors, 0 warnings, 2 notes (new submission; unable to verify
  current time). All 192 tests passed, with no test failures, warnings or
  skips; examples and vignette checks passed. This is not a current-R or
  PDF-manual check.
- [x] Upload the same tarball to win-builder R-release and R-devel. Both
  upload responses acknowledged trca_0.1.0.tar.gz (238984 bytes) on 2026-11-24.
- [ ] Read and save Windows results when emailed to the maintainer. Do not
  resubmit merely because results have not arrived; the service is asynchronous.
- [ ] Upload to macOS builder. Its form currently offers R-devel only. Two
  attempts to the advertised submission endpoint returned HTTP 502 on
  2026-11-24; no successful submission has been acknowledged.
- [ ] Push `main` to https://github.com/raminasotoudeh/trca and review all
  five jobs in `.github/workflows/R-CMD-check.yaml`.
- [ ] Resolve findings from current-R external checks and complete final
  CRAN submission checks, including the PDF manual.
- [ ] Finalize cran-comments.md with actual external results, then submit
  to CRAN and confirm the maintainer email.

Prepared tarball, submission receipts, checksum, and check logs are in
`release-artifacts/` (excluded from Git and package builds). Tarball SHA-256:
`547e27fefc596b409c36c6868a245d184ed4f2263e1411402880f3aa104c3252`.

## GitHub authentication

The first push was rejected for missing `workflow` scope. The maintainer
was instructed to refresh that scope. This session still reports invalid
credentials and cannot authenticate a push; this does not establish the
state of the maintainer's Terminal credentials.

From the package directory in the maintainer's Terminal:

```sh
git push -u origin main
```

If authentication or workflow scope is still missing:

```sh
gh auth login --hostname github.com --git-protocol https --web --scopes workflow
gh auth setup-git
git push -u origin main
```

## Paper citation

Sotoudeh, R. (2026). From typology to topology: Positions, construals, and
relational structure in attitudinal data. American Behavioral Scientist.
https://doi.org/10.1177/00027642261497123

## After CRAN acceptance

- [ ] Tag the exact accepted source as v0.1.0 and publish a GitHub release.
- [ ] Archive the release on Zenodo, verifying author, ORCID, version,
  license, and files before publishing.
- [ ] Add the assigned software DOI to the software citation and relevant
  code-availability documentation. Never guess or prefill a DOI.

No CRAN submission, release tag, or Zenodo record has been created.
