# cran-comments

## Submission type

This is a new submission.

## Test environments

- local macOS (darwin), R 4.5.x — 0 errors | 0 warnings | 0 notes
- GitHub Actions:
  - windows-latest, R release
  - macOS-latest, R release
  - ubuntu-22.04, R release and R devel
  - ubuntu-latest, R release and R devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for the reviewer

The package name resembles the existing CRAN package `Tplyr`, by the same
maintainer and organisation. `tplyr2` is a ground-up successor with a different
(spec-based) API rather than a new version of `Tplyr`; the two are intended to
coexist on CRAN while users migrate. `vignette("migration")` documents the
mapping between the two APIs.
