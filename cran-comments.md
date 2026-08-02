# cran-comments

## Submission type

Resubmission of a new submission. The previous submission's incoming pretest
returned two NOTEs; both are addressed below.

### 1. Invalid URL in README.md

    URL: https://github.com/atorus-research/tplyr2/blob/main/LICENSE.md
    Status: 404

The README's MIT badge linked to `LICENSE.md`, which did not exist — the
repository carried only the `LICENSE` stub named in the `License:` field. The
full MIT text has been added as `LICENSE.md` (and `.Rbuildignore`d, so it is not
shipped in the tarball), which makes the link resolve.

### 2. HTML validation problems in the manual

    build_col_labels.Rd:20:      <n> is not recognized
    resolve_pairwise_labels.Rd:10: <reference> / <comparison> not recognized

Three placeholder tokens appeared as bare text in roxygen comments, so roxygen2
emitted them as raw HTML (`\if{html}{\out{<n>}}`) and the validator saw unknown
tags. They are now marked up as code, which escapes the angle brackets. The
generated Rd files contain no `\out{<...>}` passthroughs.

## Remaining NOTE

"New submission" is expected — tplyr2 is not currently on CRAN.

## Test environments

- local macOS (darwin), R 4.5.x — 0 errors | 0 warnings | 0 notes
- win-builder, R-devel
- R-hub: linux (R-devel), windows (R-devel), macos (R-devel), atlas — all
  Status: OK
- GitHub Actions: windows-latest, macOS-latest, and ubuntu (22.04 and latest)
  on both R release and R devel

## R CMD check results

0 errors | 0 warnings | 0 notes locally.

## Notes for the reviewer

The package name resembles the existing CRAN package `Tplyr`, by the same
maintainer and organisation. `tplyr2` is a ground-up successor with a different
(spec-based) API rather than a new version of `Tplyr`; the two are intended to
coexist on CRAN while users migrate. `vignette("migration")` documents the
mapping between the two APIs.
