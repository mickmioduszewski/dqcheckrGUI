# CRAN submission comments — dqcheckrGUI 0.1.0

## R CMD check results

0 errors | 0 warnings | 2 notes

This is a new submission.

## Test environments

* macOS Tahoe 26.5 / aarch64-apple-darwin23, R 4.6.0, checked via
  `rcmdcheck::rcmdcheck(<built tarball>, args = "--as-cran")` (local):
  0 errors | 0 warnings | 2 notes
* win-builder: R-devel, Windows Server 2022 (submitted 2026-06-08;
  results pending — to be attached here once the results email arrives)
* win-builder: R-release, Windows Server 2022 (submitted 2026-06-08;
  results pending — to be attached here once the results email arrives)

## Notes

### NOTE 1: New submission

```
Maintainer: 'Mick Mioduszewski <mick@mioduszewski.net>'
New submission
```

Always present for first-time submissions. Not a concern.

### NOTE 2: Skipping HTML validation

```
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
```

Caused by an outdated `tidy` binary on the local check machine (used only
to validate the rendered HTML manual). It does not appear on CRAN's check
servers, which run a current version of HTML Tidy.

## Resolved since the 2026-06-04 draft of these notes

A third NOTE previously appeared here:

```
Namespaces in Imports field not imported from:
  'DBI' 'DT' 'RSQLite' 'bslib' 'callr' 'dqcheckr' 'reactable' 'readr'
  'shinyAce' 'shinyFiles' 'shinyvalidate' 'yaml'
  All declared Imports should be used.
```

These packages are called only from the bundled Shiny app
(`inst/app/R/*.R`), sourced at runtime via
`system.file("app", package = "dqcheckrGUI")` rather than loaded as part
of the package namespace -- so static analysis of `R/` cannot see them as
used. Rather than leave this NOTE for CRAN reviewers to query, added
`R/dqcheckrGUI-package.R` with one `@importFrom` reference per such
package (the same approach used in `dqcheckr` for its template-only
dependencies). This makes each declared Import demonstrably used and
removes the NOTE entirely.

## Package notes

* dqcheckrGUI is a Shiny front-end for the
  ['dqcheckr'](https://cran.r-project.org/package=dqcheckr) package. Its
  sole exported function, `run_app()`, launches a bundled Shiny application
  (`inst/app/`).
* The example for `run_app()` is wrapped in `\dontrun{}` because it
  launches an interactive Shiny application that blocks the R session.

## Downstream dependencies

None (new submission).
