# CRAN submission comments — dqcheckrGUI 0.2.1

## R CMD check results

0 errors | 0 warnings | 2 notes

This is a bug-fix update (v0.2.1) of a package currently on CRAN as v0.2.0.

## Test environments

* macOS Tahoe 26.5.1 / aarch64-apple-darwin23, R 4.6.0 (2026-04-24), checked
  via `rcmdcheck::rcmdcheck(<pkg dir>, args = "--as-cran")` (local,
  2026-07-04): 0 errors | 0 warnings | 2 notes
* win-builder: R-devel (submitted 2026-07-04): results awaited

## Notes

### NOTE 1: Days since last update (expected on all platforms)

```
Maintainer: 'Mick Mioduszewski <mick@mioduszewski.net>'
Days since last update: 5
```

Informational. 0.2.1 is a focused bug-fix release following an internal
quality review of 0.2.0: it fixes broken run-history displays for projects
created by the app's own first-run scaffold (relative `snapshot_db` paths
never resolved), adds HTML escaping for externally supplied file names
rendered in tables, stops the Global Config editor discarding hand-added
YAML keys, and closes wizard state-leak and rename-overwrite bugs. These
are user-facing defects worth shipping promptly rather than holding.

### NOTE 2: Skipping HTML validation (local only)

```
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
```

Caused by an outdated `tidy` binary on the local check machine. Does not
appear on CRAN's check servers, which run a current version of 'HTML Tidy'.

## Package notes

* dqcheckrGUI is a 'Shiny' front-end for the
  ['dqcheckr'](https://cran.r-project.org/package=dqcheckr) package. Its
  sole exported function, `run_app()`, launches a bundled 'Shiny'
  application (`inst/app/`).
* The packages declared in `Imports` but called only from the bundled
  'Shiny' app (`inst/app/R/*.R`, sourced at runtime via
  `system.file("app", package = "dqcheckrGUI")`) are referenced with one
  `@importFrom` each in `R/dqcheckrGUI-package.R`, so every declared Import
  is demonstrably used and no "Namespaces in Imports field not imported
  from" NOTE is raised.
* All 'shinytest2' app-driver tests are `skip_on_cran()` (they require a
  browser); the remaining unit suite runs in seconds.
* Works with the dqcheckr currently on CRAN (0.2.2); also tested against
  the upcoming dqcheckr 0.2.3 submission.

## Downstream dependencies

None.
