# CRAN submission comments — dqcheckrGUI 0.2.2

## R CMD check results

0 errors | 0 warnings | 1 note

This is a bug-fix and robustness update of a package currently on CRAN as
v0.2.0. Versions 0.2.1 and 0.2.2 were developed in sequence but only this
one is submitted, so it carries every change since 0.2.0.

It is also the package's feature-complete release: dqcheckrGUI now enters
maintenance mode and will receive bug fixes and compatibility updates only.
This is stated in the Description, the README, the package help page and
NEWS.md. Configuration features are developed in 'dqcheckr' itself, which
offers a script-based workflow that does not need this interface.

## Test environments

* macOS Tahoe 26.5.1 / aarch64-apple-darwin23, R 4.6.0, checked via
  `R CMD check --as-cran` on the built tarball (local, 2026-07-25):
  0 errors | 0 warnings | 1 note
* win-builder: R-devel (submitted 2026-07-__): results awaited
* win-builder: R-release (submitted 2026-07-__): results awaited

Checked against 'dqcheckr' 0.2.5 as published on CRAN (installed into a
separate library for the purpose), not a development build.

## Notes

### NOTE 1: Skipping HTML validation (local only)

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
  browser); the remaining unit suite runs in seconds. The full suite,
  including the app-driver tests, was run locally for this submission:
  512 passing, 0 failures.
* 'quarto' is newly declared in `Suggests`: one test helper probes for a
  Quarto installation via `requireNamespace("quarto")` before rendering a
  report, and skips when it is absent.

## Downstream dependencies

None.
