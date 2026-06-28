# CRAN submission comments — dqcheckrGUI 0.2.0

## R CMD check results

0 errors | 0 warnings | 1 note

## Test environments

* macOS Tahoe 26.5 / aarch64-apple-darwin23, R 4.6.0, checked via
  `rcmdcheck::rcmdcheck(<built tarball>, args = "--as-cran")` (local):
  0 errors | 0 warnings | 1 note
* win-builder: R-devel (2026-06-25 r90191 ucrt):
  0 errors | 0 warnings | 1 note

## Notes

### NOTE 1: Skipping HTML validation (local only)

```
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
```

Caused by an outdated `tidy` binary on the local check machine. Does not
appear on CRAN's check servers, which run a current version of 'HTML Tidy'.

### NOTE 2: Days since last update (win-builder only)

```
Maintainer: 'Mick Mioduszewski <mick@mioduszewski.net>'
Days since last update: 4
```

Informational only. This is a major version bump (0.1.0 → 0.2.0) with new
user-facing features (`config_dir` argument, first-run modal, file picker
improvements) and bug fixes documented in NEWS.md. The bugs on 'Windows' were self-detected and substantial enough to warrant an urgent fix.

## Package notes

* dqcheckrGUI is a 'Shiny' front-end for the
  ['dqcheckr'](https://cran.r-project.org/package=dqcheckr) package. Its
  sole exported function, `run_app()`, launches a bundled 'Shiny'
  application (`inst/app/`).
* The 12 packages declared in `Imports` but called only from the bundled
  'Shiny' app (`inst/app/R/*.R`, sourced at runtime via
  `system.file("app", package = "dqcheckrGUI")`) are referenced with one
  `@importFrom` each in `R/dqcheckrGUI-package.R`, so every declared Import
  is demonstrably used and no "Namespaces in Imports field not imported
  from" NOTE is raised.

## Downstream dependencies

None.
