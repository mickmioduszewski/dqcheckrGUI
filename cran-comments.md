# CRAN submission comments — dqcheckrGUI 0.2.0

## R CMD check results

0 errors | 0 warnings | 2 notes

## Test environments

* macOS Tahoe 26.5 / aarch64-apple-darwin23, R 4.6.0, checked via
  `rcmdcheck::rcmdcheck(<built tarball>, args = "--as-cran")` (local):
  0 errors | 0 warnings | 2 notes
* win-builder: R-devel — results pending (submitted 2026-06-28)

## Notes

### NOTE 1: Possibly invalid URL

```
Found the following (possibly) invalid URLs:
  URL: https://mickmioduszewski.github.io/dqcheckrGUI/
    From: DESCRIPTION
    Status: 404
    Message: Not Found
```

The 'pkgdown' documentation site is deployed via 'GitHub Actions' and will
be live at that URL once 'GitHub Pages' is enabled in the repository
settings. The workflow was pushed on 2026-06-28 alongside this version. The
URL is valid; the 404 is a transient state during the deployment window.

### NOTE 2: Skipping HTML validation

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
* The 12 packages declared in `Imports` but called only from the bundled
  'Shiny' app (`inst/app/R/*.R`, sourced at runtime via
  `system.file("app", package = "dqcheckrGUI")`) are referenced with one
  `@importFrom` each in `R/dqcheckrGUI-package.R`, so every declared Import
  is demonstrably used and no "Namespaces in Imports field not imported
  from" NOTE is raised.

## Downstream dependencies

None.
