# CRAN submission comments — dqcheckrGUI 0.1.0

## Resubmission

This is a resubmission of version 0.1.0. The previous submission (also
0.1.0) was rejected with two reviewer requests, both addressed here:

1. **Single-quote software/package/API names in Title and Description.**
   "Shiny" was removed from the `Title` (now "Point-and-Click GUI Client for
   'dqcheckr'"), and the remaining software names are single-quoted in both
   fields, using the correct case for the package name (`'shiny'`,
   `'dqcheckr'`).
2. **Use `if(interactive())` rather than `\dontrun{}` for interactive-only
   examples.** The example for `run_app()` — the package's sole exported
   function, which launches a blocking interactive Shiny application — now
   reads `if (interactive()) { run_app() }`.

## R CMD check results

0 errors | 0 warnings | 2 notes

## Test environments

* macOS Tahoe 26.5 / aarch64-apple-darwin23, R 4.6.0, checked via
  `rcmdcheck::rcmdcheck(<built tarball>, args = "--as-cran")` (local):
  0 errors | 0 warnings | 2 notes
* win-builder: R-devel and R-release, Windows Server 2022 (pending for this
  resubmission)

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

## Package notes

* dqcheckrGUI is a 'shiny' front-end for the
  ['dqcheckr'](https://cran.r-project.org/package=dqcheckr) package. Its
  sole exported function, `run_app()`, launches a bundled 'shiny'
  application (`inst/app/`).
* The 12 packages declared in `Imports` but called only from the bundled
  'shiny' app (`inst/app/R/*.R`, sourced at runtime via
  `system.file("app", package = "dqcheckrGUI")`) are referenced with one
  `@importFrom` each in `R/dqcheckrGUI-package.R`, so every declared Import
  is demonstrably used and no "Namespaces in Imports field not imported
  from" NOTE is raised.

## Downstream dependencies

None (new submission).
