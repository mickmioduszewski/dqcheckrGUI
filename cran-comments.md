# CRAN submission comments — dqcheckrGUI 0.1.0

## R CMD check results

0 errors | 0 warnings | 3 notes

### NOTE 1: New submission

Always present for first-time submissions. Not a concern.

### NOTE 2: Namespaces in Imports field not imported from

```
Namespaces in Imports field not imported from:
  'DBI' 'DT' 'RSQLite' 'bslib' 'callr' 'dqcheckr' 'reactable' 'readr'
  'shinyAce' 'shinyFiles' 'shinyvalidate' 'yaml'
  All declared Imports should be used.
```

These packages are all used at runtime via `library()` calls inside
`inst/app/app.R`, which is the bundled Shiny application launched by
`run_app()`. R CMD check inspects `R/` source files for `::` usage but
does not analyse `inst/` for namespace consumption, so this NOTE is
expected and unavoidable for packages that bundle a Shiny app in
`inst/app/`.

### NOTE 3: Skipping HTML validation

```
Skipping checking HTML validation: 'tidy' doesn't look like recent enough HTML Tidy.
```

This NOTE is caused by an outdated `tidy` binary on the local machine.
It does not appear on CRAN servers, which have a current version of HTML
Tidy installed.

## Test environments

* macOS Tahoe 26.5.1 (aarch64-apple-darwin23), R 4.6.0 (local):
  0 errors | 0 warnings | 3 notes (all documented above)
* win-builder R-devel (results pending — submitted 2026-06-04):
  TBD — update before final submission

## Downstream dependencies

None.
