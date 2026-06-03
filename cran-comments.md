# CRAN submission comments — dqcheckrGUI 0.1.0

## R CMD check results

0 errors | 0 warnings | 1 note

### NOTE: Namespaces in Imports field not imported from

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

## Test environments

* macOS Tahoe 26.5.1 (aarch64), R 4.6.0

## Downstream dependencies

None.
