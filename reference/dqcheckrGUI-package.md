# dqcheckrGUI: Point-and-Click GUI Client for 'dqcheckr'

A point-and-click 'shiny' front-end for dqcheckr – configure and run
automated data quality checks on recurring dataset deliveries without
writing any R code.

## Getting started

Launch the app with
[`run_app`](https://mickmioduszewski.github.io/dqcheckrGUI/reference/run_app.md).
On first run it will offer to create the standard project layout
(`config/`, `data/`, `reports/`) in the chosen directory.

The config directory can be set via the `DQCHECKR_CONFIG_DIR`
environment variable or the `config_dir` argument to
[`run_app`](https://mickmioduszewski.github.io/dqcheckrGUI/reference/run_app.md).
See the getting-started vignette for a full walkthrough including
Windows and OneDrive setups:
[`vignette("dqcheckrGUI", package = "dqcheckrGUI")`](https://mickmioduszewski.github.io/dqcheckrGUI/articles/dqcheckrGUI.md).

## Status

dqcheckrGUI is feature-complete and is maintained for corrections only.
Configuration features are developed in dqcheckr itself, which offers a
script-based workflow that does not need this interface. Existing
deployments continue to work unchanged.

## Related packages

dqcheckrGUI delegates all data processing to dqcheckr. See
[`vignette("dqcheckr", package = "dqcheckr")`](https://mickmioduszewski.github.io/dqcheckr/articles/dqcheckr.html)
for the core package introduction and
[`vignette("specification", package = "dqcheckr")`](https://mickmioduszewski.github.io/dqcheckr/articles/specification.html)
for the full config/schema reference.

These packages are only called from the 'shiny' app sourced at runtime
via `system.file("app", package = "dqcheckrGUI")` (`inst/app/R/*.R`), so
static analysis of `R/` cannot see them as used – without a reference
here, `R CMD check` reports "Namespaces in Imports field not imported
from".

## See also

Useful links:

- <https://mickmioduszewski.github.io/dqcheckrGUI/>

- <https://github.com/mickmioduszewski/dqcheckrGUI>

- Report bugs at
  <https://github.com/mickmioduszewski/dqcheckrGUI/issues>

## Author

**Maintainer**: Mick Mioduszewski <mick@mioduszewski.net>

Authors:

- Mick Mioduszewski <mick@mioduszewski.net>
