# dqcheckrGUI

A point-and-click Shiny interface for
[dqcheckr](https://github.com/mickmioduszewski/dqcheckr) — automated data
quality checks for recurring dataset deliveries.

Use this if you want to configure and run quality checks without writing any R
code. If you prefer scripting, use `dqcheckr` directly.

## What it does

- Walks you through setting up quality checks for a new dataset (wizard)
- Runs checks against incoming file deliveries and opens the HTML report
- Browses historical check results and drift comparisons

## Installation

```r
# Install dqcheckr first (required)
install.packages("dqcheckr")

# Install dqcheckrGUI from GitHub
devtools::install_github("mickmioduszewski/dqcheckrGUI")
```

## Launch

```r
dqcheckrGUI::run_app()
```

Or, if you have downloaded the source, double-click `launch.command` (Mac),
`launch.sh` (Linux), or `launch.bat` (Windows).

## Example configuration files

Example YAML configuration files using the Star Wars dataset are included with
the package. Copy them to a local folder to use as a starting point:

```r
file.copy(
  system.file("extdata/example_config", package = "dqcheckrGUI"),
  "my_config",
  recursive = TRUE
)
```

## Learn more

See the [dqcheckr documentation](https://mickmioduszewski.github.io/dqcheckr/)
for a full description of all configuration options and quality checks.
