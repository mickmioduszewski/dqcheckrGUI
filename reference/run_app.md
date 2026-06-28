# Launch the dqcheckrGUI Shiny application

Opens the point-and-click interface for configuring and running dqcheckr
dataset quality checks.

## Usage

``` r
run_app(config_dir = NULL, ...)
```

## Arguments

- config_dir:

  Path to the directory that contains (or will contain) `dqcheckr.yml`
  and individual dataset `.yml` files. Defaults to the
  `DQCHECKR_CONFIG_DIR` environment variable when set, otherwise
  `config/` inside the current working directory. Passing this argument
  explicitly is recommended when launching from a script so that the
  correct project folder is used regardless of the R session's working
  directory.

- ...:

  Arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g. `port`,
  `launch.browser`).

## Value

Called for its side effect; does not return a value.

## See also

[`vignette("dqcheckrGUI", package = "dqcheckrGUI")`](https://mickmioduszewski.github.io/dqcheckrGUI/articles/dqcheckrGUI.md)
for a full setup walkthrough.
[`run_dq_check`](https://rdrr.io/pkg/dqcheckr/man/run_dq_check.html) for
the underlying check function.

## Examples

``` r
if (interactive()) {
  run_app()

  # Explicit project folder (recommended from a launcher script)
  # run_app(config_dir = file.path(getwd(), "config"))
}
```
