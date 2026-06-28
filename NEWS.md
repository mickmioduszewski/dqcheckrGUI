# dqcheckrGUI 0.2.0

## New features

* `run_app()` gains a `config_dir` argument to point the app at a specific
  project folder regardless of the R session's working directory. Passing
  this explicitly is recommended when launching from a script on 'Windows' or
  from a 'OneDrive'-synced path, where `getwd()` may not match the script
  location. The argument sets `DQCHECKR_CONFIG_DIR` for the session and
  restores the previous value on exit.
* First-run modal: when no `dqcheckr.yml` is found, the app now shows a
  dialog offering to scaffold `config/`, `data/`, and `reports/` and write a
  default global config. Accepting opens Global Config for review before any
  dataset is added. Declining allows manual setup. This replaces the previous
  silent redirect to Global Config.
* File and folder pickers now root at the project folder (`deployment_root`)
  as their first entry, followed by the user home directory and system
  volumes, so the correct location is reachable without navigating from an
  unrelated working directory.
* Pre-run check in the Run panel now surfaces malformed-'YAML' errors
  explicitly instead of silently showing "✓ Configuration looks good" for a
  corrupt config file.
* Custom-check file validation (wizard step 7) now distinguishes `source()`
  runtime errors (e.g. missing package, top-level `stop()`) from a missing
  `custom_checks` function, and shows the specific error message.
* 'pkgdown' documentation site added at
  `https://mickmioduszewski.github.io/dqcheckrGUI/`, deployed automatically
  via 'GitHub Actions' on every push to `main`.

## Bug fixes

* Relative `folder` and `current_file` paths in the pre-run check are now
  resolved against the deployment root before the existence check, so they no
  longer show a false "Folder not found" warning when the path is valid
  relative to the project.
* Init modal wrote eight wrong key names and scales into `dqcheckr.yml`
  (e.g. `max_mean_shift_pct = 20` instead of
  `max_numeric_mean_shift_pct = 0.20`). The written config now matches the
  canonical schema used by `read_global_config()` and Global Config.
* Dead `uiOutput("step8_save_status")` removed from wizard step 8; save
  feedback is provided by notification and modal, not this slot.

## Documentation

* Vignette updated with `config_dir` argument usage, `DQCHECKR_CONFIG_DIR`
  env var option, 'Windows'/'OneDrive' launcher pattern, and first-run modal
  description.
* `?dqcheckrGUI` package help page now resolves (dropped `@noRd`); includes
  Getting started and Related packages sections with cross-references to
  'dqcheckr' vignettes.
* `?run_app` gains `@seealso` links to the vignette and
  `dqcheckr::run_dq_check`.

## Testing

* `skip_on_cran()` added to all 'shinytest2' test blocks; the suite requires
  a browser and runs ~108 s, which exceeds CRAN's time limit.
* Removed unnecessary `library()` calls from test files; all packages are
  accessed via `::` (exception: `library(shiny)` retained in
  `test-history.R` to attach `tags` to the search path for `status_badge()`).

# dqcheckrGUI 0.1.0

* Initial release: a point-and-click 'shiny' front-end for 'dqcheckr' that
  drives automated dataset quality checks without writing any R code.
* Configuration wizard for setting up checks on a new dataset (identity,
  files, structure, columns, rules, overrides, custom checks, review/save),
  a run panel for launching checks against incoming file deliveries and
  viewing the generated HTML report, and a history browser for past results
  and drift comparisons between snapshots.
* 'CSV' onboarding handles header rows with duplicate or otherwise invalid
  column names (e.g. an extract that repeats `PayeeName`/`Amount`): a
  raw-header probe (`name_repair = "minimal"`) recovers the names and
  `suggest_col_names()` proposes valid, unique, editable fixes. On save a
  renamed header is written as `col_names` plus `csv_skip: 1`, so
  'dqcheckr' (>= 0.2.2) skips the original header row instead of reading it
  as data; clean files write neither key.
* Relative `snapshot_db` / `report_output_dir` paths in the config are
  resolved against the deployment root (the parent of the config directory),
  and background 'callr' runs execute with `wd` set there, so the GUI reads
  and writes the same snapshot database the 'dqcheckr' CLI does. Absolute
  paths continue to work unchanged.
