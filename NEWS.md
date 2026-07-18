# dqcheckrGUI (development version)

## Bug fixes

* The drift-comparison report link now works again. The GUI located the rendered
  drift report by reconstructing its filename with a fixed pattern, which stopped
  matching once dqcheckr's drift filenames gained a snapshot-id suffix — so every
  drift comparison reported "report not found" even though it rendered. The GUI
  now uses the report path `compare_snapshots()` returns directly (requires
  dqcheckr >= 0.2.5 behaviour), removing the fragile filename contract.

* Report links in the dataset panel now account for a report that is still
  rendering. A snapshot whose `render_status` is `"pending"` (dqcheckr 0.2.5+)
  no longer gets a clickable "Open" link to a file that is not yet on disk; it
  shows "rendering…" until the report is written. A filename is only
  reconstructed for a genuine pre-0.2.3 row, not whenever one is absent.

* The `dqcheckr` dependency floor was raised to the version whose behaviour the
  report and drift link builders rely on.

# dqcheckrGUI 0.2.2

## Bug fixes

* The step-3 encoding sniffer no longer writes `encoding: ASCII` into dataset
  configs. `readr::guess_encoding()`'s head sample confidently reports
  "ASCII" whenever the first accented byte sits beyond the sample — and that
  config value later hard-crashed the run's R subprocess ("Invalid multibyte
  sequence" inside vroom/iconv) once a delivery contained non-ASCII bytes.
  Step 3 now scans the *entire* file: valid UTF-8 (which includes pure ASCII)
  is applied as `UTF-8` with certainty; only when the file is genuinely not
  UTF-8 does the statistical detector run — seeded with a chunk that actually
  contains the non-ASCII bytes — and its legacy-encoding candidates go
  through the usual confirm flow. New dependency: `stringi`.
* An `encoding: ASCII` in an existing config (written by hand or by an older
  GUI) is normalised to `UTF-8` when the config is read, so editing and
  re-saving a dataset upgrades it. ASCII is a strict subset of UTF-8, so the
  change is lossless.

# dqcheckrGUI 0.2.1

## Bug fixes

* Wizard state can no longer leak between datasets: opening the wizard now
  always resets to defaults before loading a config (edit mode previously
  overlaid the config on the previous session's leftovers), and positional
  step-3/4/5 inputs are namespaced per wizard open so stale checkbox/select
  values from an earlier dataset can never be collected into the current one.
* The step-5 regex "Test" buttons are handled by a single delegated observer;
  previously a new set of observers was registered on every column-list
  change without destroying the old ones.
* Report links now honour per-dataset `report_output_dir` overrides (each
  override directory is served under its own resource path), the resource
  paths are re-registered after every completed run (covering projects whose
  `reports/` directory is first created by dqcheckr itself), runs whose
  report rendering failed show "render failed" instead of a dead "Open"
  link, and links prefer the exact filename recorded by dqcheckr >= 0.2.3
  over reconstructing it from the run timestamp.
* Renaming a dataset in edit mode now removes the old config file and warns
  that run history stays under the old name; renaming onto another existing
  dataset is blocked (previously it silently overwrote that dataset's
  config).
* The pre-run check validates a per-dataset `snapshot_db` override rather
  than always checking the global path.
* Unparseable run timestamps display verbatim instead of as "NA"; an empty
  preview file no longer produces a `max()` warning.
* The wizard's type-inference preview honours `type_inference_threshold`
  (dataset override, then global default) instead of a hard-coded 0.90.

* Sidebar status badges and the dataset panel's "Recent runs" table now
  resolve a relative `snapshot_db` path (e.g. the `data/snapshots.sqlite`
  written by the first-run scaffold) against the project root instead of the
  installed app directory, where it never existed. Both views also now honour
  a per-dataset `snapshot_db` override, matching the History panel.
* Values interpolated into raw HTML in the History and dataset-panel tables
  (delivered file names, dataset names, report URLs) are now HTML-escaped.
  File names come from externally supplied deliveries, so a hostile file name
  could previously inject markup into the analyst's browser.
* Saving the Global Config no longer discards hand-added keys: unknown
  top-level keys and `default_rules` entries with no GUI widget (e.g.
  `max_row_count`, `max_file_size_mb`, `iqr_fence_multiplier`) are preserved,
  matching the round-trip behaviour dataset configs already had.
* Stepping through wizard step 6 without changing anything no longer writes a
  spurious `min_row_count` override into the dataset config (an
  integer-vs-double comparison artefact).
* Folder-scan previews in the wizard no longer consider subdirectories when
  picking the most recent file, and the folder file count excludes them.

## Performance

* The sidebar reads the latest status for all datasets with one query per
  distinct snapshot database instead of one connection + query per dataset
  per redraw.
* The run-log poll re-reads the log file only when its size has changed
  (previously the whole file was re-read every 200 ms for the duration of
  the run).

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
