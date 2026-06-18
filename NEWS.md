# dqcheckrGUI 0.1.0

* Initial release: a point-and-click 'shiny' front-end for 'dqcheckr' that
  drives automated dataset quality checks without writing any R code.
* Configuration wizard for setting up checks on a new dataset (identity,
  files, structure, columns, rules, overrides, custom checks, review/save),
  a run panel for launching checks against incoming file deliveries and
  viewing the generated HTML report, and a history browser for past results
  and drift comparisons between snapshots.
* CSV onboarding handles header rows with duplicate or otherwise invalid
  column names (e.g. an extract that repeats `PayeeName`/`Amount`): a
  raw-header probe (`name_repair = "minimal"`) recovers the names and
  `suggest_col_names()` proposes valid, unique, editable fixes. On save a
  renamed header is written as `col_names` plus `csv_skip: 1`, so
  'dqcheckr' (>= 0.2.2) skips the original header row instead of reading it
  as data; clean files write neither key.
* Relative `snapshot_db` / `report_output_dir` paths in the config are
  resolved against the deployment root (the parent of the config directory),
  and background `callr` runs execute with `wd` set there, so the GUI reads
  and writes the same snapshot database the 'dqcheckr' CLI does. Absolute
  paths continue to work unchanged.
