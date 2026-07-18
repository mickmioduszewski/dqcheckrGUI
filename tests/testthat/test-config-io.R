# Unit tests for R/config_io.R
# These catch YAML round-trip bugs without needing a running Shiny app.

# ── read_config: field reading ───────────────────────────────────────────────

test_that("read_config reads all standard fields", {
  path <- write_yaml_fixture(list(
    dataset_name     = "bonds",
    description      = "My dataset",
    format           = "csv",
    encoding         = "UTF-8",
    delimiter        = ",",
    current_file     = "/data/file.csv",
    expected_columns = list("id", "name", "value"),
    key_columns      = list("id"),
    column_types     = list(value = "numeric"),
    column_rules     = list(value = list(min_value = 0, max_value = 999)),
    rule_overrides   = list(max_missing_rate = 0.10),
    custom_checks_file = "/checks/my_checks.R"
  ))

  k <- read_config(path)$known

  expect_equal(k$dataset_name,       "bonds")
  expect_equal(k$description,        "My dataset")
  expect_equal(k$format,             "csv")
  expect_equal(k$encoding,           "UTF-8")
  expect_equal(k$delimiter,          ",")
  expect_equal(k$current_file,       "/data/file.csv")
  expect_equal(k$expected_columns,   c("id", "name", "value"))
  expect_equal(k$key_columns,        c("id"))
  expect_equal(k$col_types_override, list(value = "numeric"))
  expect_equal(k$column_rules$value$min_value, 0)
  expect_equal(k$column_rules$value$max_value, 999)
  expect_equal(k$rule_overrides$max_missing_rate, 0.10)
  expect_equal(k$custom_checks_file, "/checks/my_checks.R")
})

test_that("read_config provides sensible defaults for missing fields", {
  path <- write_yaml_fixture(list(dataset_name = "minimal"))
  k <- read_config(path)$known

  expect_equal(k$format,    "csv")
  expect_equal(k$encoding,  "UTF-8")
  expect_equal(k$delimiter, ",")
  expect_equal(k$folder,    "")
  expect_equal(k$current_file, "")
  expect_equal(k$key_columns, character(0))
  expect_equal(k$expected_columns, character(0))
  expect_equal(k$col_types_override, list())
  expect_equal(k$column_rules, list())
  expect_equal(k$rule_overrides, list())
})

# ── read_config: file_mode derivation ────────────────────────────────────────

test_that("read_config derives file_mode=explicit when only current_file set", {
  path <- write_yaml_fixture(list(dataset_name = "ds", current_file = "/data/f.csv"))
  expect_equal(read_config(path)$known$file_mode, "explicit")
})

test_that("read_config derives file_mode=folder when folder is set", {
  path <- write_yaml_fixture(list(dataset_name = "ds", folder = "/data/folder"))
  expect_equal(read_config(path)$known$file_mode, "folder")
})

test_that("read_config derives file_mode=explicit when folder is empty string", {
  path <- write_yaml_fixture(list(dataset_name = "ds", folder = "", current_file = "/f.csv"))
  expect_equal(read_config(path)$known$file_mode, "explicit")
})

# ── read_config: unknown key preservation ────────────────────────────────────

test_that("read_config preserves unknown keys in $extra", {
  path <- write_yaml_fixture(list(
    dataset_name   = "ds",
    org_flag       = TRUE,
    project_id     = "XYZ-42",
    internal_notes = "do not remove"
  ))
  result <- read_config(path)

  expect_true("org_flag"       %in% names(result$extra))
  expect_true("project_id"     %in% names(result$extra))
  expect_true("internal_notes" %in% names(result$extra))
  expect_equal(result$extra$project_id, "XYZ-42")
})

test_that("read_config does not put unknown keys in $known", {
  path <- write_yaml_fixture(list(dataset_name = "ds", my_custom = "value"))
  expect_false("my_custom" %in% names(read_config(path)$known))
})

test_that("read_config returns empty extra when all keys are known", {
  path <- write_yaml_fixture(list(dataset_name = "ds", format = "csv"))
  expect_equal(length(read_config(path)$extra), 0L)
})

# ── build_config_list: file mode branching ───────────────────────────────────
# These tests encode the bug where wiz$file_mode wasn't synced from the radio
# button, causing current_file to be silently dropped from the YAML.

test_that("build_config_list writes current_file (not folder) in explicit mode", {
  wiz <- make_wiz(file_mode = "explicit", current_file = "/data/delivery.csv")
  cfg <- build_config_list(wiz)

  expect_equal(cfg$current_file, "/data/delivery.csv")
  expect_null(cfg$folder)  # folder must NOT appear
})

test_that("build_config_list writes folder (not current_file) in folder mode", {
  wiz <- make_wiz(file_mode = "folder", folder = "/data/monthly/")
  cfg <- build_config_list(wiz)

  expect_equal(cfg$folder, "/data/monthly/")
  expect_null(cfg$current_file)  # current_file must NOT appear
})

test_that("build_config_list includes previous_file when set in explicit mode", {
  wiz <- make_wiz(file_mode = "explicit",
                  current_file = "/data/curr.csv",
                  previous_file = "/data/prev.csv")
  cfg <- build_config_list(wiz)

  expect_equal(cfg$previous_file, "/data/prev.csv")
})

test_that("build_config_list omits previous_file when empty", {
  wiz <- make_wiz(file_mode = "explicit", current_file = "/data/curr.csv", previous_file = "")
  cfg <- build_config_list(wiz)

  expect_null(cfg$previous_file)
})

# ── build_config_list: description ───────────────────────────────────────────

test_that("build_config_list omits description when empty", {
  expect_null(build_config_list(make_wiz(description = ""))$description)
})

test_that("build_config_list includes description when set", {
  expect_equal(build_config_list(make_wiz(description = "My bonds data"))$description,
               "My bonds data")
})

# ── build_config_list: column metadata ───────────────────────────────────────

test_that("build_config_list includes expected_columns and key_columns", {
  wiz <- make_wiz(
    expected_columns = c("id", "name", "amount"),
    key_columns      = c("id")
  )
  cfg <- build_config_list(wiz)

  expect_equal(unlist(cfg$expected_columns), c("id", "name", "amount"))
  expect_equal(unlist(cfg$key_columns),      c("id"))
})

test_that("build_config_list omits expected_columns and key_columns when empty", {
  wiz <- make_wiz(expected_columns = character(0), key_columns = character(0))
  cfg <- build_config_list(wiz)

  expect_null(cfg$expected_columns)
  expect_null(cfg$key_columns)
})

test_that("build_config_list writes column_types overrides", {
  wiz <- make_wiz(col_types_override = list(code = "character", amount = "numeric"))
  cfg <- build_config_list(wiz)

  expect_equal(cfg$column_types$code,   "character")
  expect_equal(cfg$column_types$amount, "numeric")
})

test_that("build_config_list omits column_types when empty", {
  expect_null(build_config_list(make_wiz(col_types_override = list()))$column_types)
})

test_that("build_config_list strips column_rules entries that are all empty", {
  wiz <- make_wiz(column_rules = list(
    id    = list(min_value = NA, max_value = NULL, pattern = ""),
    value = list(min_value = 0, max_value = 1000)
  ))
  cfg <- build_config_list(wiz)

  expect_null(cfg$column_rules$id)
  expect_equal(cfg$column_rules$value$min_value, 0)
})

test_that("build_config_list omits column_rules entirely when all empty", {
  wiz <- make_wiz(column_rules = list(id = list(min_value = NA, pattern = "")))
  expect_null(build_config_list(wiz)$column_rules)
})

# ── write_config: full round-trip ────────────────────────────────────────────

test_that("write_config round-trip preserves all known fields", {
  wiz <- make_wiz(
    dataset_name     = "round_trip",
    description      = "Test dataset",
    file_mode        = "explicit",
    current_file     = "/data/file.csv",
    expected_columns = c("a", "b", "c"),
    key_columns      = c("a"),
    col_types_override = list(b = "numeric"),
    column_rules     = list(b = list(min_value = 0, max_value = 999)),
    rule_overrides   = list(max_missing_rate = 0.10)
  )
  path <- tempfile(fileext = ".yml")
  write_config(wiz, list(), path)

  k <- read_config(path)$known
  expect_equal(k$dataset_name,   "round_trip")
  expect_equal(k$description,    "Test dataset")
  expect_equal(k$current_file,   "/data/file.csv")
  expect_equal(k$file_mode,      "explicit")
  expect_equal(k$expected_columns, c("a", "b", "c"))
  expect_equal(k$key_columns,      c("a"))
  expect_equal(k$col_types_override, list(b = "numeric"))
  expect_equal(k$column_rules$b$min_value, 0)
  expect_equal(k$rule_overrides$max_missing_rate, 0.10)
})

test_that("write_config preserves unknown keys through round-trip", {
  original <- list(
    dataset_name   = "ds",
    current_file   = "/data/f.csv",
    org_flag       = TRUE,
    project_id     = "XYZ"
  )
  path1 <- write_yaml_fixture(original)

  result <- read_config(path1)
  wiz    <- make_wiz(dataset_name = result$known$dataset_name,
                     file_mode    = result$known$file_mode,
                     current_file = result$known$current_file)

  path2 <- tempfile(fileext = ".yml")
  write_config(wiz, result$extra, path2)

  raw <- yaml::read_yaml(path2)
  expect_equal(raw$org_flag,   TRUE)
  expect_equal(raw$project_id, "XYZ")
  expect_equal(raw$current_file, "/data/f.csv")
})

test_that("write_config does not duplicate unknown keys as known fields", {
  original <- list(dataset_name = "ds", current_file = "/f.csv", extra_key = "v")
  path1    <- write_yaml_fixture(original)
  result   <- read_config(path1)

  wiz   <- make_wiz(file_mode = "explicit", current_file = "/f.csv")
  path2 <- tempfile(fileext = ".yml")
  write_config(wiz, result$extra, path2)

  raw <- yaml::read_yaml(path2)
  # extra_key should appear exactly once
  expect_equal(sum(names(raw) == "extra_key"), 1L)
})

# ── csv_skip / has_header inference (duplicate-header support, B-08) ───────────

test_that("read_config: col_names + csv_skip>=1 means the file HAS a header", {
  path <- write_yaml_fixture(list(
    dataset_name = "refunds",
    current_file = "/data/refunds.csv",
    col_names    = list("PayeeName", "PayeeName_2", "Amount"),
    csv_skip     = 1L
  ))
  k <- read_config(path)$known
  expect_true(k$has_header)                       # NOT headerless
  expect_equal(k$csv_skip, 1L)
  expect_equal(k$col_names, c("PayeeName", "PayeeName_2", "Amount"))
})

test_that("read_config: col_names without csv_skip means genuinely headerless", {
  path <- write_yaml_fixture(list(
    dataset_name = "nohdr",
    current_file = "/data/nohdr.csv",
    col_names    = list("a", "b", "c")
  ))
  k <- read_config(path)$known
  expect_false(k$has_header)
  expect_equal(k$csv_skip, 0L)
})

test_that("read_config: no col_names means clean header (defaults)", {
  path <- write_yaml_fixture(list(dataset_name = "clean", current_file = "/f.csv"))
  k <- read_config(path)$known
  expect_true(k$has_header)
  expect_equal(k$csv_skip, 0L)
  expect_equal(k$col_names, character(0))
})

test_that("build_config_list: renamed header writes col_names + csv_skip=1", {
  wiz <- make_wiz(
    file_mode        = "explicit", current_file = "/data/refunds.csv",
    has_header       = TRUE,
    raw_header_names = c("PayeeName", "PayeeName", "Amount"),   # file's real header
    col_names        = c("PayeeName", "PayeeName_2", "Amount")  # user/suggested fix
  )
  cfg <- build_config_list(wiz)
  expect_equal(unlist(cfg$col_names), c("PayeeName", "PayeeName_2", "Amount"))
  expect_equal(cfg$csv_skip, 1L)
})

test_that("build_config_list: clean header writes neither col_names nor csv_skip", {
  wiz <- make_wiz(
    file_mode        = "explicit", current_file = "/data/clean.csv",
    has_header       = TRUE,
    raw_header_names = c("id", "name", "amount"),
    col_names        = c("id", "name", "amount")               # unchanged
  )
  cfg <- build_config_list(wiz)
  expect_null(cfg$col_names)
  expect_null(cfg$csv_skip)
})

test_that("build_config_list: headerless writes col_names but no csv_skip", {
  wiz <- make_wiz(
    file_mode  = "explicit", current_file = "/data/nohdr.csv",
    has_header = FALSE,
    col_names  = c("a", "b", "c")
  )
  cfg <- build_config_list(wiz)
  expect_equal(unlist(cfg$col_names), c("a", "b", "c"))
  expect_null(cfg$csv_skip)
})

test_that("renamed-header config round-trips through write -> read -> rebuild (Gap 1)", {
  # The cycle that a single-save test hides: a renamed-header config must come
  # back as has_header = TRUE and re-serialise identically on the SECOND save.
  wiz1 <- make_wiz(
    dataset_name     = "refunds",
    file_mode        = "explicit", current_file = "/data/refunds.csv",
    has_header       = TRUE,
    raw_header_names = c("PayeeName", "PayeeName", "Amount"),
    col_names        = c("PayeeName", "PayeeName_2", "Amount")
  )
  path <- tempfile(fileext = ".yml")
  write_config(wiz1, list(), path)
  cfg1 <- yaml::read_yaml(path)
  expect_equal(cfg1$csv_skip, 1L)

  # Reopen (as the edit wizard would) — header presence must be restored.
  k <- read_config(path)$known
  expect_true(k$has_header)
  expect_equal(k$col_names, c("PayeeName", "PayeeName_2", "Amount"))

  # Reseed a wiz from the loaded config and re-serialise. On reopen the raw
  # header probe would repopulate raw_header_names from the file; simulate that.
  wiz2 <- make_wiz(
    dataset_name     = "refunds",
    file_mode        = "explicit", current_file = "/data/refunds.csv",
    has_header       = k$has_header,
    raw_header_names = c("PayeeName", "PayeeName", "Amount"),
    col_names        = k$col_names
  )
  cfg2 <- build_config_list(wiz2)
  expect_equal(unlist(cfg2$col_names), c("PayeeName", "PayeeName_2", "Amount"))
  expect_equal(cfg2$csv_skip, 1L)   # NOT dropped on the second save
})

test_that("renamed-header keys survive an edit that never re-probes step 3 (B-06)", {
  # open_wizard() resets raw_header_names to empty on edit. If the user saves
  # without visiting step 3 again, build_config_list() cannot recompute the
  # rename — but it must preserve the loaded col_names + csv_skip rather than
  # silently reverting the file to its unwanted original header.
  k <- read_config(local({
    p <- tempfile(fileext = ".yml")
    yaml::write_yaml(list(
      dataset_name = "refunds", format = "csv", encoding = "UTF-8",
      delimiter = ",", current_file = "/data/refunds.csv",
      col_names = list("PayeeName", "PayeeName_2", "Amount"), csv_skip = 1L), p)
    p
  }))$known
  expect_true(k$has_header)

  wiz <- make_wiz(
    dataset_name     = "refunds",
    file_mode        = "explicit", current_file = "/data/refunds.csv",
    has_header       = k$has_header,
    raw_header_names = character(0),   # never re-probed this edit session
    col_names        = k$col_names,
    csv_skip         = k$csv_skip
  )
  cfg <- build_config_list(wiz)
  expect_equal(unlist(cfg$col_names), c("PayeeName", "PayeeName_2", "Amount"))
  expect_equal(cfg$csv_skip, 1L)
})

test_that("clean-header edit without re-probe stays clean (B-06 no false positive)", {
  # A clean-header config loads with col_names empty + csv_skip 0; a no-re-probe
  # save must still emit neither key (the carry-forward only fires for a real
  # renamed-header config).
  wiz <- make_wiz(
    file_mode        = "explicit", current_file = "/data/clean.csv",
    has_header       = TRUE,
    raw_header_names = character(0),
    col_names        = character(0),
    csv_skip         = 0L
  )
  cfg <- build_config_list(wiz)
  expect_null(cfg$col_names)
  expect_null(cfg$csv_skip)
})

# ── update_global_config: unknown-key preservation (G-02) ────────────────────

test_that("update_global_config preserves unknown top-level and rule keys", {
  path <- tempfile(fileext = ".yml")
  withr::defer(unlink(path))
  yaml::write_yaml(list(
    snapshot_db       = "data/old.sqlite",
    report_output_dir = "reports/",
    org_contact       = "data-team@example.org",     # unknown top-level key
    default_rules = list(
      max_missing_rate     = 0.05,
      max_file_size_mb     = 500,                     # no GUI widget
      iqr_fence_multiplier = 1.5                      # no GUI widget
    )
  ), path)

  merged <- update_global_config(list(
    snapshot_db       = "data/new.sqlite",
    report_output_dir = "reports/",
    default_rules     = list(max_missing_rate = 0.10)
  ), path)

  reread <- yaml::read_yaml(path)
  for (cfg in list(merged, reread)) {
    expect_equal(cfg$snapshot_db, "data/new.sqlite")            # edited key replaced
    expect_equal(cfg$org_contact, "data-team@example.org")      # unknown key kept
    expect_equal(cfg$default_rules$max_missing_rate, 0.10)      # edited rule replaced
    expect_equal(cfg$default_rules$max_file_size_mb, 500)       # unknown rule kept
    expect_equal(cfg$default_rules$iqr_fence_multiplier, 1.5)   # unknown rule kept
  }
})

test_that("update_global_config creates the file when none exists", {
  path <- tempfile(fileext = ".yml")
  withr::defer(unlink(path))
  merged <- update_global_config(list(
    snapshot_db   = "data/snapshots.sqlite",
    default_rules = list(max_missing_rate = 0.05)
  ), path)
  expect_true(file.exists(path))
  expect_equal(merged$snapshot_db, "data/snapshots.sqlite")
  expect_equal(yaml::read_yaml(path)$default_rules$max_missing_rate, 0.05)
})

test_that("update_global_config returns exactly what is on disk", {
  path <- tempfile(fileext = ".yml")
  withr::defer(unlink(path))
  yaml::write_yaml(list(snapshot_db = "a", keep_me = TRUE), path)
  merged <- update_global_config(list(snapshot_db = "b"), path)
  expect_equal(merged, yaml::read_yaml(path))
})

test_that("update_global_config aborts and preserves the file on a parse error (B-05)", {
  # An existing global config that fails to parse (hand-edited, or a concurrent
  # write on a network share) must NOT be silently replaced with only the
  # just-submitted keys -- that wipes every no-widget key while reporting success.
  path <- tempfile(fileext = ".yml")
  withr::defer(unlink(path))
  writeLines(c("default_rules:",
               "  max_missing_rate: 0.05",
               "  max_file_size_mb: 500",
               "org_contact: [unterminated flow"),   # invalid YAML
             path)
  before <- readLines(path)

  # Aborts (so the caller shows "Save failed") instead of silently overwriting.
  expect_error(
    update_global_config(list(snapshot_db = "data/new.sqlite"), path),
    regexp = "could not be read")
  # The on-disk file is untouched -- no data was lost.
  expect_identical(readLines(path), before)
})

# ── write_yaml_atomic (B-07/B-08: crash-safe config writes) ───────────────────

test_that("write_yaml_atomic round-trips and leaves no temp file (B-07/B-08)", {
  dir  <- withr::local_tempdir()
  path <- file.path(dir, "cfg.yml")
  write_yaml_atomic(list(a = 1, b = list(c = 2)), path)
  expect_equal(yaml::read_yaml(path), list(a = 1, b = list(c = 2)))
  expect_equal(list.files(dir, pattern = "^dqcfg_"), character(0))   # temp cleaned up
})

test_that("write_yaml_atomic overwrites an existing file with new content (B-07/B-08)", {
  dir  <- withr::local_tempdir()
  path <- file.path(dir, "cfg.yml")
  yaml::write_yaml(list(old = TRUE), path)
  write_yaml_atomic(list(new = TRUE), path)
  expect_equal(yaml::read_yaml(path), list(new = TRUE))
})

# ── read_global_config resilience (B-09: no crash on a malformed config) ──────

test_that("read_global_config returns defaults for a missing file", {
  cfg <- read_global_config(tempfile(fileext = ".yml"))
  expect_equal(cfg$snapshot_db, "data/snapshots.sqlite")
  expect_equal(cfg$default_rules$max_missing_rate, 0.05)
})

test_that("read_global_config falls back to defaults with a warning on a parse error (B-09)", {
  path <- withr::local_tempfile(fileext = ".yml")
  writeLines("default_rules: [unterminated", path)      # invalid YAML
  expect_warning(cfg <- read_global_config(path), regexp = "could not be parsed")
  expect_equal(cfg$snapshot_db, "data/snapshots.sqlite")   # defaults, not a crash
  expect_true(is.list(cfg$default_rules))
})
