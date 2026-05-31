# Unit tests for R/config_io.R
# These catch YAML round-trip bugs without needing a running Shiny app.

library(testthat)
library(yaml)

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
