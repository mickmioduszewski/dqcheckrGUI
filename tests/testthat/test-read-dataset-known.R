# Regression (B-06): a corrupt per-dataset config must be logged, not swallowed.
# All three report-prefix sites (server_history, server_run, register_all_report_paths)
# read the config via read_dataset_known; on a parse error it returns NULL AND
# emits a message, so a dead report link is at least traceable rather than silent.

test_that("read_dataset_known returns known keys for a valid config", {
  dir <- make_test_config_dir()
  yaml::write_yaml(list(dataset_name = "ds1", format = "csv",
                        report_output_dir = "custom_reports/"),
                   file.path(dir, "ds1.yml"))
  known <- read_dataset_known(dir, "ds1")
  expect_equal(known$report_output_dir, "custom_reports/")
})

test_that("read_dataset_known logs and returns NULL on a corrupt config", {
  dir <- make_test_config_dir()
  # Invalid YAML (unterminated flow mapping) -> read_config errors.
  writeLines("dataset_name: {ds1", file.path(dir, "bad.yml"))
  expect_message(
    known <- read_dataset_known(dir, "bad"),
    "could not be parsed"
  )
  expect_null(known)
})
