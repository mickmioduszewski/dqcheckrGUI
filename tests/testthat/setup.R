# Shared test setup — sourced automatically by testthat before all tests in this dir

library(yaml)
library(withr)

# Source app R files so unit tests can call them directly
.app_r_dir <- system.file("app/R", package = "dqcheckrGUI")
for (.f in list.files(.app_r_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = TRUE)
}

# Path to the test fixture CSV (absolute, so configs can reference it)
fixture_csv <- normalizePath(
  file.path(test_path(), "../fixtures/sample.csv"),
  mustWork = TRUE
)

# Build a minimal wiz-like list for build_config_list tests
make_wiz <- function(...) {
  base <- list(
    dataset_name = "test_ds", description = "",
    format = "csv", encoding = "UTF-8", delimiter = ",", quote_char = '"',
    has_header = TRUE, csv_skip = 0L, col_names = character(0),
    raw_header_names = character(0), col_name_reasons = character(0),
    file_mode = "folder", folder = "", current_file = "",
    previous_file = "", fwf_widths = integer(0), fwf_col_names = character(0),
    fwf_skip = 0L, expected_columns = character(0), key_columns = character(0),
    col_types_override = list(), column_rules = list(), rule_overrides = list(),
    custom_checks_file = "", snapshot_db = "", report_output_dir = ""
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

# Write a YAML fixture to a temp file, return the path
write_yaml_fixture <- function(cfg) {
  path <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg, path)
  path
}

# Minimal global config suitable for tests (paths don't need to exist for wizard tests)
minimal_global_config <- function(report_dir = tempdir()) {
  list(
    snapshot_db       = file.path(tempdir(), "test_snapshots.sqlite"),
    report_output_dir = report_dir,
    default_rules = list(
      type_inference_threshold       = 0.90,
      max_missing_rate               = 0.05,
      max_non_numeric_rate           = 0.01,
      min_row_count                  = 0L,
      max_row_count_change_pct       = 0.10,
      max_numeric_mean_shift_pct     = 0.20,
      max_missing_rate_change_pp     = 2.0,
      max_non_numeric_rate_change_pp = 1.0,
      flag_new_columns               = TRUE,
      flag_dropped_columns           = TRUE,
      flag_type_changes              = TRUE,
      flag_column_order_change       = TRUE
    )
  )
}

# Create a temporary config directory pre-populated with a global config.
# Returns the path; registers cleanup in the calling test's environment.
make_test_config_dir <- function(envir = parent.frame()) {
  dir <- tempfile(pattern = "dqtest_")
  dir.create(dir, recursive = TRUE)
  withr::defer(unlink(dir, recursive = TRUE), envir = envir)
  yaml::write_yaml(minimal_global_config(), file.path(dir, "dqcheckr.yml"))
  dir
}

# Helper: create AppDriver pointed at the GUI app with a given config dir.
# Stops the driver when the calling test finishes.
make_app_driver <- function(config_dir, envir = parent.frame(), ...) {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  old <- Sys.getenv("DQCHECKR_CONFIG_DIR", unset = NA_character_)
  Sys.setenv(DQCHECKR_CONFIG_DIR = config_dir)
  withr::defer(
    {
      if (is.na(old)) Sys.unsetenv("DQCHECKR_CONFIG_DIR")
      else            Sys.setenv(DQCHECKR_CONFIG_DIR = old)
    },
    envir = envir
  )

  app_dir <- system.file("app", package = "dqcheckrGUI")
  app <- shinytest2::AppDriver$new(app_dir, load_timeout = 30000, ...)
  withr::defer(try(app$stop(), silent = TRUE), envir = envir)
  app
}

# Wait for the wizard Next button to become enabled (step valid).
wait_for_next_enabled <- function(app, timeout_ms = 12000) {
  app$wait_for_js(
    "document.getElementById('wizard_next') !== null &&
     !document.getElementById('wizard_next').hasAttribute('disabled')",
    timeout = timeout_ms
  )
}

# Navigate the wizard from from_step to to_step.
# Waits for Next to be enabled before each click so timing issues don't stall navigation.
open_new_wizard <- function(app) {
  app$click("btn_new_dataset")
  app$wait_for_idle()
}

wizard_go_to_step <- function(app, to_step, from_step = 1L) {
  clicks <- to_step - from_step
  if (clicks <= 0L) return(invisible(NULL))
  for (i in seq_len(clicks)) {
    wait_for_next_enabled(app)
    app$click("wizard_next")
    app$wait_for_idle(timeout = 8000)
  }
  invisible(NULL)
}
