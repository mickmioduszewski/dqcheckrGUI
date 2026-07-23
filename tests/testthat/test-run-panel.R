# Integration tests: run panel (launch → poll → completion) — previously
# uncovered (B-19). These drive a real dqcheckr::run_dq_check() in a callr
# background process, so they exercise the 200ms polling completion path end
# to end, not just the UI.

library(shinytest2)
library(yaml)

# A self-contained project root so the run writes its snapshot DB and reports
# into an isolated temp tree (not /tmp, which dirname(config_dir) would be for
# the shared make_test_config_dir()). Absolute infra paths keep it independent
# of the process working directory.
make_run_project <- function(envir = parent.frame()) {
  root <- tempfile("dqrun_")
  cfg  <- file.path(root, "config")
  dir.create(cfg, recursive = TRUE)
  dir.create(file.path(root, "data"))
  dir.create(file.path(root, "reports"))
  withr::defer(unlink(root, recursive = TRUE), envir = envir)

  gcfg <- minimal_global_config()
  gcfg$snapshot_db       <- file.path(root, "data", "snapshots.sqlite")
  gcfg$report_output_dir <- file.path(root, "reports")
  yaml::write_yaml(gcfg, file.path(cfg, "dqcheckr.yml"))

  yaml::write_yaml(list(
    dataset_name = "run_ds", format = "csv", encoding = "UTF-8",
    delimiter = ",", current_file = fixture_csv
  ), file.path(cfg, "run_ds.yml"))

  list(config_dir = cfg, db = gcfg$snapshot_db)
}

# Poll the run-status output until it reaches a terminal state or times out.
# Cannot use wait_for_idle here: the run panel's 200ms invalidateLater keeps
# the app perpetually "busy" while a run is in flight, so idle never settles.
wait_for_run_status <- function(app, timeout_s = 120) {
  deadline <- Sys.time() + timeout_s
  repeat {
    html <- tryCatch(app$get_html("#run_status_area"), error = function(e) "")
    if (length(html) == 1 && !is.na(html) && nzchar(html) &&
        grepl("PASS|WARN|FAIL|Run failed|stopped", html))
      return(html)
    if (Sys.time() > deadline) return(if (length(html) == 1) html else "")
    Sys.sleep(0.5)
  }
}

test_that("run panel launches a check and reaches a terminal status (B-19)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  proj <- make_run_project()
  app  <- make_app_driver(proj$config_dir)

  app$run_js("Shiny.setInputValue('nav_run', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)
  app$set_inputs(run_dataset = "run_ds", wait_ = FALSE)
  app$wait_for_idle(timeout = 5000)

  # Precheck must be green for a valid config before we launch.
  precheck <- app$get_html("#run_precheck_status")
  expect_match(precheck, "Configuration looks good")

  app$click("run_start")
  status <- wait_for_run_status(app)

  # The poll observer must have collected a real PASS/WARN/FAIL result...
  expect_match(status, "PASS|WARN|FAIL")
  # ...and the run must have actually written a snapshot row to the DB.
  hist <- read_snapshot_history(proj$db, dataset_name = "run_ds", n = 10)
  expect_gte(nrow(hist), 1L)
})

test_that("run panel precheck flags an invalid config before launch (B-19)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  proj <- make_run_project()
  # A dataset whose current_file does not exist.
  yaml::write_yaml(list(
    dataset_name = "broken_run", format = "csv", encoding = "UTF-8",
    delimiter = ",", current_file = "/no/such/file.csv"
  ), file.path(proj$config_dir, "broken_run.yml"))
  app <- make_app_driver(proj$config_dir)

  app$run_js("Shiny.setInputValue('nav_run', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)
  app$set_inputs(run_dataset = "broken_run", wait_ = FALSE)
  app$wait_for_idle(timeout = 5000)

  precheck <- app$get_html("#run_precheck_status")
  expect_match(precheck, "Issues found")
  expect_match(precheck, "Current file not found")
})

# ── Precheck: remaining issue branches (B-22) ────────────────────────────────
# The existing tests drive only "Current file not found". Cover the parse-error,
# folder-not-found, and snapshot-db-dir-not-found branches so a regression that
# disables any one guard is caught.

test_that("run precheck surfaces parse / folder / snapshot-db-dir problems (B-22)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  proj <- make_run_project()
  cfg  <- proj$config_dir

  # Parse error: invalid YAML.
  writeLines(c("dataset_name: parse_bad", "column_rules: [unterminated"),
             file.path(cfg, "parse_bad.yml"))
  # Folder mode pointing at a non-existent folder.
  yaml::write_yaml(list(dataset_name = "folder_bad", format = "csv",
                        encoding = "UTF-8", delimiter = ",",
                        folder = file.path(tempdir(), "no_such_folder_b22")),
                   file.path(cfg, "folder_bad.yml"))
  # Snapshot DB whose parent directory does not exist.
  yaml::write_yaml(list(dataset_name = "dbdir_bad", format = "csv",
                        encoding = "UTF-8", delimiter = ",",
                        current_file = fixture_csv,
                        snapshot_db = file.path(tempdir(), "no_such_dir_b22", "s.sqlite")),
                   file.path(cfg, "dbdir_bad.yml"))

  app <- make_app_driver(cfg)
  app$run_js("Shiny.setInputValue('nav_run', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)

  read_precheck <- function(ds) {
    app$set_inputs(run_dataset = ds, wait_ = FALSE)
    app$wait_for_idle(timeout = 5000)
    app$get_html("#run_precheck_status") %||% ""
  }
  expect_match(read_precheck("parse_bad"),  "could not be parsed", ignore.case = TRUE)
  expect_match(read_precheck("folder_bad"), "Folder not found")
  expect_match(read_precheck("dbdir_bad"),  "Snapshot DB directory not found")
})

# ── Runtime failure after a passing precheck (B-23) ──────────────────────────
# finish_completed_run()'s crash-recovery branch (proc$get_result() throwing)
# was untested: every prior test either succeeds or is stopped by the user.

test_that("run panel reports a runtime failure that passes precheck (B-23)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  proj <- make_run_project()
  # FWF config with an existing current_file but no fwf_widths: precheck passes
  # (the file exists), but dqcheckr::run_dq_check() aborts (dqcheckr_invalid_config)
  # once it actually tries to read the file — exercising the error path.
  yaml::write_yaml(list(dataset_name = "bad_fwf", format = "fwf",
                        encoding = "UTF-8", current_file = fixture_csv),
                   file.path(proj$config_dir, "bad_fwf.yml"))

  app <- make_app_driver(proj$config_dir)
  app$run_js("Shiny.setInputValue('nav_run', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)
  app$set_inputs(run_dataset = "bad_fwf", wait_ = FALSE)
  app$wait_for_idle(timeout = 5000)

  # Precheck passes (file exists) — the failure only surfaces at run time.
  expect_match(app$get_html("#run_precheck_status"), "Configuration looks good")

  app$click("run_start")
  status <- wait_for_run_status(app)
  expect_match(status, "Run failed")
})
