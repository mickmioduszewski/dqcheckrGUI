# Integration test: run panel Stop flow. Covers the race a run finishing while
# the Stop-confirm dialog is open: its real result must be collected, not
# overwritten with "stopped".

library(shinytest2)
library(yaml)

make_stop_project <- function(envir = parent.frame()) {
  root <- tempfile("dqstop_")
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
    dataset_name = "stop_ds", format = "csv", encoding = "UTF-8",
    delimiter = ",", current_file = fixture_csv
  ), file.path(cfg, "stop_ds.yml"))

  list(config_dir = cfg, db = gcfg$snapshot_db)
}

stop_run_status <- function(app, timeout_s = 120) {
  deadline <- Sys.time() + timeout_s
  repeat {
    html <- tryCatch(app$get_html("#run_status_area"), error = function(e) "")
    if (length(html) == 1 && !is.na(html) && nzchar(html) &&
        grepl("PASS|WARN|FAIL|STOPPED|ERROR|Run failed", html, ignore.case = TRUE))
      return(html)
    if (Sys.time() > deadline) return(if (length(html) == 1) html else "")
    Sys.sleep(0.4)
  }
}

open_run_for <- function(app, ds) {
  app$run_js("Shiny.setInputValue('nav_run', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)
  app$set_inputs(run_dataset = ds, wait_ = FALSE)
  app$wait_for_idle(timeout = 5000)
}

test_that("stop-confirm after a run completes keeps the real result, not 'stopped' (B-12)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  proj <- make_stop_project()
  app  <- make_app_driver(proj$config_dir)
  open_run_for(app, "stop_ds")

  app$click("run_start")
  status <- stop_run_status(app)
  expect_match(status, "PASS|WARN|FAIL")   # run reached a real terminal result

  # Fire Stop-confirm AFTER completion (the dialog-open-past-completion race).
  # r_process is retained after finish, so the confirm handler must detect the
  # dead process and re-collect the result rather than writing "stopped".
  app$run_js("Shiny.setInputValue('run_stop_confirm', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)
  after <- app$get_html("#run_status_area")
  expect_match(after, "PASS|WARN|FAIL")
  expect_no_match(after, "STOPPED", ignore.case = TRUE)

  # And the completed run's snapshot is on disk regardless.
  hist <- read_snapshot_history(proj$db, dataset_name = "stop_ds", n = 10)
  expect_gte(nrow(hist), 1L)
})

test_that("stop-confirm reaches a clean terminal state without hanging (B-12)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  proj <- make_stop_project()
  app  <- make_app_driver(proj$config_dir)
  open_run_for(app, "stop_ds")

  app$click("run_start")
  # Confirm a stop as soon as possible — depending on timing this either kills a
  # live run ("stopped") or collects an already-finished one (PASS/WARN/FAIL).
  # Both are correct; the invariant is that the app never stays stuck "running".
  app$run_js("Shiny.setInputValue('run_stop_confirm', 1, {priority:'event'});")
  status <- stop_run_status(app)
  expect_match(status, "PASS|WARN|FAIL|STOPPED", ignore.case = TRUE)
})
