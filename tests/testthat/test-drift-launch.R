# Integration test: drift comparison launch + poll (B-11, previously uncovered
# per CLAUDE.md "Not yet tested"). Seeds two real snapshots, drives the History
# panel Compare button, and verifies the async callr launch -> 500ms poll ->
# completion chain by asserting the rendered drift report actually lands on disk.

library(shinytest2)
library(yaml)

quarto_available <- function() {
  nzchar(Sys.which("quarto")) ||
    (requireNamespace("quarto", quietly = TRUE) &&
       !is.null(tryCatch(quarto::quarto_path(), error = function(e) NULL)))
}

# Isolated project with a dataset that already has two snapshots, so a drift
# comparison between them has something to diff.
make_drift_project <- function(envir = parent.frame()) {
  root <- tempfile("dqdrift_")
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
    dataset_name = "drift_ds", format = "csv", encoding = "UTF-8",
    delimiter = ",", current_file = fixture_csv
  ), file.path(cfg, "drift_ds.yml"))

  # Two runs -> two snapshot rows (ids 1 and 2) to compare.
  dqcheckr::run_dq_check("drift_ds", config_dir = cfg, open_report = FALSE)
  dqcheckr::run_dq_check("drift_ds", config_dir = cfg, open_report = FALSE)

  list(config_dir = cfg, db = gcfg$snapshot_db, reports = gcfg$report_output_dir)
}

test_that("history Compare launches a drift comparison and renders a report (B-11)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")
  skip_if_not(quarto_available(), "Quarto not available to render the drift report")

  proj <- make_drift_project()
  ids  <- read_snapshot_history(proj$db, dataset_name = "drift_ds", n = 10)$id
  expect_gte(length(ids), 2L)

  app <- make_app_driver(proj$config_dir)
  app$run_js("Shiny.setInputValue('nav_history', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)

  # Select two snapshots exactly as the checkbox JS does, then click Compare.
  app$run_js(sprintf(
    "Shiny.setInputValue('hist_selected_ids', [%d, %d], {priority:'event'});",
    ids[1], ids[2]))
  app$wait_for_idle(timeout = 3000)
  app$click("history_compare")

  # Poll the reports dir for the rendered drift report (proves launch -> callr ->
  # compare_snapshots -> render -> poll completed, not just that the click fired).
  deadline <- Sys.time() + 120
  found <- character(0)
  repeat {
    found <- list.files(proj$reports, pattern = "^drift_drift_ds_.*\\.html$",
                        full.names = TRUE)
    if (length(found) > 0 || Sys.time() > deadline) break
    Sys.sleep(1)
  }
  expect_gt(length(found), 0L)
})

test_that("a second drift launch while one is running is refused (B-11 overlap guard)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  proj <- make_drift_project()
  ids  <- read_snapshot_history(proj$db, dataset_name = "drift_ds", n = 10)$id

  app <- make_app_driver(proj$config_dir)
  app$run_js("Shiny.setInputValue('nav_history', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)
  app$run_js(sprintf(
    "Shiny.setInputValue('hist_selected_ids', [%d, %d], {priority:'event'});",
    ids[1], ids[2]))
  app$wait_for_idle(timeout = 3000)

  # Fire Compare twice in immediate succession; the second must hit the
  # "already running" guard rather than orphaning the first callr process.
  app$click("history_compare")
  app$click("history_compare")
  Sys.sleep(1)
  html <- app$get_html("body")
  expect_match(html, "already running", info = "overlap guard notification")
})
