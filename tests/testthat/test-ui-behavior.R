# Integration tests: UI behaviour
# Covers interaction bugs found during manual testing.

library(shinytest2)
library(yaml)


# ── Step 1: focus preservation ────────────────────────────────────────────────
# Regression: renderUI re-rendered the whole step on every keystroke, losing focus.

test_that("typing in step 1 does not cause excessive step re-renders", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)
  open_new_wizard(app)

  # Count DOM mutations on the step content container while typing
  app$run_js("
    window.__renderCount = 0;
    const target = document.getElementById('wizard_step_content');
    if (target) new MutationObserver(() => window.__renderCount++)
      .observe(target, { childList: true, subtree: true });
  ")

  for (ch in strsplit("my_dataset", "")[[1]]) {
    app$run_js(sprintf(
      "var el=document.getElementById('wiz_dataset_name');
       el.value+='%s';
       el.dispatchEvent(new Event('input'));", ch
    ))
    Sys.sleep(0.05)
  }
  app$wait_for_idle()

  render_count <- as.integer(app$get_js("window.__renderCount"))
  # Should not re-render 10 times (once per keystroke).
  # Allow a small number for initial binding, but not per-character re-renders.
  expect_lt(render_count, 5L,
            label = paste("step re-render count while typing:", render_count))
})

test_that("step 1 input value survives reactive settle", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)
  open_new_wizard(app)

  app$set_inputs(wiz_dataset_name = "my_test_name")
  app$wait_for_idle()

  expect_equal(app$get_value(input = "wiz_dataset_name"), "my_test_name")
})

# ── Step 4: scroll preservation ───────────────────────────────────────────────
# Regression: renderUI re-built the column table on every checkbox click, scrollTop→0.

test_that("step 4 checkbox click does not reset table scroll position", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir  <- make_test_config_dir()
  data_dir <- dirname(fixture_csv)
  app      <- make_app_driver(cfg_dir)
  open_new_wizard(app)

  app$set_inputs(wiz_dataset_name = "scroll_test")
  app$wait_for_idle()

  wizard_go_to_step(app, 2)
  app$set_inputs(wiz_folder_display = data_dir)
  Sys.sleep(0.8)
  app$wait_for_idle(timeout = 6000)

  wizard_go_to_step(app, 4, from_step = 2)

  # Scroll the column table
  scroll_set <- as.numeric(app$get_js("
    const div = document.querySelector('[style*=\"max-height:500px\"]');
    if (div && div.scrollHeight > div.clientHeight) { div.scrollTop=120; return div.scrollTop; }
    return 0;
  "))

  if (isTRUE(scroll_set > 0)) {
    app$run_js("const cb=document.querySelector('input[id^=\"s4_key_\"]'); if(cb) cb.click();")
    app$wait_for_idle()

    scroll_after <- as.numeric(app$get_js("
      const div=document.querySelector('[style*=\"max-height:500px\"]');
      return div ? div.scrollTop : -1;
    "))
    expect_gt(scroll_after, 0L,
              label = paste("scroll position after checkbox click:", scroll_after))
  } else {
    skip("Table not tall enough to scroll with fixture data — cannot test scroll preservation")
  }
})

# ── Dataset sidebar: sort order ───────────────────────────────────────────────
# Regression: list_dataset_configs returned filesystem order; explicit sort() now added.

test_that("dataset sidebar lists datasets alphabetically regardless of creation order", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  # Create in reverse alphabetical order to stress the sort
  for (nm in c("zebra_ds", "alpha_ds", "mango_ds")) {
    yaml::write_yaml(
      list(dataset_name = nm, format = "csv", folder = dirname(fixture_csv)),
      file.path(cfg_dir, paste0(nm, ".yml"))
    )
    Sys.sleep(0.02)
  }

  app <- make_app_driver(cfg_dir)
  app$wait_for_idle()

  names_in_order <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('.dataset-item span:first-child'))
       .map(s=>s.textContent.trim()).filter(t=>t.length>0)"
  ))

  expect_equal(names_in_order, c("alpha_ds", "mango_ds", "zebra_ds"),
               info = "Sidebar datasets must be alphabetically sorted")
})

# ── Run panel: dataset selector sort order ────────────────────────────────────

test_that("run panel dataset selector is sorted alphabetically", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  for (nm in c("zebra_run", "alpha_run", "mango_run")) {
    yaml::write_yaml(
      list(dataset_name = nm, format = "csv", folder = dirname(fixture_csv)),
      file.path(cfg_dir, paste0(nm, ".yml"))
    )
  }

  app <- make_app_driver(cfg_dir)
  app$run_js("Shiny.setInputValue('nav_run', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)

  # selectize replaces the native <select> with a custom widget; its internal
  # options object is the reliable source of choices in sorted order.
  # The initially-selected value will be datasets[1] from the sorted list.
  selected <- app$get_value(input = "run_dataset")
  expect_equal(selected, "alpha_run",
               info = "Run panel must default to first dataset alphabetically")

  # Also verify all three exist via selectize's internal options object
  selectize_vals <- unlist(app$get_js(
    "Object.keys(document.getElementById('run_dataset').selectize.options)"
  ))
  expect_true(all(c("alpha_run", "mango_run", "zebra_run") %in% selectize_vals),
              info = "All three datasets must appear as selectable options")
})

# ── Step 2: path status badges ────────────────────────────────────────────────

test_that("step 2 shows success badge for valid folder path", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)
  open_new_wizard(app)

  app$set_inputs(wiz_dataset_name = "badge_test")
  app$wait_for_idle()
  wizard_go_to_step(app, 2)

  app$set_inputs(wiz_folder_display = dirname(fixture_csv))
  Sys.sleep(0.8)
  app$wait_for_idle(timeout = 5000)

  badge <- app$get_html("#wiz_folder_path_status")
  expect_true(grepl("Folder found|text-success", badge, ignore.case = TRUE))
})

test_that("step 2 shows error badge for non-existent folder path", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)
  open_new_wizard(app)

  app$set_inputs(wiz_dataset_name = "badge_err")
  app$wait_for_idle()
  wizard_go_to_step(app, 2)

  app$set_inputs(wiz_folder_display = "/path/does/not/exist/xyzabc")
  Sys.sleep(0.8)
  app$wait_for_idle(timeout = 3000)

  badge <- app$get_html("#wiz_folder_path_status")
  expect_true(grepl("not found|text-danger", badge, ignore.case = TRUE))
})

test_that("step 2 explicit mode shows success badge for valid file", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)
  open_new_wizard(app)

  app$set_inputs(wiz_dataset_name = "file_badge_test")
  app$wait_for_idle()
  wizard_go_to_step(app, 2)

  app$set_inputs(wiz_file_mode = "explicit")
  app$wait_for_idle()
  app$set_inputs(wiz_current_file_display = fixture_csv)
  Sys.sleep(0.8)
  app$wait_for_idle(timeout = 3000)

  badge <- app$get_html("#wiz_current_file_status")
  expect_true(grepl("File found|text-success", badge, ignore.case = TRUE))
})

# ── Global config: save and reload ────────────────────────────────────────────

test_that("global config saves values to dqcheckr.yml", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)

  app$run_js("Shiny.setInputValue('nav_global', 1, {priority:'event'});")
  app$wait_for_idle()

  app$set_inputs(
    gcfg_max_missing_rate = 0.42,
    gcfg_snapshot_db      = file.path(cfg_dir, "test.sqlite"),
    gcfg_report_dir       = cfg_dir
  )
  app$wait_for_idle()
  app$click("gcfg_save")
  app$wait_for_idle()

  saved <- yaml::read_yaml(file.path(cfg_dir, "dqcheckr.yml"))
  expect_equal(saved$default_rules$max_missing_rate, 0.42, tolerance = 1e-6)
})

# ── History: Compare button starts disabled ───────────────────────────────────
# Regression: disabled=NA was treated as absent in htmltools, leaving the button
# clickable before any checkboxes were selected.

test_that("history Compare drift button is disabled on initial page load", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)

  app$run_js("Shiny.setInputValue('nav_history', 1, {priority:'event'});")
  app$wait_for_idle(timeout = 5000)

  disabled <- app$get_js(
    "document.getElementById('history_compare')?.hasAttribute('disabled')"
  )
  expect_true(isTRUE(disabled),
              info = "history_compare button must carry the disabled attribute on load")
})

# ── Dataset panel: Compare button starts disabled ─────────────────────────────

test_that("dataset panel Compare drift button is disabled before checkboxes are checked", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir  <- make_test_config_dir()
  data_dir <- dirname(fixture_csv)

  # Create a dataset config so the panel shows last_runs table
  yaml::write_yaml(
    list(dataset_name = "panel_ds", format = "csv", folder = data_dir),
    file.path(cfg_dir, "panel_ds.yml")
  )

  # Write a minimal snapshot row so last_runs is non-empty (table appears)
  db_path <- file.path(cfg_dir, "test.sqlite")
  yaml::write_yaml(
    list(snapshot_db = db_path, report_output_dir = cfg_dir),
    file.path(cfg_dir, "dqcheckr.yml")
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "
    CREATE TABLE snapshots (
      id INTEGER PRIMARY KEY, dataset_name TEXT, file_name TEXT,
      run_timestamp TEXT, overall_status TEXT,
      check_fail_count INTEGER, check_warn_count INTEGER, row_count INTEGER
    )")
  DBI::dbExecute(con,
    "INSERT INTO snapshots VALUES (1,'panel_ds','f.csv','2026-05-31 10:00:00','PASS',0,0,10)")
  DBI::dbDisconnect(con)

  app <- make_app_driver(cfg_dir)

  # Navigate to the dataset panel
  app$run_js("Shiny.setInputValue('sidebar_dataset_click','panel_ds',{priority:'event'});")
  app$wait_for_idle(timeout = 5000)

  disabled <- app$get_js(
    "document.getElementById('compare_drift_panel_ds')?.hasAttribute('disabled')"
  )
  expect_true(isTRUE(disabled),
              info = "compare_drift button must carry the disabled attribute on load")
})
