# Integration tests: new dataset wizard

library(shinytest2)
library(yaml)

# ── New wizard: folder mode ───────────────────────────────────────────────────

test_that("new wizard (folder mode) saves correct YAML with folder key", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir  <- make_test_config_dir()
  data_dir <- dirname(fixture_csv)
  app      <- make_app_driver(cfg_dir)

  open_new_wizard(app)

  # Step 1
  app$set_inputs(wiz_dataset_name = "folder_test_ds")
  app$wait_for_idle()

  # Step 2: folder mode (default), set path — sleep past the 600ms debounce first
  wizard_go_to_step(app, 2)
  app$set_inputs(wiz_folder_display = data_dir)
  Sys.sleep(0.8)
  app$wait_for_idle(timeout = 6000)

  # Steps 3–8 (already on step 2; navigate the remaining 6 steps)
  wizard_go_to_step(app, 8, from_step = 2)

  # Save
  app$wait_for_js("document.getElementById('wizard_save') !== null", timeout = 5000)
  app$click("wizard_save")
  app$wait_for_idle()

  saved <- file.path(cfg_dir, "folder_test_ds.yml")
  expect_true(file.exists(saved), info = "config file must be created")

  raw <- yaml::read_yaml(saved)
  expect_equal(raw$dataset_name, "folder_test_ds")
  expect_equal(raw$folder,       data_dir)
  expect_null(raw$current_file,
              info = "folder mode must not write current_file")
})

# ── New wizard: explicit file mode ────────────────────────────────────────────
# Regression: file_mode not synced from radio → current_file silently dropped.

test_that("new wizard (explicit mode) saves current_file, not folder", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)

  open_new_wizard(app)

  # Step 1
  app$set_inputs(wiz_dataset_name = "explicit_test_ds")
  app$wait_for_idle()

  # Step 2: explicit mode
  wizard_go_to_step(app, 2)
  app$set_inputs(wiz_file_mode = "explicit")
  app$wait_for_idle()
  app$set_inputs(wiz_current_file_display = fixture_csv)
  Sys.sleep(0.8)
  app$wait_for_idle(timeout = 6000)

  # Steps 3–8 (already on step 2)
  wizard_go_to_step(app, 8, from_step = 2)

  app$wait_for_js("document.getElementById('wizard_save') !== null", timeout = 5000)
  app$click("wizard_save")
  app$wait_for_idle()

  saved <- file.path(cfg_dir, "explicit_test_ds.yml")
  expect_true(file.exists(saved))

  raw <- yaml::read_yaml(saved)
  expect_equal(raw$current_file, fixture_csv,
               info = "current_file must be written in explicit mode")
  expect_null(raw$folder,
              info = "folder must not appear in explicit-mode YAML")
})

# ── Step 1 validation ─────────────────────────────────────────────────────────

test_that("step 1 Next is disabled until a valid name is entered", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)

  open_new_wizard(app)

  # Initially disabled (empty name)
  disabled_initially <- app$get_js(
    "document.getElementById('wizard_next') && document.getElementById('wizard_next').hasAttribute('disabled')"
  )
  expect_true(isTRUE(disabled_initially), info = "Next must be disabled with empty name")

  # Invalid name starting with digit
  app$set_inputs(wiz_dataset_name = "1invalid")
  app$wait_for_idle()
  disabled_invalid <- app$get_js(
    "document.getElementById('wizard_next').hasAttribute('disabled')"
  )
  expect_true(isTRUE(disabled_invalid), info = "Next must stay disabled for invalid name")

  # Valid name — Next must become enabled
  app$set_inputs(wiz_dataset_name = "valid_name")
  app$wait_for_idle()
  disabled_valid <- app$get_js(
    "document.getElementById('wizard_next').hasAttribute('disabled')"
  )
  expect_false(isTRUE(disabled_valid), info = "Next must be enabled for valid name")
})

test_that("step 1 shows duplicate-name error for existing dataset", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(list(dataset_name = "existing_ds", format = "csv"),
                   file.path(cfg_dir, "existing_ds.yml"))

  app <- make_app_driver(cfg_dir)
  open_new_wizard(app)

  app$set_inputs(wiz_dataset_name = "existing_ds")
  app$wait_for_idle()

  page_html <- app$get_html("body")
  expect_true(grepl("already exists", page_html, ignore.case = TRUE))
})

# ── Wizard cancel ─────────────────────────────────────────────────────────────

test_that("cancel wizard discards changes without creating a file", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  app     <- make_app_driver(cfg_dir)

  open_new_wizard(app)
  app$set_inputs(wiz_dataset_name = "cancelled_ds")
  app$wait_for_idle()

  app$click("wizard_cancel")
  app$wait_for_idle()
  app$click("wizard_cancel_confirm")
  app$wait_for_idle()

  expect_false(file.exists(file.path(cfg_dir, "cancelled_ds.yml")),
               info = "cancelled wizard must not write a file")
})
