# Integration tests: edit dataset wizard
# Regression suite for the "step N doesn't remember settings" class of bugs.

library(shinytest2)
library(yaml)

make_rich_config <- function(csv_path) {
  list(
    dataset_name     = "rich_test_ds",
    description      = "Integration test dataset",
    format           = "csv",
    encoding         = "UTF-8",
    delimiter        = ",",
    current_file     = csv_path,
    expected_columns = as.list(c("id", "name", "value", "date", "category")),
    key_columns      = as.list(c("id")),
    column_types     = list(value = "numeric"),
    column_rules = list(
      value    = list(min_value = 0, max_value = 1000),
      category = list(allowed_values = as.list(c("A", "B", "C")))
    ),
    rule_overrides   = list(max_missing_rate = 0.10, min_row_count = 5L)
  )
}

open_edit <- function(app, ds) {
  app$run_js(sprintf(
    "Shiny.setInputValue('ds_action',{action:'edit',ds:'%s'},{priority:'event'});", ds
  ))
  app$wait_for_idle(timeout = 5000)
}

nav_to <- function(app, step) {
  wizard_go_to_step(app, step)
}

# ── Step 1 ────────────────────────────────────────────────────────────────────

test_that("edit step 1 restores name and description", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")

  expect_equal(app$get_value(input = "wiz_dataset_name"), "rich_test_ds")
  expect_equal(app$get_value(input = "wiz_description"),  "Integration test dataset")
})

# ── Step 2 ────────────────────────────────────────────────────────────────────

test_that("edit step 2 restores explicit file mode and current_file path", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  nav_to(app, 2)

  # Check which radio is checked
  checked_val <- app$get_js(
    "document.querySelector('input[name=\"wiz_file_mode\"]:checked')?.value"
  )
  expect_equal(checked_val, "explicit")

  file_val <- app$get_value(input = "wiz_current_file_display")
  expect_equal(file_val, fixture_csv)
})

test_that("edit step 2 restores folder mode and folder path", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir  <- make_test_config_dir()
  data_dir <- dirname(fixture_csv)
  yaml::write_yaml(
    list(dataset_name = "folder_ds", format = "csv", encoding = "UTF-8",
         delimiter = ",", folder = data_dir),
    file.path(cfg_dir, "folder_ds.yml")
  )
  app <- make_app_driver(cfg_dir)

  open_edit(app, "folder_ds")
  nav_to(app, 2)

  checked_val <- app$get_js(
    "document.querySelector('input[name=\"wiz_file_mode\"]:checked')?.value"
  )
  expect_equal(checked_val, "folder")

  folder_val <- app$get_value(input = "wiz_folder_display")
  expect_equal(folder_val, data_dir)
})

# ── Step 4: column classification ─────────────────────────────────────────────
# Regression: step 4 collector fired before UI rendered, wiping loaded values.

test_that("edit step 4 restores key column checkboxes", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  nav_to(app, 4)

  # 'id' is column 1; it should be the key column. IDs are s4_key_<seq>_<i>
  # (nonce per wizard open — G-03), so match on prefix and _1 suffix.
  key1_checked <- app$get_js(
    "Array.from(document.querySelectorAll('input[id^=\"s4_key_\"]'))
       .find(c => c.id.endsWith('_1'))?.checked"
  )
  expect_true(isTRUE(key1_checked), info = "'id' (col 1) must be checked as key column")
})

test_that("edit step 4 restores expected column count", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  nav_to(app, 4)

  exp_count <- app$get_js(
    "Array.from(document.querySelectorAll('input[id^=\"s4_exp_\"]')).filter(c=>c.checked).length"
  )
  expect_equal(as.integer(exp_count), 5L, info = "All 5 expected columns must be checked")
})

test_that("edit step 4 restores type overrides", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  nav_to(app, 4)

  type_vals <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('select[id^=\"s4_type_\"]')).map(s=>s.value)"
  ))
  expect_true("numeric" %in% type_vals,
              info = "'value' column must have 'numeric' type override restored")
})

# ── Step 5: column rules ──────────────────────────────────────────────────────
# Regression: step 5 collector fired before UI rendered, wiping loaded column_rules.

test_that("edit step 5 restores min/max rules for numeric column", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  nav_to(app, 5)

  rule_vals <- unlist(app$get_js(
    "Array.from(document.querySelectorAll('input[id^=\"s5_min_\"],input[id^=\"s5_max_\"]'))
       .filter(i=>i.value!=='').map(i=>parseFloat(i.value))"
  ))
  expect_true(0    %in% rule_vals, info = "min_value=0 must be restored")
  expect_true(1000 %in% rule_vals, info = "max_value=1000 must be restored")
})

# ── Step 6: rule overrides ────────────────────────────────────────────────────
# Regression: collect_step_inputs ran on every Next click, wiping rule_overrides
# loaded from config because step 6 inputs didn't exist yet.

test_that("edit step 6 restores rule overrides from config", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  nav_to(app, 6)

  max_missing <- as.numeric(app$get_value(input = "wiz_ro_max_missing"))
  min_rows    <- as.integer(app$get_value(input = "wiz_ro_min_rows"))

  expect_equal(max_missing, 0.10, tolerance = 1e-6,
               info = "max_missing_rate override must be 0.10 from config")
  expect_equal(min_rows, 5L,
               info = "min_row_count override must be 5 from config")
})

# ── Edit → re-save round-trip ────────────────────────────────────────────────

test_that("edit wizard re-saves without corrupting the YAML", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  nav_to(app, 8)

  app$wait_for_js("document.getElementById('wizard_save') !== null", timeout = 5000)
  app$click("wizard_save")
  app$wait_for_idle()

  saved <- yaml::read_yaml(file.path(cfg_dir, "rich_test_ds.yml"))
  expect_equal(saved$dataset_name, "rich_test_ds")
  expect_equal(saved$current_file, fixture_csv)
  expect_equal(unlist(saved$key_columns), "id")
  expect_equal(saved$rule_overrides$max_missing_rate, 0.10, tolerance = 1e-6)
  expect_equal(as.integer(saved$rule_overrides$min_row_count), 5L)
})

# ── Cross-open state isolation (0.2.1, G-03) ──────────────────────────────────
# Editing dataset A then dataset B must not leak A's step-4 selections into B —
# previously stale positional inputs from A's session could clobber B's state.

test_that("editing a second dataset does not inherit the first one's key columns", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  yaml::write_yaml(
    list(dataset_name = "plain_ds", format = "csv", encoding = "UTF-8",
         delimiter = ",", current_file = fixture_csv),   # no key/expected cols
    file.path(cfg_dir, "plain_ds.yml")
  )
  app <- make_app_driver(cfg_dir)

  # Open A (has key column 'id'), render its step 4, then cancel out.
  open_edit(app, "rich_test_ds")
  nav_to(app, 4)
  app$click("wizard_cancel");         app$wait_for_idle(timeout = 5000)
  app$click("wizard_cancel_confirm"); app$wait_for_idle(timeout = 5000)

  # Open B and render its step 4: no key checkbox may be checked.
  open_edit(app, "plain_ds")
  nav_to(app, 4)
  n_checked <- app$get_js(
    "Array.from(document.querySelectorAll('input[id^=\"s4_key_\"]'))
       .filter(c => c.offsetParent !== null && c.checked).length"
  )
  expect_equal(as.integer(n_checked), 0L)

  # And B's YAML preview must not contain A's key_columns.
  wizard_go_to_step(app, 8, from_step = 4)
  yaml_text <- app$get_value(output = "yaml_preview")
  expect_false(grepl("key_columns", yaml_text, fixed = TRUE))
  expect_match(yaml_text, "dataset_name: plain_ds")
})

# ── Rename flow (0.2.1, G-07) ─────────────────────────────────────────────────

test_that("renaming a dataset in edit mode removes the old config file", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  app$set_inputs(wiz_dataset_name = "renamed_ds")
  app$wait_for_idle(timeout = 5000)
  nav_to(app, 8)
  app$click("wizard_save")
  app$wait_for_idle(timeout = 8000)

  expect_true(file.exists(file.path(cfg_dir, "renamed_ds.yml")))
  expect_false(file.exists(file.path(cfg_dir, "rich_test_ds.yml")))
})

test_that("renaming onto another existing dataset is blocked at save", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  yaml::write_yaml(make_rich_config(fixture_csv), file.path(cfg_dir, "rich_test_ds.yml"))
  yaml::write_yaml(
    list(dataset_name = "victim_ds", format = "csv", encoding = "UTF-8",
         delimiter = ",", current_file = fixture_csv, description = "keep me"),
    file.path(cfg_dir, "victim_ds.yml")
  )
  app <- make_app_driver(cfg_dir)

  open_edit(app, "rich_test_ds")
  app$set_inputs(wiz_dataset_name = "victim_ds")
  app$wait_for_idle(timeout = 5000)
  nav_to(app, 8)
  app$click("wizard_save")
  app$wait_for_idle(timeout = 8000)

  # victim_ds.yml untouched; rich_test_ds.yml still present (save was blocked)
  victim <- yaml::read_yaml(file.path(cfg_dir, "victim_ds.yml"))
  expect_equal(victim$description, "keep me")
  expect_true(file.exists(file.path(cfg_dir, "rich_test_ds.yml")))
})
