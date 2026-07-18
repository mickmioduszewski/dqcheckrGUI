# Regression (B-02): FWF per-column type selections must reach the saved config.
# The Step-3 FWF type selector writes wiz$col_types_inferred (there is no Step 4
# for FWF), but build_config_list used to emit column_types only from
# wiz$col_types_override -> every FWF type choice was silently dropped.

test_that("build_config_list emits FWF column_types from col_types_inferred", {
  wiz <- make_wiz(
    format         = "fwf",
    file_mode      = "folder", folder = "/data",
    fwf_widths     = c(5L, 8L, 3L),
    fwf_col_names  = c("id", "amount", "ccy"),
    col_types_inferred = c(id = "character", amount = "numeric", ccy = "character")
  )
  cfg <- build_config_list(wiz)
  expect_equal(cfg$column_types,
               list(id = "character", amount = "numeric", ccy = "character"))
})

test_that("editing an FWF dataset without revisiting Step 3 preserves saved types", {
  # No col_types_inferred (Step 3 not revisited); saved types loaded into
  # col_types_override must survive the resave.
  wiz <- make_wiz(
    format         = "fwf",
    file_mode      = "folder", folder = "/data",
    fwf_widths     = c(5L, 8L),
    fwf_col_names  = c("id", "amount"),
    col_types_override = list(amount = "numeric")
  )
  cfg <- build_config_list(wiz)
  expect_equal(cfg$column_types, list(amount = "numeric"))
})

test_that("CSV column_types still come from Step-4 overrides only", {
  wiz <- make_wiz(
    format         = "csv",
    file_mode      = "folder", folder = "/data",
    col_types_override = list(price = "numeric"),
    col_types_inferred = c(price = "character")  # must be ignored for CSV
  )
  cfg <- build_config_list(wiz)
  expect_equal(cfg$column_types, list(price = "numeric"))
})
