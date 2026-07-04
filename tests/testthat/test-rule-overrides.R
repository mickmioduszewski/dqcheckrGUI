# collect_step_inputs (server_wizard.R): step-6 rule-override collection.
# Regression for G-08 — identical(0, 0L) is FALSE, so untouched numericInputs
# (which return doubles) used to write spurious overrides whenever the global
# default was integer (e.g. the scaffolded min_row_count: 0).

# collect_step_inputs reads `input` with $/[[ and assigns wiz$rule_overrides,
# so a plain named list and an environment stand in for the Shiny objects.
.mock_input_at_defaults <- function(gcfg) {
  dr <- gcfg$default_rules
  list(
    wiz_ro_max_missing    = as.numeric(dr$max_missing_rate),
    wiz_ro_max_nonnumeric = as.numeric(dr$max_non_numeric_rate),
    wiz_ro_min_rows       = as.numeric(dr$min_row_count),      # double vs 0L default
    wiz_ro_max_rowchg     = round(dr$max_row_count_change_pct * 100, 2),
    wiz_ro_max_meanshift  = round(dr$max_numeric_mean_shift_pct * 100, 2),
    wiz_ro_max_misschg    = as.numeric(dr$max_missing_rate_change_pp),
    wiz_ro_max_nonnumchg  = as.numeric(dr$max_non_numeric_rate_change_pp),
    wiz_ro_type_inf       = as.numeric(dr$type_inference_threshold),
    wiz_ro_flag_new       = TRUE,
    wiz_ro_flag_drop      = TRUE,
    wiz_ro_flag_type      = TRUE,
    wiz_ro_flag_order     = TRUE
  )
}

test_that("untouched step-6 inputs write no overrides, even across int/double defaults", {
  gcfg  <- minimal_global_config()           # min_row_count = 0L (integer)
  input <- .mock_input_at_defaults(gcfg)
  wiz   <- new.env()
  collect_step_inputs(input, wiz, gcfg)
  expect_length(wiz$rule_overrides, 0)
})

test_that("changed step-6 inputs are collected as overrides", {
  gcfg  <- minimal_global_config()
  input <- .mock_input_at_defaults(gcfg)
  input$wiz_ro_max_missing <- 0.10           # changed from 0.05
  input$wiz_ro_max_rowchg  <- 15             # changed from 10 (%)
  input$wiz_ro_flag_order  <- FALSE          # changed from TRUE
  wiz <- new.env()
  collect_step_inputs(input, wiz, gcfg)
  expect_equal(wiz$rule_overrides$max_missing_rate, 0.10)
  expect_equal(wiz$rule_overrides$max_row_count_change_pct, 0.15)
  expect_false(wiz$rule_overrides$flag_column_order_change)
  expect_length(wiz$rule_overrides, 3)
})

test_that("collect_step_inputs bails out when step 6 has not rendered", {
  wiz <- new.env()
  wiz$rule_overrides <- list(max_missing_rate = 0.10)   # config-loaded value
  collect_step_inputs(list(), wiz, minimal_global_config())
  expect_equal(wiz$rule_overrides, list(max_missing_rate = 0.10))  # untouched
})
