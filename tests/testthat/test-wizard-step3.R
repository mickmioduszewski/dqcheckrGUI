library(shiny)  # UI helpers (tagList, radioButtons) used unqualified in app code

# Regression: editing an FWF dataset must not flip wiz_format back to CSV.
# wizard_step3_ui() previously hardcoded selected="csv"; on re-render this
# clobbered wiz$format to "csv" (via the observeEvent on wiz_format), so a
# no-touch resave rewrote the FWF config as broken CSV. The Format radio must
# seed from wiz$format.

test_that("step 3 UI selects FWF when editing an FWF dataset", {
  html <- as.character(wizard_step3_ui(make_wiz(format = "fwf")))
  # The rendered radio marks the selected option's <input> with checked.
  expect_match(html, 'value="fwf"[^>]*checked')
  expect_no_match(html, 'value="csv"[^>]*checked')
})

test_that("step 3 UI selects CSV for a CSV dataset and defaults when unset", {
  csv_html <- as.character(wizard_step3_ui(make_wiz(format = "csv")))
  expect_match(csv_html, 'value="csv"[^>]*checked')
  expect_no_match(csv_html, 'value="fwf"[^>]*checked')

  # Missing/empty format falls back to csv (GUI %||% treats "" as absent).
  default_html <- as.character(wizard_step3_ui(make_wiz(format = "")))
  expect_match(default_html, 'value="csv"[^>]*checked')
})
