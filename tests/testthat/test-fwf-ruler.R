# Integration test: FWF ruler -> server boundary contract. The ruler JS emits
# column boundaries as fwf_boundary_positions; this drives that exact input (as a
# drag would) and verifies the server turns it into the right fwf_widths and
# column definitions. The raw interact.js drag mechanics are third-party and out
# of scope.

library(shinytest2)
library(yaml)

# Fixed-width fixture: 5-char id, 10-char name, 8-char date => line length 23,
# column boundaries at char positions 5 and 15, widths 5 / 10 / 8.
write_fwf_fixture <- function(dir) {
  path <- file.path(dir, "fwf_sample.txt")
  writeLines(c("00001Alice     20240115",
               "00002Bob       20240220",
               "00003Clara     20231231"), path)
  path
}

test_that("ruler boundary positions become fwf_widths and column defs (B-13)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("dqcheckr")

  cfg_dir <- make_test_config_dir()
  fwf     <- write_fwf_fixture(dirname(cfg_dir))
  app     <- make_app_driver(cfg_dir)

  open_new_wizard(app)
  app$set_inputs(wiz_dataset_name = "fwf_ruler_ds")
  app$wait_for_idle()

  # Step 2: explicit file mode, point at the FWF fixture (loads raw_lines).
  wizard_go_to_step(app, 2)
  app$set_inputs(wiz_file_mode = "explicit")
  app$wait_for_idle()
  app$set_inputs(wiz_current_file_display = fwf)
  Sys.sleep(0.8)
  app$wait_for_idle(timeout = 6000)

  # Step 3: switch to FWF, then emit the boundary positions the ruler drag would.
  wizard_go_to_step(app, 3, from_step = 2)
  app$set_inputs(wiz_format = "fwf")
  app$wait_for_idle(timeout = 5000)
  app$run_js("Shiny.setInputValue('fwf_boundary_positions', [5, 15], {priority:'event'});")
  app$wait_for_idle(timeout = 5000)

  # The column-definition table must reflect three columns of widths 5 / 10 / 8.
  defs <- app$get_html("#fwf_col_def_table")
  expect_match(defs, "w:\\s*5")
  expect_match(defs, "w:\\s*10")
  expect_match(defs, "w:\\s*8")

  # And the record-length badge confirms the widths span the whole 23-char line.
  badge <- app$get_html("#fwf_record_length_badge")
  expect_match(badge, "23\\s*/\\s*23")
})
