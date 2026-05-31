#!/usr/bin/env Rscript
# Run the full dqcheckr GUI test suite.
# Usage (from the GUI/ directory):
#   Rscript tests/run_tests.R              # all tests
#   Rscript tests/run_tests.R unit         # unit tests only (no app needed)
#   Rscript tests/run_tests.R integration  # integration tests only

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) > 0) args[1] else "all"

library(testthat)

# shinytest2 uses skip_on_cran() internally; mark this as a local run.
Sys.setenv(NOT_CRAN = "true")

test_dir_path <- file.path(getwd(), "tests", "testthat")

unit_pattern        <- "config-io|utils|history"
integration_pattern <- "wizard-new|wizard-edit|ui-behavior"

filter_pattern <- switch(mode,
  unit        = unit_pattern,
  integration = integration_pattern,
  all         = NULL   # NULL = run everything
)

cat("==========================================================\n")
cat(" dqcheckr GUI Test Suite\n")
cat(sprintf(" Mode: %s\n", mode))
cat("==========================================================\n\n")

if (mode %in% c("all", "unit")) {
  cat("── Unit tests (no running app required) ──────────────────\n")
  cat("   Tests: config_io, utils, history\n\n")
}

if (mode %in% c("all", "integration")) {
  cat("── Integration tests (requires dqcheckr + shinytest2) ────\n")
  cat("   Tests: wizard-new, wizard-edit, ui-behavior\n\n")
  if (!requireNamespace("shinytest2", quietly = TRUE))
    message("WARNING: shinytest2 not installed — integration tests will be skipped.")
  if (!requireNamespace("dqcheckr", quietly = TRUE))
    message("WARNING: dqcheckr not installed — integration tests will be skipped.")
}

result <- testthat::test_dir(
  test_dir_path,
  filter   = filter_pattern,
  reporter = testthat::default_reporter()
)

cat("\n==========================================================\n")
failed <- sum(as.data.frame(result)$failed, na.rm = TRUE)
if (failed == 0) {
  cat(" All tests passed.\n")
} else {
  cat(sprintf(" %d test(s) FAILED.\n", failed))
}
cat("==========================================================\n")

quit(status = if (failed == 0) 0 else 1)
