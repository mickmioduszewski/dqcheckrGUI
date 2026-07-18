# Regression (B-05): sniff_csv_file must not swallow errors into a bare NULL.
# A caught error is marked with $sniff_error so the observer can notify the user
# (the CSV analogue of the FWF auto-detect failure notification).

test_that("sniff_csv_file marks a caught error with $sniff_error", {
  # readLines also emits a "cannot open file" warning before erroring; that is
  # incidental to this trigger — we only assert the error is captured, not swallowed.
  res <- suppressWarnings(sniff_csv_file("/no/such/path/does-not-exist.csv"))
  expect_type(res, "list")
  expect_false(is.null(res$sniff_error))
  expect_null(res$col_names)          # not a successful sniff
})

test_that("sniff_csv_file returns a clean result (no $sniff_error) on a good file", {
  res <- sniff_csv_file(fixture_csv)
  expect_type(res, "list")
  expect_null(res$sniff_error)
  expect_false(is.null(res$col_names))
  expect_false(is.null(res$delimiter))
})

test_that("sniff_csv_file returns NULL for an empty file (no rows to sniff)", {
  f <- tempfile(fileext = ".csv")
  file.create(f)
  on.exit(unlink(f))
  expect_null(sniff_csv_file(f))
})
