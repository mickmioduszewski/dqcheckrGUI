# Regression (B-03): Step-5 per-column "Max mean shift (%)" must round-trip.
# max_numeric_mean_shift_pct is STORED as a fraction (0.20 = 20%). The Step-5
# field is labelled "(%)" and merge_column_rule() divides the input by 100, so
# the display must multiply the stored fraction by 100. Previously the display
# showed the raw fraction, so a no-touch resave shrank the value 100x each time.

# The display transform used by the Step-5 numericInput (server_wizard.R).
.step5_meanshift_display <- function(stored) {
  if (length(stored) && !is.na(stored)) round(stored * 100, 2) else NULL
}

test_that("merge_column_rule stores the percent input as a fraction", {
  rules <- merge_column_rule("numeric", saved = NULL, meanshift_in = 20)
  expect_equal(rules$max_numeric_mean_shift_pct, 0.20)
})

test_that("Step-5 mean-shift display/merge round-trips (no 100x drift)", {
  for (stored in c(0.05, 0.20, 0.50, 1.00)) {
    shown  <- .step5_meanshift_display(stored)          # what the field shows
    merged <- merge_column_rule("numeric", saved = NULL,
                                meanshift_in = shown)$max_numeric_mean_shift_pct
    expect_equal(merged, stored)                         # a no-touch resave is a no-op
  }
})

test_that("Step-5 mean-shift display stays empty when no rule is saved", {
  expect_null(.step5_meanshift_display(NULL))
  expect_null(.step5_meanshift_display(NA_real_))
})
