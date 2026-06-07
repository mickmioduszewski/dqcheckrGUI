# Unit tests for R/utils.R

library(testthat)

# ── %||% ─────────────────────────────────────────────────────────────────────

test_that("%||% returns left when it has a real value", {
  expect_equal("hello"   %||% "default", "hello")
  expect_equal(42L       %||% 0L,        42L)
  expect_equal(FALSE     %||% TRUE,      FALSE)
})

test_that("%||% returns right when left is NULL", {
  expect_equal(NULL %||% "default", "default")
  expect_equal(NULL %||% 0,         0)
})

test_that("%||% returns right when left is NA", {
  expect_equal(NA      %||% "default", "default")
  expect_equal(NA_real_ %||% 99,       99)
})

test_that("%||% returns right when left is empty string", {
  expect_equal("" %||% "fallback", "fallback")
})

test_that("%||% returns right when left is zero-length vector", {
  expect_equal(character(0) %||% "x", "x")
})

# ── is_valid_r_name ───────────────────────────────────────────────────────────

test_that("is_valid_r_name accepts valid single-word names", {
  expect_true(is_valid_r_name("dataset"))
  expect_true(is_valid_r_name("MyDataset"))
  expect_true(is_valid_r_name("dataset_1"))
  expect_true(is_valid_r_name("x"))
  expect_true(is_valid_r_name("CamelCase123"))
  expect_true(is_valid_r_name("with_under_score"))
})

test_that("is_valid_r_name rejects names starting with a digit", {
  expect_false(is_valid_r_name("1dataset"))
  expect_false(is_valid_r_name("123"))
  expect_false(is_valid_r_name("0x"))
})

test_that("is_valid_r_name rejects names with hyphens", {
  expect_false(is_valid_r_name("my-dataset"))
  expect_false(is_valid_r_name("a-b"))
})

test_that("is_valid_r_name rejects names with spaces", {
  expect_false(is_valid_r_name("my dataset"))
  expect_false(is_valid_r_name(" leading"))
})

test_that("is_valid_r_name rejects names with dots (UI hint says letters/numbers/underscores only)", {
  expect_false(is_valid_r_name("my.dataset"))
})

test_that("is_valid_r_name rejects empty string", {
  expect_false(is_valid_r_name(""))
})

# ── list_dataset_configs ──────────────────────────────────────────────────────

test_that("list_dataset_configs returns names in alphabetical order", {
  dir <- withr::local_tempdir()
  # Create files in non-alphabetical order
  file.create(file.path(dir, "zebra.yml"))
  file.create(file.path(dir, "alpha.yml"))
  file.create(file.path(dir, "mango.yml"))
  file.create(file.path(dir, "beta.yml"))

  result <- list_dataset_configs(dir)
  expect_equal(result, c("alpha", "beta", "mango", "zebra"))
})

test_that("list_dataset_configs excludes dqcheckr.yml", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "my_ds.yml"))
  file.create(file.path(dir, "dqcheckr.yml"))

  result <- list_dataset_configs(dir)
  expect_equal(result, "my_ds")
  expect_false("dqcheckr" %in% result)
})

test_that("list_dataset_configs returns character(0) for empty directory", {
  dir <- withr::local_tempdir()
  expect_equal(list_dataset_configs(dir), character(0))
})

test_that("list_dataset_configs returns character(0) for non-existent path", {
  expect_equal(list_dataset_configs("/this/path/does/not/exist/xyz123"), character(0))
})

test_that("list_dataset_configs ignores non-yml files", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "dataset.yml"))
  file.create(file.path(dir, "notes.txt"))
  file.create(file.path(dir, "data.csv"))

  result <- list_dataset_configs(dir)
  expect_equal(result, "dataset")
})

test_that("list_dataset_configs result is consistently sorted regardless of filesystem order", {
  dir <- withr::local_tempdir()
  # Create in reverse alphabetical order
  for (nm in rev(c("aardvark", "elephant", "mongoose", "zebra"))) {
    file.create(file.path(dir, paste0(nm, ".yml")))
    Sys.sleep(0.01)  # ensure different mtime
  }
  result <- list_dataset_configs(dir)
  expect_equal(result, c("aardvark", "elephant", "mongoose", "zebra"))
})

# ── infer_col_type_simple ─────────────────────────────────────────────────────

test_that("infer_col_type_simple detects numeric columns", {
  expect_equal(infer_col_type_simple(c("1", "2.5", "100", "-3.14")), "numeric")
})

test_that("infer_col_type_simple detects numeric with sparse NAs", {
  # 90% threshold — 9 out of 10 valid
  x <- c("1","2","3","4","5","6","7","8","9","not_a_number")
  expect_equal(infer_col_type_simple(x), "numeric")
})

test_that("infer_col_type_simple detects ISO date columns", {
  expect_equal(infer_col_type_simple(c("2024-01-01","2024-06-15","2023-12-31")), "date")
})

test_that("infer_col_type_simple detects d/m/Y date format", {
  expect_equal(infer_col_type_simple(c("01/01/2024","15/06/2024","31/12/2023")), "date")
})

test_that("infer_col_type_simple returns character for text columns", {
  expect_equal(infer_col_type_simple(c("Alice","Bob","Carol","Dave")), "character")
})

test_that("infer_col_type_simple returns unknown for all-NA input", {
  expect_equal(infer_col_type_simple(c(NA, NA, NA)), "unknown")
})

test_that("infer_col_type_simple returns unknown for zero-length input", {
  expect_equal(infer_col_type_simple(character(0)), "unknown")
})

test_that("infer_col_type_simple ignores NA and empty string when inferring", {
  # Should still detect numeric if non-empty values are numeric
  expect_equal(infer_col_type_simple(c("1", "2", NA, "", "3")), "numeric")
})

# ── safe_file_exists / safe_dir_exists ───────────────────────────────────────

test_that("safe_file_exists returns TRUE for existing file", {
  f <- tempfile()
  file.create(f)
  on.exit(unlink(f))
  expect_true(safe_file_exists(f))
})

test_that("safe_file_exists returns FALSE for missing file", {
  expect_false(safe_file_exists("/this/does/not/exist/xyz.csv"))
})

test_that("safe_file_exists returns FALSE for NULL input", {
  expect_false(safe_file_exists(NULL))
})

test_that("safe_dir_exists returns TRUE for existing directory", {
  expect_true(safe_dir_exists(tempdir()))
})

test_that("safe_dir_exists returns FALSE for missing directory", {
  expect_false(safe_dir_exists("/no/such/directory/xyz"))
})

# ── make_ruler_string ─────────────────────────────────────────────────────────

test_that("make_ruler_string starts with a pipe character", {
  expect_true(startsWith(make_ruler_string(80), "|"))
})

test_that("make_ruler_string has length max_chars + 1 (leading pipe)", {
  expect_equal(nchar(make_ruler_string(80)),  81L)
  expect_equal(nchar(make_ruler_string(120)), 121L)
  expect_equal(nchar(make_ruler_string(40)),  41L)
})

test_that("make_ruler_string has pipe markers at every 5th character position", {
  # The ruler writes digits left-aligned before each pipe; the last digit of
  # multi-digit labels is overwritten by "|" (e.g. "1|" at position 10, not "10").
  # Test structural properties rather than literal substrings.
  ruler <- make_ruler_string(60)
  chars <- strsplit(ruler, "")[[1]]

  # Leading pipe at index 1
  expect_equal(chars[1], "|")

  # Pipe markers fall at string indices 6, 11, 16, 21 ... (char positions 5, 10, 15, 20 ...)
  pipe_idx <- which(chars == "|")
  expect_true(6  %in% pipe_idx, info = "pipe expected at char position 5")
  expect_true(11 %in% pipe_idx, info = "pipe expected at char position 10")
  expect_true(21 %in% pipe_idx, info = "pipe expected at char position 20")
  expect_true(51 %in% pipe_idx, info = "pipe expected at char position 50")

  # At least one numeric digit appears in the ruler
  expect_true(any(chars %in% as.character(1:9)))
})

# ── status_badge_html ─────────────────────────────────────────────────────────

test_that("status_badge_html returns HTML string for known statuses", {
  expect_match(status_badge_html("PASS"), "PASS")
  expect_match(status_badge_html("FAIL"), "FAIL")
  expect_match(status_badge_html("WARN"), "WARN")
})

test_that("status_badge_html returns HTML span for unknown status", {
  result <- status_badge_html("UNKNOWN_STATUS")
  expect_match(result, "<span")
})

# ── utc_to_local_display ──────────────────────────────────────────────────────
#
# Production (dqr/dqcheckr/R/snapshot.R:211) writes run_timestamp to the
# snapshot DB as format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), e.g.
# "2026-05-31T12:08:08Z". utc_to_local_display() parses *that* format with
# as.POSIXct(..., tz = "UTC") and re-renders it in the session's local
# timezone. Because the result depends on Sys.timezone(), each test fixes
# the timezone with withr::local_timezone() so the expected string is
# deterministic regardless of where the suite runs (and so DST edge cases
# can be exercised deliberately).

test_that("utc_to_local_display converts a production-format timestamp to local time (no DST)", {
  withr::local_timezone("Australia/Sydney")
  # 2026-06-01 falls in Sydney's non-DST period (AEST, UTC+10)
  expect_equal(utc_to_local_display("2026-06-01T00:00:00Z"), "2026-06-01 10:00:00")
})

test_that("utc_to_local_display converts a production-format timestamp to local time (DST)", {
  withr::local_timezone("America/New_York")
  # 2026-06-01 falls in New York's DST period (EDT, UTC-4)
  expect_equal(utc_to_local_display("2026-06-01T12:00:00Z"), "2026-06-01 08:00:00")
})

test_that("utc_to_local_display vectorises over multiple timestamps", {
  withr::local_timezone("UTC")
  result <- utc_to_local_display(c("2026-05-31T12:08:08Z", "2026-06-01T00:00:00Z"))
  expect_equal(result, c("2026-05-31 12:08:08", "2026-06-01 00:00:00"))
})

test_that("utc_to_local_display returns NA for missing or unparseable input", {
  withr::local_timezone("UTC")
  expect_true(is.na(utc_to_local_display(NA_character_)))
  expect_true(is.na(utc_to_local_display("not-a-timestamp")))
})

test_that("utc_to_local_display does not parse the space-separated timestamps used by some test fixtures", {
  # Several fixtures elsewhere in this suite (e.g. test-history.R) use
  # space-separated timestamps like "2026-05-31 12:08:08" rather than the
  # production "T...Z" form. utc_to_local_display only understands the
  # production format — a space-separated string silently parses to NA
  # rather than producing a converted display string. Pinning that behaviour
  # here so a future fixture rewrite that switches to production-format
  # timestamps doesn't go unnoticed (and so nobody "fixes" this function to
  # also accept the space form without realising real data never uses it).
  withr::local_timezone("UTC")
  expect_true(is.na(utc_to_local_display("2026-05-31 12:08:08")))
})
