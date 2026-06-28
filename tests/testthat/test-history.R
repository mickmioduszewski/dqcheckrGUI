# Unit tests for history utilities and report URL construction

library(shiny)  # attaches `tags` to the search path — needed by status_badge()

# ── Test DB helper ────────────────────────────────────────────────────────────

make_test_snapshot_db <- function(envir = parent.frame()) {
  tmp <- tempfile(fileext = ".sqlite")
  withr::defer(unlink(tmp), envir = envir)
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbExecute(con, "
    CREATE TABLE snapshots (
      id INTEGER PRIMARY KEY,
      dataset_name TEXT,
      file_name TEXT,
      run_timestamp TEXT,
      overall_status TEXT,
      check_fail_count INTEGER,
      check_warn_count INTEGER,
      row_count INTEGER
    )")
  DBI::dbExecute(con, "
    INSERT INTO snapshots VALUES
      (1, 'ds_a', 'file_a1.csv', '2026-05-31 10:00:00', 'PASS', 0, 0, 100),
      (2, 'ds_a', 'file_a2.csv', '2026-05-31 11:00:00', 'FAIL', 2, 1,  98),
      (3, 'ds_b', 'file_b1.csv', '2026-05-31 12:00:00', 'WARN', 0, 1, 200),
      (4, 'ds_b', 'file_b2.csv', '2026-05-31 13:00:00', 'PASS', 0, 0, 202)")
  DBI::dbDisconnect(con)
  tmp
}

# ── read_snapshot_history: bad paths ─────────────────────────────────────────

test_that("read_snapshot_history returns empty data frame for missing db path", {
  result <- read_snapshot_history("/no/such/path.sqlite", n = 10)
  expect_equal(nrow(result), 0L)
  expect_true("id" %in% names(result))
  expect_true("dataset_name" %in% names(result))
})

test_that("read_snapshot_history returns empty data frame for empty string path", {
  result <- read_snapshot_history("", n = 10)
  expect_equal(nrow(result), 0L)
})

test_that("read_snapshot_history returns empty data frame for NULL path", {
  result <- read_snapshot_history(NULL, n = 10)
  expect_equal(nrow(result), 0L)
})

# ── read_snapshot_history: correct data ───────────────────────────────────────

test_that("read_snapshot_history returns all rows when dataset_name is NULL", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, dataset_name = NULL, n = 100)
  expect_equal(nrow(result), 4L)
})

test_that("read_snapshot_history filters correctly by dataset_name", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, dataset_name = "ds_a", n = 100)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$dataset_name == "ds_a"))
})

test_that("read_snapshot_history returns empty frame for unknown dataset_name", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, dataset_name = "no_such_ds", n = 100)
  expect_equal(nrow(result), 0L)
})

test_that("read_snapshot_history respects n limit", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, dataset_name = NULL, n = 2)
  expect_equal(nrow(result), 2L)
})

test_that("read_snapshot_history returns rows in descending id order", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, dataset_name = NULL, n = 100)
  expect_equal(result$id, c(4L, 3L, 2L, 1L))
})

test_that("read_snapshot_history n limit returns the most recent rows (highest ids)", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, dataset_name = NULL, n = 2)
  # With DESC + LIMIT 2 we expect the two highest ids
  expect_equal(sort(result$id), c(3L, 4L))
})

test_that("read_snapshot_history returns correct column names", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, n = 1)
  expected_cols <- c("id", "dataset_name", "file_name", "run_timestamp",
                     "overall_status", "check_fail_count", "check_warn_count", "row_count")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("read_snapshot_history returns correct field values", {
  db <- make_test_snapshot_db()
  result <- read_snapshot_history(db, dataset_name = "ds_a", n = 100)
  # The most recent ds_a row (id=2) should be first
  expect_equal(result$id[1], 2L)
  expect_equal(result$overall_status[1], "FAIL")
  expect_equal(result$check_fail_count[1], 2L)
  expect_equal(result$row_count[1], 98L)
})

# ── read_all_snapshot_history ─────────────────────────────────────────────────

test_that("read_all_snapshot_history returns all datasets without filter", {
  db <- make_test_snapshot_db()
  result <- read_all_snapshot_history(db, n = 100)
  expect_equal(nrow(result), 4L)
  expect_true("ds_a" %in% result$dataset_name)
  expect_true("ds_b" %in% result$dataset_name)
})

test_that("read_all_snapshot_history returns empty frame for missing db", {
  result <- read_all_snapshot_history("/no/such.sqlite", n = 100)
  expect_equal(nrow(result), 0L)
})

# ── Report filename slug construction ─────────────────────────────────────────
# make_report_filename() lives in utils.R and is loaded by setup.R.
# A regression here silently breaks all Open links in server_history.R and ui_datasets.R.

test_that("filename slug produces correct name for standard timestamp", {
  expect_equal(
    make_report_filename("RBB_bonds", "2026-05-31 12:08:08"),
    "RBB_bonds_20260531_120808.html"
  )
})

test_that("filename slug works with T-separator timestamps", {
  expect_equal(
    make_report_filename("my_ds", "2026-05-31T14:28:26"),
    "my_ds_20260531_142826.html"
  )
})

test_that("filename slug works for midnight (all-zero time)", {
  expect_equal(
    make_report_filename("dataset", "2026-01-01 00:00:00"),
    "dataset_20260101_000000.html"
  )
})

test_that("filename slug truncates sub-second precision correctly", {
  # run_dq_check stores timestamps to the second; sub-seconds must be dropped
  expect_equal(
    make_report_filename("ds", "2026-05-31 15:04:34.123456"),
    "ds_20260531_150434.html"
  )
})

test_that("filename slug strips all non-digit characters regardless of separator style", {
  ts_dash  <- "2026-05-31 14:32:21"
  ts_slash <- "2026/05/31 14:32:21"
  ts_T     <- "2026-05-31T14:32:21"
  expect_equal(make_report_filename("ds", ts_dash),  "ds_20260531_143221.html")
  expect_equal(make_report_filename("ds", ts_slash), "ds_20260531_143221.html")
  expect_equal(make_report_filename("ds", ts_T),     "ds_20260531_143221.html")
})

test_that("filename slug handles dataset names with underscores", {
  expect_equal(
    make_report_filename("starwars_folder", "2026-05-31 11:02:03"),
    "starwars_folder_20260531_110203.html"
  )
})

test_that("filename slug vectorises correctly over multiple rows", {
  timestamps <- c("2026-05-31 12:08:08", "2026-05-31 14:28:26")
  ts_raw  <- gsub("[^0-9]", "", substr(timestamps, 1, 19))
  ts_slug <- paste0(substr(ts_raw, 1, 8), "_", substr(ts_raw, 9, 14))
  filenames <- paste0("RBB_bonds_", ts_slug, ".html")
  expect_equal(filenames,
               c("RBB_bonds_20260531_120808.html", "RBB_bonds_20260531_142826.html"))
})

# ── status_badge (htmltools version) ─────────────────────────────────────────

test_that("status_badge returns an htmltools tag for PASS", {
  result <- status_badge("PASS")
  expect_s3_class(result, "shiny.tag")
  html_str <- as.character(result)
  expect_match(html_str, "PASS")
  expect_match(html_str, "#5cb85c")   # green
})

test_that("status_badge returns correct colour for FAIL", {
  html_str <- as.character(status_badge("FAIL"))
  expect_match(html_str, "FAIL")
  expect_match(html_str, "#d9534f")   # red
})

test_that("status_badge returns correct colour for WARN", {
  html_str <- as.character(status_badge("WARN"))
  expect_match(html_str, "WARN")
  expect_match(html_str, "#f0ad4e")   # amber
})

test_that("status_badge returns correct colour for RUNNING", {
  html_str <- as.character(status_badge("RUNNING"))
  expect_match(html_str, "RUNNING")
  expect_match(html_str, "#337ab7")   # blue
})

test_that("status_badge returns a grey badge for unknown status", {
  html_str <- as.character(status_badge("UNKNOWN"))
  expect_match(html_str, "#999999")
})
