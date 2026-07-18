# Unit tests for R/utils.R


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

test_that("utc_to_local_display passes unparseable input through, NA stays NA", {
  withr::local_timezone("UTC")
  expect_true(is.na(utc_to_local_display(NA_character_)))
  # 0.2.1 (L-05): unparseable strings are shown verbatim, not as "NA"
  expect_equal(utc_to_local_display("not-a-timestamp"), "not-a-timestamp")
})

test_that("utc_to_local_display does not parse the space-separated timestamps used by some test fixtures", {
  # Several fixtures elsewhere in this suite (e.g. test-history.R) use
  # space-separated timestamps like "2026-05-31 12:08:08" rather than the
  # production "T...Z" form. utc_to_local_display only understands the
  # production format — a space-separated string is NOT converted (no
  # timezone shift); since 0.2.1 it is passed through verbatim rather than
  # displayed as "NA". Pinning that behaviour here so a future fixture
  # rewrite that switches to production-format timestamps doesn't go
  # unnoticed (and so nobody "fixes" this function to also accept the space
  # form without realising real data never uses it).
  withr::local_timezone("UTC")
  expect_equal(utc_to_local_display("2026-05-31 12:08:08"), "2026-05-31 12:08:08")
})

# ── sanitize_r_name / suggest_col_names / csv_needs_naming (B-08) ─────────────

test_that("sanitize_r_name always yields a valid R name", {
  expect_true(is_valid_r_name(sanitize_r_name("Payee Name")))
  expect_equal(sanitize_r_name("Payee Name"), "Payee_Name")
  expect_equal(sanitize_r_name("123abc"),     "col_123abc")
  expect_equal(sanitize_r_name("$$$"),        "col")
  expect_equal(sanitize_r_name(" a-b/c "),    "a_b_c")
  expect_true(is_valid_r_name(sanitize_r_name("")))
})

test_that("suggest_col_names de-duplicates with positional suffixes", {
  s <- suggest_col_names(c("PayeeName", "Amount", "PayeeName", "PayeeName"))
  expect_equal(s$names, c("PayeeName", "Amount", "PayeeName_2", "PayeeName_3"))
  expect_equal(s$reason[1], "")               # first occurrence unchanged
  expect_match(s$reason[3], "duplicate of column 1")
  # every suggestion is valid and unique — the whole point
  expect_true(all(is_valid_r_name(s$names)))
  expect_false(any(duplicated(s$names)))
})

test_that("suggest_col_names sanitises invalid-but-unique names", {
  s <- suggest_col_names(c("Payee Name", "1st", "ok_name"))
  expect_equal(s$names, c("Payee_Name", "col_1st", "ok_name"))
  expect_match(s$reason[1], "sanitised")
  expect_equal(s$reason[3], "")
})

test_that("suggest_col_names breaks collisions created by suffixing", {
  # "a" duplicated → second becomes "a_2"; a literal "a_2" already present must
  # not collide.
  s <- suggest_col_names(c("a", "a", "a_2"))
  expect_false(any(duplicated(s$names)))
  expect_true(all(is_valid_r_name(s$names)))
})

test_that("csv_needs_naming triggers on duplicate/invalid headers only", {
  ok  <- list(csv_col_names_detected = c("id", "name"),
              raw_header_names       = c("id", "name"))
  dup <- list(csv_col_names_detected = c("id", "id"),
              raw_header_names       = c("id", "id"))
  bad <- list(csv_col_names_detected = c("id", "a b"),
              raw_header_names       = c("id", "a b"))
  expect_false(csv_needs_naming(ok,  has_header = TRUE))
  expect_true( csv_needs_naming(dup, has_header = TRUE))
  expect_true( csv_needs_naming(bad, has_header = TRUE))
  # headerless always needs naming when columns are detected
  expect_true( csv_needs_naming(ok,  has_header = FALSE))
})

# ── infra path resolution (deployment-root anchoring; the GUI history bug) ────

test_that("is_absolute_path recognises absolute vs relative paths", {
  expect_true(is_absolute_path("/var/data/snapshots.sqlite"))
  expect_true(is_absolute_path("~/data"))
  expect_false(is_absolute_path("data/snapshots.sqlite"))
  expect_false(is_absolute_path("reports/"))
  expect_false(is_absolute_path(""))
})

test_that("deployment_root is the parent of the config dir", {
  expect_equal(deployment_root("/srv/deploy/config"), "/srv/deploy")
})

test_that("resolve_infra_path anchors relative paths to the deployment root (not getwd)", {
  # This is the core of the GUI history bug: shiny::runApp() changes getwd() to
  # the installed app dir, so a relative snapshot_db must be anchored to the
  # deployment root (parent of config_dir), not the working directory.
  root    <- withr::local_tempdir()
  dir.create(file.path(root, "config"))
  dir.create(file.path(root, "data"))
  cfg_dir <- file.path(root, "config")
  got     <- resolve_infra_path("data/snapshots.sqlite", cfg_dir,
                                default = "data/snapshots.sqlite")
  expect_equal(got, normalizePath(file.path(root, "data", "snapshots.sqlite"),
                                  mustWork = FALSE))
})

test_that("resolve_infra_path leaves absolute paths unchanged", {
  abs <- withr::local_tempfile(fileext = ".sqlite")
  file.create(abs)
  expect_equal(resolve_infra_path(abs, "/some/config"),
               normalizePath(abs, mustWork = FALSE))
})

test_that("resolve_infra_path falls back to default when path is empty/NULL", {
  root    <- withr::local_tempdir()
  dir.create(file.path(root, "config"))
  cfg_dir <- file.path(root, "config")
  expect_equal(resolve_infra_path(NULL, cfg_dir, default = "data/snapshots.sqlite"),
               normalizePath(file.path(root, "data", "snapshots.sqlite"),
                             mustWork = FALSE))
})

# ── html_escape (G-06) ───────────────────────────────────────────────────────

test_that("html_escape neutralises HTML in text content", {
  expect_equal(html_escape("<img src=x onerror=alert(1)>.csv"),
               "&lt;img src=x onerror=alert(1)&gt;.csv")
  expect_equal(html_escape("a & b"), "a &amp; b")
  expect_equal(html_escape("plain_name.csv"), "plain_name.csv")
})

test_that("html_escape escapes quotes only in attribute mode", {
  expect_equal(html_escape('say "hi"'),                  'say "hi"')
  expect_equal(html_escape('say "hi"', attribute = TRUE), "say &quot;hi&quot;")
  expect_equal(html_escape("it's",     attribute = TRUE), "it&#39;s")
})

test_that("html_escape escapes ampersand first (no double-escaping)", {
  expect_equal(html_escape("&lt;"), "&amp;lt;")
})

test_that("html_escape is vectorised", {
  expect_equal(html_escape(c("<a>", "b")), c("&lt;a&gt;", "b"))
})

# ── url_encode_filename (G-06) ───────────────────────────────────────────────

test_that("url_encode_filename percent-encodes unsafe characters", {
  expect_equal(url_encode_filename("ds_20260704_101112.html"),
               "ds_20260704_101112.html")
  got <- url_encode_filename("a'b\"c<d>.html")
  expect_false(grepl("[\"'<>]", got))
})

test_that("url_encode_filename is vectorised and unnamed", {
  got <- url_encode_filename(c("a.html", "b c.html"))
  expect_equal(got, c("a.html", "b%20c.html"))
  expect_null(names(got))
})

# ── list_files_only (B-03 GUI copies) ────────────────────────────────────────

test_that("list_files_only excludes subdirectories", {
  d <- withr::local_tempdir()
  writeLines("x", file.path(d, "data.csv"))
  dir.create(file.path(d, "archive"))
  got <- list_files_only(d)
  expect_equal(basename(got), "data.csv")
})

test_that("list_files_only returns empty for empty or file-free folders", {
  d <- withr::local_tempdir()
  expect_length(list_files_only(d), 0)
  dir.create(file.path(d, "only_a_dir"))
  expect_length(list_files_only(d), 0)
})

# ── effective_db_path (G-01) ─────────────────────────────────────────────────

test_that("effective_db_path resolves the relative scaffold default against the deployment root", {
  root    <- withr::local_tempdir()
  cfg_dir <- file.path(root, "config")
  dir.create(cfg_dir)
  gcfg <- list(snapshot_db = "data/snapshots.sqlite")
  expect_equal(
    effective_db_path(cfg_dir, gcfg),
    normalizePath(file.path(root, "data", "snapshots.sqlite"), mustWork = FALSE)
  )
})

test_that("effective_db_path prefers a per-dataset override over the global path", {
  root    <- withr::local_tempdir()
  cfg_dir <- file.path(root, "config")
  dir.create(cfg_dir)
  gcfg <- list(snapshot_db = "data/global.sqlite")
  expect_equal(
    effective_db_path(cfg_dir, gcfg, ds_snapshot_db = "data/override.sqlite"),
    normalizePath(file.path(root, "data", "override.sqlite"), mustWork = FALSE)
  )
  # "" (read_config's absent marker) and NULL both fall through to global
  expect_equal(
    effective_db_path(cfg_dir, gcfg, ds_snapshot_db = ""),
    normalizePath(file.path(root, "data", "global.sqlite"), mustWork = FALSE)
  )
})

test_that("effective_db_path falls back to the standard default when nothing is configured", {
  root    <- withr::local_tempdir()
  cfg_dir <- file.path(root, "config")
  dir.create(cfg_dir)
  expect_equal(
    effective_db_path(cfg_dir, list()),
    normalizePath(file.path(root, "data", "snapshots.sqlite"), mustWork = FALSE)
  )
})

test_that("effective_db_path leaves absolute paths unchanged", {
  abs <- withr::local_tempfile(fileext = ".sqlite")
  file.create(abs)
  expect_equal(effective_db_path("/some/config", list(snapshot_db = abs)),
               normalizePath(abs, mustWork = FALSE))
})

# ── utc_to_local_display fallback (0.2.1, L-05) ──────────────────────────────

test_that("utc_to_local_display shows unparseable timestamps as-is, not 'NA'", {
  expect_equal(utc_to_local_display("2026/07/04 oddball"), "2026/07/04 oddball")
  good <- utc_to_local_display("2026-07-04T01:02:03Z")
  expect_false(is.na(good))
  expect_match(good, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$")
})

# ── infer_col_type_simple threshold (0.2.1, L-07) ────────────────────────────

test_that("infer_col_type_simple honours the threshold argument", {
  x <- c(rep("1", 8), "a", "b")           # 80% numeric
  expect_equal(infer_col_type_simple(x), "character")            # default 0.90
  expect_equal(infer_col_type_simple(x, threshold = 0.80), "numeric")
})

test_that("wiz_type_threshold resolves override > global > default", {
  gcfg <- list(default_rules = list(type_inference_threshold = 0.85))
  expect_equal(wiz_type_threshold(list(rule_overrides = list()), gcfg), 0.85)
  expect_equal(wiz_type_threshold(
    list(rule_overrides = list(type_inference_threshold = 0.7)), gcfg), 0.7)
  expect_equal(wiz_type_threshold(list(rule_overrides = list()), list()), 0.90)
})

# ── report_url_prefix (0.2.1, G-05) ──────────────────────────────────────────

test_that("report_url_prefix picks the per-dataset prefix only for overrides", {
  root    <- withr::local_tempdir()
  cfg_dir <- file.path(root, "config"); dir.create(cfg_dir)
  gcfg <- list(report_output_dir = "reports/")
  expect_equal(report_url_prefix("ds1", "",         cfg_dir, gcfg), "dq_reports")
  expect_equal(report_url_prefix("ds1", "reports/", cfg_dir, gcfg), "dq_reports")
  expect_equal(report_url_prefix("ds1", "other_reports/", cfg_dir, gcfg),
               "dq_reports_ds1")
})

# ── read_snapshot_history vs old-schema databases (0.2.1, G-05) ──────────────

test_that("read_snapshot_history defaults render_status/report_file for old DBs", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE snapshots (
    id INTEGER PRIMARY KEY, dataset_name TEXT, run_timestamp TEXT,
    file_name TEXT, row_count INTEGER, col_count INTEGER,
    check_pass_count INTEGER, check_warn_count INTEGER,
    check_fail_count INTEGER, check_info_count INTEGER, overall_status TEXT)")
  DBI::dbExecute(con, "INSERT INTO snapshots
    (dataset_name, run_timestamp, file_name, row_count, col_count,
     check_pass_count, check_warn_count, check_fail_count, check_info_count,
     overall_status)
    VALUES ('old_ds', '2026-01-01T00:00:00Z', 'f.csv', 10, 2, 5, 0, 0, 1, 'PASS')")
  DBI::dbDisconnect(con)

  df <- read_snapshot_history(db, "old_ds")
  expect_equal(nrow(df), 1)
  expect_equal(df$render_status, "success")
  expect_true(is.na(df$report_file))
})

# ── read_latest_statuses (0.2.1, P-06) ───────────────────────────────────────

test_that("read_latest_statuses returns one latest status per dataset", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE snapshots (
    id INTEGER PRIMARY KEY AUTOINCREMENT, dataset_name TEXT, overall_status TEXT)")
  for (row in list(c("ds_a","PASS"), c("ds_b","PASS"),
                   c("ds_a","FAIL"), c("ds_b","WARN"))) {
    DBI::dbExecute(con,
      "INSERT INTO snapshots (dataset_name, overall_status) VALUES (?, ?)",
      list(row[1], row[2]))
  }
  DBI::dbDisconnect(con)

  st <- read_latest_statuses(db)
  expect_equal(unname(st["ds_a"]), "FAIL")
  expect_equal(unname(st["ds_b"]), "WARN")
})

test_that("read_latest_statuses returns empty for missing databases", {
  expect_length(read_latest_statuses(tempfile(fileext = ".sqlite")), 0)
  expect_length(read_latest_statuses(""), 0)
  expect_length(read_latest_statuses(NULL), 0)
})

test_that("read_snapshot_history handles zero-row results from old-schema DBs cleanly", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(con, "CREATE TABLE snapshots (
    id INTEGER PRIMARY KEY, dataset_name TEXT, run_timestamp TEXT,
    file_name TEXT, row_count INTEGER, col_count INTEGER,
    check_pass_count INTEGER, check_warn_count INTEGER,
    check_fail_count INTEGER, check_info_count INTEGER, overall_status TEXT)")
  DBI::dbDisconnect(con)

  expect_no_message(df <- read_snapshot_history(db, "no_such_ds"))
  expect_equal(nrow(df), 0)
  expect_true(all(c("render_status", "report_file") %in% names(df)))
})

# ── drift_report_url (B-01 alignment) ────────────────────────────────────────
# dqcheckr >= 0.2.5 returns the drift report path directly; the GUI links to it
# instead of re-deriving the filename with a slug regex (which broke when the
# drift filename gained its snapshot ids).

test_that("drift_report_url builds the /dq_reports/ URL from report_path", {
  f <- withr::local_tempfile(fileext = ".html")
  writeLines("<html/>", f)
  # The new id-suffixed filename shape the old regex could not match:
  target <- file.path(dirname(f), "drift_sales_20260718_072345_1_2.html")
  file.rename(f, target)
  url <- drift_report_url(list(report_path = target))
  expect_equal(url, "/dq_reports/drift_sales_20260718_072345_1_2.html")
})

test_that("drift_report_url returns NULL when no report was written", {
  # report_path is NULL when report=FALSE / Quarto absent / render failed.
  expect_null(drift_report_url(list(report_path = NULL)))
  expect_null(drift_report_url(list()))                       # element absent
})

test_that("drift_report_url returns NULL when the file is not on disk", {
  expect_null(drift_report_url(list(report_path = "/no/such/drift_x_1_2.html")))
})

# ── report_link_cell (B-02: pending render_status alignment) ──────────────────

test_that("report_link_cell links to the stored report_file when present", {
  cell <- report_link_cell("sales_20260718_010203_7.html", "success",
                           "2026-07-18 01:02:03", "sales", "dq_reports")
  expect_match(cell, "sales_20260718_010203_7\\.html")
  expect_match(cell, "Open")
})

test_that("report_link_cell shows 'rendering…' for a pending row, no link (B-02)", {
  cell <- report_link_cell(NA_character_, "pending",
                           "2026-07-18 01:02:03", "sales", "dq_reports")
  expect_match(cell, "rendering")
  expect_false(grepl("Open", cell))
  expect_false(grepl("window.open", cell))     # no dead link to a not-yet-written file
})

test_that("report_link_cell shows 'render failed' for a failed row", {
  cell <- report_link_cell(NA_character_, "failed",
                           "2026-07-18 01:02:03", "sales", "dq_reports")
  expect_match(cell, "render failed")
  expect_false(grepl("Open", cell))
})

test_that("report_link_cell reconstructs the legacy slug only for a pre-0.2.3 success/NA row", {
  # render_status success but no file = genuine pre-0.2.3 row -> reconstruct.
  legacy <- report_link_cell(NA_character_, "success",
                             "2026-07-18 01:02:03", "sales", "dq_reports")
  expect_match(legacy, "sales_20260718_010203\\.html")   # id-less legacy name
  expect_match(legacy, "Open")
  # NA render_status (very old DB, pre render tracking) also reconstructs.
  na_row <- report_link_cell(NA_character_, NA_character_,
                             "2026-07-18 01:02:03", "sales", "dq_reports")
  expect_match(na_row, "Open")
})

test_that("report_link_cell is vectorised across a mix of rows", {
  cells <- report_link_cell(
    c("a_1.html", NA,        NA),
    c("success",  "pending", "failed"),
    rep("2026-07-18 01:02:03", 3), "sales", "dq_reports")
  expect_length(cells, 3)
  expect_match(cells[1], "a_1\\.html")
  expect_match(cells[2], "rendering")
  expect_match(cells[3], "render failed")
})

test_that("report_link_cell handles per-row prefixes and datasets (history panel, B-02)", {
  # The history table spans multiple datasets, each with its own URL prefix.
  cells <- report_link_cell(
    report_file   = c("a_1.html",   NA,        NA),
    render_status = c("success",    "pending", "failed"),
    run_timestamp = rep("2026-07-18 01:02:03", 3),
    dataset_name  = c("sales",      "costs",   "hr"),
    report_prefix = c("dq_reports", "costs_rp", "hr_rp"))
  expect_match(cells[1], "/dq_reports/a_1\\.html")
  expect_match(cells[2], "rendering")           # pending -> no dead link
  expect_false(grepl("Open", cells[2]))
  expect_match(cells[3], "render failed")
})

# ── merge_column_rule (B-04: preserve rules across a type re-inference) ───────

test_that("merge_column_rule keeps min/max/mean-shift when a numeric col re-infers as character (B-04)", {
  # The bug: reopening on a differently-shaped sample infers 'character', so the
  # min/max/mean-shift inputs never render (read NULL) and the saved rule is lost.
  saved <- list(min_value = 0, max_value = 1000,
                max_numeric_mean_shift_pct = 0.20)
  rules <- merge_column_rule("character", saved,
                             allowed_in = NULL, min_in = NULL, max_in = NULL,
                             meanshift_in = NULL, maxmiss_in = 0.05)
  expect_equal(rules$min_value, 0)                      # carried forward
  expect_equal(rules$max_value, 1000)
  expect_equal(rules$max_numeric_mean_shift_pct, 0.20)
  expect_null(rules$allowed_values)
})

test_that("merge_column_rule keeps allowed_values when a character col re-infers as numeric (B-04)", {
  saved <- list(allowed_values = c("GB", "US", "DE"))
  rules <- merge_column_rule("numeric", saved,
                             allowed_in = NULL,          # input not rendered
                             min_in = NA, max_in = NA, meanshift_in = NA,
                             maxmiss_in = 0.05)
  expect_equal(rules$allowed_values, c("GB", "US", "DE"))   # carried forward
  expect_null(rules$min_value)
})

test_that("merge_column_rule uses the rendered input when the type matches (authoritative)", {
  rules <- merge_column_rule("numeric", list(min_value = 999),   # saved ignored
                             min_in = 5, max_in = 10, meanshift_in = 20,
                             maxmiss_in = 0.05)
  expect_equal(rules$min_value, 5)
  expect_equal(rules$max_value, 10)
  expect_equal(rules$max_numeric_mean_shift_pct, 0.20)          # 20% -> 0.20
})

test_that("merge_column_rule respects a user clearing a rendered input", {
  # numeric col, min input cleared (NA) -> the rule is dropped, not resurrected
  # from saved, because the input WAS rendered and is authoritative.
  rules <- merge_column_rule("numeric", list(min_value = 5),
                             min_in = NA, max_in = NA, meanshift_in = NA,
                             maxmiss_in = 0.05)
  expect_null(rules$min_value)
})

test_that("merge_column_rule collects the always-rendered pattern and missing-rate", {
  rules <- merge_column_rule("character", NULL,
                             allowed_in = "A", pattern_in = "^[A-Z]$",
                             maxmiss_in = 0.1)
  expect_equal(rules$allowed_values, "A")
  expect_equal(rules$pattern, "^[A-Z]$")
  expect_equal(rules$max_missing_rate, 0.1)
})
