# Shared utilities

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && a[1] != "") a else b

make_report_filename <- function(dataset_name, run_timestamp) {
  ts_raw  <- gsub("[^0-9]", "", substr(run_timestamp, 1, 19))
  ts_slug <- paste0(substr(ts_raw, 1, 8), "_", substr(ts_raw, 9, 14))
  paste0(dataset_name, "_", ts_slug, ".html")
}

# Escape a string for safe embedding inside a single-quoted JavaScript string
# literal — e.g. the dataset name interpolated into
# onclick="Shiny.setInputValue('ds_action', {ds:'<name>', ...})". Backslashes
# are escaped first so the backslash introduced for an escaped quote isn't
# itself re-escaped by the second gsub. `is_valid_r_name()` keeps dataset
# names alnum/underscore-only when created via the wizard, but names are
# read back from filenames on disk (list_dataset_configs()) without
# re-validation, so a manually placed/renamed config could carry characters
# that would otherwise break out of the JS string literal.
js_string_escape <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  gsub("'", "\\\\'", x)
}

# Minimal HTML escaper for text interpolated into raw HTML strings (DT columns
# rendered with escape = FALSE, hand-built attribute values). file_name comes
# from externally supplied deliveries, so it must never reach the DOM
# unescaped; dataset names read back from disk filenames are also untrusted.
html_escape <- function(x, attribute = FALSE) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  if (attribute) {
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;",  x, fixed = TRUE)
  }
  x
}

# Percent-encode a report filename for use inside a URL embedded in raw HTML.
# Vectorised wrapper (utils::URLencode is scalar-only). reserved = TRUE also
# encodes quotes and angle brackets, so the result is safe in both the JS
# string and the surrounding HTML attribute.
url_encode_filename <- function(x) {
  vapply(x, utils::URLencode, character(1), reserved = TRUE, USE.NAMES = FALSE)
}

# List files (not subdirectories) in a folder. list.files() includes
# directories in a non-recursive listing, so folder-scan previews sorted by
# mtime could otherwise pick a directory as the "current file" (mirrors the
# detect_files() fix in dqcheckr).
list_files_only <- function(path) {
  files <- list.files(path, full.names = TRUE)
  files[!dir.exists(files)]
}

# Effective snapshot DB path for a dataset: per-dataset override, then the
# global config, then the standard default — always resolved against the
# deployment root. Raw relative paths (e.g. the "data/snapshots.sqlite" the
# first-run scaffold writes) never resolve from the Shiny process's getwd(),
# which shiny::runApp() has pointed at the installed app directory.
effective_db_path <- function(config_dir, gcfg, ds_snapshot_db = NULL) {
  resolve_infra_path(ds_snapshot_db %||% gcfg$snapshot_db, config_dir,
                     default = "data/snapshots.sqlite", mustWork = FALSE)
}

.status_cfg <- function(status) {
  switch(as.character(status),
    PASS    = list(bg="#5cb85c", sym="✓", text="PASS"),
    WARN    = list(bg="#f0ad4e", sym="⚠", text="WARN"),
    FAIL    = list(bg="#d9534f", sym="✗", text="FAIL"),
    RUNNING = list(bg="#337ab7", sym="●", text="RUNNING"),
    list(bg="#999999", sym="—", text=as.character(status %||% ""))
  )
}

status_badge <- function(status) {
  cfg <- .status_cfg(status)
  tags$span(
    style=sprintf("background:%s;color:#fff;padding:2px 8px;border-radius:4px;font-size:12px;font-weight:600;white-space:nowrap;", cfg$bg),
    paste(cfg$sym, cfg$text)
  )
}

status_badge_html <- function(status) {
  cfg <- .status_cfg(status)
  sprintf('<span style="background:%s;color:#fff;padding:2px 8px;border-radius:4px;font-size:12px;font-weight:600;">%s %s</span>',
          cfg$bg, cfg$sym, cfg$text)
}

safe_dir_exists <- function(path) {
  tryCatch(isTRUE(dir.exists(path)), error = function(e) FALSE)
}

safe_file_exists <- function(path) {
  tryCatch(isTRUE(file.exists(path)), error = function(e) FALSE)
}

is_valid_r_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9_]*$", x)
}

# Coerce an arbitrary header token into a syntactically valid R name:
# non-word chars → "_", collapse/trim underscores, prefix "col_" if it does
# not start with a letter. Always returns something passing is_valid_r_name().
sanitize_r_name <- function(x) {
  x <- trimws(x %||% "")
  x <- gsub("[^A-Za-z0-9_]", "_", x)
  x <- gsub("_+", "_", x)
  x <- sub("^_+", "", x)
  x <- sub("_+$", "", x)
  if (x == "" || !grepl("^[A-Za-z]", x)) x <- paste0("col_", x)
  x <- sub("_+$", "", x)
  if (x == "" || x == "col_") x <- "col"
  x
}

# Given the raw (unmangled) header names of a CSV, produce a parallel list of
# valid, unique suggestions plus a human-readable reason for each change.
# Duplicate names get a positional suffix in first-appearance order
# (1st → base, 2nd → base_2, 3rd → base_3); names invalid for other reasons
# are sanitised. A final pass guarantees no suggestion collides with another.
suggest_col_names <- function(raw_names) {
  raw_names <- as.character(raw_names)
  n <- length(raw_names)
  out    <- character(n)
  reason <- character(n)
  taken  <- character(0)
  for (i in seq_len(n)) {
    raw  <- raw_names[i]
    base <- sanitize_r_name(raw)
    why  <- if (!identical(base, raw)) "invalid name — sanitised" else ""
    occ  <- sum(raw_names[seq_len(i)] == raw)   # 1 for first, 2 for second, ...
    cand <- base
    if (occ > 1L) {
      cand <- paste0(base, "_", occ)
      why  <- sprintf("duplicate of column %d — suggested %s",
                      match(raw, raw_names), cand)
    }
    bump <- occ
    while (cand %in% taken) {
      bump <- bump + 1L
      cand <- paste0(base, "_", bump)
      if (why == "") why <- sprintf("name collision — suggested %s", cand)
    }
    out[i]    <- cand
    reason[i] <- why
    taken     <- c(taken, cand)
  }
  list(names = out, reason = reason)
}

# Does the CSV at step 3 need the column-naming editor shown?
# - headerless files: always (names are placeholders the user should set)
# - header files: only when a raw header name is invalid or duplicated
csv_needs_naming <- function(wiz, has_header) {
  if (!isTRUE(has_header)) return(length(wiz$csv_col_names_detected) > 0)
  raw <- wiz$raw_header_names
  if (length(raw) == 0) return(FALSE)
  any(!is_valid_r_name(raw)) || any(duplicated(raw))
}

list_dataset_configs <- function(config_dir) {
  if (!safe_dir_exists(config_dir)) return(character(0))
  files <- list.files(config_dir, pattern="\\.yml$", full.names=FALSE)
  files <- files[files != "dqcheckr.yml"]
  sort(tools::file_path_sans_ext(files))
}

read_snapshot_history <- function(db_path, dataset_name = NULL, n = 10) {
  empty <- data.frame(
    id=integer(0), dataset_name=character(0), file_name=character(0),
    run_timestamp=character(0), overall_status=character(0),
    check_fail_count=integer(0), check_warn_count=integer(0),
    row_count=integer(0), render_status=character(0),
    report_file=character(0), stringsAsFactors=FALSE
  )
  if (is.null(db_path) || db_path == "" || !safe_file_exists(db_path)) return(empty)

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add=TRUE)

    # SELECT * rather than an explicit column list: databases written by
    # older dqcheckr versions lack the newer columns (render_status,
    # report_file), and naming them would error the whole query into the
    # empty fallback. Missing ones are defaulted below instead.
    df <- if (!is.null(dataset_name) && dataset_name != "") {
      DBI::dbGetQuery(con,
        "SELECT * FROM snapshots WHERE dataset_name = ? ORDER BY id DESC LIMIT ?",
        list(dataset_name, as.integer(n)))
    } else {
      DBI::dbGetQuery(con,
        "SELECT * FROM snapshots ORDER BY id DESC LIMIT ?",
        list(as.integer(n)))
    }
    # rep(): a plain scalar assignment errors on zero-row results
    if (is.null(df$render_status)) df$render_status <- rep("success", nrow(df))
    if (is.null(df$report_file))   df$report_file   <- rep(NA_character_, nrow(df))
    df
  }, error = function(e) {
    message("read_snapshot_history: query failed for db_path '", db_path,
            "': ", conditionMessage(e))
    empty
  })
}

# Latest run status per dataset in ONE query (the sidebar previously opened a
# connection and ran a query per dataset on every redraw). Returns a named
# character vector: dataset_name -> overall_status.
read_latest_statuses <- function(db_path) {
  if (is.null(db_path) || db_path == "" || !safe_file_exists(db_path))
    return(character(0))
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add=TRUE)
    df <- DBI::dbGetQuery(con,
      "SELECT dataset_name, overall_status FROM snapshots
       WHERE id IN (SELECT MAX(id) FROM snapshots GROUP BY dataset_name)")
    stats::setNames(df$overall_status, df$dataset_name)
  }, error = function(e) character(0))
}

read_all_snapshot_history <- function(db_path, n = 200) {
  read_snapshot_history(db_path, dataset_name=NULL, n=n)
}

# Infer column types from a character vector sample (dqcheckr logic).
# threshold mirrors dqcheckr's type_inference_threshold so wizard previews
# agree with run-time classification when a project overrides the default.
infer_col_type_simple <- function(x, threshold = 0.90) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return("unknown")
  date_fmts <- c("%Y-%m-%d","%d/%m/%Y","%m/%d/%Y","%Y%m%d","%d-%m-%Y")
  for (fmt in date_fmts) {
    parsed <- suppressWarnings(as.Date(x, format=fmt))
    if (all(!is.na(parsed))) return("date")
  }
  numeric_ok <- suppressWarnings(!is.na(as.numeric(x)))
  if (mean(numeric_ok) >= threshold) return("numeric")
  "character"
}

# Generate ruler string for N characters
make_ruler_string <- function(max_chars = 120) {
  ruler <- rep(" ", max_chars)
  for (i in seq(5, max_chars, by=5)) {
    label <- as.character(i)
    pos <- i
    for (j in seq_along(strsplit(label,"")[[1]])) {
      idx <- pos - nchar(label) + j
      if (idx >= 1 && idx <= max_chars) ruler[idx] <- substr(label, j, j)
    }
    if (pos <= max_chars) ruler[pos] <- "|"
  }
  paste(c("|", ruler), collapse="")
}

global_config_path <- function(config_dir) {
  file.path(config_dir, "dqcheckr.yml")
}

# TRUE if `p` is an absolute path: unix "/" or "~", a Windows drive ("C:\" /
# "C:/"), or a UNC share ("\\server").
is_absolute_path <- function(p) {
  grepl("^(/|~|[A-Za-z]:[\\\\/]|\\\\\\\\)", p %||% "")
}

# Deployment root — the directory that relative infra paths (`snapshot_db`,
# `report_output_dir`) are based on. dqcheckr's CLI resolves those relative to
# the directory it is run from, which by convention is the deployment root that
# contains config/, data/ and reports/; `config_dir` is `<root>/config`. The GUI
# must NOT use getwd() for this, because shiny::runApp() changes the working
# directory to the installed app folder — so we anchor to the parent of the
# config directory instead. (See also the matching `wd =` on the callr runs.)
deployment_root <- function(config_dir) {
  dirname(config_dir %||% ".")
}

# Resolve a (possibly relative) infra path against the deployment root. Absolute
# paths are returned unchanged, so an absolute snapshot_db/report_output_dir in
# the config also works.
resolve_infra_path <- function(path, config_dir, default = NULL, mustWork = FALSE) {
  path <- path %||% default %||% ""
  if (nchar(path) == 0) return("")
  if (!is_absolute_path(path)) path <- file.path(deployment_root(config_dir), path)
  normalizePath(path, mustWork = mustWork)
}

# Register (or re-register) the reports directory as the "dq_reports" static
# resource path. Re-registering with a new path replaces the old mapping, so
# this must be called again whenever report_output_dir changes at runtime.
register_report_resource_path <- function(report_output_dir, config_dir) {
  report_dir <- resolve_infra_path(report_output_dir, config_dir,
                                   default = "reports/", mustWork = FALSE)
  if (nchar(report_dir) > 0 && dir.exists(report_dir)) {
    addResourcePath("dq_reports", report_dir)
  }
}

# URL prefix a dataset's report links must use: "dq_reports" when the dataset
# writes to the global report dir, "dq_reports_<ds>" when it has its own
# report_output_dir override (each override dir gets its own resource path —
# a single static prefix can only map one directory).
report_url_prefix <- function(ds, ds_report_dir, config_dir, gcfg) {
  global_dir <- resolve_infra_path(gcfg$report_output_dir, config_dir,
                                   default = "reports/", mustWork = FALSE)
  ds_dir <- if (nchar(ds_report_dir %||% "") > 0)
    resolve_infra_path(ds_report_dir, config_dir, mustWork = FALSE)
  else ""
  if (nzchar(ds_dir) && !identical(ds_dir, global_dir))
    paste0("dq_reports_", ds)
  else
    "dq_reports"
}

# Register the global reports dir plus one resource path per dataset whose
# effective report dir differs from it. Called at app startup, after a global
# config save, after a wizard save, and after each completed run — the last
# also covers the case where reports/ did not exist at launch and was first
# created by dqcheckr itself (registration is skipped for missing dirs).
register_all_report_paths <- function(config_dir, gcfg) {
  register_report_resource_path(gcfg$report_output_dir, config_dir)
  global_dir <- resolve_infra_path(gcfg$report_output_dir, config_dir,
                                   default = "reports/", mustWork = FALSE)
  for (ds in list_dataset_configs(config_dir)) {
    known <- tryCatch(
      read_config(file.path(config_dir, paste0(ds, ".yml")))$known,
      error = function(e) NULL
    )
    if (is.null(known) || nchar(known$report_output_dir %||% "") == 0) next
    ds_dir <- resolve_infra_path(known$report_output_dir, config_dir,
                                 mustWork = FALSE)
    if (nzchar(ds_dir) && !identical(ds_dir, global_dir) && dir.exists(ds_dir))
      addResourcePath(paste0("dq_reports_", ds), ds_dir)
  }
}

# Effective type-inference threshold for wizard-side previews: the dataset's
# step-6 override, then the global default, then dqcheckr's 0.90.
wiz_type_threshold <- function(wiz, gcfg) {
  wiz$rule_overrides$type_inference_threshold %||%
    gcfg$default_rules$type_inference_threshold %||% 0.90
}

# Convert a UTC ISO timestamp (from the snapshot DB) to a local-time display
# string. Timestamps that don't parse (legacy/foreign formats) are shown
# as-is rather than as the string "NA".
utc_to_local_display <- function(ts) {
  parsed <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out <- format(parsed, format = "%Y-%m-%d %H:%M:%S", tz = "")
  ifelse(is.na(parsed), as.character(ts), out)
}
