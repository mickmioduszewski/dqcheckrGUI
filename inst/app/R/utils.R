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
    row_count=integer(0), stringsAsFactors=FALSE
  )
  if (is.null(db_path) || db_path == "" || !safe_file_exists(db_path)) return(empty)

  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(con), add=TRUE)

    if (!is.null(dataset_name) && dataset_name != "") {
      DBI::dbGetQuery(con,
        "SELECT id, dataset_name, file_name, run_timestamp,
                overall_status, check_fail_count, check_warn_count, row_count
         FROM snapshots WHERE dataset_name = ? ORDER BY id DESC LIMIT ?",
        list(dataset_name, as.integer(n)))
    } else {
      DBI::dbGetQuery(con,
        "SELECT id, dataset_name, file_name, run_timestamp,
                overall_status, check_fail_count, check_warn_count, row_count
         FROM snapshots ORDER BY id DESC LIMIT ?",
        list(as.integer(n)))
    }
  }, error = function(e) {
    message("read_snapshot_history: query failed for db_path '", db_path,
            "': ", conditionMessage(e))
    empty
  })
}

read_all_snapshot_history <- function(db_path, n = 200) {
  read_snapshot_history(db_path, dataset_name=NULL, n=n)
}

# Infer column types from a character vector sample (dqcheckr logic)
infer_col_type_simple <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return("unknown")
  date_fmts <- c("%Y-%m-%d","%d/%m/%Y","%m/%d/%Y","%Y%m%d","%d-%m-%Y")
  for (fmt in date_fmts) {
    parsed <- suppressWarnings(as.Date(x, format=fmt))
    if (all(!is.na(parsed))) return("date")
  }
  numeric_ok <- suppressWarnings(!is.na(as.numeric(x)))
  if (mean(numeric_ok) >= 0.90) return("numeric")
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

# Convert a UTC ISO timestamp (from the snapshot DB) to a local-time display string.
utc_to_local_display <- function(ts) {
  parsed <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  format(parsed, format = "%Y-%m-%d %H:%M:%S", tz = "")
}
