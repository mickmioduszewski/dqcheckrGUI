# Shared utilities

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && a[1] != "") a else b

make_report_filename <- function(dataset_name, run_timestamp) {
  ts_raw  <- gsub("[^0-9]", "", substr(run_timestamp, 1, 19))
  ts_slug <- paste0(substr(ts_raw, 1, 8), "_", substr(ts_raw, 9, 14))
  paste0(dataset_name, "_", ts_slug, ".html")
}

# Build the /dq_reports/ URL for a rendered drift report from the value
# compare_snapshots() returns. dqcheckr (>= 0.2.5) exposes the report path
# directly as result$report_path, so we link to it rather than re-deriving the
# filename from a slug regex -- that reconstruction broke when the drift
# filename gained its snapshot ids. Returns NULL when no report was written
# (report_path NULL, or the file is not on disk), so the caller can show a
# "no report" message instead of a dead link.
drift_report_url <- function(drift_result, report_prefix = "dq_reports") {
  rp <- drift_result$report_path
  if (is.null(rp) || length(rp) != 1 || is.na(rp) || !nzchar(rp) ||
      !file.exists(rp))
    return(NULL)
  # A dataset with a per-dataset report_output_dir renders its drift report into
  # that dir, served under "dq_reports_<ds>" — not the global "dq_reports". A
  # hardcoded /dq_reports/ prefix 404s for those datasets (B-09). Percent-encode
  # both segments (dataset-derived, untrusted) as report_link_cell() does.
  paste0("/", url_encode_filename(report_prefix), "/",
         url_encode_filename(basename(rp)))
}

# The Report-column cell (HTML) for the dataset panel's recent-runs table.
# Vectorised over runs. Aligns with dqcheckr's render_status contract so a link
# is only shown for a report that actually exists on disk:
#   report_file present               -> link to it (0.2.3+ recorded filename)
#   no file, render_status success/NA -> reconstruct the legacy slug and link
#                                        (genuine pre-0.2.3 row)
#   no file, render_status 'pending'  -> "rendering…" (0.2.5: report not written
#                                        yet; reconstructing would 404, and the
#                                        real name carries a snapshot id we omit)
#   no file, render_status 'failed'   -> "render failed"
report_link_cell <- function(report_file, render_status, run_timestamp,
                             dataset_name, report_prefix) {
  have_file <- !is.na(report_file)
  legacy_ok <- !have_file & (is.na(render_status) | render_status == "success")
  linkable  <- have_file | legacy_ok
  pending   <- !is.na(render_status) & render_status == "pending"
  filename  <- ifelse(have_file, report_file,
                      make_report_filename(dataset_name, run_timestamp))
  ifelse(linkable,
    sprintf(
      '<a href="javascript:void(0)" onclick="window.open(\'/%s/%s\',\'_blank\')">Open</a>',
      # report_prefix embeds the dataset name (dq_reports_<ds>), which is
      # untrusted (config filename / snapshots DB). It sits in a JS string
      # inside an escape=FALSE HTML attribute, so it must be percent-encoded
      # exactly like filename — HTML-entity escaping alone would be decoded back
      # to a quote before the JS parses and still break out (stored XSS, B-05).
      url_encode_filename(report_prefix), url_encode_filename(filename)),
    ifelse(pending,
      '<span class="text-muted fst-italic">rendering…</span>',
      '<span class="text-muted fst-italic">render failed</span>'))
}

# Merge one column's step-5 rule inputs with its previously-saved rule.
#
# The step-5 UI only renders the allowed-values input for a character/unknown
# column and the min/max/mean-shift inputs for a numeric column. Because a
# column's type is re-inferred each edit session (from the current file sample),
# reopening the wizard on a differently-shaped delivery can render a different
# set of rule inputs than the rule was saved under — and the collector would
# then read the missing inputs as NULL and silently drop a rule the user never
# touched (B-04). This gates on `ctype` exactly as the UI does: a rule's input
# is authoritative only when the UI actually rendered it; otherwise the saved
# value is carried forward. `saved` is the column's previously-stored rule list
# (or NULL). Returns the merged rule list (may be empty).
merge_column_rule <- function(ctype, saved,
                              allowed_in = NULL, min_in = NULL, max_in = NULL,
                              pattern_in = NULL, maxmiss_in = NULL,
                              meanshift_in = NULL) {
  if (is.null(saved)) saved <- list()
  rules   <- list()
  is_char <- ctype %in% c("character", "unknown")   # s5_allowed rendered
  is_num  <- identical(ctype, "numeric")            # s5_min/max/meanshift rendered

  if (is_char) {
    if (!is.null(allowed_in) && length(allowed_in) > 0 && any(nchar(allowed_in) > 0))
      rules$allowed_values <- allowed_in
  } else if (!is.null(saved$allowed_values)) {
    rules$allowed_values <- saved$allowed_values     # input not rendered -> keep
  }

  if (is_num) {
    if (!is.null(min_in) && !is.na(min_in)) rules$min_value <- min_in
    if (!is.null(max_in) && !is.na(max_in)) rules$max_value <- max_in
    if (!is.null(meanshift_in) && !is.na(meanshift_in))
      rules$max_numeric_mean_shift_pct <- meanshift_in / 100
  } else {
    if (!is.null(saved$min_value)) rules$min_value <- saved$min_value
    if (!is.null(saved$max_value)) rules$max_value <- saved$max_value
    if (!is.null(saved$max_numeric_mean_shift_pct))
      rules$max_numeric_mean_shift_pct <- saved$max_numeric_mean_shift_pct
  }

  # Always-rendered inputs (no carry-forward needed -- they cannot fail to render).
  if (!is.null(pattern_in) && nzchar(pattern_in)) rules$pattern <- pattern_in
  if (!is.null(maxmiss_in) && !is.na(maxmiss_in)) rules$max_missing_rate <- maxmiss_in

  rules
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

# Last non-blank line of a background-process log, for surfacing the real cause
# of a failed run/drift (usually more informative than the R condition message).
# Returns "" when the log is missing, empty, or unreadable. Shared by the run
# panel and the drift poller, which otherwise carried a verbatim copy each (B-16).
tail_nonblank_log_line <- function(log_path) {
  tryCatch({
    if (!is.null(log_path) && file.exists(log_path)) {
      lines <- readLines(log_path, warn = FALSE)
      lines <- lines[nchar(trimws(lines)) > 0]
      if (length(lines) > 0) tail(lines, 1) else ""
    } else ""
  }, error = function(e) "")
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

# Datasets whose runs are written to a snapshot DB other than the global one.
# The History panel reads only the global DB (load_history), so these datasets'
# runs are absent from that table — a known limitation of the single-DB history
# reader (B-08). Their drift comparisons are unaffected: launch_drift resolves
# the per-dataset DB. Returned names drive a disclosure banner so the omission is
# visible rather than silent. Compares resolved paths (both go through
# resolve_infra_path -> normalizePath), so a per-dataset snapshot_db that points
# back at the global file correctly counts as "shown here".
datasets_off_global_history <- function(config_dir, gcfg) {
  global_db <- effective_db_path(config_dir, gcfg, NULL)
  off <- character(0)
  for (ds in list_dataset_configs(config_dir)) {
    ds_db <- effective_db_path(config_dir, gcfg,
                               read_dataset_known(config_dir, ds)$snapshot_db)
    if (!identical(ds_db, global_db)) off <- c(off, ds)
  }
  off
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
  # Preallocated (not grown with c() each iteration): filled by index, so the
  # collision check `cand %in% taken` sees the assigned prefix plus trailing ""
  # slots — harmless since `cand` is never empty. Avoids O(n^2) reallocation on
  # very wide files (B-24).
  taken  <- character(n)
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
    taken[i]  <- cand
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
    # Tag the empty frame so the caller can distinguish a genuine read failure
    # (corrupt/unreachable DB) from a legitimately empty history ("no runs
    # yet") and surface the former instead of the reassuring empty state (B-10).
    # The guard-clause return above (missing/empty db_path) is NOT tagged: that
    # really is "no runs".
    attr(empty, "db_error") <- conditionMessage(e)
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
  }, error = function(e) {
    # Log rather than swallow silently: a corrupt/unreachable DB here blanks
    # every sidebar status badge, and without a trace the cause is invisible.
    # Matches the sibling read_snapshot_history's message (B-18).
    message("read_latest_statuses: query failed for db_path '", db_path,
            "': ", conditionMessage(e))
    character(0)
  })
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
  # Anchored shape guard, mirroring dqcheckr::infer_col_type. as.Date() delegates
  # to strptime(), which matches a *prefix* — so "2024-01-15xyz" and the 9-digit
  # id "202401159" (its first 8 chars parse under %Y%m%d) would both be accepted
  # as dates. Requiring the whole string to match the format's shape first closes
  # that, so the wizard preview agrees with run-time classification. Keep the
  # formats/shapes in sync with dqcheckr::infer_col_type.
  date_fmts   <- c("%Y-%m-%d","%d/%m/%Y","%m/%d/%Y","%Y%m%d","%d-%m-%Y",
                   "%Y-%m-%d %H:%M:%S","%Y-%m-%dT%H:%M:%S")
  date_shapes <- c("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", "^[0-9]{2}/[0-9]{2}/[0-9]{4}$",
                   "^[0-9]{2}/[0-9]{2}/[0-9]{4}$", "^[0-9]{8}$",
                   "^[0-9]{2}-[0-9]{2}-[0-9]{4}$",
                   "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$",
                   "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}$")
  for (i in seq_along(date_fmts)) {
    if (!all(grepl(date_shapes[[i]], x))) next
    parsed <- suppressWarnings(as.Date(x, format=date_fmts[[i]]))
    if (all(!is.na(parsed))) return("date")
  }
  numeric_ok <- suppressWarnings(!is.na(as.numeric(x)))
  if (mean(numeric_ok) >= threshold) return("numeric")
  "character"
}

# ASCII is a strict subset of UTF-8: declaring it can never parse differently
# from UTF-8, but passed to readr's locale verbatim it hard-crashes the R
# subprocess when a delivery contains a byte above 127 (vroom/iconv "Invalid
# multibyte sequence"). Normalise it away wherever an encoding enters the GUI.
normalise_encoding <- function(enc) {
  if (toupper(trimws(enc %||% "")) %in%
      c("ASCII", "US-ASCII", "ANSI_X3.4-1968", "ANSI_X3.4-1986",
        "ISO646-US", "646")) "UTF-8" else enc
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
# Read a per-dataset config's `known` keys, logging (not swallowing) a parse
# failure. A corrupt config otherwise silently falls back to the default report
# prefix, producing a dead report link / unregistered resource path with no
# diagnostic (B-06/G-09). Mirrors the "log rather than swallow" handling of the
# snapshot-DB reads. Used by every site that resolves a per-dataset report dir.
read_dataset_known <- function(config_dir, ds) {
  tryCatch(
    read_config(file.path(config_dir, paste0(ds, ".yml")))$known,
    error = function(e) {
      message("read_dataset_known: config for '", ds,
              "' could not be parsed: ", conditionMessage(e))
      NULL
    }
  )
}

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
    known <- read_dataset_known(config_dir, ds)
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
