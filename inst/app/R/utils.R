# Shared utilities

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && a[1] != "") a else b

make_report_filename <- function(dataset_name, run_timestamp) {
  ts_raw  <- gsub("[^0-9]", "", substr(run_timestamp, 1, 19))
  ts_slug <- paste0(substr(ts_raw, 1, 8), "_", substr(ts_raw, 9, 14))
  paste0(dataset_name, "_", ts_slug, ".html")
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
  }, error = function(e) empty)
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

# Convert a UTC ISO timestamp (from the snapshot DB) to a local-time display string.
utc_to_local_display <- function(ts) {
  parsed <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  format(parsed, format = "%Y-%m-%d %H:%M:%S", tz = "")
}
