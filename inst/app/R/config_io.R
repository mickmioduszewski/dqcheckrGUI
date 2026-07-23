# YAML config round-trip with unknown-key preservation (spec §20)

KNOWN_KEYS <- c(
  "dataset_name","format","encoding","delimiter","quote_char","col_names","csv_skip","folder",
  "current_file","previous_file","fwf_widths","fwf_col_names","fwf_skip",
  "expected_columns","key_columns","column_types","column_rules",
  "rule_overrides","custom_checks_file","snapshot_db","report_output_dir",
  "description"
)

read_config <- function(path) {
  raw <- yaml::read_yaml(path)
  known <- list(
    dataset_name       = raw$dataset_name %||% "",
    description        = raw$description %||% "",
    format             = raw$format %||% "csv",
    # Normalised on read so an edit-then-save upgrades a legacy "ASCII" config.
    encoding           = normalise_encoding(raw$encoding %||% "UTF-8"),
    delimiter          = raw$delimiter %||% ",",
    quote_char         = raw$quote_char %||% '"',
    # Header presence is the conjunction of two orthogonal facts. col_names may
    # be present either because the file is genuinely headerless (csv_skip 0) OR
    # because a usable-but-replaced header is being skipped (csv_skip >= 1). Only
    # the former means "no header". Without this 3-way rule, a renamed-header
    # config would round-trip to has_header = FALSE and lose csv_skip on re-save.
    has_header         = is.null(raw$col_names) || (raw$csv_skip %||% 0L) >= 1L,
    col_names          = if (!is.null(raw$col_names)) unlist(raw$col_names) else character(0),
    csv_skip           = as.integer(raw$csv_skip %||% 0L),
    folder             = raw$folder %||% "",
    current_file       = raw$current_file %||% "",
    previous_file      = raw$previous_file %||% "",
    fwf_widths         = raw$fwf_widths %||% integer(0),
    fwf_col_names      = raw$fwf_col_names %||% character(0),
    fwf_skip           = raw$fwf_skip %||% 0L,
    expected_columns   = raw$expected_columns %||% character(0),
    key_columns        = raw$key_columns %||% character(0),
    col_types_override = raw$column_types %||% list(),
    column_rules       = raw$column_rules %||% list(),
    rule_overrides     = raw$rule_overrides %||% list(),
    custom_checks_file = raw$custom_checks_file %||% "",
    snapshot_db        = raw$snapshot_db %||% "",
    report_output_dir  = raw$report_output_dir %||% ""
  )
  # file_mode derived
  known$file_mode <- if (nchar(known$folder) > 0) "folder" else "explicit"
  # Preserve unknown keys
  extra <- raw[setdiff(names(raw), KNOWN_KEYS)]
  list(known = known, extra = extra)
}

# Single source of truth for the default global config (spec §19). Both
# read_global_config()'s fallback and the new-project scaffold in app.R build
# from this, so the two definitions cannot drift apart (B-22).
default_global_config <- function() {
  list(
    snapshot_db       = "data/snapshots.sqlite",
    report_output_dir = "reports/",
    default_rules = list(
      type_inference_threshold       = 0.90,
      max_missing_rate               = 0.05,
      max_non_numeric_rate           = 0.01,
      min_row_count                  = 0L,
      max_row_count_change_pct       = 0.10,
      max_numeric_mean_shift_pct     = 0.20,
      max_missing_rate_change_pp     = 2.0,
      max_non_numeric_rate_change_pp = 1.0,
      flag_new_columns               = TRUE,
      flag_dropped_columns           = TRUE,
      flag_type_changes              = TRUE,
      flag_column_order_change       = TRUE
    )
  )
}

read_global_config <- function(path) {
  defaults <- default_global_config()
  if (!file.exists(path)) return(defaults)
  # A malformed global config must not crash the session at startup (this is
  # called unguarded when seeding the app's reactive config). Fall back to the
  # defaults with a warning; the save path (update_global_config) still refuses
  # to overwrite a config it could not read, so the corrupt file is preserved. B-09.
  tryCatch(
    yaml::read_yaml(path),
    error = function(e) {
      warning(sprintf(
        "Global config at '%s' could not be parsed; using defaults for this session (%s)",
        path, conditionMessage(e)), call. = FALSE)
      defaults
    })
}

# Merge edited values over the existing on-disk global config so hand-added
# keys survive a GUI save — parity with the dataset-config round-trip (spec
# §20). Two levels: unknown top-level keys are preserved, and inside
# default_rules unknown rule keys (e.g. max_row_count, max_file_size_mb,
# iqr_fence_multiplier — consumed by dqcheckr but with no GUI widget) are
# preserved while edited ones are replaced. Returns the merged list so the
# caller can keep its in-session copy identical to what is now on disk.
update_global_config <- function(values, path) {
  # A parse error must NOT be swallowed into list(): doing so would drop every
  # existing key the GUI form did not submit (the no-widget default_rules keys,
  # any hand-added keys) and then overwrite the file with the trimmed content --
  # silently wiping the config while reporting success. Abort instead so the
  # caller surfaces "Save failed" and the on-disk file is left untouched (B-05).
  existing <- if (file.exists(path))
    tryCatch(
      yaml::read_yaml(path),
      error = function(e) stop(sprintf(
        "the existing global config at '%s' could not be read, so it was not overwritten (%s)",
        path, conditionMessage(e)), call. = FALSE))
  else list()
  merged <- existing
  for (key in names(values)) {
    if (key == "default_rules" && is.list(existing$default_rules)) {
      dr <- existing$default_rules
      for (rk in names(values$default_rules)) dr[[rk]] <- values$default_rules[[rk]]
      merged$default_rules <- dr
    } else {
      merged[[key]] <- values[[key]]
    }
  }
  write_yaml_atomic(merged, path)
  merged
}

build_config_list <- function(wiz) {
  cfg <- list()
  cfg$dataset_name <- wiz$dataset_name
  if (nchar(wiz$description %||% "") > 0) cfg$description <- wiz$description

  cfg$format   <- wiz$format
  cfg$encoding <- wiz$encoding

  if (wiz$format == "csv") {
    cfg$delimiter <- wiz$delimiter
    if (!is.null(wiz$quote_char) && wiz$quote_char != '"')
      cfg$quote_char <- wiz$quote_char

    if (isFALSE(wiz$has_header)) {
      # Genuinely headerless: names are user-supplied, no header line to skip.
      if (length(wiz$col_names) > 0)
        cfg$col_names <- as.list(wiz$col_names)
    } else {
      # Header present.
      raw <- as.character(wiz$raw_header_names %||% character(0))
      if (length(raw) == 0) {
        # Step 3 was never re-probed this session (edit mode resets
        # raw_header_names to empty). We cannot recompute whether the header was
        # renamed, so we must preserve whatever the loaded config already had: a
        # renamed-header config carries explicit col_names + csv_skip >= 1, and
        # dropping them here would silently revert the file to its unwanted
        # original header on Save (B-06). A clean-header config has neither key
        # (col_names empty), so this correctly emits nothing for it.
        if (length(wiz$col_names) > 0 && (wiz$csv_skip %||% 0L) >= 1L) {
          cfg$col_names <- as.list(wiz$col_names)
          cfg$csv_skip  <- as.integer(wiz$csv_skip)
        }
      } else {
        # Step 3 was probed this session. Emit col_names + csv_skip = 1 ONLY when
        # the names were changed away from the file's actual header (duplicate/
        # invalid fixes or deliberate renames). A clean header writes neither
        # key, keeping the common case byte-identical to legacy output.
        renamed <- length(wiz$col_names) == length(raw) &&
                   !identical(as.character(wiz$col_names), raw)
        if (renamed) {
          cfg$col_names <- as.list(wiz$col_names)
          cfg$csv_skip  <- 1L
        }
      }
    }

    # Preserve a standalone csv_skip (skip N preamble rows, then auto-detect the
    # header) that is not tied to a col_names rename. Both header branches above
    # only emit csv_skip alongside col_names, so a config carrying csv_skip with
    # no col_names would otherwise be silently dropped on resave and its skipped
    # preamble rows read as data (B-03). The rename case already set col_names,
    # so guarding on is.null(cfg$col_names) avoids a double-write.
    if (is.null(cfg$col_names) && (wiz$csv_skip %||% 0L) >= 1L)
      cfg$csv_skip <- as.integer(wiz$csv_skip)
  }

  if (wiz$file_mode == "folder") {
    cfg$folder <- wiz$folder
  } else {
    cfg$current_file <- wiz$current_file
    if (nchar(wiz$previous_file %||% "") > 0)
      cfg$previous_file <- wiz$previous_file
  }

  if (wiz$format == "fwf") {
    cfg$fwf_widths    <- as.list(wiz$fwf_widths)
    cfg$fwf_col_names <- as.list(wiz$fwf_col_names)
    cfg$fwf_skip      <- as.integer(wiz$fwf_skip)
  }

  if (length(wiz$expected_columns) > 0)
    cfg$expected_columns <- as.list(wiz$expected_columns)

  if (length(wiz$key_columns) > 0)
    cfg$key_columns <- as.list(wiz$key_columns)

  # Column types. CSV: Step-4 overrides (only non-"auto" picks stored). FWF has
  # no Step 4 (it needs wiz$col_names, empty for FWF) — the Step-3 per-column
  # type selector writes wiz$col_types_inferred (named by fwf_col_names). Prefer
  # those when populated; fall back to the loaded overrides so an edit-mode
  # resave that never revisited Step 3 does not drop the saved types.
  col_types <- if (wiz$format == "fwf" &&
                   length(wiz$col_types_inferred) > 0 &&
                   !is.null(names(wiz$col_types_inferred))) {
    as.list(wiz$col_types_inferred)
  } else {
    wiz$col_types_override
  }
  if (length(col_types) > 0)
    cfg$column_types <- col_types

  # Build column_rules — only include columns that have at least one rule
  col_rules <- list()
  for (col in names(wiz$column_rules)) {
    rules <- wiz$column_rules[[col]]
    rules <- rules[!sapply(rules, function(x) is.null(x) || (length(x)==1 && (is.na(x) || x=="")))]
    if (length(rules) > 0) col_rules[[col]] <- rules
  }
  if (length(col_rules) > 0) cfg$column_rules <- col_rules

  # Rule overrides — only non-default values
  if (length(wiz$rule_overrides) > 0) {
    overrides <- wiz$rule_overrides[!sapply(wiz$rule_overrides, is.null)]
    if (length(overrides) > 0) cfg$rule_overrides <- overrides
  }

  if (nchar(wiz$custom_checks_file %||% "") > 0)
    cfg$custom_checks_file <- wiz$custom_checks_file

  # Per-dataset infrastructure path overrides (only if set and non-empty)
  if (nchar(wiz$snapshot_db %||% "") > 0)
    cfg$snapshot_db <- wiz$snapshot_db
  if (nchar(wiz$report_output_dir %||% "") > 0)
    cfg$report_output_dir <- wiz$report_output_dir

  cfg
}

# Write a list to `path` as YAML atomically: serialise to a temp file in the
# same directory, then rename it over the destination. yaml::write_yaml() opens
# the destination truncate-then-write, so an interrupted write (process killed,
# power loss) leaves the live config truncated or empty -- and these files are
# the sole config for a dataset (or the global config every dataset depends on),
# deployed to network/OneDrive shares. Same-directory temp keeps the rename on
# one filesystem so it is atomic; a copy+unlink fallback covers the rare case
# where rename is refused. B-07/B-08.
write_yaml_atomic <- function(x, path) {
  dir <- dirname(path)
  tmp <- tempfile("dqcfg_", tmpdir = dir, fileext = ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  yaml::write_yaml(x, tmp)
  if (!file.rename(tmp, path)) {
    if (!file.copy(tmp, path, overwrite = TRUE))
      stop(sprintf("Could not write config to '%s'.", path), call. = FALSE)
  }
  invisible(path)
}

# Best-effort mutex around the save critical section (the name-clash check and
# the write are otherwise a check-then-act race). dir.create() is an atomic
# test-and-set on the local filesystem, so it serialises concurrent saves from
# the SAME machine (e.g. two browser tabs / two app instances on one shared
# config dir). Cross-machine concurrent saves on a OneDrive/SMB sync share are
# NOT closable this way — a remote lock is not visible until sync propagates —
# and remain a known limitation of a file-based config store. A crashed save
# would otherwise leave a lock dir behind and wedge all future saves of that
# dataset, so a lock older than stale_seconds is reclaimed. Returns the lock
# path on success (release with unlink(recursive=TRUE)), or NULL if held.
acquire_config_lock <- function(config_dir, dataset_name, stale_seconds = 60) {
  lock <- file.path(config_dir, paste0(".", dataset_name, ".yml.lock"))
  if (dir.create(lock, showWarnings = FALSE)) return(lock)

  info  <- file.info(lock)
  stale <- !is.na(info$mtime) &&
           as.numeric(difftime(Sys.time(), info$mtime, units = "secs")) > stale_seconds
  if (!stale) return(NULL)

  # Serialise stale-lock reclaim through a short-lived meta-lock so at most one
  # caller ever removes-and-recreates the lock. The old unlink()-then-
  # dir.create() was a check-then-act: several callers who all saw the stale
  # lock both recreated it and both "acquired" it (B-13). The meta-lock is
  # created with the same atomic dir.create() and released on exit; re-checking
  # staleness while holding it prevents clobbering a lock another caller
  # refreshed in the meantime. (A hard crash in the microsecond between taking
  # the meta-lock and releasing it could wedge reclaim of this one lock — far
  # rarer, and less harmful, than the double-grant it replaces.)
  reclaim <- paste0(lock, ".reclaim")
  if (!dir.create(reclaim, showWarnings = FALSE)) return(NULL)
  on.exit(unlink(reclaim, recursive = TRUE), add = TRUE)

  info2 <- file.info(lock)
  still_stale <- is.na(info2$mtime) ||
                 as.numeric(difftime(Sys.time(), info2$mtime, units = "secs")) > stale_seconds
  if (still_stale) {
    unlink(lock, recursive = TRUE)
    if (dir.create(lock, showWarnings = FALSE)) return(lock)
  }
  NULL
}

write_config <- function(wiz, extra, path) {
  cfg <- build_config_list(wiz)
  # Merge extra keys (unknown keys from original file)
  for (key in names(extra)) {
    if (!key %in% names(cfg)) cfg[[key]] <- extra[[key]]
  }
  write_yaml_atomic(cfg, path)
}

yaml_preview_text <- function(wiz, extra) {
  cfg <- build_config_list(wiz)
  main_yaml <- yaml::as.yaml(cfg)
  if (length(extra) > 0) {
    extra_yaml <- yaml::as.yaml(extra)
    paste0(main_yaml, "\n# preserved from original file\n", extra_yaml)
  } else {
    main_yaml
  }
}
