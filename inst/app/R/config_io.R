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
    encoding           = raw$encoding %||% "UTF-8",
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

read_global_config <- function(path) {
  if (!file.exists(path)) {
    return(list(
      snapshot_db       = "data/snapshots.sqlite",
      report_output_dir = "reports/",
      default_rules = list(
        type_inference_threshold      = 0.90,
        max_missing_rate              = 0.05,
        max_non_numeric_rate          = 0.01,
        min_row_count                 = 0,
        max_row_count_change_pct      = 0.10,
        max_numeric_mean_shift_pct    = 0.20,
        max_missing_rate_change_pp    = 2.0,
        max_non_numeric_rate_change_pp= 1.0,
        flag_new_columns              = TRUE,
        flag_dropped_columns          = TRUE,
        flag_type_changes             = TRUE,
        flag_column_order_change      = TRUE
      )
    ))
  }
  yaml::read_yaml(path)
}

write_global_config <- function(values, path) {
  yaml::write_yaml(values, path)
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
      # Header present. Emit col_names + csv_skip = 1 ONLY when the names were
      # changed away from the file's actual header (duplicate/invalid fixes or
      # deliberate renames). A clean header — or one we couldn't probe — writes
      # neither key, keeping the common case byte-identical to legacy output.
      raw <- as.character(wiz$raw_header_names %||% character(0))
      renamed <- length(raw) > 0 &&
                 length(wiz$col_names) == length(raw) &&
                 !identical(as.character(wiz$col_names), raw)
      if (renamed) {
        cfg$col_names <- as.list(wiz$col_names)
        cfg$csv_skip  <- 1L
      }
    }
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

  if (length(wiz$col_types_override) > 0)
    cfg$column_types <- wiz$col_types_override

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

write_config <- function(wiz, extra, path) {
  cfg <- build_config_list(wiz)
  # Merge extra keys (unknown keys from original file)
  for (key in names(extra)) {
    if (!key %in% names(cfg)) cfg[[key]] <- extra[[key]]
  }
  yaml::write_yaml(cfg, path)
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
