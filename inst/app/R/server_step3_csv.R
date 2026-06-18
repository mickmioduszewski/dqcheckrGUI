# Step 3 CSV branch server logic (spec §11.3–11.4)

sniff_csv_file <- function(path) {
  tryCatch({
    lines <- readLines(path, n = 100, warn = FALSE)
    lines <- lines[nchar(trimws(lines)) > 0]
    if (length(lines) < 1) return(NULL)

    candidates <- c(",", "\t", ";", "|", ":")
    best_delim <- ","
    best_score <- 0
    for (d in candidates) {
      counts <- vapply(lines, function(l) length(strsplit(l, d, fixed = TRUE)[[1]]), integer(1))
      n_cols  <- stats::median(counts)
      if (n_cols > 1) {
        score <- (n_cols - 1) * mean(counts == n_cols)
        if (score > best_score) { best_score <- score; best_delim <- d }
      }
    }

    first_fields <- strsplit(lines[1], best_delim, fixed = TRUE)[[1]]
    has_header <- all(is.na(suppressWarnings(as.numeric(trimws(first_fields)))))

    df <- readr::read_delim(path, delim = best_delim,
                            col_names = has_header,
                            col_types = readr::cols(.default = "c"),
                            n_max = 500, show_col_types = FALSE)

    list(delimiter  = best_delim,
         has_header = has_header,
         col_names  = names(df),
         col_types  = vapply(df, infer_col_type_simple, character(1)))
  }, error = function(e) NULL)
}

load_raw_preview <- function(session, path, wiz) {
  lines <- tryCatch(
    readLines(path, n=50, warn=FALSE, encoding="bytes"),
    error=function(e) paste("Could not read file:", e$message)
  )
  wiz$raw_lines <- lines
  shinyAce::updateAceEditor(session, "raw_preview",
    value=paste(lines, collapse="\n"))
  # Update ruler
  max_chars <- max(nchar(lines), na.rm=TRUE)
  max_chars <- min(max(max_chars, 40), 200)
  ruler_str <- make_ruler_string(max_chars)
  wiz$ruler_string <- ruler_str
}

server_step3_csv <- function(input, output, session, wiz) {

  # CSV fields shown/hidden
  output$step3_csv_fields <- renderUI({
    req(input$wiz_format == "csv")
    tagList(
      fluidRow(
        column(3,
          selectInput("wiz_delimiter", "Delimiter",
            choices=c("Comma (,)"=",", "Tab"="\t", "Semicolon (;)"=";",
                      "Pipe (|)"="|", "Space"=" ", "Colon (:)"=":",
                      "Other"="other"),
            selected=wiz$delimiter %||% ",")
        ),
        column(3,
          uiOutput("step3_other_delimiter")
        ),
        column(3,
          selectInput("wiz_encoding", "Encoding",
            choices=wiz$encoding_choices %||% c("UTF-8","ISO-8859-1","Windows-1252","UTF-16LE","CP1250"),
            selected=wiz$encoding %||% "UTF-8")
        ),
        column(3,
          selectInput("wiz_quote_char", "Quote character",
            choices=c('Double quote "'='"', "Single quote '"="'", "None"=""),
            selected=wiz$quote_char %||% '"')
        )
      ),
      fluidRow(
        column(6,
          radioButtons("wiz_has_header", "First row",
            choices=c("Contains column headers"=TRUE, "Is data — no headers"=FALSE),
            selected=as.character(isTRUE(wiz$has_header %||% TRUE)))
        )
      ),
      div(class="mt-2",
        actionButton("step3_preview_btn", "Preview with these settings",
                     class="btn btn-outline-primary btn-sm")
      )
    )
  })

  output$step3_other_delimiter <- renderUI({
    req(input$wiz_delimiter == "other")
    textInput("wiz_delimiter_custom", "Custom:", value="", width="80px")
  })

  output$step3_fwf_fields <- renderUI({
    req(input$wiz_format == "fwf")
    tagList(
      fluidRow(
        column(4,
          numericInput("wiz_fwf_skip", "Header rows to skip",
                       value=wiz$fwf_skip %||% 0L, min=0, max=10, step=1)
        ),
        column(8,
          actionButton("step3_fwf_autodetect", "Auto-detect boundaries",
                       class="btn btn-outline-secondary btn-sm mt-4"),
          p(class="text-muted mt-1", style="font-size:11px;",
            "Works best when columns are space-separated. For tightly packed files, click on the text above to place boundaries.")
        )
      ),
      uiOutput("fwf_col_def_table"),
      uiOutput("fwf_record_length_badge")
    )
  })

  # Format hint below raw preview
  output$step3_format_hint <- renderUI({
    if (isTRUE(input$wiz_format == "fwf")) {
      div(class="alert alert-info p-2 mt-1", style="font-size:12px;",
          "Click on the text above to add a column boundary. Drag a boundary to move it. Double-click a boundary to remove it.",
          br(),
          "The character position is shown in the bottom-right of the preview.")
    }
  })

  # Auto-run sniff when file loaded.
  # New mode: auto-apply sniffed values (no config to preserve).
  # Edit mode: compare sniffed vs config-loaded; surface conflicts at step 3 without
  # overwriting. The user decides per-field whether to keep the config value or adopt
  # the detected one.
  observe({
    req(length(wiz$raw_lines) > 0)
    path <- wiz$current_preview_path
    req(nchar(path %||% "") > 0)

    sniff <- sniff_csv_file(path)

    if (!is.null(sniff) && !is.null(sniff$col_names)) {
      wiz$sniff_col_names <- sniff$col_names
      wiz$sniff_col_types <- sniff$col_types
    }

    enc_raw <- tryCatch(readr::guess_encoding(path), error = function(e) NULL)
    top_enc     <- NULL
    enc_choices <- NULL
    if (!is.null(enc_raw) && nrow(enc_raw) > 0) {
      top_enc <- enc_raw$encoding[1]
      enc_choices <- unique(c(
        setNames(enc_raw$encoding[seq_len(min(3, nrow(enc_raw)))],
                 sprintf("%s (%.0f%%)", enc_raw$encoding[seq_len(min(3, nrow(enc_raw)))],
                         enc_raw$confidence[seq_len(min(3, nrow(enc_raw)))] * 100)),
        c("UTF-8" = "UTF-8", "ISO-8859-1" = "ISO-8859-1",
          "Windows-1252" = "Windows-1252", "UTF-16LE" = "UTF-16LE", "CP1250" = "CP1250")
      ))
      wiz$encoding_choices <- enc_choices
    }

    if (isTRUE(wiz$mode == "edit")) {
      conflicts <- list()

      if (!is.null(sniff) && isTRUE(wiz$format == "csv")) {
        detected_delim <- sniff$delimiter %||% ","
        if (detected_delim %in% c(",", "\t", ";", "|", " ", ":") &&
            detected_delim != (wiz$delimiter %||% ",")) {
          conflicts[["delimiter"]] <- list(
            field       = "delimiter",
            config_val  = wiz$delimiter %||% ",",
            sniffed_val = detected_delim,
            label       = "Delimiter"
          )
        }
        sniff_hdr <- isTRUE(sniff$has_header)
        if (sniff_hdr != isTRUE(wiz$has_header)) {
          conflicts[["has_header"]] <- list(
            field       = "has_header",
            config_val  = isTRUE(wiz$has_header),
            sniffed_val = sniff_hdr,
            label       = "Header row"
          )
        }
      }

      if (!is.null(top_enc) && top_enc != (wiz$encoding %||% "UTF-8")) {
        conflicts[["encoding"]] <- list(
          field       = "encoding",
          config_val  = wiz$encoding %||% "UTF-8",
          sniffed_val = top_enc,
          label       = "Encoding"
        )
      }

      if (!is.null(enc_choices))
        updateSelectInput(session, "wiz_encoding",
                          choices  = enc_choices,
                          selected = wiz$encoding %||% "UTF-8")
      wiz$sniff_conflicts <- conflicts

    } else {
      if (!is.null(sniff)) {
        detected_delim <- sniff$delimiter %||% ","
        if (detected_delim %in% c(",", "\t", ";", "|", " ", ":")) {
          updateSelectInput(session, "wiz_delimiter", selected = detected_delim)
          wiz$delimiter <- detected_delim
        }
        has_hdr <- isTRUE(sniff$has_header)
        updateRadioButtons(session, "wiz_has_header", selected = as.character(has_hdr))
        wiz$has_header <- has_hdr
      }
      if (!is.null(enc_choices) && !is.null(top_enc)) {
        updateSelectInput(session, "wiz_encoding", choices = enc_choices, selected = top_enc)
        wiz$encoding <- top_enc
      }
    }
  })

  # Conflict banner — shown at step 3 in edit mode when sniffed values differ from config
  output$step3_sniff_conflicts <- renderUI({
    conflicts <- wiz$sniff_conflicts
    if (length(conflicts) == 0) return(NULL)

    fmt_val <- function(field, val) {
      if (field == "has_header") {
        if (isTRUE(val)) "has header row" else "no header row"
      } else {
        as.character(val)
      }
    }

    panels <- lapply(conflicts, function(cf) {
      div(class = "alert alert-warning p-2 mb-2", style = "font-size:12px;",
        tags$strong(paste0(cf$label, " mismatch — ")),
        sprintf("config says \"%s\", file looks like \"%s\".",
                fmt_val(cf$field, cf$config_val),
                fmt_val(cf$field, cf$sniffed_val)),
        div(class = "mt-1 d-flex gap-2",
          actionButton(paste0("sniff_keep_", cf$field),
                       paste0("Keep config: ", fmt_val(cf$field, cf$config_val)),
                       class = "btn btn-outline-secondary btn-sm",
                       style = "font-size:11px; padding:2px 8px;"),
          actionButton(paste0("sniff_use_", cf$field),
                       paste0("Use detected: ", fmt_val(cf$field, cf$sniffed_val)),
                       class = "btn btn-outline-primary btn-sm",
                       style = "font-size:11px; padding:2px 8px;")
        )
      )
    })
    tagList(panels)
  })

  # Dismiss conflict — keep config value
  observeEvent(input$sniff_keep_delimiter, {
    cf <- wiz$sniff_conflicts; cf[["delimiter"]] <- NULL; wiz$sniff_conflicts <- cf
  })
  observeEvent(input$sniff_keep_has_header, {
    cf <- wiz$sniff_conflicts; cf[["has_header"]] <- NULL; wiz$sniff_conflicts <- cf
  })
  observeEvent(input$sniff_keep_encoding, {
    cf <- wiz$sniff_conflicts; cf[["encoding"]] <- NULL; wiz$sniff_conflicts <- cf
  })

  # Accept detected value and dismiss
  observeEvent(input$sniff_use_delimiter, {
    cf <- wiz$sniff_conflicts[["delimiter"]]; req(!is.null(cf))
    wiz$delimiter <- cf$sniffed_val
    updateSelectInput(session, "wiz_delimiter", selected = cf$sniffed_val)
    cfs <- wiz$sniff_conflicts; cfs[["delimiter"]] <- NULL; wiz$sniff_conflicts <- cfs
  })
  observeEvent(input$sniff_use_has_header, {
    cf <- wiz$sniff_conflicts[["has_header"]]; req(!is.null(cf))
    v <- isTRUE(cf$sniffed_val)
    wiz$has_header <- v
    updateRadioButtons(session, "wiz_has_header", selected = as.character(v))
    cfs <- wiz$sniff_conflicts; cfs[["has_header"]] <- NULL; wiz$sniff_conflicts <- cfs
  })
  observeEvent(input$sniff_use_encoding, {
    cf <- wiz$sniff_conflicts[["encoding"]]; req(!is.null(cf))
    wiz$encoding <- cf$sniffed_val
    updateSelectInput(session, "wiz_encoding", selected = cf$sniffed_val)
    cfs <- wiz$sniff_conflicts; cfs[["encoding"]] <- NULL; wiz$sniff_conflicts <- cfs
  })

  # Update wiz from inputs
  observeEvent(input$wiz_format,    { wiz$format    <- input$wiz_format
    if (input$wiz_format == "fwf") session$sendCustomMessage("fwf_ruler_activate", list())
    else session$sendCustomMessage("fwf_ruler_deactivate", list()) })
  observeEvent(input$wiz_delimiter, { wiz$delimiter  <- if(input$wiz_delimiter=="other") input$wiz_delimiter_custom %||% "," else input$wiz_delimiter })
  observeEvent(input$wiz_encoding,  { wiz$encoding   <- input$wiz_encoding })
  observeEvent(input$wiz_quote_char,{ wiz$quote_char <- input$wiz_quote_char })
  observeEvent(input$wiz_has_header,{ wiz$has_header <- isTRUE(as.logical(input$wiz_has_header)) })
  observeEvent(input$wiz_fwf_skip,  { wiz$fwf_skip   <- as.integer(input$wiz_fwf_skip %||% 0) })

  # Preview trigger (CSV)
  observeEvent(input$step3_preview_btn, {
    trigger_csv_preview(input, wiz)
  })
  # Also auto-preview when format is CSV and file is loaded
  observe({
    req(input$wiz_format == "csv", length(wiz$raw_lines) > 0)
    trigger_csv_preview(input, wiz)
  })

  # FWF boundary positions → widths
  observeEvent(input$fwf_boundary_positions, {
    positions <- sort(unique(c(0L, as.integer(input$fwf_boundary_positions))))
    line_len  <- max(nchar(wiz$raw_lines[nchar(wiz$raw_lines)>0]), na.rm=TRUE)
    if (!is.finite(line_len)) line_len <- 80L
    widths <- diff(c(positions, as.integer(line_len)))
    widths <- widths[widths > 0]
    wiz$fwf_widths <- widths
    wiz$fwf_starts <- positions[positions > 0] + 1L
    wiz$fwf_line_len <- line_len
  })

  # FWF auto-detect
  observeEvent(input$step3_fwf_autodetect, {
    path <- wiz$current_preview_path
    req(nchar(path %||% "") > 0)
    result <- tryCatch(
      readr::fwf_empty(path, skip=as.integer(input$wiz_fwf_skip %||% 0), n=100),
      error=function(e) NULL
    )
    if (!is.null(result) && nrow(result) > 1) {
      # Convert to 0-based char positions (boundary starts)
      positions <- result$begin[-1]  # skip the first (=0)
      session$sendCustomMessage("fwf_restore_boundaries", list(positions=as.list(positions)))
      showNotification(sprintf("Auto-detected %d columns — please verify.", nrow(result)), type="message")
    } else {
      showNotification("Auto-detection found no boundaries. Set manually using the ruler.", type="warning")
    }
  })

  # FWF column name inputs
  output$fwf_col_def_table <- renderUI({
    n <- length(wiz$fwf_widths)
    if (n == 0) return(p(class="text-muted fst-italic", "No column boundaries set yet. Click on the text preview above."))

    starts <- c(1L, cumsum(wiz$fwf_widths[-length(wiz$fwf_widths)]) + 1L)

    rows <- lapply(seq_len(n), function(i) {
      name_id <- paste0("fwf_col_name_", i)
      type_id <- paste0("fwf_col_type_", i)
      current_name <- if (i <= length(wiz$fwf_col_names)) wiz$fwf_col_names[i] else paste0("col_", i)
      fluidRow(class="mb-1 align-items-center",
        column(1, span(class="badge bg-secondary", i)),
        column(2, span(class="text-muted", style="font-size:12px;", sprintf("%d–%d", starts[i], starts[i]+wiz$fwf_widths[i]-1L))),
        column(2, span(class="text-muted", style="font-size:12px;", paste("w:", wiz$fwf_widths[i]))),
        column(4, textInput(name_id, NULL, value=current_name, placeholder=paste0("col_", i))),
        column(3, selectInput(type_id, NULL, width="100%",
          choices=c("character","numeric","date"), selected="character"))
      )
    })

    tagList(
      div(class="mt-2 mb-1", tags$strong("Column definitions", style="font-size:13px;")),
      div(style="max-height:300px;overflow-y:auto;", rows)
    )
  })

  # Collect FWF column names from inputs
  observe({
    n <- length(wiz$fwf_widths)
    if (n == 0) return()
    names_vec <- character(n)
    for (i in seq_len(n)) {
      v <- input[[paste0("fwf_col_name_", i)]]
      names_vec[i] <- if (!is.null(v) && nchar(v) > 0) v else paste0("col_", i)
    }
    wiz$fwf_col_names <- names_vec
  })

  # Collect FWF column types from inputs
  observe({
    n <- length(wiz$fwf_widths)
    if (n == 0) return()
    col_names <- wiz$fwf_col_names
    types_vec <- character(n)
    for (i in seq_len(n)) {
      v <- input[[paste0("fwf_col_type_", i)]]
      types_vec[i] <- if (!is.null(v)) v else "character"
    }
    names(types_vec) <- if (length(col_names) == n) col_names else paste0("col_", seq_len(n))
    wiz$col_types_inferred <- types_vec
  })

  # FWF record length badge
  output$fwf_record_length_badge <- renderUI({
    req(length(wiz$fwf_widths) > 0, length(wiz$raw_lines) > 0)
    line_len    <- max(nchar(wiz$raw_lines[nchar(wiz$raw_lines) > 0]), na.rm=TRUE)
    total_width <- sum(wiz$fwf_widths)
    if (!is.finite(line_len)) return(NULL)

    if (total_width == line_len) {
      div(class="alert alert-success p-1 mt-2", style="font-size:12px;",
          sprintf("✅ Record length: %d / %d characters", total_width, line_len))
    } else if (total_width < line_len) {
      div(class="alert alert-warning p-1 mt-2", style="font-size:12px;",
          sprintf("⚠ Column definitions cover %d of %d characters — %d characters unaccounted for at end of record.",
                  total_width, line_len, line_len - total_width))
    } else {
      div(class="alert alert-danger p-1 mt-2", style="font-size:12px;",
          sprintf("✗ Column definitions (%d) exceed actual line length (%d).", total_width, line_len))
    }
  })

  # Column-naming panel. Renders for headerless files (always) and for header
  # files whose raw header names are invalid or duplicated (e.g. RBB RefundsPaid
  # repeats `PayeeName`). Pre-fills suggestions with an inline reason; fully
  # overridable. Input ids are shared across both modes (`csv_name_<i>`).
  output$step3_no_header_naming <- renderUI({
    req(input$wiz_format == "csv")
    has_header <- isTRUE(as.logical(input$wiz_has_header %||% "TRUE"))
    cols <- wiz$csv_col_names_detected          # reactive trigger: the column SET
    if (is.null(cols) || length(cols) == 0) return(NULL)
    if (!csv_needs_naming(wiz, has_header)) return(NULL)

    # Read the editable values WITHOUT subscribing to them. The collector below
    # writes wiz$col_names on every keystroke; if this panel re-rendered on each
    # change it would rebuild the DOM mid-edit, resetting the preview's scroll
    # position and fighting the cursor. We only want to rebuild when the column
    # set itself changes (handled by the reactive reads above). Per CLAUDE.md,
    # isolate() is the wizard's standard tool for exactly this.
    isolate({
      reasons <- wiz$col_name_reasons
      rows <- lapply(seq_along(cols), function(i) {
        name_id <- paste0("csv_name_", i)
        current <- if (i <= length(wiz$col_names)) wiz$col_names[i] else paste0("col_", i)
        left    <- if (has_header) cols[i] else paste0("col_", i)
        hint    <- if (length(reasons) >= i && nzchar(reasons[i]))
          span(class="text-warning", style="font-size:11px;", paste0("⚠ ", reasons[i]))
        else NULL
        fluidRow(class="mb-1 align-items-center",
          column(3, span(class="text-muted", style="font-size:12px;", paste0(left, " →"))),
          column(4, textInput(name_id, NULL, value=current, placeholder=paste0("col_",i), width="100%")),
          column(5, hint)
        )
      })

      div(class="p-2 border rounded bg-light mb-3",
        tags$strong("Column names", style="font-size:13px;"),
        if (has_header)
          p(class="text-muted mb-1", style="font-size:11px;",
            "Some names in the file's header are invalid or duplicated. Adjust as needed — the original header row will be skipped when the file is read."),
        div(style="max-height:240px;overflow-y:auto;", rows)
      )
    })
  })

  # Collect names from the editor (runs under the same condition that renders it)
  observe({
    req(input$wiz_format == "csv")
    has_header <- isTRUE(as.logical(input$wiz_has_header %||% "TRUE"))
    cols <- wiz$csv_col_names_detected
    req(!is.null(cols), length(cols) > 0)
    if (!csv_needs_naming(wiz, has_header)) return()
    names_vec <- character(length(cols))
    for (i in seq_along(cols)) {
      v <- input[[paste0("csv_name_", i)]]
      names_vec[i] <- if (!is.null(v) && nchar(v) > 0) v
        else if (i <= length(wiz$col_names)) wiz$col_names[i]
        else paste0("col_", i)
    }
    wiz$col_names <- names_vec
  })

  # Parsed preview (CSV)
  output$step3_parsed_preview <- renderUI({
    req(input$wiz_format == "csv", !is.null(wiz$csv_preview_df))
    reactable::reactableOutput("step3_reactable_preview")
  })

  output$step3_reactable_preview <- reactable::renderReactable({
    req(!is.null(wiz$csv_preview_df))
    df <- wiz$csv_preview_df
    reactable::reactable(df,
      resizable=TRUE, wrap=FALSE, fullWidth=TRUE,
      defaultPageSize=20, striped=TRUE, highlight=TRUE,
      style=list(fontSize="12px"))
  })

  # FWF parsed preview
  observe({
    req(input$wiz_format == "fwf",
        length(wiz$fwf_widths) > 0,
        length(wiz$fwf_col_names) > 0,
        nchar(wiz$current_preview_path %||% "") > 0)

    path <- wiz$current_preview_path
    tryCatch({
      df <- readr::read_fwf(path,
        col_positions=readr::fwf_widths(wiz$fwf_widths, wiz$fwf_col_names),
        col_types=readr::cols(.default="c"),
        n_max=20,
        skip=as.integer(wiz$fwf_skip %||% 0),
        locale=readr::locale(encoding=wiz$encoding %||% "UTF-8"))
      wiz$fwf_preview_df <- as.data.frame(df)
    }, error=function(e) { wiz$fwf_preview_df <- NULL })
  })

  # FWF parsed preview output
  output$step3_fwf_preview <- reactable::renderReactable({
    req(!is.null(wiz$fwf_preview_df))
    reactable::reactable(wiz$fwf_preview_df,
      resizable=TRUE, wrap=FALSE, fullWidth=TRUE,
      defaultPageSize=20, striped=TRUE, highlight=TRUE,
      style=list(fontSize="12px"))
  })

}

# Helper to trigger CSV re-parse
trigger_csv_preview <- function(input, wiz) {
  path <- wiz$current_preview_path
  if (is.null(path) || nchar(path) == 0) return()

  delim <- if (!is.null(input$wiz_delimiter) && input$wiz_delimiter == "other")
    input$wiz_delimiter_custom %||% ","
  else input$wiz_delimiter %||% ","
  enc  <- input$wiz_encoding %||% "UTF-8"
  hdr  <- isTRUE(as.logical(input$wiz_has_header %||% "TRUE"))
  quot <- input$wiz_quote_char %||% '"'

  tryCatch({
    df <- readr::read_delim(path,
      delim=delim,
      col_names=hdr,
      col_types=readr::cols(.default="c"),
      n_max=20,
      quote=quot,
      locale=readr::locale(encoding=enc),
      show_col_types=FALSE)
    df <- as.data.frame(df)
    ncols <- ncol(df)

    # `wiz$col_names` is the single source of truth for column names. Reseed it
    # ONLY when the column count changes — never clobber an existing list of the
    # right length, or we would erase the user's edits / our own suggestions on
    # every re-preview (the auto-preview observer fires on each step-3 render).
    if (!hdr) {
      wiz$raw_header_names <- character(0)
      wiz$col_name_reasons <- character(0)
      wiz$csv_col_names_detected <- names(df)
      if (length(wiz$col_names) != ncols)
        wiz$col_names <- paste0("col_", seq_len(ncols))
    } else {
      # Probe the *unmangled* header so duplicates survive (readr's default
      # name_repair mangles them to `name...NN`, which we can't fix in the UI).
      raw <- tryCatch(
        names(readr::read_delim(path, delim=delim, n_max=0, quote=quot,
                                name_repair="minimal",
                                locale=readr::locale(encoding=enc),
                                show_col_types=FALSE)),
        error=function(e) names(df))
      if (length(raw) != ncols) raw <- names(df)   # safety: keep counts aligned
      sug <- suggest_col_names(raw)
      wiz$raw_header_names <- raw
      wiz$col_name_reasons <- sug$reason
      wiz$csv_col_names_detected <- raw
      if (length(wiz$col_names) != ncols)
        wiz$col_names <- sug$names
    }

    # Show the preview under the resolved names, not the raw/mangled ones.
    if (length(wiz$col_names) == ncols) names(df) <- wiz$col_names

    # Infer types from sample
    wiz$col_types_inferred <- vapply(df, function(x) infer_col_type_simple(x), character(1))
    wiz$csv_preview_df <- df
  }, error=function(e) {
    wiz$csv_preview_df <- data.frame(Error=paste("Parse failed:", e$message))
  })
}
