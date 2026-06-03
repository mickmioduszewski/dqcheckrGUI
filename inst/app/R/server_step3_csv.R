# Step 3 CSV branch server logic (spec §11.3–11.4)

sniff_csv_file <- function(path) {
  tryCatch({
    con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
    on.exit(duckdb::dbDisconnect(con, shutdown=TRUE), add=TRUE)
    path_esc <- gsub("'", "''", path)
    DBI::dbGetQuery(con, sprintf("FROM sniff_csv('%s', sample_size=500)", path_esc))
  }, error=function(e) NULL)
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

  # Auto-run sniff when file loaded
  observe({
    req(length(wiz$raw_lines) > 0)
    path <- wiz$current_preview_path
    req(nchar(path %||% "") > 0)

    sniff <- sniff_csv_file(path)
    if (!is.null(sniff) && nrow(sniff) > 0) {
      # Delimiter
      detected_delim <- sniff$Delimiter[1] %||% ","
      if (detected_delim %in% c(",","\t",";","|"," ",":")) {
        updateSelectInput(session, "wiz_delimiter", selected=detected_delim)
        wiz$delimiter <- detected_delim
      }
      # Header
      has_hdr <- isTRUE(sniff$HasHeader[1])
      updateRadioButtons(session, "wiz_has_header", selected=as.character(has_hdr))
      wiz$has_header <- has_hdr

      # Column names and types from sniff
      cols_raw <- sniff$Columns[1]
      if (!is.null(cols_raw) && is.character(cols_raw)) {
        # DuckDB returns Columns as a JSON-like string; parse it
        # Format: [{name: x, type: y}, ...]
        col_data <- tryCatch({
          jsonlite::fromJSON(cols_raw)
        }, error=function(e) NULL)
        if (!is.null(col_data) && is.data.frame(col_data)) {
          wiz$sniff_col_names <- col_data$name
          wiz$sniff_col_types <- col_data$type
        }
      }
    }

    # Encoding
    enc_raw <- tryCatch(readr::guess_encoding(path), error=function(e) NULL)
    if (!is.null(enc_raw) && nrow(enc_raw) > 0) {
      top_enc <- enc_raw$encoding[1]
      choices <- unique(c(
        setNames(enc_raw$encoding[seq_len(min(3,nrow(enc_raw)))],
                 sprintf("%s (%.0f%%)", enc_raw$encoding[seq_len(min(3,nrow(enc_raw)))],
                         enc_raw$confidence[seq_len(min(3,nrow(enc_raw)))]*100)),
        c("UTF-8"="UTF-8","ISO-8859-1"="ISO-8859-1","Windows-1252"="Windows-1252",
          "UTF-16LE"="UTF-16LE","CP1250"="CP1250")
      ))
      wiz$encoding_choices <- choices
      updateSelectInput(session, "wiz_encoding", choices=choices, selected=top_enc)
      wiz$encoding <- top_enc
    }
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

  # No-header column naming panel
  output$step3_no_header_naming <- renderUI({
    req(input$wiz_format == "csv", !isTRUE(as.logical(input$wiz_has_header %||% "TRUE")))
    cols <- wiz$csv_col_names_detected
    if (is.null(cols) || length(cols) == 0) return(NULL)

    rows <- lapply(seq_along(cols), function(i) {
      name_id <- paste0("csv_noheader_name_", i)
      current <- if (i <= length(wiz$col_names)) wiz$col_names[i] else paste0("col_", i)
      fluidRow(class="mb-1",
        column(2, span(class="text-muted", style="font-size:12px;", paste0("col_", i, " →"))),
        column(4, textInput(name_id, NULL, value=current, placeholder=paste0("col_",i), width="100%"))
      )
    })

    div(class="p-2 border rounded bg-light mb-3",
      tags$strong("Column names", style="font-size:13px;"),
      div(style="max-height:200px;overflow-y:auto;", rows)
    )
  })

  # Collect no-header names
  observe({
    req(input$wiz_format == "csv", !isTRUE(as.logical(input$wiz_has_header %||% "TRUE")))
    cols <- wiz$csv_col_names_detected
    req(!is.null(cols))
    names_vec <- character(length(cols))
    for (i in seq_along(cols)) {
      v <- input[[paste0("csv_noheader_name_", i)]]
      names_vec[i] <- if (!is.null(v) && nchar(v) > 0) v else paste0("col_", i)
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

    if (!hdr) {
      wiz$csv_col_names_detected <- names(df)
      if (length(wiz$col_names) != ncol(df)) {
        wiz$col_names <- paste0("col_", seq_len(ncol(df)))
      }
      names(df) <- wiz$col_names
    } else {
      wiz$col_names <- names(df)
      wiz$csv_col_names_detected <- names(df)
    }

    # Infer types from sample
    wiz$col_types_inferred <- vapply(df, function(x) infer_col_type_simple(x), character(1))
    wiz$csv_preview_df <- df
  }, error=function(e) {
    wiz$csv_preview_df <- data.frame(Error=paste("Parse failed:", e$message))
  })
}
