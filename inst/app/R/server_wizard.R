# Config wizard server logic — state management, navigation, steps 1-2, 4-8
# Step 3 CSV/FWF split into server_step3_csv.R

server_wizard <- function(input, output, session, rv, config_dir, gcfg_rv) {

  # ── Wizard state ─────────────────────────────────────────────────────
  # Single source of truth for defaults — used to seed reactiveValues and to
  # reset state when the wizard is opened for a new dataset.
  .wiz_defaults <- list(
    mode="new", dataset_name="", description="",
    file_mode="folder", folder="", current_file="", previous_file="",
    format="csv", encoding="UTF-8", delimiter=",", quote_char='"',
    has_header=TRUE, csv_skip=0L,
    col_names=character(0), col_types_inferred=character(0),
    raw_header_names=character(0), col_name_reasons=character(0),
    col_types_override=list(),
    key_columns=character(0), expected_columns=character(0),
    fwf_widths=integer(0), fwf_col_names=character(0), fwf_skip=0L,
    column_rules=list(), rule_overrides=list(),
    custom_checks_file="", snapshot_db="", report_output_dir="",
    extra_keys=list(),
    sniff_conflicts=list(),
    current_step=1L,
    step_valid=rep(FALSE, 8L),
    raw_lines=character(0),
    ruler_string=make_ruler_string(80),
    current_preview_path="",
    encoding_choices=c("UTF-8","ISO-8859-1","Windows-1252","UTF-16LE","CP1250"),
    sniff_col_names=character(0), sniff_col_types=character(0),
    csv_preview_df=NULL, csv_col_names_detected=character(0),
    fwf_preview_df=NULL, fwf_starts=integer(0), fwf_line_len=80L
  )
  wiz <- do.call(reactiveValues, .wiz_defaults)

  open_wizard <- function(mode, dataset_name=NULL) {
    if (mode == "edit" && !is.null(dataset_name)) {
      path <- file.path(config_dir(), paste0(dataset_name, ".yml"))
      if (safe_file_exists(path)) {
        cfg <- tryCatch(read_config(path), error=function(e) NULL)
        if (!is.null(cfg)) {
          k <- cfg$known
          for (nm in names(k)) wiz[[nm]] <- k[[nm]]
          wiz$extra_keys    <- cfg$extra
          wiz$mode          <- "edit"
          wiz$current_step  <- 1L
          # Trigger file preview
          preview_path <- if (nchar(k$folder %||%"") > 0) {
            files <- list.files(k$folder, full.names=TRUE)
            if (length(files) > 0) files[order(file.mtime(files), decreasing=TRUE)[1]] else ""
          } else k$current_file %||% ""
          if (nchar(preview_path) > 0) {
            wiz$current_preview_path <- preview_path
            load_raw_preview(session, preview_path, wiz)
          }
        }
      }
    } else {
      for (nm in names(.wiz_defaults)) wiz[[nm]] <- .wiz_defaults[[nm]]
    }
    rv$wizard_open <- TRUE
    rv$active_section <- "wizard"
  }

  # Watch for wizard open requests from parent (rv$wizard_request)
  observeEvent(rv$wizard_request, {
    req(!is.null(rv$wizard_request))
    open_wizard(rv$wizard_request$mode, rv$wizard_request$dataset)
  })

  # ── Breadcrumb ────────────────────────────────────────────────────────
  output$wizard_breadcrumb <- renderUI({
    wizard_breadcrumb(wiz$current_step, wiz$step_valid)
  })

  # ── Step content ─────────────────────────────────────────────────────
  # Only react to step navigation (wiz$current_step); isolate all wiz field
  # reads so that typing in a text input doesn't re-render the whole step
  # and steal keyboard focus from the active field.
  output$wizard_step_content <- renderUI({
    s <- wiz$current_step          # reactive: re-render on step change only
    isolate({                      # isolate: wiz field reads don't subscribe
      gcfg <- gcfg_rv()
      if      (s == 1) wizard_step1_ui(wiz)
      else if (s == 2) wizard_step2_ui(wiz)
      else if (s == 3) wizard_step3_ui()
      else if (s == 4) wizard_step4_ui()
      else if (s == 5) wizard_step5_ui()
      else if (s == 6) wizard_step6_ui(wiz, gcfg)
      else if (s == 7) wizard_step7_ui(wiz)
      else if (s == 8) wizard_step8_ui()
    })
  })

  # ── Step validation ───────────────────────────────────────────────────
  step_valid <- reactive({
    valid <- rep(FALSE, 8L)
    # Step 1
    nm <- input$wiz_dataset_name %||% ""
    valid[1] <- nchar(nm) > 0 && is_valid_r_name(nm)
    # Step 2
    path <- wiz$current_preview_path
    valid[2] <- nchar(path %||% "") > 0
    # Step 3
    if (isTRUE(input$wiz_format == "fwf")) {
      # FWF valid when: widths sum <= line len, at least 1 col, all names valid
      total <- sum(wiz$fwf_widths)
      line_len <- wiz$fwf_line_len %||% 80L
      valid[3] <- total > 0 && total <= line_len &&
                  length(wiz$fwf_col_names) > 0 &&
                  all(nchar(wiz$fwf_col_names) > 0) &&
                  all(is_valid_r_name(wiz$fwf_col_names)) &&
                  !any(duplicated(wiz$fwf_col_names))
    } else {
      valid[3] <- length(wiz$col_names) > 0 &&
                  all(nchar(wiz$col_names) > 0) &&
                  all(is_valid_r_name(wiz$col_names)) &&
                  !any(duplicated(wiz$col_names))
    }
    # Steps 4-8 are unlocked once step 3 is complete (col_names populated)
    step3_done <- isTRUE(valid[3])
    valid[4] <- step3_done
    valid[5] <- step3_done
    valid[6] <- step3_done
    valid[7] <- step3_done
    valid[8] <- step3_done
    valid
  })

  observe({ wiz$step_valid <- step_valid() })

  # ── Navigation ────────────────────────────────────────────────────────
  output$wizard_back_btn <- renderUI({
    if (wiz$current_step > 1)
      actionButton("wizard_back", "◄ Back", class="btn btn-outline-secondary btn-sm")
  })
  output$wizard_next_btn <- renderUI({
    if (wiz$current_step < 8) {
      disabled <- !isTRUE(wiz$step_valid[wiz$current_step])
      btn <- actionButton("wizard_next", "Next ►", class="btn btn-primary btn-sm")
      if (disabled) tagAppendAttributes(btn, disabled=NA) else btn
    }
  })

  # (Re-)activate the FWF ruler whenever step 3 becomes the visible step.
  # `output$wizard_step_content` (above) rebuilds the entire step UI — and
  # with it a fresh, empty `#fwf-ruler-wrap` / SVG overlay — on every step
  # change, including backward navigation. So the JS-side ruler must be
  # told to reattach (and redraw any previously-placed boundaries) every
  # time we land on step 3, not just when advancing into it from step 2.
  enter_fwf_step3 <- function() {
    output$fwf_char_ruler_text <- renderText({ wiz$ruler_string })
    positions <- if (length(wiz$fwf_starts) > 0)
      as.list(as.integer(wiz$fwf_starts) - 1L) else list()
    session$sendCustomMessage("fwf_reinit", list(positions = positions))
  }

  observeEvent(input$wizard_back, {
    if (wiz$current_step > 1) {
      wiz$current_step <- wiz$current_step - 1L
      if (wiz$current_step == 3L) enter_fwf_step3()
    }
  })

  observeEvent(input$wizard_next, {
    sv <- step_valid()
    if (isTRUE(sv[wiz$current_step])) {
      collect_step_inputs(input, wiz, gcfg_rv())
      wiz$current_step <- wiz$current_step + 1L
      if (wiz$current_step == 3L) enter_fwf_step3()
    }
  })

  # ── Step 1 inputs ────────────────────────────────────────────────────
  observeEvent(input$wiz_dataset_name, { wiz$dataset_name <- input$wiz_dataset_name %||% "" })
  observeEvent(input$wiz_description,  { wiz$description  <- input$wiz_description  %||% "" })

  # Step 1 validation
  iv_step1 <- shinyvalidate::InputValidator$new()
  iv_step1$add_rule("wiz_dataset_name", function(v) {
    # Empty on first open: no error — Next button gate handles the required check
    if (is.null(v) || nchar(v) == 0) return(NULL)
    if (!is_valid_r_name(v))
      return("Must start with a letter; only letters, numbers, and underscores.")
    existing <- list_dataset_configs(config_dir())
    if (wiz$mode == "new" && v %in% existing)
      return(sprintf("A dataset named '%s' already exists.", v))
  })
  iv_step1$enable()

  # ── Step 2: File location ────────────────────────────────────────────
  file_roots <- c(Home=path.expand("~"), WD=getwd())

  shinyFiles::shinyDirChoose(input, "wiz_folder_browse",
    roots=file_roots, session=session)
  shinyFiles::shinyFileChoose(input, "wiz_current_file_browse",
    roots=file_roots, session=session)
  shinyFiles::shinyFileChoose(input, "wiz_prev_file_browse",
    roots=file_roots, session=session)

  # ── shinyFiles browse callbacks ─────────────────────────────────────
  # These fire when the Browse dialog is used successfully.
  # Direct text input is the primary path entry (see observers below).
  observeEvent(input$wiz_folder_browse, {
    req(is.list(input$wiz_folder_browse))
    p <- shinyFiles::parseDirPath(file_roots, input$wiz_folder_browse)
    if (length(p) > 0) {
      path <- as.character(p[1])
      updateTextInput(session, "wiz_folder_display", value=path)
      activate_folder(path)
    }
  })

  observeEvent(input$wiz_current_file_browse, {
    req(is.list(input$wiz_current_file_browse))
    p <- shinyFiles::parseFilePaths(file_roots, input$wiz_current_file_browse)
    if (nrow(p) > 0) {
      path <- as.character(p$datapath[1])
      updateTextInput(session, "wiz_current_file_display", value=path)
      activate_file(path)
    }
  })

  observeEvent(input$wiz_prev_file_browse, {
    req(is.list(input$wiz_prev_file_browse))
    p <- shinyFiles::parseFilePaths(file_roots, input$wiz_prev_file_browse)
    if (nrow(p) > 0) {
      path <- as.character(p$datapath[1])
      updateTextInput(session, "wiz_prev_file_display", value=path)
      wiz$previous_file <- path
    }
  })

  # ── Helpers: activate a folder/file path from any source ────────────
  activate_folder <- function(path) {
    path <- trimws(path)
    if (!safe_dir_exists(path)) return()
    wiz$folder <- path
    files <- list.files(path, full.names=TRUE)
    if (length(files) > 0) {
      preview <- files[order(file.mtime(files), decreasing=TRUE)[1]]
      wiz$current_preview_path <- preview
      load_raw_preview(session, preview, wiz)
    }
  }

  activate_file <- function(path) {
    path <- trimws(path)
    if (!safe_file_exists(path)) return()
    wiz$current_file        <- path
    wiz$current_preview_path <- path
    load_raw_preview(session, path, wiz)
  }

  # ── Direct text input observers (primary path entry method) ─────────
  # Debounce the folder text input so we only react after the user pauses typing.
  folder_input_r <- reactive({ input$wiz_folder_display })
  folder_input_d <- shiny::debounce(folder_input_r, 600)

  observe({
    path <- folder_input_d() %||% ""
    if (nchar(trimws(path)) > 0) activate_folder(path)
  })

  observeEvent(input$wiz_current_file_display, {
    path <- trimws(input$wiz_current_file_display %||% "")
    if (nchar(path) > 0 && safe_file_exists(path)) activate_file(path)
    else if (nchar(path) > 0) wiz$current_file <- path  # store even if not yet valid
  }, ignoreInit=TRUE)

  observeEvent(input$wiz_prev_file_display, {
    path <- trimws(input$wiz_prev_file_display %||% "")
    wiz$previous_file <- path
  }, ignoreInit=TRUE)

  # Sync file_mode radio to wiz state so build_config_list uses the correct branch
  observeEvent(input$wiz_file_mode, {
    wiz$file_mode <- input$wiz_file_mode %||% "folder"
  }, ignoreInit=TRUE)

  # ── Render Step 2 file inputs ────────────────────────────────────────
  output$wiz_file_inputs <- renderUI({
    mode <- input$wiz_file_mode %||% "folder"
    if (mode == "folder") {
      tagList(
        p(class="text-muted mb-1", style="font-size:12px;",
          "Type or paste the full folder path, or use Browse."),
        div(class="d-flex gap-2 align-items-end",
          div(style="flex:1;",
            textInput("wiz_folder_display", "Folder path",
                      value=wiz$folder %||% "", width="100%",
                      placeholder="/path/to/data/folder/")
          ),
          shinyFiles::shinyDirButton("wiz_folder_browse", "Browse",
            "Select data folder", style="height:38px;white-space:nowrap;")
        ),
        uiOutput("wiz_folder_path_status"),
        uiOutput("wiz_folder_preview_info")
      )
    } else {
      tagList(
        p(class="text-muted mb-1", style="font-size:12px;",
          "Type or paste the full file path, or use Browse."),
        div(class="d-flex gap-2 align-items-end",
          div(style="flex:1;",
            textInput("wiz_current_file_display", "Current file *",
                      value=wiz$current_file %||% "", width="100%",
                      placeholder="/path/to/data/delivery_2026.csv")
          ),
          shinyFiles::shinyFilesButton("wiz_current_file_browse", "Browse",
            "Select current file", FALSE, style="height:38px;white-space:nowrap;")
        ),
        uiOutput("wiz_current_file_status"),
        div(class="d-flex gap-2 align-items-end mt-3",
          div(style="flex:1;",
            textInput("wiz_prev_file_display", "Previous file (optional)",
                      value=wiz$previous_file %||% "", width="100%",
                      placeholder="/path/to/data/delivery_2026_prev.csv")
          ),
          shinyFiles::shinyFilesButton("wiz_prev_file_browse", "Browse",
            "Select previous file", FALSE, style="height:38px;white-space:nowrap;")
        ),
        uiOutput("wiz_prev_file_status")
      )
    }
  })

  # ── Path status badges ───────────────────────────────────────────────
  output$wiz_folder_path_status <- renderUI({
    path <- trimws(input$wiz_folder_display %||% "")
    if (nchar(path) == 0) return(NULL)
    if (safe_dir_exists(path)) {
      n <- length(list.files(path))
      div(class="text-success mt-1", style="font-size:12px;",
          sprintf("✓ Folder found  (%d files)", n))
    } else {
      div(class="text-danger mt-1", style="font-size:12px;",
          "✗ Folder not found — check the path")
    }
  })

  output$wiz_current_file_status <- renderUI({
    path <- trimws(input$wiz_current_file_display %||% "")
    if (nchar(path) == 0) return(NULL)
    if (safe_file_exists(path)) {
      sz <- format(file.size(path), big.mark=",")
      div(class="text-success mt-1", style="font-size:12px;",
          sprintf("✓ File found  (%s bytes)", sz))
    } else {
      div(class="text-danger mt-1", style="font-size:12px;",
          "✗ File not found — check the path")
    }
  })

  output$wiz_prev_file_status <- renderUI({
    path <- trimws(input$wiz_prev_file_display %||% "")
    if (nchar(path) == 0) return(NULL)
    if (safe_file_exists(path)) {
      div(class="text-success mt-1", style="font-size:12px;", "✓ File found")
    } else {
      div(class="text-warning mt-1", style="font-size:12px;",
          "⚠ File not found — leave blank to skip comparison checks")
    }
  })

  output$wiz_folder_preview_info <- renderUI({
    path <- wiz$folder %||% ""
    if (nchar(path) == 0) return(NULL)
    files <- list.files(path, full.names=TRUE)
    if (length(files) == 0) return(p(class="text-warning", "Folder is empty."))
    files_sorted <- files[order(file.mtime(files), decreasing=TRUE)]
    info <- lapply(seq_len(min(2, length(files_sorted))), function(i) {
      lbl <- if (i==1) "Current" else "Previous"
      sz  <- format(file.size(files_sorted[i]), big.mark=",")
      sprintf("%s: %s (%s bytes)", lbl, basename(files_sorted[i]), sz)
    })
    p(class="text-muted", style="font-size:12px;", paste(info, collapse=" | "))
  })

  # ── Step 4: Column Classification ────────────────────────────────────
  # Reactive dependency only on wiz$col_names (set once in step 3).
  # All other wiz fields (key_columns, expected_columns, col_types_override)
  # are isolated so that ticking a checkbox doesn't rebuild the whole table
  # and scroll the user back to row 1.
  output$step4_column_table <- renderUI({
    cols <- wiz$col_names          # reactive: rebuild only when column list changes
    req(length(cols) > 0)

    isolate({                      # isolate: checkbox/select state reads don't subscribe
      types    <- wiz$col_types_inferred
      overrides <- wiz$col_types_override
      key_cols  <- wiz$key_columns
      exp_cols  <- wiz$expected_columns

      rows <- lapply(seq_along(cols), function(i) {
        col      <- cols[i]
        inf_type <- if (i <= length(types)) types[i] else "character"
        ovr_type <- overrides[[col]] %||% "auto"
        is_key   <- col %in% key_cols
        is_exp   <- length(exp_cols) == 0 || col %in% exp_cols

        fluidRow(class="mb-1 border-bottom pb-1 align-items-center",
          column(3, tags$strong(col, style="font-size:13px;")),
          column(2, span(class="badge bg-light text-dark border", inf_type)),
          column(2,
            selectInput(paste0("s4_type_", i), NULL, width="100%",
              choices=c("auto","character","numeric","date"), selected=ovr_type)
          ),
          column(2,
            checkboxInput(paste0("s4_key_", i), "Key col", value=is_key)
          ),
          column(3,
            checkboxInput(paste0("s4_exp_", i), "Expected", value=is_exp)
          )
        )
      })

      div(
        fluidRow(class="mb-2 fw-semibold text-muted",
          column(3, "Column"), column(2, "Inferred type"),
          column(2, "Override"), column(2, "Key col"), column(3, "Expected")
        ),
        div(style="max-height:500px;overflow-y:auto;", rows)
      )
    })
  })

  # Collect step 4 inputs
  observe({
    cols <- wiz$col_names
    if (length(cols) == 0) return()
    # Bail out if step 4 UI hasn't rendered yet (inputs don't exist).
    # This prevents wiping config-loaded values during edit mode step 3 auto-parse.
    if (is.null(input[[paste0("s4_type_1")]])) return()
    key_cols <- character(0)
    exp_cols <- character(0)
    overrides <- list()
    for (i in seq_along(cols)) {
      col <- cols[i]
      type_val <- input[[paste0("s4_type_", i)]] %||% "auto"
      if (type_val != "auto") overrides[[col]] <- type_val
      if (isTRUE(input[[paste0("s4_key_", i)]])) key_cols <- c(key_cols, col)
      if (isTRUE(input[[paste0("s4_exp_", i)]])) exp_cols <- c(exp_cols, col)
    }
    wiz$col_types_override <- overrides
    wiz$key_columns <- key_cols
    wiz$expected_columns <- exp_cols
  })

  observeEvent(input$step4_select_all_expected, {
    cols <- wiz$col_names
    for (i in seq_along(cols)) updateCheckboxInput(session, paste0("s4_exp_", i), value=TRUE)
    wiz$expected_columns <- cols
  })
  observeEvent(input$step4_select_none_expected, {
    cols <- wiz$col_names
    for (i in seq_along(cols)) updateCheckboxInput(session, paste0("s4_exp_", i), value=FALSE)
    wiz$expected_columns <- character(0)
  })

  # ── Step 5: Column Rules ────────────────────────────────────────────
  # Isolate wiz field reads so accordion doesn't collapse/scroll on every
  # regex test result or allowed-value addition.
  output$step5_column_rules <- renderUI({
    cols <- wiz$col_names          # reactive dependency: col list only
    if (length(cols) == 0)
      return(p(class="text-muted fst-italic", "No columns defined yet. Complete Step 3 first."))

    isolate({
    types <- if (length(wiz$col_types_inferred) > 0) wiz$col_types_inferred else rep("character", length(cols))

    panels <- lapply(seq_along(cols), function(i) {
      col  <- cols[i]
      ctype <- if (!is.null(wiz$col_types_override[[col]])) wiz$col_types_override[[col]] else types[i]
      rules <- wiz$column_rules[[col]] %||% list()

      has_rule <- any(c(!is.null(rules$allowed_values), !is.null(rules$min_value),
                        !is.null(rules$max_value), !is.null(rules$pattern)))

      standard_fields <- tagList(
        if (ctype == "character" || ctype == "unknown") {
          existing_vals <- rules$allowed_values %||% character(0)
          div(class="mt-1",
            tags$label(paste0("Allowed values (QC-09)"), class="form-label form-label-sm"),
            selectizeInput(paste0("s5_allowed_", i), NULL,
              choices  = existing_vals,
              selected = existing_vals,
              multiple = TRUE,
              options  = list(
                create      = TRUE,
                placeholder = "Type a value, press Enter or Tab to add",
                plugins     = list("remove_button")
              ),
              width = "100%"
            )
          )
        },
        if (ctype == "numeric") {
          fluidRow(
            column(4, numericInput(paste0("s5_min_", i), "Min value (QC-10)",
                   value=rules$min_value, min=NA, max=NA)),
            column(4, numericInput(paste0("s5_max_", i), "Max value (QC-10)",
                   value=rules$max_value, min=NA, max=NA))
          )
        }
      )

      adv_fields <- tagList(
        div(class="mt-2",
          tags$label("Regex pattern (QC-13)", class="form-label form-label-sm"),
          div(class="d-flex gap-2",
            textInput(paste0("s5_pattern_", i), NULL,
                      value=rules$pattern %||% "", placeholder="e.g. ^[A-Z]{2}$", width="300px"),
            actionButton(paste0("s5_test_regex_", i), "Test",
                         class="btn btn-outline-secondary btn-sm", style="height:38px;")
          ),
          uiOutput(paste0("s5_regex_result_", i))
        ),
        fluidRow(
          column(4, numericInput(paste0("s5_maxmiss_", i), "Max missing rate",
                 value=rules$max_missing_rate, min=0, max=1, step=0.01)),
          if (ctype == "numeric") {
            column(4, numericInput(paste0("s5_maxmeanshift_", i), "Max mean shift (%)",
                   value=rules$max_numeric_mean_shift_pct, min=0, max=100, step=1))
          }
        )
      )

      bslib::accordion_panel(
        title=col,
        value=paste0("col_", i),
        tagList(
          span(class="badge bg-light text-dark border me-2", style="font-size:10px;", ctype),
          if (has_rule) span(class="badge bg-success text-white", style="font-size:10px;", "rules set"),
          div(class="mt-2", standard_fields),
          bslib::accordion(
            bslib::accordion_panel("Advanced ▼",
              value=paste0("adv_", i), adv_fields
            ),
            open=FALSE
          )
        )
      )
    })

    bslib::accordion(!!!panels, open=FALSE)
    }) # end isolate
  })

  # Regex test observers
  observe({
    cols <- wiz$col_names
    lapply(seq_along(cols), function(i) {
      local({
        ii <- i
        col <- cols[ii]
        observeEvent(input[[paste0("s5_test_regex_", ii)]], {
          pattern <- input[[paste0("s5_pattern_", ii)]] %||% ""
          if (nchar(pattern) == 0) return()
          # Test against sample
          sample_vals <- if (!is.null(wiz$csv_preview_df)) {
            v <- wiz$csv_preview_df[[col]]
            v[!is.na(v) & nchar(as.character(v)) > 0]
          } else character(0)

          result <- tryCatch({
            err_msg <- tryCatch({ grepl(pattern, "test", perl=TRUE); NULL },
                                error=function(e) e$message)
            if (!is.null(err_msg)) {
              list(type="error", msg=paste("Invalid regex:", err_msg))
            } else if (length(sample_vals) == 0) {
              list(type="info", msg="No sample data to test against.")
            } else {
              matches <- grepl(pattern, sample_vals, perl=TRUE)
              n_fail <- sum(!matches)
              if (n_fail == 0) {
                list(type="success", msg=sprintf("Pattern matches all %d non-empty values.", length(sample_vals)))
              } else {
                fail_examples <- paste(head(sample_vals[!matches], 5), collapse="', '")
                list(type="warning", msg=sprintf("Pattern does not match %d value(s): '%s'", n_fail, fail_examples))
              }
            }
          }, error=function(e) list(type="error", msg=e$message))

          output[[paste0("s5_regex_result_", ii)]] <- renderUI({
            cls <- switch(result$type, success="alert-success", warning="alert-warning",
                          error="alert-danger", info="alert-info", "alert-info")
            div(class=paste("alert p-1 mt-1", cls), style="font-size:11px;", result$msg)
          })
        })
      })
    })
  })

  # Collect step 5 inputs
  observe({
    cols <- wiz$col_names
    if (length(cols) == 0) return()
    # Bail out if step 5 UI hasn't rendered yet.
    if (is.null(input[[paste0("s5_maxmiss_1")]])) return()
    types <- wiz$col_types_inferred
    col_rules <- list()
    for (i in seq_along(cols)) {
      col   <- cols[i]
      ctype <- if (!is.null(wiz$col_types_override[[col]])) wiz$col_types_override[[col]]
               else if (i <= length(types)) types[i] else "character"
      rules <- list()
      av <- input[[paste0("s5_allowed_", i)]]
      if (!is.null(av) && length(av) > 0 && any(nchar(av) > 0)) rules$allowed_values <- av
      minv <- input[[paste0("s5_min_", i)]]
      maxv <- input[[paste0("s5_max_", i)]]
      if (!is.null(minv) && !is.na(minv)) rules$min_value <- minv
      if (!is.null(maxv) && !is.na(maxv)) rules$max_value <- maxv
      pat <- input[[paste0("s5_pattern_", i)]]
      if (!is.null(pat) && nchar(pat) > 0) rules$pattern <- pat
      miss <- input[[paste0("s5_maxmiss_", i)]]
      if (!is.null(miss) && !is.na(miss)) rules$max_missing_rate <- miss
      ms   <- input[[paste0("s5_maxmeanshift_", i)]]
      if (!is.null(ms) && !is.na(ms)) rules$max_numeric_mean_shift_pct <- ms / 100
      if (length(rules) > 0) col_rules[[col]] <- rules
    }
    wiz$column_rules <- col_rules
  })

  # ── Step 7: Custom checks ────────────────────────────────────────────
  shinyFiles::shinyFileChoose(input, "wiz_custom_browse",
    roots=file_roots, session=session)

  observeEvent(input$wiz_custom_browse, {
    req(is.list(input$wiz_custom_browse))
    p <- shinyFiles::parseFilePaths(file_roots, input$wiz_custom_browse)
    if (nrow(p) > 0) {
      updateTextInput(session, "wiz_custom_file", value=as.character(p$datapath[1]))
      wiz$custom_checks_file <- as.character(p$datapath[1])
    }
  })

  observeEvent(input$wiz_custom_file, { wiz$custom_checks_file <- input$wiz_custom_file %||% "" })

  # Debounce the custom-checks path before validating it — validation below
  # parses *and sources* the file (to confirm it defines `custom_checks`),
  # which executes the user's script. Without debouncing, that source() call
  # — and any top-level side effects in the script (package loads, file/
  # network I/O, global state changes) — would re-fire on every keystroke,
  # including for every transient partial path typed along the way. Mirrors
  # the folder_input_r/folder_input_d pattern used for the folder path above.
  custom_file_r <- reactive({ input$wiz_custom_file })
  custom_file_d <- shiny::debounce(custom_file_r, 600)

  output$step7_validation_badge <- renderUI({
    path <- custom_file_d() %||% ""
    if (nchar(path) == 0) return(NULL)
    if (!safe_file_exists(path))
      return(div(class="alert alert-danger p-1 mt-1", style="font-size:12px;",
                 "✗ File not found."))
    parse_err <- tryCatch({ parse(file=path); NULL }, error=function(e) e$message)
    if (!is.null(parse_err))
      return(div(class="alert alert-danger p-1 mt-1", style="font-size:12px;",
                 paste("✗ R syntax error:", parse_err)))
    env <- new.env(parent=baseenv())
    tryCatch(source(path, local=env), error=function(e) NULL)
    fn <- tryCatch(get("custom_checks", envir=env), error=function(e) NULL)
    if (is.null(fn) || !is.function(fn))
      return(div(class="alert alert-danger p-1 mt-1", style="font-size:12px;",
                 "✗ File must define a function named 'custom_checks'."))
    sig <- paste(names(formals(fn)), collapse=", ")
    div(class="alert alert-success p-1 mt-1", style="font-size:12px;",
        sprintf("✓ Valid — custom_checks(%s)", sig))
  })

  # ── Step 8: Review and Save ──────────────────────────────────────────
  output$step8_summary <- renderUI({
    bslib::card(bslib::card_body(
      fluidRow(
        column(6, tags$dl(
          tags$dt("Dataset"), tags$dd(wiz$dataset_name),
          tags$dt("Format"),  tags$dd(toupper(wiz$format)),
          tags$dt("Location"),tags$dd(if(nchar(wiz$folder%||%"")>0) wiz$folder else wiz$current_file)
        )),
        column(6, tags$dl(
          tags$dt("Columns"),      tags$dd(sprintf("%d total, %d expected, %d key",
                                            length(wiz$col_names),
                                            length(wiz$expected_columns),
                                            length(wiz$key_columns))),
          tags$dt("Column rules"), tags$dd(sprintf("%d configured", length(wiz$column_rules))),
          tags$dt("Custom checks"),tags$dd(if(nchar(wiz$custom_checks_file%||%"")>0) basename(wiz$custom_checks_file) else "none")
        ))
      )
    ))
  })

  output$yaml_preview <- renderText({
    yaml_preview_text(wiz, wiz$extra_keys)
  })

  observeEvent(input$wizard_save, {
    nm     <- wiz$dataset_name
    cd     <- config_dir()
    path   <- file.path(cd, paste0(nm, ".yml"))
    dir.create(cd, showWarnings=FALSE, recursive=TRUE)
    tryCatch({
      write_config(wiz, wiz$extra_keys, path)
      showNotification(sprintf("Saved: %s", path), type="message", duration=4)
      rv$wizard_open <- FALSE
      rv$active_section <- "datasets"
      rv$active_dataset <- nm
      rv$dataset_refresh <- Sys.time()
    }, error=function(e) {
      showModal(modalDialog(
        title="Save failed",
        paste("Could not write config:", e$message),
        easyClose=TRUE
      ))
    })
  })

  wiz
}

# Helper to collect inputs from various steps
collect_step_inputs <- function(input, wiz, gcfg) {
  # Bail if step 6 UI hasn't rendered — prevents wiping config-loaded rule_overrides
  # when Next is clicked on steps 1-5 (same guard pattern as step 4/5 collectors).
  if (is.null(input$wiz_ro_max_missing)) return()

  # Rule overrides from step 6
  dr <- gcfg$default_rules %||% list()
  overrides <- list()
  # Returns the input value if it is set and differs from the default, else NULL.
  # Assigning NULL to an absent list key is a no-op, so only changed keys persist
  # (a pure helper — avoids the `<<-` an accumulating closure would need).
  chk_num <- function(input_id, dflt) {
    v <- input[[input_id]]
    if (!is.null(v) && !is.na(v) && !identical(v, dflt)) v else NULL
  }
  overrides[["max_missing_rate"]]               <- chk_num("wiz_ro_max_missing",    dr$max_missing_rate %||% 0.05)
  overrides[["max_non_numeric_rate"]]           <- chk_num("wiz_ro_max_nonnumeric", dr$max_non_numeric_rate %||% 0.01)
  overrides[["min_row_count"]]                  <- chk_num("wiz_ro_min_rows",       dr$min_row_count %||% 0)
  v <- input$wiz_ro_max_rowchg;   dflt <- round((dr$max_row_count_change_pct %||% 0.10)*100,2)
  if (!is.null(v) && !is.na(v) && !identical(v, dflt)) overrides$max_row_count_change_pct <- v/100
  v <- input$wiz_ro_max_meanshift; dflt <- round((dr$max_numeric_mean_shift_pct %||% 0.20)*100,2)
  if (!is.null(v) && !is.na(v) && !identical(v, dflt)) overrides$max_numeric_mean_shift_pct <- v/100
  overrides[["max_missing_rate_change_pp"]]     <- chk_num("wiz_ro_max_misschg",   dr$max_missing_rate_change_pp %||% 2.0)
  overrides[["max_non_numeric_rate_change_pp"]] <- chk_num("wiz_ro_max_nonnumchg", dr$max_non_numeric_rate_change_pp %||% 1.0)
  overrides[["type_inference_threshold"]]       <- chk_num("wiz_ro_type_inf",      dr$type_inference_threshold %||% 0.90)
  for (pair in list(
    list("flag_new_columns","wiz_ro_flag_new",TRUE),
    list("flag_dropped_columns","wiz_ro_flag_drop",TRUE),
    list("flag_type_changes","wiz_ro_flag_type",TRUE),
    list("flag_column_order_change","wiz_ro_flag_order",TRUE)
  )) {
    v <- input[[pair[[2]]]]; dflt <- pair[[3]]
    if (!is.null(v) && !identical(isTRUE(v), dflt)) overrides[[pair[[1]]]] <- isTRUE(v)
  }
  wiz$rule_overrides <- overrides
}
