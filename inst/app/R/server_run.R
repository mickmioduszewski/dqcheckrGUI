# Run panel server logic (spec §17)

server_run <- function(input, output, session, rv, config_dir, gcfg_rv) {

  rv_run <- reactiveValues(
    r_process  = NULL,
    log_path   = NULL,
    log_lines  = character(0),
    log_bytes  = NULL,             # last seen log size — skip unchanged reads
    status     = "idle",
    report_path = NULL,
    report_prefix = "dq_reports",  # URL prefix for the running dataset's reports
    error_msg  = NULL
  )

  # Update dataset selector
  observe({
    datasets <- list_dataset_configs(config_dir())
    selected <- if (!is.null(rv$run_preselect) && rv$run_preselect %in% datasets)
                  rv$run_preselect else datasets[1]
    updateSelectInput(session, "run_dataset",
                      choices=if(length(datasets)>0) datasets else list("(no datasets)"=""),
                      selected=selected)
  })

  # Pre-run validation
  output$run_precheck_status <- renderUI({
    ds <- input$run_dataset
    if (is.null(ds) || nchar(ds) == 0)
      return(div(class="alert alert-warning p-2 mb-2", style="font-size:12px;",
                 "Select a dataset to run."))
    cd <- config_dir()
    cfg_path <- file.path(cd, paste0(ds, ".yml"))
    gcfg <- gcfg_rv()

    issues <- character(0)
    if (!safe_file_exists(cfg_path))
      issues <- c(issues, paste("Config file not found:", cfg_path))

    ds_db <- NULL
    if (safe_file_exists(cfg_path)) {
      cfg_result <- tryCatch(read_config(cfg_path), error = function(e) e)
      if (inherits(cfg_result, "error")) {
        issues <- c(issues, paste("Config could not be parsed:", conditionMessage(cfg_result)))
      } else {
        cfg <- cfg_result$known
        ds_db <- cfg$snapshot_db
        # Resolve relative paths against deployment root before checking —
        # the Shiny process's getwd() is the package install dir, not the
        # project root, so raw relative paths would always fail here.
        if (nchar(cfg$folder %||% "") > 0 &&
            !safe_dir_exists(resolve_infra_path(cfg$folder, cd)))
          issues <- c(issues, paste("Folder not found:", cfg$folder))
        if (nchar(cfg$current_file %||% "") > 0 &&
            !safe_file_exists(resolve_infra_path(cfg$current_file, cd)))
          issues <- c(issues, paste("Current file not found:", cfg$current_file))
      }
    }

    # Honour a per-dataset snapshot_db override — validating only the global
    # path let a broken override sail through precheck and fail at run time.
    db_path <- effective_db_path(cd, gcfg, ds_db)
    if (nchar(db_path) > 0) {
      db_parent <- dirname(db_path)
      if (!safe_dir_exists(db_parent))
        issues <- c(issues, paste("Snapshot DB directory not found:", db_parent))
    }

    if (length(issues) == 0) {
      div(class="alert alert-success p-2 mb-2", style="font-size:12px;",
          "✓ Configuration looks good.")
    } else {
      div(class="alert alert-danger p-2 mb-2", style="font-size:12px;",
          tags$strong("Issues found:"),
          tags$ul(lapply(issues, tags$li)))
    }
  })

  # Run button — disabled while a run is in progress
  output$run_start_btn <- renderUI({
    ds  <- input$run_dataset
    cd  <- config_dir()
    ok  <- !is.null(ds) && nchar(ds) > 0 &&
           safe_file_exists(file.path(cd, paste0(ds, ".yml"))) &&
           rv_run$status != "running"
    btn <- actionButton("run_start", "▶ Run check", class = "btn btn-primary")
    if (!ok) tagAppendAttributes(btn, disabled = "disabled") else btn
  })

  # Run start
  observeEvent(input$run_start, {
    req(input$run_dataset, rv_run$status != "running")
    ds <- input$run_dataset
    cd <- config_dir()

    rv_run$log_path  <- tempfile("dqcheckr_run_", fileext=".log")
    rv_run$log_lines <- character(0)
    rv_run$log_bytes <- NULL
    rv_run$status    <- "running"
    rv_run$report_path <- NULL
    rv_run$error_msg   <- NULL
    # URL prefix for this dataset's reports (per-dataset report_output_dir
    # overrides are served under their own resource path).
    known <- read_dataset_known(cd, ds)
    rv_run$report_prefix <- report_url_prefix(ds, known$report_output_dir,
                                              cd, gcfg_rv())

    rv_run$r_process <- callr::r_bg(
      func   = function(dn, cd) dqcheckr::run_dq_check(dn, config_dir=cd),
      args   = list(dn=ds, cd=cd),
      stdout = rv_run$log_path,
      stderr = "2>&1",
      # Run in the deployment root so dqcheckr resolves relative snapshot_db /
      # report_output_dir the same way the CLI does (shiny::runApp has changed
      # this process's getwd() to the installed app directory).
      wd      = deployment_root(cd),
      package = TRUE
    )
  })

  # Collect a finished run's result and move out of the "running" state.
  # Extracted so the Stop handler can call it too: a run can finish inside the
  # 200ms poll window, and if the user clicks Stop in that gap we must report
  # the real (already-on-disk) PASS/FAIL result rather than "stopped" (B-12).
  finish_completed_run <- function() {
    proc <- rv_run$r_process
    # On failure, surface the *actual* cause — either the condition message
    # from the background process, or (more often informative) the last
    # non-blank line of its log — rather than a generic "check the log"
    # message. Mirrors the pattern already used for drift failures in
    # server_history.R's poll observer.
    result <- tryCatch(proc$get_result(), error = function(e) {
      last_line <- tryCatch({
        lp <- rv_run$log_path
        if (!is.null(lp) && file.exists(lp)) {
          lines <- readLines(lp, warn = FALSE)
          lines <- lines[nchar(trimws(lines)) > 0]
          if (length(lines) > 0) tail(lines, 1) else ""
        } else ""
      }, error = function(e2) "")
      msg <- if (nchar(last_line) > 0) last_line else conditionMessage(e)
      rv_run$error_msg <- paste("Run failed:", msg)
      NULL
    })
    rv_run$status <- if (!is.null(result)) as.character(result$status) else "error"
    if (!is.null(result)) rv_run$report_path <- result$report_path
    rv$history_refresh <- Sys.time()
    # Re-register report resource paths: the reports directory may only now
    # exist (dqcheckr creates it on first run), and per-dataset dirs may
    # have received their first report.
    register_all_report_paths(config_dir(), gcfg_rv())
  }

  # 200ms polling observer
  observe({
    req(rv_run$status == "running")
    invalidateLater(200)

    # Only re-read (and re-scroll) when the log actually grew — reading the
    # whole file every tick is O(n^2) I/O over the run.
    if (safe_file_exists(rv_run$log_path)) {
      sz <- file.size(rv_run$log_path)
      if (!identical(sz, rv_run$log_bytes)) {
        rv_run$log_bytes <- sz
        rv_run$log_lines <- readLines(rv_run$log_path, warn=FALSE)
        session$sendCustomMessage("scroll_log", list())
      }
    }

    proc <- rv_run$r_process
    if (!is.null(proc) && !proc$is_alive()) finish_completed_run()
  })

  # Stop button
  output$run_stop_btn <- renderUI({
    if (rv_run$status == "running")
      actionButton("run_stop", "■ Stop", class="btn btn-danger btn-sm")
  })

  observeEvent(input$run_stop, {
    showModal(modalDialog(
      title="Stop run?",
      "Stop the current run? The snapshot will not be written if the run has not yet completed.",
      footer=tagList(
        modalButton("Keep running"),
        actionButton("run_stop_confirm", "Stop", class="btn btn-danger")
      )
    ))
  })

  observeEvent(input$run_stop_confirm, {
    removeModal()
    proc <- rv_run$r_process
    # The run may have finished between the poll ticks while the confirm dialog
    # was open. Its result is already written to disk — collect it rather than
    # discarding a completed run's PASS/FAIL and reporting "stopped" (B-12).
    if (!is.null(proc) && !proc$is_alive()) {
      finish_completed_run()
      return()
    }
    if (!is.null(proc)) tryCatch(proc$kill(), error=function(e) NULL)
    rv_run$status <- "stopped"
    rv_run$log_lines <- c(rv_run$log_lines, "\n[Run stopped by user]")
  })

  # Status area
  output$run_status_area <- renderUI({
    s <- rv_run$status
    if (s == "idle") return(NULL)
    div(class="d-flex gap-3 align-items-center mt-2",
      status_badge(toupper(s)),
      if (s %in% c("PASS","WARN","FAIL") && !is.null(rv_run$report_path)) {
        report_url <- paste0("/", rv_run$report_prefix, "/",
                             utils::URLencode(basename(rv_run$report_path), reserved = TRUE))
        div(
          tags$a(href=report_url, target="_blank",
                 class="btn btn-outline-primary btn-sm", "Open report ↗"),
          actionButton("run_view_log", "View log",
                       class="btn btn-outline-secondary btn-sm ms-2")
        )
      } else if (s == "stopped") {
        span(class="text-muted fst-italic", "Run stopped by user.")
      } else if (s == "error") {
        span(class="text-danger", rv_run$error_msg %||% "Run failed.")
      }
    )
  })

  # Log output
  output$run_log_area <- renderUI({
    if (rv_run$status == "idle") return(NULL)
    div(
      verbatimTextOutput("run_log"),
    )
  })

  output$run_log <- renderText({
    req(length(rv_run$log_lines) > 0)
    paste(rv_run$log_lines, collapse="\n")
  })

  observeEvent(input$run_view_log, {
    req(rv_run$log_path, safe_file_exists(rv_run$log_path))
    log_text <- paste(readLines(rv_run$log_path, warn=FALSE), collapse="\n")
    showModal(modalDialog(
      title="Run log",
      pre(log_text, style="font-size:11px;max-height:500px;overflow-y:auto;"),
      easyClose=TRUE, size="xl"
    ))
  })
}
