# History panel server logic (spec §18)

server_history <- function(input, output, session, rv, config_dir, gcfg_rv) {

  hist_data  <- reactiveVal(data.frame())
  hist_limit <- reactiveVal(50L)

  # Load / reload history
  load_history <- function() {
    db_path <- resolve_infra_path(
      gcfg_rv()$snapshot_db, config_dir(),
      default = "data/snapshots.sqlite", mustWork = FALSE
    )
    tryCatch({
      df <- read_all_snapshot_history(db_path, n = hist_limit())
      hist_data(df)
    }, error = function(e) hist_data(data.frame()))
  }

  # Refresh when section activated or after a run
  observe({
    rv$active_section
    rv$history_refresh
    load_history()
  })

  observeEvent(input$history_load_more, {
    hist_limit(hist_limit() + 50L)
    load_history()
  })

  output$history_table <- DT::renderDataTable({
    df <- hist_data()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Message = "No run history found. Run a quality check first."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }

    # Build direct report links — avoids Shiny round-trip and popup blocking.
    # escape = FALSE below applies to ALL columns, so anything interpolated here
    # must be escaped by hand — file_name in particular is supplier-controlled.
    # Prefer the filename recorded by dqcheckr >= 0.2.3; reconstruct from the
    # run timestamp only for older rows. Rows whose render failed get no link.
    filename <- ifelse(!is.na(df$report_file), df$report_file,
                       make_report_filename(df$dataset_name, df$run_timestamp))
    failed   <- !is.na(df$render_status) & df$render_status == "failed"

    # Per-dataset URL prefix: datasets with a report_output_dir override are
    # served under their own resource path (see report_url_prefix()).
    cd   <- config_dir()
    gcfg <- gcfg_rv()
    prefixes <- vapply(unique(df$dataset_name), function(ds) {
      known <- tryCatch(read_config(file.path(cd, paste0(ds, ".yml")))$known,
                        error = function(e) NULL)
      report_url_prefix(ds, known$report_output_dir, cd, gcfg)
    }, character(1))
    row_prefix <- unname(prefixes[df$dataset_name])

    display_df <- data.frame(
      ` ` = sprintf(
        '<input type="checkbox" class="hist-check" data-id="%d" data-ds="%s" onchange="window.__dqHC(this)"/>',
        df$id, html_escape(df$dataset_name, attribute = TRUE)),
      Dataset = html_escape(df$dataset_name),
      Date    = utc_to_local_display(df$run_timestamp),
      File    = html_escape(df$file_name),
      Status  = vapply(df$overall_status, status_badge_html, character(1)),
      Fails   = df$check_fail_count,
      Rows    = df$row_count,
      Report  = ifelse(failed,
        '<span class="text-muted fst-italic">render failed</span>',
        sprintf(
          '<a href="javascript:void(0)" onclick="window.open(\'/%s/%s\',\'_blank\')">Open</a>',
          row_prefix, url_encode_filename(filename))),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    DT::datatable(display_df,
      escape    = FALSE, rownames = FALSE, selection = "none",
      filter    = "top",
      options   = list(pageLength = 50, scrollX = TRUE, dom = "ftip"),
      class     = "compact stripe hover"
    )
  })

  # Shared drift-launch helper
  launch_drift <- function(ds_name, id1, id2) {
    cd         <- config_dir()
    db_path    <- resolve_infra_path(gcfg_rv()$snapshot_db, cd,
                                     default = "data/snapshots.sqlite", mustWork = FALSE)
    report_dir <- resolve_infra_path(gcfg_rv()$report_output_dir, cd,
                                     default = "reports/", mustWork = FALSE)
    prev_id    <- min(as.integer(c(id1, id2)))
    curr_id    <- max(as.integer(c(id1, id2)))
    showNotification("Starting drift comparison...", type = "message", duration = 4)
    drift_log           <- tempfile("dqcheckr_drift_", fileext = ".log")
    rv_drift$proc       <- callr::r_bg(
      func = function(dn, p, c, db, cd) {
        dqcheckr::compare_snapshots(dn,
          snapshot_id_prev = p, snapshot_id_curr = c,
          db_path = db, config_dir = cd,
          report = TRUE, open_report = FALSE)
      },
      args    = list(dn = ds_name, p = prev_id, c = curr_id, db = db_path, cd = cd),
      stdout  = drift_log,
      stderr  = "2>&1",
      # Run in the deployment root so dqcheckr resolves any relative paths the
      # same way the CLI does (shiny::runApp has changed this process's getwd()).
      wd      = deployment_root(cd),
      package = TRUE
    )
    rv_drift$log_path   <- drift_log
    rv_drift$report_dir <- report_dir
    rv_drift$ds_name    <- ds_name
  }

  # Compare drift — async, non-blocking
  rv_drift <- reactiveValues(proc = NULL, report_dir = NULL, ds_name = NULL, log_path = NULL)

  observeEvent(input$history_compare, {
    ids <- input$hist_selected_ids
    req(length(ids) == 2)
    df   <- hist_data()
    row1 <- df[df$id == as.integer(ids[1]), ]
    req(nrow(row1) > 0)
    launch_drift(row1$dataset_name[1], ids[1], ids[2])
  })

  # Drift request from dataset panel compare button
  observeEvent(rv$drift_request, {
    req(!is.null(rv$drift_request))
    req(length(rv$drift_request$ids) == 2)
    launch_drift(rv$drift_request$ds, rv$drift_request$ids[1], rv$drift_request$ids[2])
  })

  # Poll for drift completion — 500ms intervals, non-blocking
  observe({
    req(!is.null(rv_drift$proc))
    invalidateLater(500)
    proc <- rv_drift$proc
    if (is.null(proc) || proc$is_alive()) return()

    result <- tryCatch(proc$get_result(), error = function(e) {
      last_line <- tryCatch({
        lp <- rv_drift$log_path
        if (!is.null(lp) && file.exists(lp)) {
          lines <- readLines(lp, warn = FALSE)
          lines <- lines[nchar(trimws(lines)) > 0]
          if (length(lines) > 0) tail(lines, 1) else ""
        } else ""
      }, error = function(e2) "")
      msg <- if (nchar(last_line) > 0) last_line else conditionMessage(e)
      showNotification(paste("Drift comparison failed:", msg),
                       type = "error", duration = 10)
      NULL
    })
    rv_drift$proc <- NULL
    if (is.null(result)) return()

    # dqcheckr (>= 0.2.5) returns the rendered report's path directly; link to
    # it rather than re-deriving the filename with a slug regex (which broke when
    # the drift filename gained its snapshot ids). NULL means no report was
    # written (e.g. Quarto unavailable) -- the drift still ran.
    url <- drift_report_url(result)
    if (!is.null(url)) {
      showModal(modalDialog(
        title  = "Drift comparison complete",
        tags$a(href = url, target = "_blank", class = "btn btn-primary",
               "Open drift report ↗"),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    } else {
      showNotification(
        "Drift comparison complete, but no HTML report was written (Quarto may be unavailable).",
        type = "warning"
      )
    }
  })
}
