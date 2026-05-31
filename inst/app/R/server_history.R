# History panel server logic (spec §18)

server_history <- function(input, output, session, rv, config_dir, gcfg_rv) {

  hist_data  <- reactiveVal(data.frame())
  hist_limit <- reactiveVal(50L)

  # Load / reload history
  load_history <- function() {
    db_path <- normalizePath(
      gcfg_rv()$snapshot_db %||% "data/snapshots.sqlite",
      mustWork = FALSE
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

    # Build direct /dq_reports/<file> links — avoids Shiny round-trip and popup blocking
    ts_raw   <- gsub("[^0-9]", "", substr(df$run_timestamp, 1, 19))
    ts_slug  <- paste0(substr(ts_raw, 1, 8), "_", substr(ts_raw, 9, 14))
    filename <- paste0(df$dataset_name, "_", ts_slug, ".html")

    display_df <- data.frame(
      ` ` = sprintf(
        '<input type="checkbox" class="hist-check" data-id="%d" data-ds="%s" onchange="window.__dqHC(this)"/>',
        df$id, df$dataset_name),
      Dataset = df$dataset_name,
      Date    = df$run_timestamp,
      File    = df$file_name,
      Status  = vapply(df$overall_status, status_badge_html, character(1)),
      Fails   = df$check_fail_count,
      Rows    = df$row_count,
      Report  = sprintf(
        '<a href="javascript:void(0)" onclick="window.open(\'/dq_reports/%s\',\'_blank\')">Open</a>',
        filename),
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
    db_path    <- normalizePath(gcfg_rv()$snapshot_db %||% "data/snapshots.sqlite", mustWork = FALSE)
    report_dir <- normalizePath(gcfg_rv()$report_output_dir %||% "reports/", mustWork = FALSE)
    cd         <- config_dir()
    prev_id    <- min(as.integer(c(id1, id2)))
    curr_id    <- max(as.integer(c(id1, id2)))
    showNotification("Starting drift comparison...", type = "message", duration = 4)
    rv_drift$proc       <- callr::r_bg(
      func = function(dn, p, c, db, cd) {
        dqcheckr::compare_snapshots(dn,
          snapshot_id_prev = p, snapshot_id_curr = c,
          db_path = db, config_dir = cd,
          report = TRUE, open_report = FALSE)
      },
      args    = list(dn = ds_name, p = prev_id, c = curr_id, db = db_path, cd = cd),
      package = TRUE
    )
    rv_drift$report_dir <- report_dir
    rv_drift$ds_name    <- ds_name
  }

  # Compare drift — async, non-blocking
  rv_drift <- reactiveValues(proc = NULL, report_dir = NULL, ds_name = NULL)

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
      showNotification(paste("Drift comparison failed:", conditionMessage(e)),
                       type = "error", duration = 10)
      NULL
    })
    rv_drift$proc <- NULL
    if (is.null(result)) return()

    ds_name     <- rv_drift$ds_name
    drift_files <- list.files(rv_drift$report_dir,
                              pattern    = paste0("^drift_", ds_name, "_.*\\.html$"),
                              full.names = FALSE)
    if (length(drift_files) > 0) {
      url <- paste0("/dq_reports/", drift_files[length(drift_files)])
      showModal(modalDialog(
        title  = "Drift comparison complete",
        tags$a(href = url, target = "_blank", class = "btn btn-primary",
               "Open drift report ↗"),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    } else {
      showNotification(
        "Drift comparison complete. Report not found in reports directory.",
        type = "warning"
      )
    }
  })
}
