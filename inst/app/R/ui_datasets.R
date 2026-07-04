# Dataset list and dataset panel UI (spec §7)

ui_dataset_panel <- function(dataset_name, config, last_runs, gcfg,
                             report_prefix = "dq_reports") {
  format_str  <- toupper(config$known$format %||% "CSV")
  loc_str     <- if (nchar(config$known$folder %||% "") > 0)
                   config$known$folder
                 else config$known$current_file %||% "(not set)"
  # Escaped once for every onclick JS-string interpolation below — see
  # js_string_escape() for why this matters even though is_valid_r_name()
  # constrains names created via the wizard.
  ds_js <- js_string_escape(dataset_name)

  tagList(
    div(class="d-flex align-items-center gap-3 mb-3",
      h4(dataset_name, style="margin:0;"),
      span(style="font-size:12px;background:#e9ecef;padding:2px 8px;border-radius:4px;", format_str)
    ),
    p(class="text-muted", style="font-size:13px;margin-bottom:4px;",
      tags$strong("Location: "), loc_str),

    div(class="d-flex gap-2 mt-3 mb-4",
      tags$button("Edit config",
        class="btn btn-outline-secondary btn-sm",
        onclick=sprintf("Shiny.setInputValue('ds_action', {action:'edit', ds:'%s', ts:Date.now()}, {priority:'event'});", ds_js)),
      tags$button("▶ Run check",
        class="btn btn-primary btn-sm",
        onclick=sprintf("Shiny.setInputValue('ds_action', {action:'run', ds:'%s', ts:Date.now()}, {priority:'event'});", ds_js))
    ),

    h6("Recent runs", class="text-muted"),
    if (nrow(last_runs) == 0) {
      p(class="text-muted fst-italic", "No runs recorded yet.")
    } else {
      local({
        # Prefer the filename recorded by dqcheckr >= 0.2.3; reconstruct from
        # the run timestamp only for older rows. Failed renders get no link.
        filename <- ifelse(!is.na(last_runs$report_file),
                           last_runs$report_file,
                           make_report_filename(dataset_name, last_runs$run_timestamp))
        failed   <- !is.na(last_runs$render_status) &
                    last_runs$render_status == "failed"
        # This data.frame is rendered with escape = FALSE (all columns), so
        # every interpolated value is escaped by hand; file_name is
        # supplier-controlled. ds_js is JS-string-escaped, then
        # attribute-escaped for the raw onchange attribute it sits in.
        tagList(
          DT::datatable(
            data.frame(
              ` ` = sprintf(
                '<input type="checkbox" class="drift-check" data-id="%d" data-ds="%s" onchange="window.__dqDC(\'%s\')"/>',
                last_runs$id, html_escape(dataset_name, attribute = TRUE),
                html_escape(ds_js, attribute = TRUE)),
              Date   = utc_to_local_display(last_runs$run_timestamp),
              File   = html_escape(last_runs$file_name),
              Status = vapply(last_runs$overall_status, status_badge_html, character(1)),
              Fails  = last_runs$check_fail_count,
              Report = ifelse(failed,
                '<span class="text-muted fst-italic">render failed</span>',
                sprintf(
                  '<a href="javascript:void(0)" onclick="window.open(\'/%s/%s\',\'_blank\')">Open</a>',
                  report_prefix, url_encode_filename(filename))),
              stringsAsFactors = FALSE, check.names = FALSE
            ),
            escape = FALSE, rownames = FALSE, selection = "none",
            options = list(dom="t", ordering=FALSE, pageLength=5, scrollX=TRUE),
            class = "compact stripe hover"
          ),
          div(class="d-flex gap-2 mt-2 align-items-center",
            tags$button("Compare drift ▶",
              id    = paste0("compare_drift_", dataset_name),
              class = "btn btn-outline-info btn-sm",
              disabled = "",
              onclick = sprintf(
                "var ids=Array.from(document.querySelectorAll('.drift-check:checked')).map(function(c){return c.getAttribute('data-id');});Shiny.setInputValue('ds_action',{action:'compare',ds:'%s',ids:ids,ts:Date.now()},{priority:'event'});",
                ds_js)),
            span(class="text-muted fst-italic", style="font-size:12px;",
                 "Select 2 runs above to compare")
          )
        )
      })
    },
    div(class="mt-2",
      tags$a("View all in History →", href="#", class="btn btn-link btn-sm p-0",
        onclick=sprintf("Shiny.setInputValue('ds_action', {action:'history', ds:'%s', ts:Date.now()}, {priority:'event'}); return false;", ds_js))
    )
  )
}
