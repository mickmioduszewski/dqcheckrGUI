# Global config server logic (spec §19)

server_global <- function(input, output, session, rv, config_dir, gcfg_rv) {

  # shinyFiles 0.9.3 bug: roots must be a plain named vector (see server_wizard.R).
  roots <- c(
    Project = deployment_root(isolate(config_dir())),
    Home    = path.expand("~"),
    shinyFiles::getVolumes()()
  )

  shinyFiles::shinyFileChoose(input, "gcfg_db_browse",
    roots=roots, session=session)
  shinyFiles::shinyDirChoose(input, "gcfg_dir_browse",
    roots=roots, session=session)

  observeEvent(input$gcfg_db_browse, {
    req(is.list(input$gcfg_db_browse))
    p <- shinyFiles::parseFilePaths(roots, input$gcfg_db_browse)
    if (nrow(p) > 0) updateTextInput(session, "gcfg_snapshot_db", value=as.character(p$datapath[1]))
  })

  observeEvent(input$gcfg_dir_browse, {
    req(is.list(input$gcfg_dir_browse))
    p <- shinyFiles::parseDirPath(roots, input$gcfg_dir_browse)
    if (length(p) > 0) updateTextInput(session, "gcfg_report_dir", value=as.character(p[1]))
  })

  # Validation — resolve relative paths against deployment root so that
  # relative values like "data/snapshots.sqlite" validate correctly.
  iv_gcfg <- shinyvalidate::InputValidator$new()
  iv_gcfg$add_rule("gcfg_snapshot_db", function(v) {
    if (is.null(v) || !is.character(v) || nchar(v) == 0) return(NULL)
    parent <- dirname(resolve_infra_path(v, config_dir()))
    if (!safe_dir_exists(parent)) sprintf("Directory not found: %s", parent)
  })
  iv_gcfg$add_rule("gcfg_report_dir", function(v) {
    if (is.null(v) || !is.character(v) || nchar(v) == 0) return(NULL)
    if (!safe_dir_exists(resolve_infra_path(v, config_dir())))
      "Directory not found. Create it first or change path."
  })
  iv_gcfg$enable()

  # Render validation message outputs (replacing sv_output which doesn't exist)
  output$sv_gcfg_snapshot_db <- renderUI({
    v <- input$gcfg_snapshot_db
    if (is.null(v) || nchar(v) == 0) return(NULL)
    parent <- dirname(resolve_infra_path(v, config_dir()))
    if (!safe_dir_exists(parent))
      tags$div(class="text-danger", style="font-size:12px;",
               sprintf("Directory not found: %s", parent))
  })
  output$sv_gcfg_report_dir <- renderUI({
    v <- input$gcfg_report_dir
    if (is.null(v) || nchar(v) == 0) return(NULL)
    if (!safe_dir_exists(resolve_infra_path(v, config_dir())))
      tags$div(class="text-danger", style="font-size:12px;",
               "Directory not found. Create it first or change path.")
  })

  observeEvent(input$gcfg_save, {
    if (!iv_gcfg$is_valid()) {
      showNotification("Fix validation errors before saving.", type="error")
      return()
    }
    gcfg <- list(
      snapshot_db       = input$gcfg_snapshot_db,
      report_output_dir = input$gcfg_report_dir,
      default_rules     = list(
        type_inference_threshold       = input$gcfg_type_inf_threshold,
        max_missing_rate               = input$gcfg_max_missing_rate,
        max_non_numeric_rate           = input$gcfg_max_non_numeric_rate,
        min_row_count                  = as.integer(input$gcfg_min_row_count),
        max_row_count_change_pct       = input$gcfg_max_row_count_chg / 100,
        max_numeric_mean_shift_pct     = input$gcfg_max_mean_shift / 100,
        max_missing_rate_change_pp     = input$gcfg_max_missing_chg,
        max_non_numeric_rate_change_pp = input$gcfg_max_nonnumeric_chg,
        flag_new_columns               = isTRUE(input$gcfg_flag_new_cols),
        flag_dropped_columns           = isTRUE(input$gcfg_flag_drop_cols),
        flag_type_changes              = isTRUE(input$gcfg_flag_type_chg),
        flag_column_order_change       = isTRUE(input$gcfg_flag_col_order)
      )
    )
    tryCatch({
      write_global_config(gcfg, global_config_path(config_dir()))
      register_report_resource_path(gcfg$report_output_dir, config_dir())
      gcfg_rv(gcfg)
      showNotification("Global config saved.", type="message", duration=3)
    }, error = function(e) {
      showModal(modalDialog(
        title="Save failed",
        paste("Could not write global config:", e$message),
        easyClose=TRUE
      ))
    })
  })
}
