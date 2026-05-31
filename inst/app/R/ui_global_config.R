# Global config editor UI (spec §19)

ui_global_config <- function(gcfg) {
  dr <- gcfg$default_rules %||% list()

  tagList(
    h4("Global Configuration", style="margin-bottom:20px;"),

    bslib::card(
      bslib::card_header("Infrastructure Paths"),
      bslib::card_body(
        fluidRow(
          column(8,
            tags$label("Snapshot database", class="form-label fw-semibold"),
            div(class="d-flex gap-2",
              textInput("gcfg_snapshot_db", NULL,
                        value = gcfg$snapshot_db %||% "data/snapshots.sqlite",
                        width = "100%"),
              shinyFiles::shinyFilesButton("gcfg_db_browse", "Browse",
                "Select snapshot database file", FALSE,
                style="white-space:nowrap;height:38px;")
            ),
            uiOutput("sv_gcfg_snapshot_db")
          )
        ),
        fluidRow(
          column(8,
            tags$label("Report output directory", class="form-label fw-semibold mt-2"),
            div(class="d-flex gap-2",
              textInput("gcfg_report_dir", NULL,
                        value = gcfg$report_output_dir %||% "reports/",
                        width = "100%"),
              shinyFiles::shinyDirButton("gcfg_dir_browse", "Browse",
                "Select report output directory",
                style="white-space:nowrap;height:38px;")
            ),
            uiOutput("sv_gcfg_report_dir")
          )
        )
      )
    ),

    bslib::card(
      class = "mt-3",
      bslib::card_header("Default Rule Thresholds"),
      bslib::card_body(
        h6("Single-snapshot thresholds", class="text-muted mt-1"),
        fluidRow(
          column(4, numericInput("gcfg_max_missing_rate", "Max missing rate", value=dr$max_missing_rate %||% 0.05, min=0, max=1, step=0.01)),
          column(4, numericInput("gcfg_max_non_numeric_rate", "Max non-numeric rate", value=dr$max_non_numeric_rate %||% 0.01, min=0, max=1, step=0.01)),
          column(4, numericInput("gcfg_min_row_count", "Min row count (0=disabled)", value=dr$min_row_count %||% 0, min=0, step=1))
        ),
        h6("Version comparison thresholds", class="text-muted mt-3"),
        fluidRow(
          column(4, numericInput("gcfg_max_row_count_chg", "Max row count change (%)", value=round((dr$max_row_count_change_pct %||% 0.10)*100,2), min=0, max=100, step=1)),
          column(4, numericInput("gcfg_max_mean_shift", "Max mean shift (%)", value=round((dr$max_numeric_mean_shift_pct %||% 0.20)*100,2), min=0, max=100, step=1)),
          column(4, numericInput("gcfg_max_missing_chg", "Max missing rate change (pp)", value=dr$max_missing_rate_change_pp %||% 2.0, min=0, step=0.1))
        ),
        fluidRow(
          column(4, numericInput("gcfg_max_nonnumeric_chg", "Max non-numeric change (pp)", value=dr$max_non_numeric_rate_change_pp %||% 1.0, min=0, step=0.1)),
          column(4, numericInput("gcfg_type_inf_threshold", "Type inference threshold", value=dr$type_inference_threshold %||% 0.90, min=0, max=1, step=0.01))
        ),
        h6("Schema change flags", class="text-muted mt-3"),
        fluidRow(
          column(3, checkboxInput("gcfg_flag_new_cols",   "Flag new columns",   value=isTRUE(dr$flag_new_columns %||% TRUE))),
          column(3, checkboxInput("gcfg_flag_drop_cols",  "Flag dropped columns",value=isTRUE(dr$flag_dropped_columns %||% TRUE))),
          column(3, checkboxInput("gcfg_flag_type_chg",   "Flag type changes",   value=isTRUE(dr$flag_type_changes %||% TRUE))),
          column(3, checkboxInput("gcfg_flag_col_order",  "Flag column order",   value=isTRUE(dr$flag_column_order_change %||% TRUE)))
        )
      )
    ),

    div(class="mt-3",
      actionButton("gcfg_save", "Save global config", class="btn btn-primary")
    )
  )
}
