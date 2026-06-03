# Run panel UI (spec §17)

ui_run <- function(datasets, selected = NULL) {
  tagList(
    h4("Run Quality Check", style="margin-bottom:20px;"),

    fluidRow(
      column(6,
        selectInput("run_dataset", "Dataset", choices=datasets,
                    selected=selected, width="100%")
      )
    ),

    uiOutput("run_precheck_status"),

    div(class="mt-3 d-flex gap-2 align-items-center",
      uiOutput("run_start_btn"),
      uiOutput("run_stop_btn")
    ),

    div(class="mt-3",
      uiOutput("run_status_area")
    ),

    div(class="mt-2",
      uiOutput("run_log_area")
    )
  )
}
