# dqcheckr client — main entry point (spec §3, §5)

library(shiny)
library(bslib)
library(shinyvalidate)
library(shinyFiles)
library(shinyAce)
library(reactable)
library(DT)
library(callr)
library(yaml)
library(readr)
library(DBI)
library(dqcheckr)

# Source all R/ files into this script's own environment rather than the
# global environment (source()'s `local=FALSE` default writes to .GlobalEnv,
# which would dump every ui_*/server_*/helper function from R/ into the
# user's workspace — a problem for anything sharing the R session, e.g.
# running multiple apps, or tests that load this app via shinytest2/testthat).
# `local=TRUE` sources into the calling frame: the same top-level environment
# in which `ui()` and the `server` closure below are evaluated, so both still
# resolve these definitions via ordinary lexical scoping.
for (f in list.files(file.path(getwd(), "R"), pattern="\\.R$", full.names=TRUE)) {
  source(f, local=TRUE)
}

# Config directory: env var override or working directory
get_config_dir <- function() {
  Sys.getenv("DQCHECKR_CONFIG_DIR", unset=file.path(getwd(), "config"))
}

# Register the reports directory as a static resource path at startup.
# Must be called before shinyApp() so it is available for the first request.
local({
  gcfg <- tryCatch(
    read_global_config(global_config_path(get_config_dir())),
    error = function(e) list()
  )
  register_report_resource_path(gcfg$report_output_dir, get_config_dir())
})

# ── UI ────────────────────────────────────────────────────────────────
shinyApp(
  ui = ui(),

  # ── Server ─────────────────────────────────────────────────────────
  server = function(input, output, session) {

    # Reactive config dir
    config_dir <- reactive({ get_config_dir() })

    # Reactive global config
    gcfg_rv <- reactiveVal(read_global_config(global_config_path(get_config_dir())))

    # Shared reactive state
    rv <- reactiveValues(
      active_section  = "datasets",
      active_dataset  = NULL,
      wizard_open     = FALSE,
      wizard_request  = NULL,
      dataset_refresh = NULL,
      history_refresh = NULL,
      run_preselect   = NULL,
      drift_request   = NULL
    )

    # ── Main panel routing ──────────────────────────────────────────
    output$main_panel <- renderUI({
      s <- rv$active_section
      if (s == "wizard") {
        tagList(
          div(class="d-flex justify-content-between align-items-center mb-2",
            uiOutput("wizard_breadcrumb"),
            actionButton("wizard_cancel", "✕ Cancel",
                         class="btn btn-outline-secondary btn-sm")
          ),
          uiOutput("wizard_step_content"),
          div(class="d-flex gap-2 mt-4",
            uiOutput("wizard_back_btn"),
            uiOutput("wizard_next_btn")
          )
        )
      } else if (s == "datasets") {
        ds <- rv$active_dataset
        if (!is.null(ds)) {
          cfg_path <- file.path(config_dir(), paste0(ds, ".yml"))
          cfg <- if (safe_file_exists(cfg_path)) tryCatch(read_config(cfg_path), error=function(e) list(known=list(), extra=list())) else list(known=list(), extra=list())
          gcfg <- gcfg_rv()
          db_path <- cfg$known$snapshot_db %||% gcfg$snapshot_db %||% ""
          last_runs <- read_snapshot_history(db_path, ds, n=5)
          ui_dataset_panel(ds, cfg, last_runs, gcfg)
        } else {
          tagList(
            h4("Datasets", style="margin-bottom:16px;"),
            p(class="text-muted",
              "Select a dataset from the sidebar, or click ", tags$strong("+ New dataset"), " to create one.")
          )
        }
      } else if (s == "run") {
        datasets <- list_dataset_configs(config_dir())
        ui_run(datasets, selected=rv$run_preselect)
      } else if (s == "history") {
        ui_history()
      } else if (s == "global") {
        gcfg <- gcfg_rv()
        ui_global_config(gcfg)
      }
    })

    # ── Dataset sidebar list ────────────────────────────────────────
    output$dataset_sidebar_list <- renderUI({
      rv$dataset_refresh  # dependency for refresh after save

      datasets <- list_dataset_configs(config_dir())
      if (length(datasets) == 0)
        return(p(class="text-muted px-3", style="font-size:12px;", "No datasets configured."))

      gcfg <- gcfg_rv()
      db_path <- gcfg$snapshot_db %||% ""

      items <- lapply(datasets, function(ds) {
        last_run <- read_snapshot_history(db_path, ds, n=1)
        status_text <- if (nrow(last_run) > 0) last_run$overall_status[1] else ""
        active <- isTRUE(rv$active_dataset == ds)

        tags$div(
          class=paste("dataset-item", if(active) "active"),
          onclick=sprintf("Shiny.setInputValue('sidebar_dataset_click', '%s', {priority:'event'});", js_string_escape(ds)),
          span(ds, style="flex:1;"),
          if (nchar(status_text) > 0) status_badge(status_text)
        )
      })
      tagList(items)
    })

    # ── Sidebar navigation ──────────────────────────────────────────
    observeEvent(input$sidebar_dataset_click, {
      rv$active_dataset <- input$sidebar_dataset_click
      rv$active_section <- "datasets"
    })
    observeEvent(input$btn_new_dataset, {
      rv$wizard_request <- list(mode="new", dataset=NULL, ts=Sys.time())
    })
    observeEvent(input$nav_run, {
      rv$active_section <- "run"
      rv$run_preselect <- rv$active_dataset
    })
    observeEvent(input$nav_history, {
      rv$active_section <- "history"
      rv$history_refresh <- Sys.time()
    })
    observeEvent(input$nav_global, {
      rv$active_section <- "global"
    })

    # ── Dataset panel actions (edit, run, history, compare) ─────────
    observeEvent(input$ds_action, {
      req(is.list(input$ds_action))
      action <- input$ds_action$action
      ds     <- input$ds_action$ds
      if (action == "edit") {
        rv$wizard_request <- list(mode="edit", dataset=ds, ts=Sys.time())
      } else if (action == "run") {
        rv$run_preselect  <- ds
        rv$active_section <- "run"
      } else if (action == "history") {
        rv$active_section  <- "history"
        rv$history_refresh <- Sys.time()
      } else if (action == "compare") {
        ids <- input$ds_action$ids
        if (length(ids) == 2) {
          rv$drift_request <- list(ds = ds, ids = ids, ts = Sys.time())
        }
      }
    })

    # ── Wizard cancel ───────────────────────────────────────────────
    observeEvent(input$wizard_cancel, {
      showModal(modalDialog(
        title="Cancel wizard?",
        "Unsaved changes will be lost.",
        footer=tagList(
          modalButton("Keep editing"),
          actionButton("wizard_cancel_confirm", "Discard and exit", class="btn btn-danger")
        )
      ))
    })
    observeEvent(input$wizard_cancel_confirm, {
      removeModal()
      rv$wizard_open <- FALSE
      rv$active_section <- "datasets"
    })

    # ── Initialise sub-servers ──────────────────────────────────────
    wiz <- server_wizard(input, output, session, rv, config_dir, gcfg_rv)
    server_step3_csv(input, output, session, wiz)
    server_global(input, output, session, rv, config_dir, gcfg_rv)
    server_run(input, output, session, rv, config_dir, gcfg_rv)
    server_history(input, output, session, rv, config_dir, gcfg_rv)

    # ── First-run: open global config if no dqcheckr.yml ───────────
    observe({
      cfg_path <- global_config_path(config_dir())
      if (!safe_file_exists(cfg_path)) {
        rv$active_section <- "global"
        showNotification(
          "Welcome! Please configure the infrastructure paths below to get started.",
          type="message", duration=6
        )
      }
    })

  }
)
