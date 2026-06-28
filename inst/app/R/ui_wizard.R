# Config wizard UI — 8-step framework (spec §8)

wizard_breadcrumb <- function(current, step_valid) {
  labels <- c("Identity","File","Structure","Columns","Rules","Overrides","Custom","Review")
  items <- lapply(seq_along(labels), function(i) {
    cls <- if (i == current) "wizard-step-num active"
           else if (isTRUE(step_valid[i])) "wizard-step-num done"
           else "wizard-step-num disabled"
    tagList(
      span(class=cls, i),
      if (i < length(labels)) span(class="wizard-step-sep", "›")
    )
  })
  div(class="wizard-breadcrumb", items)
}

# ── Step 1: Identity ────────────────────────────────────────────────────
wizard_step1_ui <- function(wiz) {
  tagList(
    h5("Step 1 — Dataset Identity"),
    div(class="mt-3",
      textInput("wiz_dataset_name", "Dataset name *",
                value=wiz$dataset_name, width="400px",
                placeholder="e.g. customer_accounts"),
      p(class="text-muted", style="font-size:12px;",
        "Must start with a letter; only letters, numbers, and underscores.")
    ),
    div(class="mt-2",
      textInput("wiz_description", "Description (optional)",
                value=wiz$description %||% "", width="400px")
    )
  )
}

# ── Step 2: File Location ───────────────────────────────────────────────
wizard_step2_ui <- function(wiz) {
  tagList(
    h5("Step 2 — File Location"),
    div(class="mt-3",
      radioButtons("wiz_file_mode", "How are files identified?",
                   choices=c("Folder scan (automatic)"="folder",
                             "Explicit file paths"="explicit"),
                   selected=wiz$file_mode %||% "folder", inline=TRUE)
    ),
    uiOutput("wiz_file_inputs")
  )
}

# ── Step 3: Format & Structure (CSV or FWF) ─────────────────────────────
wizard_step3_ui <- function() {
  tagList(
    h5("Step 3 — Format and Structure"),
    div(class="mt-3",
      # Character position ruler
      tags$div(id="fwf-char-ruler", class="fwf-char-ruler"),
      # Raw text preview (shinyAce)
      div(id="fwf-ruler-wrap",
        shinyAce::aceEditor("raw_preview", value="",
                            mode="text", readOnly=TRUE,
                            fontSize=13, theme="chrome",
                            height="280px", wordWrap=FALSE)
      ),
      uiOutput("step3_format_hint")
    ),

    div(class="mt-3 p-3 border rounded bg-light",
      h6("File properties"),
      uiOutput("step3_sniff_conflicts"),
      fluidRow(
        column(4,
          radioButtons("wiz_format", "Format",
                       choices=c("CSV"="csv","Fixed-Width (FWF)"="fwf"),
                       selected="csv", inline=TRUE)
        )
      ),
      uiOutput("step3_csv_fields"),
      uiOutput("step3_fwf_fields")
    ),

    div(class="mt-3",
      h6("Parsed preview"),
      uiOutput("step3_no_header_naming"),
      uiOutput("step3_parsed_preview"),
      reactable::reactableOutput("step3_fwf_preview")
    )
  )
}

# ── Step 4: Column Classification ──────────────────────────────────────
wizard_step4_ui <- function() {
  tagList(
    h5("Step 4 — Column Classification"),
    p(class="text-muted", "Review detected columns. Set type overrides, mark key columns, and choose which columns are expected in every delivery."),
    div(class="mb-2 d-flex gap-3 align-items-center",
      tags$span(class="text-muted", style="font-size:12px;", "Expected columns:"),
      actionButton("step4_select_all_expected",  "Select all",  class="btn btn-outline-secondary btn-xs", style="font-size:11px;padding:1px 8px;"),
      actionButton("step4_select_none_expected", "Select none", class="btn btn-outline-secondary btn-xs", style="font-size:11px;padding:1px 8px;")
    ),
    uiOutput("step4_column_table")
  )
}

# ── Step 5: Column Rules ────────────────────────────────────────────────
wizard_step5_ui <- function() {
  tagList(
    h5("Step 5 — Column Rules"),
    p(class="text-muted", "Set optional validation rules per column. Expand a column to add rules. Advanced settings are hidden by default."),
    uiOutput("step5_column_rules")
  )
}

# ── Step 6: Rule Overrides ──────────────────────────────────────────────
wizard_step6_ui <- function(wiz, gcfg) {
  dr <- gcfg$default_rules %||% list()
  overrides <- wiz$rule_overrides %||% list()

  def <- function(key, fallback) overrides[[key]] %||% (dr[[key]] %||% fallback)

  tagList(
    h5("Step 6 — Rule Overrides"),
    p(class="text-muted", "Override global thresholds for this dataset only. Leave unchanged to use global defaults."),
    fluidRow(
      column(4, numericInput("wiz_ro_max_missing",     "Max missing rate",          value=def("max_missing_rate",0.05), min=0,max=1,step=0.01)),
      column(4, numericInput("wiz_ro_max_nonnumeric",  "Max non-numeric rate",      value=def("max_non_numeric_rate",0.01), min=0,max=1,step=0.01)),
      column(4, numericInput("wiz_ro_min_rows",        "Min row count",             value=def("min_row_count",0), min=0,step=1))
    ),
    fluidRow(
      column(4, numericInput("wiz_ro_max_rowchg",      "Max row count change (%)",  value=round(def("max_row_count_change_pct",0.10)*100,2), min=0,step=1)),
      column(4, numericInput("wiz_ro_max_meanshift",   "Max mean shift (%)",        value=round(def("max_numeric_mean_shift_pct",0.20)*100,2), min=0,step=1)),
      column(4, numericInput("wiz_ro_max_misschg",     "Max missing change (pp)",   value=def("max_missing_rate_change_pp",2.0), min=0,step=0.1))
    ),
    fluidRow(
      column(4, numericInput("wiz_ro_max_nonnumchg",   "Max non-numeric change (pp)", value=def("max_non_numeric_rate_change_pp",1.0), min=0,step=0.1)),
      column(4, numericInput("wiz_ro_type_inf",        "Type inference threshold",  value=def("type_inference_threshold",0.90), min=0,max=1,step=0.01))
    ),
    h6("Schema change flags", class="mt-2"),
    fluidRow(
      column(3, checkboxInput("wiz_ro_flag_new",    "Flag new columns",   value=isTRUE(def("flag_new_columns",TRUE)))),
      column(3, checkboxInput("wiz_ro_flag_drop",   "Flag dropped cols",  value=isTRUE(def("flag_dropped_columns",TRUE)))),
      column(3, checkboxInput("wiz_ro_flag_type",   "Flag type changes",  value=isTRUE(def("flag_type_changes",TRUE)))),
      column(3, checkboxInput("wiz_ro_flag_order",  "Flag col order",     value=isTRUE(def("flag_column_order_change",TRUE))))
    )
  )
}

# ── Step 7: Custom Checks ───────────────────────────────────────────────
wizard_step7_ui <- function(wiz) {
  tagList(
    h5("Step 7 — Custom Checks (optional)"),
    p(class="text-muted",
      "Custom checks are organisation-specific rules written in R. They run alongside the standard quality checks."),
    div(class="d-flex gap-2 align-items-center",
      textInput("wiz_custom_file", "Custom checks R file",
                value=wiz$custom_checks_file %||% "", width="400px",
                placeholder="e.g. custom/my_dataset_checks.R"),
      shinyFiles::shinyFilesButton("wiz_custom_browse", "Browse",
        "Select custom checks R file", FALSE,
        style="white-space:nowrap;height:38px;margin-top:24px;")
    ),
    uiOutput("step7_validation_badge"),
    p(class="text-muted mt-2", style="font-size:12px;",
      "The file must define a function named ", tags$code("custom_checks(df)"),
      " or ", tags$code("custom_checks(df, config)"), "."),
    p(class="text-muted", style="font-size:12px;", "Leave blank to skip custom checks.")
  )
}

# ── Step 8: Review and Save ─────────────────────────────────────────────
wizard_step8_ui <- function() {
  tagList(
    h5("Step 8 — Review and Save"),
    uiOutput("step8_summary"),
    div(class="mt-3",
      h6("YAML preview"),
      verbatimTextOutput("yaml_preview"),
    ),
    div(class="mt-3 d-flex gap-2",
      actionButton("wizard_save", "Save config ✓", class="btn btn-success")
    )
  )
}
