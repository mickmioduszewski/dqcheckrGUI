# Main application UI (spec §6)

ui <- function() {
  bslib::page_sidebar(
    title = tags$span(style="font-weight:700;font-size:16px;", "dqcheckr"),
    theme = bslib::bs_theme(version=5, bootswatch="flatly", base_font_size="14px"),

    sidebar = bslib::sidebar(
      width = 240,
      style = "padding:0;",

      # Datasets section
      tags$div(
        style="padding:8px 12px 4px;font-size:11px;font-weight:700;text-transform:uppercase;color:#888;letter-spacing:.08em;",
        "Datasets"
      ),
      uiOutput("dataset_sidebar_list"),
      tags$div(
        style="padding:4px 12px 8px;",
        actionButton("btn_new_dataset", "+ New dataset",
                     class="btn btn-sm btn-outline-primary w-100")
      ),
      tags$hr(style="margin:4px 0;"),

      # Other nav buttons
      actionButton("nav_run",     "▶  Run",          class="btn btn-light text-start w-100 border-0 rounded-0 py-2", style="text-align:left;padding-left:16px;"),
      actionButton("nav_history", "⏱  History",      class="btn btn-light text-start w-100 border-0 rounded-0 py-2", style="text-align:left;padding-left:16px;"),
      actionButton("nav_global",  "⚙  Global Config", class="btn btn-light text-start w-100 border-0 rounded-0 py-2", style="text-align:left;padding-left:16px;")
    ),

    # Main panel — content swapped by server
    uiOutput("main_panel"),

    # FWF ruler JS + scroll sync
    tags$head(
      tags$script(src="interact.min.js"),
      tags$script(src="fwf_ruler.js"),
      tags$link(rel="stylesheet", href="app.css")
    ),

    # Global JS handlers — always present in the page
    tags$script('
      Shiny.addCustomMessageHandler("scroll_log", function(msg) {
        var el = document.getElementById("run_log");
        if (el) el.scrollTop = el.scrollHeight;
      });

      // Dataset panel drift-check onchange handler
      window.__dqDC = function(ds) {
        var checked = document.querySelectorAll(".drift-check:checked");
        var btn = document.getElementById("compare_drift_" + ds);
        if (btn) btn.disabled = (checked.length !== 2);
      };

      // Called via onchange= on each history checkbox (inline handler fires before DT)
      window.__dqHC = function(el) {
        var checked = document.querySelectorAll(".hist-check:checked");
        var btn = document.getElementById("history_compare");
        if (!btn) return;
        if (checked.length === 2) {
          var ds1 = checked[0].getAttribute("data-ds");
          var ds2 = checked[1].getAttribute("data-ds");
          if (ds1 === ds2) {
            btn.disabled = false;
            Shiny.setInputValue("hist_selected_ids",
              [checked[0].getAttribute("data-id"), checked[1].getAttribute("data-id")],
              {priority: "event"});
          } else {
            btn.disabled = true;
            Shiny.setInputValue("hist_selected_ids", null, {priority: "event"});
          }
        } else {
          btn.disabled = true;
        }
      };
    ')
  )
}
