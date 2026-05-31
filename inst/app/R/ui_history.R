# History panel UI (spec §18)

ui_history <- function() {
  tagList(
    h4("Run History", style="margin-bottom:20px;"),
    div(class="d-flex gap-2 align-items-center mb-3",
      actionButton("history_compare", "Compare drift ▶",
                   class="btn btn-outline-info btn-sm", disabled=TRUE),
      span(class="text-muted fst-italic", style="font-size:12px;",
           "Select exactly 2 runs from the same dataset")
    ),
    DT::dataTableOutput("history_table"),
    div(class="mt-2",
      actionButton("history_load_more", "Load more", class="btn btn-outline-secondary btn-sm")
    )
  )
}
