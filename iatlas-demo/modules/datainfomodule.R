datainfo_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Data Description"),
    fluidRow(
      tableBox(
        width = 12,
        div(style = 'overflow-x: scroll', 
            DT::dataTableOutput(ns('feature_table'))
        )
      )
    )
  )
}

datainfo <- function(input, output, session) {
  # TODO: add renderDataTable function
  output$feature_table <- DT::renderDT({
    feature_table %>% 
      select(
        FriendlyLabel, 
        `Variable Class`, 
        Unit, 
        VariableType,
        Origin
      ) %>% 
      datatable(
        selection = list(
          mode = 'single'
        ),
        options = list(
          scrollX = TRUE,
          autoWidth = F,
          dom = "tip"
        ),
        rownames = FALSE
      )
  }, server = FALSE
  )
}
