datainfo_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Data Description"),
    textBox(
      width = 12,
      p("Some overview/summary text describing this module and the data presented within.")  
    ),
    sectionBox(
      title = "PanImmune Features",
      messageBox(
        width = 12,
        p("Select a row in the feature table to view more details about variables in the same class.")
      ),
      fluidRow(
        tableBox(
          width = 12,
          div(style = 'overflow-x: scroll', 
              DT::dataTableOutput(ns('feature_table'))
          )
        )
      )
    ),
    sectionBox(
      title = "Variable Class Details",
      messageBox(
        width = 12,
        p("This is some information about the features in the variable class you clicked.")
      ),
      fluidRow(
        tableBox(
          width = 10,
          tableOutput(ns('variable_class_table'))
        ),
        actionButton(ns("show_methods"), "Show Methods")
      )
    )
  )
}

datainfo <- function(input, output, session) {
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
  
  observeEvent(input$show_methods, {
    showModal(modalDialog(
      title = "Methods",
      includeMarkdown("data/MethodsText/Methods_BCR.txt"),
      size = "l", easyClose = TRUE
    ))
  })
  
  observeEvent(input$feature_table_rows_selected, {
    output$variable_class_table <- renderTable({
      feature_row <- input$feature_table_rows_selected
      selected_class <- feature_table[[feature_row, "Variable Class"]]
      feature_table %>% 
        filter(`Variable Class` == selected_class) %>% 
        select(
          `Variable Class Order`,
          FriendlyLabel, 
          Unit, 
          VariableType,
          Origin
        ) %>% 
        arrange(`Variable Class Order`, FriendlyLabel)
    })
  })
}
