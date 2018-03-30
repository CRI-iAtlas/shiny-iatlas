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
    uiOutput(ns("variable_details_section"))
  )
}

datainfo <- function(input, output, session) {
  ns <- session$ns
  
  output$feature_table <- DT::renderDT({
    panimmune_data$feature_df %>% 
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
  
  output$variable_class_table <- renderTable({
    feature_row <- input$feature_table_rows_selected
    selected_class <- panimmune_data$feature_df[[feature_row, "Variable Class"]]
    panimmune_data$feature_df %>% 
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
  
  observeEvent(input$feature_table_rows_selected, {
    output$variable_details_section <- renderUI({
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
    })
  })
}
