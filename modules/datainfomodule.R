datainfo_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Data Description"),
    textBox(
      width = 12,
      p("Each row in the table corresponds to a variable for which data are available for exploration in CRI iAtlas. Variables are organized into classes, as displayed in the Variable Class column.")  
    ),
    sectionBox(
      title = "PanImmune Variables",
      messageBox(
        width = 12,
        p("Select a row in the table to view more details about variables in the same class.")
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
    uiOutput(ns("variable_details_section")) %>% 
      shinycssloaders::withSpinner()
  )
}

datainfo <- function(input, output, session) {
  ns <- session$ns
  
  output$feature_table <- DT::renderDT({
    panimmune_data$feature_df %>% 
      select(
        `Feature Name` = FriendlyLabel, 
        `Feature Matrix Label` = FeatureMatrixLabelTSV,
        `Variable Class`, 
        Unit, 
        VariableType
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
  
  feature_class_df <- reactive({
    feature_row <- input$feature_table_rows_selected
    selected_class <- panimmune_data$feature_df[[feature_row, "Variable Class"]]
    panimmune_data$feature_df %>% 
      filter(`Variable Class` == selected_class) %>% 
      select(
        `Variable Class Order`,
        `Feature Name` = FriendlyLabel, 
        `Feature Matrix Label` = FeatureMatrixLabelTSV,
        Unit, 
        VariableType,
        Origin
      ) %>% 
      left_join(
        panimmune_data$feature_method_df %>% 
          select(`Feature Origin`, `Methods Tag`), 
        by = c("Origin" = "Feature Origin")
      ) %>% 
      select(-Origin) %>% 
      arrange(`Variable Class Order`, `Feature Name`)
  })
  
  output$variable_class_table <- renderTable({
    feature_class_df()
  })
  
  output$method_buttons <- renderUI({
    tag_list <- feature_class_df() %>% 
      filter(!is.na(`Methods Tag`)) %>% 
      distinct(`Methods Tag`) %>% 
      pluck("Methods Tag") %>% 
      map(function(tag) {
        fluidRow(
          actionButton(ns(paste0("show_", tag)), tag)
        )
      }) %>% 
      tagList()
    print(tag_list)
    tag_list
  })
  
  observeEvent(input$feature_table_rows_selected, {
    feature_class_df() %>% 
      filter(!is.na(`Methods Tag`)) %>% 
      distinct(`Methods Tag`) %>% 
      pluck("Methods Tag") %>% 
      map(function(tag) {
        observeEvent(input[[paste0("show_", tag)]], {
          showModal(modalDialog(
            title = "Methods",
            includeMarkdown(paste0("data/MethodsText/Methods_", tag, ".txt")),
            size = "l", easyClose = TRUE
          ))
        })
      })
  })
  
  observeEvent(input$feature_table_rows_selected, {
    output$variable_details_section <- renderUI({
      sectionBox(
        title = "Variable Class Details",
        messageBox(
          width = 12,
          p("Here is additional information about the variables in the Variable Class you selected. To the right you can access description of the methods used to obtain the variables.")
        ),
        fluidRow(
          tableBox(
            width = 9,
            tableOutput(ns('variable_class_table'))
          ),
          column(
            width = 3,
            h5(strong("Click to view methods")),
            uiOutput(ns("method_buttons"))
          )
        )
      )
    })
  })
}
