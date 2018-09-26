datainfo_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Data Description"),
        textBox(
            width = 12,
            p("Each row in the table corresponds to a variable for which data are available for exploration in CRI iAtlas. Variables are organized into classes, as displayed in the Variable Class column. In a number of iAtlas modules the Variable Class will appear in drop-down selections for variables.")  
        ),
        sectionBox(
            title = "PanImmune Readouts",
            messageBox(
                width = 12,
                p("Select a row in the table to view more details about variables in the same class."),
                p("Use the class selector to filter on the Variable class column")
            ),
            fluidRow(
                optionsBox(
                    width = 6,
                    uiOutput(ns("classes")))),
            fluidRow(
                tableBox(
                    width = 12,
                    div(
                        DT::dataTableOutput(ns('feature_table')) %>% 
                            shinycssloaders::withSpinner()
                    )
                )
            )
        ),
        uiOutput(ns("variable_details_section")) 
    )
}

datainfo <- function(input, output, session) {
    ns <- session$ns
    
    output$classes <- renderUI({
        choices <- panimmune_data$feature_df %>% 
            use_series(`Variable Class`) %>% 
            sort %>% 
            unique %>% 
            c("All classes", .)
        print(choices)
        selectInput(
            ns("class_choice"),
            label = "Select Class",
            choices = choices)
    })
    
    
    
    output$feature_table <- DT::renderDT({
        df <- panimmune_data$feature_df %>% 
            select(
                `Feature Name` = FriendlyLabel, 
                `Feature Matrix Label` = FeatureMatrixLabelTSV,
                `Variable Class`, 
                Unit, 
                VariableType
            ) 
        if(input$class_choice != "All classes"){
            df <- filter(df, `Variable Class` == input$class_choice)
        }
        datatable(
            df, 
            selection = list(
                mode = 'single'
            ),
            options = list(
                scrollX = TRUE,
                autoWidth = F
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
