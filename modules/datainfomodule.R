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

datainfo <- function(
    input, 
    output, 
    session,
    features_con,
    features_named_list
){
    ns <- session$ns
    
    output$classes <- renderUI({
        req(features_named_list())
        choices <- features_named_list() %>% 
            names() %>% 
            sort() %>% 
            c("All classes", .)
        selectInput(
            ns("class_choice"),
            label = "Select Class",
            choices = choices
        )
    })
    
    filtered_feature_con <- reactive({
        req(features_con(), input$class_choice)
        if(input$class_choice != "All classes"){
            return(dplyr::filter(features_con(), class == input$class_choice))
        } else {
            return(features_con())
        }
    })
    
    output$feature_table <- DT::renderDT({
        req(filtered_feature_con())
        
        tbl <- filtered_feature_con() %>%
            dplyr::select(
                `Feature Name` = display,
                `Variable Class` = class,
                Unit = unit
            ) %>% 
            dplyr::as_tibble()

        DT::datatable(
            tbl, 
            selection = list(mode = 'single'),
            options = list(scrollX = TRUE, autoWidth = F ),
            rownames = FALSE
        )
    }, server = FALSE)
    
    feature_class_tbl <- reactive({
        req(input$feature_table_rows_selected, filtered_feature_con())
        
        clicked_row_num <- input$feature_table_rows_selected
        print(clicked_row_num)
        
        selected_class <- filtered_feature_con() %>% 
            dplyr::pull(class) %>% 
            .[[clicked_row_num]]
        
        filtered_feature_con() %>% 
            dplyr::filter(class == selected_class) %>% 
            dplyr::arrange(order, display) %>% 
            dplyr::as_tibble()
    })
    
    output$variable_class_table <- renderTable({
        req(feature_class_tbl())
        feature_class_tbl()
    })
    
    output$method_buttons <- renderUI({
        req(feature_class_tbl())
        feature_class_tbl() %>% 
            dplyr::filter(!is.na(methods_tag)) %>% 
            dplyr::distinct(methods_tag) %>% 
            dplyr::pull(methods_tag) %>% 
            purrr::map(~fluidRow(actionButton(ns(paste0("show_", .x)), .x))) %>%
            tagList()
    })
    
    observeEvent(input$feature_table_rows_selected, {
        feature_class_tbl() %>% 
            dplyr::filter(!is.na(methods_tag)) %>% 
            dplyr::distinct(methods_tag) %>% 
            dplyr::pull(methods_tag) %>% 
            purrr::map(function(tag) {
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
