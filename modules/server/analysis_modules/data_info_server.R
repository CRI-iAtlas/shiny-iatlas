data_info_server <- function(
    input, 
    output, 
    session,
    features_con
){
    ns <- session$ns
    
    output$classes <- shiny::renderUI({
        shiny::req(features_con()) 
        choices <- features_con() %>%
            dplyr::filter(!is.na(class_name), !is.na(class_id)) %>%
            dplyr::select(class_name, class_id) %>% 
            dplyr::distinct() %>% 
            dplyr::arrange(class_name) %>% 
            dplyr::collect() %>% 
            tibble::deframe() %>% 
            c("All classes" = -1L)
        shiny::selectInput(
            ns("class_choice_id"),
            label = "Select or Search for Class",
            choices = choices,
            selected = -1
        )
    })
    
    filtered_feature_con <- shiny::reactive({
        shiny::req(features_con(), input$class_choice_id)
        class_choice_id <- as.numeric(input$class_choice_id)
        if(class_choice_id != -1L){
            con <- features_con() %>% 
                dplyr::filter(class_id == local(class_choice_id)) %>% 
                dplyr::compute() 
        } else {
            con <- features_con()
        }
        return(con)
    })
    
    output$feature_table <- DT::renderDT({
        shiny::req(filtered_feature_con())
        
        tbl <- filtered_feature_con() %>%
            dplyr::select(
                `Feature Name` = feature_name,
                `Variable Class` = class_name,
                Unit = unit
            ) %>% 
            dplyr::collect()

        DT::datatable(
            tbl, 
            selection = list(mode = 'single'),
            options = list(scrollX = TRUE, autoWidth = F ),
            rownames = FALSE
        )
    }, server = FALSE)
    
    feature_class_tbl <- shiny::reactive({
        shiny::req(input$feature_table_rows_selected, filtered_feature_con())
        
        clicked_row_num <- input$feature_table_rows_selected
        
        selected_class_id <- filtered_feature_con() %>% 
            dplyr::pull(class_id) %>% 
            magrittr::extract2(clicked_row_num) 
        
        filtered_feature_con() %>% 
            dplyr::filter(class_id == selected_class_id) %>% 
            dplyr::arrange(order, feature_name) %>% 
            dplyr::collect()
    })
    
    output$variable_class_table <- shiny::renderTable({
        shiny::req(feature_class_tbl())
        feature_class_tbl()
    })
    
    output$method_buttons <- shiny::renderUI({
        shiny::req(feature_class_tbl())
        feature_class_tbl() %>% 
            dplyr::filter(!is.na(method_tag)) %>% 
            dplyr::distinct(method_tag) %>% 
            dplyr::pull(method_tag) %>% 
            purrr::map(
                ~fluidRow(actionButton(ns(paste0("show_", .x)), .x))
            ) %>%
            shiny::tagList()
    })
    
    shiny::observeEvent(input$feature_table_rows_selected, {
        feature_class_tbl() %>% 
            dplyr::filter(!is.na(method_tag)) %>% 
            dplyr::distinct(method_tag) %>% 
            dplyr::pull(method_tag) %>% 
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
    
    shiny::observeEvent(input$feature_table_rows_selected, {
        output$variable_details_section <- shiny::renderUI({
            .GlobalEnv$sectionBox(
                title = "Variable Class Details",
                .GlobalEnv$messageBox(
                    width = 12,
                    p("Here is additional information about the variables in the Variable Class you selected. To the right you can access description of the methods used to obtain the variables.")
                ),
                shiny::fluidRow(
                    .GlobalEnv$tableBox(
                        width = 9,
                        tableOutput(ns('variable_class_table'))
                    ),
                    shiny::column(
                        width = 3,
                        shiny::h5(shiny::strong("Click to view methods")),
                        shiny::uiOutput(ns("method_buttons"))
                    )
                )
            )
        })
    })
}
