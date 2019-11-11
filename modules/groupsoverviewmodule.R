groupsoverview_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Sample Groups Overview"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/sample_groups.markdown")
        ),
        # # custom groups -------------------------------------------------------
        # sectionBox(
        #     title = "Custom Groups",
        #     collapsed = TRUE,
        #     messageBox(
        #         width = 12,
        #         p("Upload a comma-separated table with your own sample/group assignments to use in iAtlas analysis modules.")
        #     ),
        #     fluidRow(
        #         optionsBox(
        #             width = 12,
        #             tags$head(tags$script(src = "message-handler.js")),
        #             actionButton(
        #                 ns("filehelp"),
        #                 " Formatting instructions",
        #                 icon = icon("info-circle")
        #             ),
        #             hr(),
        #             fileInput(
        #                 ns("file1"),
        #                 "Choose CSV File",
        #                 multiple = FALSE,
        #                 accept = c(
        #                     "text/csv",
        #                     "text/comma-separated-values,text/plain",
        #                     ".csv"
        #                 )
        #             )
        #         )
        #     ),
        #     messageBox(
        #         width = 12,
        #         p("After uploading your file, the table below will show your defined groups."),
        #         DT::dataTableOutput(ns("user_group_tbl"))
        #     )
        # ),
        # group key -----------------------------------------------------------
        data_table_module_UI(
            ns("sg_table"),
            title = "Group Key",
            message_html = p(stringr::str_c(
                "This displays attributes and annotations of your choice of",
                "groups.",
                sep = " "
            ))
        ),
        # group overlap -------------------------------------------------------
        sectionBox(
            title = "Group Overlap",
            messageBox(
                width = 12,
                includeMarkdown("data/markdown/sample_groups_overlap.markdown")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    uiOutput(ns("mosaic_group_select")),
                    uiOutput(ns("mosaic_subset_select"))
                ),
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    column(
                        width = 12,
                        plotlyOutput(ns("mosaicPlot"), height = "600px") %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    )
}



groupsoverview <- function(
    input, 
    output, 
    session, 
    groups_con,
    feature_values_con,
    groups_list,
    tcga_subtypes_list,
    group_internal_choice,
    group_values_con,
    subtypes,
    plot_colors
){
    ns <- session$ns
    
    # custom groups -----------------------------------------------------------
    
    # observeEvent(input$filehelp, {
    #     showModal(modalDialog(
    #         title = "Formatting custom groups",
    #         includeMarkdown("data/user_groups.md"),
    #         size = "l", easyClose = TRUE
    #     ))
    # })
    # 
    # user_group_tbl <- reactive({
    #     if(is.null(input$file1$datapath)){
    #         return("none")
    #     }
    #     result <- try(readr::read_csv(input$file1$datapath))
    #     if(is.data.frame(result)){
    #         tbl <- result %>% 
    #             dplyr::rename(sample = 1) %>% 
    #             dplyr::arrange(sample)
    #         return(tbl)  
    #     } else {
    #         return("none")
    #     }
    # })
    # 
    # output$user_group_tbl <- DT::renderDataTable({
    #     req(is.data.frame(user_group_tbl()))
    #     user_group_tbl()
    # })
    
    user_group_tbl <- reactive("none")
    
    # group key ---------------------------------------------------------------
    
    group_key_tbl <- reactive({
        req(groups_con(), feature_values_con())
        feature_values_con() %>% 
            dplyr::select(sample, group) %>% 
            dplyr::distinct() %>% 
            dplyr::group_by(group) %>% 
            dplyr::summarise(size = dplyr::n()) %>% 
            dplyr::inner_join(groups_con()) %>% 
            dplyr::select(
                `Sample Group` = group, 
                `Group Name` = group_name, 
                `Group Size` = size,
                Characteristics = characteristics,
                `Plot Color` = color
            ) %>% 
            dplyr::as_tibble()
    })
    
    callModule(
        data_table_module, 
        "sg_table", 
        group_key_tbl,
        options = list(
            dom = "tip",
            pageLength = 10,
            columnDefs = list(
                list(width = '50px',
                     targets = c(1)
                )
            )
        ),
        color = T,
        color_column = "Plot Color",
        colors = ""
    )
    
    # group overlap -----------------------------------------------------------
    
    output$mosaic_group_select <- renderUI({
        req(groups_list(), group_internal_choice())
        radioButtons(
            ns("mosaic_group_choice"), 
            "Select second sample group to view overlap:",
            choices = setdiff(groups_list(), group_internal_choice()),
            inline = TRUE
        )
    })
    
    output$mosaic_subset_select <- renderUI({
        req(input$mosaic_group_choice)
        if (input$mosaic_group_choice == "TCGA_Subtype") {
            req(tcga_subtypes_list())
            selectInput(
                ns("mosaic_subset_choice"),
                "Choose study subset:",
                choices = tcga_subtypes_list()
            )
        }
    })
    
    mosaic_subtypes <- reactive({
        req(input$mosaic_group_choice)
        if(input$mosaic_group_choice == "TCGA_Subtype"){
            req(input$mosaic_subset_choice, groups_con())
            subtypes <- groups_con() %>%  
                dplyr::filter(
                    subtype_group == local(input$mosaic_subset_choice)
                ) %>% 
                dplyr::pull(group)
        } else {
            subtypes <- "none"
        }
        return(subtypes)
    })
    
    output$mosaicPlot <- renderPlotly({
        
        
        req(
            group_values_con(),
            group_internal_choice(),
            input$mosaic_group_choice,
            subtypes(),
            mosaic_subtypes(),
            groups_con(),
            plot_colors()
        )
        con <- group_values_con() %>% 
            dplyr::select(
                y = local(group_internal_choice()),
                x = local(input$mosaic_group_choice)
            )
        
        if(group_internal_choice() == "TCGA_Subtype"){
            con <- dplyr::filter(con, y %in% local(subtypes()))
        } else if (input$mosaic_group_choice == "TCGA_Subtype"){
            con <- dplyr::filter(con, x %in% local(mosaic_subtypes()))
        }
        
        mosaic_plot_tbl <- con %>% 
            dplyr::filter_all(dplyr::all_vars(!is.na(.))) %>% 
            dplyr::as_tibble()
        
        validate(need(
            nrow(mosaic_plot_tbl) > 0,
            "Group choices have no samples in common"
        ))
        
        y_display_name <- groups_con() %>% 
            translate_value(
                group_internal_choice(),
                "parent_group",
                "parent_group_display"
            ) %>% 
            unique()
        
        x_display_name <- groups_con() %>% 
            translate_value(
                input$mosaic_group_choice,
                "parent_group",
                "parent_group_display"
            ) %>% 
            unique()
        
        create_mosaicplot(
            mosaic_plot_tbl,
            fill_colors = plot_colors(),
            title = stringr::str_c(
                y_display_name, 
                "by", 
                x_display_name, 
                sep = " "
            )
        )
    })
    
    return(user_group_tbl)
    
}

