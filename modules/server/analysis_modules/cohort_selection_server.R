cohort_selection_server <- function(
    input, 
    output, 
    session,
    feature_named_list
){
    ns <- session$ns
    
    source("modules/server/submodules/cohort_group_selection_server.R", local = T)
    source("modules/server/submodules/cohort_filter_selection_server.R", local = T)
    source("modules/server/submodules/data_table_server.R", local = T)
    source("functions/cohort_selection_functions.R", local = T)
    
    # cohort selection --------------------------------------------------------
    
    dataset_to_module_tbl <- shiny::reactive({
        dplyr::tribble(
            ~module,                  ~dataset,
            "Sample Group Overview",  "TCGA",
            "Tumor Microenvironment", "TCGA",
            "Immune Feature Trends",  "TCGA",
            "Clinical Outcomes",      "TCGA",
            "IO Targets",             "TCGA",
            "TIL Maps",               "TCGA",
            "Driver Associations",    "TCGA"
            # "Sample Group Overview",  "PCAWG",
            # "Tumor Microenvironment", "PCAWG",
            # "Immune Feature Trends",  "PCAWG",
            # "IO Targets",             "PCAWG",
            # "Driver Associations",    "PCAWG"
        )
    })
    

    output$module_selection_ui <- shiny::renderUI({
        choices <- dataset_to_module_tbl() %>% 
            dplyr::pull(module) %>% 
            unique()
        shiny::checkboxGroupInput(
            ns("module_choices"),
            "Select modules:",
            choices
        )
    })
    
    selected_dataset <- shiny::reactive({
        if(is.null(input$dataset_choice)){
            return("TCGA")
        } else {
            return(input$dataset_choice)
        }
    })
    
    output$dataset_selection_ui <- shiny::renderUI({
        if(input$select_by_module & !is.null(input$module_choices)){
            choices <- dataset_to_module_tbl() %>% 
                dplyr::filter(module %in% input$module_choices) %>% 
                dplyr::group_by(dataset) %>% 
                dplyr::summarise(count = dplyr::n()) %>% 
                dplyr::filter(count == length(input$module_choices)) %>% 
                dplyr::pull(dataset)
        } else {
            choices <- dataset_to_module_tbl() %>% 
                dplyr::pull(dataset) %>% 
                unique()
        }
        shiny::selectInput(
            inputId = ns("dataset_choice"),
            label = strong("Select or Search for Dataset"),
            choices = choices,
            selected = "TCGA"
        )
    })
    
    available_modules <- shiny::reactive({
        shiny::req(selected_dataset())
        dataset_to_module_tbl() %>% 
            dplyr::filter(dataset == selected_dataset()) %>% 
            dplyr::pull(module)
    })
    
    output$module_availibility_string <- shiny::renderText({
        shiny::req(selected_dataset(), available_modules())
        available_modules() %>% 
            stringr::str_c(collapse = ", ") %>% 
            stringr::str_c(
                "Modules available for dataset :", 
                selected_dataset(),
                "are",
                .,
                sep = " "
            )
    })
    
    all_sample_ids <- shiny::reactive({
        req(selected_dataset())
        get_all_dataset_ids(selected_dataset())
    })
    
    selected_sample_ids <- cohort_obj <- shiny::callModule(
        cohort_filter_selection_server,
        "cohort_filter_selection",
        feature_named_list,
        selected_dataset,
        all_sample_ids
    )
    
    sample_ids <- shiny::reactive({
        if(is.null(selected_sample_ids())){
            req(all_sample_ids())
            return(all_sample_ids())
        } else {
            return(selected_sample_ids())
        }
    })

    cohort_obj <- shiny::callModule(
        cohort_group_selection_server,
        "cohort_group_selection",
        feature_named_list,
        sample_ids,
        selected_dataset
    )
    
    # group key ---------------------------------------------------------------
    
    group_key_tbl <- shiny::reactive({
        shiny::req(cohort_obj())
        tbl <- cohort_obj()$plot_colors %>% 
            tibble::enframe(name = "group", value = "color") %>% 
            dplyr::inner_join(cohort_obj()$group_tbl, by = "group") %>% 
            dplyr::select(
                `Sample Group` = group,
                `Group Name` = name,
                `Group Size` = size,
                Characteristics = characteristics,
                `Plot Color` = color
            ) 
    })
    
    shiny::callModule(
        data_table_server, 
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
    
    output$mosaic_group_select <- shiny::renderUI({
        shiny::req(groups_list(), group_internal_choice())
        shiny::radioButtons(
            ns("mosaic_group_choice"), 
            "Select second sample group to view overlap:",
            choices = setdiff(groups_list(), group_internal_choice()),
            inline = TRUE
        )
    })
    
    output$mosaic_subset_select <- shiny::renderUI({
        shiny::req(input$mosaic_group_choice)
        if (input$mosaic_group_choice == "TCGA_Subtype") {
            shiny::req(tcga_subtypes_list())
            selectInput(
                ns("mosaic_subset_choice"),
                "Select or Search for Study Subset",
                choices = tcga_subtypes_list()
            )
        }
    })
    
    mosaic_subtypes <- shiny::reactive({
        shiny::req(input$mosaic_group_choice)
        if(input$mosaic_group_choice == "TCGA_Subtype"){
            shiny::req(input$mosaic_subset_choice, groups_con())
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
    
    output$mosaicPlot <- plotly::renderPlotly({
        
        
        shiny::req(
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
        
        shiny::validate(shiny::need(
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
    
    # return ------------------------------------------------------------------
    return(cohort_obj)
}

