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
        
        # cohort selection ----------------------------------------------------
        sectionBox(
            title = "Cohort Selection",
            messageBox(
                width = 12,
                p("Group Selection and filtering"),
            ),
            # dataset selection ----
            optionsBox(
                width = 4,
                checkboxInput(
                    inputId = ns("select_by_module"),
                    label = strong("Select By Module?"),
                ),
                conditionalPanel(
                    condition = "input.select_by_module",
                    uiOutput(ns("module_selection_ui")),
                    ns = ns
                ),
                uiOutput(ns("dataset_selection_ui")),
            ),
            messageBox(
                width = 12,
                textOutput(ns("module_availibility_string"))
            ),
            # sample filtering ----
            optionsBox(
                width = 12,
                insert_remove_element_module_ui(
                    ns("group_filter"), 
                    "Add group filter"
                )
            ),
            optionsBox(
                width = 12,
                insert_remove_element_module_ui(
                    ns("numeric_filter"), 
                    "Add numeric filter"
                )
            ),
            tableBox(
                width = 12,
                textOutput(ns("samples_text"))
            ),
            # group selection ----
            optionsBox(
                width = 12,
                uiOutput(ns("select_group_ui")),
                conditionalPanel(
                    condition = "output.display_custom_numeric",
                    uiOutput(ns("select_custom_numeric_group_ui")),
                    sliderInput(
                        inputId = ns("custom_numeric_group_number_choice"),
                        label = "Select number of divsions",
                        min = 2,
                        max = 10,
                        value = 2,
                        step = 1
                    ),
                    ns = ns
                ),
                conditionalPanel(
                    condition = "output.display_custom_mutation",
                    uiOutput(ns("select_custom_mutation_group_ui")),
                    ns = ns
                ),
            ),
        ),
        
        # group key -----------------------------------------------------------
        # data_table_module_UI(
        #     ns("sg_table"),
        #     title = "Group Key",
        #     message_html = p(stringr::str_c(
        #         "This displays attributes and annotations of your choice of",
        #         "groups.",
        #         sep = " "
        #     ))
        # ),
        # group overlap -------------------------------------------------------
        # sectionBox(
        #     title = "Group Overlap",
        #     messageBox(
        #         width = 12,
        #         includeMarkdown("data/markdown/sample_groups_overlap.markdown")
        #     ),
        #     fluidRow(
        #         optionsBox(
        #             width = 12,
        #             uiOutput(ns("mosaic_group_select")),
        #             uiOutput(ns("mosaic_subset_select"))
        #         ),
        #     ),
        #     fluidRow(
        #         plotBox(
        #             width = 12,
        #             column(
        #                 width = 12,
        #                 plotlyOutput(ns("mosaicPlot"), height = "600px") %>%
        #                     shinycssloaders::withSpinner()
        #             )
        #         )
        #     )
        # )
    )
}

groupsoverview <- function(
    input, 
    output, 
    session,
    groups_con
    # groups2_con,
    # features_named_list,
    # groups_list,
    # tcga_subtypes_list,
    # group_internal_choice,
    # group_values_con,
    # subtypes,
    # plot_colors
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
    
    # cohort selection --------------------------------------------------------
    
    dataset_to_module_tbl <- reactive({
        dplyr::tribble(
            ~module,                  ~dataset,
            "Sample Group Overview",  "TCGA",
            "Tumor Microenvironment", "TCGA",
            "Immune Feature Trends",  "TCGA",
            "Clinical Outcomes",      "TCGA",
            "IO Targets",             "TCGA",
            "TIL Maps",               "TCGA",
            "Driver Associations",    "TCGA",
            "Sample Group Overview",  "PCAWG",
            "Tumor Microenvironment", "PCAWG",
            "Immune Feature Trends",  "PCAWG",
            "IO Targets",             "PCAWG",
            "Driver Associations",    "PCAWG"
        )
    })
    
    dataset_to_group_tbl <- reactive({
        dplyr::tribble(
            ~group,            ~dataset,
            "Immune_Subtype",  "TCGA",
            "TCGA_Subtype",    "TCGA",
            "TCGA_Study",      "TCGA",
            "Gender",          "TCGA",
            "Race",            "TCGA",
            "Ethnicity",       "TCGA",
            "Custom Numeric",  "TCGA",
            "Custom Mutation", "TCGA",
            "Immune_Subtype",  "PCAWG",
            "PCAWG_Study",     "PCAWG",
            "Gender",          "PCAWG",
            "Race",            "PCAWG",
            "Custom Numeric",  "PCAWG"
        ) %>%
            dplyr::mutate(display = stringr::str_replace_all(group, "_", " ")) #fix!!!
    })
    
    
    samples_con <- reactive({
        req(PANIMMUNE_DB2)
        PANIMMUNE_DB2 %>%
            dplyr::tbl("samples") %>%
            dplyr::rename(sample_id = id, sample_barcode = sample_id)
    })
    
    tags_con <- reactive({
        req(PANIMMUNE_DB2)
        PANIMMUNE_DB2 %>%
            dplyr::tbl("samples_to_tags") %>%
            dplyr::left_join(
                dplyr::tbl(PANIMMUNE_DB2, "tags"),
                by = c("tag_id" = "id")
            ) 
    })
    
    feature_values_con <- reactive({
        req(PANIMMUNE_DB2)
        PANIMMUNE_DB2 %>%
            dplyr::tbl("features_to_samples") %>%
            dplyr::select(sample_id, feature_id, value) %>% 
            dplyr::left_join(
                dplyr::tbl(PANIMMUNE_DB2, "features"), 
                by = c("feature_id" = "id")
            ) %>% 
            dplyr::select(- value.y) %>%  ### remove!!!
            dplyr::rename(value = value.x)  ### remove!
    })
    
    
    features_list <- reactive({
        req(PANIMMUNE_DB2)
        features_list <- 
            dplyr::full_join(
                dplyr::tbl(PANIMMUNE_DB2, "features"),
                dplyr::tbl(PANIMMUNE_DB2, "classes"), 
                by = c("class" = "id")
            ) %>% 
            dplyr::select(class = name.y, name = display, feature_id = id) %>% 
            dplyr::as_tibble() %>% 
            dplyr::group_by(class) %>%
            tidyr::nest() %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(
                class = dplyr::if_else(is.na(class), "Other", class),
                data = purrr::map(data, tibble::deframe)
            ) %>%
            tibble::deframe()
    })
    

    
    genes_list <- reactive({
        req(PANIMMUNE_DB2)
        # PANIMMUNE_DB2 %>%
        #     dplyr::tbl("genes_to_samples") %>%
        #     dplyr::filter(!is.na(status)) %>%
        #     dplyr::pull(gene_id)
        PANIMMUNE_DB2 %>% 
            dplyr::tbl("genes") %>% 
            dplyr::pull(hgnc)
    })
    
    # dataset selection ----
    
    output$module_selection_ui <- renderUI({
        choices <- dataset_to_module_tbl() %>% 
            dplyr::pull(module) %>% 
            unique()
        checkboxGroupInput(
            ns("module_choices"),
            "Select modules:",
            choices
        )
    })
    
    output$dataset_selection_ui <- renderUI({
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
        selectInput(
            inputId = ns("dataset_choice"),
            label = strong("Select Dataset"),
            choices = choices,
            selected = "TCGA"
        )
    })
    
    available_modules <- reactive({
        req(input$dataset_choice)
        dataset_to_module_tbl() %>% 
            dplyr::filter(dataset == input$dataset_choice) %>% 
            dplyr::pull(module)
    })
    
    output$module_availibility_string <- renderText({
        req(input$dataset_choice, available_modules())
        available_modules() %>% 
            stringr::str_c(collapse = ", ") %>% 
            stringr::str_c(
                "Modules available for dataset:", 
                input$dataset_choice,
                "are",
                .,
                sep = " "
            )
    })

    available_groups <- reactive({
        req(input$dataset_choice)
        dataset_to_group_tbl() %>% 
            dplyr::filter(dataset == input$dataset_choice) %>% 
            dplyr::pull(group)
    })
    
    
    output$select_group_ui <- renderUI({
        req(available_groups())

        selectInput(
            inputId = ns("group_choice"),
            label = strong("Select Grouping Variable"),
            choices = c(available_groups()),
            selected = "Immune Subtype"
        )
    })
    
    
    
    # insert/remove filters ----
    
    filter_groups <- reactive({
        req(available_groups())
        setdiff(available_groups(), c("Custom Numeric", "Custom Mutation"))
    })
    
    group_element_module <- reactive({
        req(filter_groups, groups_con)
        purrr::partial(
            group_filter_element_module,
            group_names_list = filter_groups,
            group_values_con = groups_con
        )
    })
    
    group_element_module_ui <- reactive(group_filter_element_module_ui)
    
    group_filter_output <- callModule(
        insert_remove_element_module2,
        "group_filter",
        element_module = group_element_module,
        element_module_ui = group_element_module_ui,
        remove_ui_event = reactive(input$dataset_choice)
    )
    
    
    numeric_element_module <- reactive({
        req(features_list, feature_values_con)
        
        purrr::partial(
            numeric_filter_element_module,
            feature_names_list = features_list,
            feature_values_con = feature_values_con
        )
        
    })
    
    numeric_element_module_ui <- reactive(numeric_filter_element_module_ui)
    
    numeric_filter_output <- callModule(
        insert_remove_element_module2,
        "numeric_filter",
        element_module = numeric_element_module,
        element_module_ui = numeric_element_module_ui,
        remove_ui_event = reactive(input$dataset_choice)
    )
    
    selected_samples <- reactive({
        req(samples_con(), group_filter_output(), numeric_filter_output())
        group_filters <- group_filter_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null))
        numeric_filters <- numeric_filter_output() %>% 
            reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null))
        samples <- samples_con() %>% 
            dplyr::pull(sample_id)
        for(item in group_filters){
            req(item$parent_group_choice, item$group_choices)
            if(item$parent_group_choice %in% c("Gender","Race", "Ethnicity")){
            } else if (item$parent_group_choice %in% c("Immune_Subtype", "TCGA_Subtype", "TCGA_Study")){
                sample_ids <- PANIMMUNE_DB2 %>% 
                    dplyr::tbl("tags") %>% 
                    dplyr::filter(name ==  local(item$parent_group_choice)) %>% 
                    dplyr::left_join(
                        dplyr::tbl(PANIMMUNE_DB2, "tags"),
                        by = c("id" = "parent")
                    ) %>% 
                    dplyr::filter(name.y %in% local(item$group_choices)) %>% 
                    dplyr::select(tag_id = id.y) %>% 
                    dplyr::left_join(
                        dplyr::tbl(PANIMMUNE_DB2, "samples_to_tags")
                    ) %>% 
                    dplyr::pull(sample_id)

                samples <- intersect(samples, sample_ids)
            } else {
                stop("bad selection")
            }
        }
        for(item in numeric_filters){
            req(item$feature_choice, item$feature_range)
            sample_ids <- PANIMMUNE_DB2 %>%
                dplyr::tbl("features_to_samples") %>%
                dplyr::select(sample_id, feature_id, value) %>% 
                dplyr::left_join(
                    dplyr::tbl(PANIMMUNE_DB2, "features"), 
                    by = c("feature_id" = "id")
                ) %>% 
                dplyr::select(- value.y) %>%  ### remove!!!
                dplyr::rename(value = value.x) %>%  ### remove!
                dplyr::filter(
                    feature_id == local(as.integer(item$feature_choice)),
                    value <=  local(item$feature_range[[2]]),
                    value >= local(item$feature_range[[1]])
                ) %>% 
                dplyr::pull(sample_id)
               
            
            samples <- intersect(samples, sample_ids)
        }
        
        
        return(samples)
    })
    
    output$samples_text <- renderText({
        c("Number of current samples:", length(selected_samples()))
    })
    
    # custom groups ----
    # This is so that the conditional panel can see the various reactives
    output$display_custom_numeric <- reactive(input$group_choice == "Custom Numeric")
    outputOptions(output, "display_custom_numeric", suspendWhenHidden = FALSE)
    output$display_custom_mutation <- reactive(input$group_choice == "Custom Mutation")
    outputOptions(output, "display_custom_mutation", suspendWhenHidden = FALSE)
    

  
    output$select_custom_numeric_group_ui <- renderUI({
        req(features_list())
        selectInput(
            inputId = ns("custom_numeric_feature_choice"),
            label = "Select feature:",
            choices = features_list()
        )
    })
    
    output$select_custom_mutation_group_ui <- renderUI({
        req(genes_list())
        selectInput(
            inputId = ns("custom_gene_mutaton_choice"),
            label = "Select gene:",
            choices = genes_list()
        )
    })
    



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

