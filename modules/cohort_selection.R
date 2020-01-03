cohort_selection_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Cohort Selection"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/sample_groups.markdown")
        ),
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

cohort_selection <- function(
    input, 
    output, 
    session
){
    ns <- session$ns
    
    
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
            ~group,            ~dataset, ~type,
            "Immune Subtype",  "TCGA",   "tag",
            "TCGA Subtype",    "TCGA",   "tag",
            "TCGA Study",      "TCGA",   "tag",
            "Gender",          "TCGA",   "sample",
            "Race",            "TCGA",   "sample",
            "Ethnicity",       "TCGA",   "sample",
            "Custom Numeric",  "TCGA",   NA,
            "Custom Mutation", "TCGA",   NA,
            "Immune Subtype",  "PCAWG",  "tag",
            "PCAWG Study",     "PCAWG",  "tag",
            "Gender",          "PCAWG",  "sample",
            "Race",            "PCAWG",  "sample",
            "Custom Numeric",  "PCAWG",   NA
        ) 
    })
    
    dataset_tags <- reactive({
        req(dataset_to_group_tbl())
        dataset_to_group_tbl() %>% 
            dplyr::filter(
                dataset == input$dataset_choice,
                type == "tag"
            ) %>% 
            dplyr::pull(group)
    })
    
    samples_con <- reactive({
        con <- 
            create_conection("samples") %>% 
            dplyr::rename(sample_id = id, sample_barcode = sample_id)
    })
    
    tags_con <- reactive({
        con <- 
            create_conection("samples_to_tags") %>% 
            dplyr::left_join(
                dplyr::tbl(PANIMMUNE_DB2, "tags"),
                by = c("tag_id" = "id")
            ) 
    })
    
    group_members_con <- reactive({
        req(dataset_tags())
        con <- 
            dplyr::inner_join(
                create_conection("tags") %>% 
                    dplyr::select(tag_id = id, name),
                create_conection("tags_to_tags"),
                by = c("tag_id")
            ) %>% 
            dplyr::inner_join(
                create_conection("tags") %>% 
                    dplyr::filter(display %in% local(dataset_tags())) %>% 
                    dplyr::select(related_tag_id = id, display),
                by = c("related_tag_id")
            ) %>% 
            dplyr::as_tibble() %>% 
            dplyr::select(group = name, parent_group = display)
    })
    
    feature_values_con <- reactive({
        con <-
            create_conection("features_to_samples") %>%
            dplyr::select(sample_id, feature_id, value) %>% 
            dplyr::inner_join(
                create_conection("features"), 
                by = c("feature_id" = "id")
            ) 
    })
    
    
    features_list <- reactive({
        list <- 
            dplyr::full_join(
                create_conection("features"),
                create_conection("classes"), 
                by = c("class_id" = "id")
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
    
    gene_mutation_list <- reactive({
        list <- 
            dplyr::inner_join(
                create_conection("genes") %>% 
                    dplyr::select(hgnc, id),
                create_conection("genes_to_samples") %>%
                    dplyr::filter(!is.na(status)) %>% 
                    dplyr::select(gene_id) %>% 
                    dplyr::distinct(),
                by = c("id" = "gene_id")
            ) %>% 
            dplyr::as_tibble() %>% 
            tibble::deframe()
    })
    
    gene_expression_list <- reactive({
        list <- 
            dplyr::inner_join(
                create_conection("genes") %>% 
                    dplyr::select(hgnc, id),
                create_conection("genes_to_samples") %>%
                    dplyr::filter(!is.na(rna_seq_expr)) %>% 
                    dplyr::select(gene_id) %>% 
                    dplyr::distinct(),
                by = c("id" = "gene_id")
            ) %>% 
            dplyr::as_tibble() %>% 
            tibble::deframe()
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
                "Modules available for dataset :", 
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
    
    
    # insert/remove filters ----
    
    filter_groups <- reactive({
        req(available_groups())
        setdiff(available_groups(), c("Custom Numeric", "Custom Mutation"))
    })
    
    group_element_module <- reactive({
        req(filter_groups, group_members_con)
        purrr::partial(
            group_filter_element_module,
            group_names_list = filter_groups,
            group_values_con = group_members_con
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
            } else if (item$parent_group_choice %in% c("Immune Subtype", "TCGA Subtype", "TCGA Study")){
                sample_ids <- 
                    dplyr::inner_join(
                        create_conection("tags_to_tags"),
                        create_conection("tags") %>%
                            # dplyr::filter(display == "Immune Subtype"), 
                            dplyr::filter(display ==  local(item$parent_group_choice)),
                        by = c("related_tag_id" = "id")
                    ) %>% 
                    dplyr::select(tag_id) %>% 
                    dplyr::inner_join(
                        create_conection("tags"),
                        by = c("tag_id" = "id")
                    ) %>%
                    dplyr::filter(name %in% local(item$group_choices)) %>%
                    # dplyr::filter(name %in% c("C1", "C2")) %>%
                    dplyr::select(tag_id) %>%
                    dplyr::inner_join(
                        create_conection("samples_to_tags")
                    ) %>%
                    dplyr::pull(sample_id)
                samples <- intersect(samples, sample_ids)
            } else {
                stop("bad selection")
            }
        }
        for(item in numeric_filters){
            req(item$feature_choice, item$feature_range)
            sample_ids <- 
                create_conection("features_to_samples") %>%
                dplyr::select(sample_id, feature_id, value) %>% 
                dplyr::left_join(
                    create_conection("features"), 
                    by = c("feature_id" = "id")
                ) %>% 
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
    
    # custom selection ---
    output$select_group_ui <- renderUI({
        req(available_groups())
        
        selectInput(
            inputId = ns("group_choice"),
            label = strong("Select Grouping Variable"),
            choices = c(available_groups()),
            selected = "Immune Subtype"
        )
    })
    
    # This is so that the conditional panel can see the various reactives
    output$display_custom_numeric <- reactive(input$group_choice == "Custom Numeric")
    outputOptions(output, "display_custom_numeric", suspendWhenHidden = FALSE)
    output$display_custom_mutation <- reactive(input$group_choice == "Custom Mutation")
    outputOptions(output, "display_custom_mutation", suspendWhenHidden = FALSE)
    
    
    
    output$select_custom_numeric_group_ui <- renderUI({
        req(req(input$group_choice == "Custom Numeric"))
        selectInput(
            inputId = ns("custom_numeric_feature_choice"),
            label = "Select feature:",
            choices = features_list()
        )
    })
    
    output$select_custom_mutation_group_ui <- renderUI({
        req(req(input$group_choice == "Custom Mutation"))
        selectInput(
            inputId = ns("custom_gene_mutaton_choice"),
            label = "Select gene:",
            choices = gene_mutation_list()
        )
    })
    
    cohort_obj <- reactive({
        if(is.null(input$group_choice)){
            group_choice <- "Immune Subtype"  
        } else {
            group_choice <- input$group_choice
        }
        req(selected_samples())
        if(group_choice %in% c("Gender","Race", "Ethnicity")){
            group_name <- group_choice
        } else if (group_choice %in% c("Immune Subtype", "TCGA Subtype", "TCGA Study")){
            group_name <- group_choice
            parent_id <-  
                create_conection("tags") %>% 
                dplyr::filter(display == group_choice) %>%
                # dplyr::filter(display == "Immune Subtype") %>%
                dplyr::pull(id)
            sample_con <- 
                create_conection("samples_to_tags") %>%
                dplyr::left_join(
                    create_conection("tags"),
                    by = c("tag_id" = "id")
                ) %>% 
                dplyr::inner_join(
                    create_conection("tags_to_tags"),
                    by = c("tag_id")
                ) %>% 
                dplyr::filter(
                    related_tag_id == parent_id,
                    sample_id %in% local(selected_samples())
                    # sample_id %in% c(9747, 9475)
                ) %>% 
                dplyr::select(sample_id, tag_id, group = name)
            group_con <- sample_con %>% 
                dplyr::group_by(tag_id) %>% 
                dplyr::summarise(size = dplyr::n()) %>% 
                dplyr::inner_join(
                    create_conection("tags"), 
                    by = c("tag_id" = "id")
                ) %>% 
                dplyr::select(group = name, name = display, size, characteristics, color)
            sample_con <- dplyr::select(sample_con, -tag_id)
        } else if (group_choice == "Custom Mutation"){
            req(input$custom_gene_mutaton_choice)
            group_name <- input$custom_gene_mutaton_choice
            sample_con <- 
                create_conection("genes_to_samples") %>%
                dplyr::filter(
                    gene_id == as.integer(local(input$custom_gene_mutaton_choice)),
                    !is.na(status),
                    sample_id %in% local(selected_samples())
                ) %>% 
                dplyr::select(sample_id, group = status)
            group_con <- sample_con %>% 
                dplyr::group_by(group) %>% 
                dplyr::summarise(size = dplyr::n()) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(
                    group = as.character(group),
                    name = "",
                    characteristics = "",
                    color = NA
                )
        } else if (group_choice == "Custom Numeric"){
            req(
                input$custom_numeric_feature_choice,
                input$custom_numeric_group_number_choice
            )
            group_name <- input$custom_numeric_feature_choice
            sample_con <-
                create_conection("features_to_samples") %>%
                dplyr::filter(
                    # feature_id == 1,
                    feature_id == as.integer(local(input$custom_numeric_feature_choice)),
                    !is.na(value),
                    # sample_id %in% c(1L,2L,3L,4L,5L)
                    sample_id %in% local(selected_samples())
                ) %>%
                # dplyr::mutate(group = value - value %% (max(value) / (2 -1))) %>% 
                # dplyr::mutate(group = (group / (max(value) / (2 -1))) + 1) %>% 
                dplyr::mutate(group = value - value %% (max(value) / (local(input$custom_numeric_group_number_choice) -1))) %>%
                dplyr::mutate(group = (group / (max(value) / (local(input$custom_numeric_group_number_choice) -1))) + 1) %>%
                dplyr::mutate(group = as.character(as.integer(group))) %>% 
                dplyr::select(sample_id, group)
            group_con <- sample_con %>% 
                dplyr::group_by(group) %>% 
                dplyr::summarise(size = dplyr::n()) %>% 
                dplyr::ungroup() %>% 
                dplyr::mutate(
                    group = as.character(group),
                    name = "",
                    characteristics = "",
                    color = NA
                )
        } else {
            stop("bad selection")
        }
        color_tbl <- group_con %>% 
            dplyr::select(group, color) %>% 
            dplyr::as_tibble()
        if(any(is.na(color_tbl$color))){
            color_tbl <- dplyr::mutate(color_tbl, color = viridisLite::viridis(dplyr::n()))
        }
        plot_colors <- tibble::deframe(color_tbl)
        list(
            "sample_con" = sample_con, 
            "group_con" = group_con, 
            "group_name" = group_name,
            "plot_colors" = plot_colors
        )
    })
    
    # group key ---------------------------------------------------------------
    
    group_key_tbl <- reactive({
        req(cohort_obj())
        tbl <- cohort_obj()$group_con %>% 
            dplyr::select(
                `Sample Group` = group,
                `Group Name` = name,
                `Group Size` = size,
                Characteristics = characteristics,
                `Plot Color` = color
            ) %>% 
            dplyr::as_tibble()
        if(any(is.na(tbl$`Plot Color`))){
            tbl <- dplyr::mutate(tbl, `Plot Color` = viridisLite::viridis(dplyr::n()))
        }
        return(tbl)
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
    
    # return ------------------------------------------------------------------
    return(cohort_obj)
}

