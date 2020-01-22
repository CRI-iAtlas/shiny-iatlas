cohort_selection_server <- function(
    input, 
    output, 
    session
){
    ns <- session$ns
    
    source("modules/server/submodules/data_table_server.R", local = T)
    source("modules/server/submodules/insert_remove_element_server.R", local = T)
    source("modules/ui/submodules/elements_ui.R", local = T)
    source("modules/server/submodules/elements_server.R", local = T)
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
    
    dataset_to_group_tbl <- shiny::reactive({
        dplyr::tribble(
            ~group,            ~dataset, ~type,
            "Immune Subtype",  "TCGA",   "tag",
            "TCGA Subtype",    "TCGA",   "tag",
            "TCGA Study",      "TCGA",   "tag",
            # "Gender",          "TCGA",   "sample",
            # "Race",            "TCGA",   "sample",
            # "Ethnicity",       "TCGA",   "sample",
            "Custom Numeric",  "TCGA",   NA,
            "Custom Mutation", "TCGA",   NA
            # "Immune Subtype",  "PCAWG",  "tag",
            # "PCAWG Study",     "PCAWG",  "tag",
            # "Gender",          "PCAWG",  "sample",
            # "Race",            "PCAWG",  "sample",
            # "Custom Numeric",  "PCAWG",   NA
        ) 
    })
    
    dataset_tags <- shiny::reactive({
        shiny::req(dataset_to_group_tbl())
        dataset_to_group_tbl() %>% 
            dplyr::filter(
                dataset == input$dataset_choice,
                type == "tag"
            ) %>% 
            dplyr::pull(group)
    })
    
    samples_con <- shiny::reactive({
        con <- 
            create_connection("samples") %>% 
            dplyr::rename(sample_id = id, sample_barcode = sample_id) %>% 
            dplyr::compute()
    })
    
    tags_con <- shiny::reactive({
        con <- 
            create_connection("samples_to_tags") %>% 
            dplyr::left_join(
                create_connection("tags"),
                by = c("tag_id" = "id")
            ) %>% 
            dplyr::compute()
    })
    
    group_members_con <- shiny::reactive({
        shiny::req(dataset_tags())
        con <- 
            dplyr::inner_join(
                create_connection("tags") %>% 
                    dplyr::select(tag_id = id, name),
                create_connection("tags_to_tags"),
                by = c("tag_id")
            ) %>% 
            dplyr::inner_join(
                create_connection("tags") %>% 
                    dplyr::filter(display %in% local(dataset_tags())) %>% 
                    dplyr::select(related_tag_id = id, display),
                by = c("related_tag_id")
            ) %>% 
            dplyr::as_tibble() %>% 
            dplyr::select(group = name, parent_group = display) %>% 
            dplyr::compute() 
    })
    
    feature_values_con <- shiny::reactive({
        con <-
            create_connection("features_to_samples") %>%
            dplyr::select(sample_id, feature_id, value) %>% 
            dplyr::inner_join(
                create_connection("features"), 
                by = c("feature_id" = "id")
            ) %>% 
            dplyr::compute()
    })
    
    
    features_list <- shiny::reactive({
        list <- 
            dplyr::full_join(
                create_connection("features"),
                create_connection("classes"), 
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
    
    gene_mutation_list <- shiny::reactive({
        create_gene_mutation_list()
    })
    
    gene_expression_list <- shiny::reactive({
        list <- 
            dplyr::inner_join(
                create_connection("genes") %>% 
                    dplyr::select(hgnc, id),
                create_connection("genes_to_samples") %>%
                    dplyr::filter(!is.na(rna_seq_expr)) %>% 
                    dplyr::select(gene_id) %>% 
                    dplyr::distinct(),
                by = c("id" = "gene_id")
            ) %>% 
            dplyr::as_tibble() %>% 
            tibble::deframe()
    })
    
    # dataset selection ----
    
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
    
    available_groups <- shiny::reactive({
        shiny::req(selected_dataset())
        dataset_to_group_tbl() %>% 
            dplyr::filter(dataset == selected_dataset()) %>% 
            dplyr::pull(group)
    })
    
    
    # insert/remove filters ----
    
    filter_groups <- shiny::reactive({
        shiny::req(available_groups())
        setdiff(available_groups(), c("Custom Numeric", "Custom Mutation"))
    })
    
    group_element_module_server <- shiny::reactive({
        shiny::req(filter_groups, group_members_con)
        purrr::partial(
            group_filter_element_server,
            group_names_list = filter_groups,
            group_values_con = group_members_con
        )
    })
    
    group_element_module_ui <- shiny::reactive(group_filter_element_ui)
    
    group_filter_output <- shiny::callModule(
        insert_remove_element_server,
        "group_filter",
        element_module = group_element_module_server,
        element_module_ui = group_element_module_ui,
        remove_ui_event = shiny::reactive(selected_dataset())
    )
    
    numeric_element_module_server <- shiny::reactive({
        shiny::req(features_list, feature_values_con)
        
        purrr::partial(
            numeric_filter_element_server,
            feature_names_list = features_list,
            feature_values_con = feature_values_con
        )
        
    })
    
    numeric_element_module_ui <- shiny::reactive(numeric_filter_element_ui)
    
    numeric_filter_output <- shiny::callModule(
        insert_remove_element_server,
        "numeric_filter",
        element_module = numeric_element_module_server,
        element_module_ui = numeric_element_module_ui,
        remove_ui_event = shiny::reactive(selected_dataset())
    )
    
    sample_ids <- shiny::reactive({
        req(selected_dataset())
        get_all_dataset_ids(selected_dataset())
    })
    
    numeric_filter_samples <- shiny::reactive({
        shiny::req(sample_ids(), numeric_filter_output())
        samples <- sample_ids()
        numeric_filters <- numeric_filter_output() %>% 
            shiny::reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) 
        for(item in numeric_filters){
            shiny::req(
                item$feature_choice,
                item$feature_range[[1]],
                item$feature_range[[2]]
            )
            sample_ids <- get_numeric_filter_samples(
                item$feature_choice,
                item$feature_range[[1]],
                item$feature_range[[2]]
            )
            samples <- intersect(samples, sample_ids)
        }
        return(samples)
    })
    
    group_filter_samples <- shiny::reactive({
        shiny::req(sample_ids(), group_filter_output())
        group_filters <- group_filter_output() %>% 
            shiny::reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null))
        samples <- sample_ids()
        for(item in group_filters){
            shiny::req(
                item$parent_group_choice,
                item$group_choices
            )
            sample_ids <- get_group_filter_samples(
                item$parent_group_choice,
                item$group_choices
            )
            samples <- intersect(samples, sample_ids)
        }
        return(samples)
    })
    
    selected_samples <- shiny::reactive({
        shiny::req(numeric_filter_samples(), group_filter_samples())
        intersect(numeric_filter_samples(), group_filter_samples())
    })
    
    output$samples_text <- shiny::renderText({
        c("Number of current samples:", length(selected_samples()))
    })
    
    # custom selection ---
    output$select_group_ui <- shiny::renderUI({
        shiny::req(available_groups())
        
        shiny::selectInput(
            inputId = ns("group_choice"),
            label = strong("Select or Search for Grouping Variable"),
            choices = c(available_groups()),
            selected = "Immune Subtype"
        )
    })
    
    # This is so that the conditional panel can see the various shiny::reactives
    output$display_custom_numeric <- shiny::reactive(input$group_choice == "Custom Numeric")
    shiny::outputOptions(output, "display_custom_numeric", suspendWhenHidden = FALSE)
    output$display_custom_mutation <- shiny::reactive(input$group_choice == "Custom Mutation")
    shiny::outputOptions(output, "display_custom_mutation", suspendWhenHidden = FALSE)
    
    
    
    output$select_custom_numeric_group_ui <- shiny::renderUI({
        shiny::req(req(input$group_choice == "Custom Numeric"))
        selectInput(
            inputId = ns("custom_numeric_feature_choice"),
            label = "Select or Search for feature",
            choices = features_list()
        )
    })
    
    output$select_custom_mutation_group_ui <- shiny::renderUI({
        shiny::req(req(input$group_choice == "Custom Mutation"))
        selectInput(
            inputId = ns("custom_gene_mutaton_choice"),
            label = "Select or Search for gene",
            choices = gene_mutation_list()
        )
    })
    
    cohort_obj <- shiny::reactive({
        if(is.null(input$group_choice)){
            group_choice <- "Immune Subtype"  
        } else {
            group_choice <- input$group_choice
        }
        shiny::req(selected_samples())
        if (group_choice %in% c("Immune Subtype", "TCGA Subtype", "TCGA Study")){
            group_name <- group_choice
            group_tbl <- create_group_tbl1(group_choice) 
            sample_tbl <- selected_samples() %>% 
                create_sample_tbl1() %>% 
                dplyr::inner_join(group_tbl, by = "tag_id") %>% 
                dplyr::select(sample_id, sample_name, group) 
            group_tbl <- dplyr::select(group_tbl, - tag_id)
        } else if (group_choice == "Custom Mutation"){
            shiny::req(input$custom_gene_mutaton_choice)
            group_name <- "Mutation Status"
            sample_tbl <- create_sample_tbl2(
                selected_samples(), 
                input$custom_gene_mutaton_choice
            )
            group_tbl <- sample_tbl %>% 
                dplyr::select(group) %>% 
                dplyr::distinct() %>% 
                dplyr::mutate(
                    name = "",
                    characteristics = "",
                    color = NA
                )
        } else {
            stop("bad selection")
        }
        group_tbl <- sample_tbl %>% 
            dplyr::group_by(group) %>% 
            dplyr::summarise(size = dplyr::n()) %>%
            dplyr::ungroup() %>% 
            dplyr::inner_join(group_tbl, by = "group")
        
        if(any(is.na(group_tbl$color))){
            group_tbl <- dplyr::mutate(group_tbl, color = viridisLite::viridis(dplyr::n()))
        }
        plot_colors <- group_tbl %>% 
            dplyr::select(group, color) %>% 
            tibble::deframe() 
        list(
            "sample_tbl" = sample_tbl, 
            "group_tbl" = group_tbl, 
            "group_name" = group_name,
            "plot_colors" = plot_colors,
            "dataset" = selected_dataset(),
            "groups" = available_groups()
        )
        # } else if (group_choice == "Custom Mutation"){
            # shiny::req(input$custom_gene_mutaton_choice)
            # group_name <- input$custom_gene_mutaton_choice
            # sample_con <- 
            #     create_connection("genes_to_samples") %>%
            #     dplyr::filter(
            #         gene_id == as.integer(local(input$custom_gene_mutaton_choice)),
            #         !is.na(status),
            #         sample_id %in% local(selected_samples())
            #     ) %>% 
            #     dplyr::select(sample_id, group = status) %>% 
            #     dplyr::compute()
            # group_con <- sample_con %>% 
            #     dplyr::group_by(group) %>% 
            #     dplyr::summarise(size = dplyr::n()) %>% 
            #     dplyr::ungroup() %>% 
            #     dplyr::mutate(
            #         group = as.character(group),
            #         name = "",
            #         characteristics = "",
            #         color = NA
            #     ) %>% 
            #     dplyr::compute()
        # } else if (group_choice == "Custom Numeric"){
            # shiny::req(
            #     input$custom_numeric_feature_choice,
            #     input$custom_numeric_group_number_choice
            # )
            # group_name <- input$custom_numeric_feature_choice
            # sample_con <-
            #     create_connection("features_to_samples") %>%
            #     dplyr::filter(
            #         feature_id == as.integer(local(input$custom_numeric_feature_choice)),
            #         !is.na(value),
            #         sample_id %in% local(selected_samples())
            #     ) %>%
            #     dplyr::mutate(group = value - value %% (max(value) / (local(input$custom_numeric_group_number_choice) -1))) %>%
            #     dplyr::mutate(group = (group / (max(value) / (local(input$custom_numeric_group_number_choice) -1))) + 1) %>%
            #     dplyr::mutate(group = as.character(as.integer(group))) %>% 
            #     dplyr::select(sample_id, group) %>% 
            #     dplyr::compute() 
            # group_con <- sample_con %>% 
            #     dplyr::group_by(group) %>% 
            #     dplyr::summarise(size = dplyr::n()) %>% 
            #     dplyr::ungroup() %>% 
            #     dplyr::mutate(
            #         group = as.character(group),
            #         name = "",
            #         characteristics = "",
            #         color = NA
            #     ) %>% 
            #     dplyr::compute()
    })
    
    # group key ---------------------------------------------------------------
    
    group_key_tbl <- shiny::reactive({
        shiny::req(cohort_obj())
        tbl <- cohort_obj()$group_tbl %>% 
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

