drivers_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Association with Driver Mutations"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/driver.markdown")
        ),
        univariate_driver_ui(ns("univariate_driver")),
        # multivariate_driver_ui(ns("multivariate_driver"))
    )
}

# Server ----
drivers <- function(
    input, 
    output, 
    session,
    features_con,
    feature_values_con,
    features_named_list
){
    ns <- session$ns

    callModule(
        univariate_driver,
        "univariate_driver",
        features_con,
        feature_values_con,
        features_named_list
    )
    
    callModule(
        multivariate_driver,
        "multivariate_driver",
        features_con,
        feature_values_con,
        features_named_list
    )
 
}

univariate_driver_ui <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Immune Response Association With Driver Mutations -- single variable",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/driver_single.markdown")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 4,
                    uiOutput(ns("response_options"))
                ),
                column(
                    width = 4,
                    numericInput(
                        ns("min_mut"),
                        "Minimum number of mutant samples per group:",
                        20,
                        min = 2
                    )
                ),
                column(
                    width = 4,
                    numericInput(
                        ns("min_wt"),
                        "Minimum number of wild type samples per group:",
                        20,
                        min = 2
                    )
                )
            )
        ),
        volcano_plot_module_ui(ns("univariate_driver"))
    )
}

univariate_driver <- function(
    input, 
    output, 
    session,
    features_con,
    feature_values_con,
    features_named_list
){
    ns <- session$ns
    
    ## single variable models ----
    
    output$response_options <- renderUI({
        req(features_named_list())
        selectInput(
            ns("response_variable"),
            "Select Response Variable",
            choices = features_named_list(),
            selected = 36
        )
    })
    
    volcano_con <- reactive({
        req(
            input$response_variable,
            input$min_wt,
            input$min_mut
        )
        
        tbl <- create_conection("driver_results") %>% 
            # dplyr::filter(
            #     feature_id == local(input$response_variable),
            #     n_wt    >= local(input$min_wt),
            #     n_mut   >= local(input$min_mut)
            # ) %>% 
            dplyr::filter(
                feature_id == 36,
            ) %>%
            dplyr::select(-c(n_wt, n_mut)) %>% 
            # dplyr::left_join(
            #     dplyr::select(features_con(), feature_id, feature_name),
            #     by = "feature_id"
            # ) %>% 
            dplyr::left_join(
                dplyr::select(create_conection("genes"), id, hgnc),
                by = ("gene_id" = "id")
            ) %>% 
            dplyr::select(gene_id, hgnc) %>% 
            dplyr::collect()
            dplyr::left_join(
                dplyr::select(create_conection("tags"), id, group = display),
                by = c("tag_id" = "id")
            ) %>% 
            dplyr::select(-c(feature_id, gene_id, tag_id)) %>% 
            dplyr::compute() 
    })
    
    violin_value_con <- reactive({
        req(feature_values_con(), input$response_variable)
        feature_values_con() %>% 
            dplyr::filter(feature == local(input$response_variable)) %>% 
            dplyr::select(sample, value) 
    })
    
    violin_group_con <- reactive({
        req(driver_mutations_con())
        driver_mutations_con() %>% 
            dplyr::mutate(label = paste0(gene, ";", group)) %>% 
            dplyr::select(sample, label, status)
    })
    
    
    callModule(
        volcano_plot_module,
        "univariate_driver",
        volcano_con,
        violin_value_con,
        violin_group_con,
        "Immune Response Association With Driver Mutations",
        "univariate_driver",
        "Wt",
        "Mut"
    ) 
}

multivariate_driver_ui <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Immune Response Association With Driver Mutations -- multivariate",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/driver_multi.markdown")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 3,
                    uiOutput(ns("response_options"))
                ),
                column(
                    width = 3,
                    numericInput(
                        ns("min_mutants"),
                        "Minimum number of mutant samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                column(
                    width = 3,
                    numericInput(
                        ns("min_wildtype"),
                        "Minimum number of wild type samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                column(
                    width = 3,
                    selectInput(
                        ns("group_mode"),
                        "Select mode", 
                        choices = c("By group", "Across groups"),
                        selected = "Across groups"
                    )
                ),
            )
        ),
        model_selection_module_ui(ns("module1")),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 6,
                    textOutput(ns("model_text"))
                ),
                column(
                    width = 6,
                    actionButton(ns("calculate_button"), "Calculate")
                )
            )
        ),
        volcano_plot_module_ui(ns("multivariate_driver"))
    )
}


multivariate_driver <- function(
    input, 
    output, 
    session,
    features_con,
    feature_values_con,
    features_named_list
){
    
    ns <- session$ns
    
    output$response_options <- renderUI({
        req(features_named_list(), features_con())
        selected_id <- features_con() %>% 
            dplyr::filter(feature_name == "Leukocyte Fraction") %>% 
            dplyr::pull(feature_id)
        
        selectInput(
            ns("response_choice_id"),
            "Select Response Variable",
            choices = features_named_list(),
            selected = selected_id
        )
    })
    
    
    numerical_covariate_tbl <- reactive({
        features_con() %>%
            dplyr::select(
                class = class_name, 
                display = feature_name, 
                feature = feature_id,
                internal = feature_internal_name
            ) %>% 
            dplyr::collect() 
    })
    
    categorical_covariate_tbl <- reactive({
        dplyr::tribble(
            ~class,     ~display,         ~feature,         ~internal,
            "Groups",   "Immune Subtype", "Immune_Subtype", "Immune_Subtype",
            "Groups",   "TCGA Study",     "TCGA_Study",     "TCGA_Study",
            "Groups",   "TCGA Subtype",   "TCGA_Subtype",   "TCGA_Subtype"
            # "Clinical", "Gender",         "Gender", 
            # "Clinical", "Race",           "Race",
            # "Clinical", "Ethnicty",       "Ethnicty"
        )
    })
    
    response_variable_name <- reactive({
        req(features_con(), input$response_choice_id)
        translate_value(
            features_con(),
            as.integer(input$response_choice_id),
            "feature_id", 
            "feature_name"
        )
    })
    
    model_string_prefix <- reactive({
        req(response_variable_name())
        stringr::str_c(response_variable_name(), " ~ Mutation status") 
    })
    
    module_parameters <- callModule(
        model_selection_module, 
        "module1", 
        numerical_covariate_tbl,
        categorical_covariate_tbl,
        model_string_prefix
    )
    
    output$model_text <- renderText({
        module_parameters()$display_string
    })
    
    response_con <- reactive({
        
        req(
            feature_values_con(), 
            input$response_choice_id,
            input$group_mode
        )
        res_con <- feature_values_con() %>% 
            dplyr::filter(feature_id == local(input$response_choice_id)) %>% 
            dplyr::select(sample_id, response = value, group) 
        if(input$group_mode == "Across groups") {
            res_con <- dplyr::select(res_con, -group)
        }
        dplyr::compute(res_con)
    })
    
    status_con <- reactive({
        create_conection("gene_types") %>%
            dplyr::filter(name == "driver_mutation") %>%
            dplyr::select(type_id = id) %>%
            dplyr::inner_join(
                create_conection("genes_to_types"),
                by = "type_id"
            ) %>%
            dplyr::select(gene_id) %>%
            dplyr::filter(gene_id < 5L) %>% # remove!
            dplyr::inner_join(
                create_conection("genes"),
                by = c("gene_id" = "id")
            ) %>%
            dplyr::select(gene_id, gene_name = hgnc) %>%
            dplyr::inner_join(
                create_conection("genes_to_samples") %>% 
                    dplyr::filter(!is.na(status)),
                by = "gene_id"
            ) %>%
            dplyr::select(sample_id, gene_name, status) %>%
            dplyr::compute()
    })
    
    group_covariate_tbl <- reactive({
        covariates <- 
            module_parameters()$categorical_covariates %>% 
            intersect(c("Immune_Subtype", "TCGA_Subtype", "TCGA_Study"))
        if(length(covariates) == 0){
            res <- NULL
        } else {
            res <- create_conection("tags") %>% 
                dplyr::filter(name %in% covariates) %>%
                dplyr::select(parent_group_id = id, parent_group = name) %>% 
                dplyr::inner_join(
                    create_conection("tags_to_tags"),
                    by = c("parent_group_id" = "related_tag_id")
                ) %>% 
                dplyr::inner_join(
                    create_conection("tags"),
                    by = c("tag_id" = "id")
                ) %>% 
                dplyr::select(parent_group, group = name, tag_id) %>% 
                dplyr::inner_join(
                    create_conection("samples_to_tags"),
                    by = "tag_id"
                ) %>% 
                dplyr::collect() %>% 
                dplyr::select(parent_group, group, sample_id) %>% 
                tidyr::pivot_wider(values_from = group, names_from = parent_group) %>% 
                tidyr::drop_na()
        }
        return(res)
    })
    
    feature_covariate_tbl <- reactive({
        covariate_ids <- module_parameters()$numerical_covariates
        
        if(is.null(covariate_ids)){
            res <- NULL
        } else {
            req(feature_values_con())
            res <- feature_values_con() %>% 
                dplyr::filter(feature_id %in% covariate_ids) %>% 
                dplyr::inner_join(features_con(), by = "feature_id") %>% 
                dplyr::select(sample_id, feature_internal_name, value) %>% 
                dplyr::collect() %>% 
                tidyr::pivot_wider(values_from = value, names_from = feature_internal_name)
        }
        return(res)
    })
    
    
    combined_con <- reactive({
        
        req(
            response_con(),
            status_con()
        )
        
        con <- 
            dplyr::inner_join(
                response_con(), 
                status_con(), 
                by = "sample_id",
            ) %>% 
            dplyr::compute()
    })
    
    cov_combined_con <- reactive({

        req(
            combined_con(),
            input$group_mode
        )

        con <- combined_con()

        if (!is.null(feature_covariate_tbl())){
            con <- dplyr::inner_join(
                con, 
                feature_covariate_tbl(), 
                by = "sample_id",
                copy = T
            )
        }
        if (!is.null(group_covariate_tbl())){
            con <- dplyr::inner_join(
                con, 
                group_covariate_tbl(), 
                by = "sample_id",
                copy = T
            )
        }
        con <- dplyr::select(con, -sample_id) 
        
        if(input$group_mode == "Across groups") {
            con <- con %>% 
                dplyr::rename(label = gene_name) %>% 
                dplyr::compute()
        } else{
            con <- con %>% 
                dplyr::mutate(label = paste0(gene_name, group, sep = ";")) %>% 
                dplyr::select(-c(gene_name, group)) %>% 
                dplyr::compute()
        }
        return(con)
    })
    
    
    
    summary_con <- reactive({
        req(
            cov_combined_con(),
            input$min_mutants,
            input$min_wildtype,
        )
        summary_con <- cov_combined_con() %>% 
            dplyr::group_by(label) %>% 
            dplyr::mutate(status = dplyr::if_else(
                status == "Wt",
                1L,
                0L
            )) %>% 
            dplyr::summarise(
                n_total = dplyr::n(),
                n_wt = sum(status),
            ) %>% 
            dplyr::mutate(n_mut = n_total - n_wt) %>% 
            dplyr::filter(
                n_mut >= local(input$min_mutants),
                n_wt >= local(input$min_wildtype),
            ) %>% 
            dplyr::ungroup() %>% 
            dplyr::select(-c(n_mut, n_total, n_wt)) %>% 
            dplyr::compute() 
            
    })
    
    combined_con2 <- reactive({

        req(
            cov_combined_con(),
            summary_con()
        )
        
        cov_combined_con() %>% 
            dplyr::inner_join(summary_con()) %>% 
            dplyr::compute()
    })
    
    model_tbl <- reactive({

        req(
            combined_con2(),
            module_parameters()$formula_string
        )

        combined_con2() %>% 
            dplyr::collect() %>% 
            tidyr::nest(tbl = -label) %>%
            dplyr::mutate(p_value = as.double(parallel::mclapply(
                tbl,
                calculate_lm_pvalue,
                module_parameters()$formula_string,
                "statusWt"
            ))) %>%
            dplyr::filter(!is.na(p_value)) %>% 
            dplyr::select(-tbl) %>%
            dplyr::mutate(log10_p_value = -log10(p_value))
    })
    
    effect_size_tbl <- reactive({

        req(
            combined_con2()
        )
        combined_con2() %>% 
            dplyr::collect() %>% 
            dplyr::select(label, response, status) %>% 
            dplyr::group_by(label, status) %>%
            dplyr::summarise(responses = list(response)) %>% 
            dplyr::mutate(status = as.character(status)) %>% 
            tidyr::pivot_wider(names_from = status, values_from = responses) %>%
            dplyr::rename(GROUP1 = Mut, GROUP2 = Wt) %>%
            tidyr::nest(data = c(GROUP1, GROUP2)) %>% 
            dplyr::mutate(fold_change = as.double(parallel::mclapply(
                data, 
                get_effect_size_from_df,
                ratio_effect_size
            ))) %>% 
            dplyr::mutate(log10_fold_change = -log10(fold_change)) %>% 
            dplyr::select(-data) %>% 
            tidyr::drop_na() 
    })
    
    volcano_tbl <- eventReactive(input$calculate_button, {

        req(
            model_tbl(),
            effect_size_tbl()
        )
        
        tbl <-
            dplyr::inner_join(
                model_tbl(),
                effect_size_tbl(),
                by = "label"
            ) 
    })
    
    callModule(
        volcano_plot_module,
        "multivariate_driver",
        volcano_tbl,
        combined_con2,
        "Immune Response Association With Driver Mutations",
        "multivariate_driver",
        "Wt",
        "Mut",
        response_variable_name
    )
    

}



