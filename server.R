################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100 * 1024^2)

################################################################################
# Begin Shiny Server definition.
################################################################################
shinyServer(function(input, output, session) {
  
  observe({
  query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['module']])) {
      shinydashboard::updateTabItems(session, "explorertabs", query[['module']])
    }
  })
    
    # analysis modules --------------------------------------------------------
    
    
    features_con <- reactive({
        con <- 
            dplyr::left_join(
                create_conection("features"),
                create_conection("classes"),
                by = c("class_id" = "id")
            ) %>% 
            dplyr::left_join(
                create_conection("method_tags"),
                by = c("method_tag_id" = "id")
            ) %>% 
            dplyr::select(
                class_name = name.y, 
                class_id, 
                feature_name = display, 
                feature_id = id,
                method_tag = name,
                method_tag_id,
                order,
                unit
            ) %>% 
            dplyr::compute() 
    })
    
    genes_con <- reactive({
        create_conection("genes") %>% 
            dplyr::left_join(
                create_conection("gene_families"), 
                by = c("gene_family_id" = "id")
            ) %>% 
            dplyr::rename(gene_family = name) %>% 
            dplyr::left_join(
                create_conection("gene_functions"), 
                by = c("gene_function_id" = "id")
            ) %>% 
            dplyr::rename(gene_function = name) %>% 
            dplyr::left_join(
                create_conection("immune_checkpoints"), 
                by = c("immune_checkpoint_id" = "id")
            ) %>% 
            dplyr::rename(immune_checkpoint = name) %>%
            dplyr::left_join(
                create_conection("pathways"), 
                by = c("pathway_id" = "id")
            ) %>% 
            dplyr::rename(pathway = name) %>% 
            dplyr::left_join(
                create_conection("super_categories"), 
                by = c("super_cat_id" = "id")
            ) %>% 
            dplyr::rename(super_category = name) %>% 
            dplyr::left_join(
                create_conection("therapy_types"), 
                by = c("therapy_type_id" = "id")
            ) %>% 
            dplyr::rename(therapy = name) %>% 
            dplyr::select(
                gene_id = id,
                entrez,
                hgnc,
                canonical,
                display,
                description,
                references,
                gene_family,
                gene_function,
                immune_checkpoint,
                pathway,
                super_category,
                therapy
            ) %>% 
            dplyr::compute()
    })
    

    
    cohort_cons <- callModule(
        cohort_selection,
        "cohort_selection"
    )
    
    cohort_sample_con <- reactive(cohort_cons()$sample_con)
    cohort_group_con  <- reactive(cohort_cons()$group_con)
    cohort_group_name <- reactive(cohort_cons()$group_name)
    cohort_colors     <- reactive(cohort_cons()$plot_colors)
    
    cohort_feature_values_con <- reactive({
        req(cohort_sample_con())
        tbl <- 
            create_conection("features_to_samples") %>% 
            dplyr::filter(sample_id %in% local(cohort_sample_con()$sample_id)) %>%
            dplyr::select(sample_id, feature_id, value) %>% 
            dplyr::compute()
    })
    
    # Data info
    callModule(
        datainfo,
        "moduleX",
        features_con
    )
    
    # Cell content
    callModule(
        cellcontent,
        "module1",
        cohort_feature_values_con,
        features_con,
        cohort_sample_con,
        cohort_group_con
    )

    #Immune features
    callModule(
        immunefeatures,
        "module6",
        cohort_feature_values_con,
        features_con,
        cohort_sample_con,
        cohort_group_con,
        cohort_group_name,
        cohort_colors
    )
    
    # Immunomodulators
    callModule(
        immunomodulator,
        "module5",
        cohort_sample_con,
        cohort_group_con,
        genes_con,
        cohort_group_name,
        cohort_colors
    )

    # Survival curves
    callModule(
        survival,
        "module4",
        cohort_feature_values_con,
        features_con,
        cohort_sample_con,
        cohort_group_con,
        cohort_group_name,
        cohort_colors
    )
    
    # # IO Target
    callModule(
        iotarget,
        "module9",
        cohort_sample_con,
        cohort_group_con,
        genes_con,
        cohort_group_name,
        cohort_colors
    )
    


    
    # # TILmap features
    # callModule(
    #     tilmap,
    #     "module7",
    #     group_display_choice,
    #     subset_groups_con,
    #     til_image_links_con,
    #     subset_feature_values_long_con,
    #     features_con,
    #     plot_colors
    # )
    # 
    # Driver associations
    # callModule(
    #     drivers,
    #     "module8",
    #     subset_driver_results_con,
    #     subset_driver_mutations_con,
    #     subset_feature_values_long_con,
    #     subset_feature_values_wide_con,
    #     features_named_list,
    #     categories_con,
    #     subset_category_values_wide_con
    # )

    # subtype predictor
    callModule(
        subtypeclassifier, 
        "module_subtypeclassifier")

    observeEvent(input$link_to_module1, {
        shinydashboard::updateTabItems(session, "explorertabs", "cell_content")
    })
    observeEvent(input$link_to_module2, {
        shinydashboard::updateTabItems(session, "explorertabs", "clonal_diversity")
    })
    observeEvent(input$link_to_cohort_selection_module, {
        shinydashboard::updateTabItems(session, "explorertabs", "cohort_selection")
    })
    observeEvent(input$link_to_module4, {
        shinydashboard::updateTabItems(session, "explorertabs", "survival_curves")
    })
    observeEvent(input$link_to_module5, {
        shinydashboard::updateTabItems(session, "explorertabs", "immunomodulators")
    })
    observeEvent(input$link_to_module6, {
        shinydashboard::updateTabItems(session, "explorertabs", "immune_features")
    })
    observeEvent(input$link_to_module8, {
        shinydashboard::updateTabItems(session, "explorertabs", "drivers")
    })
    observeEvent(input$link_to_module7, {
        shinydashboard::updateTabItems(session, "explorertabs", "tilmap_features")
    })
    observeEvent(input$link_to_module9, {
      shinydashboard::updateTabItems(session, "explorertabs", "iotargets")
    })
    observeEvent(input$link_to_module_subtypeclassifier, {
        updateNavlistPanel(session, "toolstabs", "Immune Subtype Classifier")
    })

})


