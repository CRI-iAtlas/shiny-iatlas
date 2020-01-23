################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100 * 1024^2)

################################################################################
# Begin Shiny Server definition.
################################################################################
shiny::shinyServer(function(input, output, session) {
    
    shiny::observe({
        query <- shiny::parseQueryString(session$clientData$url_search)
        if (!is.null(query[['module']])) {
            shinydashboard::updateTabItems(
                session, 
                "explorertabs", 
                query[['module']]
            )
        }
    })
    
    
    # analysis modules --------------------------------------------------------
    analysis_module_dir   <- "modules/server/analysis_modules/"
    analysis_module_files <- list.files(analysis_module_dir, full.names = T)
    for(item in analysis_module_files){
        source(item, local = T)
    }

    cohort_cons <- shiny::callModule(
        cohort_selection_server,
        "cohort_selection"
    )
    
    cohort_sample_tbl  <- shiny::reactive(cohort_cons()$sample_tbl)
    cohort_group_tbl   <- shiny::reactive(cohort_cons()$group_tbl)
    cohort_group_name  <- shiny::reactive(cohort_cons()$group_name)
    cohort_colors      <- shiny::reactive(cohort_cons()$plot_colors) 
    cohort_dataset     <- shiny::reactive(cohort_cons()$dataset) 
    cohort_groups      <- shiny::reactive(cohort_cons()$groups) 
    
    feature_named_list <- reactive({
        subquery1 <- "SELECT id, display, class_id FROM features"
        subquery2 <- "SELECT * FROM classes"
        
        query <- paste(
            "SELECT b.name AS class, a.display, a.id AS feature FROM",
            "(", subquery1, ") a",
            "LEFT OUTER JOIN",
            "(", subquery2, ") b",
            "ON a.class_id = b.id"
        )
        
        query %>%
            dplyr::sql() %>% 
            .GlobalEnv$perform_query("build feature table") %>% 
            dplyr::mutate(class = dplyr::if_else(
                is.na(class),
                "Other",
                class
            )) %>% 
            .GlobalEnv$create_nested_named_list()
    })

    shiny::callModule(
        tumor_microenvironment_server,
        "tumor_microenvironment",
        cohort_sample_tbl,
        cohort_group_tbl
    )
    
    shiny::callModule(
        immune_features_server,
        "immune_features",
        cohort_sample_tbl,
        cohort_group_tbl,
        cohort_group_name,
        feature_named_list,
        cohort_colors
    )
    # 
    # shiny::callModule(
    #     til_maps_server,
    #     "til_maps",
    #     features_con,
    #     cohort_feature_values_con,
    #     cohort_sample_con,
    #     cohort_group_con,
    #     cohort_group_name,
    #     cohort_colors
    # )
    # 
    # 
    # shiny::callModule(
    #     immunomodulators_server,
    #     "immunomodulators",
    #     cohort_sample_con,
    #     cohort_group_con,
    #     genes_con,
    #     cohort_group_name,
    #     cohort_colors
    # )
    # 
    shiny::callModule(
        clinical_outcomes_server,
        "clinical_outcomes",
        cohort_sample_tbl,
        cohort_group_tbl,
        cohort_group_name,
        cohort_colors
    )
    # 
    # shiny::callModule(
    #     io_targets_server,
    #     "io_targets",
    #     cohort_sample_con,
    #     cohort_group_con,
    #     genes_con,
    #     cohort_group_name,
    #     cohort_colors
    # )
    # 
    # shiny::callModule(
    #     driver_associations_server,
    #     "driver_associations",
    #     features_con,
    #     cohort_feature_values_con,
    #     features_named_list
    # )
    # 
    
    shiny::observeEvent(input$link_to_tumor_microenvironment, {
        shinydashboard::updateTabItems(session, "explorertabs", "tumor_microenvironment")
    })
    
    shiny::observeEvent(input$link_to_cohort_selection, {
        shinydashboard::updateTabItems(session, "explorertabs", "cohort_selection")
    })
    
    shiny::observeEvent(input$link_to_clinical_outcomes, {
        shinydashboard::updateTabItems(session, "explorertabs", "clinical_outcomes")
    })
    
    shiny::observeEvent(input$link_to_immunomodulators, {
        shinydashboard::updateTabItems(session, "explorertabs", "immunomodulators")
    })
    
    shiny::observeEvent(input$link_to_immune_features, {
        shinydashboard::updateTabItems(session, "explorertabs", "immune_features")
    })
    
    shiny::observeEvent(input$link_to_driver_associations, {
        shinydashboard::updateTabItems(session, "explorertabs", "driver_associations")
    })
    
    shiny::observeEvent(input$link_to_til_maps, {
        shinydashboard::updateTabItems(session, "explorertabs", "til_maps")
    })
    
    shiny::observeEvent(input$link_to_io_targets, {
        shinydashboard::updateTabItems(session, "explorertabs", "io_targets")
    })
    
    
    
    # other modules --------------------------------------------------------
    
    # shiny::callModule(
    #     data_info_server,
    #     "data_info",
    #     features_con
    # )
    
    # tool modules --------------------------------------------------------

    # shiny::callModule(
    #     immune_subtype_classifier_server,
    #     "immune_subtype_classifier")
    
    
    shiny::observeEvent(input$link_to_immune_subtype_classifier, {
        updateNavlistPanel(session, "toolstabs", "Immune Subtype Classifier")
    })

})


