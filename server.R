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
    
    source("modules/server/analysis_modules/clinical_outcomes_server.R", local = T)
    source("modules/server/analysis_modules/cohort_selection_server.R", local = T)
    source("modules/server/analysis_modules/data_info_server.R", local = T)
    source("modules/server/analysis_modules/driver_associations_server.R", local = T)  
    source("modules/server/analysis_modules/immune_features_server.R", local = T)
    source("modules/server/analysis_modules/immunomodulators_server.R", local = T)
    source("modules/server/analysis_modules/io_targets_server.R", local = T)
    source("modules/server/analysis_modules/immune_subtype_classifier_server.R", local = T)
    source("modules/server/analysis_modules/til_maps_server.R", local = T)
    source("modules/server/analysis_modules/tumor_microenvironment_server.R", local = T)
    
    tags_con <- reactive({
        create_connection("tags") %>% 
            dplyr::select(
                parent_group_id = id, 
                parent_group_name = name, 
                parent_group_display = display
            ) %>% 
            dplyr::filter( parent_group_display %in% c(
                    "Immune Subtype", 
                    "TCGA Study", 
                    "TCGA Subtype"
            )) %>% 
            dplyr::inner_join(
                create_connection("tags_to_tags"),
                by = c("parent_group_id" = "related_tag_id")
            ) %>% 
            dplyr::inner_join(
                create_connection("tags"),
                by = c("tag_id" = "id")
            ) %>% 
            dplyr::select(parent_group, group = name, tag_id) %>% 
            dplyr::compute()
    })
    
    features_con <- reactive({
        con <- 
            dplyr::left_join(
                create_connection("features"),
                create_connection("classes"),
                by = c("class_id" = "id")
            ) %>% 
            dplyr::left_join(
                create_connection("method_tags"),
                by = c("method_tag_id" = "id")
            ) %>% 
            dplyr::select(
                class_name = name.y, 
                class_id, 
                feature_name = display, 
                feature_internal_name = name.x, 
                feature_id = id,
                method_tag = name,
                method_tag_id,
                order,
                unit
            ) %>% 
            dplyr::compute() 
    })
    
    genes_con <- reactive({
        create_connection("genes") %>% 
            dplyr::left_join(
                create_connection("gene_families"), 
                by = c("gene_family_id" = "id")
            ) %>% 
            dplyr::rename(gene_family = name) %>% 
            dplyr::left_join(
                create_connection("gene_functions"), 
                by = c("gene_function_id" = "id")
            ) %>% 
            dplyr::rename(gene_function = name) %>% 
            dplyr::left_join(
                create_connection("immune_checkpoints"), 
                by = c("immune_checkpoint_id" = "id")
            ) %>% 
            dplyr::rename(immune_checkpoint = name) %>%
            dplyr::left_join(
                create_connection("pathways"), 
                by = c("pathway_id" = "id")
            ) %>% 
            dplyr::rename(pathway = name) %>% 
            dplyr::left_join(
                create_connection("super_categories"), 
                by = c("super_cat_id" = "id")
            ) %>% 
            dplyr::rename(super_category = name) %>% 
            dplyr::left_join(
                create_connection("therapy_types"), 
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
    
    features_named_list <- reactive({
        req(features_con()) 
        features_con() %>% 
            dplyr::select(
                class = class_name, 
                display = feature_name, 
                feature = feature_id
            ) %>% 
            create_nested_named_list() 
    })
    
    cohort_cons <- callModule(
        cohort_selection_server,
        "cohort_selection"
    )
    
    cohort_sample_con <- reactive(cohort_cons()$sample_con) 
    cohort_group_con  <- reactive(cohort_cons()$group_con)
    cohort_group_name <- reactive(cohort_cons()$group_name)
    cohort_colors     <- reactive(cohort_cons()$plot_colors) 
    cohort_dataset    <- reactive(cohort_cons()$dataset) 
    cohort_groups     <- reactive(cohort_cons()$groups) 
    
    cohort_feature_values_con <- reactive({
        req(cohort_sample_con())
        con <- 
            create_connection("features_to_samples") %>% 
            dplyr::inner_join(cohort_sample_con(), by = "sample_id") %>% 
            dplyr::select(sample_id, feature_id, value, sample_name = name, group) %>% 
            dplyr::compute() 
    })
    
    callModule(
        tumor_microenvironment_server,
        "tumor_microenvironment",
        cohort_feature_values_con,
        features_con,
        cohort_group_con
    )
    
    callModule(
        immune_features_server,
        "immune_features",
        cohort_feature_values_con,
        features_con,
        cohort_group_con,
        cohort_group_name,
        cohort_colors
    )
    
    callModule(
        til_maps_server,
        "til_maps",
        features_con,
        cohort_feature_values_con,
        cohort_sample_con,
        cohort_group_con,
        cohort_group_name,
        cohort_colors
    )
    
    callModule(
        data_info_server,
        "data_info",
        features_con
    )

    callModule(
        immunomodulators_server,
        "immunomodulators",
        cohort_sample_con,
        cohort_group_con,
        genes_con,
        cohort_group_name,
        cohort_colors
    )

    callModule(
        clinical_outcomes_server,
        "clinical_outcomes",
        cohort_feature_values_con,
        features_con,
        cohort_group_con,
        cohort_group_name,
        cohort_colors
    )
    
    callModule(
        io_targets_server,
        "io_targets",
        cohort_sample_con,
        cohort_group_con,
        genes_con,
        cohort_group_name,
        cohort_colors
    )
    
    callModule(
        driver_associations_server,
        "driver_associations",
        features_con,
        cohort_feature_values_con,
        features_named_list
    )

    callModule(
        immune_subtype_classifier_server, 
        "immune_subtype_classifier")

    observeEvent(input$link_to_tumor_microenvironment, {
        shinydashboard::updateTabItems(session, "explorertabs", "tumor_microenvironment")
    })
    observeEvent(input$link_to_cohort_selection, {
        shinydashboard::updateTabItems(session, "explorertabs", "cohort_selection")
    })
    observeEvent(input$link_to_clinical_outcomes, {
        shinydashboard::updateTabItems(session, "explorertabs", "clinical_outcomes")
    })
    observeEvent(input$link_to_immunomodulators, {
        shinydashboard::updateTabItems(session, "explorertabs", "immunomodulators")
    })
    observeEvent(input$link_to_immune_features, {
        shinydashboard::updateTabItems(session, "explorertabs", "immune_features")
    })
    observeEvent(input$link_to_driver_associations, {
        shinydashboard::updateTabItems(session, "explorertabs", "driver_associations")
    })
    observeEvent(input$link_to_til_maps, {
        shinydashboard::updateTabItems(session, "explorertabs", "til_maps")
    })
    observeEvent(input$link_to_io_targets, {
      shinydashboard::updateTabItems(session, "explorertabs", "io_targets")
    })
    observeEvent(input$link_to_immune_subtype_classifier, {
        updateNavlistPanel(session, "toolstabs", "Immune Subtype Classifier")
    })

})


