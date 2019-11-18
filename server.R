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
    
    # Cell content
    callModule(
        cellcontent,
        "module1",
        group_display_choice,
        subset_feature_values_long_con,
        features_con,
        subset_groups_con
    )
    
    user_group_tbl <- callModule(
        groupsoverview,
        "module3",
        groups_con,
        subset_feature_values_long_con,
        groups_list,
        tcga_subtypes_list,
        reactive(input$group_internal_choice),
        group_values_con,
        subtypes,
        plot_colors
    )

    # Survival curves
    callModule(
        survival,
        "module4",
        features_named_list,
        group_display_choice,
        reactive(input$group_internal_choice),
        subset_feature_values_long_con,
        groups_list,
        subset_groups_con,
        features_con,
        plot_colors
    )
    

    # Immune features
    callModule(
        immunefeatures,
        "module6",
        group_display_choice,
        subset_groups_con,
        subset_feature_values_long_con,
        features_con, 
        plot_colors
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
    # # Driver associations
    # callModule(
    #     drivers,
    #     "module8",
    #     subset_driver_results_con,
    #     subset_driver_mutations_con,
    #     subset_feature_values_long_con,
    #     features_named_list
    # )
    # 
    # # IO Target
    # callModule(
    #     iotarget,
    #     "module9",
    #     group_display_choice,
    #     subset_groups_con,
    #     subset_io_target_expr_con,
    #     io_targets_con,
    #     plot_colors
    # )
    # 
    # # Immunomodulators
    # callModule(
    #     immunomodulator,
    #     "module5",
    #     group_display_choice,
    #     subset_groups_con,
    #     subset_immunomodulator_expr_con,
    #     immunomodulators_con,
    #     plot_colors
    # )
    
    # subtype predictor
    callModule(
        subtypeclassifier, 
        "module_subtypeclassifier")

    # Data info
    callModule(
        datainfo,
        "moduleX",
        features_con,
        features_named_list
    )
    
    output$group_choice <- renderText({
        group_display_choice()
    })

    observeEvent(input$link_to_module1, {
        shinydashboard::updateTabItems(session, "explorertabs", "cell_content")
    })
    observeEvent(input$link_to_module2, {
        shinydashboard::updateTabItems(session, "explorertabs", "clonal_diversity")
    })
    observeEvent(input$link_to_module3, {
        shinydashboard::updateTabItems(session, "explorertabs", "groups_overview")
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
    
    # create db connections ---------------------------------------------------
    
    con_function <- reactive({
        if(DATAMODE == "SQLite_db"){
            con_function <- purrr::partial(
                dplyr::tbl,
                src = PANIMMUNE_DB
            )
        } else {
            stop("DATAMODE value not currently supported")
        }
    })
    
    groups_con               <- reactive(con_function()("groups"))
    features_con             <- reactive(con_function()("features"))
    feature_values_long_con  <- reactive(con_function()("feature_values_long"))
    driver_mutations_con     <- reactive(con_function()("driver_mutations"))
    driver_results_con       <- reactive(con_function()("driver_results"))
    immunomodulator_expr_con <- reactive(con_function()("immunomodulator_expr"))
    io_target_expr_con       <- reactive(con_function()("io_target_expr"))
    immunomodulators_con     <- reactive(con_function()("immunomodulators"))
    io_targets_con           <- reactive(con_function()("io_targets"))
    til_image_links_con      <- reactive(con_function()("til_image_links"))
    
    # group selection ---------------------------------------------------------
    
    groups_list <- reactive({
        req(groups_con(), user_group_tbl())
        lst <- create_named_list(
            groups_con(), 
            "parent_group_display", 
            "parent_group"
        )
        if(is.data.frame(user_group_tbl())){
            lst <- user_group_tbl() %>% 
                dplyr::select(-sample) %>% 
                colnames() %>% 
                purrr::set_names(., .) %>% 
                c(lst, .)

        }
        return(lst)
    })
    
    tcga_subtypes_list <- reactive({
        req(groups_con())
        create_named_list(
            groups_con(), 
            "subtype_group_display", 
            "subtype_group"
        )
    })
    
    output$select_group_UI <- renderUI({
        req(groups_list())
        selectInput(
            inputId = "group_internal_choice",
            label = strong("Select Sample Groups"),
            choices = groups_list(),
            selected = "Immune Subtype"
        )
    })
    
    group_display_choice <- reactive({
        req(input$group_internal_choice) 
        stringr::str_replace_all(input$group_internal_choice, "_", " ")
    })
    
    output$study_subset_UI <- renderUI({
        req(input$group_internal_choice)
        if (input$group_internal_choice == "TCGA_Subtype") {
            req(tcga_subtypes_list())
            selectInput(
                "tcga_subset_choice",
                "Choose study subset:",
                choices = tcga_subtypes_list()
            )
        }
    })
    
    subtypes <- reactive({
        req(input$group_internal_choice)
        if(input$group_internal_choice == "TCGA_Subtype"){
            req(input$tcga_subset_choice, groups_con())
            subtypes <- groups_con() %>%  
                dplyr::filter(
                    subtype_group == local(input$tcga_subset_choice)
                ) %>% 
                dplyr::pull(group)
        } else {
            subtypes <- "none"
        } 
        return(subtypes)
    })
    
    # connections subset by selected group ------------------------------------
    # This includes using the current user group, but is currently disabled
    
    subset_feature_values_long_con <- reactive({
        req(feature_values_long_con(), input$group_internal_choice, subtypes())
        subset_long_con_with_group(
            feature_values_long_con(),
            user_group_tbl(),
            input$group_internal_choice, 
            group_values = subtypes()
        )
    })
    
    subset_immunomodulator_expr_con <- reactive({
        req(immunomodulator_expr_con(), input$group_internal_choice, subtypes())
        subset_long_con_with_group(
            immunomodulator_expr_con(),
            user_group_tbl(),
            input$group_internal_choice, 
            group_values = subtypes(),
            feature_col = "gene"
        )
    })
    
    subset_io_target_expr_con <- reactive({
        req(io_target_expr_con(), input$group_internal_choice, subtypes())
        subset_long_con_with_group(
            io_target_expr_con(),
            user_group_tbl(),
            input$group_internal_choice, 
            group_values = subtypes(),
            feature_col = "gene"
        )
    })
    
    subset_driver_mutations_con <- reactive({
        req(driver_mutations_con(), input$group_internal_choice, subtypes())
        subset_long_con_with_group(
            driver_mutations_con(),
            user_group_tbl(),
            input$group_internal_choice,
            group_values = subtypes(),
            feature_col = "gene",
            value_col = "status"
        )
    })
    
    subset_driver_results_con <- reactive({
        req(driver_results_con(), input$group_internal_choice)
        con <- driver_results_con() %>%
            dplyr::filter(parent_group == local(input$group_internal_choice))
        if(input$group_internal_choice == "TCGA_Subtype"){
            req(subtypes())
            con <- dplyr::filter(con, group %in% local(subtypes()))
        }
        return(con)
    })
    
    subset_groups_con <- reactive({
        req(groups_con(), input$group_internal_choice)
        con <- groups_con() %>% 
            dplyr::filter(parent_group == local(input$group_internal_choice))
        if(input$group_internal_choice == "TCGA_Subtype"){
            req(subtypes())
            con <- dplyr::filter(con, group %in% local(subtypes()))
        } 
        return(con)
    })
    
    # other connections -------------------------------------------------------
    
    group_values_con <- reactive({
        req(feature_values_long_con(), groups_list())
        feature_values_long_con() %>% 
            dplyr::select(c("sample", local(groups_list()))) %>% 
            dplyr::distinct()
    })
    
    # other -------------------------------------------------------------------
    
    plot_colors <- reactive({
        req(subset_groups_con())
        create_named_list(subset_groups_con(), "group", "color") 
    })
    
    features_named_list <- reactive({
        req(features_con())
        create_nested_named_list(features_con())
    })
})


