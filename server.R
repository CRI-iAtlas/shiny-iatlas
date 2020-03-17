################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100 * 1024^2)
library(Cairo)
options(shiny.usecairo = T)

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

  
    # Cell content
    callModule(
        cellcontent,
        "module1",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(sample_group_df()),
        reactive(subset_df()))
    
    # Clonal diversity
    # callModule(
    #     immuneinterface,
    #     "module2",
    #     reactive(input$ss_choice),
    #     reactive(group_internal_choice()),
    #     reactive(subset_df()),
    #     reactive(plot_colors()))
    
    # Groups
    user_group_df <- callModule(
        groupsoverview,
        "module3",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(sample_group_df()),
        reactive(subset_df()),
        reactive(plot_colors()),
        reactive(group_options()),
        reactive(width()))
    
    # Survival curves
    callModule(
        survival,
        "module4",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(group_options()),
        reactive(sample_group_df()),
        reactive(subset_df()),
        reactive(plot_colors()))
    
    # Immunomodulators
    callModule(
        immunomodulator,
        "module5",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(sample_group_df()),
        reactive(subset_df()),
        reactive(plot_colors()))
    
    # Immune features
    callModule(
        immunefeatures,
        "module6",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(sample_group_df()),
        reactive(subset_df()),
        reactive(plot_colors()))
    
    # TILmap features
    callModule(
        tilmap,
        "module7",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(sample_group_df()),
        reactive(subset_df()),
        reactive(plot_colors()))
    
    # Driver associations
    callModule(
        drivers,
        "module8",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(subset_df()),
        reactive(plot_colors()))
    
    # IO Target
    callModule(
        iotarget,
        "module9",
        reactive(input$ss_choice),
        reactive(group_internal_choice()),
        reactive(sample_group_df()),
        reactive(subset_df()),
        reactive(plot_colors()))
    
    # CNV associations
    callModule(
      cnvs,
      "module10",
      reactive(input$ss_choice),
      reactive(group_internal_choice()),
      reactive(input$study_subset_selection),
      reactive(subset_df()),
      reactive(plot_colors()))
    
    # Cytokine Network
    callModule(
      cytokinenetwork,
      "module11",
      reactive(input$ss_choice),
      reactive(group_internal_choice()),
      reactive(input$study_subset_selection),
      reactive(sample_group_df()),
      reactive(subset_df()),
      reactive(plot_colors()))
    
    # subtype predictor
    callModule(
        subtypeclassifier, 
        "module_subtypeclassifier")

    # Data info
    callModule(
        datainfo,
        "moduleX")
    
    # Cell Image
    callModule(
      cellimage,
      "module12",
      reactive(input$ss_choice),
      reactive(group_internal_choice()),
      reactive(input$study_subset_selection),
      reactive(subset_df()),
      reactive(plot_colors()))
    
    output$ss_choice <- renderText({
        input$ss_choice
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
    observeEvent(input$link_to_module10, {
      shinydashboard::updateTabItems(session, "explorertabs", "cnvs")
    })
    observeEvent(input$link_to_module11, {
      shinydashboard::updateTabItems(session, "explorertabs", "cytokine_network")
    })
    observeEvent(input$link_to_module12, {
      shinydashboard::updateTabItems(session, "explorertabs", "cell_image")
    })
    observeEvent(input$link_to_module_subtypeclassifier, {
        updateNavlistPanel(session, "toolstabs", "Immune Subtype Classifier")
    })

    # group selection ui --------------------------------------------------------

    group_options <- reactive({
        groups <-  c("Immune Subtype", "TCGA Study", "TCGA Subtype")
        user_groups <- try(colnames(user_group_df()))
        if(is.vector(user_groups)) groups <- c(groups, user_groups[-1])
        return(groups)
    })

    output$select_group_UI <- renderUI({

        selectInput(
            inputId = "ss_choice",
            label = strong("Select Sample Groups"),
            choices = as.character(
                group_options()
            ),
            selected = "Immune Subtype"
        )
    })

    output$study_subset_UI <- renderUI({
        req(input$ss_choice, panimmune_data$sample_group_df, cancelOutput = TRUE)
        if (input$ss_choice == "TCGA Subtype") {

              choices <- panimmune_data$sample_group_df %>%
                dplyr::filter(sample_group == "Subtype_Curated_Malta_Noushmehr_et_al") %>%
                dplyr::select("FeatureDisplayName", "TCGA Studies") %>%
                dplyr::distinct() %>%
                dplyr::arrange(`TCGA Studies`) %>%
                tibble::deframe()

            selectInput("study_subset_selection",
                        "Choose study subset:",
                        choices = choices,
                        selected = names(choices[1]))
        }
    })

    group_internal_choice <- reactive({
        req(input$ss_choice, panimmune_data$feature_df, cancelOutput = T)
        get_group_internal_name(input$ss_choice)
    })

    sample_group_df <- reactive({

        req(
            panimmune_data$sample_group_df,
            group_internal_choice(),
            !is.null(user_group_df()),
            cancelOutput = T)

        sample_group_df <- subset_sample_group_df(
            group_internal_choice(),
            input$study_subset_selection,
            user_group_df())

        return(sample_group_df)
    })

    subset_df <- reactive({
        req(
            group_internal_choice(),
            sample_group_df(),
            !is.null(user_group_df()),
            cancelOutput = T)

        subset_df <- subset_panimmune_df(
            group_column = group_internal_choice(),
            user_group_df = user_group_df(),
            sample_group_df = sample_group_df()
        )
    })

    plot_colors <- reactive({
        req(
            group_internal_choice(),
            sample_group_df(),
            cancelOutput = T)

        plot_colors <- sample_group_df() %>%
            dplyr::filter(sample_group == group_internal_choice()) %>%
            dplyr::select(FeatureValue, FeatureHex) %>%
            tibble::deframe()
    })
    


})
###############################################################################

