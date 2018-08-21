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

  # Cell content
  callModule(
      cellcontent, 
      "module1", 
      reactive(input$ss_choice), 
      reactive(subset_df()))
  # Clonal diversity
  callModule(
      immuneinterface,
      "module2",
      reactive(input$ss_choice),
      reactive(subset_df()))
  # Correlation heatmaps
  callModule(
      groupsoverview,
      "module3",
      reactive(input$ss_choice),
      reactive(subset_df()),
      reactive(width()))
  # Survival curves
  callModule(
      survival, 
      "module4", 
      reactive(input$ss_choice),
      reactive(subset_df()))
  # Immunomodulators
  callModule(
      immunomodulator, 
      "module5", 
      reactive(input$ss_choice),
      reactive(subset_df()))
  # Immune features
  callModule(
      immunefeatures, 
      "module6", 
      reactive(input$ss_choice),
      reactive(subset_df()))
  # TILmap features
  callModule(
    tilmap, 
    "module7", 
    reactive(input$ss_choice),
    reactive(subset_df()))
    #    reactive(subset_tilmap_df()))
  
    
  # Data info
  callModule(datainfo, "moduleX")
  
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
  observeEvent(input$link_to_module7, {
    shinydashboard::updateTabItems(session, "explorertabs", "tilmap_features")
  })

    
  output$study_subset_UI <- renderUI({
      if (input$ss_choice == "TCGA Subtype") {
          choices <- panimmune_data$sample_group_df %>% 
            filter(sample_group == "tcga_subtype", !is.na(FeatureValue)) %>% 
            distinct(`TCGA Studies`) %>% 
            extract2("TCGA Studies")
              
          selectInput("study_subset_selection", 
                      "Choose study subset:",
                      choices = choices,
                      selected = NULL)
      }
  })
  
  subset_df <- reactive(
      subset_panimmune_df(
          group_column = get_variable_internal_name(input$ss_choice), 
          study_option = input$study_subset_selection
      )
  )

})
################################################################################
