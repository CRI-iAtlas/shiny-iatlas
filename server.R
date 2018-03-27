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
  callModule(featurecorrelation, "module3", reactive(input$ss_choice))
  # Survival curves
  callModule(survival, "module4", reactive(input$ss_choice))
  # immunomodulators
  callModule(immunomodulator, "module5", reactive(input$ss_choice))
  
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
    shinydashboard::updateTabItems(session, "explorertabs", "feature_correlations")
  })
  observeEvent(input$link_to_module4, {
    shinydashboard::updateTabItems(session, "explorertabs", "survival_curves")
  })
  observeEvent(input$link_to_module5, {
    shinydashboard::updateTabItems(session, "explorertabs", "immunomodulators")
  })
  
  output$study_subset_UI <- renderUI({
      choices <- panimmune_data$df %>%
          extract2(get_variable_internal_name(input$ss_choice)) %>% 
          unique %>% 
          sort
      checkboxGroupInput("study_subset_selections", 
                         "Choose subset:",
                         choices = choices,
                         selected = choices)
  })
  
  subset_df <- reactive(subset_panimmune_df(
      get_variable_internal_name(input$ss_choice), 
      input$study_subset_selections))
  

})
################################################################################
