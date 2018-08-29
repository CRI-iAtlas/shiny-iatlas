drivers_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Association with Driver Mutations"),
        textBox(
            width = 12,
            p("This module allows you to see how immune readouts associate with driver mutations.")  
        ),
        sectionBox(
            title = "Immune Response Association With Driver Mutations",
            messageBox(
                width = 12,
                p("This display values for the degree of association between driver mutation status and an immune readout."),
                p("Manuscript context: This allows you to display distributions such as those shown in Figures 4D.")
            ),
            fluidRow(
                optionsBox(
                    width = 6,
                    selectInput(
                        ns("response_variable"),
                        "Select Response Variable",
                        choices = get_feature_df_nested_list(),
                        selected = "Leukocyte Fraction"
                        
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("scatterPlot")) %>% 
                        shinycssloaders::withSpinner()
                )
            ),
            fluidRow(
              plotBox(
                width = 12,
                plotlyOutput(ns("violinPlot")) %>%
                  shinycssloaders::withSpinner()
              )
            )
        )
    )
}

# Server ----
drivers <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    ns <- session$ns
  
    df_for_regression <- reactive({
      build_df_for_driver_regression(
      df=subset_df(),
      response_var = input$response_variable,
      group_column = group_internal_choice(),
      group_options = get_unique_column_values(group_internal_choice(), subset_df()))
    
    })
    
    # plots ----
    output$scatterPlot <- renderPlotly({
      
      df_for_plot <- compute_driver_associations(df_for_regression(),
                                                 response_var = input$response_variable,
                                                 group_column = group_internal_choice(),
                                                 group_options = get_unique_column_values(group_internal_choice(), subset_df()) %>%
                                                 rename(label="combo",y="neglog_pval",x="effect_size")
      ) %>% rename(label="combo",y="neglog_pval",x="effect_size")
                                                 
      create_scatterplot(df_for_plot,
                         xlab = "Effect Size", 
                         ylab = "- log10(P-value)", 
                         title = "Immune Response Association With Driver Mutations",
                         source = "scatterplot"
      )
    })
    
    output$violinPlot <- renderPlotly({
      
      eventdata <- event_data("plotly_click", source = "scatterplot")
      
# from immuneinterfacemodule.R     - need to introduce validation here  
#      validate(need(
#        check_immunefeatures_scatterplot_click_data(
#          eventdata, 
#          subset_df(), 
#          group_internal_choice(), 
#          intermediate_corr_df()),
#        "Click above heatmap"))
      
      combo_selected <- eventdata[["key"]][[1]][1]
      cat (combo_selected,"\n")
      dff <- df_for_regression() %>% filter(combo==combo_selected)
      dfb <- dff %>% rename(x=value,y=input$response_variable) %>% select(x,y)

      plot_title = paste(c("Cohort",df[1,group_internal_choice()]),collapse=" ")
      xlab = paste(c(df[1,"mutation"],"mutation status"),collapse=" ")
      ylab = get_variable_display_name(input$response_variable)
    
      create_violinplot(dfb,xlab=xlab,ylab=ylab,title=plot_title,fill_colors=c("blue"))
  })
    
      
}
