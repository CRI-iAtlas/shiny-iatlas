drivers_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Association with Driver Mutations"),
        textBox(
            width = 12,
            p("This module allows you to see how immune readouts associate with driver mutations.")  
        ),
        sectionBox(
            title = "Volcano Plot",
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
            )
        )
    )
}

# Server ----
drivers <- function(
    input, output, session, group_display_choice, group_internal_choice, 
    subset_df, plot_colors) {
    
    ns <- session$ns
    
#    driver_associations_df <- reactive({
#      sample_groups <- get_unique_column_values(
#        group_internal_choice(), 
#        subset_df())
#      df <- 
#        compute_driver_associations(
#            subset_df(),
#            group_column = group_internal_choice(),
#            value1_column = input$response_variable,
#            group_options = sample_groups) 
#    })
    
    # plots ----
    output$scatterPlot <- renderPlotly({
      
      df_for_plot <-  compute_driver_associations(
        fmx_df=subset_df(),
        response_var = input$response_variable,
        group_column = group_internal_choice(),
        group_options = get_unique_column_values(group_internal_choice(), subset_df())
      ) %>%
      rename(label="combo",y="neglog_pval",x="effect_size")

      create_scatterplot(df_for_plot,
                         xlab = "Effect Size", 
                         ylab = "- Log10(P-value)", 
                         title = ""
      )
      
    })

}
