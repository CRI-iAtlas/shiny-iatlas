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
                p("This displays the degree of association between driver mutation status and an immune readout, as determined by the Select Response Variable option." ),
                p("Every point in the scatter plot corresponds to a comparison of the values of that immune readout in samples in which a particular driver gene is mutated to the values in samples in which is not."),
                p("This comparison is made within each cohort among the Sample Groups. Each point thus corresponds to a single driver gene and cohort. The driver-cohort combination can be seen by hovering on a point (separated by a dot)."),
                p("The x-axis show the effect size, defined as the ratio of the mean readout value in mutated vs non-mutated samples."),
                p("The y-axis represents the P-value of the significance test comparing the readout in mutated vs non-mutated samples. A line is drawn for P=0.05, with the more significant values above that line"),
                p("Manuscript context: This allows you to display distributions such as those shown in Figure 4D.","\n"),
                p("Click on point to see a violin plot for the immune readout value distribution in mutated vs non-mutated samples for the selected cohort and driver."),
                p(""),
                p("Please Note: This is an initial stage of this module. It works best with moderate group sizes. Multiple hypothesis testing correction and incorporation of covariates will be added at a later stage.")
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
    
    scatter_plot_df <- reactive({
        df_for_plot <- 
            compute_driver_associations(
                df_for_regression(),
                response_var = input$response_variable,
                group_column = group_internal_choice(),
                group_options = get_unique_column_values(
                    group_internal_choice(), 
                    subset_df()) %>% 
                    rename(label="combo",y="neglog_pval",x="effect_size")) %>%
            rename(label="combo",y="neglog_pval",x="effect_size") 
    })
    
    # plots ----
    output$scatterPlot <- renderPlotly({
      
      
                                                 
      create_scatterplot(
          scatter_plot_df(),
          xlab = "log10(Effect Size)", 
          ylab = "- log10(P-value)", 
          title = "Immune Response Association With Driver Mutations",
          source = "scatterplot",
          key_col = "label",
          label_col = "label",
          horizontal_line = T,
          horizontal_line_y = (- log10(0.05))
      )
    })
    
    output$violinPlot <- renderPlotly({
      
      eventdata <- event_data("plotly_click", source = "scatterplot")
      
      validate(need(
        check_driver_violinplot_click_data(
          eventdata,
          df_for_regression(),
          subset_df(), 
          group_internal_choice()),
        "Click a point on the above scatterplot to see a violin plot for the comparison"))
      
      df <- df_for_regression()
      


      combo_selected <- eventdata[["key"]][[1]][1]
      dff <- df %>% filter(combo==combo_selected)
      mutation <- as.character(dff[1,"mutation"])
      
      scatter_plot_selected_row <- filter(scatter_plot_df(), label == combo_selected)
      point_selected_pval <- 10^(-scatter_plot_selected_row$y) %>% 
          round(4) %>% 
          as.character()
      point_selected_es <- scatter_plot_selected_row$x %>% 
          round(4) %>% 
          as.character()
      
      cohort <- stringr::str_replace(combo_selected,fixed(paste(c(mutation,"."),collapse="")),"")
      # cohort by string parsing above. For some reason, the following returns a number when working with TCGA Subtypes
      # cohort <- as.character(dff[1,group_internal_choice()])

      dfb <- dff %>% rename(x=value,y=input$response_variable) %>% select(x,y)
      plot_title = paste(c("Cohort:",cohort,
                           "; P-value:",point_selected_pval,
                           "; log10(Effect size):", point_selected_es),
                         collapse=" ")
      xlab = paste(c(mutation,"mutation status"),collapse=" ")
      ylab = get_variable_display_name(input$response_variable)

      create_violinplot(
          dfb,
          xlab = xlab,
          ylab = ylab,
          title = plot_title, 
          fill_colors = c("blue"),
          showlegend = FALSE)
  })
    
      
}
