drivers_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Association with Driver Mutations"),
        textBox(
            width = 12,
            p(
                "This module can be used to test whether an immune readout is statistically associated with ",
                ncol(panimmune_data$driver_mutation_df) - 1,
                " cancer driver mutations, within each of your sample groups."
            )  
        ),
        sectionBox(
            title = "Immune Response Association With Driver Mutations",
            messageBox(
                width = 12,
                includeMarkdown("data/markdown/driver_single.markdown")
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
                messageBox(
                    width = 12,
                    p(textOutput(ns("text")))
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
    
    mutation_df <- reactive({
        req(!is.null(subset_df()), cancelOutput = T)
        
        build_mutation_df(
            df = subset_df(),
            response_var = input$response_variable,
            group_column = group_internal_choice(),
            group_options = get_unique_column_values(group_internal_choice(), subset_df())
        )
    })
        
    
    mutation_group_summary_df <- reactive({
        req(!is.null(mutation_df()), cancelOutput = T)
        build_mutation_group_summary_df(mutation_df())
    })
    
    testable_mutation_groups <- reactive(get_testable_mutation_groups(mutation_group_summary_df()))
    untestable_mutation_groups <- reactive(get_untestable_mutation_groups(mutation_group_summary_df(), testable_mutation_groups()))
    
    output$text <- renderText({
        if(is.null(mutation_df())){
            return("Members in current selected groupings do not have driver mutation data")
        } else if (length(testable_mutation_groups()) == 0) {
            return("No cohort and driver combinations can be tested.")
        } else {
            string <- stringr::str_c(
                "Testable driver-cohort combinations: ", 
                as.character(length(testable_mutation_groups())),
                ";\t ",as.character(round(length(testable_mutation_groups())/(length(testable_mutation_groups())+length(untestable_mutation_groups()))*100,1)),
                "% of total possible."
#                "Untestable driver-cohort combinations: ", 
#                as.character(length(untestable_mutation_groups()))
            )
            return(string)
        }
    })
    
    
    df_for_regression <- reactive({
        if(is.null(mutation_df())){
            return(NULL)
        }
        filter(mutation_df(), mutation_group %in% testable_mutation_groups())
    })
    
    scatter_plot_df <- reactive({
        validate(need(
            !is.null(df_for_regression()),
            "No results to display, pick a different group."))
        df_for_plot <- 
            compute_driver_associations(
                df_for_regression(),
                response_var = input$response_variable,
                group_column = group_internal_choice(),
                group_options = get_unique_column_values(
                    group_internal_choice(), 
                    subset_df())) %>% 
            rename(label="mutation_group",y="neglog_pval",x="effect_size") 
    })
    
    # plots ----
    output$scatterPlot <- renderPlotly({
        
        validate(
            need(!is.null(scatter_plot_df()), "No testable cohort and driver combinations, see above for explanation"))
        
        
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
            check_driver_violinplot_click_data(eventdata, df_for_regression()),
            "Click a point on the above scatterplot to see a violin plot for the comparison"))
        
        mutation_group_selected <- eventdata[["key"]][[1]][1]
        
        df <- 
            df_for_regression() %>% 
            dplyr::filter(mutation_group == mutation_group_selected)
        
        
        mutation <- as.character(df[1,"mutation"])
        
        scatter_plot_selected_row <- filter(scatter_plot_df(), label == mutation_group_selected)
        point_selected_pval <- 10^(-scatter_plot_selected_row$y) %>% 
            round(4) %>% 
            as.character()
        point_selected_es <- scatter_plot_selected_row$x %>% 
            round(4) %>% 
            as.character()
        
        cohort <- stringr::str_replace(
            mutation_group_selected,
            stringr::fixed(paste(c(mutation,"."),collapse="")),
            "")
        # cohort by string parsing above. For some reason, the following returns a number when working with TCGA Subtypes
        # cohort <- as.character(dff[1,group_internal_choice()])
        
        dfb <- df %>% rename(x=value,y=input$response_variable) %>% dplyr::select(x,y)
        
        
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
