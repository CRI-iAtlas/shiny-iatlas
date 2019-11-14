cnvs_UI <- function(id) {
    ns <- NS(id)

    tagList(
        titleBox("iAtlas Explorer — Association with Copy Number Variations"),
        textBox(
            width = 12,
            p("This module can be used to test whether an immune readout is statistically associated with copy number variations, within each user selected sample group.")
        ),
        sectionBox(
            title = "Immune Response Association With Copy Number Variants",
            messageBox(
                width = 12,
                p("This plot displays the degree of association between copy number events in samples and an immune readout, as determined by the Select Response Variable option. Every point in the scatter plot corresponds to a comparison of the values of that immune readout in samples in which a particular gene contains variants to the values in samples in which is not. This comparison is made within each cohort among the Sample Groups. Each point thus corresponds to a single gene and cohort. The cnv-cohort combination can be seen by hovering on a point (separated by a dot)."),
                tags$ul(
                  tags$li("The x-axis shows the T statistic, defined as the ratio of the mean readout value in altered vs non-altered samples."),
                  tags$li("The y-axis represents the P-value of the significance test comparing the readout in altered vs non-altered samples. A line is drawn for P=0.05, with the more significant values above that line")
                ),
                p("Manuscript context: This allows you to display distributions such as those shown in Figure S4A.","\n"),
                p("Click on a point to see a violin plot for the immune readout value distribution in mutated vs non-mutated samples for the selected cohort and driver."),
                p(""),
                p("A statistical test is performed in a group only when the number of altered samples exceeds a minimum required count (currently 3). In rare instances all (or all but one) samples within a group contain the alteration and a test cannot be performed."),
                p(""),
                p("Please Note: This is an initial version of this module. It works best with moderate group sizes. Multiple hypothesis testing correction and incorporation of covariates will be added at a later stage."),
                p("")
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
cnvs <- function(
    input, 
    output, 
    session, 
    group_display_choice, 
    group_internal_choice,
    subset_df, 
    plot_colors) {

    ns <- session$ns

    cnvs_df <- reactive({
        req(!is.null(subset_df()), cancelOutput = T)

        build_cnvs_df(
            df = subset_df(),
            response_var = input$response_variable,
            group_column = group_internal_choice(),
            group_options = get_unique_column_values(group_internal_choice(), subset_df())
        )
        
    })

    #cnvs_group_summary_df <- reactive({
    #    req(!is.null(cnvs_df()), cancelOutput = T)
    #    build_cnvs_group_summary_df(cnvs_df())
    #})

    #testable_cnvs_groups <- reactive(get_testable_cnvs_groups(cnvs_group_summary_df()))
    #untestable_cnvs_groups <- reactive(get_untestable_cnvs_groups(cnvs_group_summary_df(), testable_cnvs_groups()))

    #output$text <- renderText({
    #    if(is.null(cnvs_df())){
    #        return("Members in current selected groupings do not have driver CNV data")
    #    } else if (length(testable_cnvs_groups()) == 0) {
    #        return("No cohort and driver combinations can be tested.")
    #    } else {
    #        string <- stringr::str_c(
    #            "Testable driver-cohort combinations: ",
    #            as.character(length(testable_cnvs_groups())),
    #            ";\t ",as.character(round(length(testable_cnvs_groups())/(length(testable_cnvs_groups())+length(untestable_cnvs_groups()))*100,1)),
    #            "% of total possible."
#                "Untestable driver-cohort combinations: ",
#                as.character(length(untestable_cnvs_groups()))
     #       )
     #       return(string)
     #   }
    #})


    scatter_plot_df <- reactive({
        print('IN SCATTER PLOT DF')
        
        print('response var')
        print(input$response_variable)
        
        print('group')
        print(group_internal_choice())
        
        #validate(need(
        #    !is.null(cnvs_df()),
        #    "No results to display, pick a different group."))
        res0 <- cnvs_df() %>%  # df_for_regression(),
            dplyr::filter(metric == input$response_variable) %>%
            dplyr::filter(group == group_internal_choice()) %>%
            dplyr::filter(pvalue < 0.05)
        
        print('FINISHED FILTER')
        
        print(dim(res0))
        print(head(res0))
        
        return(res0)
    })
    

    # plots ----
    output$scatterPlot <- renderPlotly({

        #validate(
        #    #need(!is.null(scatter_plot_df()), "No testable cohort and driver combinations, see above for explanation"))
        #    need(!is.null(scatter_plot_df()), 'empty df'))

        create_scatterplot(
            scatter_plot_df(),
            ylab = "-log10_pvalue",
            xlab = "Mean_Diff",
            x_col = 'Mean_Diff',
            y_col = 'neg_log10_pvalue',
            title = "Immune Response Association With CNVs",
            source = "scatterplot",
            key_col = "label",
            label_col = "label",
            horizontal_line = T,
            horizontal_line_y = (- log10(0.05))
        )
    })

    output$violinPlot <- renderPlotly({

        eventdata <- event_data("plotly_click", source = "scatterplot")

        ## FILTER here
        
        vdf1 <- data.frame(y=scatter_plot_df()$Mean_Norm, x='Normal')
        vdf2 <- data.frame(y=scatter_plot_df()$Mean_CNV, x='CNV')
        vdf <- rbind(vdf1, vdf2)
        
        create_violinplot(
            vdf,
            xlab = 'x',
            ylab = 'y',
            title = 'plot_title',
            fill_colors = c("blue"),
            showlegend = FALSE)
    })


}
