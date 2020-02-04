cnvs_UI <- function(id) {
    ns <- NS(id)

    tagList(
        titleBox("iAtlas Explorer: Association with Copy Number Variations"),
        textBox(
            width = 12,
            p("This module can be used to explore statistical associations between immune readouts and copy number variations.")
        ),
        sectionBox(
            title = "Immune Response Association With Copy Number Variants",
            messageBox(
                width = 12,
                p("This module allows exploration of CNV association with immune readouts. Initially, all genes and all groups are shown, use
                  the filter controls to limit the number of points shown. This also limits the table and violin plots below."),
                p("Within each group, a T-test was performed between samples with no copy number variation, and samples with either amplified 
                  or deleted regions (two separate tests for amplified genes (Amp) and deleted genes (Del)."),
                p("There are three components to the module: a volcano plot, a table of results, and a violin plot."),
                p("The violin plot displays the degree of association between copy number events in samples and the immune readout, 
                  as determined by the Select Response Variable option. Every point in the scatter plot corresponds to a 
                  comparison of immune readout values in normal samples compared to altered samples. A separate comparison is made within 
                  each cohort among the Sample Groups. Each point thus corresponds to the T-test of a single gene within a cohort. 
                  The cnv-cohort combination can be seen by hovering on a point (separated by a dot)."),
                tags$ul(
                  tags$li("The x-axis shows the T statistic, defined as the ratio of the mean readout value in altered vs non-altered samples."),
                  tags$li("The y-axis represents the Log10 p-value of the significance test comparing the readout in altered vs non-altered samples. 
                          A line is drawn for P=0.05, with the more significant values above that line")
                ),
                p("Immune landscape manuscript context: This allows you to display distributions such as those shown in Figure S4A.","\n"),
                p("Click on a point to see a violin plot for the immune readout value distribution in 
                  mutated vs non-mutated samples for the selected cohort and driver."),
                p(""),
                p("A statistical test is performed in a group only when the number of altered samples exceeds a minimum required count (currently 3). 
                  In rare instances all (or all but one) samples within a group contain the alteration and a test cannot be performed."),
                p(""),
                p("")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 6,
                        selectInput(
                            ns("response_variable"),
                            "Select Response Variable",
                            choices = get_feature_df_nested_list(),
                            selected = "Leukocyte Fraction"

                        )
                    ),
                    column (
                        width = 6,
                        uiOutput(ns("select_cn_group_ui"))
                    ),
                    column (
                        width = 6,
                        selectInput(
                            ns("cn_gene_point_filter"),
                            "Select Gene Filter",
                            choices = c('All', get_cn_feature_df_nested_list('gene')),
                            selected = "All"
                        )
                    ),
                    column (
                        width = 6,
                        selectInput(
                            ns("cn_dir_point_filter"),
                            "Select CNV Direction",
                            choices = c('All', 'Amp', 'Del'),
                            selected = "All"
                            
                        )
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
            
            tableBox(
                width = 12,
                div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("cnvtable")) %>%
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
    
    # for the group selection
    output$select_cn_group_ui <- renderUI({

        res0 <- cnvs_df() %>%  # df_for_regression(),
            dplyr::filter(metric == input$response_variable) %>%
            dplyr::filter(sample_group == group_internal_choice())  %>%
            dplyr::pull(group)
        
        selectInput(
            ns("cn_group_point_filter"),
            "Select Group Filter",
            choices = c('All', res0),
            selected = "All"
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
        
        print('group display choice')
        print(group_display_choice())
        
        #validate(need(
        #    !is.null(cnvs_df()),
        #    "No results to display, pick a different group."))
        res0 <- cnvs_df() %>%  # df_for_regression(),
            dplyr::filter(metric == input$response_variable) %>%
            dplyr::filter(sample_group == group_internal_choice()) 
            
        if (input$cn_gene_point_filter != 'All') {
            res0 <- res0 %>% dplyr::filter(gene == input$cn_gene_point_filter)
        }
        
        if (input$cn_dir_point_filter != 'All') {
            res0 <- res0 %>% dplyr::filter(direction == input$cn_dir_point_filter)
        }
        
        if (input$cn_group_point_filter != 'All') {
            res0 <- res0 %>% dplyr::filter(group == input$cn_group_point_filter)
        }
        
        
        
        #dplyr::filter(pvalue < 0.05)  # prefiltered at p-value > 0.1
        
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
        
        create_scatterplot_cnv(
            scatter_plot_df(),
            ylab = "-log10 p-value",
            xlab = "Difference in Means",
            x_col = 'Mean_Diff',
            y_col = 'neg_log10_pvalue',
            title = "",
            source = "scatterplot",
            key_col = "label",
            color_col = 'direction',
            label_col = "label",
            fill_color = 'direction', # 
            fill_colors = c('#027AB0', '#AE3918'),
            horizontal_line = T,
            horizontal_line_y = (- log10(0.05))
        )
    })
    

    # Filter data based on selections
    output$cnvtable <- DT::renderDataTable(
        
        DT::datatable(
            scatter_plot_df() %>% 
                dplyr::select(-sample_group, -label, -group_label, -pvalue) %>%
                dplyr::mutate_at(vars(Mean_Norm, Mean_CNV, t, neg_log10_pvalue , Mean_Diff), dplyr::funs(round(., 3))) %>%
                dplyr::select(metric, group, gene, direction, Mean_Norm, Mean_CNV, Mean_Diff, t, neg_log10_pvalue),
            extensions = 'Buttons', options = list(
                scrollY = '300px', 
                paging = FALSE, 
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons =
                    list('copy', 'print',
                         list(
                             extend = 'collection',
                             buttons = c('csv', 'excel', 'pdf'),
                             text = 'Download')
                    )
            )
            
        )
    )

    output$violinPlot <- renderPlotly({

        eventdata <- event_data("plotly_click", source = "scatterplot")

        print('EVENTDATA')
        print(eventdata)
        
        if(is.null(eventdata)) {
            
            eventdata <- list(key='All')
            violin_df <- scatter_plot_df()
            
        } else {
            
            violin_df <- scatter_plot_df() %>% filter(label == eventdata$key)
            
        }
                
        vdf1 <- data.frame(y=violin_df$Mean_Norm, x='Normal')
        
        vdf2 <- data.frame(y=violin_df$Mean_CNV, 
                           x=ifelse(input$cn_dir_point_filter != 'All', 
                                    yes=input$cn_dir_point_filter, 
                                    no='Copy Number Altered'))
        vdf <- rbind(vdf1, vdf2)
        
        create_violinplot(
            vdf,
            xlab = '',
            ylab = paste0('mean ', input$response_variable),
            title = ifelse(input$cn_gene_point_filter == 'All', 
                           yes=ifelse(input$cn_group_point_filter == 'All', 
                                      yes = paste0('All Genes, ', group_display_choice() ),       # general group all genes
                                      no  = paste0('All Genes, ', input$cn_group_point_filter)),  # all genes, specific group
                           no =ifelse(input$cn_group_point_filter == 'All',
                                      yes = paste0(input$cn_gene_point_filter, ', ', group_display_choice() ),  # selected gene, general groups
                                      no  = paste0(input$cn_gene_point_filter, ', ', input$cn_group_point_filter)) 
                           ),  
            fill_colors = c("blue"),
            showlegend = FALSE)
    })


}
