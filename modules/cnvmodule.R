cnvs_UI <- function(id) {
    ns <- NS(id)

    tagList(
        titleBox("iAtlas Explorer: Association with Copy Number Variations"),
        textBox(
            width = 12,
            p("Explore statistical associations between immune readouts and copy number variations.")
        ),
        sectionBox(
            title = "Immune Response Association With Copy Number Variants",
            messageBox(
                width = 12,
                p("This module contains a large table of associations (>3 Million) between copy number variants (by gene) and immune readouts. Initially, all genes and all groups are shown. 
                Use the filter controls to limit results to your interests.\n\n"),
                
                p("Within each group (TCGA study, subtype, or immune subtype), a T-test was performed on immune readouts 
                  between samples with no copy number variation for a given gene,  
                  and samples with either amplified or deleted regions (two separate tests).\n\n"),
                
                p("There are three components to the module: filter controls, a summary plot, and a table of results.\n"),
                
                p("The filter controls remove statistics from the table and plot. It's possible to select multiple groups and genes."),
                
                p("The ridge plot shows the distribution of T statistics, given the filter settings."),
                
                tags$ul(
                  tags$li("The x-axis shows the T statistic value, positive if the normal group has higher immune readout scores."),
                  tags$li("The y-axis represents the number of genes with that statistic.")
                ),
                p("\n\nImmune landscape manuscript context: The results are comparible to those shown in Figure S4A.","\n"),
                p(""),
                p("Notes: A statistical test is performed only when the number of samples exceeds a minimum required group count (currently 3). 
                  In rare instances all (or all but one) samples within a group contain the alteration and a test cannot be performed.
                  Only statistics with p-values less than 0.001 are retained"),
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
                        uiOutput(ns("select_cn_gene_ui"))
                    ),
                    column (
                      width = 6,
                      selectInput(
                        ns("cn_dir_point_filter"),
                        "Select CNV Direction",
                        choices = c('All', 'Amp', 'Del'),
                        selected = "All"
                        
                      )
                    ),
                    #column (
                    #  width = 6,
                    #  numericInput(inputId = ns('num_genes_shown'), 
                    #               label = 'Number of Points', 
                    #               min=1, max=1000, value = 200)
                    #),
                    #column ( width = 6,
                    #      selectInput(inputId = ns('sort_genes_on'), 
                    #                  label = 'Sort results on', 
                    #                  choices = c('Mean_Diff', 'neg_log10_pvalue', 't'), selected = 't')
                    #      ),
                    column(width = 6,
                           uiOutput(ns('cnv_results_count')))
                    
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
                    plotlyOutput(ns("cnvPlot")) %>%
                        shinycssloaders::withSpinner()
                )
            ),
            
            tableBox(
                width = 12,
                div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("cnvtable")) %>%
                        shinycssloaders::withSpinner()
                )
            ) #,
            
            #fluidRow(
            #    plotBox(
            #        width = 12,
            #        plotlyOutput(ns("violinPlot")) %>%
            #            shinycssloaders::withSpinner()
            #    )
            #)
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

        res0 <- sort(unique(cnvs_df() %>%  # df_for_regression(),
            dplyr::filter(metric == input$response_variable) %>%
            dplyr::filter(group_label == group_internal_choice())  %>%
            dplyr::pull(group)))

        selectInput(
            ns("cn_group_point_filter"),
            "Select Group Filter",
            choices = c('All', res0),
            selected = "All",
            multiple = T
        )
        
    })
    
    # for the gene selection
    output$select_cn_gene_ui <- renderUI({
      
      res1 <- sort(unique(cnvs_df() %>%  # df_for_regression(),
                            dplyr::filter(metric == input$response_variable) %>%
                            dplyr::filter(group_label == group_internal_choice())  %>%
                            dplyr::pull(gene)))
      
      selectInput(
        ns("cn_gene_point_filter"),
        "Select Gene Filter",
        choices = c('All', res1),
        selected = "All",
        multiple = T
      )
    })
    

    output$cnv_results_count <- renderText({
        if(is.null(cnvs_df())){
            return("Members in current selected groupings do not have driver CNV results")
        } else {
            res1 <- sort(unique(cnvs_df() %>% 
                                dplyr::filter(metric == input$response_variable) %>%
                                dplyr::filter(group_label == group_internal_choice())  %>%
                                dplyr::pull(gene)))
          
            string <- stringr::str_c(
                "Total number of rows: ", as.character(  dim(cnvs_df())[1]  ), 
                ",  Number of genes:  ", as.character(length(res1))  
                )
            return(string)
        }
    })


    filter_df <- reactive({

        res0 <- cnvs_df() %>% dplyr::filter(metric == input$response_variable)
        
        if (input$cn_gene_point_filter != 'All') {
            res0 <- res0 %>% dplyr::filter(gene %in% input$cn_gene_point_filter)
        }
        
        if (input$cn_dir_point_filter != 'All') {
            res0 <- res0 %>% dplyr::filter(direction == input$cn_dir_point_filter)
        }
        
        if (input$cn_group_point_filter != 'All') {
            res0 <- res0 %>% dplyr::filter(group == input$cn_group_point_filter)
        }

        return(res0)
    })
    
    #### PLOT ####
    output$cnvPlot <- renderPlotly({
      
      plot_ly(x = filter_df()$t_stat, type = "histogram", name = "Histogram")
      
      #plot_ly(x = ~rnorm(50), type = "histogram")
      
      #p <- ggplot::ggplot(filter_df(), aes(x = t, y = direction)) + geom_point(shape=1),
           #ggridges::geom_density_ridges(scale = 1) + facet_wrap(~group) + 
           #xlab('T statistics') + ylab('number of genes'),

    })
    
    # Filter data based on selections
    output$cnvtable <- DT::renderDataTable(
        
      DT::datatable(
        
        filter_df() %>% 
          dplyr::select(-group_label, -label, -group_label, -pvalue) %>%
          dplyr::mutate_at(vars(Mean_Norm, Mean_CNV, t_stat, neg_log10_pvalue , Mean_Diff), dplyr::funs(round(., 3))) %>%
          dplyr::select(metric, group, gene, direction, Mean_Norm, Mean_CNV, Mean_Diff, t_stat, neg_log10_pvalue),
        
        extensions = 'Buttons', options = list(
          scrollY = '300px', 
          paging = TRUE, 
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

    ##########################################
    ##########################################  COMMENTED OUT
    ##########################################
    if (FALSE) {
      
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
}
