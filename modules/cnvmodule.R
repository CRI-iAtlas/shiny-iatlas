cnvs_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Association with Copy Number Variations"),
    textBox(
      width = 12,
      p("Explore statistical associations between immune readouts and copy number variations.")
    ),
    sectionBox(
      title = "Immune Response Association With Copy Number Variation",
      messageBox(
        width = 12,
        p("This module allows you to identify associations between specific somatic gene copy number alterations (SCNAs) and immune readouts. Gene SCNAs are specified as either amplified or deleted.\n\n"),

        p("The module begins with a large table of SCNA statistical associations (>3 Million) based on the t-test. Initially, statistics from all genes and all sample groups are shown. 
                Use the filter controls to limit results to your interests.\n\n"),
        
        p("There are three components to the module: filter controls, a summary plot, and a table of results.\n"),
        
        p("The filter controls allow you to specify which subset of values are shown in the table and plot. It's possible to select multiple groups and genes."),
        
        p("The histogram shows the distribution of t-statistic values, given the filter settings."),
        
        tags$ul(
          tags$li("The x-axis shows the t-statistic value, positive if the group of normal (un-altered) samples has higher immune readout scores, and negative if the opposite is true."),
          tags$li("The y-axis represents the number of genes with that statistic.")
        ),
        p("\n\nImmune landscape manuscript context: The results are comparable to those shown in Figure S4A.","\n"),
        p(""),
        p("Notes: A statistical test is performed only when the number of samples exceeds a minimum required group count (currently 3). 
                  In rare instances all (or all but one) samples within a group contain the alteration and a test cannot be performed.
                  Only statistics with p-values less than 0.001 are retained. As this module relies heavily on pre-computation, it is 
                  not currently compatible with loading of user-defined custom sample groups."),
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
  study_subset_selection,
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
    
    if (group_internal_choice() == 'Subtype_Immune_Model_Based') {
      res0 <- sort(unique(subset_df() %>%  dplyr::pull(Subtype_Immune_Model_Based)))
    }
    else if (group_internal_choice() == 'Study') {
      res0 <- sort(unique(subset_df() %>%  dplyr::pull(Study)))
    }
    else if (group_internal_choice() == 'Subtype_Curated_Malta_Noushmehr_et_al') {
      res0 <- sort(unique(subset_df() %>%  dplyr::pull(Subtype_Curated_Malta_Noushmehr_et_al)))
    }
    else { res0 <- NULL }
    
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
                          dplyr::filter(Metric == input$response_variable) %>%
                          dplyr::filter(Group_label == group_internal_choice())  %>%
                          dplyr::pull(Gene)))
    
    # table of genes
    gdf <- panimmune_data$im_potential_factors
    res2 <- gdf %>% dplyr::pull(`Gene Family`) %>% unique() %>% na.omit() 
    res2 <- ifelse(res2 == "Nectin and Nectin-like ligand/receptor co-signalling molecules", "Nectin / Nectin-like", res2)
    res2 <- as.character(sapply(res2, function(a) paste0('<geneset> ', a)))
    
    selectInput(
      ns("cn_gene_point_filter"),
      "Select Gene Filter",
      choices = c('All', 'ImmunoModulators', res2, res1),
      selected = "All",
      multiple = T
    )
  })
  
  
  output$cnv_results_count <- renderText({
    if(is.null(cnvs_df())){
      return("Members in current selected groupings do not have driver CNV results")
    } else {
      res1 <- sort(unique(cnvs_df() %>% 
                            dplyr::filter(Metric == input$response_variable) %>%
                            dplyr::filter(Group_label == group_internal_choice())  %>%
                            dplyr::pull(Gene)))
      
      string <- stringr::str_c(
        "Total number of rows: ", as.character(  dim(cnvs_df())[1]  ), 
        ",  Number of genes:  ", as.character(length(res1))  
      )
      return(string)
    }
  })
  
  
  # filter genes by family or singles    
  gene_filters <- function(res0) {
    
    # table of genes
    gdf <- panimmune_data$im_potential_factors
    
    # gene families
    gcats <- gdf %>% dplyr::pull(`Gene Family`) %>% unique() %>% na.omit() 
    gcats <- ifelse(gcats == "Nectin and Nectin-like ligand/receptor co-signalling molecules", 
                    "Nectin / Nectin-like", gcats)
    
    # the gene filter selection
    gene_input <- input$cn_gene_point_filter
    gene_input <- gsub('<geneset> ', '', gene_input)
    
    if ('ImmunoModulators' %in% gene_input) {
      # then put all families in selection
      gene_input <- c(gene_input, gcats)
    }
    
    # add genes to whatever was already in the gene input
    genes <- c(gene_input, gdf %>% dplyr::filter(`Gene Family` %in% gene_input) %>% dplyr::pull(Gene))
    
    # fiter those genes!
    res0 %>% dplyr::filter(Gene %in% genes)
  }
  
  
  # applying the filter controls.
  filter_df <- reactive({
    
    res0 <- cnvs_df() %>% dplyr::filter(Metric == input$response_variable)
    
    if (! 'All' %in% input$cn_gene_point_filter) {
      res0 <- gene_filters(res0)
    } 
    
    if (! 'All' %in% input$cn_dir_point_filter ) {
      res0 <- res0 %>% dplyr::filter(Direction == input$cn_dir_point_filter)
    }
    
    if (! 'All' %in% input$cn_group_point_filter ) {
      res0 <- res0 %>% dplyr::filter(Group %in% input$cn_group_point_filter)
    }
    
    return(res0)
  })
  
  #### PLOT ####
  output$cnvPlot <- renderPlotly({
    df <- dplyr::select(filter_df(), x = T_stat)
    
    create_histogram(
      df,
      x_lab = 'T statistics, Positive if normal value higher',
      y_lab = 'Number of tests',
      title = 'Distribution of T statistics',
      source_name = "cnv_hist"
    )
  })
  
  ### Table ###
  create_data_table <- function(eventdata, filter_df) {
    
    t_stats <- eventdata$x
    
    DT::datatable(
      
      filter_df %>% 
        dplyr::select(-Group_label, -Label, -Pvalue) %>%
        dplyr::mutate_at(vars(Mean_Normal, Mean_CNV, T_stat, Neg_log10_pvalue , Mean_Diff), dplyr::funs(round(., 3))) %>%
        dplyr::select(Metric, Group, Gene, Direction, Mean_Normal, Mean_CNV, Mean_Diff, T_stat, Neg_log10_pvalue),
      
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
  }
  
  
  # Create the Data Table given the filter settings
  output$cnvtable <- DT::renderDataTable({
    eventdata <- event_data("plotly_selected", source = "cnv_hist")
    
    create_data_table(eventdata, filter_df())
  })
  
} # end Server
