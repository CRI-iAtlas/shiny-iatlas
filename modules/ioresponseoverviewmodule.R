ioresponseoverview_UI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Datasets of Treatment with Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            #includeMarkdown("data/markdown/io_response_description.markdown")
            p("This module describes the datasets that are available in iAtlas for the analysis of molecular response to 
              Immune Checkpoint Inhibitor immunotherapy(ICI). Primary data processing and scoring of immune response, including 
              immune signatures and cell-content estimates were performed by UNC Lineberger team Dante Bortone, Sarah Entwistle,
              led by Benjamin G. Vincent.")
        ),
        
        sectionBox(
            title = "Dataset Information",
            
            messageBox(
                width = 24,
                includeMarkdown("data/markdown/io_response_description.markdown")
            ),
            plotBox(
                width = 12,
                DT::DTOutput(
                    ns("io_datasets_df")
                ),
                downloadButton(ns('download_metadata'), 'Download Dataset Metadata')
                # downloadButton(ns('download_data'), 'Download Immune Features and Clinical data'),
                # downloadButton(ns('download_expr'), 'Download Gene Expression data'),
                # br(),
                # tags$a(href="https://www.synapse.org/", "Download the complete gene expression data from Synapse")
            )#plotBox
        ),
        sectionBox(
          title = "Group Key",
          
          messageBox(
            width = 24,
            #includeMarkdown("data/markdown/io_response_description.markdown")
             p("Samples in each dataset can be grouped based on study characteristics. 
               Here you can obtain details of these categories and the number of samples in each group per dataset.")
          ),
          optionsBox(
            width = 12,
            selectInput(ns("group"), "Select Category", choices = ioresponse_data$categories_df$CategoryLabel)
          ),
          plotBox(
            width = 12,
            DT::DTOutput(
              ns("io_groups_df")
            ),
            br(),
            DT::DTOutput(
              ns("io_per_ds_df")
            )
          )

        ),
        sectionBox(
          title = "Group Overlap",
          messageBox(
            width = 24,
            p("See a mosaic plot for two sample groups.")
          ),
          optionsBox(
            width = 12,
            uiOutput(ns("select_group2"))
          ),
          plotBox(
            width = 12, 
            plotlyOutput(ns("io_mosaic"), height = "600px") %>% 
              shinycssloaders::withSpinner()
          )
        )
    )
        
}

ioresponseoverview <- function(input, 
                       output, 
                       session, 
                       group_display_choice, 
                       group_internal_choice,
                       study_subset_choice,
                       sample_group_df,
                       subset_df, 
                       plot_colors){
    
    ns <- session$ns
    
    output$select_group2 <- renderUI(
      selectInput(ns("group2"), "Select second category to see groups overlap", 
                  choices = (ioresponse_data$categories_df %>% 
                               dplyr::filter(CategoryLabel != input$group))$CategoryLabel)
    )
    
    output$io_datasets_df <- DT::renderDT({
       DT::datatable((ioresponse_data$dataset_df %>% 
                       dplyr::mutate(
                         Reference = paste(
                           "<a href=\"",
                           Paper,"\">",
                           Citation,"</a>", 
                           sep=""
                         )
                       )%>% 
                      select(Dataset, Study, Antibody, `Primary Sample` = `PrimarySample(s)`, Samples, Patients, Reference)),
                     escape= FALSE)
    })
    
    # output$download_metadata <- downloadHandler(
    #   filename = function() stringr::str_c("iatlas-io-metadata-", Sys.Date(), ".csv"),
    #   content = function(con) readr::write_csv( con)
    # )
    # 
    # output$download_data <- downloadHandler(
    #   filename = function() stringr::str_c("iatlas-io-data-", Sys.Date(), ".csv"),
    #   content = function(con) readr::write_csv(con)
    # )
    # 
    # output$download_expr <- downloadHandler(
    #   filename = function() stringr::str_c("iatlas-io-im-expr", Sys.Date(), ".csv"),
    #   content = function(con) readr::write_csv( con)
    # )
    
    group1 <- reactive({
      convert_value_between_columns(input_value = input$group,
                                    df = ioresponse_data$feature_df,
                                    from_column = "FriendlyLabel",
                                    to_column = "FeatureMatrixLabelTSV")
    }) 
    
    group2 <- reactive({
      convert_value_between_columns(input_value = input$group2,
                                    df = ioresponse_data$feature_df,
                                    from_column = "FriendlyLabel",
                                    to_column = "FeatureMatrixLabelTSV")
    }) 
    
    
    
    output$io_groups_df <- DT::renderDT({
      DT::datatable(ioresponse_data$categories_df %>% 
                      dplyr::filter(CategoryLabel == input$group) %>% 
                      dplyr::select(Category = CategoryLabel, Definition, `Sample Groups`, `Available for`),
                    rownames = FALSE,
                    options = list(dom = 't'))
    })
    
    output$io_per_ds_df <- DT::renderDT({
      req(input$group)
      
      DT::datatable(get_io_overview_table(group1()) %>% 
                    data.table::setcolorder(c("Order", "Sample Group", "Group Name", "Plot Color")),
                    rownames = FALSE,
                    caption = paste("Group Size per dataset for", input$group),
                    options = list(dom = 't',
                                   order = list(list(0, 'asc')))) %>%
        DT::formatStyle(
          'Plot Color',
          backgroundColor = DT::styleEqual(unique("Plot Color"), "Plot Color"))

    })
    
    output$io_mosaic <- renderPlotly({
      req(input$group, input$group2)
      
      df_mosaic <- get_io_mosaic_df(ioresponse_data$fmx_df, group1(), group2())
      
      df_colors <- df_mosaic %>% 
                    dplyr::select(y, plot_color) %>% 
                    distinct() 
      
      plot_colors <- c("#C9C9C9", df_colors$plot_color)
      names(plot_colors) <- c("Not annotated", as.character(df_colors$y))
     
      create_mosaicplot(df_mosaic %>% dplyr::select(x,y),
                        title = stringr::str_c(input$group2, "by", input$group, sep = " "),
                        fill_colors = plot_colors) %>% 
        layout(
          autosize = TRUE,
          margin = list(b=0)
        )
    })
    
}




   