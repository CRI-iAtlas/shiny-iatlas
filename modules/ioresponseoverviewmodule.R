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
          title = "Groups Information",
          
          messageBox(
            width = 24,
            #includeMarkdown("data/markdown/io_response_description.markdown")
             p("Samples in each dataset can be grouped based on study characteristics. Here you can obtain details of these groups, and when you click in a row, you will be able to see the number of samples in each group per dataset.")
          ),
          plotBox(
            width = 12,
            DT::DTOutput(
              ns("io_groups_df")
            )
          ),
          plotBox(
            width = 12,
            DT::DTOutput(
              ns("io_per_ds_df")
            ),
            br()
          )#plotBox
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
    
    output$io_groups_df <- DT::renderDT({
      DT::datatable(ioresponse_data$group_df, 
                    selection = 'single',
                    options = list(pageLength = 15))
    })
    
    output$io_per_ds_df <- DT::renderDT({
  
      clicked_group <- ioresponse_data$group_df[input$io_groups_df_rows_selected, ]$Group
      validate(need(length(clicked_group)>0, "Click table above"))
      
      group <- convert_value_between_columns(input_value = clicked_group,
                                             df = ioresponse_data$feature_df,
                                             from_column = "FriendlyLabel",
                                             to_column = "FeatureMatrixLabelTSV")
      
      DT::datatable(get_io_overview_table(group) %>% 
                    data.table::setcolorder(c("Order", "Sample Group", "Group Name", "Plot Color")),
                    rownames = FALSE,
                    caption = paste("Group Size per dataset for", clicked_group),
                    options = list(dom = 't',
                                   order = list(list(0, 'asc')))) %>%
        DT::formatStyle(
          'Plot Color',
          backgroundColor = DT::styleEqual(unique("Plot Color"), "Plot Color"))

    })
}



   