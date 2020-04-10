ioresponseoverview_UI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Datasets of Treatment with Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            #includeMarkdown("data/markdown/io_response_description.markdown")
            p("This module describes the datasets that are available for the analyses of molecular response to Immune Checkpoint Inhibitor Inhibitors (ICI). Immune signatures and CIBERSORT cell estimates were computed by Dante, Sarah, Ben")
        ),
        
        sectionBox(
            title = "Dataset Information",
            
            messageBox(
                width = 24,
                includeMarkdown("data/markdown/io_response_description.markdown")
               # p("All the modules in the Molecular Response to ICI section of CRI-iAtlas provide visualization of immunogenomics features of the datasets described in the table below. 
               #   You can also download the data for further analysis. Details of the methods to compute the features are provided in the Data Description section.")
            ),
            plotBox(
                width = 12,
                DT::DTOutput(
                    ns("io_datasets_df")
                ),
                downloadButton(ns('download_metadata'), 'Download Dataset Metadata'),
                downloadButton(ns('download_data'), 'Download Immune Features and Clinical data'),
                downloadButton(ns('download_expr'), 'Download Gene Expression data')
                
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
       DT::datatable((dataset_io_df %>% 
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
        # DT::datatable((fmx_io %>% 
        #                  #dplyr::group_by(Dataset,Response) %>% 
        #                    dplyr::group_by(Dataset, Study, Drug, Antibody) %>% 
        #                    summarise(Samples = dplyr::n_distinct(Sample_ID))),
        #               options = list(pageLength = 20))
        
    })
    
    output$download_metadata <- downloadHandler(
      filename = function() stringr::str_c("iatlas-io-metadata-", Sys.Date(), ".csv"),
      content = function(con) readr::write_csv(dataset_io_df, con)
    )
    
    output$download_data <- downloadHandler(
      filename = function() stringr::str_c("iatlas-io-data-", Sys.Date(), ".csv"),
      content = function(con) readr::write_csv(fmx_io, con)
    )
    
    output$download_expr <- downloadHandler(
      filename = function() stringr::str_c("iatlas-io-im-expr", Sys.Date(), ".csv"),
      content = function(con) readr::write_csv(im_expr_io_df, con)
    )
    
}



   