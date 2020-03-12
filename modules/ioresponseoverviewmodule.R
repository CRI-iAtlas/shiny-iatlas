ioresponseoverview_UI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Datasets of Treatment with Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            p("Explore the ‘omics’ data sets on response to checkpoint inhibitors treatments")
        ),
        
        sectionBox(
            title = "Dataset Information",
            
            messageBox(
                width = 24,
                includeMarkdown("data/markdown/io_response_description.markdown")
#                p("Check the attributes about the available datasets.")
            ),
            plotBox(
                width = 12,
                DT::DTOutput(
                    ns("io_datasets_df")
                )
                
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
        DT::datatable((fmx_io %>% 
                         dplyr::group_by(Dataset,Response) %>% 
                           #dplyr::group_by(Dataset, Study, Drug, Antibody) %>% 
                           summarise(Samples = dplyr::n_distinct(Sample_ID))),
                      options = list(pageLength = 20))
        
    })
}
   