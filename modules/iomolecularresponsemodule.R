ioresponse_UI <- function(id){
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Clinical Outcomes to Immune Checkpoint Inhibitors"),
        textBox(
            width = 12,
            p("Explore the ‘omics’ data sets on response to checkpoint inhibitors treatments")
        ),
        
        sectionBox(
            title = "Clinical Outcomes",
            
            messageBox(
                width = 24,
                p("This module generates different analysis of response of immune checkpoint inhibitors (ICI) treatment.")
                #includeMarkdown("data/markdown/cytokine_network.markdown")
            ),
            
            optionsBox(
                width=12,
                fluidRow(
                    column(
                        width = 2,
                        checkboxGroupInput(ns("datasets"), "Select Datasets", choices = c("Auslander, 2018", "Gide, 2019", "Hugo, 2016", "IMVigor210", "Prins, 2019", "Riaz, 2017", "Van Allen, 2015")) 
                    ),
                    column(
                        width = 4,
                        fluidRow(
                            #HTML("<p><b><center>Select Features of Interest </center></b></p>")
                            p(strong("Select Features of Interest"))   
                        ),
                        fluidRow(
                            
                            column(
                                width = 6, 
                                selectizeInput(ns("cellInterest"), "Cell phenotypes", choices = c("T_cells_CD8", "NK_cells"), 
                                               multiple = TRUE, options = list(placeholder = "Default: all cells")),
                                selectizeInput(ns("geneInterest"), "Genes", choices = panimmune_data$im_direct_relationships$`HGNC Symbol`, 
                                               multiple = TRUE, options = list(placeholder = "Default: immunomodulator genes"))
                            ),
                            column(
                                width = 6,
                                selectizeInput(ns("genesigs"), "Gene signatures", choices = c("T_cells_CD8", "NK_cells"), 
                                               multiple = TRUE),
                                selectizeInput(ns("immunefeat"), "Immune Features", choices = panimmune_data$im_direct_relationships$`HGNC Symbol`, 
                                               multiple = TRUE)
                                #           p(strong("Response Categories")),
                            )
                        ),
                        fluidRow(
                            a(href = "", "Or upload features of interest")
                        )
                    ),
                    column(
                        width = 2,
                        checkboxGroupInput(ns("response"), "Select Response Categories", choices = c("OS_d", "OS_y", "OS_e", "PR", "CR", "SD"))
                        
                    ),
                    column(
                        width = 2,
                        checkboxGroupInput(ns("analyses"), "Select Analyses", choices = c("Groupwise Comparison", "CoxPH")),
                        br(),
                        div(class = "form-group shiny-input-container", actionButton(ns("calculate_button"), tags$b("GO"), width = "100%"))
                    )
                    
                )
                
            ),#optionsBox
            plotBox(
                width = 12,
                height = "100%"
            )
        )
    )
}

ioresponse <- function(input, 
                       output, 
                       session, 
                       group_display_choice, 
                       group_internal_choice,
                       study_subset_choice,
                       sample_group_df,
                       subset_df, 
                       plot_colors){
    
}