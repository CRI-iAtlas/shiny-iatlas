immune_subtype_classifier_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        
        titleBox("iAtlas Tools — Immune Subtype Classifier"),
        
        textBox(
            width = 12,
            p("Upload gene expression* and classify immune subtypes.")
        ),
        
        # Immunomodulator distributions section ----
        sectionBox(
            title = "Immune Subtype Classification",
            
            messageBox(
                width = 12,
                
                p("Upload gene expression (csv or tsv). **BETA** any gene quantification pipeline should be OK."),
                
                p(""),
                p(""),
                p("Notes on input data:"),
                tags$ul(
                    tags$li("First row should be a header, with a 'GeneSymbol' column label, followed by sample IDs."),
                    tags$li("First column should contain gene symbols, after that, samples."),
                    tags$li("For an example of outputs, leave the input file blank, set ensemble size to a small number (32) and click GO.")
                ),
                p(""),
                tags$a(href="https://github.com/CRI-iAtlas/shiny-iatlas/blob/develop/data/ebpp_test1_1to20.tsv", "Get an example input file here."),
                p(""),
                tags$hr(),
                p(tags$b("Data Formatting Example:")),
                p(""),
                p("GeneSymbol, Sample1, Sample2,..."),
                p("RKK1,       14.5,    100.1,..."),
                p("CMQ4,       1.10,    80.711,..."),
                p("....,       ....,    and etc..."),
                p(""),
                p(""),
                tags$hr(),
                
                p("Tool settings:"),
                tags$ul(
                    tags$li(shiny::em('File separator'), ", select commas or tabs.")
                ),
                
                p(""),
                p("Notes on the classification"),
                tags$ul(
                    tags$li("An ensemble of XGBoost classifiers was used."),
                    tags$li("Robust features are computed that are not dependent on expression value.")
                ),
                p(""),
                p("Outputs:"),
                tags$ul(
                    tags$li("Table shows TCGA reported subtypes with new aligned subtype calls."),
                    tags$li("Barplot shows subtypes given to the new data."),
                    tags$li("Table gives aligned subtypes, signature scores, and cluster probabilities.")
                ),
                p(""),
                p("Manuscript context:  See figure 1A.")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 2,
                        radioButtons(ns("sepa"), "File Separator",
                                     choices = c(Tab = "\t", Comma = ","), selected = "\t")
                    ),
                    
                    column(
                        width = 4,
                        fileInput(ns("expr_file_pred"), "Choose file. Leave blank for example run.",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv",
                                             ".csv.gz",
                                             "text/tsv",
                                             "text/comma-separated-values,text/plain",
                                             ".tsv",
                                             ".tsv.gz"),
                                  placeholder = 'data/ebpp_test1_1to20.tsv')
                    ),
                    
                    column(
                        width = 3,
                        actionButton(ns("subtypeGObutton"), "GO")
                    )
                )
            ),
            
            fluidRow(
                plotBox(
                    width = 12,
                    plotOutput(ns('barPlot')) %>%
                        shinycssloaders::withSpinner()
                )
            )
        ),
        
        # Immunomodulator annotations section ----
        sectionBox(
            title = "Subtype Classification Table",
            messageBox(
                width = 12,
                p("The table shows the results of subtype classification. Use the Search box in the upper right to find a sample of interest.")
            ),
            fluidRow(
                tableBox(
                    width = 12,
                    div(style = "overflow-x: scroll",
                        DT::dataTableOutput(ns("subtypetable")) %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    ) # taglist
}