subtypeclassifier_UI <- function(id) {

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
          tags$li("Any missing genes required by the classifier are reported."),
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
          ),
          column(
            width = 3,
            downloadLink(ns("genelist"), "Download list of genes required by the classifier.")
          )
        )
      ),
      fluidRow(
        plotBox(
          width=12,
          htmlOutput(ns('geneMatchCnt'))
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

    # Main results
    sectionBox(
      title = "Subtype Classification Table",
      messageBox(
        width = 12,
        p("The table shows the results of subtype classification. Use the Search box in the upper right to find a sample of interest.")
      ),
      fluidRow(
        tableBox(
          width = 12,
          DT::dataTableOutput(ns("subtypetable")) %>%
            shinycssloaders::withSpinner(),
          downloadButton(ns('download_calls'), 'Download')
        )
      )
    ),
    
    # Any missing genes table
    sectionBox(
      title = "Missing genes from upload",
      messageBox(
        width = 12,
        p("The table shows any missing genes.")
      ),
      fluidRow(
        tableBox(
          width = 12,
          DT::dataTableOutput(ns("missinggenetable")) %>%
            shinycssloaders::withSpinner(),
          downloadButton(ns('download_missing'), 'Download')
        )
      )
    )
  ) # taglist
}


subtypeclassifier <- function(
    input, 
    output, 
    session, 
    group_display_choice, 
    group_internal_choice,
    subset_df, 
    plot_colors) {

    ns <- session$ns
    
    library(ImmuneSubtypeClassifier)

    newData <- eventReactive(input$subtypeGObutton, {
      readNewDataTable(input$expr_file_pred, input$sepa)
    })
    
    ebpp_genes <- reactive({
      data(ebpp_gene)
      ebpp_genes_sig
      })
    
    output$genelist <- downloadHandler(
     filename = "subtypeclassifier_genes.tsv",
     content = function(con){
       write.csv(ebpp_genes(), con)
     }
    )
    
    #' geneMatchErrorReport
    #' Check whether the incoming data matches the 485 model gene IDs
    #' @export
    #' @param X gene expression matrix, genes in rows, samples in columns
    #' @return list with percent missing genes and a vector of missing genes
    #' @examples
    #' missingGenes <- geneMatchErrorReport(X)
    #'
    geneMatchErrorReport <- function(X, geneid='symbol') {
      data(ebpp_gene)
      
      if (geneid == 'symbol') {
        idx <- match(table = rownames(X), x = ebpp_genes_sig$Symbol)  ### this is just for the EBPP genes ###
        
      } else {
        print("For geneids, please use gene symbols.")
        return(NA)
      }
      
      # idx will be 485 elements long... non matched ebpp_sig_genes
      # will show as NAs in the list.
      # SO... we calculate sum of NAs over size of ebpp_genes_sig
      
      matchError <- sum(is.na(idx)) / nrow(ebpp_genes_sig)
      
      # NAs in idx will enter NA rows in X2 
      
      g <- ebpp_genes_sig[is.na(idx),]  ### Adds NA rows in missing genes
      
      return(list(matchError=matchError, missingGenes=g, numGenesInClassifier=nrow(ebpp_genes_sig)))
    }

    
    # get new calls
    getCalls <- reactive({
      shiny::validate(
        shiny::need(ncol(newData())>1,
                    "Only one column available in the uploaded file. Please check if the selected file separator is the correct one."))
    
      output$geneMatchCnt <- renderText({
        matchInfo <- geneMatchErrorReport(X=newData())

        n <- (round(matchInfo$matchError, digits = 3))*100
        m <- matchInfo$numGenesInClassifier
          
        paste0('<b> Match error: ', n, '% from a total of ', m, ' genes required by the classifier.  Missing genes shown below. </b>')
      })
      
      classifySubtype(newData())
    })
    
  
    output$barPlot <- renderPlot({
      counts <- table(getCalls()$Calls$BestCall)
      barplot(counts, main="New Cluster Label Calls",
              xlab="Cluster Labels")
    })


    # Filter data based on selections
    output$subtypetable <- DT::renderDataTable(
      DT::datatable(as.data.frame(getCalls()$Calls))
    )

    missing_genes <- reactive({
      as.data.frame(geneMatchErrorReport(X=newData())$missingGenes)
    })
    # Missing Genes Table
    output$missinggenetable <- DT::renderDataTable(
      DT::datatable(missing_genes()
        #as.data.frame(geneMatchErrorReport(X=newData())$missingGenes)
      )
    )
    
    output$download_calls <- downloadHandler(
      filename = function() stringr::str_c("immune-calls-", Sys.Date(), ".csv"),
      content = function(con) readr::write_csv(getCalls()$Calls, con)
    )
    
    output$download_missing <- downloadHandler(
      filename = function() stringr::str_c("missing-genes-", Sys.Date(), ".csv"),
      content = function(con) readr::write_csv(missing_genes(), con)
    )

}
