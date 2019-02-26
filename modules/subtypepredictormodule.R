subtypepredictor_UI <- function(id) {

  ns <- NS(id)

  tagList(

    titleBox("iAtlas Tools — Immune Subtype Classifier"),

    textBox(
      width = 12,
      p("Upload gene expression* and classify immune subtypes ( *RSEM expression ).")
    ),

    # Immunomodulator distributions section ----
    sectionBox(
      title = "Model Based Clustering",

      messageBox(
        width = 12,

        p("Upload gene expression (csv or tsv). RSEM expression values were used to train the model, and for best results, your expression data should also be RSEM (RPKM and FPKMs may also work)."),
        p(""),
        p(""),
        p("Notes on input data:"),
        tags$ul(
          tags$li("First row should be a header, with a 'GeneSymbol' column label, followed by sample IDs."),
          tags$li("First column should contain gene symbols, after that, samples."),
          tags$li("For an example of outputs, leave the input file blank, set ensemble size to a small number (32) and click GO.")
        ),
        p(""),
        tags$a(href="https://github.com/CRI-iAtlas/shiny-iatlas/blob/develop/data/ivy20.csv", "Get an example input file here."),
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
          tags$li(shiny::em('File separator'), ", select commas or tabs."),
          tags$li(shiny::em('Log 2'), ", if the data is not already log transformed, select this."),
          tags$li(shiny::em('Combat'), ", if the data should be batch corrected when joined to the PanCancer data, select this."),
          tags$li(shiny::em('Ensemble size'), ", try different ensemble sizes to check for robust results (256 used in manuscript).")
        ),

        p(""),
        p("Notes on the data transforms for computing signatures:"),
        tags$ul(
          tags$li("1. Rows with duplicate gene names are removed."),
          tags$li("2. Data is upper quantile normalized and multiplied by 1000"),
          tags$li("3. Data is transformed with log2(x+1)."),
          tags$li("4. Each gene is median centered, in the PanCancer and new-data sets independently."),
          tags$li("5. TCGA EB++ data is joined to the new-data, and subset to genes needed for signatures (2316 genes)."),
          tags$li("6. Combat (in SVA package) used for batch correction (optional)."),
          tags$li("7. Signatures are computed for each sample independently."),
          tags$li("8. Signature scores are Z-scored by sample (column-wise)."),
          tags$li("9. Normalized scores are given to the PanCancer pre-trained cluster model."),
          tags$li("10. Called clusters are aligned with the reported TCGA immune subtypes in a greedy fashion."),
          tags$li("11. Aligned subtype calls and signatures scores are reported in a final table.")
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
          width = 10,
          column(
            width = 5,
            radioButtons(ns("sepa"), "File Separator",
                         choices = c(Comma = ",", Tab = "\t"), selected = ","),
            checkboxInput(ns("normed"), "Apply UQ Norm.", TRUE),
            checkboxInput(ns("logged"), "Apply Log2", TRUE),
            checkboxInput(ns("combat"), "Apply Combat", TRUE)
          ),

          column(
            width = 5,
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
                      placeholder = 'data/ivy20.csv'),

            numericInput(ns("ensemblenum"), "Ensemble Size (64-256)", 256, max = 256, min = 64, width = '200'),

            actionButton(ns("subtypeGObutton"), "GO")
          )
        )
      ),

      fluidRow(
        plotBox(
          width = 12,
          plotOutput(ns("distPlot")) %>%
            shinycssloaders::withSpinner()
        )
      ),

      fluidRow(
        plotBox(
          width = 12,
          plotOutput(ns("preCombat")) %>%
            shinycssloaders::withSpinner()
        )
      ),

      fluidRow(
        plotBox(
          width = 12,
          plotOutput(ns("postCombat")) %>%
            shinycssloaders::withSpinner()
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
        p("The table shows the results of subtype classification. Use the Search box in the upper right to find sample of interest.")
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


subtypepredictor <- function(
    input, output, session, group_display_choice, group_internal_choice,
    subset_df, plot_colors) {

    ns <- session$ns

    # in src files ... have same path as app.R
    reportedClusters <- getSubtypeTable()

    # get new calls
    getCalls <- eventReactive(input$subtypeGObutton, {

      newdat <- input$expr_file_pred

      print(head(newdat))

      newScores(newdat, input$logged, input$ensemblenum, input$combat, input$sepa, input$normed)
    })

    # plot of where a sample is in signature space X clusters
    output$distPlot <- renderPlot({
      #heatmap(as.matrix(getCalls()$Table), xlab = 'Reported Clusters', ylab = 'New Calls')
      imagePlot(getCalls()$Table)
    })


    # plot of where a sample is in signature space X clusters
    output$preCombat <- renderPlot({
      #heatmap(as.matrix(getCalls()$Table), xlab = 'Reported Clusters', ylab = 'New Calls')
      qplot(data=getCalls()$PreCombat, col=SampleSource, x=value, geom='density', main = 'Distribution of expression values pre-batch-correction')
    })

    # plot of where a sample is in signature space X clusters
    output$postCombat <- renderPlot({
      #heatmap(as.matrix(getCalls()$Table), xlab = 'Reported Clusters', ylab = 'New Calls')
      qplot(data=getCalls()$PostCombat, col=SampleSource, x=value, geom='density', main = 'Distribution of expression values post-batch-correction')
    })



    output$barPlot <- renderPlot({
      counts <- table(getCalls()$AlignedCalls)
      barplot(counts, main="New Cluster Label Calls",
              xlab="Cluster Labels")
    })


    # Filter data based on selections
    output$subtypetable <- DT::renderDataTable(
      DT::datatable(
        as.data.frame(getCalls()$ProbCalls),
        extensions = 'Buttons', options = list(
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

}
