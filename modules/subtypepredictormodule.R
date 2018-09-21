immunomodulator_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Immune Subtype Predictor"),
    textBox(
      width = 12,
      p("Upload gene expression* and predict immune subtypes (* RSEM RPKM).")  
    ),
    
    # Immunomodulator distributions section ----
    sectionBox(
      title = "Model based clustering",
      messageBox(
        width = 12,
        p("Upload gene expression (csv or tsv). RSEM RPKM expression values were used to train the model, and for best results, your expression data should also be RSEM RPKMs. There are several settings:"),
        tags$ul(
          tags$li(em('Log 10'), ", if the data is not already log transformed, select this."), 
          tags$li(em('Ensemble size'), ", try different ensemble sizes to check for robust results."),
          tags$li(em('File separator'), ", select commas or tabs.")
        ),
        p(""),
        p("Manuscript context:  See figure 1A."),
        p(".....")
      ),
      fluidRow(
        optionsBox(
          width = 12,
          column(
              width = 2,
              radioButtons(ns("sep"), "File Separator",
                           choices = c(Comma = ",", Tab = "\t"), selected = ","),
              checkboxInput(ns("logged"), "Apply Log10", TRUE)
          ),
          column(
            width = 4,
            fileInput(ns("expr_file_pred"), "Choose CSV file with \n1st column gene symbols",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv",
                                 ".csv.gz",
                                 "text/tsv",
                                 "text/comma-separated-values,text/plain",
                                 ".tsv",
                                 ".tsv.gz"),
                      placeholder = 'data/ivy20.csv')
          ),
          column(
            width = 3,
            numericInput(ns("ensemblenum"), "Ensemble Size", 256, max = 256, min = 32, width = '100')
          ),
          column(
              width = 3,
              numericInput(ns("corenum"), "Cores", 4, width = '100'),
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
          plotOutput(ns('barPlot')) %>% 
            shinycssloaders::withSpinner()
        )
      )
    ),
    
    # Immunomodulator annotations section ----
    sectionBox(
      title = "Subtype Prediction Results Table",
      messageBox(
        width = 12,
        p("The table shows the results of subtype prediction. Use the Search box in the upper right to find sample of interest.")  
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
  )
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
      #withProgress(message = 'Working...', value = 0, {
      #  newScores(newdat, input$logged, input$corenum)
      #})
      newScores(newdat, input$logged, input$corenum, input$ensemblenum)
    })
    
    # plot of where a sample is in signature space X clusters    
    output$distPlot <- renderPlot({
      heatmap(as.matrix(getCalls()$Table), xlab = 'Reported Clusters', ylab = 'New Calls')
      #imagePlot(getCalls()$Table)
    })
    
    
    output$barPlot <- renderPlot({
      counts <- table(getCalls()$MaxCalls)
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