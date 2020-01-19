immune_subtype_classifier_server <- function(
    input, output, session, group_display_choice, group_internal_choice,
    subset_df, plot_colors) {

    ns <- session$ns
    
    source("functions/immune_subtype_classifier_functions.R", local = T)

    # in src files ... have same path as app.R
    # reportedClusters <- getSubtypeTable()

    # get new calls
    getCalls <- eventReactive(input$subtypeGObutton, {

      newdat <- input$expr_file_pred

      print(head(newdat))

      classifySubtype(newdat, input$sepa)
    })


    output$barPlot <- renderPlot({
      counts <- table(getCalls()$Calls$BestCall)
      barplot(counts, main="New Cluster Label Calls",
              xlab="Cluster Labels")
    })


    # Filter data based on selections
    output$subtypetable <- DT::renderDataTable(
      DT::datatable(
        as.data.frame(getCalls()$Calls),
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
