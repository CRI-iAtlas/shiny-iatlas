germline_rarevariants_ui <- function(id){
  
  ns <- shiny::NS(id)
  shiny::tagList(
    messageBox(
      width = 12,
      shiny::p("The boxplots show the values of selected immune traits across samples with germline mutations in genes belonging to defined functional categories, or pathways."), 
      shiny::p("We performed association analyses between germline pathogenic and likely pathogenic cancer predisposition variants in high penetrance susceptibility genes, and immune traits and immune
                subtypes.  Since mutations in most of the genes were rare we collapsed genes into categories summarizing different biologic processes or functions, when possible."),
      shiny::actionLink(ns("method_link"), "Click to view method description.")
    ),
    optionsBox(
      width = 12,
      shiny::column(
        width = 8,
        shiny::uiOutput(ns("features"))
      ),
      shiny::column(
        width = 4,
        shiny::selectizeInput(ns("order_box"),
                              "Order plot by ",
                              choices = list(
                                "p-value" = "p_value",
                                "Median" = "q2",
                                "Mean" = "mean",
                                "Min" = "min",
                                "Max" = "max",
                                "Number of patients with mutation" = "n_mutations"
                              ),
                              selected = "p_value")
      )
    ),
    plotBox(
      width = 12,
      plotly::plotlyOutput(ns("dist_plot"), height = "700px") %>%
        shinycssloaders::withSpinner(.)
    ),
    messageBox(
      width = 3,
      shiny::p("Tests comparing a pathway group with all other groups were performed. 
              In the Pathway column, 'Multiple' refers to samples with mutation in more than one pathway; and 'No defect' refers to samples with no mutation in the studied pathways.")
    ),
    plotBox(
      width = 9,
      DT::dataTableOutput(ns("stats_tbl"))
    )
    
  )
}

germline_rarevariants_server <- function(input, output, session) {
      
      ns <- session$ns

      output$features <- renderUI({

        trait_choices <- germline_data$rare_variants %>% 
          dplyr::select(display,category) %>%
          dplyr::group_by(category) %>%
          tidyr::nest(data = c(display))%>%
          dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>%
          tibble::deframe()
        
        shiny::selectInput(ns("feature"),
                           "Search and select Immune Trait",
                           choices = trait_choices,
                           selected = trait_choices[1])
      })
      
      selected_data <- reactive({
        germline_data$rare_variants %>%
          dplyr::filter(display == input$feature)
      })
      
      output$dist_plot <- plotly::renderPlotly({
        shiny::req(input$feature)

        df <- selected_data() %>% tidyr::drop_na()
        plot_levels <- (df %>% dplyr::arrange(desc(.[[input$order_box]])))$pathway
        
        create_boxplot_from_summary_stats(
          df,
          "pathway",
          "q1",
          "q2",
          "q3",
          "min",
          "max",
          "mean",
          order_by = plot_levels#,
          #color_col = "p_value"#,
          # fill_colors = bar_colors
        )
      })
      
      output$stats_tbl <- DT::renderDataTable({
        shiny::req(input$feature)
        DT::datatable(
          selected_data() %>% dplyr::select(Pathway = pathway, 'Patients with mutation' = n_mutations, 'Total patients' = n, 'p-value' = p_value) ,
          rownames = FALSE,
          options = list(order = list(3, 'asc'))
        ) %>% DT::formatRound(columns= "p-value", digits=3)
      })
      
      observeEvent(input$method_link,{
        shiny::showModal(modalDialog(
          title = "Method",
          includeMarkdown("data/MethodsText/germline-rarevariants.md"),
          easyClose = TRUE,
          footer = NULL
        ))
      })
}