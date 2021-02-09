germline_heritability_ui <- function(id){
  
  ns <- shiny::NS(id)
  shiny::tagList(
    messageBox(
      width = 12,
      includeMarkdown("data/markdown/germline_heritability.markdown")
    ),
    optionsBox(
      width = 3,
      shiny::column(
        width = 12,
        shiny::selectizeInput(ns("parameter"), "Subset by",
                              choices = c("Ancestry" = "cluster",
                                          "Immune Feature" = "display",
                                          "Immune Category" = "category",
                                          "Immune Module" = "module"
                              ),
                              selected = "Ancestry"),
        shiny::uiOutput(ns("selection_options")),
        shiny::sliderInput(ns("pvalue"),
                           "Select p-value threshold",
                           min = 0, max = 0.5, value = 0.05, step = 0.01),
        shiny::selectizeInput(ns("order_bars"),
                              "Order bars by ",
                              choices = list("V(Genotype)/Vp" = "variance",
                                             "LRT p-value" = "p_value",
                                             "LRT FDR" = "fdr",
                                             "Immune Trait Category" = "category",
                                             "Immune Trait Module" = "module",
                                             "Ancestry" = "cluster"
                              ),
                              selected = "Variance")
      )
    ),
    plotBox(
      width = 9,
      plotly::plotlyOutput(ns("heritability"), height = "700px") %>%
        shinycssloaders::withSpinner(.)
    )
  )
}

germline_heritability_server <- function(input, output, session){

      ancestry_options <- reactive({
        c("Ad Mixed American" = "American", "African" = "African", "Asian" = "Asian", "European"= "European", "European by Immune Subtype" = "European_immune")
      })
      
      ns <- session$ns
      
      output$selection_options <- renderUI({
        shiny::req(input$parameter)
        
        if(input$parameter == "cluster") opt <- ancestry_options()
        
        if(input$parameter == "display"){
          opt <- germline_data$heritability %>%
            dplyr::select(display,category) %>%
            dplyr::group_by(category) %>%
            tidyr::nest(data = c(display))%>%
            dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>%
            tibble::deframe()
        }
        if(input$parameter == "category") opt <- unique(germline_data$heritability$category)
        if(input$parameter == "module") opt <- unique(germline_data$heritability$module)
        
        shiny::selectizeInput(ns("group"), "Show associated results for", choices = opt, selected = opt[4])
        
      })
      
      plot_title <- reactive({
        if(input$parameter == "cluster"){
          if(input$group != "European_immune") "V(Genotype)/Vp"
          else "V(Genotype x Immune Subtype)/Vp"
        } else{
          paste("V(Genotype)/Vp", input$group, sep = " - ")
        }
      })
      
      hdf <- reactive({
        shiny::req(input$group)
        create_heritability_df(
          heritablity_data = germline_data$heritability,
          parameter = input$parameter,
          group = input$group,
          pval_thres =input$pvalue,
          ancestry_labels = ancestry_options()
        )
      })
      
      output$heritability <- plotly::renderPlotly({
        shiny::req(hdf())
        shiny::validate(
          shiny::need(nrow(hdf())>0, "No Immune Trait with a p-value lower than selected.")
        )
        
        #order bars
        if(is.numeric(hdf()[[input$order_bars]]))  plot_levels <-levels(reorder(hdf()[["ylabel"]], hdf()[[input$order_bars]], sort))
        else plot_levels <- (hdf() %>%
                               dplyr::arrange(.[[input$order_bars]], variance))$ylabel %>%
          as.factor()
        
        
        hdf() %>%
          dplyr::mutate('Neg_log10_p_value' = -log10(p_value)) %>% #changing column name to legend title display
          create_barplot_horizontal(
            df = .,
            x_col = "variance",
            y_col = "ylabel",
            error_col = "se",
            key_col = NA,
            color_col = "Neg_log10_p_value",
            label_col = "label",
            xlab = "Heritability",
            ylab = "",
            order_by = plot_levels,
            title = plot_title(),
            showLegend = TRUE,
            legendTitle = "LRT \n p-value",
            source_name = "heritability_plot",
            bar_colors = NULL
          ) %>%
          format_heritability_plot(., hdf(), fdr = TRUE)
      })
      
      # output$heritability_cov <- plotly::renderPlotly({
      #   
      #   eventdata <- plotly::event_data( "plotly_click", source = "heritability_plot")
      #   sub_clusters <- c("Covar:Immune Subtype", "C1", "C2", "C3")
      #   
      #   shiny::validate(
      #     shiny::need(!is.null(eventdata),
      #                 "Click bar plot"))
      #   selected_plot_trait <- eventdata$y[[1]]
      #   
      #   hdf_plot <- germline_data$heritability %>%
      #     dplyr::filter(cluster %in% sub_clusters & display == selected_plot_trait)
      #   
      #   plot_colors <- c("#bebebe", "#FF0000", "#FFFF00", "#00FF00")
      #   names(plot_colors) <- sub_clusters
      #   
      #   hdf_plot$cluster <- factor(hdf_plot$cluster, levels = c("C3", "C2", "C1", "Covar:Immune Subtype" ))
      #   
      #   create_barplot_horizontal(
      #     df = hdf_plot,
      #     x_col = "Variance",
      #     y_col = "cluster",
      #     error_col = "SE",
      #     key_col = NA,
      #     color_col = "cluster",
      #     label_col = NA,
      #     xlab = "Heritability",
      #     ylab = "",
      #     title = paste("Random data for", selected_plot_trait),
      #     showLegend = FALSE,
      #     source_name = NULL,
      #     bar_colors = plot_colors
      #   ) %>%
      #     format_heritability_plot(., hdf_plot, fdr = FALSE)
      # })
      
      observeEvent(input$method_link,{
        shiny::showModal(modalDialog(
          title = "Method",
          includeMarkdown("data/MethodsText/germline-heritability.md"),
          easyClose = TRUE,
          footer = NULL
        ))
      })
}
