cellimage_UI <-function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Cell-Interaction Diagram"),
    textBox(
      width = 12,
      p("This module allows you to depict the estimated abundance of tumor cells and representative innate and adaptive cells in the microenvironment, along with the abundance of receptor and ligands mediating interactions between those cells.")
    ),
    
    sectionBox(
      title = "Cell-Interaction Diagram",
      messageBox(
        width = 12,
        includeMarkdown("data/markdown/cell_image.markdown"),
        actionLink(ns("method_link"), "Click to view method.")
      ),
      fluidRow(

        optionsBox(
          column(
            width = 6,
            radioButtons(ns("ui1"), "Select type of visualization:", choices = c("Diagram", "Network"), selected = "Diagram")
          ),
          column(
            width = 6,
            uiOutput(ns("select_ui"))
          )
        ),
        optionsBox(
          column(
            width = 6,
            radioButtons(ns("ui2"), "Select type of visualization:", choices = c("Diagram", "Network"), selected = "Network")
          ),
          column(
            width = 6,
            uiOutput(ns("select_ui2"))
          )
        )
      ),
        fluidRow(
          plotBox(
            width = 6,
            uiOutput(ns("plot1")) %>% shinycssloaders::withSpinner()
            ),
          plotBox(
            width = 6,
            uiOutput(ns("plot2")) %>% shinycssloaders::withSpinner()
          )
        ),
      img(src = "images/cell-image-legend.png", width = "100%"),
      br(),
      actionButton(ns("methodButton"), "Click to view method")
    ) # closes sectionBox
  ) # closes tagList
}
  
cellimage <- function(
  input, 
  output, 
  session,
  group_display_choice, 
  group_internal_choice,
  study_subset_choice,
  sample_group_df,
  subset_df, 
  plot_colors
){
  
  ns <- session$ns
  
  ## Generating the UI for group selection
  
  get_groups_ui <- reactive({
    req(
      panimmune_data$sample_group_df,
      group_internal_choice()
    )

    if(group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al"){
      req(study_subset_choice())
    }
    
    if(group_internal_choice() %in% c("Study", "Subtype_Curated_Malta_Noushmehr_et_al", "Subtype_Immune_Model_Based")){
      sample_group_vector <-  panimmune_data$sample_group_df %>%
        dplyr::filter(sample_group ==  group_internal_choice()) %>%
        `if`(
          group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al",
          dplyr::filter(., `TCGA Studies`== study_subset_choice()),
          .
        ) %>% dplyr::pull(FeatureValue) %>% 
        sort()
    }else{ #Custom group
      sample_group_vector <- sample_group_df() %>%
        dplyr::select_(group_internal_choice()) %>%
        unique()
    }
    
    sample_group_vector
  })
  
  
  output$select_ui <- renderUI({
    req(get_groups_ui())
    
    selectInput(
      ns("groupselect_method1"),
      "Select Group",
      choices = get_groups_ui()
    )
  })
  
  output$select_ui2 <- renderUI({
    req(get_groups_ui())

    selectInput(
      ns("groupselect_method2"),
      "Select Group",
      choices = get_groups_ui()
    )
  })

  #organizing abundance data for easier referencing
  
  nodes_ratio <- function(selected_group){
    if(selected_group == "Subtype_Immune_Model_Based") panimmune_data$ecn_df$immune$upbin_ratio
    else if(selected_group == "Study") panimmune_data$ecn_df$study$upbin_ratio
    else if(selected_group == "Subtype_Curated_Malta_Noushmehr_et_al") panimmune_data$ecn_df$subtype$upbin_ratio
    else compute_abundance(sample_group_df(),
                           subset_col = group_internal_choice(),
                           panimmune_data$fmx_df,
                           panimmune_data$ecn_expr,
                           cois = get_cells_from_image(panimmune_data$cellimage_base),
                           gois = get_genes_from_image(panimmune_data$cellimage_base)) %>% 
      dplyr::select(Node, IncludeFeature) %>% 
      tidyr::unnest(c(IncludeFeature))
  }
  
  #Output depending on the option selected by the user
  
  output$plot1 <- renderUI({
    req(input$groupselect_method1)
    
    nodes_ratio <- isolate(nodes_ratio(selected_group = group_internal_choice()))
    
    if(input$ui1 == "Diagram"){
      output$cellPlot1 <- renderPlot({
        shiny::validate(need((input$groupselect_method1 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))
        image_grob <- get_cell_image_object(cellimage_base = panimmune_data$cellimage_base, subtype_selected = input$groupselect_method1, vals_for_cellplot = nodes_ratio)
        grid::grid.draw(image_grob)
      })
      plotOutput(ns("cellPlot1"), height = 600) 

    } else if(input$ui1 == "Network"){
      
      output$imageNetwork1 <- cyjShiny::renderCyjShiny({
        shiny::validate(need((input$groupselect_method1 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))
        graph.json <- get_network_object(input$groupselect_method1, nodes = nodes_ratio, friendly_df = panimmune_data$ecn_labels, positions_df = panimmune_data$ci_coord, scaffold = panimmune_data$ci_scaffold)
        cyjShiny::cyjShiny(graph.json, layoutName = "preset", styleFile = "data/javascript/style_network_cellimage.js")
      })
      cyjShiny::cyjShinyOutput(ns("imageNetwork1"), height = 600)
    }
  }) 
  
  output$plot2 <- renderUI({
    req(input$groupselect_method2)
    
    nodes_ratio <- isolate(nodes_ratio(selected_group = group_internal_choice()))
    
    if(input$ui2 == "Diagram"){
      output$cellPlot2 <- renderPlot({
        shiny::validate(need((input$groupselect_method2 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))  
        image_grob <- get_cell_image_object(cellimage_base = panimmune_data$cellimage_base, subtype_selected = input$groupselect_method2, vals_for_cellplot = nodes_ratio)
        grid::grid.draw(image_grob)
      })
      plotOutput(ns("cellPlot2"), height = 600) 
     
    } else if(input$ui2 == "Network"){
      
      output$imageNetwork2 <- cyjShiny::renderCyjShiny({
        shiny::validate(need((input$groupselect_method2 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))
        graph.json <- get_network_object(input$groupselect_method2, nodes = nodes_ratio, friendly_df = panimmune_data$ecn_labels, positions_df = panimmune_data$ci_coord, scaffold = panimmune_data$ci_scaffold)
        cyjShiny::cyjShiny(graph.json, layoutName = "preset", styleFile = "data/javascript/style_network_cellimage.js")
      })
      cyjShiny::cyjShinyOutput(ns("imageNetwork2"), height = 600) 
    }
  })
  
  #Button with method information
  
    observeEvent(input$methodButton, {
      showModal(modalDialog(
        title = "Method",
        includeMarkdown("data/MethodsText/Methods_CellImage.txt"),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$method_link,{
      showModal(modalDialog(
        title = "Method",
        includeMarkdown("data/MethodsText/Methods_CellImage.txt"),
        easyClose = TRUE,
        footer = NULL
      ))
    })
}


