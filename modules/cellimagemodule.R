#Loading data for the network visualization
cell_scaffold <- readr::read_tsv("data/network/scaffold_network_cellimage.tsv") %>% as.data.frame()
positions <- readr::read_csv("data/network/nodes_position.csv", col_types = c("cii")) %>% as.data.frame()

net_data = list(
  "immune"= feather::read_feather("data/network/node_cellimage_immune.feather"),
  "subtype"= feather::read_feather("data/network/node_cellimage_subtype.feather"),
  "study"= feather::read_feather("data/network/node_cellimage_study.feather")
)
      
styleNodes = "data/javascript/style_network_cellimage.js"

#organize data to display FriendlyName
friendly <- feather::read_feather("data/network/network_node_label_friendly.feather") %>% as.data.frame()
rownames(friendly) <- friendly$Obj

#Flag to change the cell image to ABUNDANCE data. If FALSE uses value average
abundance <- TRUE
  
cellimage_UI <-function(id){
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Cell Image"),
    textBox(
      width = 12,
      includeMarkdown("data/markdown/cell_image.markdown")
    ),
    
    sectionBox(
      title = "Cell Image Module",
      messageBox(
        width = 12,
        p("Select a subtype to view mean abundance among samples in that type")
      ),
      fluidRow(

        optionsBox(
          column(
            width = 6,
            radioButtons(ns("ui1"), "Select type of visualization:", choices = c("Image", "Network"), selected = "Image")
          ),
          column(
            width = 6,
            uiOutput(ns("select_ui"))
          )
        ),
        optionsBox(
          column(
            width = 6,
            radioButtons(ns("ui2"), "Select type of visualization:", choices = c("Image", "Network"), selected = "Network")
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
        )
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

    sample_group_vector <-  panimmune_data$sample_group_df %>%
      dplyr::filter(sample_group ==  group_internal_choice()) %>%
      `if`(
        group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al",
        dplyr::filter(., `TCGA Studies`== study_subset_choice()),
        .
      ) %>% dplyr::pull(FeatureValue)

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
    if(selected_group == "Subtype_Immune_Model_Based") net_data$immune 
    else if(selected_group == "Subtype_Curated_Malta_Noushmehr_et_al") net_data$subtype
    else net_data$study
  }
  
  #Output depending on the option selected by the user
  
  output$plot1 <- renderUI({
    req(input$groupselect_method1)
    
    nodes_ratio <- isolate(nodes_ratio(selected_group = group_internal_choice()))
    
    if(input$ui1 == "Image"){
      output$cellPlot1 <- renderPlot({
        shiny::validate(need((input$groupselect_method1 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))
        image_grob <- get_cell_image_object(cellimage_base = panimmune_data$cellimage_base, subtype_selected = input$groupselect_method1, vals_for_cellplot = nodes_ratio)
        grid::grid.draw(image_grob)
      })
      plotOutput(ns("cellPlot1"), height = 600) 

    } else if(input$ui1 == "Network"){
      
      output$imageNetwork1 <- cyjShiny::renderCyjShiny({
        shiny::validate(need((input$groupselect_method1 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))
        graph.json <- get_network_object(input$groupselect_method1, nodes = nodes_ratio)
        cyjShiny::cyjShiny(graph.json, layoutName = "preset", styleFile = styleNodes)
      })
      cyjShiny::cyjShinyOutput(ns("imageNetwork1"), height = 600)
    }
  }) 
  
  output$plot2 <- renderUI({
    req(input$groupselect_method2)
    
    nodes_ratio <- isolate(nodes_ratio(selected_group = group_internal_choice()))
    
    if(input$ui2 == "Image"){
      output$cellPlot2 <- renderPlot({
        shiny::validate(need((input$groupselect_method2 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))  
        image_grob <- get_cell_image_object(cellimage_base = panimmune_data$cellimage_base, subtype_selected = input$groupselect_method2, vals_for_cellplot = nodes_ratio)
        grid::grid.draw(image_grob)
      })
      plotOutput(ns("cellPlot2"), height = 600) 
     
    } else if(input$ui2 == "Network"){
      
      output$imageNetwork2 <- cyjShiny::renderCyjShiny({
        shiny::validate(need((input$groupselect_method2 %in% nodes_ratio$Group), "Please select another subtype - this one has limited data."))
        graph.json <- get_network_object(input$groupselect_method2, nodes = nodes_ratio)
        cyjShiny::cyjShiny(graph.json, layoutName = "preset", styleFile = styleNodes)
      })
      cyjShiny::cyjShinyOutput(ns("imageNetwork2"), height = 600) 
    }
  })
  
  
  #Functions to build the plots
  
  ##Network visualization
  
  get_network_object <- function(subtype_selected, nodes, friendly_df = friendly, positions_df = positions, scaffold = cell_scaffold){
    
    ##Edges data
    tbl_edges <- scaffold %>%
      dplyr::mutate(interaction = "interacts with")
    
    colnames(tbl_edges) <- c("source", "target", "interaction") #names required by cyjShiny package
    
    ##Nodes data
    
    nodes$Variable <- gsub("T_cells_CD8.Aggregate2", "T_cells_CD8", nodes$Variable)
    nodes$Variable <- gsub("Dendritic_cells.Aggregate1", "Dendritic_cells",  nodes$Variable)
    nodes$Variable <- gsub("Macrophage.Aggregate1", "Macrophage", nodes$Variable)
    
    nodes <- nodes %>% 
      dplyr::mutate(FriendlyName = dplyr::case_when(
        Variable %in% friendly_df$Obj ~ friendly_df[Variable, 3],
        !(Variable %in% friendly_df$Obj) ~ Variable
      ))
    
    #include nodes position 
    nodes <- merge(nodes, positions_df, by = "FriendlyName") #%>% dplyr::select(id, UpBinRatio, FriendlyName)
    
    tbl_nodes <- nodes %>%
      dplyr::filter(Group == subtype_selected) %>% 
      dplyr::rename(id = Variable, UpBinRatio = Value) %>% 
      dplyr::select(id, UpBinRatio, x, y, FriendlyName) %>% 
      dplyr::arrange(id)
    
    cyjShiny::dataFramesToJSON(tbl_edges, tbl_nodes)
  }
  
}


