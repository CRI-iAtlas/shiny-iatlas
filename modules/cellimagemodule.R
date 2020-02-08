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
friendly <- panimmune_data$im_direct_relationships %>%
  dplyr::filter(!is.na(`HGNC Symbol`)) %>% 
  dplyr::select(`HGNC Symbol`, `Friendly Name`) %>% as.data.frame()

friendly[which(friendly$`HGNC Symbol` == "TNFRSF18"), 2] <- "GITR"
friendly[which(friendly$`HGNC Symbol` == "CD40LG"), 2] <- "CD40L"
friendly <- rbind(friendly, c("TNFSF18", "GITRL"))
friendly <- rbind(friendly, c("PVR", "CD155"))
friendly <- rbind(friendly, c("T_cells_CD8", "T cell"))
friendly <- rbind(friendly, c("Dendritic_cells", "Dendritic cell"))
friendly <- rbind(friendly, c("Tumor_cell", "Tumor"))
friendly <- rbind(friendly, c("Macrophage", "Tissue Macrophage"))

rownames(friendly) <- friendly$`HGNC Symbol`


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
            uiOutput(ns("plot1"))
            ),
          plotBox(
            width = 6,
            uiOutput(ns("plot2"))
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
   
    selectInput(
      ns("groupselect_method1"),
      "Select Group",
      choices = get_groups_ui()
    )
  })
  
  output$select_ui2 <- renderUI({
    
    if (input$ui1 != input$ui2){
      start_select =  input$groupselect_method1
    } else{
      start_select =  tail(get_groups_ui(), 1)
    }
    selectInput(
      ns("groupselect_method2"),
      "Select Group",
      choices = get_groups_ui(),
      selected = start_select
    )
  })
  
  #organizing abundance data for easier referencing
  
  nodes_ratio <- reactive({
    if(group_internal_choice() == "Subtype_Immune_Model_Based") net_data$immune
    else if(group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al") net_data$subtype
    else net_data$study
  })

#Functions to build the plots
  
get_cell_image_object <- function(subtype_selected, vals_for_cellplot){
  cellimage_base <- panimmune_data$cellimage_base ## Multipart object with all information on the cell image
  cois <- get_cells_from_image(cellimage_base) ## Cells  in the image 
  gois <- get_genes_from_image(cellimage_base) ## Proteins in the image
  
  #subtype_selected <- input$groupselect_method1
  enough_data=TRUE  ## assume to start with that we can do the plot, but re-assess as we get data
  
  ### Before proceeding with plot, obtain vals_for_cellplot and ranges_for_cellplot
  
  ## vals_for_cellplot
  ### Columns are 
  ### Group: the subtype
  ### Variable: the cell or gene variable
  ### Value: the value in that subtype and for that variable, either by the averaging, or abundance ratio
  
  ## ranges_for_cellplot
  ### Group: the subtype
  ### Variable: the cell or gene variable
  ### MinBound and Maxbound: the lower and upper range of values to correspond with the color range
  
  #if (abundance == TRUE){
    #vals_for_cellplot <- nodes_ratio()
    ranges_for_cellplot <- tibble::tibble(Variable=c(cois,gois),MinBound=0,MaxBound=1)
    enough_data= subtype_selected %in% vals_for_cellplot$Group
    
  #}else {
  #   
  #   group_col <- group_internal_choice()
  #   group_df <- sample_group_df() %>% dplyr::mutate(Tumor_Fraction=1-Stromal_Fraction) %>%
  #     dplyr::mutate(Tumor_cell=Tumor_Fraction)
  #   ## values for all proteins, genes, for all subtypes
  #   all_vals_df <- generate_value_df(group_df,group_col,cois,gois)
  #   dranges <- data_ranges(all_vals_df)
  #   vals_for_cellplot <- dranges$meanz %>% dplyr::rename(Value=Mean)
  #   ranges_for_cellplot <- dranges$bounds
  #   
  #   availability <- all_vals_df %>% dplyr::group_by(Group,Variable) %>% dplyr::summarise(Count=length(Value)) %>%
  #     dplyr::group_by(Group) %>% dplyr::summarize(MinCount=min(Count))
  #   if(nrow(availability %>% dplyr::filter(Group==subtype_selected))==0){
  #     enough_data=FALSE
  #   }
  #   if (enough_data==TRUE){
  #     scount <- availability %>% dplyr::filter(Group==subtype_selected) %>% purrr::pluck("MinCount")
  #     if (scount <= 3 ){ enough_data=FALSE }
  #   }
  #   
  # }
  #shiny::validate(need((enough_data == TRUE), "Please select another subtype - this one has limited data."))
  
  ## Data ready, now alter the image object and display
  
  image_grob <- get_colored_image(subtype_selected,cellimage_base,vals_for_cellplot,ranges_for_cellplot)
  image_grob
}
  
  
##Network visualization
  
  get_network_object <- function(subtype_selected, nodes, friendly_df = friendly, positions_df = positions, scaffold = cell_scaffold){
    
    ##Edges data
    tbl_edges <- scaffold %>%
      dplyr::mutate(interaction = "interacts with")
    
    colnames(tbl_edges) <- c("source", "target", "interaction") #names required by cyjShiny package
    
    ##Nodes data
    #nodes <- nodes_ratio()
    
    nodes$Variable <- gsub("T_cells_CD8.Aggregate2", "T_cells_CD8", nodes$Variable)
    nodes$Variable <- gsub("Dendritic_cells.Aggregate1", "Dendritic_cells",  nodes$Variable)
    nodes$Variable <- gsub("Macrophage.Aggregate1", "Macrophage", nodes$Variable)
    
    nodes <- nodes %>% 
      dplyr::mutate(FriendlyName = dplyr::case_when(
        Variable %in% friendly_df$`HGNC Symbol` ~ friendly_df[Variable, 2],
        !(Variable %in% friendly_df$`HGNC Symbol`) ~ Variable
      ))
    
    #include nodes position 
    nodes <- merge(nodes, positions_df, by = "FriendlyName") #%>% dplyr::select(id, UpBinRatio, FriendlyName)
    
    tbl_nodes <- nodes %>%
      dplyr::filter(Group == subtype_selected) %>% 
      dplyr::rename(id = Variable, UpBinRatio = Value) %>% 
      dplyr::select(id, UpBinRatio, x, y, FriendlyName) %>% 
      dplyr::arrange(id)
    
    print(head(tbl_nodes))
    
    cyjShiny::dataFramesToJSON(tbl_edges, tbl_nodes)
  }

  #Organizing output depending on the option selected by the user
  
  output$plot1 <- renderUI({
    if(input$ui1 == "Image"){
      output$cellPlot1 <- renderPlot({
        image_grob <- get_cell_image_object(subtype_selected = input$groupselect_method1, vals_for_cellplot = nodes_ratio())
        grid::grid.draw(image_grob)
      })
      plotOutput(ns("cellPlot1"), height = 600) %>%
        shinycssloaders::withSpinner()
      
    } else if(input$ui1 == "Network"){
      
      output$imageNetwork1 <- cyjShiny::renderCyjShiny({
        graph.json <- get_network_object(input$groupselect_method1, nodes = nodes_ratio())
        cyjShiny::cyjShiny(graph.json, layoutName = "preset", styleFile = styleNodes)
      })
      cyjShiny::cyjShinyOutput(ns("imageNetwork1"), height = 600)%>% 
        shinycssloaders::withSpinner()
    }
  }) 
  
  output$plot2 <- renderUI({
    if(input$ui2 == "Image"){
      output$cellPlot2 <- renderPlot({
        image_grob <- get_cell_image_object(subtype_selected = input$groupselect_method2, vals_for_cellplot = nodes_ratio())
        grid::grid.draw(image_grob)
      })
      plotOutput(ns("cellPlot2"), height = 600) %>%
        shinycssloaders::withSpinner()
      
    } else if(input$ui2 == "Network"){
      
      output$imageNetwork2 <- cyjShiny::renderCyjShiny({
        graph.json <- get_network_object(input$groupselect_method2, nodes = nodes_ratio())
        cyjShiny::cyjShiny(graph.json, layoutName = "preset", styleFile = styleNodes)
      })
      cyjShiny::cyjShinyOutput(ns("imageNetwork2"), height = 600)%>% 
        shinycssloaders::withSpinner()
    }
  })   
  
}


