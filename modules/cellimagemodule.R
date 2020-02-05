#Loading data for the network visualization
cell_scaffold <- readr::read_tsv("data/network/scaffold_network_cellimage.tsv") %>% as.data.frame()

net_data = list(
  "immune"= feather::read_feather("data/network/node_cellimage_immune.feather"),
  "subtype"= feather::read_feather("data/network/node_cellimage_subtype.feather"),
  "study"= feather::read_feather("data/network/node_cellimage_study.feather")
)
      
styleNodes = "data/javascript/style_network_cellimage.js"


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
            uiOutput(ns("select_ui"))
          )
        )
        ),
        fluidRow(
          plotBox(
            width = 5,
            plotOutput(ns("cellPlot"), height = 600) %>%
              shinycssloaders::withSpinner()
          ),
          plotBox(
            width = 7,
            cyjShiny::cyjShinyOutput(ns("image_network"), height = 600)%>% 
              shinycssloaders::withSpinner()
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
  
  output$select_ui <- renderUI({
    
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
    
    
    selectInput(
      ns("groupselect_method"),
      "Select Group",
      choices = sample_group_vector
    )
    
  })
  
  #organizing data to network visualization
  
  nodes_ratio <- reactive({
    if(group_internal_choice() == "Subtype_Immune_Model_Based") net_data$immune
    else if(group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al") net_data$subtype
    else net_data$study
  })

  output$cellPlot <- renderPlot({
    req(nodes_ratio())

    cellimage_base <- panimmune_data$cellimage_base ## Multipart object with all information on the cell image
    cois <- get_cells_from_image(cellimage_base) ## Cells  in the image 
    gois <- get_genes_from_image(cellimage_base) ## Proteins in the image
    
    subtype_selected <- input$groupselect_method
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
        
    if (abundance == TRUE){
      vals_for_cellplot <- nodes_ratio()
      ranges_for_cellplot <- tibble::tibble(Variable=c(cois,gois),MinBound=0,MaxBound=1)
      enough_data= subtype_selected %in% vals_for_cellplot$Group
    
    }else {
    
      group_col <- group_internal_choice()
      group_df <- sample_group_df() %>% dplyr::mutate(Tumor_Fraction=1-Stromal_Fraction) %>% 
        dplyr::mutate(Tumor_cell=Tumor_Fraction)
      ## values for all proteins, genes, for all subtypes
      all_vals_df <- generate_value_df(group_df,group_col,cois,gois)
      dranges <- data_ranges(all_vals_df)
      vals_for_cellplot <- dranges$meanz %>% dplyr::rename(Value=Mean)
      ranges_for_cellplot <- dranges$bounds
      
      availability <- all_vals_df %>% dplyr::group_by(Group,Variable) %>% dplyr::summarise(Count=length(Value)) %>% 
        dplyr::group_by(Group) %>% dplyr::summarize(MinCount=min(Count))
      if(nrow(availability %>% dplyr::filter(Group==subtype_selected))==0){
        enough_data=FALSE
      }
      if (enough_data==TRUE){
        scount <- availability %>% dplyr::filter(Group==subtype_selected) %>% purrr::pluck("MinCount")
        if (scount <= 3 ){ enough_data=FALSE }
      }
      
    }
    shiny::validate(need((enough_data == TRUE), "Please select another subtype - this one has limited data."))
    
    ## Data ready, now alter the image object and display
    
    image_grob <- get_colored_image(subtype_selected,cellimage_base,vals_for_cellplot,ranges_for_cellplot)
    grid::grid.draw(image_grob)
    
  })
  
##Network visualization
  
  tbl_edges <- reactive({ #doesn't need to be reactive, actually
    
    network <- cell_scaffold %>%
      dplyr::mutate(interaction = "interacts with")
    
    colnames(network) <- c("source", "target", "interaction") #names required by cyjShiny package
    
    network
  })
  
  ##Getting the nodes annotation to send to cyjShiny
  tbl_nodes <- reactive({
    req(nodes_ratio(), tbl_edges())
    
    nodes <- nodes_ratio()
    
    nodes$Variable <- gsub("T_cells_CD8.Aggregate2", "T_cells_CD8", nodes$Variable)
    nodes$Variable <- gsub("Dendritic_cells.Aggregate1", "Dendritic_cells",  nodes$Variable)
    nodes$Variable <- gsub("Macrophage.Aggregate1", "Macrophage", nodes$Variable)
    
    nodes %>%
      filter(Group == input$groupselect_method) %>% 
      rename(id = Variable, UpBinRatio = Value) %>% 
      select(id, UpBinRatio) %>% 
      arrange(id)
    
  })
  
  graph.json <- reactive({
    cyjShiny::dataFramesToJSON(tbl_edges(), tbl_nodes())
  })
  
  output$image_network <- cyjShiny::renderCyjShiny({
    cyjShiny::cyjShiny(graph.json(), layoutName = "cose", styleFile = styleNodes)
  })
    
}
