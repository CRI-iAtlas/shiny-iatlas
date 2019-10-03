library(cyjShiny)

#-------------------------------------------------------------------
#TEMPORARY - reading in files - ATTENTION: columns cannot be factors 
#-------------------------------------------------------------------

# df_ternary_info <- readRDS("data/network/testRDS.rds")
ternary <- readRDS("data/network/ternary.rds")

upbin_ratio <- read_csv("data/network/upbinratio.csv")
edges_scores <- read_csv("data/network/edgesScore.csv")

styles <- c("Edges by Immune Type" = 'data/network/stylesEdges.js',
            "Black Edges" = "data/network/styles.js")
node_type <- read_tsv("data/network/network_node_labels.tsv")

filterNodes <- function(list_edges, annot){
  colnames(annot) <- c("Type", "name")
  annot[annot$name== "CD80"& annot$Type == "Ligand",] <- NA #hardcoded here the fact that CD80 is with two different annotations and this caused a problem in the JSON file
  nodes <- append(list_edges$source, list_edges$target) %>% unique() %>% as.data.frame()
  colnames(nodes) <- "name"

  nodes <- merge(nodes, annot, all.x = TRUE) #we want to keep all nodes, even those that do not have annotations
  
  return(nodes)
}



cytokinenetwork_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Cytokine Network"),
    textBox(
      width = 12,
      p(stringr::str_c(
        "Explore the cytokine network for each immune subtype",
        sep = " "
      ))  
    ),
    
    sectionBox(
      title = "Here we go",
      
      messageBox(
        width = 24,
        p("Here is what you must do.")
      ),
      
      fluidRow(
        optionsBox(
          column(
            width = 4,
            selectInput(
              ns("showGroup"), 
              "Select Immune Subtype:", 
              choices=c("All", "C1", "C2", "C3", "C4", "C5", "C6"), 
              selected = "All")
          ),
          column(
            width = 4,
            selectInput(
              ns("doLayout"), 
              "Select Layout:",
              choices=c("",
                        "cose",
                        "cola",
                        "circle",
                        "concentric",
                        "breadthfirst",
                        "grid",
                        "random",
                        "dagre",
                        "cose-bilkent"),
              selected = "dagre")
          ),
         column(
           width = 4,
           selectInput(
             ns("loadStyleFile"), 
             "Select Style: ", 
             choices = styles)
         ),
         column(
           width = 4,
           selectInput(ns("selectName"), "Select Node by ID:", choices = c("", node_type$Obj))
           ),
        fluidRow(
          actionButton(ns("fit"), "Fit Graph"),
          actionButton(ns("fitSelected"), "Fit Selected"),
          actionButton(ns("sfn"), "Select First Neighbor"),
          actionButton(ns("clearSelection"), "Unselect Nodes"),
         # actionButton(ns("loopConditions"), "Loop Conditions"),
          actionButton(ns("getSelectedNodes"), "Get Selected Nodes"),
          actionButton(ns("removeGraphButton"), "Remove Graph")
        ),
        column(
          width = 6,
           numericInput(ns("abundance"), "Set Abundance Threshold (%)", value = 66, min = 0, max = 100),
           numericInput(ns("concordance"), "Set Concordance Threshold", value = 2.94, step = 0.01) 
          ),
        column(
          width = 6,
          actionButton(ns("calculate_button"), "Calculate")
        )
        
        ),
      
        mainPanel(
          width = 12,
          fluidRow(
            cyjShinyOutput(ns("cyjShiny"), height = 700)%>% 
              shinycssloaders::withSpinner(),
            
            tableBox(
              DT::DTOutput(ns("table")) %>% 
                shinycssloaders::withSpinner()
            ),
          downloadButton(ns('download_data'), 'Download')
          )
        )
      )
    )
  ) 
}

cytokinenetwork <- function(
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
  
  #Loading data (to be changed)
  ## Read data file containing group assignments - these will eventually come from the iAtlas app user choices
  # group_df=read_tsv("data/network/PanImmune_FMx_ImmuneSubtypes.tsv")
  # group_col="Subtype_Immune_Model_Based"
  # 
  # ## Read available gene expression data 
  dfe_in <- read_tsv("data/network/GenExp_All_CKine_genes.tsv")
  
  ## Read file with available cell data
  #dfc_in <- panimmune_data$fmx_df #read_tsv("data/network/PanImmune_FMx.tsv") #is it the same as panimmune_data$fmx_df?
  
  ## Read scaffold interaction file and node type file
  #node_type <- read_tsv("data/network/network_node_labels.tsv")
  scaffold <- read_tsv("data/network/try_3a.tsv")
  
  ##Subsetting to genes of interest
  gois <- read_lines("data/network/immunomodulator_genes.txt")
  gois <- c(gois, "CXCL10")
  
  #Scaffold and genes based on list of genes of interest (assuming all cells are of interest)
  scaffold <- get_scaffold(node_type, scaffold, dfe_in, gois)
  
  #Getting list of genes and cells that are present in the selected scaffold #REACTIVE: selection of cells/genes of interest
  cells <- get_cells_scaffold(scaffold, node_type)
  genes <- unique(c(scaffold$From, scaffold$To)) %>% setdiff(cells) #getting all the genes in the edges selected
  
  ##Subsetting ternary info to genes of interest
  
  # df_ternary_info <- df_ternary_info %>% 
  #                     filter(Node %in% cells | Node %in% genes)
  
  upbin_ratio <- upbin_ratio %>% 
                      filter(Node %in% cells | Node %in% genes)
  
  edges_scores <- merge(edges_scores, scaffold, by.x = c("From", "To"), by.y = c("From", "To") )
      
  ternary <- ternary[names(ternary) %in% cells | names(ternary) %in% genes]
  
  # #preparing data to compute the quantiles
  # group_participant <- get_participants(group_df, group_col)
  # 
  # dfclong.generic <- get_cell_long(dfc_in, group_participant, cells)
  # dfelong.generic <- get_gene_long(dfe_in, group_participant, genes)
  # 
  # dfn <- dplyr::bind_rows(dfelong.generic, dfclong.generic)
  
  ternary_info <- eventReactive(input$calculate_button,{
    #get_abundant_nodes(df_ternary_info, ab_threshold = input$abundance/100)
    nodes <- upbin_ratio %>%
      mutate(IncludeFeature=map_lgl(.x = UpBinRatio, upbinfrac.threshold = (input$abundance/100), keep.node)) %>% 
      filter(IncludeFeature == TRUE)
    
    nodes
  })
  
  predicted_network <- eventReactive(input$calculate_button, {
    #get_network(scaffold, ternary_info(), ternary, conc_threshold = input$concordance) %>% as.data.frame()
    network <- merge(edges_scores, ternary_info(), by.x = c("From", "Group"), by.y = c("Node", "Group")) %>% #filtering nodes that are abundant
      merge(ternary_info(), by.x = c("To", "Group"), by.y = c("Node", "Group")) %>% 
      filter(ratioScore > input$concordance) %>% #filtering concordant edges 
      select(From, To, Group, ratioScore) %>% 
      as.data.frame()
    
    colnames(network) <- c("source", "target", "interaction", "score") #names required by cyjShiny package
    network
  })
  

#--------------------------------------------------------------  
#Consolidating the edges table with selection of Immune Subtype
#--------------------------------------------------------------
  
  tbl_edges <- reactive({

    if(input$showGroup == "All" | is.null(input$showGroup)) return(predicted_network())

    predicted_network() %>%
      dplyr::filter(interaction == input$showGroup)
  })

  
  tbl_nodes <- reactive({
    filterNodes(tbl_edges(), node_type)
  })

  
  graph.json.v2 <- reactive({
    cyjShiny::dataFramesToJSON(tbl_edges(), tbl_nodes())
  })
  
  output$cyjShiny <- renderCyjShiny({
    cyjShiny(graph.json.v2(), layoutName = input$doLayout, style_file = "data/network/stylesEdges.js")
    })
  
  #output$legend <- renderImage("www/images/legend_V1.png")
  
  output$table = DT::renderDataTable(tbl_edges(),
                                     width="100%",
                                     options = list(
                                       autoWidth = TRUE
                                     ))
  

  
  #tried to use the data_table_module, but it didn't work
  #callModule(data_table_module, "table", data_df = tbl.edges())
  
  output$download_data <- downloadHandler(
    filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
    content = function(con) readr::write_csv(tbl_edges(), con)
  )

  
  #---------------------------------  
  # Network visualization -related 
  #----------------------------------
  observeEvent(input$loadStyleFile,  ignoreInit=TRUE, {
    if(input$loadStyleFile != ""){
      tryCatch({
        loadStyleFile(input$loadStyleFile)
      }, error=function(e) {
        msg <- sprintf("ERROR in stylesheet file '%s': %s", input$loadStyleFile, e$message)
        showNotification(msg, duration=NULL, type="error")
      })
      later::later(function() {updateSelectInput(session, "loadStyleFile", selected=character(0))}, 0.5)
    }
  })
  
  observeEvent(input$selectName,  ignoreInit=TRUE,{
    session$sendCustomMessage(type="selectNodes", message=list(input$selectName))
  })
  
  observeEvent(input$sfn,  ignoreInit=TRUE,{
    session$sendCustomMessage(type="sfn", message=list())
  })
  
  observeEvent(input$fit, ignoreInit=TRUE, {
    fit(session, 80)
  })
  
  observeEvent(input$fitSelected,  ignoreInit=TRUE,{
    cyjShiny::fitSelected(session, 100)
  })
  
  observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
    output$selectedNodesDisplay <- renderText({" "})
    cyjShiny::getSelectedNodes(session)
    
  })
  
  observeEvent(input$selectedNodes, {
    newNodes <- input$selectedNodes;
    output$selectedNodesDisplay <- renderText({
      paste(newNodes)
    })
  })
  
  observeEvent(input$clearSelection,  ignoreInit=TRUE, {
    session$sendCustomMessage(type="clearSelection", message=list())
  })
  
  observeEvent(input$removeGraphButton, ignoreInit=TRUE, {
    removeGraph(session)
  })  

}  
