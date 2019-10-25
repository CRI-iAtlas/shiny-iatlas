if(!require(cyjShiny)){
  githubinstall::githubinstall(packages = "cytoscape/cyjShiny", ask = F)
}

library(cyjShiny)
#-------------------------------------------------------------------
#TEMPORARY - reading in files 
#-------------------------------------------------------------------

## Loading all data of nodes abundance and edges scores
all_net_info <- list(
  "immune"= list("upbin_ratio" = readr::read_csv("data/network/all_nodes_Immune_noNegative.csv"), edges_score = readr::read_csv("data/network/all_edges_Immune_noNegative.csv")),
  #"immune"= list("upbin_ratio" = readr::read_csv("data/network/upbinratio.csv"), edges_score = readr::read_csv("data/network/edgesScore.csv")),
  "subtype"= list("upbin_ratio" = readr::read_csv("data/network/nodes_TCGASubtype.csv"), edges_score = readr::read_csv("data/network/edges_TCGASubtype.csv")),
  "study"= list("upbin_ratio" = readr::read_csv("data/network/nodes_TCGAStudy.csv"), edges_score = readr::read_csv("data/network/edges_TCGAStudy.csv")),
  "studyImmune" = list("upbin_ratio" = readr::read_csv("data/network/nodes_TCGAStudy_Immune.csv"), edges_score = readr::read_csv("data/network/edges_TCGAStudy_Immune.csv"))
)

dfe_in <- readr::read_tsv("data/network/GenExp_All_CKine_genes.tsv")
main_scaffold <- readr::read_tsv("data/network/try_3a.tsv")
#immunogenes <- readr::read_lines("data/network/immunomodulator_genes.txt")
styles <- c("Edges by Immune Type" = 'data/network/stylesEdges.js',
            "Black Edges" = "data/network/styles.js")
node_type <- readr::read_tsv("data/network/network_node_labels.tsv")



cytokinenetwork_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — Extracellular Networks"),
    textBox(
      width = 12,
      p(stringr::str_c(
        "Explore the extracellular networks modulating tumoral immune response, encompassing direct interaction among cells and communication via soluble proteins such as cytokines to mediate interactions among those cells.",
        sep = " "
      )),
      p('This module uses the network of documented ligand-receptor, cell-receptor, and cell-ligand pairs retrieved from FANTOM5, published by ', a(href = "http://fantom.gsc.riken.jp/5/suppl/Ramilowski_et_al_2015/", "Ramilowski et al., 2015."))
      
    ),
    
    sectionBox(
      title = "Extracellular networks",
      
      messageBox(
        width = 24,
        p("Select the subset of interest of analysis, depending on the Sample Group selected. The other two parameters required to build a network are:"),
        tags$li("Abundance threshold (%): Nodes in the network are selected if they meet the selected abundance threshold. The abundance threshold, in %, is the frequency of samples 
                in the upper two tertiles of cell abundance or gene expression distributions. For example, for an abundance of 66%, a node is entered into the subtype-network if at least 66% of samples within a subtype map to mid or high value bins in a tertile distribution."),
        p(""),
        tags$li("Concordance threshold: Edges in the scaffold network between any two abundant nodes is evaluated for inclusion based on the selected concordance threshold. A concordance 
                threshold of 2, for example, selects edges that have both nodes simultaneously highly or lowly expressed at least twice as frequent than samples with one node highly expressed and the other lowly expressed."), 
        p(""),
        p("For more information on the method to compute these thresholds, refer to the Data Description module."),
        p("In addition, a user can also select a set of cells or genes of interest. This selection will limit the network to the edges in the scaffold with the selected cells and genes, and then check if they meet the abundance and concordance thresholds. By default, a network will use all cells and the immunomodulator genes (as described in the Immunomodulators module)."),
        p("Manuscript context:  This module allows you to display networks similar to Figure 7A, 7B, 7C, and S7A. 
          If you are looking at Immune Subtypes, select Immune Subtype C4, 66% abundance threshold, and 1.62 for concordance score to get the extracellular network for C4 present in Figure S7A.")
      ),
        
        optionsBox(
          width=2,
            verticalLayout(
              
              conditionalPanel( 
                condition = "input.ss_choice == 'TCGA Study'",
                uiOutput(ns("showStudy"),
                         ),
                checkboxInput(
                ns("byImmune"),
                "Stratify by Immune Subtype")
              ),
              conditionalPanel( 
                condition = "input.ss_choice == 'TCGA Subtype'",
                uiOutput(ns("showSubtype"))
              ),
              conditionalPanel(
                condition = paste("input.ss_choice == 'Immune Subtype' || (input.ss_choice == 'TCGA Study' &" , paste0("input['", ns("byImmune"), "'] == true)")),
                selectizeInput(
                  ns("showGroup"), 
                  "Select Immune Subtype", 
                  choices=c( "C1", "C2", "C3", "C4", "C5", "C6"), 
                  selected = c("C1", "C2"),
                  multiple = TRUE)
              ),
              numericInput(ns("abundance"), "Set Abundance Threshold (%)", value = 66, min = 0, max = 100),
              numericInput(ns("concordance"), "Set Concordance Threshold", value = 2.94, step = 0.01),
              
              selectizeInput(ns("cellInterest"), "Select cells of interest (optional)", choices = (node_type %>% dplyr::filter(Type == "Cell") %>% select("Obj")), 
                             multiple = TRUE, options = list(placeholder = "Default: all cells")),
              selectizeInput(ns("geneInterest"), "Select genes of interest (optional)", choices = (union(main_scaffold$From, main_scaffold$To) %>% as.data.frame() %>% unique() %>% dplyr::arrange()), 
                             multiple = TRUE, options = list(placeholder = "Default: immunomodulator genes")),
              div(class = "form-group shiny-input-container", actionButton(ns("calculate_button"), tags$b("GO"), width = "100%")),
              hr(),
              selectInput(
                ns("doLayout"), 
                "Select Layout",
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
                selected = "cose"),
              # selectInput(
              #   ns("loadStyleFile"), 
              #   "Select Style", 
              #   choices = styles),
              
              uiOutput(ns("selectNode")),
              actionButton(ns("fit"), "Fit Graph"),
              actionButton(ns("fitSelected"), "Fit Selected"),
              actionButton(ns("sfn"), "Select First Neighbor"),
              actionButton(ns("clearSelection"), "Unselect Nodes"),
              actionButton(ns("hideSelection"), "Hide Selected Nodes"),
              actionButton(ns("showAll"), "Show All Nodes"),
              # actionButton(ns("loopConditions"), "Loop Conditions"),
              #actionButton(ns("getSelectedNodes"), "Get Selected Nodes"),
              actionButton(ns("removeGraphButton"), "Remove Graph"),
              actionButton(ns("savePNGbutton"), "Save PNG")
          )
        ),
        
        column(
          width = 10,
          mainPanel(
            width = 10,
            fluidRow(
              cyjShiny::cyjShinyOutput(ns("cyjShiny"), height =1000)%>% 
                shinycssloaders::withSpinner()
            )
        ),
        column(
          width = 1,
          img(src = "images/network_legend.png", width = 720)
        )
        )
      ),
      
      sectionBox(
        title = "Network information",
        
        messageBox(
          width = 24,
          "Here it goes the table with edges."),
        
        column(
          width = 12,
          tableBox(
            DT::DTOutput(ns("table")) %>% 
              shinycssloaders::withSpinner()
          ), 
          downloadButton(ns('download_data'), 'Download')
        )
      )
  
)#taglist
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

  
#--------UIs
  
  output$showStudy <- renderUI({
    choices <- panimmune_data$sample_group_df %>% 
      dplyr::filter(sample_group == "Study"& !(FeatureValue %in% c("LAML", "THYM", "DLBC"))) %>% 
      dplyr::select("FeatureValue") %>% 
      dplyr::distinct() %>% 
      dplyr::arrange(FeatureValue)
    
    selectInput(ns("study_selection"), 
                "Choose tumor type:",
                choices = choices,
                selected = choices[1])
  })
  
  output$showSubtype <- renderUI({
    #req(input$sample_mosaic_group, panimmune_data$sample_group_df, cancelOutput = TRUE)
     
      choices <- panimmune_data$sample_group_df %>% 
        dplyr::filter(sample_group == "Subtype_Curated_Malta_Noushmehr_et_al" & `TCGA Studies` == study_subset_choice()) %>% 
        dplyr::select("FeatureValue") %>% 
        dplyr::distinct()
      
        selectInput(ns("study_subset_selection"), 
                    "Choose subtype subset:",
                    choices = choices,
                    selected = choices[1])
  })
  
  output$selectNode <- renderUI({
    selectInput(ns("selectName"), "Select Node:", choices = c("", tbl_nodes() %>% dplyr::select(name)))
  })
  
  
 #---- organizing the desired scaffold based on the cells and genes of interest
  
  immunogenes <- panimmune_data$im_direct_relationships$`HGNC Symbol`
 
  ## Read scaffold interaction file
  #main_scaffold <- readr::read_tsv("data/network/try_3a.tsv")
  
  ##Subsetting to cells and genes of interest
  
  gois <- reactive({
    if (is.null(input$geneInterest))  return(as.vector(immunogenes))
    
    as.vector(input$geneInterest)
  })

  cois <- reactive({
    if (is.null(input$cellInterest)) get_cells_scaffold(main_scaffold, node_type)
    
    as.vector(input$cellInterest)
  })
  
  ##Scaffold and genes based on list of cells and genes of interest 
  scaffold <- reactive({
    sca <- get_scaffold(node_type, main_scaffold, dfe_in, cois(), gois())
    #in case user only selected a cell of interest, get rid of the edges that have only genes
    if (is.null(input$geneInterest) & !(is.null(input$cellInterest))) {
      sca <- sca %>% 
        dplyr::filter(From %in% cois() | To %in% cois())
    }
    
    return(sca)
  }) 
    
  ##Getting list of genes and cells that are present in the selected scaffold
  cells <- reactive({
    as.vector(get_cells_scaffold(scaffold(), node_type))
  })
    
  genes <- reactive({
    unique(c(scaffold()$From, scaffold()$To)) %>% setdiff(cells()) #getting all the genes in the edges selected
  })
  
#------ Subsetting nodes and edges list based on the Sample Group Selection and cells of interest
  
  subset_data <- reactive({
    get_netdata(group_internal_choice(), all_net_info, input$byImmune)
  })
    
  upbin_ratio <- reactive({
    subset_data()$upbin_ratio %>%
      dplyr::filter(Node %in% cells() | Node %in% genes())
  })
  
  edges_scores <- reactive ({
    merge(subset_data()$edges_score, scaffold(), by.x = c("From", "To"), by.y = c("From", "To"))
  })

#--------------------------------------------------------------------------------
# Selection of nodes and edges based on the abundance and concordance threshold
#--------------------------------------------------------------------------------
  subset_criteria <- reactive({
    if(group_internal_choice() == "Subtype_Immune_Model_Based"){
      #if(input$showGroup == "All") return(c("C1","C2", "C3", "C4", "C5", "C6"))
      input$showGroup
    }else if(group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al"){
      input$study_subset_selection
    }else{
      input$study_selection
    }
    
  })  

  abundant_nodes <- eventReactive(input$calculate_button,{
   
    if(input$byImmune == FALSE){
      nodes <- upbin_ratio() %>%
        dplyr::filter(Group %in% subset_criteria()) %>% 
        dplyr::mutate(IncludeFeature = purrr::map_lgl(.x = UpBinRatio, upbinfrac.threshold = (input$abundance/100), keep.node)) %>% 
        dplyr::filter(IncludeFeature == TRUE)
    }else{
      nodes <- upbin_ratio() %>%
        dplyr::filter(Group %in% subset_criteria() & Immune %in% input$showGroup) %>% 
        dplyr::mutate(IncludeFeature = purrr::map_lgl(.x = UpBinRatio, upbinfrac.threshold = (input$abundance/100), keep.node)) %>% 
        dplyr::filter(IncludeFeature == TRUE)
    }
    
    nodes
  })
  
  tbl_edges <- eventReactive(input$calculate_button, {
    
    if(input$byImmune == FALSE){
     
       network <- edges_scores() %>% 
        dplyr::filter(Group %in% subset_criteria()) %>% 
        merge(abundant_nodes(), by.x = c("From", "Group"), by.y = c("Node", "Group")) %>% #filtering nodes that are abundant
        merge(abundant_nodes(), by.x = c("To", "Group"), by.y = c("Node", "Group")) %>% #filtering nodes that are abundant
        dplyr::filter(ratioScore > input$concordance) %>% #filtering concordant edges 
        dplyr::select(From, To, Group, ratioScore) %>% 
        as.data.frame()
      
    }else if(input$byImmune == TRUE){
      
      network <- edges_scores() %>% 
        dplyr::filter(Group %in% subset_criteria() & Immune %in% input$showGroup) %>% 
        merge(abundant_nodes(), by.x = c("From", "Group", "Immune"), by.y = c("Node", "Group", "Immune")) %>% #filtering nodes that are abundant
        merge(abundant_nodes(), by.x = c("To", "Group", "Immune"), by.y = c("Node", "Group", "Immune")) %>% #filtering nodes that are abundant
        dplyr::filter(ratioScore > input$concordance) %>% #filtering concordant edges 
        dplyr::select(From, To, Immune, ratioScore) %>% 
        as.data.frame() 
    }
    
    shiny::validate(need(nrow(as.data.frame(network)) != 0, "No network for this selection. Try changing the thresholds or selecting another subset."))
    colnames(network) <- c("source", "target", "interaction", "score") #names required by cyjShiny package
    
    network
  })
  

#------------------------------------------------------------------------------------------  
#Consolidating the edges table with selection of Immune Subtype and displaying the network
#------------------------------------------------------------------------------------------
  
  # tbl_edges <- reactive({
  # 
  #   if(input$showGroup == "All" | is.null(input$showGroup)) return(predicted_network())
  # 
  #   network <- predicted_network() %>%
  #     dplyr::filter(interaction == input$showGroup)
  #   
  #   shiny::validate(need(nrow(as.data.frame(network)) != 0, "No network for this Immune Subtype. Try changing the thresholds or selecting another subtype."))
  #   network
  # })

  #Getting the nodes annotation
  tbl_nodes <- reactive({
    nodes <- filterNodes(tbl_edges(), node_type)
    
  })

  
  graph.json.v2 <- reactive({
    cyjShiny::dataFramesToJSON(tbl_edges(), tbl_nodes())
  })
  
  output$cyjShiny <- cyjShiny::renderCyjShiny({
    cyjShiny::cyjShiny(graph.json.v2(), layoutName = input$doLayout, style_file = "data/network/stylesEdges.js")
    })
  
  # output$table = reactive({
  #   print(colnames(tbl_edges()))
  #     tbl_edges() %>% 
  #     DT::formatRound('score', digits=3) %>% 
  #     DT::renderDataTable(
  #       width="100%",
  #       options = list(
  #         autoWidth = TRUE
  #       ))
  # })
    
  
  output$table <- DT::renderDataTable(
                      tbl_edges(),
                      width="100%",
                      options = list(
                          autoWidth = TRUE)
                      )
  
  
  output$download_data <- downloadHandler(
    filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
    content = function(con) readr::write_csv(tbl_edges(), con)
  )

  
  #---------------------------------  
  # Network visualization-related 
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
  
  observeEvent(input$hideSelection,  ignoreInit=TRUE, {
    session$sendCustomMessage(type="hideSelection", message=list())
  })
  
  observeEvent(input$showAll,  ignoreInit=TRUE, {
    session$sendCustomMessage(type="showAll", message=list())
  })
  
  observeEvent(input$clearSelection,  ignoreInit=TRUE, {
    session$sendCustomMessage(type="clearSelection", message=list())
  })
  
  observeEvent(input$removeGraphButton, ignoreInit=TRUE, {
    removeGraph(session)
  })
  
  observeEvent(input$savePNGbutton, ignoreInit=TRUE, {
    file.name <- tempfile(fileext=".png")
    savePNGtoFile(session, file.name)

  })

  observeEvent(input$pngData, ignoreInit=TRUE, {
    
    R.utils::printf("received pngData")
#    ns_png <- NS(id)
#    pngData_ns <- ns_png(pngData)
    png.parsed <- jsonlite::fromJSON(input$pngData)
    substr(png.parsed, 1, 30) # [1] "data:image/png;base64,iVBORw0K"
    nchar(png.parsed)  # [1] 768714
    png.parsed.headless <- substr(png.parsed, 23, nchar(png.parsed))  # chop off the uri header
    png.parsed.binary <- base64::base64decode(png.parsed.headless)
    R.utils::printf("writing png to foo.png")
    conn <- file("savedNetwork.png", "wb")
    writeBin(png.parsed.binary, conn)
    close(conn)
 })

  # input$savePNGbutton <- downloadHandler(
  #   filename = function() stringr::str_c("network-", Sys.Date(), ".png"),
  #   cyjShiny::savePNGtoFile(session, file.name)
  # )

}  
