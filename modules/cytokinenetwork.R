if(!require(cyjShiny)){
  devtools::install_github("cytoscape/cyjShiny")
}

library(cyjShiny)


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
      p('This module uses the network of documented ligand-receptor, cell-receptor, and cell-ligand pairs published by ', 
        a(href = " https://www.nature.com/articles/ncomms8866", "Ramilowski et al., 2015"), " and retrieved from", a(href = "http://fantom.gsc.riken.jp/5/suppl/Ramilowski_et_al_2015/", "FANTOM5."))
      
    ),
   
    sectionBox(
      title = "Extracellular networks",
      
      messageBox(
        width = 24,
        includeMarkdown("data/markdown/cytokine_network.markdown")
      ),
        
        optionsBox(
          width=2,
            verticalLayout(
              #this tags$head makes sure that the checkboxes are formatted appropriately
              tags$head(
                tags$style(
                  HTML(
                          ".checkbox-inline { 
                          margin-left: 0px;
                          margin-right: 10px;
                          }
                         .checkbox-inline+.checkbox-inline {
                                    margin-left: 0px;
                                    margin-right: 10px;
                          }
                        "
                  )
                ) 
              ),
              uiOutput(ns("select_ui")),
               
              numericInput(ns("abundance"), "Set Abundance Threshold (%)", value = 66, min = 0, max = 100),
              numericInput(ns("concordance"), "Set Concordance Threshold", value = 2.94, step = 0.01),
              
              uiOutput(ns("selectCell")),
              uiOutput(ns("selectGene")),
              
              div(class = "form-group shiny-input-container", actionButton(ns("calculate_button"), tags$b("GO"), width = "100%")),
              
              hr(),
              
              fluidRow(
                column(
                  width = 12,
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
                    
                    uiOutput(ns("selectStyle")),
                  
                    uiOutput(ns("selectNode")),
                    actionButton(ns("fitSelected"), "Fit Selected", width = "100%", style = 'white-space: pre-line'),
                    actionButton(ns("fit"), "Fit Graph", width = "100%", style = 'white-space: pre-line'),
                    actionButton(ns("sfn"), "Select First Neighbor", width = "100%", style = 'white-space: pre-line'),
                    actionButton(ns("clearSelection"), "Unselect Nodes", width = "100%", style = 'white-space: pre-line'),
                    actionButton(ns("hideSelection"), "Hide Selected Nodes", width = "100%", style = 'white-space: pre-line'),
                    actionButton(ns("showAll"), "Show All Nodes", width = "100%", style = 'white-space: pre-line'),
                    #actionButton(ns("savePNGbutton"), "Save PNG"),
                    actionButton(ns("removeGraphButton"), "Remove Graph", width = "100%", style = 'white-space: pre-line') 
                )
              )
          ) #verticalLayout
        ), #optionsBox
        plotBox(
          width = 10,
          column(
            width = 11,
            mainPanel(
              width = 11,
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
        )#plotBox
      ),#sectionBox
      
      sectionBox(
        title = "Network information",
        
        messageBox(
          width = 24,
          "The tables describe the nodes and edges in the network displayed above. You can download the tables for further processing in network visualization softwares."),
      
        fluidRow(
            tableBox(
              DT::DTOutput(ns("tableNodes")) %>% 
                shinycssloaders::withSpinner(),
              downloadButton(ns('download_data_nodes'), 'Download')
          ),
            tableBox(
              DT::DTOutput(ns("table")) %>% 
                shinycssloaders::withSpinner(),
              downloadButton(ns('download_data'), 'Download')
            )
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

  output$select_ui <- renderUI({

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
        ) %>%
        dplyr::filter(!(FeatureValue %in% c("LAML", "THYM", "DLBC"))) %>%
        dplyr::arrange(FeatureValue) %>% 
        dplyr::pull(FeatureValue)
      

    }else{
      sample_group_vector <- subset_df() %>%
        dplyr::select_(group_internal_choice()) %>%
        unique()
    }
    
    
    #Generating UI depending on the sample group
    if(group_internal_choice() == "Subtype_Immune_Model_Based"){
      
      checkboxGroupInput(
        ns("showGroup"),
        "Select Immune Subtype",
        choices = sample_group_vector,
        selected = c("C1", "C2"),
        inline = TRUE)
      
    }else if(group_internal_choice() == "Subtype_Curated_Malta_Noushmehr_et_al"){
      
      selectInput(
        ns("group_selected"),
        "Choose subtype subset",
        choices = sample_group_vector)
      

    }else if(!group_internal_choice() %in% c("Subtype_Curated_Malta_Noushmehr_et_al", "Subtype_Immune_Model_Based")){
      #UI for TCGA Study and for custom groups - both allow stratification by Immune Subtype

      tagList(
        selectInput(ns("group_selected"),
                    "Choose subset",
                    choices = sample_group_vector,
                    selected = sample_group_vector[1]),

        checkboxInput(
          ns("byImmune"),
          "Stratify by Immune Subtype"),

        conditionalPanel(
          condition = paste("" , paste0("input['", ns("byImmune"), "'] == true")),
          checkboxGroupInput(
            ns("showGroup"),
            "Select Immune Subtype",
            choices=c("C1", "C2", "C3", "C4", "C5", "C6"),
            selected = c("C1", "C2"),
            inline = TRUE)
        )
      )
      
    }
  })
  
  output$selectStyle <- renderUI({
    
    styles <- c("Edges by Immune Type" = 'data/javascript/extracellular_network_stylesEdges.js',
                  "Black Edges" = "data/javascript/extracellular_network_styles.js")
      
    selectInput(
      ns("loadStyleFile"),
      "Select Style",
      choices = styles)
  })
  
  output$selectCell <- renderUI({
    selectizeInput(ns("cellInterest"), "Search and select cells of interest (optional)", choices = (panimmune_data$ext_net_labels %>% dplyr::filter(Type == "Cell") %>% dplyr::select("Cells"="Obj")), 
                   multiple = TRUE, options = list(placeholder = "Default: all cells"))
  })
  
  output$selectGene <- renderUI({
    #getting all nodes in the main_scaffold, and displaying it as FriendlyName
    scanodes <- (union(main_scaffold$From, main_scaffold$To) %>% as.data.frame() %>% 
                   unique() %>% merge(panimmune_data$ext_net_labels, by.x = ".", by.y = "Obj") %>% select(Genes = FriendlyName) %>% filter(!is.na(Genes)))
    selectizeInput(ns("geneInterest"), "Search and select genes of interest (optional)", choices = scanodes,
                   multiple = TRUE, options = list(placeholder = "Default: immunomodulator genes"))
  })
  
  output$selectNode <- renderUI({
    selectInput(ns("selectName"), "Search and select Node", choices = c("", tbl_nodes() %>% dplyr::select(Node = FriendlyName) %>% 
                                                               dplyr::filter(!is.na(Node))))
  })
  
  
 #---- organizing the desired scaffold based on the cells and genes of interest
  
  #this current solution to generate the scaffold might need to be adapted after change of database
  main_scaffold <-  panimmune_data$ext_net_df$immune$edges_score %>% filter(Group == "C1") %>% select(From, To)
  
  default_groups <- unique(panimmune_data$sample_group_df$sample_group)
  
  ##Subsetting to cells and genes of interest
  
  gois <- reactive({
    #if no gene is selected, all immunomodulator genes are considered genes of interest
    if (is.null(input$geneInterest))  return(as.vector(panimmune_data$im_direct_relationships$`HGNC Symbol`))
    
    #converting the FriendlyName to HGNC Symbol
    gois <- data.frame(FriendlyName = input$geneInterest) %>% merge(panimmune_data$ext_net_labels) %>% dplyr::select(Obj) 
  
    gois$Obj
  })

  cois <- reactive({
    #if no cell is selected, all cells are considered cells of interest 
    if (is.null(input$cellInterest)) get_cells_scaffold(main_scaffold, panimmune_data$ext_net_labels)
    
    as.vector(input$cellInterest)
  })
  
  ##Scaffold and genes based on list of cells and genes of interest 
  scaffold <- reactive({
    
    sca <- get_scaffold(panimmune_data$ext_net_labels, main_scaffold, panimmune_data$ext_net_expr, cois(), gois())
    #in case user only selected a cell of interest, get rid of the edges that only have genes
    if (is.null(input$geneInterest) & !(is.null(input$cellInterest))) {
      sca <- sca %>% 
        dplyr::filter(From %in% cois() | To %in% cois())
    }
    
    return(sca)
  }) 
    
  ##Getting list of genes and cells that are present in the selected scaffold
  cells <- reactive({
    as.vector(get_cells_scaffold(scaffold(), panimmune_data$ext_net_labels))
  })
    
  genes <- reactive({
    unique(c(scaffold()$From, scaffold()$To)) %>% setdiff(cells()) #getting all the genes in the edges selected
  })
  
  
  
#------Computing scores for a custom grouping

  ternary_info <- reactive({
    req(!group_internal_choice() %in% default_groups)
    print("calculando nodes scores")
    compute_abundance(subset_df(),
                       subset_col = group_internal_choice(),
                       panimmune_data$fmx_df,
                       panimmune_data$ext_net_expr,
                       cells(),
                       genes(),
                       stratify$byImmune)
            
})

  scaffold_scores <- reactive({
    req(!group_internal_choice() %in% default_groups, ternary_info())
    print("calculando edges scores")
    compute_concordance(scaffold(), 
                        ternary_info(), 
                        stratify$byImmune) %>%
      as.data.frame()
 
  })
  
#------ Subsetting nodes and edges list based on the Sample Group Selection and cells of interest
  
  #adjusting the flag for stratification by Immune Subtype (available only for TCGA Study and Custom Groups)
  
  stratify <- reactiveValues(byImmune = FALSE)

  observe({
    try(
      if(input$byImmune == FALSE){
        stratify$byImmune = FALSE
      }else if(input$byImmune == TRUE & group_internal_choice() %in% c("Subtype_Curated_Malta_Noushmehr_et_al", "Subtype_Immune_Model_Based")){
        stratify$byImmune = FALSE
      }else{
        stratify$byImmune = TRUE
      },
      silent = TRUE
    )
  })
  
  subset_data <- reactive({
    
    if(group_internal_choice() %in% default_groups){
      get_netdata(group_internal_choice(), panimmune_data$ext_net_df, stratify$byImmune)
    }else{
      return(NA)
    }
  })
    
  upbin_ratio <- reactive({
    
    if(group_internal_choice() %in% default_groups){
      
      subset_data()$upbin_ratio %>%
        dplyr::filter(Node %in% cells() | Node %in% genes())
        
    }else{
      ternary_info() %>% 
      dplyr::select(Node, IncludeFeature) %>% tidyr::unnest(c(IncludeFeature)) %>%
      dplyr::filter(Node %in% cells() | Node %in% genes())
    }
    
  })
  
  edges_scores <- reactive ({
   
    if(group_internal_choice() %in% default_groups){
      merge(subset_data()$edges_score, scaffold(), by.x = c("From", "To"), by.y = c("From", "To"))
    }else{
      merge(scaffold_scores(), scaffold(), by = c("From", "To"))
    }
  })

#--------------------------------------------------------------------------------
# Selection of nodes and edges based on the abundance and concordance thresholds
#--------------------------------------------------------------------------------
  subset_criteria <- reactive({
    req(group_internal_choice())
    if(group_internal_choice() == "Subtype_Immune_Model_Based"){
      input$showGroup
    }else{
      input$group_selected
    }
  })  

  abundant_nodes <- eventReactive(input$calculate_button,{
    req(upbin_ratio(), edges_scores(), subset_criteria())
    shiny::validate(need((is.numeric(input$abundance) & input$abundance <= 100 & input$abundance >= 0), 
                         "Abundance threshold input should be numeric, between 0 and 100. Please adjust it."))
    shiny::validate(need(is.numeric(input$concordance), 
                         "Concordance threshold input should be numeric, please adjust it."))
    
    get_nodes(upbin_ratio(), 
              input$abundance, 
              subset_criteria(), 
              stratify$byImmune, 
              input$showGroup)
   
  })
  
  tbl_edges <- eventReactive(input$calculate_button, {
    req(abundant_nodes(), subset_criteria())
    
    network <- get_conc_edges(edges_scores(), 
                              abundant_nodes(), 
                              input$concordance, 
                              subset_criteria(), 
                              byImmune = stratify$byImmune, 
                              immuneSubtype = input$showGroup)
    
    shiny::validate(need(nrow(as.data.frame(network)) != 0, "No network for this selection. Try changing the thresholds or selecting another subset."))
    colnames(network) <- c("source", "target", "interaction", "score") #names required by cyjShiny package
    
    network
  })
  

#------------------------------------------------------------------------------------------  
#Consolidating the nodes table and displaying the network
#------------------------------------------------------------------------------------------
  
  #Getting the nodes annotation to send to cyjShiny
  tbl_nodes <- reactive({
    req(tbl_edges())
    filterNodes(tbl_edges(), panimmune_data$ext_net_labels)
  })
  
  graph.json <- reactive({
    cyjShiny::dataFramesToJSON(tbl_edges(), tbl_nodes())
  })
  
  output$cyjShiny <- cyjShiny::renderCyjShiny({
    cyjShiny::cyjShiny(graph.json(), layoutName = input$doLayout, style_file = "data/javascript/extracellular_network_stylesEdges.js")
    })
    
  output$table <- DT::renderDataTable(
                      DT::datatable(tbl_edges(), colnames= c("From" = "source", "To" = "target", "Concordance" = "score"),
                                    caption = "Edges Table", width = "100%", rownames = FALSE)
                      )
  

  output$tableNodes <- DT::renderDataTable(
                            DT::datatable(get_ab_nodes(abundant_nodes(), tbl_edges(), panimmune_data$ext_net_labels, stratify$byImmune), 
                                          caption = "Nodes Table",  width = "100%", rownames = FALSE)
                        )
  
  output$download_data <- downloadHandler(
    filename = function() stringr::str_c("edges-", Sys.Date(), ".csv"),
    content = function(con) readr::write_csv(tbl_edges(), con)
  )
  
  output$download_data_nodes <- downloadHandler(
    filename = function() stringr::str_c("nodes-", Sys.Date(), ".csv"),
    content = function(con) readr::write_csv(get_ab_nodes(abundant_nodes(), tbl_edges(), stratify$byImmune), con)
  )

#----- Network visualization-related (from the cyjShiny examples)  
  
  observeEvent(input$loadStyleFile,  ignoreInit=TRUE, {
    if(input$loadStyleFile != ""){
      tryCatch({
        loadStyleFile(input$loadStyleFile)
      }, error=function(e) {
        msg <- sprintf("ERROR in stylesheet file '%s': %s", input$loadStyleFile, e$message)
        showNotification(msg, duration=NULL, type="error")
      })
      #later::later(function() {updateSelectInput(session, "loadStyleFile", selected=character(0))}, 0.5)
    }
  })
  
  observeEvent(input$selectName,  ignoreInit=TRUE,{
    snode <- as.character(panimmune_data$ext_net_labels[which(panimmune_data$ext_net_labels$FriendlyName == input$selectName), "Obj"])
    session$sendCustomMessage(type="selectNodes", message=list(snode))
  })
  
  observeEvent(input$sfn,  ignoreInit=TRUE,{
    session$sendCustomMessage(type="sfn", message=list())
  })
  
  observeEvent(input$fit, ignoreInit=TRUE, {
    cyjShiny::fit(session, 80)
  })
  
  observeEvent(input$fitSelected,  ignoreInit=TRUE,{
    cyjShiny::fitSelected(session, 100)
  })
  
  # observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
  #   output$selectedNodesDisplay <- renderText({" "})
  #   cyjShiny::getSelectedNodes(session)
  #   
  # })
  
  # observeEvent(input$selectedNodes, {
  #   newNodes <- input$selectedNodes;
  #   output$selectedNodesDisplay <- renderText({
  #     paste(newNodes)
  #   })
  # })
  # 
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

 #  observeEvent(input$pngData, ignoreInit=TRUE, {
 #    
 #    R.utils::printf("received pngData")
 #    png.parsed <- jsonlite::fromJSON(input$pngData)
 #    substr(png.parsed, 1, 30) # [1] "data:image/png;base64,iVBORw0K"
 #    nchar(png.parsed)  # [1] 768714
 #    png.parsed.headless <- substr(png.parsed, 23, nchar(png.parsed))  # chop off the uri header
 #    png.parsed.binary <- base64::base64decode(png.parsed.headless)
 #    R.utils::printf("writing png to foo.png")
 #    conn <- file("savedNetwork.png", "wb")
 #    writeBin(png.parsed.binary, conn)
 #    close(conn)
 # })


}  
