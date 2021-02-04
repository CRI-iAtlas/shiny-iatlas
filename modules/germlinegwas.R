germline_gwas_ui <- function(id){
  
  ns <- shiny::NS(id)
  shiny::tagList(
    messageBox(
      width = 12,
      shiny::p("GWAS were performed on 33 immune traits that demonstrated nominally significant heritability (p < 0.05) in at least one ancestry group.  Here you can visualize the significant (p < 10-6) GWAS hits."),
      shiny::p("Select Immune Features of interest to highlight or select the GWAS hits associated with this trait. You can also choose to exclude GWAS hits associated with a selected immune feature."),
      shiny::p("If you select the 'Select a region' option, you will be able to select a region of interest and see the genomic region on a IGV plot."),
      shiny::p("More information on a particular SNP can be obtained by clicking on the manhattan plot, or by searching on the dropdown menu on the right."),
      shiny::p("Manuscript context: Figure 3A is reproduced with the 'See all chromosomes' option. To generate Figure 4B, change the range of visualization to 'Select a region', Chromosome 2 and then select the coordinates by zooming in the plot or by manually updating the start and end of the region of interest."),
      shiny::actionLink(ns("method_link_gwas"), "Click to view method description.")
      ),
    shiny::column(
      width = 9,
      optionsBox(
        width = 12,
        shiny::column(
          width = 3,
          shiny::radioButtons(ns("selection"), "Select range of visualization", choices = c("See all chromosomes", "Select a region"), selected = "See all chromosomes")
        ),
        shiny::column(
          width = 5,
          shiny::uiOutput(ns("features")),
          shiny::checkboxInput(ns("only_selected"), "Display only selected feature(s)")
        ),
        shiny::column(
          width = 4,
          shiny:: conditionalPanel(
            condition = paste("" , paste0("input['", ns("only_selected"), "'] == false")),
            shiny::uiOutput(ns("to_exclude"))
          )
        )
      ),
      plotBox(
        width = 12,
        plotly::plotlyOutput(ns("mht_plot"), height = "300px") %>%
          shinycssloaders::withSpinner(.),
        shiny::conditionalPanel(paste0("input['", ns("selection"), "'] == 'Select a region'"),
                                igvShiny::igvShinyOutput(ns('igv_plot')) %>%
                                  shinycssloaders::withSpinner(.)
        )
      )
    ),
    shiny::column(
      width = 3,
      shiny::verticalLayout(
        optionsBox(
          width = 12,
          shiny::uiOutput(ns("search_snp"))
        ),
        messageBox(
          width = 12,
          shiny::uiOutput(ns("links"))
        ),
        tableBox(
          width = 12,
          DT::DTOutput(ns("snp_tbl"))
        )
      )
    ),
    messageBox(
      width = 12,
      shiny::p("We conducted a GWAS paired with colocalization analyses, with eQTL and sQTL analyses performed in TCGA and GTEx. 
               Results with a Colocalization Posterior Probability (CLPP) > 0.01 are summarized in the tables below."), 
      shiny::p("Click on a table row to visualize the plot. The table is updated with available plots in the region displayed at the manhattan plot."),
      shiny::actionLink(ns("method_link_colocalization"), "Click to view method description.")
    ),
    column(
      width = 6,
      tableBox(
        width = 12,
        div(DT::DTOutput(ns("colocalization_tcga")) %>%
              shinycssloaders::withSpinner(.), 
            style = "font-size: 75%"),
        shiny::uiOutput(ns("tcga_colocalization_plot"))
      )
    ),
    column(
      width = 6,
      tableBox(
        width = 12,
        div(DT::DTOutput(ns("colocalization_gtex")) %>%
              shinycssloaders::withSpinner(.), 
            style = "font-size: 75%"),
        shiny::uiOutput(ns("gtex_colocalization_plot"))
      )
    )
  )
}

germline_gwas_server <- function(input, output, session) {
      
      ns <- session$ns
      
      colocalization_plot <- reactive({ #get files with the selected SNP
        feather::read_feather("data/germline/colocalization_TCGA_df.feather")
      })
      
      gtex_coloc <- reactive({
        feather::read_feather("data/germline/colocalization_GTEX_df.feather")
      })
      
      immune_feat <- reactive({
        
        germline_data$gwas %>%
          dplyr::select(display,`Annot.Figure.ImmuneCategory`) %>%
          dplyr::group_by(`Annot.Figure.ImmuneCategory`) %>%
          tidyr::nest(data = c(display))%>%
          dplyr::mutate(data = purrr::map(data, tibble::deframe)) %>%
          tibble::deframe()
        
      })
      
      output$features <- renderUI({
        shiny::selectizeInput(ns('immunefeature'), "Select Immune Feature(s)",
                              choices = immune_feat(),
                              selected = c("CD8 T cells", "NK cells"),
                              multiple = TRUE)
      })
      
      output$to_exclude <- renderUI({
        shiny::selectizeInput(ns('exclude_feat'), "Exclude Immune Feature (optional)",
                              choices = immune_feat(),
                              selected = c("MHC2 21978456", "Th17 cells"),
                              multiple = TRUE)
      })
      
      output$search_snp <- renderUI({
        shiny::req(subset_gwas())
        snp_options <- (subset_gwas() %>% dplyr::filter(snp_id != "NA"))$snp_id
        shiny::selectInput(ns("snp_int"), "Click on the plot or search for a SNP id:",
                           choices = c("", snp_options))
      })
      
      observe({
        updateSelectInput(session, "snp_int", selected = snp_of_int$ev)
      })
      
      #keeping track of the selected chromossome
      selected_chr_reactive <- reactiveValues(ev = 1)

      observeEvent(input[[sprintf("currentGenomicRegion.%s", "igv_plot")]], {
        shiny::req(input$selection == "Select a region")
        if(!("width" %in% names(plotly::event_data("plotly_relayout", source = "gwas_mht", priority = "event")))){
          new_chr <- as.numeric(sub("chr(.*):.*", "\\1", input[[sprintf("currentGenomicRegion.%s", "igv_plot")]]))
          if(new_chr %in% c(1:22)) selected_chr_reactive$ev <- new_chr
        }
      })
      
      selected_chr <- reactive({
        switch(
          input$selection,
          "See all chromosomes" = c(1:22),
          "Select a region" = selected_chr_reactive$ev
        )
      })
      
      chr_size <- reactive({
        shiny::req(input$selection == "Select a region", selected_chr())
        
        c((min((germline_data$gwas %>% dplyr::filter(chr_col == selected_chr()))$bp_col)),
          (max((germline_data$gwas %>% dplyr::filter(chr_col == selected_chr()))$bp_col)))
      })
      
      #adding interactivity to select a region from zooming in the plot
      
      clicked_int <- reactiveValues(ev=NULL)
      
      observe({
        shiny::req(input$selection == "Select a region")
        #if a user resizes the browser, a new event data will be released, so let's guarantee that this will not affect the visualization
        if(!("width" %in% names(plotly::event_data("plotly_relayout", source = "gwas_mht", priority = "event")))){
          clicked_int$ev <- plotly::event_data("plotly_relayout", source = "gwas_mht", priority = "event")
        }
      })
      
      #reset region with change of chromosome or selection of all chromosomes
      toReset <- reactive({
        shiny::req(input$selection == "Select a region")
        list(selected_chr(),
             input$selection)
      })
      
      observeEvent(toReset(), {
        clicked_int$ev <- NULL
      })
      
      #Creating the object that stores the selected range (either by zoom on manhattan plot or change in the IGV plot)
      chr_range <- reactiveValues(range = NULL)
      
      observe({
        shiny::req(input$selection == "Select a region",
                   chr_size())
        
        if(is.null(clicked_int$ev)){ #default option is min and max positions for the chromosome
          chr_range$range = chr_size()
        } else { #Update range after a zoom in the plot - need to compare with chr_size because plot has x-axis longer than chr_size
          chr_range$range = c((floor(max(chr_size()[1], clicked_int$ev$`xaxis.range[0]`))),
                              (ceiling(min(chr_size()[2], clicked_int$ev$`xaxis.range[1]`))))
        }
      })
      
      observeEvent(input[[sprintf("currentGenomicRegion.%s", "igv_plot")]], { #this observer is only activated when a change is made on the IGV plot
        #if(!("width" %in% names(plotly::event_data("plotly_relayout", source = "gwas_mht", priority = "event")))){
        newLoc <- input[[sprintf("currentGenomicRegion.%s", "igv_plot")]]
        
        pattern <- "chr(.*):(.*)-(.*)"
        sel_start <- as.numeric(gsub(",", "", sub(pattern, "\\2", newLoc)))
        sel_end <- as.numeric(gsub(",", "", sub(pattern, "\\3", newLoc)))
        
        chr_range$range <- c(sel_start,
                             sel_end)
        #}
      })
      
      selected_min <- reactive({
        switch(
          input$selection,
          "See all chromosomes" = 1,
          "Select a region" = chr_range$range[1]
        )
      })
      
      selected_max <- reactive({
        switch(
          input$selection,
          "See all chromosomes" = 245246279,
          "Select a region" = chr_range$range[2]
        )
      })
      
      # Prepare the dataset
      toUpdate <- reactive({
        list(
          input$immunefeature,
          input$exclude_feat,
          input$only_selected,
          selected_chr(),
          input$go_button
        )
      })
      
      gwas_mht <- eventReactive(toUpdate(),{
        shiny::req(immune_feat(), selected_chr(), selected_min(), selected_max())
        build_manhattanplot_tbl(
          gwas_df = germline_data$gwas,
          chr_selected = selected_chr(),
          bp_min = selected_min(),
          bp_max = selected_max(),
          to_select = input$immunefeature,
          to_highlight = input$only_selected,
          to_exclude = input$exclude_feat)
      })
      
      subset_gwas <- reactive({ #updates when range is changed
        shiny::req(gwas_mht())
        gwas_mht() %>%
          dplyr::filter(chr_col %in% selected_chr()) %>%
          dplyr::filter(bp_col >= selected_min() & bp_col <= selected_max())
      })
      
      axisdf <- eventReactive(gwas_mht(), {
        shiny::req(gwas_mht(), subset_gwas())
        get_mhtplot_xlabel(
          selected_region = input$selection,
          gwas_df = gwas_mht(),
          x_min = selected_min(),
          x_max = selected_max()
        )
      })
      
      x_title <- reactive({
        if(input$selection == "See all chromosomes") "Chromosome"
        else ""
      })
      
      output$mht_plot <- plotly::renderPlotly({
        shiny::req(subset_gwas(), axisdf())
        
        shiny::validate(
          shiny::need(nrow(subset_gwas()) > 0, "Select a region with a GWAS hit.")
        )
        
        if(input$only_selected == 0) gwas_df <- subset_gwas() %>% dplyr::mutate(is_highlight=ifelse(display %in% input$immunefeature, "yes", "no"))
        else gwas_df <- subset_gwas() %>% dplyr::mutate(is_highlight = "no")
        
        gwas_df %>%
          dplyr::mutate(is_highlight = replace(is_highlight, snp_id == snp_of_int$ev, "snp")) %>%
          create_manhattanplot(
            df = .,
            x_label = axisdf(),
            y_min = 6,
            y_max = ceiling(max(gwas_df$log10p)),
            x_limits = c(selected_min(), max(selected_max(), subset_gwas()$x_col)),
            x_name = x_title(),
            y_name = "- log10(p-value)",
            plot_title = "",
            source_name = "gwas_mht"
          )
      })
      
      output$igv_plot <- igvShiny::renderIgvShiny({
        shiny::req(subset_gwas())
        shiny::req(input$selection == "Select a region")

        igvShiny::igvShiny(list(
          genomeName="hg19",
          initialLocus= paste0("chr", selected_chr(), ":", scales::comma(selected_min()), "-", scales::comma(selected_max()))
        ),
        displayMode="SQUISHED")
      })
      
      #adding interactivity to select a SNP from the plot or from the dropdown menu
      
      clicked_snp <- reactiveValues(ev=NULL)
      
      observe({
        eventdata <- plotly::event_data( "plotly_click", source = "gwas_mht")
        if(is.null(eventdata)){
          clicked_snp$ev <- NULL
        }else{
          x_pos <- eventdata$x
          y_pos <- round(eventdata$y, 2)
          
          check_df <- gwas_mht()
          check_df$log10p <- round(check_df$log10p, 2)
          
          clicked_snp$ev <- as.character(check_df %>%
                                           dplyr::filter(x_col == x_pos & log10p  == y_pos) %>%
                                           dplyr::select(snp_id))
        }
      })
      
      snp_of_int <- reactiveValues(ev="")
      
      shiny::observeEvent(clicked_snp$ev,{
        snp_of_int$ev <- clicked_snp$ev
      })
      
      shiny::observeEvent(input$snp_int,{
        snp_of_int$ev <- input$snp_int
      })
      
      # clear selected snp on double click
      observeEvent(plotly::event_data("plotly_doubleclick", source = "gwas_mht"), {
        snp_of_int$ev <- ""
      })
      
      selected_snp <- reactive({
        shiny::req(gwas_mht(), axisdf())
        
        shiny::validate(
          shiny::need(!is.null(snp_of_int$ev),
                      "Click manhattan plot to select a SNP."))
        
        shiny::validate(
          shiny::need(snp_of_int$ev != "NA",
                      "Selected SNP has no SNP id"))
        
        gwas_mht() %>%
          dplyr::filter(snp_id == snp_of_int$ev) %>%
          dplyr::select(snp_id, snp_col, chr_col, bp_col) %>%
          dplyr::distinct()
        
      })
      
      output$links <- renderUI({
        shiny::validate(
          shiny::need(selected_snp()$snp_id %in% gwas_mht()$snp_id, "Select SNP")
        )
        #creating the links for external sources
        dbsnp <- paste0("https://www.ncbi.nlm.nih.gov/snp/", selected_snp()$snp_id)
        gtex <- paste0("https://gtexportal.org/home/snp/", selected_snp()$snp_id)
        gwascat <- paste0("https://www.ebi.ac.uk/gwas/search?query=", selected_snp()$snp_id)
        pheweb <- paste0("http://pheweb.sph.umich.edu/SAIGE-UKB/variant/",  gsub(':([[:upper:]])', "-\\1", selected_snp()$snp_col))
        dice <- paste0("https://dice-database.org/eqtls/",  selected_snp()$snp_id)
        
        p(strong(selected_snp()$snp_id), tags$br(),
          selected_snp()$snp_col, tags$br(),
          "View more SNP information at",
          tags$a(href = dbsnp, "dbSNP, "),
          tags$a(href = gtex, "GTEx, "),
          tags$a(href = gwascat, "GWAS Catalog, "),
          tags$a(href = pheweb, "PheWeb, "),
          tags$a(href = dice, "DICE")
        )
      })
      
      output$snp_tbl <- DT::renderDT({
        shiny::req(gwas_mht())
        shiny::validate(
          shiny::need(selected_snp()$snp_id %in% gwas_mht()$snp_id, "")
        )

        snp_df <- gwas_mht() %>%
          dplyr::filter(snp_id == selected_snp()$snp_id) %>%
          dplyr::mutate(nlog = round(log10p, 2)) %>%
          dplyr::select(
            Trait = display,
            `-log10(p)` = nlog)
        
        DT::datatable(
          snp_df,
          rownames = FALSE,
          caption = paste("GWAS hits"),
          options = list(dom = 't')
        )
      })
      
      #COLOCALIZATION
      coloc_label <- reactive({
        switch(
          input$selection,
          "See all chromosomes" = "for all chromossomes",
          "Select a region" = paste("for chromossome", selected_chr())
        )
      })
      ##TCGA
      col_tcga <- reactive({
        germline_data$coloc_tcga %>%
          dplyr::filter(CHR %in% selected_chr()) %>%
          dplyr::select(Type, "SNP" = snp_id, Trait = display, QTL, gene, Splice = TCGA.Splice.ID, CHR, link_plot)
      })
      
      output$colocalization_tcga <- DT::renderDT({
        
        shiny::req(selected_chr())
        
        DT::datatable(
          col_tcga() %>% dplyr::select(!link_plot),
          escape = FALSE,
          rownames = FALSE,
          caption = paste("TCGA colocalization plots available ", coloc_label()),
          selection = 'single',
          options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20)
          )
        )
      })
      
      output$tcga_colocalization_plot <- shiny::renderUI({
        shiny::req(selected_snp(), input$colocalization_tcga_rows_selected)

        shiny::validate(
          shiny::need(
            !is.null(input$colocalization_tcga_rows_selected), "Click on table to see plot"))
        
        link_plot <- as.character(col_tcga()[input$colocalization_tcga_rows_selected, "link_plot"])
        
        tags$div(
          tags$hr(),
          tags$img(src = link_plot,
                   width = "100%")
        )
      })
      
      ##GTEX
      
      output$colocalization_gtex <- DT::renderDT({
        shiny::req(selected_chr())
        
        DT::datatable(
          germline_data$coloc_gtex %>%
            dplyr::filter(CHR %in% selected_chr()) %>%
            dplyr::select("SNP" = snp_id, Trait = display, QTL, Tissue, Gene, CHR),
          escape = FALSE,
          rownames = FALSE,
          caption = paste("GTEX colocalization plots available ", coloc_label()),
          selection = 'single',
          options = list(
            pageLength = 5,
            lengthMenu = c(5, 10, 15, 20)
          )
        )
      })
      
      output$gtex_colocalization_plot <- shiny::renderUI({
        shiny::req(input$colocalization_gtex_rows_selected)
        
        shiny::validate(
          shiny::need(
            !is.null(input$colocalization_gtex_rows_selected), "Click on table to see plot"))
        
        link_plot <- as.character(germline_data$coloc_gtex[input$colocalization_gtex_rows_selected, "link_plot"])
        
        tags$div(
          tags$hr(),
          tags$p(paste("GTEX Splice ID: ", as.character(germline_data$coloc_gtex[input$colocalization_gtex_rows_selected, "GTEXspliceID"]))),
          tags$img(src = link_plot,
                   width = "100%")
        )
      })
      
      observeEvent(input$method_link_gwas,{
        shiny::showModal(modalDialog(
          title = "Method",
          includeMarkdown("data/MethodsText/germline-gwas.md"),
          easyClose = TRUE,
          footer = NULL
        ))
      })
      
      observeEvent(input$method_link_colocalization,{
        shiny::showModal(modalDialog(
          title = "Method",
          includeMarkdown("data/MethodsText/germline-colocalization.md"),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
 
