iotarget_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — IO Targets"),
    textBox(
      width = 12,
      p("This module allows you to explore the tumor expression of some of the immuno-oncological (IO) targets discussed in ",
        tags$a(href="https://www.cancerresearch.org","Cancer Research Institute"), 
        tags$a(href="https://www.cancerresearch.org/scientists/clinical-accelerator/leadership","Clinicial Accelerator"),
        "publications"
        ),
      
      tags$ul(
        tags$li("Comprehensive 2017 IO landscape analysis (",
                tags$a(href="https://academic.oup.com/annonc/article/29/1/84/4693829","Annals of Oncology"),
                ", December 2017)"),
        tags$li("Comprehensive 2018 IO landscape analysis including a comparison to the previous year (",
                tags$a(href="https://www.nature.com/articles/nrd.2018.167","Nature Reviews Drug Discovery"),
                ", October 2018)")
      ),
      
     p("All displayed targets and descriptions are compiled and annotated by the",
       tags$a(href="https://www.cancerresearch.org","Cancer Research Institute"),
       tags$a(href="https://www.cancerresearch.org/scientists/clinical-accelerator/leadership","Clinicial Accelerator"),
       "management team using publicly available data, 
       including trade news, company press releases, academic publications, and NCBI."),
     
     p("For more information about publications and Cancer Research Institute’s IO Landscape, please go to",
       tags$a(href="https://www.cancerresearch.org/scientists/clinical-accelerator/landscape-of-immuno-oncology-drug-development","Landscape of Immuno-Oncology Drug Development"),
       "."
        )
      ), 
  
    # IO Target distributions section ----
    sectionBox(
      title = "IO Target Gene Expression Distributions",
      messageBox(
        width = 12,
        p("To explore gene expression distributions, vary sample grouping by selecting an option from Explorer Settings in the left panel. Options include Immune Subtype Groups (default), TCGA Subtype Groups, and TCGA Study.","\n"),
        p("For more information about Sample Groups, refer to Sample Group Overview in the left panel.","\n"),      
        p("Select an IO Target Gene below to see its expression in tumor samples. By default, IO Target Genes are grouped by therapy type. To vary IO Target Category, select a different option from IO Target Category (drop-down menu on the right) to re-organize the IO Target Gene list."),
        p("The IO Target Categories are:"),
        
        p(em('Therapy Type'),", the type of therapy as described in",
          tags$a(href="https://academic.oup.com/annonc/article/29/1/84/4693829","Annals of Oncology"),
          ", December 2017"),
        tags$ul(
          tags$li(strong('T-cell-targeted immunomodulators'), "that act on inhibitory or activating molecules expressed by T cells"),
          tags$li(strong('Other immunomodulators'), " that act on other immune cells or the tumor immune microenvironment to unleash antitumor immunity"),
          tags$li(strong('Targeted by Other Immuno-Oncology Therapy Type'), " (Cancer Vaccine, Cell therapy, Oncolytic Virus, and CD3-targeted bispecific antibody) to unleash antitumoral immunity")
        ), 
        p(em('Pathway'),", one of the molecular pathways to which the target protein belongs. Sourced from",
          tags$a(href="https://www.ncbi.nlm.nih.gov/biosystems/","NCBI Biosystems"),
          "(e.g. KEGG database, Reactome database, WikiPathways).")

      ),
      fluidRow(
        optionsBox(
          width = 12,
          column(
              width = 6,
              uiOutput(ns("gene_choices"))
          ),
          column(
              width = 6,
              selectInput(
                  inputId = ns("io_target_category_choice_choice"),
                  label = "Select IO Target Category",
                  choices = c("Therapy Type","Pathway")
              )
          )
        )
      ),
      fluidRow(
        plotBox(
          width = 12,
          plotlyOutput(ns("violinPlot")) %>% 
            shinycssloaders::withSpinner(),
          p(),
          textOutput(ns("violin_group_text"))
        )
      ),
      fluidRow(
        plotBox(
          width = 12,
          plotlyOutput(ns("histPlot")) %>% 
            shinycssloaders::withSpinner()
        )
      )
    ),
    
    # IO Target annotations section ----
    sectionBox(
      title = "IO Target Annotations",
      messageBox(
        width = 12,
        p("The table shows annotations of the IO Targets, with columns as described above and description based on public resources such as NCBI. Use the Search box in the upper right to find an IO target of interest.")  
      ),
      fluidRow(
        tableBox(
          width = 12,
          div(style = "overflow-x: scroll",
              DT::dataTableOutput(ns("io_target_annotations_table")) %>%
                shinycssloaders::withSpinner()
          )
        )
      )
    )
  )
}

iotarget <- function(
    input, 
    output, 
    session,
    group_display_choice, 
    group_internal_choice, 
    sample_group_df,
    subset_df, 
    plot_colors
) {
    
    ns <- session$ns
    
    io_target_expr_plot_df <- reactive({
        req(subset_df(), 
            input$io_target_gene_choice, 
            group_internal_choice(),
            cancelOutput = T)
        
        build_io_target_expr_plot_df(
            subset_df(),
            filter_value = input$io_target_gene_choice, 
            group_option = group_internal_choice())
    })
    
    output$violinPlot <- renderPlotly({
        req(io_target_expr_plot_df(), cancelOutput = T)
        
        validate(
            need(nrow(io_target_expr_plot_df()) > 0, 
                 "Current selected group and selected variable have no overlap")
        )
        
        create_violinplot(
            io_target_expr_plot_df(),
            xlab = group_display_choice(), 
            ylab = "log10(count + 1)",
            source_name = "violin",
            fill_colors = plot_colors())
    })
        
    
    output$violin_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "violin", 
            group_internal_choice(),
            sample_group_df())  
    })
    
    output$histPlot <- renderPlotly({
        
        eventdata <- event_data("plotly_click", source = "violin")
        validate(need(!is.null(eventdata), "Click violin plot above"))
        clicked_group <- eventdata$x[[1]]
        
        current_violin_groups <- io_target_expr_plot_df() %>% 
            magrittr::use_series(x) %>% 
            unique
        
        validate(need(clicked_group %in% current_violin_groups, "Click violin plot above"))
        
        histplot_df <- io_target_expr_plot_df() %>% 
            select(GROUP = x, log_count = y) %>% 
            filter(GROUP == clicked_group)
        
        create_histogram(
            histplot_df,
            x_col = "log_count",
            x_lab = "log10(count + 1)",
            title = eventdata$x[[1]])
    })
    
    output$io_target_annotations_table <- DT::renderDT({
        
        panimmune_data$io_target_annotations %>% 
          mutate(LinkText=.$IO_target_URL %>% str_split(";") %>% map(last) %>% flatten_chr()) %>%
          mutate(`Link to IO Landscape`=paste("<a href=\"",IO_target_URL,"\">",LinkText,"</a>",
                                                sep="")) %>% select(-IO_target_URL,-LinkText) %>% 
            datatable(
                options = list(pageLength = 10),
                rownames = FALSE,
                escape = setdiff(colnames(.),"Link to IO Landscape") ## To get hyperlink displayed
                )
    })
    
    output$gene_choices <- renderUI({
      query <- parseQueryString(session$clientData$url_search)
      selected_gene <- NULL
      if (!is.null(query[['gene']])) {
        selected_gene = query[['gene']]
      }
      choices <- get_iotarget_nested_list(
        class_column = input$io_target_category_choice_choice)
      selectInput(
        ns("io_target_gene_choice"),
        label = "Select IO Target Gene",
        choices = choices,
        selected = selected_gene)
    })

}