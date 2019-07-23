iotarget_UI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — IO Targets"),
    textBox(
      width = 12,
      p("This module allows you to explore the tumor expression of some of the immuno-oncological (IO) targets discussed in ",
        tags$a(href="https://www.cancerresearch.org","Cancer Research Institute"), 
        tags$a(href="https://www.cancerresearch.org/scientists/clinical-accelerator/leadership","Clinical Accelerator"),
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
      p("and to connect to additional resources on agents for those targets provided by those studies."),
      
      p("All displayed targets and descriptions are compiled and annotated by the",
        tags$a(href="https://www.cancerresearch.org","Cancer Research Institute"),
        tags$a(href="https://www.cancerresearch.org/scientists/clinical-accelerator/leadership","Clinical Accelerator"),
        "management team using publicly available data, 
       including trade news, company press releases, academic publications, and NCBI."),
      
      p("For more information about publications and Cancer Research Institute’s IO Landscape, please go to",
        tags$a(href="https://www.cancerresearch.org/scientists/clinical-accelerator/landscape-of-immuno-oncology-drug-development","Landscape of Immuno-Oncology Drug Development"),
        "."
      )
    ), 
    
    distributions_plot_module_UI(
      ns("dist"),
      title_text = "IO Target Gene Expression Distributions",
      scale_default = "Log10",
      plot_clicked_group_default = T,
      message_html = p(
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
    ),
    
    data_table_module_UI(
      ns("io_table"), 
      title = "IO Target Annotations",
      message_html = p(stringr::str_c(
        "The table shows annotations of the IO Targets, with columns as",
        "described above and description based on public resources such as",
        "NCBI. Use the Search box in the upper right to find an IO target of",
        "interest.",
        "\n",
        "The last column provides a direct link to target information on the",
        "IO Landscape resource such as number of target agents under active",
        "development, and development stage.",
        sep = " "
      ))
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
  
  url_gene <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    gene  <- query[['gene']]
    if (!is.null(gene)) {
      url_gene <- gene
    } else {
      url_gene <- NA
    }
    return(url_gene)
  })
  
  data_df <- reactive({
    subset_df() %>%
      dplyr::select(
        x = group_internal_choice(),
        "ParticipantBarcode") %>% 
      dplyr::inner_join(panimmune_data$io_target_expr_df, by = "ParticipantBarcode") %>% 
      dplyr::rename(label = "ParticipantBarcode")
  })
  
  metadata_df <- reactive({
    panimmune_data$io_target_annotations %>%  
      dplyr::select(
        INTERNAL = `HGNC Symbol`, 
        DISPLAY = `Gene`,
        Pathway, `Therapy Type`) 
  })
  
  callModule(
    distributions_plot_module,
    "dist",
    "io_targets_dist_plot",
    data_df,
    metadata_df,
    sample_group_df,
    plot_colors,
    group_display_choice,
    variable_selection_default = url_gene(),
    key_col = "label",
  )

  table_df <- reactive({
    panimmune_data$io_target_annotations %>% 
      dplyr::mutate(LinkText = .$IO_target_URL%>% 
                      stringr::str_split(";") %>% 
                      purrr::map(rev) %>% 
                      purrr::map_chr(1)
      ) %>% 
      dplyr::mutate(
        `Link to IO Landscape`= paste(
          "<a href=\"",
          IO_target_URL,"\">",
          LinkText,"</a>", 
          sep=""
        )
      ) %>% 
      dplyr::select(-IO_target_URL, -LinkText)
  })       
  
  
  callModule(data_table_module, "io_table", table_df, escape = F)
  
}