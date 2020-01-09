iotarget_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — IO Targets"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/io_target.markdown")
        ),
        
        distributions_plot_module_UI(
            ns("dist"),
            message_html = includeMarkdown("data/markdown/io_target_dist.markdown"),
            title_text = "IO Target Gene Expression Distributions",
            scale_default = "Log10",
            plot_clicked_group_default = T,
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
    sample_con,
    group_con,
    genes_con,
    group_name,
    cohort_colors
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
    
    io_id_con <- reactive({
        create_conection("gene_types") %>% 
            dplyr::filter(name == "io_target") %>% 
            dplyr::select(id) %>% 
            dplyr::inner_join(
                create_conection("genes_to_types"), 
                by = c("id" = "type_id")
            ) %>% 
            dplyr::select(gene_id) %>% 
            dplyr::compute()
    })
    
    expression_con <- reactive({
        req(
            io_id_con(),
            sample_con()
        )
        
        io_id_con() %>% 
            dplyr::inner_join(
                create_conection("genes_to_samples"), 
                by = c("gene_id")
            ) %>% 
            dplyr::select(feature_id = gene_id, sample_id, value = rna_seq_expr) %>% 
            dplyr::inner_join(sample_con(), by = "sample_id") %>% 
            dplyr::compute()
    })
    
    io_con <- reactive({
        req(io_id_con(), genes_con())
        
        genes_con() %>% 
            dplyr::inner_join(io_id_con(), by = "gene_id") %>% 
            dplyr::compute() 
    })
    
    relationship_con <- reactive({
        req(io_con())
        
        io_con() %>% 
            dplyr::select(
                INTERNAL = gene_id,
                DISPLAY = hgnc, 
                pathway,
                therapy
            ) %>% 
            dplyr::compute()
    })
    
    io_dt_tbl <- reactive({
        req(io_con())
        
        io_con() %>%  
            dplyr::select(
                Hugo = hgnc, 
                `Entrez ID` =  entrez,
                Pathway = pathway,
                `Therapy Type` = therapy,
                Description = description,
                display
            ) %>% 
            dplyr::collect() %>% 
            dplyr::mutate(url = stringr::str_c(
                "https://www.cancerresearch.org/scientists/immuno-oncology-landscape?2019IOpipelineDB=2019;Target;",
                display
            )) %>% 
            dplyr::mutate(`Link to IO Landscape` =  stringr::str_c(
                "<a href=\'",
                url,
                "\'>",
                display,
                "</a>"
            )) %>% 
            dplyr::select(-c(display, url))
        
    })
 
    callModule(
        distributions_plot_module,
        "dist",
        "io_targets_dist_plot",
        expression_con,
        relationship_con,
        group_con,
        group_name,
        cohort_colors,
        key_col = "label",
        variable_group_names = c("Pathway", "Therapy Type")
    )
    
    callModule(data_table_module, "io_table", io_dt_tbl, escape = F)
    
}