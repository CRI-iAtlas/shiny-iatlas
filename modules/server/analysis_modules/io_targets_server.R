io_targets_server <- function(
    input, 
    output, 
    session, 
    sample_con,
    group_con,
    genes_con,
    group_name,
    cohort_colors
) {
    
    source("modules/server/submodules/data_table_server.R", local = T)
    source("modules/server/submodules/distribution_plot_server.R", local = T)
    
    ns <- session$ns
    
    url_gene <- shiny::reactive({
        query <- shiny::parseQueryString(session$clientData$url_search)
        gene  <- query[['gene']]
        if (!is.null(gene)) {
            url_gene <- gene
        } else {
            url_gene <- NA
        }
        return(url_gene)
    })
    
    io_id_con <- shiny::reactive({
        create_connection("gene_types") %>% 
            dplyr::filter(name == "io_target") %>% 
            dplyr::select(id) %>% 
            dplyr::inner_join(
                create_connection("genes_to_types"), 
                by = c("id" = "type_id")
            ) %>% 
            dplyr::select(gene_id) %>% 
            dplyr::compute()
    })
    
    expression_con <- shiny::reactive({
        shiny::req(
            io_id_con(),
            sample_con()
        )
        
        io_id_con() %>% 
            dplyr::inner_join(
                create_connection("genes_to_samples"), 
                by = c("gene_id")
            ) %>% 
            dplyr::select(feature_id = gene_id, sample_id, value = rna_seq_expr) %>% 
            dplyr::inner_join(sample_con(), by = "sample_id") %>% 
            dplyr::compute()
    })
    
    io_con <- shiny::reactive({
        shiny::req(io_id_con(), genes_con())
        
        genes_con() %>% 
            dplyr::inner_join(io_id_con(), by = "gene_id") %>% 
            dplyr::compute() 
    })
    
    relationship_con <- shiny::reactive({
        shiny::req(io_con())
        
        io_con() %>% 
            dplyr::select(
                feature_id = gene_id,
                feature_name = hgnc, 
                Pathway = pathway,
                Therapy = therapy
            ) %>% 
            dplyr::compute()
    })
    
    io_dt_tbl <- shiny::reactive({
        shiny::req(io_con())
        
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
 
    shiny::callModule(
        distributions_plot_server,
        "dist",
        "io_targets_dist_plot",
        expression_con,
        relationship_con,
        group_con,
        group_name,
        cohort_colors,
        key_col = "label"
    )
    
    shiny::callModule(data_table_server, "io_table", io_dt_tbl, escape = F)
    
}