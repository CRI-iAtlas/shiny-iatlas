immunomodulators_server <- function(
    input, 
    output, 
    session, 
    sample_con,
    group_con,
    genes_con,
    group_name,
    cohort_colors
){
    
    source("modules/server/submodules/data_table_server.R", local = T)
    source("modules/server/submodules/distribution_plot_server.R", local = T)
    
    im_id_con <- reactive({
        create_connection("gene_types") %>% 
            dplyr::filter(name == "immunomodulator") %>% 
            dplyr::select(id) %>% 
            dplyr::inner_join(
                create_connection("genes_to_types"), 
                by = c("id" = "type_id")
            ) %>% 
            dplyr::select(gene_id) %>% 
            dplyr::compute()
    })
    
    expression_con <- reactive({
        req(
            im_id_con(),
            sample_con()
        )
        
        im_id_con() %>% 
            dplyr::inner_join(
                create_connection("genes_to_samples"), 
                by = c("gene_id")
            ) %>% 
            dplyr::select(feature_id = gene_id, sample_id, value = rna_seq_expr) %>% 
            dplyr::inner_join(sample_con(), by = "sample_id") %>% 
            dplyr::compute()
    })
    
    im_con <- reactive({
        req(im_id_con(), genes_con())
        
        genes_con() %>% 
            dplyr::inner_join(im_id_con(), by = "gene_id") %>% 
            dplyr::compute() 
    })
    
    relationship_con <- reactive({
        req(im_con())
        
        im_con() %>% 
            dplyr::select(
                feature_id = gene_id,
                feature_name = hgnc, 
                `Gene Family` = gene_family,
                `Super Category` = super_category,
                `Immune Checkpoint` = immune_checkpoint,
            ) %>% 
            dplyr::compute()
    })
    
    im_dt_tbl <- reactive({
        req(im_con())
        
        im_con() %>%  
            dplyr::select(
                Hugo = hgnc, 
                `Entrez ID` =  entrez,
                `Gene Family` = gene_family,
                `Super Category` = super_category,
                `Immune Checkpoint` = immune_checkpoint,
                Function = gene_function,
                `Reference(s) [PMID]` = references
            ) %>% 
            dplyr::collect()
    })
    
    callModule(
        distributions_plot_server,
        "dist",
        "immunomodulators_dist_plot",
        expression_con,
        relationship_con,
        group_con,
        group_name,
        cohort_colors,
        key_col = "label"
    )
    
    callModule(
        data_table_server, 
        "im_table", 
        im_dt_tbl
    )
}