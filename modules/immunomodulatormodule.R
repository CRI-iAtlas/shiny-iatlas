immunomodulator_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Immunomodulators"),
        textBox(
            width = 12,
            p(stringr::str_c(
                "Explore the expression of genes that code for immunomodulating",
                "proteins, including checkpoint proteins.",
                sep = " "
            ))  
        ),
        
        distributions_plot_module_UI(
            ns("dist"),
            message_html = includeMarkdown("data/markdown/im_dist.markdown"),
            title_text = "Immunomodulator Distributions",
            scale_default = "Log10",
            plot_clicked_group_default = T,
        ),
        
        data_table_module_UI(
            ns("im_table"),
            title = "Immunomodulator Annotations",
            message_html = p(stringr::str_c(
                "The table shows annotations of the immumodulators, and source.",
                "Use the Search box in the upper right to find an immumodulator of",
                "interest.",
                sep = " "
            ))
        )
    )
}

immunomodulator <- function(
    input, 
    output, 
    session, 
    sample_con,
    group_con,
    genes_con,
    group_name,
    cohort_colors
){
    
    im_id_con <- reactive({
        create_conection("gene_types") %>% 
            dplyr::filter(name == "immunomodulator") %>% 
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
            im_id_con(),
            sample_con()
        )
        
        im_id_con() %>% 
            dplyr::inner_join(
                create_conection("genes_to_samples"), 
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
        distributions_plot_module,
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
        data_table_module, 
        "im_table", 
        im_dt_tbl
    )
}