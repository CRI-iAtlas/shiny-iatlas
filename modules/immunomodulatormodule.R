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
    sample_tbl,
    group_tbl,
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
            dplyr::select(gene_id)
    })
    
    expression_tbl <- reactive({
        req(
            im_id_con(),
            sample_tbl()
        )
        
        im_id_con() %>% 
            dplyr::inner_join(
                create_conection("genes_to_samples"), 
                by = c("gene_id")
            ) %>% 
            dplyr::select(feature_id = gene_id, sample_id, value = rna_seq_expr) %>% 
            dplyr::filter(sample_id %in% local(sample_tbl()$sample_id)) %>% 
            dplyr::as_tibble() 
    })
    
    im_tbl <- reactive({
        req(im_id_con())
        
        im_id_con() %>%
            dplyr::inner_join(
                create_conection("genes"), 
                by = c("gene_id" = "id")
            )  %>% 
            dplyr::left_join(
                create_conection("gene_families"), 
                by = c("gene_family_id" = "id")
            ) %>% 
            dplyr::rename(gene_family = name) %>% 
            dplyr::left_join(
                create_conection("gene_functions"), 
                by = c("gene_function_id" = "id")
            ) %>% 
            dplyr::rename(gene_function = name) %>% 
            dplyr::left_join(
                create_conection("immune_checkpoints"), 
                by = c("immune_checkpoint_id" = "id")
            ) %>% 
            dplyr::rename(immune_checkpoint = name) %>% 
            dplyr::left_join(
                create_conection("super_categories"), 
                by = c("super_cat_id" = "id")
            ) %>% 
            dplyr::rename(super_category = name) %>% 
            dplyr::as_tibble() 
    })
    
    relationship_tbl <- reactive({
        req(im_tbl())
        
        im_tbl() %>% 
            dplyr::select(
                INTERNAL = gene_id,
                DISPLAY = hgnc, 
                `Gene Family` = gene_family,
                `Super Category` = super_category,
                `Immune Checkpoint` = immune_checkpoint,
            )
    })
    
    im_dt_tbl <- reactive({
        req(im_tbl())
        
        im_tbl() %>%  
            dplyr::select(
                Hugo = hgnc, 
                `Entrez ID` =  entrez,
                `Gene Family` = gene_family,
                `Super Category` = super_category,
                `Immune Checkpoint` = immune_checkpoint,
                Function = gene_function,
                `Reference(s) [PMID]` = references
            ) 
    })
    
    callModule(
        distributions_plot_module,
        "dist",
        "immunomodulators_dist_plot",
        expression_tbl,
        relationship_tbl,
        sample_tbl,
        group_tbl,
        group_name,
        cohort_colors,
        key_col = "label",
        variable_group_names = c(
            "Gene Family", 
            "Super Category", 
            "Immune Checkpoint"
        )
    )
    
    callModule(data_table_module, "im_table", im_dt_tbl)
}