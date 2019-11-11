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
    group_display_choice, 
    group_con,
    immunomodulator_expr_con,
    immunomodulators_con,
    plot_colors
){
    
    expression_con <- reactive({
        req(immunomodulator_expr_con())
        immunomodulator_expr_con() %>% 
            dplyr::select(
                label = sample,
                x = group,
                y = value,
                feature = gene
            )
    })
    
    relationship_con <- reactive({
        req(immunomodulators_con())
        immunomodulators_con() %>% 
            dplyr::select(
                INTERNAL = gene,
                DISPLAY = display,
                gene_family, 
                super_category, 
                immune_checkpoint
            )
    })
    
    im_tbl <- reactive({
        req(immunomodulators_con())
        immunomodulators_con() %>% 
            dplyr::select(
                Gene = display, 
                `HGNC Symbol` = gene, 
                `Friendly Name` = display2,
                `Entrez ID` =  entrez,
                `Gene Family` = gene_family,
                `Super Category` = super_category,
                `Immune Checkpoint` = immune_checkpoint,
                Function = gene_function,
                `Reference(s) [PMID]` = reference
            ) %>% 
            dplyr::as_tibble()
    })
    
    callModule(
        distributions_plot_module,
        "dist",
        "immunomodulators_dist_plot",
        expression_con,
        relationship_con,
        group_con,
        group_display_choice,
        plot_colors,
        key_col = "label",
        variable_group_names = c(
            "Gene Family", 
            "Super Category", 
            "Immune Checkpoint"
        )
    )
    
    callModule(data_table_module, "im_table", im_tbl)
}