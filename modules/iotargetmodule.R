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
    group_display_choice, 
    group_con,
    io_target_expr_con,
    io_targets_con,
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
    
    expression_con <- reactive({
        req(io_target_expr_con())
        io_target_expr_con() %>% 
            dplyr::select(
                label = sample,
                x = group,
                y = value,
                feature = gene
            )
    })
    
    relationship_con <- reactive({
        req(io_targets_con())
        io_targets_con() %>% 
            dplyr::select(
                INTERNAL = gene,
                DISPLAY = display,
                pathway, 
                therapy_type
            )
    })
    
    io_tbl <- reactive({
        req(io_targets_con())
        io_targets_con() %>% 
            dplyr::select(
                Gene = display, 
                `HGNC Symbol` = gene, 
                `Friendly Name` = display2,
                `Entrez ID` =  entrez,
                Pathway = pathway,
                `Therapy Type` = therapy_type,
                Description = description, 
                `Link to IO Landscape` = link
            ) %>% 
            dplyr::as_tibble()
    })  

    
    callModule(
        distributions_plot_module,
        "dist",
        "io_targets_dist_plot",
        expression_con,
        relationship_con,
        group_con,
        group_display_choice,
        plot_colors,
        variable_selection_default = url_gene(),
        key_col = "label",
        variable_group_names = c("Pathway", "Therapy Type")
    )
    
    callModule(data_table_module, "io_table", io_tbl, escape = F)
    
}