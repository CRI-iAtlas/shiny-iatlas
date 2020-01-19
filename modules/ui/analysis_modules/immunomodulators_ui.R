immunomodulators_ui <- function(id) {
    
    ns <- NS(id)
    
    source("modules/ui/submodules/data_table_ui.R", local = T)
    source("modules/ui/submodules/distribution_plot_ui.R", local = T)
    
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
        
        distributions_plot_ui(
            ns("dist"),
            message_html = includeMarkdown("data/markdown/im_dist.markdown"),
            title_text = "Immunomodulator Distributions",
            scale_default = "Log10",
            plot_clicked_group_default = T,
        ),
        
        data_table_ui(
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