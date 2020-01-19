til_maps_ui <- function(id) {
    
    ns <- NS(id)
    
    source("modules/ui/submodules/data_table_ui.R", local = T)
    source("modules/ui/submodules/distribution_plot_ui.R", local = T)
    
    tagList(
        titleBox("iAtlas Explorer — TIL Maps"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/tilmap.markdown")
        ),
        
        distributions_plot_ui(
            ns("dist"),
            message_html = includeMarkdown("data/markdown/tilmap_dist.markdown"),
            title_text = "TIL Map Characteristics",
            click_text = 
                "Click point or violin/box to filter samples in table below"
        ),
        
        
        data_table_ui(
            ns("til_table"),
            title = "TIL Map Annotations",
            message_html = includeMarkdown("data/markdown/tilmap_table.markdown")
        )
    )
}