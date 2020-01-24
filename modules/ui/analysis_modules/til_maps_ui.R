til_maps_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    source("modules/ui/submodules/til_map_distributions_ui.R", local = T)
    source("modules/ui/submodules/data_table_ui.R", local = T)
    
    shiny::tagList(
        .GlobalEnv$titleBox("iAtlas Explorer — TIL Maps"),
        .GlobalEnv$textBox(
            width = 12,
            shiny::includeMarkdown("data/markdown/tilmap.markdown")
        ),
        
        til_map_distributions_ui(ns("til_map_distributions")),
        #     message_html = shiny::includeMarkdown("data/markdown/tilmap_dist.markdown"),
        #     title_text = "TIL Map Characteristics",
        #     click_text = 
        #         "Click point or violin/box to filter samples in table below"
        # ),
        
        
        data_table_ui(
            ns("til_table"),
            title = "TIL Map Annotations",
            message_html = shiny::includeMarkdown("data/markdown/tilmap_table.markdown")
        )
    )
}