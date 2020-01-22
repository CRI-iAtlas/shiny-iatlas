cell_type_fractions_ui <- function(id){
    
    ns <- NS(id)
    
    .GlobalEnv$sectionBox(
        title = "Cell Type Fractions",
        .GlobalEnv$messageBox(
            width = 12,
            includeMarkdown("data/markdown/cell_type_fractions.markdown")
        ),
        fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                selectInput(
                    inputId = ns("fraction_group_choice"),
                    label = "Select Cell Fraction Type",
                    choices = c(
                        "Immune Cell Proportion - Common Lymphoid and Myeloid Cell Derivative Class",
                        "Immune Cell Proportion - Differentiated Lymphoid and Myeloid Cell Derivative Class",
                        "Immune Cell Proportion - Multipotent Progenitor Cell Derivative Class",
                        "Immune Cell Proportion - Original"
                    ),
                    selected = "Immune Cell Proportion - Differentiated Lymphoid and Myeloid Cell Derivative Class"
                )
            )
        ),
        fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                plotly::plotlyOutput(ns("barplot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("barplot_text"))
            )
        )
    )
}