cell_type_fractions_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    source("modules/ui/submodules/plotly_ui.R", local = T)
    
    .GlobalEnv$sectionBox(
        title = "Cell Type Fractions",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown("data/markdown/cell_type_fractions.markdown")
        ),
        fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::selectInput(
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
        shiny::fluidRow(
            plotly_ui(ns("plotly_barplot"))
        )
    )
}