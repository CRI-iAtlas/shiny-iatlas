tumor_microenvironment_ui <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
        .GlobalEnv$titleBox("iAtlas Explorer — Tumor Microenvironment"),
        .GlobalEnv$textBox(
            width = 12,
            shiny::p("Explore the immune cell proportions in your sample groups.")  
        ),
        overall_cell_proportions_module_ui(ns("ocp_module")),
        cell_type_fractions_module_ui(ns("ctf_module"))
    )
}

overall_cell_proportions_module_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    .GlobalEnv$sectionBox(
        title = "Overall Cell Proportions",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown("data/markdown/cell_proportions1.markdown")
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                plotly::plotlyOutput(ns("barplot")) %>% 
                    shinycssloaders::withSpinner(),
                shiny::textOutput(ns("barplot_text"))
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                .GlobalEnv$messageBox(
                    width = 6,
                    shiny::includeMarkdown("data/markdown/cell_proportions2.markdown")
                ),
                column(
                    width = 6,
                    shiny::br(),
                    shiny::fluidRow(
                        plotly::plotlyOutput(ns("scatterplot")) %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    )
}

cell_type_fractions_module_ui <- function(id){
    
    ns <- NS(id)
    
    .GlobalEnv$sectionBox(
        title = "Cell Type Fractions",
        .GlobalEnv$messageBox(
            width = 12,
            includeMarkdown("data/markdown/cell_fractions.markdown")
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