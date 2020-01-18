driver_associations_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Association with Driver Mutations"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/driver.markdown")
        ),
        # univariate_driver_ui(ns("univariate_driver")),
        multivariate_driver_ui(ns("multivariate_driver"))
    )
}

univariate_driver_ui <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Immune Response Association With Driver Mutations -- single variable",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/driver_single.markdown")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 4,
                    uiOutput(ns("response_options"))
                ),
                column(
                    width = 4,
                    numericInput(
                        ns("min_mut"),
                        "Minimum number of mutant samples per group:",
                        20,
                        min = 2
                    )
                ),
                column(
                    width = 4,
                    numericInput(
                        ns("min_wt"),
                        "Minimum number of wild type samples per group:",
                        20,
                        min = 2
                    )
                )
            )
        ),
        volcano_plot_ui(ns("univariate_driver"))
    )
}

multivariate_driver_ui <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Immune Response Association With Driver Mutations -- multivariate",
        messageBox(
            width = 12,
            includeMarkdown("data/markdown/driver_multi.markdown")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 3,
                    uiOutput(ns("response_options"))
                ),
                column(
                    width = 3,
                    numericInput(
                        ns("min_mutants"),
                        "Minimum number of mutant samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                column(
                    width = 3,
                    numericInput(
                        ns("min_wildtype"),
                        "Minimum number of wild type samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                column(
                    width = 3,
                    selectInput(
                        ns("group_mode"),
                        "Select or Search for Mode", 
                        choices = c("By group", "Across groups"),
                        selected = "Across groups"
                    )
                ),
            )
        ),
        model_selection_ui(ns("module1")),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 6,
                    textOutput(ns("model_text"))
                ),
                column(
                    width = 6,
                    actionButton(ns("calculate_button"), "Calculate")
                )
            )
        ),
        volcano_plot_ui(ns("multivariate_driver"))
    )
}