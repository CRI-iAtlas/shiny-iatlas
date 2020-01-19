driver_associations_ui <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
        .GlobalEnv$titleBox("iAtlas Explorer — Association with Driver Mutations"),
        .GlobalEnv$textBox(
            width = 12,
            shiny::includeMarkdown("data/markdown/driver.markdown")
        ),
        # univariate_driver_ui(ns("univariate_driver")),
        multivariate_driver_ui(ns("multivariate_driver"))
    )
}

univariate_driver_ui <- function(id){
    
    ns <- shiny::NS(id)
    
    source("modules/ui/submodules/volcano_plot_ui.R", local = T)
    
    .GlobalEnv$sectionBox(
        title = "Immune Response Association With Driver Mutations -- single variable",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown("data/markdown/driver_single.markdown")
        ),
        shiny::fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::column(
                    width = 4,
                    uiOutput(ns("response_options"))
                ),
                shiny::column(
                    width = 4,
                    numericInput(
                        ns("min_mut"),
                        "Minimum number of mutant samples per group:",
                        20,
                        min = 2
                    )
                ),
                shiny::column(
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
    
    ns <- shiny::NS(id)
    
    source("modules/ui/submodules/volcano_plot_ui.R", local = T)
    source("modules/ui/submodules/model_selection_ui.R", local = T)
    
    .GlobalEnv$sectionBox(
        title = "Immune Response Association With Driver Mutations -- multivariate",
        .GlobalEnv$messageBox(
            width = 12,
            shiny::includeMarkdown("data/markdown/driver_multi.markdown")
        ),
        shiny::fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::column(
                    width = 3,
                    uiOutput(ns("response_options"))
                ),
                shiny::column(
                    width = 3,
                    numericInput(
                        ns("min_mutants"),
                        "Minimum number of mutant samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                shiny::column(
                    width = 3,
                    numericInput(
                        ns("min_wildtype"),
                        "Minimum number of wild type samples per group:", 
                        20, 
                        min = 2
                    )
                ),
                shiny::column(
                    width = 3,
                    shiny::selectInput(
                        ns("group_mode"),
                        "Select or Search for Mode", 
                        choices = c("By group", "Across groups"),
                        selected = "Across groups"
                    )
                ),
            )
        ),
        model_selection_ui(ns("module1")),
        shiny::fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::column(
                    width = 6,
                    shiny::textOutput(ns("model_text"))
                ),
                shiny::column(
                    width = 6,
                    shiny::actionButton(ns("calculate_button"), "Calculate")
                )
            )
        ),
        volcano_plot_ui(ns("multivariate_driver"))
    )
}