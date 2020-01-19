distributions_plot_ui <- function(
    id, 
    title_text = "Distributions",
    message_html = shiny::p(stringr::str_c(
        "Select variable to its to see its distribution over sample groups.",
        "Plots are available as violin plots, and box plots with full data",
        "points superimposed."
    )),
    click_text = "Click plot to see group information.",
    scale_default = "None",
    scale_options = c(
        "None", 
        "Log2", 
        "Log2 + 1",
        "Log10",
        "Log10 + 1"
    ),
    plot_clicked_group_default = F
){
    
    ns <- shiny::NS(id)
    
    .GlobalEnv$sectionBox(
        title = title_text,
        .GlobalEnv$messageBox(width = 12, message_html),
        shiny::fluidRow(
            .GlobalEnv$optionsBox(
                width = 12,
                shiny::conditionalPanel(
                    condition =  "output.display_group_choice",
                    shiny::column(
                        width = 4, 
                        shiny::uiOutput(ns("group_choice_ui"))
                    ),
                    ns = ns
                ),
                shiny::column(
                    width = 4,
                    shiny::uiOutput(ns("variable_choice_ui"))
                ),
                shiny::column(
                    width = 4,
                    shiny::selectInput(
                        ns("plot_type"),
                        "Select or Search for Plot Type",
                        choices = c("Violin", "Box")
                    )
                ),
                shiny::column(
                    width = 4,
                    shiny::selectInput(
                        ns("scale_method"), 
                        "Select or Search for variable scaling", 
                        selected = scale_default,
                        choices = scale_options
                    )
                ),
                shiny::column(
                    width = 4,
                    shiny::checkboxInput(
                        ns("see_drilldown"), 
                        "Plot clicked group?", 
                        plot_clicked_group_default
                    )
                )
            )
        ),
        shiny::fluidRow(
            .GlobalEnv$plotBox(
                width = 12,
                plotly::plotlyOutput(ns("plot")) %>%
                    shinycssloaders::withSpinner(),
                shiny::p(),
                shiny::textOutput(ns("plot_text")),
                shiny::h4(click_text)
            )
        ),
        shiny::downloadButton(ns('download_data'), 'Download'),
        shiny::conditionalPanel(
            condition =  "input.see_drilldown",
            ns = ns,
            shiny::fluidRow(
                .GlobalEnv$plotBox(
                    width = 12,
                    "drilldown_plot" %>% 
                        ns() %>% 
                        plotly::plotlyOutput() %>% 
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}