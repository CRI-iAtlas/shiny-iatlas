distributions_plot_ui <- function(
    id, 
    title_text = "Distributions",
    message_html = p(stringr::str_c(
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
    
    ns <- NS(id)
    
    sectionBox(
        title = title_text,
        messageBox(width = 12, message_html),
        fluidRow(
            optionsBox(
                width = 12,
                conditionalPanel(
                    condition =  "output.display_group_choice",
                    column(width = 4,uiOutput(ns("group_choice_ui"))),
                    ns = ns
                ),
                column(
                    width = 4,
                    uiOutput(ns("variable_choice_ui"))
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("plot_type"),
                        "Select or Search for Plot Type",
                        choices = c("Violin", "Box")
                    )
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("scale_method"), 
                        "Select or Search for variable scaling", 
                        selected = scale_default,
                        choices = scale_options
                    )
                ),
                column(
                    width = 4,
                    checkboxInput(
                        ns("see_drilldown"), 
                        "Plot clicked group?", 
                        plot_clicked_group_default
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotly::plotlyOutput(ns("plot")) %>%
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("plot_text")),
                h4(click_text)
            )
        ),
        downloadButton(ns('download_data'), 'Download'),
        conditionalPanel(
            condition =  "input.see_drilldown",
            ns = ns,
            fluidRow(
                plotBox(
                    width = 12,
                    plotly::plotlyOutput(ns("drilldown_plot")) %>% 
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}