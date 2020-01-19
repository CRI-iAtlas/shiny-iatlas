resourcespage <- shiny::fluidPage(
    shiny::br(),
    .GlobalEnv$titleBox("Resources"),
    shiny::fluidRow(
        shiny::column(
            width = 12,
            shiny::column(
                width = 12,
                shiny::includeMarkdown("www/md/resources.md")
            )
        )
    )
)