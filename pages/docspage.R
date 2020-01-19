docspage <- shiny::fluidPage(
    shiny::br(),
    .GlobalEnv$titleBox("Documentation"),
    .GlobalEnv$messageBox(
        width = 12,
        shiny::includeMarkdown("www/md/docs.md")
    ),
    shiny::fluidRow(
        shiny::column(
            width = 12,
            shiny::column(
                width = 12,
                shiny::includeMarkdown("README.md")
            )
        )
    )
)