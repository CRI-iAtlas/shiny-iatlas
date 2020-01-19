aboutpage <- fluidPage(
    br(),
    titleBox("About"),
    fluidRow(
        column(
            width = 12,
            column(
                width = 12,
                includeMarkdown("www/md/about.md")     
            )
        )
    )
)
