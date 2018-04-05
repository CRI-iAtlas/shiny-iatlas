resourcespage <- fluidPage(
  br(),
  titleBox("Resources"),
  fluidRow(
    column(
      width = 12,
      column(
        width = 12,
        includeMarkdown("www/md/resources.md")
      )
    )
  )
)