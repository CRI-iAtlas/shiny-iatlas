docspage <- fluidPage(
  br(),
  titleBox("Documentation"),
  fluidRow(
    column(
      width = 12,
      includeMarkdown("www/md/doc.md")
    )
  )
)