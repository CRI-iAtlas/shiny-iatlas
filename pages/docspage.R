docspage <- fluidPage(
  br(),
  titleBox("Documentation"),
  textBox(
    width = 12,
    p(
      "The current version of the", 
      strong("iAtlas Portal"),
      "was built in R using code hosted at",
      a("https://github.com/CRI-iAtlas/shiny-iatlas", 
        href = "https://github.com/CRI-iAtlas/shiny-iatlas/"),
      ". The details below are copied directly from the",
      strong("README"), 
      "for the code repository."
    )
  ),
  fluidRow(
    column(
      width = 12,
      includeMarkdown("README.md")
    )
  )
)