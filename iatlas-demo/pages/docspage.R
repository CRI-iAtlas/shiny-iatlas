docspage <- fluidPage(
  br(),
  fluidRow(
    box(width = 12, background = "black",
        span(strong("Documentation"),
             style = "font-size:18px")
    )
  ),
  fluidRow(
    column(12,
           "iAtlas is cool."
    )
  )
)