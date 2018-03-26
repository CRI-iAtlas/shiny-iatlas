datainfo_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 12, background = "black",
          span(strong("Data Description"),
               style = "font-size:18px")
      )
    ),
    fluidRow(
      box(width = 12,
          "Table goes here..."
          )
      )
  )
}

# datainfo <- function(input, output, session) {
#   # TODO: add renderDataTable function
# }
