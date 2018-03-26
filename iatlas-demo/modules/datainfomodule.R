datainfo_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    titleBox("Data Description"),
    fluidRow(
      tableBox(width = 12,
          "Table goes here..."
          )
      )
  )
}

# datainfo <- function(input, output, session) {
#   # TODO: add renderDataTable function
# }
