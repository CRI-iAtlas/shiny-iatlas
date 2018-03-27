# dashboard_UI <- function(id) {
#   ns <- NS(id)
#   
#   tagList(
#     titlePanel("Welcome to the iAtlas Portal prototype!"),
#     fluidRow(
#       box(width = 4,
#           "Module 1",
#           p(),
#           actionButton(ns("link_to_module1"), "Open Module 1")
#       ),
#       box(width = 4,
#           "Module 2"
#       ),
#       box(width = 4,
#           "Module 3"
#       )
#     ),
#     fluidRow(
#       box(width = 4,
#           "Module 4"
#       ),
#       box(width = 4,
#           "Module 5"
#       ),
#       box(width = 4,
#           "Module 6"
#       )
#     )
#   )
# }
# 
# dashboard <- function(input, output, session) {
#   observeEvent(input$link_to_module1, {
#     shinydashboard::updateTabItems(session, "explorertabs", "cell_content")
#   })
# }
