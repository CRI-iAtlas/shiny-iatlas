# data_table_module ----

data_table_module_UI <- function(
    id, 
    title = "", 
    message_html = ""
){
    
    ns <- NS(id)
    
    sectionBox(
        title = title,
        messageBox(width = 12, message_html),
        fluidRow(
            tableBox(
                width = 12,
                div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("data_table_module")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}

data_table_module <- function(
    input, 
    output, 
    session,
    df,
    options = list(pageLength = 10),
    color = F,
    color_column = NULL,
    colors = NULL,
    ...
){
    output$data_table_module <- DT::renderDT({
        print(df)
        dt <- DT::datatable(
            df,
            options = options,
            rownames = FALSE,
            ...
        )
        if(color){
           dt <-  DT::formatStyle(
               dt,
               color_column,
               backgroundColor = DT::styleEqual(colors, colors))
        }
        return(dt)
    })
}

# data_table_module2 ----

# data_table_module2_UI <- function(
#     id, 
#     title = "", 
#     message_html = ""
# ){
#     
#     ns <- NS(id)
#     
#     sectionBox(
#         title = title,
#         messageBox(width = 12, message_html),
#         # fluidRow(
#         #     optionsBox(
#         #         width = 6,
#         #         uiOutput(ns("classes")))),
#         fluidRow(
#             optionsBox(
#                 width = 6,
#                 selectInput(
#                     "class_choice",
#                     label = "Select Class",
#                     choices = c("All classes", "Overall Proportion")))),
#         fluidRow(
#             tableBox(
#                 width = 12,
#                 div(style = "overflow-x: scroll",
#                     DT::dataTableOutput(ns("data_table_module")) %>%
#                         shinycssloaders::withSpinner()
#                 )
#             )
#         )
#     )
# }
# 
# data_table_module2 <- function(
#     input, 
#     output, 
#     session,
#     df,
#     options = list(pageLength = 10),
#     color = F,
#     color_column = NULL,
#     colors = NULL,
#     ...
# ){
#     
#     # ns <- session$ns
#     # 
#     # output$classes <- renderUI({
#     #     choices <- panimmune_data$feature_df %>% 
#     #         magrittr::use_series(`Variable Class`) %>% 
#     #         sort %>% 
#     #         unique %>% 
#     #         c("All classes", .)
#     #     selectInput(
#     #         ns("class_choice"),
#     #         label = "Select Class",
#     #         choices = choices)
#     # })
#     
#     table_df <- reactive({
#         df <- panimmune_data$feature_df %>% 
#             dplyr::select(
#                 `Feature Name` = FriendlyLabel, 
#                 `Variable Class`, 
#                 Unit, 
#                 VariableType
#             ) 
#         print(input$class_choice)
#         if(input$class_choice != "All classes"){
#             df <- dplyr::filter(df, `Variable Class` == input$class_choice)
#         }
#         return(df)
#     })
#     
#     output$data_table_module <- DT::renderDT({
#         dt <- DT::datatable(
#             df,
#             options = options,
#             rownames = FALSE,
#             ...
#         )
#         if(color){
#             dt <-  DT::formatStyle(
#                 dt,
#                 color_column,
#                 backgroundColor = DT::styleEqual(colors, colors))
#         }
#         return(dt)
#     })
# }
