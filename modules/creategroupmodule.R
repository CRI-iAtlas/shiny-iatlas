# UI ----
creategroups_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox(""),
        textBox(
            width = 12,
            p("")  
        ),
        sectionBox(
            title = "",
            messageBox(
                width = 12,
                p(""), 
                p("")
            ),
            fluidRow(
                optionsBox(
                    width = 8,
                    fileInput(
                        ns("file1"),
                        "Choose CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                    ),
                    DT::dataTableOutput(
                        ns("user_group_df")),
                    style = "color:black"
                )
            )
        )
    )
}

# Server ----
creategroups <- function(input, output, session, ss_choice, subset_df) {
  
    user_group_df <- reactive({
        req(input$file1)
        tryCatch(
            {
                df <- readr::read_csv(input$file1$datapath)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        return(df)
    })
    
    output$user_group_df <- DT::renderDataTable(user_group_df())
    return(user_group_df)
    
}

