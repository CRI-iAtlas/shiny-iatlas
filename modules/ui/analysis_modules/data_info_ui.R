data_info_ui <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
        .GlobalEnv$titleBox("iAtlas Explorer — Data Description"),
        .GlobalEnv$textBox(
            width = 12,
            p("Each row in the table corresponds to a variable for which data are available for exploration in CRI iAtlas. Variables are organized into classes, as displayed in the Variable Class column. In a number of iAtlas modules the Variable Class will appear in drop-down selections for variables.")  
        ),
        sectionBox(
            title = "PanImmune Readouts",
            .GlobalEnv$messageBox(
                width = 12,
                p("Select a row in the table to view more details about variables in the same class."),
                p("Use the class selector to filter on the Variable class column")
            ),
            shiny::fluidRow(
                .GlobalEnv$optionsBox(
                    width = 6,
                    shiny::uiOutput(ns("classes")))),
            shiny::fluidRow(
                .GlobalEnv$tableBox(
                    width = 12,
                    shiny::div(
                        'feature_table' %>% 
                            ns() %>% 
                            DT::dataTableOutput() %>% 
                            shinycssloaders::withSpinner()
                    )
                )
            )
        ),
        shiny::uiOutput(ns("variable_details_section")) 
    )
}