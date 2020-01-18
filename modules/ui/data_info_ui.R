data_info_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Data Description"),
        textBox(
            width = 12,
            p("Each row in the table corresponds to a variable for which data are available for exploration in CRI iAtlas. Variables are organized into classes, as displayed in the Variable Class column. In a number of iAtlas modules the Variable Class will appear in drop-down selections for variables.")  
        ),
        sectionBox(
            title = "PanImmune Readouts",
            messageBox(
                width = 12,
                p("Select a row in the table to view more details about variables in the same class."),
                p("Use the class selector to filter on the Variable class column")
            ),
            fluidRow(
                optionsBox(
                    width = 6,
                    uiOutput(ns("classes")))),
            fluidRow(
                tableBox(
                    width = 12,
                    div(
                        DT::dataTableOutput(ns('feature_table')) %>% 
                            shinycssloaders::withSpinner()
                    )
                )
            )
        ),
        uiOutput(ns("variable_details_section")) 
    )
}