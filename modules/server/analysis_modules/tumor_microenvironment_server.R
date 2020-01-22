tumor_microenvironment_server <- function(
    input,
    output, 
    session, 
    sample_tbl,
    group_tbl
) {
    
    source("modules/server/submodules/overall_cell_proportions_server.R", local = T)
    source("modules/server/submodules/cell_type_fractions_server.R", local = T)
    
    shiny::callModule(
        overall_cell_proportions_server, 
        "overall_cell_proportions",
        sample_tbl,
        group_tbl
    )

    shiny::callModule(
        cell_type_fractions_server, 
        "cell_type_fractions",
        sample_tbl,
        group_tbl
    )
}