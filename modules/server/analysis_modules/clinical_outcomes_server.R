clinical_outcomes_server <- function(
    input, 
    output, 
    session, 
    sample_tbl,
    group_tbl,
    group_name,
    plot_colors
){
    source("modules/server/submodules/clinical_outcomes_survival_server.R", local = T)
    source("modules/server/submodules/clinical_outcomes_heatmap_server.R", local = T)
    
    ns <- session$ns
    
    shiny::callModule(
        clinical_outcomes_survival_server, 
        "clinical_outcomes_survival",
        sample_tbl,
        group_tbl,
        group_name,
        plot_colors
    )
    
    shiny::callModule(
        clinical_outcomes_heatmap_server, 
        "clinical_outcomes_heatmap",
        sample_tbl,
        group_tbl,
        group_name,
        plot_colors
    )
}


