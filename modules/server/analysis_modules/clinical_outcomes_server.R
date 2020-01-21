clinical_outcomes_server <- function(
    input, 
    output, 
    session, 
    feature_values_con,
    features_con,
    groups_con,
    group_name,
    cohort_colors
){
    source("modules/server/submodules/clinical_outcomes_survival_server.R", local = T)
    source("modules/server/submodules/clinical_outcomes_heatmap_server.R", local = T)
    
    ns <- session$ns
    
    shiny::callModule(
        clinical_outcomes_survival_server, 
        "clinical_outcomes_survival",
        feature_values_con,
        features_con,
        groups_con,
        group_name,
        cohort_colors
    )
    
    shiny::callModule(
        clinical_outcomes_heatmap_server, 
        "clinical_outcomes_heatmap",
        feature_values_con,
        features_con,
        groups_con,
        group_name,
        cohort_colors
    )
}


