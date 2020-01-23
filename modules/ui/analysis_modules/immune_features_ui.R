immune_features_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    source(
        "modules/ui/submodules/immune_feature_distributions_ui.R",
        local = T
    )
    
    source(
        "modules/ui/submodules/immune_feature_correlations_ui.R",
        local = T
    )
    
    shiny::tagList(
        .GlobalEnv$titleBox("iAtlas Explorer — Immune Feature Trends"),
        .GlobalEnv$textBox(
            width = 12,
            shiny::p(stringr::str_c(
                "This module allows you to see how immune readouts vary ",
                "across your groups, and how they relate to one another."
            ))  
        ),
        immune_feature_distributions_ui(ns("immune_feature_distributions")),
        immune_feature_correlations_ui(ns("immune_feature_correlations"))
    )
}