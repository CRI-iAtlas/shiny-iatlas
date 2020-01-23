immune_features_server <- function(
    input,
    output, 
    session,
    sample_tbl,
    group_tbl,
    group_name,
    feature_named_list,
    plot_colors
){
    
    source(
        "modules/server/submodules/immune_feature_distributions_server.R",
        local = T
    )
    source(
        "modules/server/submodules/immune_feature_correlations_server.R",
        local = T
    )
    
    shiny::callModule(
        immune_feature_distributions_server,
        "immune_feature_distributions",
        sample_tbl,
        group_tbl,
        group_name,
        feature_named_list,
        plot_colors
    )
    

    shiny::callModule(
        immune_feature_correlations_server,
        "immune_feature_correlations",
        sample_tbl,
        group_tbl,
        feature_named_list
    )
}