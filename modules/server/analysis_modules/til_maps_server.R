til_maps_server <- function(
    input, 
    output, 
    session,
    sample_tbl,
    group_tbl,
    group_name,
    plot_colors
){
    
    source(
        "modules/server/submodules/til_map_distributions_server.R",
        local = T
    )
    source(
        "modules/server/submodules/data_table_server.R",
        local = T
    )
    
    shiny::callModule(
        til_map_distributions_server,
        "til_map_distributions",
        sample_tbl,
        group_tbl,
        group_name,
        plot_colors
    )
    
    tilmap_tbl <- shiny::reactive({
        subquery1 <- "SELECT id FROM classes WHERE name = 'TIL Map Characteristic'"
        
        subquery2 <- paste(
            "SELECT id AS feature FROM features",
            "WHERE class_id = (",
            subquery1,
            ")"
        )
        
        subquery3 <- paste(
            "SELECT feature_id, sample_id, value FROM features_to_samples",
            "WHERE feature_id IN (",
            subquery2,
            ")"
        )
        
        query <- paste(
            "SELECT a.sample_id, a.value, b.display, c.tissue_id FROM",
            "(", subquery3, ") a",
            "INNER JOIN",
            "(SELECT id, display from features) b",
            "ON a.feature_id = b.id",
            "INNER JOIN",
            "(SELECT * FROM samples) c",
            "ON a.sample_id = c.id"
        )
        
        query %>%
            dplyr::sql() %>% 
            .GlobalEnv$perform_query("build feature table") %>%
            dplyr::inner_join(sample_tbl(), by = "sample_id") %>% 
            dplyr::mutate(value = round(value, digits = 1)) %>%
            dplyr::select(
                tissue_id,
                Sample = sample_name,
                `Selected Group` = group,
                display,
                value
            ) %>%
            tidyr::pivot_wider(names_from = display, values_from = value) %>% 
            dplyr::mutate(Image = stringr::str_c(
                "<a href=\"",
                "https://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
                tissue_id,
                "\">",
                tissue_id,
                "</a>"
            )) %>% 
            dplyr::select(-tissue_id)
    })

    
    shiny::callModule(data_table_server, "til_table", tilmap_tbl, escape = F)
    
    
    
}