# UI ----
cellcontent_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Tumor Microenvironment"),
        textBox(
            width = 12,
            p("Explore the immune cell proportions in your sample groups.")  
        ),
        overall_cell_proportions_module_UI(ns("ocp_module")),
        cell_type_fractions_module_UI(ns("ctf_module"))
    )
}

# Server ----
cellcontent <- function(
    input,
    output, 
    session, 
    group_display_choice, 
    group_internal_choice, 
    sample_group_df,
    subset_df) {
    
    callModule(
        overall_cell_proportions_module, 
        "ocp_module",
        group_display_choice, 
        group_internal_choice, 
        sample_group_df,
        subset_df)
    
    callModule(
        cell_type_fractions_module, 
        "ctf_module",
        group_display_choice, 
        group_internal_choice, 
        sample_group_df,
        subset_df)
}

