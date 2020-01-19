cohort_selection_ui <- function(id) {
    
    ns <- NS(id)
    
    source("modules/ui/submodules/data_table_ui.R", local = T)
    source("modules/ui/submodules/insert_remove_element_ui.R", local = T)
    
    tagList(
        titleBox("iAtlas Explorer — Cohort Selection"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/sample_groups.markdown")
        ),
        # cohort selection ----------------------------------------------------
        sectionBox(
            title = "Cohort Selection",
            messageBox(
                width = 12,
                p("Group Selection and filtering"),
            ),
            # dataset selection ----
            optionsBox(
                width = 4,
                checkboxInput(
                    inputId = ns("select_by_module"),
                    label = strong("Select By Module?"),
                ),
                conditionalPanel(
                    condition = "input.select_by_module",
                    uiOutput(ns("module_selection_ui")),
                    ns = ns
                ),
                uiOutput(ns("dataset_selection_ui")),
            ),
            messageBox(
                width = 12,
                textOutput(ns("module_availibility_string"))
            ),
            # sample filtering ----
            optionsBox(
                width = 12,
                insert_remove_element_ui(
                    ns("group_filter"), 
                    "Add group filter"
                )
            ),
            optionsBox(
                width = 12,
                insert_remove_element_ui(
                    ns("numeric_filter"), 
                    "Add numeric filter"
                )
            ),
            tableBox(
                width = 12,
                textOutput(ns("samples_text"))
            ),
            # group selection ----
            optionsBox(
                width = 12,
                uiOutput(ns("select_group_ui")),
                conditionalPanel(
                    condition = "output.display_custom_numeric",
                    uiOutput(ns("select_custom_numeric_group_ui")),
                    sliderInput(
                        inputId = ns("custom_numeric_group_number_choice"),
                        label = "Select number of divsions",
                        min = 2,
                        max = 10,
                        value = 2,
                        step = 1
                    ),
                    ns = ns
                ),
                conditionalPanel(
                    condition = "output.display_custom_mutation",
                    uiOutput(ns("select_custom_mutation_group_ui")),
                    ns = ns
                ),
            ),
        ),
        
        # group key -----------------------------------------------------------
        data_table_ui(
            ns("sg_table"),
            title = "Group Key",
            message_html = p(stringr::str_c(
                "This displays attributes and annotations of your choice of",
                "groups.",
                sep = " "
            ))
        ),
        # group overlap -------------------------------------------------------
        # sectionBox(
        #     title = "Group Overlap",
        #     messageBox(
        #         width = 12,
        #         includeMarkdown("data/markdown/sample_groups_overlap.markdown")
        #     ),
        #     fluidRow(
        #         optionsBox(
        #             width = 12,
        #             uiOutput(ns("mosaic_group_select")),
        #             uiOutput(ns("mosaic_subset_select"))
        #         ),
        #     ),
        #     fluidRow(
        #         plotBox(
        #             width = 12,
        #             column(
        #                 width = 12,
        #                 plotlyOutput(ns("mosaicPlot"), height = "600px") %>%
        #                     shinycssloaders::withSpinner()
        #             )
        #         )
        #     )
        # )
    )
}