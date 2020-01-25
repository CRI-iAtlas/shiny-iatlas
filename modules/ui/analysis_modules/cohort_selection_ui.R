cohort_selection_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    source("modules/ui/submodules/cohort_group_selection_ui.R", local = T)
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
            cohort_group_selection_ui(ns("cohort_group_selection"))

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
        #                 plotly::plotlyOutput(ns("mosaicPlot"), height = "600px") %>%
        #                     shinycssloaders::withSpinner()
        #             )
        #         )
        #     )
        # )
    )
}