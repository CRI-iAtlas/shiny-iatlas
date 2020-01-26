cohort_selection_ui <- function(id) {
    
    ns <- shiny::NS(id)
    
    source("modules/ui/submodules/cohort_group_selection_ui.R", local = T)
    source("modules/ui/submodules/cohort_filter_selection_ui.R", local = T)
    source("modules/ui/submodules/cohort_dataset_selection_ui.R", local = T)
    source("modules/ui/submodules/data_table_ui.R", local = T)
    
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
            cohort_dataset_selection_ui(ns("cohort_dataset_selection")),
            cohort_filter_selection_ui(ns("cohort_filter_selection")),
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