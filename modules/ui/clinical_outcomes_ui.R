clinical_outcomes_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Clinical Outcomes"),
        textBox(
            width = 12,
            p("Plot survival curves based on immune characteristics and identify variables associated with outcome.")  
        ),
        
        # Survival comparison section ----
        sectionBox(
            title = "Sample Group Survival",
            messageBox(
                width = 12,
                p("Select the variable, and outcome in terms of either overall survival (OS) or progression free interval (PFI) endpoints to generate a Kaplan-Meier plot. For a continuous (numeric) variable, the slider can be used to specify how the range of values of that variable is split.  Selecting 2 splits the values by the middle of the range, 3 splits the range into three even intervals and so on."),
                p("For immune subtypes Figure 3A can be generated (OS), and Figure S3A for (PFI).")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 8,
                        selectInput(
                            ns("suvivial_time_feature_choice"),
                            "Select or Search for Survival Endpoint",
                            c("Overall Survival" = "OS Time", "Progression Free Interval" = "PFI Time"),
                            selected = "OS Time"
                        )
                    ),
                    column(
                        width = 2,
                        checkboxInput(ns("confint"), "Confidence Intervals", value = F)
                    ),
                    column(
                        width = 2,
                        checkboxInput(ns("risktable"), "Risk Table", value = T)
                    )
                ),
                
                # ** Survival Kaplan-Meier plot ----
                plotBox(
                    width = 12,
                    plotOutput(ns("survival_plot"), height = 600) %>%
                        shinycssloaders::withSpinner()
                )
            )
        ),
        
        # Survival comparison section ----
        sectionBox(
            title = "Concordance Index",
            messageBox(
                width = 12,
                p("Here, you can explore which variables are associated with improved or diminished survival within your sample groups. Select a variable class, and you will get a heatmap, with one row for each variable in that class. For a given variable (row) and sample group (column) red denotes decreased survival, and blue increased survival as the variable is increased."),
                p("Manuscript context:  Selecting variable class “Core Expression Signature”, you can generate Figure 3B. Figures 3C, and Figures S3B, S3C, and S3C can also be generated with different selection options.")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 6,
                        selectInput(
                            ns("heatmap_time_feature_choice"),
                            "Select or Search for Survival Endpoint",
                            c("Overall Survival" = "OS Time", "Progression Free Interval" = "PFI Time"),
                            selected = "OS Time"
                        ),
                    ),
                    column(
                        width = 6,
                        uiOutput(ns("survival_class_opts"))
                    )
                ),
                plotBox(
                    width = 12,
                    fluidRow(
                        plotlyOutput(ns("heatmapplot"), height = 600) %>%
                            shinycssloaders::withSpinner(),
                        p(),
                        textOutput(ns("heatmap_group_text"))
                    )
                )
            )
        )
    )
}