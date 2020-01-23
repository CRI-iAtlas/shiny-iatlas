source("modules/ui/tool_modules/immune_subtype_classifier_ui.R", local = T)

toolspage <- shiny::fluidPage(
    shiny::br(),
    shiny::navlistPanel(
        widths = c(3, 9), 
        well = FALSE,
        id = "toolstabs",
        shiny::tabPanel(
            shiny::h5("iAtlas Tools Home"),
            .GlobalEnv$titleBox("iAtlas Explorer — Tools"),
            shiny::fluidRow(
                .GlobalEnv$imgLinkBox(
                    width = 12,
                    title = "Immune Subtype Classifier",
                    linkId = "link_to_immune_subtype_classifier",
                    imgSrc = "images/immunomodulators.png",
                    boxText = "Use an ensemble model trained on all TCGA data to classify gene expression profiles from your own data into the six immune subtypes identified in Thorsson et al.",
                    linkText = "Open Tool"
                )
            )
        ),
        shiny::tabPanel(
            "Immune Subtype Classifier",
            immune_subtype_classifier_ui("immune_subtype_classifier")
        )
    )
)
