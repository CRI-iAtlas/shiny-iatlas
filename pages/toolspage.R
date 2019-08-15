toolspage <- fluidPage(
  br(),
  navlistPanel(
    widths = c(3, 9), well = FALSE,
    id = "toolstabs",
    tabPanel(
      h5("iAtlas Tools Home"),
      titleBox("iAtlas Explorer — Tools"),
      fluidRow(
        imgLinkBox(
          width = 12,
          title = "Immune Subtype Classifier",
          linkId = "link_to_module_subtypeclassifier",
          imgSrc = "images/immunomodulators.png",
          boxText = "Use an ensemble model trained on all TCGA data to classify gene expression profiles from your own data into the six immune subtypes identified in Thorsson et al.",
          linkText = "Open Tool"
        )
      )
    ),
    tabPanel(
      "Immune Subtype Classifier",
      subtypeclassifier_UI("module_subtypeclassifier")
    )
  )
)
