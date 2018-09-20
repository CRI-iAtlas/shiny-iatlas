toolspage <- fluidPage(
  br(),
  titleBox("Tools"),
  fluidRow(
    imgLinkBox(
      width = 12,
      linkId = "link_to_classifiertool",
      title = "Immune Subtypes Classification",
      imgSrc = "images/immunefeatures.png",
      boxText = "Use this tool to classify gene expression profiles from your own data into the six immune subtypes identified in Thorsson et al.",
      linkText = "Open Tool"
    )
  )
)