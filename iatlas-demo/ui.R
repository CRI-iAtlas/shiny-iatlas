################################################################################
# Multi-Widget Definitions
################################################################################
# Theme and details. Some elements are optional. Suffix is required.
# Attempts to return a single row with palette, theme, and optionally point-size and opacity.
# `addList` is a list of additional elements for UI, attempt to add to row.
theme_ui_details <- function(suffix, secTitle="Details", pal=TRUE, them=TRUE,
                             ptsz=FALSE, alpha=FALSE, addList=NULL) {
  elementList <- list(width = 12, h4(secTitle))
  if (pal) {
    elementList <- c(elementList, list(
      div(class = "col-md-4", uipal(paste0("pal", suffix)))
    ))
  }
  if (them) {
    elementList <- c(elementList, list(
      div(class = "col-md-4", uitheme(paste0("theme", suffix)))
    ))
  }
  if (ptsz) {
    elementList <- c(elementList, list(
      div(class = "col-md-3", uiptsz(paste0("size", suffix), class = "col-md-12"))
    ))
  }
  if (alpha) {
    elementList <- c(elementList, list(
      div(class = "col-md-3", uialpha(paste0("alpha", suffix), class = "col-md-12"))
    ))
  }
  # Add any additional row elements, if present
  elementList <- c(elementList, addList)
  return(fluidRow(do.call("column", args = elementList)))
}
################################################################################
# Define each fluid page
################################################################################

# explorepage = fluidPage(theme = shinytheme("sandstone"),
#                      headerPanel("Explore Data"),
#                      fluidRow(column(width = 12
#                      ))
# )

# Data I/O page
sbp_data <- sidebarPanel(
  h4("Select Data"),
  selectizeInput("disease_types",
    label = "Cancer types:",
    choices = "ALL types"
  ),
  selectizeInput("cohort",
    label = "Cohort:",
    choices = "ALL patients + samples"
  ),
  actionButton("more_filters", "Additional Sample Filters"),
  hr(),
  h4("Cohort Summary"),
  p(strong("33"), "diseases, ", strong("10,059"), "samples")
)

cellcontenttab <- tabPanel("Cell Content", cellcontent_UI("module1"))
immuneinterfacetab <- tabPanel("Clonal Diversity", immuneinterface_UI("module2"))
featurecorrelationtab <- tabPanel("Feature Correlations", featurecorrelation_UI("module3"))
featurecorrelationtab2 <- tabPanel("Feature Correlations", featurecorrelation_UI2("module6"))
survivaltab <- tabPanel("Survival Curves", survival_UI("module4"))
immunomodulatortab <- tabPanel("Immunomodulators", immunomodulator_UI("module5"))

explorepage <- fluidPage(
  theme = shinytheme("sandstone"),
  headerPanel("Explore TCGA Immune Characterizations"),
  column(
    width = 12,
    tabsetPanel(
      selected = "Cell Content",
      cellcontenttab,
      immuneinterfacetab,
      featurecorrelationtab,
      featurecorrelationtab2,
      survivaltab,
      immunomodulatortab
    )
  )
)

################################################################################
# Define general header tag list
# List of tags to display as a common header above all tabPanels.
################################################################################
headerTagList <- list(
  tags$style(type = "text/css", ".navbar .navbar-nav {float: right; font-size: 14px} .navbar .navbar-nav li a {font-size: 14px} .nav-tabs {font-size: 12px}"),
  tags$base(target = "_blank")
)
################################################################################
# Define the full user-interface, `ui`
################################################################################
ui <- navbarPage(
  title = strong("iATLAS"), selected = "Explore",
  tabPanel("Data", icon = icon("database")),
  tabPanel("Explore", explorepage, icon = icon("bar-chart")),
  tabPanel("Projects", icon = icon("flask")),
  tabPanel("Documentation", icon = icon("file-text")),
  tabPanel("Help", icon = icon("question")),
  tabPanel("Logout", icon = icon("sign-out")),
  header = headerTagList,
  collapsible = TRUE,
  windowTitle = "iATLAS"
)

shinyUI(ui)
################################################################################
