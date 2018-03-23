
################################################################################
# Define each fluid page
################################################################################

cellcontenttab <- tabPanel("Cell Content", cellcontent_UI("module1"))
immuneinterfacetab <- tabPanel("Clonal Diversity", immuneinterface_UI("module2"))
featurecorrelationtab <- tabPanel("Feature Correlations", featurecorrelation_UI("module3"))
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
  title = strong("CRI iAtlas"), selected = "Explore",
  tabPanel("Explore", explorepage, icon = icon("bar-chart")),
  tabPanel("Documentation", icon = icon("file-text")),
  tabPanel("Help", icon = icon("question")),
  header = headerTagList,
  collapsible = TRUE,
  windowTitle = "iATLAS"
)

shinyUI(ui)
################################################################################
