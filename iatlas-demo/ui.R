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
  includeCSS("www/custom.css"),
  title = strong("CRI iAtlas Portal"), selected = "Explore",	
  tabPanel("Explore", explorepage, icon = icon("bar-chart")),	
  tabPanel("About", aboutpage, icon = icon("info-circle")),	
  tabPanel("Documentation", icon = icon("file-text")),	
  tabPanel("Resources", icon = icon("link")),	
  header = headerTagList,	
  collapsible = TRUE,	inverse = TRUE,
  windowTitle = "CRI iAtlas Portal"	
)	

shinyUI(ui)