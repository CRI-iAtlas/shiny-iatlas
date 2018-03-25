################################################################################
# Define each fluid page
################################################################################

explorepage <- dashboardPage(skin = "black",
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "iAtlas Explorer",   
                tabName = "dashboard", 
                icon = icon("dashboard")),
            menuItem(
                "Cell Content", 
                tabName = "cell_content", 
                icon = icon("th")),
            menuItem(
                "Clonal Diversity",    
                tabName = "clonal_diversity", 
                icon = icon("th")),
            menuItem(
                "Feature Correlations", 
                tabName = "feature_correlations", 
                icon = icon("th")),
            menuItem(
                "Survival Curves",
                tabName = "survival_curves", 
                icon = icon("th")),
            menuItem(
                "Immunomodulators",
                tabName = "immunomodulators", 
                icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    h2("Welcome to the iAtlas Portal prototype!")
            ),
            tabItem(tabName = "cell_content",
                    cellcontent_UI("module1")
            ),
            tabItem(tabName = "clonal_diversity",
                    immuneinterface_UI("module2")
            ),
            tabItem(tabName = "feature_correlations",
                    featurecorrelation_UI("module3")
            ),
            tabItem(tabName = "survival_curves",
                    survival_UI("module4")
            ),
            tabItem(tabName = "immunomodulators",
                    immunomodulator_UI("module5")
            )
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
  title = strong("CRI iAtlas Portal"), selected = "Explore",	
  tabPanel("Explore", explorepage, icon = icon("bar-chart")),	
  tabPanel("Documentation", icon = icon("file-text")),	
  tabPanel("Help", icon = icon("question")),	
  header = headerTagList,	
  collapsible = TRUE,	
  windowTitle = "iATLAS"	
)	

shinyUI(ui)