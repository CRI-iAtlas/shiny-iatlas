explorepage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem("iAtlas Explorer",   
               tabName = "dashboard", 
               icon = icon("dashboard")),
      menuItem(startExpanded = TRUE,
               "Modules",   
               icon = icon("bar-chart"),
               menuSubItem(
                 "Cell Content", 
                 tabName = "cell_content", 
                 icon = icon("th")),
               menuSubItem(
                 "Clonal Diversity",    
                 tabName = "clonal_diversity", 
                 icon = icon("th")),
               menuSubItem(
                 "Feature Correlations", 
                 tabName = "feature_correlations", 
                 icon = icon("th")),
               menuSubItem(
                 "Survival Curves",
                 tabName = "survival_curves", 
                 icon = icon("th")),
               menuSubItem(
                 "Immunomodulators",
                 tabName = "immunomodulators", 
                 icon = icon("th"))
      )
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