explorepage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(id = "explorertabs",
      menuItem("iAtlas Explorer Dashboard",   
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
      tabItem(
        tabName = "dashboard",
        box(width = 12, background = "black",
          span(strong("Welcome to the iAtlas Portal Explorer!"),
               style = "font-size:18px")
        ),
        box(width = 12,
            title = "Select module to explore data...",
            solidHeader = TRUE, status = "warning",
            fluidRow(
              box(width = 4,
                  title = "Cell Content",
                  solidHeader = TRUE, status = "primary",
                  actionButton("link_to_module1", "Open Module")
              ),
              box(width = 4,
                  title = "Clonal Diversity",
                  solidHeader = TRUE, status = "primary",
                  actionButton("link_to_module2", "Open Module")
              ),
              box(width = 4,
                  title = "Feature Correlations",
                  solidHeader = TRUE, status = "primary",
                  actionButton("link_to_module3", "Open Module")
              )
            ),
            fluidRow(
              box(width = 4,
                  title = "Survival Curves",
                  solidHeader = TRUE, status = "primary",
                  actionButton("link_to_module4", "Open Module")
              ),
              box(width = 4,
                  title = "Immunomodulators",
                  solidHeader = TRUE, status = "primary",
                  actionButton("link_to_module5", "Open Module")
              )
            )
        )
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