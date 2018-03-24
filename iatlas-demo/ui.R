dashboardPage(
    dashboardHeader(title = "iATLAS"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Dashboard",   
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
                    h2("Welcome to the iATLAS portal prototype!")
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