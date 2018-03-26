explorepage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(id = "explorertabs",
      menuItem("iAtlas Explorer",   
               tabName = "dashboard", 
               icon = icon("dashboard")),
      menuItem("Analysis Modules",   
               icon = icon("bar-chart"), startExpanded = TRUE,
               menuSubItem(
                 "Cell Content", 
                 tabName = "cell_content", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Clonal Diversity",    
                 tabName = "clonal_diversity", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Feature Correlations", 
                 tabName = "feature_correlations", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Survival Curves",
                 tabName = "survival_curves", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Immunomodulators",
                 tabName = "immunomodulators", 
                 icon = icon("chevron-circle-right"))
      ),
      menuItem("Data Description",
               icon = icon("th-list"),
               tabName = "datainfo"
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        titleBox("Welcome to the iAtlas Explorer!"),
        fluidRow(
          box(width = 12,
            p("Here, you can explore iAtlas data through interactive visualizations and detailed descriptions of individual features.")
          )
        ),
        fluidRow(
          box(width = 12,
            p("A summary of what's in the portal..."),
            fluidRow(
              infoBox("Features", 78, width = 4, color = "black", fill = TRUE),
              infoBox("Data Types", 10, width = 4, color = "black", fill = TRUE),
              infoBox("TCGA Cancers", 33, width = 4, color = "black", fill = TRUE)
            )
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Analysis Modules",
              solidHeader = TRUE, status = "warning",
              p("Select module to explore data."),
              fluidRow(
                box(
                  width = 6,
                  title = "Cell Content",
                  solidHeader = TRUE, status = "primary",
                  fluidRow(
                    column(
                      width = 4,
                      shiny::img(src = "images/cell_content.png", width = "100%")
                    ),
                    column(
                      width = 8,
                      p("Check out the Cell Content module for some box plots of cell types in different sample groups and stuff."),
                      actionButton("link_to_module1", "Open Module")
                    )
                  )
                ),
                box(
                  width = 6,
                  title = "Clonal Diversity",
                  solidHeader = TRUE, status = "primary",
                  fluidRow(
                    column(
                      width = 4,
                      shiny::img(src = "images/cell_content.png", width = "100%")
                    ),
                    column(
                      width = 8,
                      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."),
                      actionButton("link_to_module2", "Open Module")
                    )
                  )
                )
              ),
              fluidRow(
                box(width = 6,
                    title = "Feature Correlations",
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column(
                        width = 4,
                        shiny::img(src = "images/cell_content.png", width = "100%")
                      ),
                      column(
                        width = 8,
                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."),
                        actionButton("link_to_module3", "Open Module")
                      )
                    )
                ),
                box(width = 6,
                    title = "Survival Curves",
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column(
                        width = 4,
                        shiny::img(src = "images/cell_content.png", width = "100%")
                      ),
                      column(
                        width = 8,
                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."),
                        actionButton("link_to_module4", "Open Module")
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 6,
                    title = "Immunomodulators",
                    solidHeader = TRUE, status = "primary",
                    fluidRow(
                      column(
                        width = 4,
                        shiny::img(src = "images/cell_content.png", width = "100%")
                      ),
                      column(
                        width = 8,
                        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."),
                        actionButton("link_to_module5", "Open Module")
                      )
                    )
                )
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
      ),
      tabItem(tabName = "datainfo",
              datainfo_UI("moduleX"))
    )
  )
)