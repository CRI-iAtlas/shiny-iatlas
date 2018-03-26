explorepage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(id = "explorertabs",
      menuItem("iAtlas Explorer",   
               tabName = "dashboard", 
               icon = icon("dashboard")),
      selectInput(
        inputId = "ss_choice",
        label = "Select Sample Groups",
        choices = as.character(
          panimmune_data$sample_selection_choices
        ),
        selected = "Immune Subtype"
      ),
      menuItem("Analysis Modules",   
               icon = icon("bar-chart"), startExpanded = TRUE,
               menuSubItem(
                 "Sample Groups Overview",
                 tabName = "feature_correlations", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Tumor Composition", 
                 tabName = "cell_content", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Clinical Outcomes",
                 tabName = "survival_curves", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Genomic State", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Immune Interface",    
                 tabName = "clonal_diversity", 
                 icon = icon("chevron-circle-right")),
               menuSubItem(
                 "Immunomodulators",
                 tabName = "immunomodulators", 
                 icon = icon("chevron-circle-right"))
      ),
      menuItem("Data Description",
               icon = icon("th-list"),
               tabName = "datainfo"
      ),
      hr(),
      wellPanel(
        
        strong("Selected groups:"),
        textOutput("ss_choice")
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
            p("A summary of what's in the portal... (JE: might make more sense to include this on a 'Home' page for the portal"),
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
                imgLinkBox(
                  width = 6,
                  linkId = "link_to_module3",
                  title = "Sample Groups Overview",
                  imgSrc = "images/cell_content.png",
                  boxText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
                  linkText = "Open Module"
                ),
                imgLinkBox(
                  width = 6,
                  linkId = "link_to_module1",
                  title = "Tumor Composition",
                  imgSrc = "images/cell_content.png",
                  boxText = "Check out the Cell Content module for some box plots of cell types in different sample groups and stuff.",
                  linkText = "Open Module"
                )
              ),
              fluidRow(
                imgLinkBox(
                  width = 6,
                  linkId = "link_to_module4",
                  title = "Clinical Outcomes",
                  imgSrc = "images/cell_content.png",
                  boxText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
                  linkText = "Open Module"
                ),
                imgLinkBox(
                  width = 6,
                  linkId = "#",
                  title = "Genomic State",
                  imgSrc = "images/cell_content.png",
                  boxText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
                  linkText = "Open Module"
                )
              ),
              fluidRow(
                imgLinkBox(
                  width = 6,
                  linkId = "link_to_module2",
                  title = "Immune Interface",
                  imgSrc = "images/cell_content.png",
                  boxText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
                  linkText = "Open Module"
                ),
                imgLinkBox(
                  width = 6,
                  title = "Immunomodulators",
                  linkId = "link_to_module5",
                  imgSrc = "images/cell_content.png",
                  boxText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
                  linkText = "Open Module"
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