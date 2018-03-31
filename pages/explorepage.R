explorepage <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    sidebarMenu(
      id = "explorertabs",
      menuItem("CRI iAtlas Explorer",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      selectInput(
        inputId = "ss_choice",
        label = "Select Sample Groups",
        choices = as.character(
          panimmune_data$sample_group_names
        ),
        selected = "Immune Subtype"
      ),
      uiOutput("study_subset_UI"),
      menuItem("Analysis Modules",
        icon = icon("bar-chart"), startExpanded = TRUE,
        menuSubItem(
          "Sample Group Overview",
          tabName = "feature_correlations",
          icon = icon("chevron-circle-right")
        ),
        menuSubItem(
          "Tumor Microenvironment",
          tabName = "cell_content",
          icon = icon("chevron-circle-right")
        ),
        menuSubItem(
          "Immune Feature Trends",
          tabName = "immune_features",
          icon = icon("chevron-circle-right")
        ),
        menuSubItem(
          "Clinical Outcomes",
          tabName = "survival_curves",
          icon = icon("chevron-circle-right")
        ),
        # menuSubItem(
        #   "Genomic State",
        #   icon = icon("chevron-circle-right")),
        # menuSubItem(
        #   "Immune Interface",
        #   tabName = "clonal_diversity",
        #   icon = icon("chevron-circle-right")),
        menuSubItem(
          "Immunomodulators",
          tabName = "immunomodulators",
          icon = icon("chevron-circle-right")
        )
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
        titleBox("Welcome to the CRI iAtlas Explorer!"),
        fluidRow(
          box(
            width = 12,
            p("Here you can explore CRI iAtlas data through interactive visualizations.")
          )
        ),
        fluidRow(
          box(
            width = 12,
            p("A quick summary of what's in the portal..."),
            fluidRow(
              infoBox("Immune Readouts", 86, width = 4, color = "black", fill = TRUE),
              infoBox("Classes of Readouts", 12, width = 4, color = "black", fill = TRUE),
              infoBox("TCGA Cancers", 33, width = 4, color = "black", fill = TRUE)
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Analysis Modules",
            solidHeader = TRUE, status = "warning",
            p("Select a module to explore data. You can organize the data by choosing how to divide tumor samples: by TCGA tumor projects, by TCGA molecular subtypes, or by immune subtypes that span multiple tumor types.  Start by  making your selection with the dropdown selection box on the upper left. Use the Data Description to learn which immune readouts are available. "),
            fluidRow(
              imgLinkBox(
                width = 6,
                linkId = "link_to_module3",
                title = "Sample Group Overview",
                imgSrc = "images/cell_content.png",
                boxText = "This module provides short summaries of your selected groups, and allows you to see how they overlap with other groups.",
                linkText = "Open Module"
              ),
              imgLinkBox(
                width = 6,
                linkId = "link_to_module1",
                title = "Tumor Microenvironment",
                imgSrc = "images/cell_content.png",
                boxText = "Explore the immune cell proportions in your sample groups.",
                linkText = "Open Module"
              )
            ),
            fluidRow(
                imgLinkBox(
                    width = 6,
                    title = "Immune Feature Trends",
                    linkId = "link_to_module6",
                    imgSrc = "images/cell_content.png",
                    boxText = "This module allows you to see how immune readouts vary across your groups, and how they relate to one another.",
                    linkText = "Open Module"
                ),
              imgLinkBox(
                width = 6,
                linkId = "link_to_module4",
                title = "Clinical Outcomes",
                imgSrc = "images/cell_content.png",
                boxText = "Plot survival curves based on immune characteristics and identify variables associated with outcome.",
                linkText = "Open Module"
              )
              #   imgLinkBox(
              #     width = 6,
              #     linkId = "#",
              #     title = "Genomic State",
              #     imgSrc = "images/cell_content.png",
              #     boxText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
              #     linkText = "Open Module"
              #   )
              # ),
              # fluidRow(
              #   imgLinkBox(
              #     width = 6,
              #     linkId = "link_to_module2",
              #     title = "Immune Interface",
              #     imgSrc = "images/cell_content.png",
              #     boxText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
              #     linkText = "Open Module"
              #   ),

              ),
              fluidRow(
                  imgLinkBox(
                      width = 6,
                      title = "Immunomodulators",
                      linkId = "link_to_module5",
                      imgSrc = "images/cell_content.png",
                      boxText = "Explore the expression of genes that code for immunomodulating proteins, including checkpoint proteins.",
                      linkText = "Open Module"
                  )
              )
          )
        )
      ),
      tabItem(
        tabName = "cell_content",
        cellcontent_UI("module1")
      ),
      tabItem(
        tabName = "clonal_diversity",
        immuneinterface_UI("module2")
      ),
      tabItem(
        tabName = "feature_correlations",
        groupsoverview_UI("module3")
      ),
      tabItem(
        tabName = "survival_curves",
        survival_UI("module4")
      ),
      tabItem(
        tabName = "immunomodulators",
        immunomodulator_UI("module5")
      ),
      tabItem(
          tabName = "immune_features",
          immunefeatures_UI("module6")
      ),
      tabItem(
        tabName = "datainfo",
        datainfo_UI("moduleX")
      )
    )
  )
)