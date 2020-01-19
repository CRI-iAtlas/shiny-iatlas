source("modules/ui/analysis_modules/clinical_outcomes_ui.R", local = T)
source("modules/ui/analysis_modules/cohort_selection_ui.R", local = T)
source("modules/ui/analysis_modules/data_info_ui.R", local = T)
source("modules/ui/analysis_modules/driver_associations_ui.R", local = T)  
source("modules/ui/analysis_modules/immune_features_ui.R", local = T)
source("modules/ui/analysis_modules/immunomodulators_ui.R", local = T)
source("modules/ui/analysis_modules/io_targets_ui.R", local = T)
source("modules/ui/analysis_modules/til_maps_ui.R", local = T)
source("modules/ui/analysis_modules/tumor_microenvironment_ui.R", local = T)

explorepage <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(disable = TRUE),
    shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
            id = "explorertabs",
            shinydashboard::menuItem(
                "iAtlas Explorer Home",
                tabName = "dashboard",
                icon = shiny::icon("dashboard")
            ),
            shinydashboard::menuItem(
                "Analysis Modules",
                icon = shiny::icon("bar-chart"), 
                startExpanded = TRUE,
                shinydashboard::menuSubItem(
                    "Cohort Selection",
                    tabName = "cohort_selection",
                    icon = shiny::icon("cog")
                ),
                shinydashboard::menuSubItem(
                    "Tumor Microenvironment",
                    tabName = "tumor_microenvironment",
                    icon = shiny::icon("cog")
                ),
                shinydashboard::menuSubItem(
                    "Immune Feature Trends",
                    tabName = "immune_features",
                    icon = shiny::icon("cog")
                ),
                shinydashboard::menuSubItem(
                    "Clinical Outcomes",
                    tabName = "clinical_outcomes",
                    icon = shiny::icon("cog")
                ),
                shinydashboard::menuSubItem(
                    "Immunomodulators",
                    tabName = "immunomodulators",
                    icon = shiny::icon("cog")
                ),
                shinydashboard::menuSubItem(
                    "IO Targets",
                    tabName = "io_targets",
                    icon = shiny::icon("cog")
                ),
                shinydashboard::menuSubItem(
                    "TIL Maps",
                    tabName = "til_maps",
                    icon = shiny::icon("cog")
                ),
                shinydashboard::menuSubItem(
                    "Driver Associations",
                    tabName = "driver_associations",
                    icon = shiny::icon("cog")
                )
            ),
            shinydashboard::menuItem(
                "Data Description",
                icon = shiny::icon("th-list"),
                tabName = "data_info"
            )
        )
    ),
    shinydashboard::dashboardBody(
        shinydashboard::tabItems(
            shinydashboard::tabItem(
                tabName = "dashboard",
                .GlobalEnv$titleBox("iAtlas Explorer — Home"),
                .GlobalEnv$textBox(
                    width = 12,
                    shiny::includeMarkdown("www/md/explore.md")
                ),
                .GlobalEnv$sectionBox(
                    title = "What's Inside",
                    shiny::fluidRow(
                        shinydashboard::infoBox(
                            "Immune Readouts:", 
                            86, 
                            width = 3, 
                            color = "black", 
                            fill = FALSE, 
                            icon = icon("search")
                        ),
                        shinydashboard::infoBox(
                            "Classes of Readouts:", 
                            12, 
                            width = 3, 
                            color = "black", 
                            fill = FALSE, 
                            icon = icon("filter")
                        ),
                        shinydashboard::infoBox(
                            "TCGA Cancers:",
                            33, 
                            width = 3, 
                            color = "black", 
                            fill = FALSE, 
                            icon = icon("flask")
                        ),
                        shinydashboard::infoBox(
                            "TCGA Samples:", 
                            "11,080", 
                            width = 3, 
                            color = "black", 
                            fill = FALSE, 
                            icon = icon("users")
                        )
                    )
                ),
                .GlobalEnv$sectionBox(
                    title = "Analysis Modules",
                    .GlobalEnv$messageBox(
                        width = 12,
                        shiny::p(
                            "Each module presents information organized by theme, with multiple views and interactive controls.",
                            "Within each module, you can find ",
                            shiny::strong("“Manuscript Context”"), " describing how that module can generate figures analogous to those in the manuscript ", 
                            shiny::em("Thorsson et al., The Immune Landscape of Cancer, Immunity (2018).")
                        )
                    ),
                    shiny::fluidRow(
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            linkId = "link_to_cohort_selection",
                            title = "Cohort Selection",
                            imgSrc = "images/groupsoverview.png",
                            boxText = "Use this module to create a cohort of interest.",
                            linkText = "Open Module"
                        ),
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            linkId = "link_to_tumor_microenvironment",
                            title = "Tumor Microenvironment",
                            imgSrc = "images/cellcontent.png",
                            boxText = "Explore the immune cell proportions in your sample groups.",
                            linkText = "Open Module"
                        )
                    ),
                    shiny::fluidRow(
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            title = "Immune Feature Trends",
                            linkId = "link_to_immune_features",
                            imgSrc = "images/immunefeatures.png",
                            boxText = "This module allows you to see how immune readouts vary across your groups, and how they relate to one another.",
                            linkText = "Open Module"
                        ),
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            linkId = "link_to_clinical_outcomes",
                            title = "Clinical Outcomes",
                            imgSrc = "images/survival.png",
                            boxText = "Plot survival curves based on immune characteristics and identify variables associated with outcome.",
                            linkText = "Open Module"
                        )
                        
                    ),
                    shiny::fluidRow(
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            title = "Immunomodulators",
                            linkId = "link_to_immunomodulators",
                            imgSrc = "images/immunomodulators.png",
                            boxText = "Explore the expression of genes that code for immunomodulating proteins, including checkpoint proteins.",
                            linkText = "Open Module"
                        ),
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            title = "TIL Maps",
                            linkId = "link_to_til_maps",
                            imgSrc = "images/TILmap.png",
                            boxText = "Explore the characteristics of maps of tumor infiltrating lymphocytes obtained from analysis of H&E images.",
                            linkText = "Open Module"
                        )
                    ),
                    shiny::fluidRow(
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            title = "Driver Associations",
                            linkId = "link_to_driver_associations",
                            imgSrc = "images/drivers.png",
                            boxText = "Explore Associations of Microenvironment with Driver Mutations.",
                            linkText = "Open Module"
                        ),
                        .GlobalEnv$imgLinkBox(
                            width = 6,
                            title = "IO Targets",
                            linkId = "link_to_io_targets",
                            imgSrc = "images/iotargets.png",
                            boxText = "Explore the expression of genes that code for immuno-oncological (IO) targets .",
                            linkText = "Open Module"
                        )
                    )
                )
            ),
            shinydashboard::tabItem(
                tabName = "tumor_microenvironment",
                tumor_microenvironment_ui("tumor_microenvironment")
            ),
            shinydashboard::tabItem(
                tabName = "cohort_selection",
                cohort_selection_ui("cohort_selection")
            ),
            shinydashboard::tabItem(
                tabName = "clinical_outcomes",
                clinical_outcomes_ui("clinical_outcomes")
            ),
            shinydashboard::tabItem(
                tabName = "immunomodulators",
                immunomodulators_ui("immunomodulators")
            ),
            shinydashboard::tabItem(
                tabName = "immune_features",
                immune_features_ui("immune_features")
            ),
            shinydashboard::tabItem(
                tabName = "driver_associations",
                driver_associations_ui("driver_associations")
            ),
            shinydashboard::tabItem(
                tabName = "til_maps",
                til_maps_ui("til_maps")
            ),
            shinydashboard::tabItem(
                tabName = "io_targets",
                io_targets_ui("io_targets")
            ),
            shinydashboard::tabItem(
                tabName = "data_info",
                data_info_ui("data_info")
            )
        )
    )
)