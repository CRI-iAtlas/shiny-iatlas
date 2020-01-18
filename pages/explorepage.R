
module_uis <- list.files("modules/ui/", full.names = T)

for(item in module_uis){
    source(item, local = T)
}

explorepage <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
        sidebarMenu(
            id = "explorertabs",
            menuItem("iAtlas Explorer Home",
                     tabName = "dashboard",
                     icon = icon("dashboard")
            ),
            menuItem("Analysis Modules",
                     icon = icon("bar-chart"), startExpanded = TRUE,
                     menuSubItem(
                         "Cohort Selection",
                         tabName = "cohort_selection",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Tumor Microenvironment",
                         tabName = "tumor_microenvironment",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Immune Feature Trends",
                         tabName = "immune_features",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Clinical Outcomes",
                         tabName = "clinical_outcomes",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Immunomodulators",
                         tabName = "immunomodulators",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "IO Targets",
                         tabName = "io_targets",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "TIL Maps",
                         tabName = "til_maps",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Driver Associations",
                         tabName = "driver_associations",
                         icon = icon("cog")
                     )
            ),
            menuItem("Data Description",
                     icon = icon("th-list"),
                     tabName = "data_info"
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dashboard",
                titleBox("iAtlas Explorer — Home"),
                textBox(
                    width = 12,
                    p("Select a module to explore CRI iAtlas data through interactive visualizations. You can organize the data by choosing how to divide tumor samples using ",
                      strong("“Select Sample Groups”"), " on the left navigation bar.",  
                      "You can also select ",
                      strong("“Data Description”"), 
                      "on the left navigation bar to learn which immune readouts are available.")
                ),
                sectionBox(
                    title = "What's Inside",
                    fluidRow(
                        infoBox("Immune Readouts:", 86, width = 3, color = "black", 
                                fill = FALSE, icon = icon("search")),
                        infoBox("Classes of Readouts:", 12, width = 3, color = "black", 
                                fill = FALSE, icon = icon("filter")),
                        infoBox("TCGA Cancers:", 33, width = 3, color = "black", 
                                fill = FALSE, icon = icon("flask")),
                        infoBox("TCGA Samples:", "11,080", width = 3, color = "black", 
                                fill = FALSE, icon = icon("users"))
                    )
                ),
                sectionBox(
                    title = "Analysis Modules",
                    messageBox(
                        width = 12,
                        p("Each module presents information organized by theme, with multiple views and interactive controls.",
                          "Within each module, you can find ",
                          strong("“Manuscript Context”"), " describing how that module can generate figures analogous to those in the manuscript ", 
                          em("Thorsson et al., The Immune Landscape of Cancer, Immunity (2018).")
                        )
                    ),
                    fluidRow(
                        imgLinkBox(
                            width = 6,
                            linkId = "link_to_cohort_selection",
                            title = "Cohort Selection",
                            imgSrc = "images/groupsoverview.png",
                            boxText = "Use this module to create a cohort of interest.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
                            width = 6,
                            linkId = "link_to_tumor_microenvironment",
                            title = "Tumor Microenvironment",
                            imgSrc = "images/cellcontent.png",
                            boxText = "Explore the immune cell proportions in your sample groups.",
                            linkText = "Open Module"
                        )
                    ),
                    fluidRow(
                        imgLinkBox(
                            width = 6,
                            title = "Immune Feature Trends",
                            linkId = "link_to_immune_features",
                            imgSrc = "images/immunefeatures.png",
                            boxText = "This module allows you to see how immune readouts vary across your groups, and how they relate to one another.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
                            width = 6,
                            linkId = "link_to_clinical_outcomes",
                            title = "Clinical Outcomes",
                            imgSrc = "images/survival.png",
                            boxText = "Plot survival curves based on immune characteristics and identify variables associated with outcome.",
                            linkText = "Open Module"
                        )
                        
                    ),
                    fluidRow(
                        imgLinkBox(
                            width = 6,
                            title = "Immunomodulators",
                            linkId = "link_to_immunomodulators",
                            imgSrc = "images/immunomodulators.png",
                            boxText = "Explore the expression of genes that code for immunomodulating proteins, including checkpoint proteins.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
                            width = 6,
                            title = "TIL Maps",
                            linkId = "link_to_til_maps",
                            imgSrc = "images/TILmap.png",
                            boxText = "Explore the characteristics of maps of tumor infiltrating lymphocytes obtained from analysis of H&E images.",
                            linkText = "Open Module"
                        )
                    ),
                    fluidRow(
                        imgLinkBox(
                            width = 6,
                            title = "Driver Associations",
                            linkId = "link_to_driver_associations",
                            imgSrc = "images/drivers.png",
                            boxText = "Explore Associations of Microenvironment with Driver Mutations.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
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
            tabItem(
                tabName = "tumor_microenvironment",
                tumor_microenvironment_ui("tumor_microenvironment")
            ),
            tabItem(
                tabName = "cohort_selection",
                cohort_selection_ui("cohort_selection")
            ),
            tabItem(
                tabName = "clinical_outcomes",
                clinical_outcomes_ui("clinical_outcomes")
            ),
            tabItem(
                tabName = "immunomodulators",
                immunomodulators_ui("immunomodulators")
            ),
            tabItem(
                tabName = "immune_features",
                immune_features_ui("immune_features")
            ),
            tabItem(
                tabName = "driver_associations",
                driver_associations_ui("driver_associations")
            ),
            tabItem(
                tabName = "til_maps",
                til_maps_ui("til_maps")
            ),
            tabItem(
                tabName = "io_targets",
                io_targets_ui("io_targets")
            ),
            tabItem(
                tabName = "data_info",
                data_info_ui("data_info")
            )
        )
    )
)