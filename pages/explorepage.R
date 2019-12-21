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
                         tabName = "cell_content",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Immune Feature Trends",
                         tabName = "immune_features",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Clinical Outcomes",
                         tabName = "survival_curves",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Immunomodulators",
                         tabName = "immunomodulators",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "IO Targets",
                         tabName = "iotargets",
                         icon = icon("cog")
                     ),
                     # menuSubItem(
                     #   "Genomic State",
                     #   icon = icon("chevron-circle-right")),
                     # menuSubItem(
                     #   "Immune Interface",
                     #   tabName = "clonal_diversity",
                     #   icon = icon("chevron-circle-right")),
                     menuSubItem(
                         "TIL Maps",
                         tabName = "tilmap_features",
                         icon = icon("cog")
                     ),
                     menuSubItem(
                         "Driver Associations",
                         tabName = "drivers",
                         icon = icon("cog")
                     )
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
                            linkId = "link_to_cohort_selection_module",
                            title = "Cohort Selection",
                            imgSrc = "images/groupsoverview.png",
                            boxText = "Use this module to create a cohort of interest.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
                            width = 6,
                            linkId = "link_to_module1",
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
                            linkId = "link_to_module6",
                            imgSrc = "images/immunefeatures.png",
                            boxText = "This module allows you to see how immune readouts vary across your groups, and how they relate to one another.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
                            width = 6,
                            linkId = "link_to_module4",
                            title = "Clinical Outcomes",
                            imgSrc = "images/survival.png",
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
                            imgSrc = "images/immunomodulators.png",
                            boxText = "Explore the expression of genes that code for immunomodulating proteins, including checkpoint proteins.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
                            width = 6,
                            title = "TIL Maps",
                            linkId = "link_to_module7",
                            imgSrc = "images/TILmap.png",
                            boxText = "Explore the characteristics of maps of tumor infiltrating lymphocytes obtained from analysis of H&E images.",
                            linkText = "Open Module"
                        )
                    ),
                    fluidRow(
                        imgLinkBox(
                            width = 6,
                            title = "Driver Associations",
                            linkId = "link_to_module8",
                            imgSrc = "images/drivers.png",
                            boxText = "Explore Associations of Microenvironment with Driver Mutations.",
                            linkText = "Open Module"
                        ),
                        imgLinkBox(
                            width = 6,
                            title = "IO Targets",
                            linkId = "link_to_module9",
                            imgSrc = "images/iotargets.png",
                            boxText = "Explore the expression of genes that code for immuno-oncological (IO) targets .",
                            linkText = "Open Module"
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
                tabName = "cohort_selection",
                cohort_selection_UI("cohort_selection")
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
                tabName = "drivers",
                drivers_UI("module8")
            ),
            tabItem(
                tabName = "tilmap_features",
                tilmap_UI("module7")
            ),
            tabItem(
                tabName = "iotargets",
                iotarget_UI("module9")
            ),
            tabItem(
                tabName = "datainfo",
                datainfo_UI("moduleX")
            )
        )
    )
)