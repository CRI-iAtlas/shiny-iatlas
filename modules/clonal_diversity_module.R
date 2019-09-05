clonal_diversity_module_UI <- function(id) {
    ns <- NS(id)

    tagList(
        titleBox("iAtlas Explorer — Clonal Diversity"),
        textBox(
            width = 12,
            p("Placeholder text")
        ),
        fluidRow(
            sectionBox(
                title = "General Statistics",
                messageBox(
                    width = 12,
                    p("Placeholder text")
                )
            )
        ),
        fluidRow(
            optionsBox(
                column(
                    width = 6,
                    selectInput(
                        ns("gen_stats_plot_method"),
                        "Select Method",
                        choices = c(
                            "volume", 
                            "count", 
                            "len", 
                            "clones"
                        )
                    )
                ),
                column(
                    width = 6,
                    selectInput(
                        ns("gen_stats_sequence_type"),
                        "Select Sequence Type",
                        choices = c("nt", "aa")
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotOutput(ns("gen_stats_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            sectionBox(
                title = "Clonality",
                messageBox(
                    width = 12,
                    p("Placeholder text")
                )
            )
        ),
        fluidRow(
            optionsBox(
                column(
                    width = 6,
                    selectInput(
                        ns("clonality_plot_method"),
                        "Select Method",
                        choices = c(
                            "clonal.prop", 
                            "homeo",
                            "top",
                            "tail"
                        )
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotOutput(ns("clonality_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            sectionBox(
                title = "Public Repertoire",
                messageBox(
                    width = 12,
                    p("Placeholder text")
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotOutput(ns("public_repertoire_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            sectionBox(
                title = "Repertoire Overlap Clustering",
                messageBox(
                    width = 12,
                    p("Placeholder text")
                )
            )
        ),
        fluidRow(
            optionsBox(
                column(
                    width = 6,
                    selectInput(
                        ns("overlap_plot_method"),
                        "Select Method",
                        choices = c(
                            "public", 
                            "overlap",
                            "jaccard",
                            "tversky",
                            "cosine",
                            "morisita",
                            "inc+public",
                            "inc+morisita"
                        )
                    )
                ),
                column(
                    width = 6,
                    selectInput(
                        ns("overlap_cluster_plot_method"),
                        "Select Method",
                        choices = c(
                            "mds",
                            "tsne",
                            "mds+kmeans"
                        )
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotOutput(ns("overlap_cluster_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            sectionBox(
                title = "Gene Usage",
                messageBox(
                    width = 12,
                    p("Placeholder text")
                )
            )
        ),
        fluidRow(
            optionsBox(
                column(
                    width = 6,
                    selectInput(
                        ns("usage_gene"),
                        "Select Gene",
                        choices = c(
                            "hs.trbv",
                            "hs.ighd",
                            "hs.ighj",
                            "hs.ighv",
                            "hs.igij",
                            "hs.igkj",
                            "hs.igkv",
                            "hs.iglj", 
                            "hs.iglv", 
                            "hs.traj",
                            "hs.trav",
                            "hs.trbd",
                            "hs.trbj",
                            "hs.trdd",
                            "hs.trdj",
                            "hs.trdv",
                            "hs.trgj",
                            "hs.trgv",
                            "hs.vprv"
                        )
                    )
                ),
                column(
                    width = 6,
                    selectInput(
                        ns("gene_usage_plot_type"),
                        "Select Plot",
                        choices = c(
                            "hist", 
                            "box"
                        )
                    )
                ),
                column(
                    width = 6,
                    selectInput(
                        ns("gene_analysis_method"),
                        "Select Method",
                        choices = c(
                            "js+pca+kmeans", 
                            "js+mds+kmeans", 
                            "js+tsne+kmeans"
                        )
                    )
                ),
                column(
                    width = 6,
                    numericInput(
                        ns("gene_analysis_clusters"),
                        "Select Amount of Clusters",
                        value = 2,
                        min = 2
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotOutput(ns("gene_usage_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotOutput(ns("gene_analysis_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        ),
        fluidRow(
            sectionBox(
                title = "Diversity",
                messageBox(
                    width = 12,
                    p("Placeholder text")
                )
            )
        ),
        fluidRow(
            optionsBox(
                column(
                    width = 6,
                    selectInput(
                        ns("diversity_plot_method"),
                        "Select Method",
                        choices = c(
                            "chao1",
                            "hill",
                            "div",
                            "gini.simp",
                            "inv.simp",
                            "raref",
                            "d50",
                            "dxx"
                        )
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotOutput(ns("diversity_plot")) %>%
                    shinycssloaders::withSpinner()
            )
        )
    )
}

clonal_diversity_module <- function(input, output, session) {
    
    data <- reactive({
        library(immunarch)
        utils::data(immdata)
        x <- immdata
        return(x)
    })
    
    output$gen_stats_plot <- renderPlot({
        data()$data %>% 
            immunarch::repExplore(
                .method = input$gen_stats_plot_method,
                .col = input$gen_stats_sequence_type
            ) %>% 
            immunarch::vis(
                .by = c("Status"), 
                .meta = data()$meta
            ) %>% 
            print()
    })
    
    output$clonality_plot <- renderPlot({
        data()$data %>% 
            immunarch::repClonality(
                .method = input$clonality_plot_method
            ) %>% 
            immunarch::vis(
                .by = c("Status"), 
                .meta = data()$meta
            ) %>% 
            print()
    })
    
    output$public_repertoire_plot <- renderPlot({
        data()$data %>% 
            immunarch::pubRep() %>% 
            immunarch::vis(.by = c("Status"), .meta = data()$meta) %>% 
            print()
    })
    
    output$overlap_cluster_plot <- renderPlot({
        data()$data %>% 
            immunarch::repOverlap(
                .method = input$overlap_plot_method, 
                .verbose = F
            ) %>% 
            immunarch::repOverlapAnalysis(input$overlap_cluster_plot_method) %>% 
            immunarch::vis(.by = c("Status"), .meta = data()$meta) %>% 
            print()
    })
    
    gene_usage <- reactive({
        data()$data %>% 
            immunarch::geneUsage(input$usage_gene, .norm = T, .ambig = "exc")
    })
    
    output$gene_usage_plot <- renderPlot({
        gene_usage() %>% 
            immunarch::vis(
                .by = c("Status"), 
                .meta = data()$meta, 
                .plot = input$gene_usage_plot_type
            ) %>% 
            print
    })
    
    output$gene_analysis_plot <- renderPlot({
        gene_usage() %>% 
            immunarch::geneUsageAnalysis(
                input$gene_analysis_method, 
                .k = input$gene_analysis_clusters,
                .verbose = F
            ) %>% 
            immunarch::vis(.plot = "clust") %>% 
            print
    })
    
    output$diversity_plot <- renderPlot({
        data()$data %>% 
            immunarch::repDiversity(
               input$diversity_plot_method
            ) %>% 
            immunarch::vis(
                .by = c("Status"), 
                .meta = data()$meta, 
            ) %>% 
            print
    })
}

