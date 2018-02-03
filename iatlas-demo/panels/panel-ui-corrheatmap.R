corrheatmappage <- fluidPage(
    titlePanel("Immune Feature Correlation Heatmap"),
    sidebarLayout(
        sidebarPanel(
            selectInput("var1", "Variable 1", c("Leukocytes", 
                                                "Gene Sets",
                                                "Genes RNA-seq & RPPA"), selected = "Leukocytes"),
            selectInput("var2", "Variable 2", c("Leukocyte Fraction"="leukocyte_fraction",
                                                "OS Time"="OS_time",
                                                "Mutation Rate, Non-Silent"="mutationrate_nonsilent_per_Mb",
                                                "Indel Neoantigens"="indel_neoantigen_num",
                                                "SNV Neoantigens"="numberOfImmunogenicMutation",
                                                "Stemness Score RNA"="StemnessScoreRNA"
            ), selected = "Leukocyte Fraction"),
            selectInput("catx", "Category",   c("Immune Subtypes"="Subtype_Immune_Model_Based",
                                                "TCGA Tissues"="Study",
                                                "TCGA Subtypes"="Subtype_Curated_Malta_Noushmehr_et_al"), selected = "Immune Subtypes"),
            checkboxInput("clustercols", label = "Cluster Columns", value = F),
            checkboxInput("clusterrows", label = "Cluster Rows", value = T)
        ),
        
        mainPanel(
            plotlyOutput("corrPlot", height = "600px"),
            HTML("<br><br><br>")
            #verbatimTextOutput("event")
        )
    )
)