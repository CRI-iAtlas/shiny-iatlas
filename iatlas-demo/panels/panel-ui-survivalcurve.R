
survivalcurvepage <- fluidPage(
    titlePanel("Immune Feature Kaplan-Meier Plot"),
    sidebarLayout(
        sidebarPanel(
            selectInput("var1_surv", "Variable", 
                        c("Immune Subtypes"="Subtype_Immune_Model_Based",
                          "Leukocyte Fraction"="leukocyte_fraction",
                          "Mutation Rate, Non-Silent"="mutationrate_nonsilent_per_Mb",
                          "Indel Neoantigens"="indel_neoantigen_num",
                          "SNV Neoantigens"="numberOfImmunogenicMutation",
                          "Stemness Score RNA"="StemnessScoreRNA"), selected = "Subtype_Immune_Model_Based"),
            selectInput("timevar", "Time Varible", c("OS Time"="OS_time", "PFI Time"="PFI_time_1"), selected = "OS_time"),
            sliderInput("divk", "Divider", min = 0, max = 10, value = 2),
            checkboxInput("confint", "Confidence Intervals", value = F),
            checkboxInput("risktable", "Risk Table", value = T)
        ),
        mainPanel(
            plotOutput("survPlot", height = 600)
        )
    )
)