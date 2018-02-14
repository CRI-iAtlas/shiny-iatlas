survival_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            titlePanel("Immune Feature Kaplan-Meier Plot"),
            sidebarLayout(
                sidebarPanel(
                    selectInput(ns("var1_surv"), "Variable", 
                                c("Immune Subtypes"="Subtype_Immune_Model_Based",
                                  "Leukocyte Fraction"="leukocyte_fraction",
                                  "Mutation Rate, Non-Silent"="mutationrate_nonsilent_per_Mb",
                                  "Indel Neoantigens"="indel_neoantigen_num",
                                  "SNV Neoantigens"="numberOfImmunogenicMutation",
                                  "Stemness Score RNA"="StemnessScoreRNA"), selected = "Subtype_Immune_Model_Based"),
                    selectInput(ns("timevar"), "Time Varible", c("OS Time"="OS_time", "PFI Time"="PFI_time_1"), selected = "OS_time"),
                    sliderInput(ns("divk"), "Divider", min = 0, max = 10, value = 2),
                    checkboxInput(ns("confint"), "Confidence Intervals", value = F),
                    checkboxInput(ns("risktable"), "Risk Table", value = T)
                ),
                mainPanel(
                    plotOutput(ns("survPlot"), height = 600)
                )
            )
        )
    )
}

survival <- function(input, output, session){
    
    output$survPlot <- renderPlot({
        df <- buildDataFrame_surv(survivalcurve_data$dat, input$var1_surv, input$timevar, input$divk)
        fit <- survfit(Surv(Time, Status) ~ Variable, data=df) 
        #ggsurv(fit) + 
        #  ggplot2::guides(linetype = FALSE) +
        #  ggplot2::scale_colour_discrete(
        #    name   = getNiceName(input$var1)
        #  )
        ggsurvplot(fit, data=df, conf.int = input$confint, 
                   risk.table = input$risktable, title=getNiceName(input$var1_surv))
    })
}