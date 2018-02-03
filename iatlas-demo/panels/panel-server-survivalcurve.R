
output$survPlot <- renderPlot({
    df <- buildDataFrame_surv(survivalcurve_data$dat, input$var1_surv, input$timevar, input$divk)
    fit <- survfit(Surv(Time, Status) ~ Variable, data=df) 
    #ggsurv(fit) + 
    #  ggplot2::guides(linetype = FALSE) +
    #  ggplot2::scale_colour_discrete(
    #    name   = getNiceName(input$var1)
    #  )
    print(fit)
    ggsurvplot(fit, data=df, conf.int = input$confint, 
               risk.table = input$risktable, title=getNiceName(input$var1_surv))
})