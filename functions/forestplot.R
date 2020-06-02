create_forestplot <- function(
    df,
    x,
    y,
    xmin,
    xmax,
    xintercept,
    title = NULL, 
    fill_colors = NA,
    xlab = NULL, 
    ylab = NULL,
    facet = NULL) {

    plot <- df %>%
        ggplot(aes(y=y, x=x, xmin=xmin, xmax=xmax))+
        geom_point(color = 'black')+
        geom_errorbarh(height=.1)+
        scale_x_continuous(name= xlab)+
        ylab(ylab)+
        geom_vline(xintercept=xintercept, color='black', linetype='dashed')+
        ggtitle(title)+
        theme_bw() 
    
    if (!is.null(facet)) {
        plot <- plot + facet_wrap(facet)
    }
    p <- plotly_build(plot) %>%
        layout(title = title) #%>% 
        # format_plotly()
    
    p
}


create_forestplot_plotly <- function(
                                     x,
                                     y,
                                     error
                                     ){
  
  plotly::plot_ly(
                  type='scatter',
                  mode="markers",
                  y= y, 
                  x= x,
          error_x=list(visible = T,
               type = "data",
               symmetric = T,
               array=error)) %>%  
    plotly::layout(xaxis=list(title="Hazard Ratio (log10)",
                              zeroline=T,
                              showticklabels=T),
                   yaxis=list(zeroline=F,
                              showticklabels=T))
}

