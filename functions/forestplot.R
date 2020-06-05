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
                                     error,
                                     p.values,
                                     #dataset,
                                     xlab = NULL
                                     ){
  
  # library(grDevices)
  # mycramp<-colorRamp(c("darkred","gray"))
  # mycolors<-rgb(mycramp(p.values),maxColorValue = 255)
  # print(mycolors)
  
  ### The colorbar-adjustment kicks out the original colors of the scatter points. Either you plot them over
  # plot_ly(type='scatter',mode="markers",y=~factors,x=~effect.sizes,
  #         color=~p.values,colors=grDevices::colorRamp(c("darkred","gray")),
  #         error_x=list(array=effect.errors,color=mycolors),split=factors,showlegend=FALSE,marker=list(color=mycolors)) %>%
  #   layout(xaxis=list(title="Effect Size",zeroline=T,showticklabels=T),yaxis=yform) %>%
  #   colorbar(limits=c(0,1),len=0.4,title="P-Value",inherit=FALSE) %>%
  #   add_trace(type='scatter',mode="markers",y=~factors,x=~effect.sizes,
  #             showlegend=FALSE,marker=list(color=mycolors),inherit=FALSE) %>%
  #   layout(xaxis=list(title="Effect Size",zeroline=T,showticklabels=T),yaxis=yform)
  
  
  plotly::plot_ly(
                  type='scatter',
                  mode="markers",
                  y= y, 
                  x= x,
                  marker = list(color='#000000'),
                  # ~(as.character(p.values)),
                  # colors=grDevices::colorRamp(c("darkred","gray")),
                  showlegend = F,
          error_x=list(visible = T,
               type = "data",
               symmetric = T,
               array=error,
               color = '#000000'))  %>%
    plotly::layout(
      autosize = TRUE,
      xaxis=list(title=xlab,
                zeroline=T,
                showticklabels=T)
     )#%>% 
  # format_plotly()
}

