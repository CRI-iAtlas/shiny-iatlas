create_forestplot_plotly <- function(
                                     x,
                                     y,
                                     error,
                                     plot_title = "",
                                     xlab = NULL
                                     ){
  
  plotly::plot_ly(
                  type='scatter',
                  mode="markers",
                  y= y, 
                  x= x,
                  marker = list(color='#000000'),
                  showlegend = F,
          error_x=list(visible = T,
               type = "data",
               symmetric = T,
               array=error,
               color = '#000000'))  %>%
    plotly::layout(
      autosize = TRUE,
      font = list(
        family = "Roboto, Open Sans, sans-serif"),
      xaxis=list(title=xlab,
                zeroline=T,
                showticklabels=T))%>% 
    add_title_subplot_plotly(plot_title)
}

