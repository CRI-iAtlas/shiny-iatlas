create_boxplot <- function(
    df, 
    x_col = "x",
    y_col = "y",
    key_col = NA,
    color_col = NA, 
    label_col = NA,
    split_col = NA,
    xlab = "",
    ylab = "", 
    title = "", 
    source_name = NULL, 
    custom_data = "",
    fill_colors = NA,
    showlegend = T){
    
    if(is.na(key_col)) key_col <- x_col
    if(is.na(color_col)) color_col <- x_col
    if(is.na(label_col)) label_col <- x_col
    if(is.na(split_col)) split_col <- x_col
    
    wrapr::let(
        alias = c(
            X = x_col, 
            Y = y_col, 
            KEY = key_col,
            COLOR = color_col,
            LABEL = label_col,
            SPLIT = split_col),
        plot_ly(
            df,
            x = ~X,
            y = ~Y,
            split = ~SPLIT,
            color = ~COLOR,
            key = ~KEY,
            text = ~LABEL,
            type = "box", 
            boxpoints = "all", 
            jitter = 0.7,
            pointpos = 0, 
            colors = fill_colors,
            source = source_name,
            customdata = custom_data,
            showlegend = showlegend
        )) %>% 
        layout(
            title = title,
            xaxis = list(title = xlab),
            yaxis = list(title = ylab)
        ) %>% 
        format_plotly() %>%
        I
}


create_boxplot_from_summary_stats <- function(
  df,
  y_col = "y",
  q1_col,
  median_col,
  q3_col,
  min_col,
  max_col,
  mean_col = NA,
  key_col = NA,
  color_col = NA,
  label_col = NA,
  order_by = NULL,
  xlab = "",
  ylab = "",
  source_name = NULL,
  fill_colors = NA){
  
  if(is.na(key_col)) key_col <- y_col
  if(is.na(color_col)) color_col <- y_col
  
  wrapr::let(
    alias = c(
      Y = y_col,
      Q1 = q1_col,
      Q2 = median_col,
      Q3 = q3_col,
      MIN = min_col,
      MAX = max_col,
      MEAN = mean_col,
      KEY = key_col,
      COLOR = color_col),
    plotly::plot_ly(
      df,
      y = ~Y,
      #color = ~COLOR,
      type = "box",
      q1= ~Q1,
      median= ~Q2,
      q3= ~Q3,
      lowerfence= ~MIN,
      upperfence= ~MAX,
      mean= ~MEAN,
      #fillcolor = fill_colors,
      #colors = fill_colors,
      source = source_name
    )) %>%
    plotly::layout(
      xaxis = list(title = xlab),
      yaxis = list(title = ylab,
                   ategoryorder = "array",
                   categoryarray = ~order_by)
    ) %>%
    format_plotly() %>%
    I
}

