create_kmplot <- function(fit, df, confint, risktable, title, subtitle = NULL, group_colors, facet = FALSE) {
  
  if(!is.null(subtitle)){
    long_title <- paste0(title, '\n', subtitle)
  }else{
    long_title <- title
  }
  
  if(facet == FALSE){
    survminer::ggsurvplot(
          fit,
          data = df,
          conf.int = confint,
          risk.table = risktable,
          title = long_title,
          palette = group_colors
    )
  }else{
    survminer::ggsurvplot_list(
      fit,
      data = df,
      pval = TRUE,
      pval.method = TRUE,
      conf.int = confint,
      risk.table = risktable,
      title = long_title,
      palette = group_colors
    )
      
  }

}
