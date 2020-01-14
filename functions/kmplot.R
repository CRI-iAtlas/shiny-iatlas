create_kmplot <- function(fit, df, confint, risktable, title, group_colors, facet = FALSE) {

  print(group_colors)
  print("confint")
  print(confint)
  print("risktable")
  print(risktable)
  
  if(facet == FALSE){
    survminer::ggsurvplot(
      fit,
      data = df,
      pval=TRUE,
      conf.int = confint,
      risk.table = risktable,
      title = title,
      palette = group_colors
    )
  }else{
    survminer::ggsurvplot_list(
      fit,
      data = df,
      pval = TRUE,
      conf.int = confint,
      risk.table = risktable,
      title = title,
      palette = group_colors
    )
      
  }

}
