create_kmplot <- function(fit, df, confint, risktable, title, group_colors) {

  print(group_colors)
  print("confint")
  print(confint)
  print("risktable")
  print(risktable)
  
    survminer::ggsurvplot(
        fit,
        data = df,
        conf.int = confint,
        risk.table = risktable,
        title = title,
        palette = group_colors
    )
}
