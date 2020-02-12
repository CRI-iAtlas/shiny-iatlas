create_kmplot <- function(fit, df, confint, risktable, title, subtitle, group_colors) {
  
  long_title <- paste0(title, '\n', subtitle)
  
  survminer::ggsurvplot(
        fit,
        data = df,
        conf.int = confint,
        risk.table = risktable,
        title = long_title,
        palette = group_colors
    )
}
