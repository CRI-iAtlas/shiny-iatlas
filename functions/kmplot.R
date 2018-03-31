create_kmplot <- function(fit, df, confint, risktable, title) {
  survminer::ggsurvplot(
    fit,
    data = df,
    conf.int = confint,
    risk.table = risktable,
    title = title
  )
}
