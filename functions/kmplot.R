create_kmplot <- function(fit, df, confint, risktable, title, group_colors) {
    survminer::ggsurvplot(
        fit,
        data = df,
        conf.int = confint,
        risk.table = risktable,
        title = title,
        palette = group_colors
    )
}
