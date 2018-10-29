create_kmplot <- function(fit, df, confint, risktable, title, group_colors) {
    group_colors <- set_names(group_colors, NULL)
    survminer::ggsurvplot(
        fit,
        data = df,
        conf.int = confint,
        risk.table = risktable,
        title = title,
        palette = group_colors
    )
}
