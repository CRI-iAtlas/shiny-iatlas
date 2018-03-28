# create_violinplot <- function(
#     df, x, y, fill_factor, 
#     xlab = NULL, 
#     ylab = NULL, 
#     title = NULL, 
#     fill_colors = NA, 
#     facet = NA){
#     
#     plot <- df %>%
#         ggplot(aes_string(x, y)) +
#         geom_violin(aes_string(fill = fill_factor)) +
#         geom_boxplot(alpha = 0.4, outlier.shape = NA) +
#         guides(colour = FALSE, fill = FALSE) +
#         theme_bw() +
#         theme_1012 +
#         xlab(xlab) +
#         ylab(ylab) +
#         labs(title) +
#         theme(axis.text.x = element_text(angle = 90, hjust = 1))
#     if (!is.na(fill_colors)) {
#         plot <- plot + scale_fill_manual(values = fill_colors)
#     }
#     if (!is.na(facet)) {
#         plot <- plot + facet_grid(facet)
#     }
#     return(plot)
# }

create_violinplot <- function(
        df, x, y, fill_factor, 
        xlab = NULL, 
        ylab = NULL, 
        title = NULL, 
        fill_colors = NA, 
        facet = NA
) {
    let(
        alias = c(xvar = x, yvar = y, splitvar = fill_factor),
        df %>% 
            plot_ly(
                x = ~xvar,
                y = ~yvar,
                split = ~splitvar,
                color = ~splitvar,
                colors = fill_colors,
                type = 'violin',
                box = list(
                    visible = TRUE
                ),
                meanline = list(
                    visible = TRUE
                )
            ) %>% 
            layout(
                xaxis = list(title = xlab),
                yaxis = list(title = ylab)
            )
    )
}