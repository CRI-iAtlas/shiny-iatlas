create_mosaicplot <- function(
    df, x, y, fill_factor, 
    xlab = NULL, 
    ylab = NULL, 
    title = NULL, 
    fill_colors = NA, 
    facet = NA){
    
    plot <- df %>%
        ggplot(aes(x = product(Subtype_Immune_Model_Based, Study))) +
        geom_mosaic(aes_string(fill = fill_factor)) +
        theme_bw() +
        theme_1012 +
        xlab(xlab) +
        ylab(ylab) +
        labs(title) +
        theme(legend.title = element_blank()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    if (!is.na(fill_colors)) {
        plot <- plot + scale_fill_manual(values = fill_colors)
    }
    if (!is.na(facet)) {
        plot <- plot + facet_grid(facet)
    }
    return(plot)
}