#cellcontentmodule
create_boxplot <- function(df, sample_group, cellcontent, x_label, y_label ){
    df %>% 
        ggplot(aes_string(x = sample_group, y = cellcontent, fill = sampgroup)) + 
        geom_boxplot() +
        guides(colour = FALSE, fill = FALSE) +
        ylab(y_label) + 
        xlab(x_label) +
        theme_bw() +
        theme_1012 +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

#immuneinterfacemodule
create_boxplot2 <- function(df, sample_group, scale_label, x_label){
    df %>% 
        ggplot(aes_string(x = sample_group, y = "diversity")) + 
        geom_boxplot(aes_string(fill = sample_group)) +
        guides(colour = FALSE, fill = FALSE) +
        ylab(glue::glue("Diversity [{label}]", label = scale_label)) + 
        xlab(x_label) +
        theme_bw() +
        theme_1012  + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
        facet_grid(receptor ~ .)
}