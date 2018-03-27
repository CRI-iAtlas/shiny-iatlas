
# immunomodulator helpers -----------------------------------------------------

get_selected_group_from_plotly_boxplot <- function(
    plot_df, plot_column, eventdata) {
    selected_box_index <- eventdata$x[[1]]
    plot_df %>%
        extract2(plot_column) %>%
        as.character() %>% 
        as.factor() %>%
        levels() %>%
        extract2(selected_box_index)
}