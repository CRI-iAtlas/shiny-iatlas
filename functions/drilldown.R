
# immunomodulator helpers -----------------------------------------------------

get_selected_group_from_violinplot <- function(
    plot_df, plot_column, eventdata
) {
    selected_violin_group <- eventdata$x[[1]]
    plot_df %>%
      filter_at(.vars = plot_column, 
                .vars_predicate = all_vars(. == selected_violin_group)) %>% 
        extract2(plot_column)
}