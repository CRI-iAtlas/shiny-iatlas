io_targets_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — IO Targets"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/io_target.markdown")
        ),
        
        distributions_plot_ui(
            ns("dist"),
            message_html = includeMarkdown("data/markdown/io_target_dist.markdown"),
            title_text = "IO Target Gene Expression Distributions",
            scale_default = "Log10",
            plot_clicked_group_default = T,
        ),
        
        data_table_ui(
            ns("io_table"), 
            title = "IO Target Annotations",
            message_html = p(stringr::str_c(
                "The table shows annotations of the IO Targets, with columns as",
                "described above and description based on public resources such as",
                "NCBI. Use the Search box in the upper right to find an IO target of",
                "interest.",
                "\n",
                "The last column provides a direct link to target information on the",
                "IO Landscape resource such as number of target agents under active",
                "development, and development stage.",
                sep = " "
            ))
        )
        
        
    )
}