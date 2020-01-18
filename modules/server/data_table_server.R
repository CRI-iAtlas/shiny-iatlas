data_table_server <- function(
    input, 
    output, 
    session,
    data_df,
    options = list(pageLength = 10),
    color = F,
    color_column = NULL,
    colors = NULL,
    ...
){
    output$data_table_module <- DT::renderDT({
        dt <- DT::datatable(
            data_df(),
            options = options,
            rownames = FALSE,
            ...
        )
        if(color){
            dt <-  DT::formatStyle(
                dt,
                color_column,
                backgroundColor = DT::styleEqual(colors, colors))
        }
        return(dt)
    })
}
