## cellimage_UI <- function(id){
## ns <- NS(id)
## tagList()
## }

cellimage <- function(
##    input,
##    output,
    ##    session,
    group_display_choice,
    group_internal_choice,
    sample_group_df,
    subset_df
){

    data_df <-
 ##   data_df <- reactive({
    subset_df() %>%
      dplyr::select(
        x = group_internal_choice(),
        "ParticipantBarcode") %>%
      dplyr::inner_join(panimmune_data$im_expr_df, by = "ParticipantBarcode") %>%
      dplyr::rename(label = "ParticipantBarcode")
  })

##}) closing the reactive

