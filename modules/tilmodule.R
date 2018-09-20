tilmap_UI <- function(id) {
    
    get_friendly_numeric_columns <- function(){
        get_numeric_columns() %>% 
            purrr::map(get_variable_display_name) %>%
            compact() %>% 
            unlist() %>% 
            discard(~is.na(.))
    }
    
    get_friendly_numeric_columns_by_group <- function() {
        panimmune_data$feature_df %>% 
            select(Class = `Variable Class`, FriendlyLabel, FeatureMatrixLabelTSV) %>% 
            filter(FriendlyLabel %in% get_friendly_numeric_columns()) %>% 
            mutate(Class = ifelse(is.na(Class), "Other", Class)) %>% 
            nest(-Class) %>% 
            mutate(data = map(data, deframe)) %>% 
            deframe()
    }
    
    get_numeric_columns <- function(){
        panimmune_data$fmx_df %>% 
            select_if(is.numeric) %>% 
            colnames()
    }
  
  ns <- NS(id)
  
  tagList(
    titleBox("iAtlas Explorer — TIL Maps"),
    textBox(
      width = 12,
      p("Explore TIL distributions.")  
    ),
    
    # TIL distributions section ----
    sectionBox(
      title = "TIL map characteristics",
      messageBox(
        width = 12,
        p("Select a TIL map characteristic to see its distribution over sample groups."),
        p("Manuscript context:  If you are looking at immune subtypes, select TIL Regional Fraction to ge Figure 3B")
      ),
      fluidRow(
        optionsBox(
          width = 4,
          selectInput(
            ns("violin_y"),
            "Select TIL Map characteristic",
            choices = get_friendly_numeric_columns_by_group()["TIL Map Characteristic"]
          )
        )
      ),
      fluidRow(
        plotBox(
          width = 12,
          plotlyOutput(ns("violinPlot")) %>% 
            shinycssloaders::withSpinner(),
          h4("Click point or violin to filter samples in table below"),
          verbatimTextOutput(ns("click"))
        )
      )
    ),
    
    # TIL Map annotations section ----
    sectionBox(
      title = "TIL Map Annotations",
      messageBox(
        width = 12,
        p("The table shows annotations of the TIL Map characteristics")  
      ),
      fluidRow(
        tableBox(
          width = 12,
          div(style = "overflow-x: scroll",
              DT::dataTableOutput(ns("til_table")) %>% 
                shinycssloaders::withSpinner()
          )
        )
      )
    )
  )
}

# tilmap <- function(input, output, session, ss_choice, subset_df){
tilmap <- function(input, output, session, group_display_choice, group_internal_choice, 
                   subset_df, plot_colors, group_options){
  
  ns <- session$ns
  
  
  output$violinPlot <- renderPlotly({
    
    display_y  <- get_variable_display_name(input$violin_y)
    
    plot_df <- subset_df() %>%
      select(x = group_internal_choice(), y = input$violin_y, label = "Slide") %>%
      drop_na()
    
    print(plot_df)
    
    plot_df %>%
      create_violinplot(
        xlab = group_display_choice(),
        ylab = display_y,
        fill_colors = plot_colors(),
        source_name = "violin", 
        points = "all"
      )
    
  })
  
  output$click <- renderPrint({
      df <- event_data("plotly_click", source = "violin")
      if (is.null(df)) {
          "Hover events appear here (unhover to clear)" 
      } else {
          df %>% 
              select(x,y,key)
      } 
  })
  
  output$til_table <- DT::renderDT({
      
      d <- event_data("plotly_click", source = "violin")
      if (!is.null(d)) {
          slide_ids <- d %>% 
              use_series(key)
          data_df <- filter(panimmune_data$fmx_df, Slide %in% slide_ids)
      } else {
          data_df <- panimmune_data$fmx_df
      }
      
      
      TIL_map_columns <- panimmune_data$feature_df %>% 
          filter(`Variable Class` == "TIL Map Characteristic") %>% 
          filter(VariableType == "Numeric") %>% 
          use_series(FeatureMatrixLabelTSV)

      data_df %>% 
          select("ParticipantBarcode", "Study", "Slide", TIL_map_columns) %>% 
          mutate(Image = paste(
              "<a href=\"",
              "http://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
              Slide,
              "\">",
              Slide,
              "</a>",
              sep="")) %>% 
          select(-"Slide") %>% ## column width/wrap problem at the moment for this
          datatable(
              rownames = FALSE,
              escape = setdiff(colnames(.),"Image") ## To get hyperlink displayed
          ) %>% formatRound(c('til_percentage','NP_mean',"NP_sd","WCD_mean","WCD_sd","CE_mean",
                              "CE_sd","Ball_Hall","Banfeld_Raftery","C_index","Det_Ratio","Ball_Hall_Adjusted",
                              "Banfeld_Raftery_Adjusted","C_index_Adjusted","Det_Ratio_Adjusted"), digits = 1) 

    ## Probably want to  get_variable_display_name for the column display
      })
}