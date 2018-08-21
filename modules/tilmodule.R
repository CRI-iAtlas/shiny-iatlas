tilmap_UI <- function(id) {
  
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
            shinycssloaders::withSpinner()
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

tilmap <- function(input, output, session, ss_choice, subset_df){
  
  ns <- session$ns
  
  ss_internal <- reactive(get_variable_internal_name(ss_choice()))
  sample_groups <- reactive(get_category_group(ss_internal()))
  
  output$violinPlot <- renderPlotly({
    
    display_x  <- ss_choice()
    internal_x <- get_variable_internal_name(display_x)
    internal_y <- input$violin_y
    display_y  <- get_variable_display_name(internal_y)
    
    plot_df <- subset_df() %>%
      select_(.dots = c(internal_x, internal_y)) %>%
      .[complete.cases(.),]
    
    plot_df %>% 
      create_violinplot(
        internal_x,
        internal_y,
        internal_x,
        xlab = display_x,
        ylab = display_y,
        fill_colors = decide_plot_colors(panimmune_data, internal_x)
      )
  })
  
  output$til_table <- DT::renderDT({

      panimmune_data$fmx_df %>% 
      select("ParticipantBarcode","Study", "Slide", panimmune_data$feature_df %>% 
               filter(`Variable Class`=="TIL Map Characteristic" & VariableType=="Numeric") %>% .$FeatureMatrixLabelTSV) %>% 
      .[complete.cases(.), ] %>%
      mutate(Image=paste("<a href=\"","http://quip1.bmi.stonybrook.edu:443/camicroscope/osdCamicroscope.php?tissueId=",
                            .$Slide,"\">",.$Slide,"</a>",sep="") ) %>% 
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