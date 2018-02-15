cellcontent_UI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidPage(
            # Application title
            titlePanel("Immune Cell Content by Sample Group"),
            sidebarLayout(
                sidebarPanel(
                    # Drop-down selected sample groups
                    selectInput(inputId=ns("selectionchoice"),
                                label="Select Sample Groups",
                                choices=as.character(cellcontent_data$sampleselectionchoices),
                                selected="Immune Subtype"),
                    
                    # Drop-down selected cell content
                    selectInput(inputId=ns("cellcontentchoice"),
                                label="Select Cellular Content",
                                choices=as.character(cellcontent_data$cellcontentchoices),
                                selected="Leukocyte Fraction")
                ),
                
                mainPanel(
                    # Show a plot of the generated distribution
                    plotOutput(ns("distPlot"))
                )
            )    
        )
    )
}

cellcontent <- function(input, output, session){
    
    output$distPlot <- renderPlot({
        sample_group_label <- get_cellcontent_label("samplegroups", input$selectionchoice)
        cellcontent_label <- get_cellcontent_label("cellcontent", input$cellcontentchoice)
        ## create dfp, the data frame for plotting, based on choices
        plot_df <- create_cellcontent_df(sample_group_label, cellcontent_label)
        plot <- create_boxplot(
            plot_df, 
            x = sample_group_label, 
            y = cellcontent_label, 
            fill = sample_group_label, 
            input$selectionchoice, 
            input$cellcontentchoice)
        ## custom colors if available 
        plot_colors <- decide_plot_colors(sample_group_label)
        if(!is.null(plot_colors)){
            plot <- plot + scale_fill_manual(values = plot_colors)}
        print(plot)
    })
}

# helper functions ------------------------------------------------------------

get_cellcontent_label <- function(lst, selection){
    cellcontent_data %>% 
        magrittr::extract2(lst) %>% 
        magrittr::extract2(selection) %>% 
        as.character
}

decide_plot_colors <- function(sample_group_label){
    if (sample_group_label == 'Study'){
        plot_colors <- cellcontent_data$tcga_colors
    }
    else if (sample_group_label == 'Subtype_Immune_Model_Based'){
        plot_colors <- cellcontent_data$subtype_colors
    }
    else {
        plot_colors <- NULL
    }
    return(plot_colors)
}