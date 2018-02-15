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
        sampgroup   <- get_cellcontent_label("samplegroups", input$selectionchoice)
        cellcontent <- get_cellcontent_label("cellcontent", input$cellcontentchoice)
        ## create dfp, the data frame for plotting, based on choices
        dfp <- create_cellcontent_df(sampgroup, cellcontent)
        ## custom colors if available 
        plotcolors <- decide_plotcolors(sampgroup)
        plot <- create_boxplot(
            dfp, 
            x = sampgroup, 
            y = cellcontent, 
            fill = sampgroup, 
            input$selectionchoice, 
            input$cellcontentchoice)
        if(!is.null(plotcolors)){
            plot <- plot + scale_fill_manual(values = plotcolors)}
        print(p)
    })
}

# helper functions ------------------------------------------------------------

get_cellcontent_label <- function(lst, selection){
    cellcontent_data %>% 
        magrittr::extract2(lst) %>% 
        magrittr::extract2(selection) %>% 
        as.character
}

decide_plotcolors <- function(sampgroup){
    if (sampgroup == 'Study'){
        plotcolors <- cellcontent_data$tcga_colors
    }
    else if (sampgroup == 'Subtype_Immune_Model_Based'){
        plotcolors <- cellcontent_data$subtype_colors
    }
    else {
        plotcolors <- NULL
    }
    return(plotcolors)
}