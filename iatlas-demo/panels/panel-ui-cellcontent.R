cellcontentpage <- fluidPage(
    
    # Application title
    titlePanel("Immune Cell Content by Sample Group"),
    sidebarLayout(
        sidebarPanel(
            # Drop-down selected sample groups
            selectInput(inputId="selectionchoice",
                        label="Select Sample Groups",
                        choices=as.character(cellcontent_data$sampleselectionchoices),
                        selected="Immune Subtype"),
            
            # Drop-down selected cell content
            selectInput(inputId="cellcontentchoice",
                        label="Select Cellular Content",
                        choices=as.character(cellcontent_data$cellcontentchoices),
                        selected="Leukocyte Fraction")
        ),
        
        mainPanel(
            # Show a plot of the generated distribution
            plotOutput(outputId="distPlot")
        )
    )    
)