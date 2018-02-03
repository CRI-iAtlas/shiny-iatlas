clonaldiversitypage <- fluidPage(
    # Application title
    titlePanel("Clonal Diversity by Sample Group"),
    sidebarLayout(
        sidebarPanel(
            # Drop-down selected sample groups
            selectInput(
                inputId = "selection_choice",
                label = "Select Sample Groups",
                choices = as.character(clonaldiversity_data$sample_selection_choices),
                selected = "Immune Subtype"
            ), 
            
            # Drop-down selected diversity metrics
            selectInput(
                inputId = "diversity_metric_choice",
                label = "Select Receptor Type(s)",
                choices = as.character(clonaldiversity_data$diversity_metric_choices),
                selected = "Shannon"
            ),
            
            # Checkbox selected receptor type(s)
            checkboxGroupInput(
                inputId = "receptor_type_choices",
                label = "Select Receptor Type(s)",
                choices = as.character(clonaldiversity_data$receptor_type_choices),
                selected = "TCR"
            ),
            
            # Checkbox z-score option
            checkboxInput(
                inputId = "ztransform",
                label = "Plot Z-scores",
                value = FALSE
            )
        ),
        
        mainPanel(
            # Show a plot of the generated distribution
            plotOutput(outputId = "diversityPlot")        
        )
    )
)