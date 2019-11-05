# -----------------------------------------------------------------------------
cellcontent_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Tumor Microenvironment"),
        textBox(
            width = 12,
            p("Explore the immune cell proportions in your sample groups.")  
        ),
        overall_cell_proportions_module_UI(ns("ocp_module")),
        cell_type_fractions_module_UI(ns("ctf_module"))
    )
}

cellcontent <- function(
    input,
    output, 
    session, 
    group_display_choice, 
    feature_values_long_con,
    feature_con,
    group_con
) {
    
    callModule(
        overall_cell_proportions_module, 
        "ocp_module",
        group_display_choice, 
        feature_values_long_con,
        feature_con,
        group_con
    )
    
    callModule(
        cell_type_fractions_module, 
        "ctf_module",
        group_display_choice, 
        feature_values_long_con,
        feature_con,
        group_con
    )
}

# -----------------------------------------------------------------------------
overall_cell_proportions_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Overall Cell Proportions",
        messageBox(
            width = 12,
            p("The barplots show the mean proportion of the tumor fraction, overall stromal fraction (one minus tumor fractions) and the leukocyte fraction of samples with each group.  Error bars show standard error of the mean.")
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("barplot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("barplot_text"))
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                messageBox(
                    width = 6,
                    p("Click on the bars for a sample group and you will get generate a scatter plot, showing leukocyte fraction on the Y-axis and stromal fraction on the X-axis. Points near the diagonal correspond to tumor samples in which non-tumor stromal cells are nearly all immune cells, and points away from the diagonal correspond to a more mixed or a non-immune stromal tumor microenvironment.  Points in the upper-left triangle of the plot are estimation artifacts."),
                    p("Manuscript context:  Looking at TCGA tumor types, select PRAD and then SKCM and you will get what corresponds to Figure 2C.")
                ),
                column(
                    width = 6,
                    br(),
                    fluidRow(
                        plotlyOutput(ns("scatterplot")) %>%
                            shinycssloaders::withSpinner()
                    )
                )
            )
        )
    )
    
}

overall_cell_proportions_module  <- function(
    input, 
    output, 
    session,
    group_display_choice, 
    feature_values_long_con,
    feature_con,
    group_con
){
    
    cp_feature_con <- reactive({
        req(feature_con())
        feature_con() %>%  
            dplyr::filter(class == "Overall Proportion") %>% 
            dplyr::filter(feature != "til_percentage")
    })
    
    cp_value_con <- reactive({
        req(feature_values_long_con(), cp_feature_con())
        build_cell_proportion_con(cp_feature_con(), feature_values_long_con())
    })
    
    barplot_tbl <- reactive({
        req(cp_value_con())
        build_cell_proportion_barplot_tbl(cp_value_con())
    })
    
    output$barplot <- renderPlotly({
        
        req(barplot_tbl())
        
        validate(need(
            nrow(barplot_tbl()) > 0, 
            "Samples in current selected groups have no fraction data."))
        
        create_barplot(
            barplot_tbl(),
            color_col = "color",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "op_barplot"
        )
    })
    
    output$barplot_text <- renderText({
        req(group_con())
        eventdata <- event_data("plotly_click", source = "op_barplot")
        validate(need(eventdata, "Click above plot"))
        
        group_con() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(group_name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    output$scatterplot <- renderPlotly({
        req(cp_value_con())
        eventdata <- event_data( "plotly_click", source = "op_barplot")
        validate(need(eventdata, "Click above plot"))
        
        
        selected_group <- eventdata$x[[1]]
        groups         <- dplyr::pull(cp_value_con(), group)
        validate(need(selected_group %in% groups, "Click above barchart"))
        
        scatterplot_tbl <-  build_cell_proportion_scatterplot_tbl(
            cp_value_con()
        ) 
        
        create_scatterplot(
            scatterplot_tbl,
            xlab = "Stromal Fraction",
            ylab = "Leukocyte Fraction",
            label_col = "label",
            fill_colors = "blue",
            title = selected_group,
            identity_line = TRUE
        ) %>%
            layout(margin = list(t = 39))
    })
}

# -----------------------------------------------------------------------------
cell_type_fractions_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Cell Type Fractions",
        messageBox(
            width = 12,
            p("This allows you to draw barplots for the estimate proportion of different cell types in the immune compartment.  Cellular proportions are estimated using CIBERSORT. In addition to the original CIBERSORT estimates (22 cell fractions), estimates combining related cell types are provided. (In the associated manuscript, these are referred to as Aggregates 1, 2, and 3.)"),
            p("Manuscript context:  These bargraphs are similar to Figure 2A, and Figure S2A, but with a different arrangement of bars.")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                selectInput(
                    inputId = ns("fraction_group_choice"),
                    label = "Select Cell Fraction Type",
                    choices = config_yaml$cell_type_aggregates,
                    selected = config_yaml$cell_type_aggregates[[3]]
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("barplot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("barplot_text"))
            )
        )
    )
}

cell_type_fractions_module <- function(
    input, 
    output, 
    session,
    group_display_choice, 
    feature_values_long_con,
    feature_con,
    group_con
){
    
    cf_feature_con <- reactive({
        req(feature_con(), input$fraction_group_choice)
        feature_con() %>%  
            dplyr::filter(class == local(input$fraction_group_choice))
    })
    
    cf_value_tbl <- reactive({
        req(feature_values_long_con(), cf_feature_con())
        build_cell_fractions_barplot_tbl(
            cf_feature_con(), feature_values_long_con()
        )
    })
    
    output$barplot <- renderPlotly({
        
        req(cf_value_tbl())

        validate(need(
            nrow(cf_value_tbl()) > 0,
            "Samples in current selected groups have no selected fraction data.")
        )

        create_barplot(
            cf_value_tbl(),
            color_col = "color",
            error_col = "error",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "cf_barplot"
        )

    })
    
    output$barplot_text <- renderText({
        req(group_con())
        eventdata <- event_data("plotly_click", source = "cf_barplot")
        validate(need(eventdata, "Click above plot"))
        
        group_con() %>% 
            dplyr::filter(group == local(unique(dplyr::pull(eventdata, "x")))) %>% 
            dplyr::mutate(text = paste0(group_name, ": ", characteristics)) %>%
            dplyr::pull(text)
    })
    
    
}

