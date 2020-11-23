# overall_cell_proportions_module ----

overall_cell_proportions_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Overall Cell Proportions",
        messageBox(
            width = 12,
            p("The barplots show the mean proportion of the tumor fraction, overall stromal fraction (one minus tumor fractions) and the leukocyte fraction of samples with each group.  Error bars show standard error of the mean. For reordering bars, first choose a variable (bar) to sort on, then a sorting function like Mean. Reordering function Max sorts on the mean+error and min sorts on the mean-error.")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                conditionalPanel(
                    condition =  "output.display_group_choice",
                    column(width = 4,uiOutput(ns("group_choice_ui"))),
                    ns = ns
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("reorder_cp_bars_var"),
                        "Reorder Variable", 
                        choices=c('Group','Tumor_Fraction','leukocyte_fraction', 'Stromal_Fraction'),
                        selected = 'Group'
                    )
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("reorder_cp_bars_functions"), 
                        "Reorder Function", 
                        choices=c('None','Mean','Max', 'Min'),
                        selected = 'None'
                    )
                )
            )
        ),
        # ** Overall proportions bar plot ----
        plotBox(
            width = 12,
            plotlyOutput(ns("overall_props_barplot")) %>% 
                shinycssloaders::withSpinner(),
            p(),
            textOutput(ns("op_barplot_group_text"))
        ),
        fluidRow(
            
            # ** Overall proportions correlation plots ----
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
                        
                        plotlyOutput(ns("lf_sf_corr_scatterplot")) %>% 
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
    group_internal_choice, 
    sample_group_df,
    subset_df
){
    # ** Overall proportions bar plot render ----
    output$overall_props_barplot <- renderPlotly({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        cellcontent_df <- build_cellcontent_df(
            subset_df(),
            group_column = group_internal_choice())
        
        validate(need(
            nrow(cellcontent_df) > 0, 
            "Samples in current selected groups have no fraction data."))
        
        barplot_df <- build_cellcontent_barplot_df2(
            cellcontent_df,
            x_column = "fraction_type",
            y_column = "fraction",
            input$reorder_cp_bars_var,
            input$reorder_cp_bars_functions)
        
        create_barplot(
            barplot_df,
            color_col = "color",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "op_barplot"
        )
    })
    
    output$op_barplot_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "op_barplot", 
            group_internal_choice(),
            sample_group_df()
        )
    })
    
    # ** Overall proportions scatter plot renders ----
    output$lf_sf_corr_scatterplot <- renderPlotly({
        
        eventdata <- event_data( "plotly_click", source = "op_barplot")
        selected_plot_subgroup <- eventdata$x[[1]]
        validate(
            need(all(!is.null(eventdata),
                     selected_plot_subgroup %in% magrittr::extract2(subset_df(), group_internal_choice())),
                 "Click bar plot"))
        
        scatterplot_df <- build_cellcontent_scatterplot_df(
            subset_df(),
            group_column = group_internal_choice(),
            group_filter_value = selected_plot_subgroup
        ) 
        
        create_scatterplot(
            scatterplot_df,
            xlab = "Stromal Fraction",
            ylab = "Leukocyte Fraction",
            label_col = "label",
            title = selected_plot_subgroup,
            identity_line = TRUE) %>% 
            layout(margin = list(t = 39))
    })
}

####################################
# cell_type_fractions_module ----###
####################################

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
                
                fluidRow(
                    column(
                        width = 8,
                        selectInput(
                            inputId = ns("cf_choice"),
                            label = "Select Cell Fraction Type",
                            choices = config_yaml$cell_type_aggregates,
                            selected = config_yaml$cell_type_aggregates[[3]]
                        )
                    )
                ),
                
                fluidRow(
                    column(
                        width = 4,
                        uiOutput(ns("create_dropdown_cell_types_control")) # ns("reorder_bars_cf_cond_var")
                    ),
                    column(
                        width = 4,
                        selectInput(
                            ns("reorder_cf_bars_functions"), 
                            "Reorder Function", 
                            choices=c('None','Mean','Max', 'Min'),
                            selected = 'None'
                        )
                    )
                )
            )
        ),
        fluidRow(
            
            # ** Cell fractions bar plot ----
            plotBox(
                width = 12,
                plotlyOutput(ns("cell_frac_barplot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("cf_barplot_group_text"))
            )
        )
    )
}



cell_type_fractions_module <- function(
    input, 
    output, 
    session,
    group_display_choice, 
    group_internal_choice, 
    sample_group_df,
    subset_df
){
    ns <- session$ns
    
    output$create_dropdown_cell_types_control <- renderUI({
        selectInput(
            ns("reorder_cf_bars_var"),
            "Reorder Variable", 
            choices=c('Group', get_factored_variables_from_feature_df(
                input$cf_choice) %>% 
                as.character),
            selected = 'Group')
    })

    output$cell_frac_barplot <- renderPlotly({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        cell_fractions <- get_factored_variables_from_feature_df(
            input$cf_choice) %>% 
            as.character
        
        cell_fraction_df <- build_cell_fraction_df(
            subset_df(), 
            group_column = group_internal_choice(), 
            value_columns = cell_fractions
        )
        
        validate(need(
            nrow(cell_fraction_df) > 0, 
            "Samples in current selected groups have no selected fraction data."))
        
        barplot_df <- 
            build_cellcontent_barplot_df2(
                cell_fraction_df,
                y_column = "fraction",
                x_column = "fraction_type",
                input$reorder_cf_bars_var,
                input$reorder_cf_bars_functions) %>%
            mutate(color = purrr::map_chr(color, get_variable_display_name))
        
        create_barplot(
            barplot_df,
            color_col = "color",
            error_col = "se",
            label_col = "label",
            xlab = "Fraction type by group",
            ylab = "Fraction mean",
            source_name = "cf_barplot"
        )
        
    })
    
    output$cf_barplot_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "cf_barplot", 
            group_internal_choice(),
            sample_group_df = sample_group_df()
        )
    })
    
    
}

###########################
# data_table_module ----###
###########################

data_table_module_UI <- function(
    id, 
    title = "", 
    message_html = ""
){
    
    ns <- NS(id)
    
    sectionBox(
        title = title,
        messageBox(width = 12, message_html),
        fluidRow(
            tableBox(
                width = 12,
                div(style = "overflow-x: scroll",
                    DT::dataTableOutput(ns("data_table_module")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}

data_table_module <- function(
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
