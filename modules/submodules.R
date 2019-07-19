# immune_feature_distributions ----

immune_feature_distributions_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Distributions",
        messageBox(
            width = 12,
            p("This displays the value of immune readouts by sample group. Select a variable class to see the distribution of variables within that class displayed as as violin plot."),
            p("Manuscript context: This allows you to display distributions such as those shown in Figures 1C and 1D.")
        ),
        fluidRow(
            optionsBox(
                width = 6,
                selectInput(
                    ns("violin_y"),
                    "Select Violin Plot Y Variable",
                    choices = get_feature_df_nested_list()
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("violinPlot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("violin_group_text"))
            )
        )
    )
}

immune_feature_distributions_module <- function(
    input, 
    output, 
    session,
    subset_df,
    sample_group_df,
    group_internal_choice,
    group_display_choice,
    plot_colors
){
    output$violinPlot <- renderPlotly({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        display_y  <- get_variable_display_name(input$violin_y)
        
        plot_df <- build_immunefeatures_violin_plot_df(
            subset_df(), 
            x_col = group_internal_choice(),
            y_col = input$violin_y) 
        
        validate(
            need(nrow(plot_df) > 0, 
                 "Current selected group and selected variable have no overlap")
        )
        
        create_violinplot(
            plot_df,
            xlab = group_display_choice(),
            ylab = display_y,
            fill_colors = plot_colors(),
            source_name = "violin"
        )
    })
    
    
    output$violin_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "violin", 
            group_internal_choice(),
            sample_group_df())
    })
}

# immunomodulator_distributions_module ----

immunomodulator_distributions_module_ui <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Immunomodulator Distributions",
        messageBox(
            width = 12,
            p("Select Immumodulator Gene to see its expression in the data set. Use Select Immumodulator Category (drop-down menu on the right) to organize the selection by particular categories. The categories will subsequently appear in the left drop-down menu. The Categories are:"),
            tags$ul(
                tags$li(em('Gene Family'), ", such as TNF, MHC Class II, Immunoglobulin, or CXC chemokine"), 
                tags$li(em('Super Category'), ", such as Ligand, Receptor, or Antigen Presentation"),
                tags$li(em('Immune Checkpoint'), " classified as  Inhibitory or Stimulatory")
            ),
            p(""),
            p("Manuscript context:  If you are looking at Immune Subtypes, select EDNRB or CXCL10 to get figure 6B."),
            p("You can view a histogram for any indvidual distributions by clicking on its violin plot.")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 6,
                    uiOutput(ns("gene_choices"))
                ),
                column(
                    width = 6,
                    selectInput(
                        inputId = ns("im_category_choice_choice"),
                        label = "Select Immunomodulator Category",
                        choices = c("Gene Family", "Super Category", "Immune Checkpoint")
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("violinPlot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("violin_group_text"))
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("histPlot")) %>% 
                    shinycssloaders::withSpinner()
            )
        )
    )
}

immunomodulator_distributions_module <- function(
    input, 
    output, 
    session, 
    group_display_choice, 
    group_internal_choice, 
    sample_group_df,
    subset_df, 
    plot_colors
){
    ns <- session$ns
    
    # reactives ----
    
    expression_df <- reactive({
        
        req(input$im_gene_choice,
            subset_df(), 
            group_internal_choice(), 
            cancelOutput = T)
        
        build_immunomodulator_expression_df(
            subset_df(),
            filter_value = input$im_gene_choice, 
            group_col = group_internal_choice())
    })
    
    # ui ----
    
    output$gene_choices <- renderUI({
        choices <- get_immunomodulator_nested_list(
            class_column = input$im_category_choice_choice)
        selectInput(
            ns("im_gene_choice"),
            label = "Select Immunomodulator Gene",
            choices = choices)
    })
    
    # output ----
    
    output$violinPlot <- renderPlotly({
        
        validate(need(
            nrow(expression_df()) > 0, 
            "Samples in current selected groups have no expression data for the currently selected gene."))
        
        violin_plot_df <- 
            expression_df() %>% 
            build_immunomodulator_violin_plot_df() 
        
        create_violinplot(
            violin_plot_df,
            xlab = group_display_choice(), 
            ylab = "log10(count + 1)",
            source_name = "violin",
            fill_colors = plot_colors()
        )
    })
    
    
    output$violin_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "violin", 
            group_internal_choice(),
            sample_group_df())  
    })
    
    output$histPlot <- renderPlotly({
        
        validate(need(
            nrow(expression_df()) > 0, 
            "Samples in current selected groups have no expression data for the currently selected gene."))
        
        eventdata <- event_data("plotly_click", source = "violin")
        validate(need(!is.null(eventdata), "Click violin plot above"))
        clicked_group <- eventdata$x[[1]]
        
        current_violin_groups <- expression_df() %>% 
            magrittr::use_series(GROUP) %>% 
            unique
        
        validate(need(clicked_group %in% current_violin_groups, "Click violin plot above"))
        
        histogram_df <-  build_immunomodulator_histogram_df(expression_df(), clicked_group) 
        
        
        create_histogram(
            histogram_df ,
            x_lab = "log10(count + 1)",
            title = clicked_group)
        
    })
}

# overall_cell_proportions_module ----

overall_cell_proportions_module_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "Overall Cell Proportions",
        messageBox(
            width = 12,
            p("The barplots show the mean proportion of the tumor fraction, overall stromal fraction (one minus tumor fractions) and the leukocyte fraction of samples with each group.  Error bars show standard error of the mean.")
        ),
        fluidRow(
            
            # ** Overall proportions bar plot ----
            plotBox(
                width = 12,
                plotlyOutput(ns("overall_props_barplot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("op_barplot_group_text"))
            )
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
        
        barplot_df <- build_cellcontent_barplot_df(
            cellcontent_df,
            x_column = "fraction_type",
            y_column = "fraction")
        
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

# cell_type_fractions_module ----

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
                    inputId = ns("cf_choice"),
                    label = "Select Cell Fraction Type",
                    choices = config_yaml$cell_type_aggregates,
                    selected = config_yaml$cell_type_aggregates[[3]]
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
            build_cellcontent_barplot_df(
                cell_fraction_df,
                y_column = "fraction",
                x_column = "fraction_type") %>%
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

# data_table_module ----

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
    df,
    options = list(pageLength = 10),
    color = F,
    color_column = NULL,
    colors = NULL,
    ...
){
    output$data_table_module <- DT::renderDT({
        dt <- DT::datatable(
            df,
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

# TIL_map_characteristics ----

TIL_map_characteristics_UI <- function(id){
    
    ns <- NS(id)
    
    sectionBox(
        title = "TIL Map Characteristics",
        messageBox(
            width = 12,
            p("Select a TIL map characteristic to see its distribution over sample groups. Plots are available as violin plots, and box plots with full data points superimposed."),
            p("Main immune manuscript context:  If you are looking at immune subtypes, select TIL Regional Fraction to get Figure 3B.")
        ),
        fluidRow(
            optionsBox(
                width = 12,
                column(
                    width = 4,
                    selectInput( ## would be good to initiate on til_percentage/"TIL Regional Fraction (Percent)"
                        ns("violin_y"),
                        "Select TIL Map Characteristic",
                        get_tilmap_nested_list()
                    )
                ),
                column(
                    width = 4,
                    selectInput(
                        ns("plot_type"),
                        "Select Plot Type",
                        choices = c("Violin", "Box")
                    )
                )
            )
        ),
        fluidRow(
            plotBox(
                width = 12,
                plotlyOutput(ns("plot")) %>% 
                    shinycssloaders::withSpinner(),
                p(),
                textOutput(ns("plot_group_text")),
                h4("Click point or violin/box to filter samples in table below")
            )
        )
    )
}

TIL_map_characteristics <- function(
    input, 
    output, 
    session,
    subset_df,
    sample_group_df,
    group_internal_choice,
    group_display_choice,
    plot_colors
){
    output$plot <- renderPlotly({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        display_y  <- get_variable_display_name(input$violin_y)
        
        plot_df <- subset_df() %>%
            dplyr::select(x = group_internal_choice(), y = input$violin_y, label = "Slide") %>%
            tidyr::drop_na()
        
        validate(
            need(nrow(plot_df) > 0, 
                 "Group choices have no overlap with current variable choice"))
        
        if(input$plot_type == "Violin"){
            plot_df %>%
                create_violinplot(
                    xlab = group_display_choice(),
                    ylab = display_y,
                    source_name = "plot",
                    fill_colors = plot_colors(),
                    key_col = "label"
                )
        } else {
            plot_df %>% 
                create_boxplot(
                    xlab = group_display_choice(),
                    ylab = display_y,
                    source_name = "plot",
                    fill_colors = plot_colors(),
                    key_col = "label"
                )
        }
    })
    
    output$plot_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
        
        create_group_text_from_plotly(
            "plot",
            group_internal_choice(),
            sample_group_df(),
            prompt_text = "",
            key_column = "x")
    })
    
    plot_data <- reactive({
        print("reactive")
        event_data("plotly_click", source = "plot")
    })
    
    return(plot_data)
}

# data_table_module2 ----

# data_table_module2_UI <- function(
#     id, 
#     title = "", 
#     message_html = ""
# ){
#     
#     ns <- NS(id)
#     
#     sectionBox(
#         title = title,
#         messageBox(width = 12, message_html),
#         # fluidRow(
#         #     optionsBox(
#         #         width = 6,
#         #         uiOutput(ns("classes")))),
#         fluidRow(
#             optionsBox(
#                 width = 6,
#                 selectInput(
#                     "class_choice",
#                     label = "Select Class",
#                     choices = c("All classes", "Overall Proportion")))),
#         fluidRow(
#             tableBox(
#                 width = 12,
#                 div(style = "overflow-x: scroll",
#                     DT::dataTableOutput(ns("data_table_module")) %>%
#                         shinycssloaders::withSpinner()
#                 )
#             )
#         )
#     )
# }
# 
# data_table_module2 <- function(
#     input, 
#     output, 
#     session,
#     df,
#     options = list(pageLength = 10),
#     color = F,
#     color_column = NULL,
#     colors = NULL,
#     ...
# ){
#     
#     # ns <- session$ns
#     # 
#     # output$classes <- renderUI({
#     #     choices <- panimmune_data$feature_df %>% 
#     #         magrittr::use_series(`Variable Class`) %>% 
#     #         sort %>% 
#     #         unique %>% 
#     #         c("All classes", .)
#     #     selectInput(
#     #         ns("class_choice"),
#     #         label = "Select Class",
#     #         choices = choices)
#     # })
#     
#     table_df <- reactive({
#         df <- panimmune_data$feature_df %>% 
#             dplyr::select(
#                 `Feature Name` = FriendlyLabel, 
#                 `Variable Class`, 
#                 Unit, 
#                 VariableType
#             ) 
#         if(input$class_choice != "All classes"){
#             df <- dplyr::filter(df, `Variable Class` == input$class_choice)
#         }
#         return(df)
#     })
#     
#     output$data_table_module <- DT::renderDT({
#         dt <- DT::datatable(
#             df,
#             options = options,
#             rownames = FALSE,
#             ...
#         )
#         if(color){
#             dt <-  DT::formatStyle(
#                 dt,
#                 color_column,
#                 backgroundColor = DT::styleEqual(colors, colors))
#         }
#         return(dt)
#     })
# }
