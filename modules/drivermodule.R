generate_covariate_selector <- function(
    name, 
    ns_func,
    choices,
    label = "Select Covariate:"
){
    column(
        width = 12,
        selectInput(
            ns_func(name),
            label = label,
            choices = choices
        )
    )
}


drivers_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Association with Driver Mutations"),
        textBox(
            width = 12,
            p(
                "This module can be used to test whether an immune readout is statistically associated with ",
                ncol(panimmune_data$driver_mutation_df) - 1,
                " cancer driver mutations, within each of your sample groups."
            )  
        ),
        
        # single variable ----
        sectionBox(
            title = "Immune Response Association With Driver Mutations -- single variable",
            messageBox(
                width = 12,
                includeMarkdown("data/markdown/driver_single.markdown")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 4,
                        selectInput(
                            ns("response_variable"),
                            "Select Response Variable",
                            choices = get_feature_df_nested_list(),
                            selected = "Leukocyte Fraction"
                        )
                    ),
                    column(
                        width = 4,
                        numericInput(
                            ns("min_mut"),
                            "Minimum number of mutant samples per group:", 
                            20, 
                            min = 2
                        )
                    ),
                    column(
                        width = 4,
                        numericInput(
                            ns("min_wt"),
                            "Minimum number of wild type samples per group:", 
                            20, 
                            min = 2
                        )
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("scatter_plot")) %>% 
                        shinycssloaders::withSpinner()
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("violin_plot")) %>%
                        shinycssloaders::withSpinner()
                )
            )
        ),
        
        # multi_variable ----
        sectionBox(
            title = "Immune Response Association With Driver Mutations -- multi-variable",
            messageBox(
                width = 12,
                includeMarkdown("data/markdown/driver_single.markdown")
            ),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 4,
                        selectInput(
                            ns("response_variable_mv"),
                            "Select Response Variable",
                            choices = get_feature_df_nested_list(),
                            selected = "Leukocyte Fraction"
                        )
                    ),
                    column(
                        width = 4,
                        numericInput(
                            ns("min_mut_mv"),
                            "Minimum number of mutant samples per group:", 
                            20, 
                            min = 2
                        )
                    ),
                    column(
                        width = 4,
                        numericInput(
                            ns("min_wt_mv"),
                            "Minimum number of wild type samples per group:", 
                            20, 
                            min = 2
                        )
                    ),
                    column(
                        width = 4,
                        numericInput(
                            ns("n_covariates"),
                            "Number of covariates in model:", 
                            1,
                            min = 1
                        )
                    ),
                    column(
                        width = 4,
                        selectInput(
                            ns("group_mode"),
                            "Select mode", 
                            choices = c("By group", "Across groups"),
                            selected = "Across groups"
                        )
                    ),
                    uiOutput(ns("covariate_choice_ui")),
                    column(
                        width = 6,
                        textOutput(ns("model_text"))
                    ),
                    column(
                        width = 6,
                        actionButton(ns("mv_button"), "Calculate")
                    )
                )
            ),
            fluidRow(
                plotBox(
                    width = 12,
                    plotlyOutput(ns("scatter_plot2")) %>% 
                        shinycssloaders::withSpinner()
                )
            )
        )
    )
}

# Server ----
drivers <- function(
    input, 
    output, 
    session, 
    driver_result_df,
    metric_df,
    group_internal_choice
){
    
    ## single variable models ----
    scatter_plot_df <- reactive({
        driver_result_df() %>% 
            dplyr::filter(
                metric == input$response_variable,
                n_wt  >= input$min_wt,
                n_mut >= input$min_mut
            ) %>% 
            dplyr::select(x = effect_size, y = neg_log10_pvalue, label)
    })
    
    output$scatter_plot <- renderPlotly({

        create_scatterplot(
            scatter_plot_df(),
            xlab = "- log10(Effect Size)",
            ylab = "- log10(P-value)",
            title = "Immune Response Association With Driver Mutations",
            source = "scatterplot",
            key_col = "label",
            label_col = "label",
            horizontal_line = T,
            horizontal_line_y = (- log10(0.05))
        )
    })
    
    output$violin_plot <- renderPlotly({

        eventdata <- event_data("plotly_click", source = "scatterplot")
        
        # plot not clicked on yet
        validate(need(
            !is.null(eventdata),
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))

        selected_label <- eventdata$key[[1]]
        split_label <- selected_label %>% 
            stringr::str_split(";") %>% 
            unlist()
        
        selected_gene   <- split_label[[1]]
        selected_group  <- split_label[[2]]
        selected_es     <- eventdata$x[[1]]
        selected_pvalue <- eventdata$y[[1]]
        
        req(metric_df())
        
        metric_df <-  metric_df() %>% 
            dplyr::select(
                sample = "ParticipantBarcode",
                group_value = group_internal_choice(),
                metric_value = input$response_variable
            ) %>% 
            dplyr::filter(group_value == selected_group) %>% 
            tidyr::drop_na()
        
        # plot clicked on but event data stale due to parameter change
        validate(need(
            selected_group %in% metric_df$group_value,
            "Click a point on the above scatterplot to see a violin plot for the comparison"
        ))
        
        
        violin_plot_df <- panimmune_data$driver_mutations_df %>% 
            dplyr::select(
                sample = "ParticipantBarcode",
                mutation_status = selected_gene
            ) %>% 
            dplyr::inner_join(metric_df, by = "sample") %>% 
            dplyr::select(x = mutation_status, y = metric_value)
        
        title <- stringr::str_c(
            "Cohort:",
            selected_group,
            "; P-value:",
            round(selected_pvalue, 4),
            "; log10(Effect size):", 
            round(selected_es, 4),
            sep = " "
        )

        create_violinplot(
            violin_plot_df,
            xlab = stringr::str_c("Mutation status: ", selected_gene),
            ylab = get_variable_display_name(input$response_variable),
            title = title,
            fill_colors = c("blue"),
            showlegend = FALSE)
    })
    
    ## multi-variable models ----
    
    covariate_names <- reactive({
        req(input$n_covariates)
        1:input$n_covariates %>% 
            as.character %>% 
            stringr::str_c("covariate", .)
    })
    
    output$covariate_choice_ui <- renderUI({
        if(input$group_mode == "Across groups"){
            choices = c(get_covariate_nested_list(), get_group_nested_list())
        } else {
            choices = get_covariate_nested_list()
        }
        purrr::map(
            covariate_names(),
            generate_covariate_selector,
            session$ns,
            choices
        )
    })
    
    covariates <- reactive({
        req(covariate_names())
        purrr::walk(covariate_names(), ~req(input[[.x]]))
        purrr::map_chr(covariate_names(), ~magrittr::extract2(input, .x))
    })
    
    model_text <- reactive({
        req(covariates())
        
        covariates() %>% 
            stringr::str_c(collapse = " + ") %>% 
            stringr::str_c(
                input$response_variable_mv, 
                " ~ mutation_status + ",
                .
            )
    })
    
    output$model_text <- renderText({
        model_text()
    })

    scatter_plot_df2 <- reactive({
        req(
            metric_df(), 
            covariates(), 
            input$response_variable_mv,
            group_internal_choice(),
            input$group_mode,
            input$min_wt_mv, 
            input$min_mut_mv
        )
        
        mv_driver_df <- build_mv_driver_mutation_tbl(
            metric_df(),
            input$response_variable_mv,
            covariates(),
            input$group_mode,
            group_internal_choice(),
            input$min_wt_mv, 
            input$min_mut_mv
        )
        
        # plot clicked on but event data stale due to parameter change
        validate(need(
            nrow(mv_driver_df) > 0,
            "Response variable has no groups that meet current thresholding"
        ))
        
        build_mv_driver_mutation_scatterplot_df(
            mv_driver_df, 
            covariates()
        )
    })

    output$scatter_plot2 <- renderPlotly({
        
        input$mv_button
        
        isolate(create_scatterplot(
            scatter_plot_df2(),
            xlab = "- log10(Effect Size)",
            ylab = "- log10(P-value)",
            title = "Immune Response Association With Driver Mutations",
            source = "scatterplot",
            key_col = "label",
            label_col = "label",
            horizontal_line = T,
            horizontal_line_y = (- log10(0.05))
        ))
    })
    
}