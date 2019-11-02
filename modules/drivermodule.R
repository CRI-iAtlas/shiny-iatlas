drivers_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Association with Driver Mutations"),
        textBox(
            width = 12,
            includeMarkdown("data/markdown/driver.markdown")
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
            volcano_plot_module_ui(ns("single_variable"))
        ),
        
        # multi_variable ----
        sectionBox(
            title = "Immune Response Association With Driver Mutations -- multi-variable",
            messageBox(
                width = 12,
                includeMarkdown("data/markdown/driver_multi.markdown")
            ),
            model_selection_module_ui(ns("module1")),
            fluidRow(
                optionsBox(
                    width = 12,
                    column(
                        width = 6,
                        textOutput(ns("model_text"))
                    ),
                    column(
                        width = 6,
                        actionButton(ns("calculate_button"), "Calculate")
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
    volcano_df <- reactive({
        x <- driver_result_df() %>% 
            dplyr::filter(
                metric == input$response_variable,
                n_wt  >= input$min_wt,
                n_mut >= input$min_mut
            ) %>% 
            dplyr::rename(
                LABEL = label,
                METRIC = metric
            )
        print("Volcano df")
        print(x)
        return(x)
    })
    
    violin_df <- reactive({
        mutation_df <-  panimmune_data$driver_mutations_df %>% 
            dplyr::select(
                SAMPLE = "ParticipantBarcode",
                dplyr::everything()) %>% 
            tidyr::gather(GENE, STATUS, -SAMPLE) %>% 
            tidyr::drop_na()
        
        metric_df <- metric_df() %>% 
            dplyr::select(
                SAMPLE = "ParticipantBarcode",
                GROUP = group_internal_choice(),
                METRIC = input$response_variable
            ) %>% 
            tidyr::drop_na()
        
        violin_df <- 
            dplyr::inner_join(mutation_df, metric_df, by = "SAMPLE") %>% 
            dplyr::select(-SAMPLE) %>% 
            dplyr::mutate(LABEL = stringr::str_c(GENE, GROUP, sep = ";")) %>% 
            dplyr::select(-c(GENE, GROUP)) %>% 
            dplyr::rename(GROUP = STATUS) %>% 
            dplyr::mutate(GROUP = forcats::fct_relevel(
                GROUP,
                "Wt",
                "Mut"
            ))
        
        print("Violin df")
        print(violin_df)
        return(violin_df)
    })
    
    callModule(
        volcano_plot_module,
        "single_variable",
        volcano_df,
        violin_df,
        "Immune Response Association With Driver Mutations",
        "driver_single_variable",
        "Wt",
        "Mut"
    ) 

    
    ## multi-variable models ----
    
    module_parameters <- callModule(model_selection_module, "module1")
    
    covariates <- reactive({
        c(
            module_parameters()$numerical_covariates,
            module_parameters()$categorical_covariates
        )
    })
    
    transformations <- reactive({
        c(
            module_parameters()$numerical_transformations,
            module_parameters()$categorical_transformations
        )
    })
    
    display_formula_string <- reactive({
        req(module_parameters(), covariates(), transformations())
        response  <- module_parameters()$response_variable
        
        transform_variable <- function(var, trans){
            if(trans == "None") res <- var
            else if (trans == "Squared") res <- stringr::str_c(var, "**2")
            else if (trans == "Log10") res <- stringr::str_c("log10(", var, ")")
            else if (trans == "Reciprocal")  res <- stringr::str_c("1/", var)
            return(res)
        }
        
        string <-  covariates() %>% 
            purrr::map(get_variable_display_name) %>% 
            purrr::map2_chr(transformations(), transform_variable) %>% 
            stringr::str_c(collapse = " + ") %>% 
            stringr::str_c(
                get_variable_display_name(response),
                " ~ Mutation status + ", 
                .
            )
    })
    
    model_formula_string <- reactive({
        
        req(covariates(), transformations())
        
        transform_variable <- function(var, trans){
            if(trans == "None") res <- var
            else if (trans == "Squared") res <- stringr::str_c("I(", var, "**2)")
            else if (trans == "Log10") res <- stringr::str_c("I(log10(", var, "))")
            else if (trans == "Reciprocal")  res <- stringr::str_c("I(1/", var, ")")
            return(res)
        }
        
        string <- covariates() %>% 
            purrr::map2_chr(transformations(), transform_variable) %>% 
            stringr::str_c(collapse = " + ") %>% 
            stringr::str_c("RESPONSE ~ STATUS + ", .) 
    })

    output$model_text <- renderText({
        display_formula_string()
    })

    scatter_plot_df2 <- reactive({
        req(
            metric_df(),
            module_parameters(),
            group_internal_choice(),
        )
        
        # no covariates not currently supported
        validate(need(
            !is.null(covariates()),
            "Please select a covariate"
        ))

        mv_driver_df <- build_mv_driver_mutation_tbl(
            metric_df(),
            module_parameters()$response_variable,
            covariates(),
            module_parameters()$group_mode,
            group_internal_choice(),
            module_parameters()$min_wildtype,
            module_parameters()$min_mutants,
            module_parameters()$scale_response
        )
        
        # plot clicked on but event data stale due to parameter change
        validate(need(
            nrow(mv_driver_df) > 0,
            "Response variable has no groups that meet current thresholding"
        ))
        
        print("Volcano df2")
        print(mv_driver_df)

        x <- build_mv_driver_mutation_scatterplot_df(
            mv_driver_df,
            covariates(),
            model_formula_string()
        )
        print(x)
        return(x)
    })

    output$scatter_plot2 <- renderPlotly({
        
        req(input$calculate_button > 0)
        input$calculate_button

        isolate(create_scatterplot(
            scatter_plot_df2(),
            xlab = "Log10(Fold Change)",
            ylab = "- Log10(P-value)",
            title = "Immune Response Association With Driver Mutations",
            source = "scatterplot",
            key_col = "label",
            label_col = "label",
            color_col = "color",
            fill_colors = c("blue" = "blue", "red" = "red"),
            horizontal_line = T,
            horizontal_line_y = (- log10(0.05))
        ))
    })
    
}