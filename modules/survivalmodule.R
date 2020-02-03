# UI ----

survival_UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        titleBox("iAtlas Explorer — Clinical Outcomes"),
        textBox(
            width = 12,
            p("Plot survival curves based on immune characteristics and identify variables associated with outcome.")  
        ),
        
        # Survival comparison section ----
        sectionBox(
            title = "Sample Group Survival",
            messageBox(
                width = 12,
                p("Select the variable, and outcome in terms of either overall survival (OS) or progression free interval (PFI) endpoints to generate a Kaplan-Meier plot. For a continuous (numeric) variable, the slider can be used to specify how the range of values of that variable is split.  Selecting 2 splits the values by the middle of the range, 3 splits the range into three even intervals and so on."),
                p("For immune subtypes Figure 3A can be generated (OS), and Figure S3A for (PFI).")
            ),
            fluidRow(
                optionsBox(
                    width = 4,
                    
                    uiOutput(ns("survplot_opts")),
                    
                    uiOutput(ns("survplot_study_opts")),
                    
                    selectInput(
                        ns("timevar"),
                        "Survival Endpoint",
                        c("Overall Survival" = "OS_time", "Progression Free Interval" = "PFI_time_1"),
                        selected = "OS_time"
                    ),
                    
                    sliderInput(
                        ns("divk"),
                        "Value Range Divisions",
                        min = 2,
                        max = 10,
                        value = 2
                    ),
                    
                    checkboxInput(ns("confint"), "Confidence Intervals", value = F),
                    checkboxInput(ns("risktable"), "Risk Table", value = T)
                ),
                
                # ** Survival Kaplan-Meier plot ----
                plotBox(
                    width = 8,
                    plotOutput(ns("survPlot"), height = 600) %>%
                        shinycssloaders::withSpinner()
                )
            )
        ),
        
        # Survival comparison section ----
        sectionBox(
            title = "Concordance Index",
            messageBox(
                width = 12,
                p("Here, you can explore which variables are associated with improved or diminished survival within your sample groups. Select a variable class, and you will get a heatmap, with one row for each variable in that class. For a given variable (row) and sample group (column) red denotes decreased survival, and blue increased survival as the variable is increased."),
                p("Manuscript context:  Selecting variable class “Core Expression Signature”, you can generate Figure 3B. Figures 3C, and Figures S3B, S3C, and S3C can also be generated with different selection options.")
            ),
            fluidRow(
                optionsBox(
                    width = 4,
                    radioButtons(
                        ns("survival_type"),
                        "Survival Endpoint",
                        c("Progression Free Interval" = "PFI",
                          "Overall Survival" = "OS"
                        ),
                        selected = "PFI"
                    ),
                    selectInput(
                        ns("survival_class"),
                        "Select Variables Class (rows)",
                        choices = get_numeric_classes_from_feature_df(),
                        selected = "T Helper Cell Score"
                    )
                ),
                plotBox(
                    width = 8,
                    fluidRow(
                        plotlyOutput(ns("heatmapplot"), height = 600) %>%
                            shinycssloaders::withSpinner(),
                        p(),
                        textOutput(ns("heatmap_group_text"))
                    )
                )
            )
        )
    )
}

# Server ----
survival <- function(
    input, 
    output, 
    session, 
    ss_choice,
    group_internal_choice,
    group_options, 
    sample_group_df,
    subset_df,
    plot_colors
){
    ns <- session$ns

    
    
    # This function creates the drop down with variable selection like signature scores.
    output$survplot_opts <- renderUI({
      
      group_choice <- magrittr::set_names(list(group_internal_choice()), ss_choice())
      
      var_choices <- c(
        list("Current Sample Groups" = group_choice),
        get_feature_df_nested_list())
      
      if (group_choice == 'Study') {  # if we start out at study, then display SOMETHING
        selectInput(
          ns("var1_surv"),
          "Variable",
          var_choices,
          selected = 'leukocyte_fraction'
        )  
        
      } else {  # starting at immune subtypes, we can display those.
        selectInput(
          ns("var1_surv"),
          "Variable",
          var_choices,
          selected = group_internal_choice()
        )  
      }
      
    })
    
        
    # this is to make a drop down appear when the TCGA study is selected... to select a specific study.
    output$survplot_study_opts <- renderUI({
      
      group_choice <- magrittr::set_names(list(group_internal_choice()), ss_choice())
        
      print('group choice') 
      print(group_choice)
      
      if (group_choice == 'Study') {
        
          study_choices <- c('All', na.omit(panimmune_data$sample_group_df %>% 
            dplyr::select("FeatureDisplayName", "TCGA Studies") %>% 
            dplyr::distinct() %>% 
            dplyr::arrange(`TCGA Studies`) %>%
            tibble::deframe())
          )
        
          selectInput(
            ns("var0_study"),
            "Select Study",
            study_choices,
            selected = 'All'
          )  
          
      } else if (group_choice == "Subtype_Immune_Model_Based") {

          subtypes <- c("All", "C1", "C2", "C3", "C4", "C5", "C6")
        
          selectInput(
            ns("var0_study"),
            "Select Study",
            subtypes,
            selected = 'All'
          )  
        }
        
    })
    
        
    # here we put the plot together, using the prev. selected options.
    output$survPlot <- renderPlot({
      
        req(!is.null(subset_df()), cancelOutput = T)
      
        group_choice <- magrittr::set_names(list(group_internal_choice()), ss_choice())
      
        sample_groups <- get_unique_column_values(group_internal_choice(), subset_df())
        
        print('in output surv plot')
        print('group choice')
        print(group_choice)
        print('sample groups')
        print(sample_groups)
        
        n_groups <- dplyr::n_distinct(sample_groups)

        print(n_groups)
        
        validate(
            need(input$var1_surv, "Waiting for input."),
            need(dplyr::n_distinct(sample_groups) <= 10 | !input$var1_surv == group_internal_choice(), 
                 paste0("Too many sample groups (", n_groups, ") for KM plot; ",
                        "choose a continuous variable or select different sample groups."))
        )
        
        survival_df <- subset_df() %>%
            build_survival_df(
                group_column = input$var1_surv,
                group_options = purrr::map(group_options(), get_group_internal_name),
                time_column = input$timevar,
                k = input$divk,
                group_choice,
                input$var0_study
            )

        print('survival df')
        print(head(survival_df))
        
        #survival_df %>% 
        #  dplyr::group_by(variable) %>% 
        #  dplyr::summarize(Num1 = sum(status == 1), Num0 = sum(status == 0))
        
        print('variable')
        print(unique(survival_df$variable))
        
        # if you pick Immune Subtype as variable, and then C1, there is no variation in the variable.
        validate(
          need(length(unique(survival_df$variable)) > 1, "No results to display, pick a different variable.")
        )
        
        fit <- survival::survfit(survival::Surv(time, status) ~ variable, data = survival_df)
        
        title <- ''
        try({
          title <- get_variable_display_name(input$var1_surv)
        })


        if (identical(title, character(0))){
            title <- input$var1_surv
        }
        
        if (title %in% group_options()) { 
            group_colors <- plot_colors()

            if (title == "TCGA Subtype") {
              # ggsurvplot takes the subtype names and pastes the column name onto it 'variable='
              # and colors need to have this modified name.
              group_colors <- group_colors[names(group_colors) %in% survival_df$variable]
            }

            names(group_colors) <- sapply(names(group_colors), function(a) paste('variable=',a,sep='')) 
            
        } else {
            group_colors <- viridisLite::viridis(input$divk)
        }
        
        create_kmplot(
            fit = fit, 
            df = survival_df, 
            confint = input$confint, 
            risktable = input$risktable, 
            title = title, 
            group_colors = group_colors)
    })
    
    
    output$heatmapplot <- renderPlotly({
        
        req(!is.null(subset_df()), cancelOutput = T)
        
        if(input$survival_type == "PFI"){
            time_col <- "OS_time"
            status_col <- "OS"
        } else{
            time_col <- "PFI_time_1"
            status_col <- "PFI_1"
        }
        
        features <- get_factored_variables_from_feature_df(
            input$survival_class) %>% 
            as.character
        
        ci_mat <- subset_df() %>%
            build_ci_mat(
                group_column = group_internal_choice(),
                value_columns = features,
                time_column = time_col,
                status_column = status_col
            )
        
        validate(
          need(nrow(ci_mat[rowSums(is.na(ci_mat)) == 0,]) > 0, "No results to display, pick a different group.")
        )
        
        create_heatmap(ci_mat, "ci")
    })

        
    output$heatmap_group_text <- renderText({
        req(group_internal_choice(), sample_group_df(), cancelOutput = T)
      
        create_group_text_from_plotly(
            "ci",
            group_internal_choice(), 
            sample_group_df(),
            key_column = "x")
    })
        
    
}


