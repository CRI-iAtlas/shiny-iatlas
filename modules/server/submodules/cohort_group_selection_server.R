cohort_group_selection_server <- function(
    input, 
    output, 
    session,
    feature_named_list,
    sample_ids,
    selected_dataset,
    available_groups
){
    ns <- session$ns
    
    
    source("functions/cohort_group_selection_functions.R", local = T)
    
    default_group <- shiny::reactive({
        shiny::req(selected_dataset())
        if(selected_dataset() == "TCGA"){
            group = "Immune Subtype"
        } else {
            stop("Dataset not currently supported!")
        }
        return(group)
    })
    

    output$select_group_ui <- shiny::renderUI({
        
        shiny::req(available_groups(), default_group())
        
        shiny::selectInput(
            inputId = ns("group_choice"),
            label = strong("Select or Search for Grouping Variable"),
            choices = c(available_groups()),
            selected = default_group()
        )
    })
    
    group_choice <- shiny::reactive({
        req(default_group())
        if(is.null(input$group_choice)){
            group_choice <- default_group()
        } else {
            group_choice <- input$group_choice
        }
        return(group_choice)
    })
    
    # This is so that the conditional panel can see the various shiny::reactives
    output$display_driver_mutation <- shiny::reactive(group_choice() == "Driver Mutation")
    shiny::outputOptions(output, "display_driver_mutation", suspendWhenHidden = FALSE)
    
    default_driver_gene <- "ABL1"
    
    default_driver_gene_id <- .GlobalEnv$get_gene_id(default_driver_gene)

    output$select_driver_mutation_group_ui <- shiny::renderUI({
        shiny::req(req(input$group_choice == "Driver Mutation"))
        selectInput(
            inputId = ns("driver_mutation_choice"),
            label = "Select or Search for Driver Mutation",
            choices = create_driver_mutation_list(),
            selected = default_driver_gene_id
        )
    })
    
    driver_gene_id <- shiny::reactive({
        if(is.null(input$driver_mutation_choice)){
            return(default_driver_gene_id)
        } else {
            return(input$driver_mutation_choice)
        }
    })
    
    driver_gene_name <- reactive({
        shiny::req(driver_gene_id())
        .GlobalEnv$get_gene_name(driver_gene_id())
    })
    
    # This is so that the conditional panel can see the various shiny::reactives
    output$display_immune_feature_bins <- shiny::reactive(group_choice() == "Immune Feature Bins")
    shiny::outputOptions(output, "display_immune_feature_bins", suspendWhenHidden = FALSE)
    
    output$select_immune_feature_bins_group_ui <- shiny::renderUI({
        shiny::req(feature_named_list())
        shiny::selectInput(
            inputId = ns("immune_feature_bin_choice"),
            label = "Select or Search for feature",
            choices = feature_named_list()
        )
    })
    
    immune_feature_bin_name <- reactive({
        shiny::req(input$immune_feature_bin_choice)
        .GlobalEnv$get_feature_name(input$immune_feature_bin_choice)
    })
    
    cohort_obj <- reactive({
        req(
            group_choice(), 
            sample_ids(),
            selected_dataset(),
            available_groups()
        )
        if (group_choice() %in% c("Immune Subtype", "TCGA Subtype", "TCGA Study")){
            cohort_tbl  <- create_tag_cohort_tbl(sample_ids(), group_choice())
            sample_tbl  <- create_tag_sample_tbl(cohort_tbl)
            group_tbl   <- create_tag_group_tbl(cohort_tbl)
            group_name  <- group_choice()
            plot_colors <- create_tag_plot_colors(cohort_tbl)
        } else if (group_choice() == "Driver Mutation") {
            shiny::req(driver_gene_id(), driver_gene_name())
            sample_tbl <- create_mutation_sample_tbl(
                sample_ids(), 
                driver_gene_id()
            )
            group_tbl   <- create_mutation_group_tbl(
                sample_tbl,
                driver_gene_name()
            )
            group_name  <- paste("Mutation Status:", driver_gene_name())
            plot_colors <- create_mutation_plot_colors(sample_tbl)
        } else if (group_choice() == "Immune Feature Bins"){
            shiny::req(
                input$immune_feature_bin_choice, 
                input$immune_feature_bin_number,
                immune_feature_bin_name()
            )
            sample_tbl <- create_feature_bin_sample_tbl(
                sample_ids(),
                input$immune_feature_bin_choice, 
                input$immune_feature_bin_number
            ) 
            group_tbl   <- create_feature_bin_group_tbl(
                sample_tbl,
                immune_feature_bin_name()
            )
            group_name  <- paste(
                "Immune Feature Bins:", 
                immune_feature_bin_name()
            )
            plot_colors <- create_feature_bin_plot_colors(sample_tbl)
        }
        list(
            "sample_tbl"  = sample_tbl, 
            "group_tbl"   = group_tbl, 
            "group_name"  = group_name,
            "plot_colors" = plot_colors,
            "dataset"     = selected_dataset(),
            "groups"      = available_groups()
        )
    })
    return(cohort_obj)
}