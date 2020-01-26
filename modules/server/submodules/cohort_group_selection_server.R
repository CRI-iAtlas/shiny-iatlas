cohort_group_selection_server <- function(
    input, 
    output, 
    session,
    feature_named_list,
    sample_ids,
    selected_dataset
){
    ns <- session$ns
    
    source("functions/cohort_group_selection_functions.R", local = T)
    
    dataset_to_group_tbl <- dplyr::tribble(
        ~group,                 ~dataset, ~type,
        "Immune Subtype",       "TCGA",   "tag",
        "TCGA Subtype",         "TCGA",   "tag",
        "TCGA Study",           "TCGA",   "tag",
        # "Gender",          "TCGA",   "sample",
        # "Race",            "TCGA",   "sample",
        # "Ethnicity",       "TCGA",   "sample",
        "Immune Feature Bins",  "TCGA",    NA,
        "Driver Mutation",      "TCGA",    NA
        # "Immune Subtype",  "PCAWG",  "tag",
        # "PCAWG Study",     "PCAWG",  "tag",
        # "Gender",          "PCAWG",  "sample",
        # "Race",            "PCAWG",  "sample"
    )
    
    available_groups <- shiny::reactive({
        shiny::req(selected_dataset())
        dataset_to_group_tbl %>% 
            dplyr::filter(dataset == selected_dataset()) %>% 
            dplyr::pull(group)
    })
    
    default_group <- shiny::reactive({
        shiny::req(available_groups())
        available_groups()[[1]]
    })
    

    output$select_group_ui <- shiny::renderUI({
        
        shiny::req(available_groups(), default_group())
        
        shiny::selectInput(
            inputId = ns("group_choice"),
            label = strong("Select or Search for Grouping Variable"),
            choices = available_groups(),
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
    
    
    cohort_obj <- reactive({
        req(
            group_choice(), 
            sample_ids(),
            selected_dataset(),
            available_groups()
        )
        if (group_choice() == "Driver Mutation") {
            shiny::req(driver_gene_id())
        } else if (group_choice() == "Immune Feature Bins"){
            shiny::req(
                input$immune_feature_bin_choice,
                input$immune_feature_bin_number
            )
        }
        create_cohort_oject(
            sample_ids(),
            group_choice(),
            selected_dataset(),
            available_groups(),
            driver_gene_id(), 
            input$immune_feature_bin_choice,
            input$immune_feature_bin_number
        )
    })
    
    return(cohort_obj)
}