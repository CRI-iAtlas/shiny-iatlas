cohort_dataset_selection_server <- function(
    input, 
    output, 
    session,
    default_dataset
){
    ns <- session$ns
    
    dataset_to_module_tbl <- dplyr::tribble(
        ~module,                  ~dataset,
        "Sample Group Overview",  "TCGA",
        "Tumor Microenvironment", "TCGA",
        "Immune Feature Trends",  "TCGA",
        "Clinical Outcomes",      "TCGA",
        "IO Targets",             "TCGA",
        "TIL Maps",               "TCGA",
        "Driver Associations",    "TCGA"
        # "Sample Group Overview",  "PCAWG",
        # "Tumor Microenvironment", "PCAWG",
        # "Immune Feature Trends",  "PCAWG",
        # "IO Targets",             "PCAWG",
        # "Driver Associations",    "PCAWG"
    )
    
    output$module_selection_ui <- shiny::renderUI({
        choices <- dataset_to_module_tbl %>% 
            dplyr::pull(module) %>% 
            unique()
        shiny::checkboxGroupInput(
            ns("module_choices"),
            "Select modules:",
            choices
        )
    })
    
    output$dataset_selection_ui <- shiny::renderUI({
        if(input$select_by_module & !is.null(input$module_choices)){
            choices <- dataset_to_module_tbl %>% 
                dplyr::filter(module %in% input$module_choices) %>% 
                dplyr::group_by(dataset) %>% 
                dplyr::summarise(count = dplyr::n()) %>% 
                dplyr::filter(count == length(input$module_choices)) %>% 
                dplyr::pull(dataset)
        } else {
            choices <- dataset_to_module_tbl %>% 
                dplyr::pull(dataset) %>% 
                unique()
        }
        shiny::selectInput(
            inputId = ns("dataset_choice"),
            label = strong("Select or Search for Dataset"),
            choices = choices,
            selected = default_dataset
        )
    })
    
    available_modules <- shiny::reactive({
        shiny::req(input$dataset_choice)
        dataset_to_module_tbl %>% 
            dplyr::filter(dataset == input$dataset_choice) %>% 
            dplyr::pull(module)
    })
    
    output$module_availibility_string <- shiny::renderText({
        shiny::req(input$dataset_choice, available_modules())
        available_modules() %>% 
            stringr::str_c(collapse = ", ") %>% 
            stringr::str_c(
                "Modules available for dataset :", 
                input$dataset_choice,
                "are",
                .,
                sep = " "
            )
    })
    
    return(shiny::reactive(input$dataset_choice))
}