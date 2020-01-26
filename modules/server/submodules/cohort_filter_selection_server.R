cohort_filter_selection_server <- function(
    input, 
    output, 
    session,
    feature_named_list,
    selected_dataset,
    sample_ids
){
    ns <- session$ns
    
    source("modules/server/submodules/insert_remove_element_server.R", local = T)
    source("modules/ui/submodules/elements_ui.R", local = T)
    source("modules/server/submodules/elements_server.R", local = T)
    source("functions/cohort_filter_selection_functions.R", local = T)
    
    dataset_to_group_tbl <- dplyr::tribble(
        ~group,                 ~dataset, ~type,
        "Immune Subtype",       "TCGA",   "tag",
        "TCGA Subtype",         "TCGA",   "tag",
        "TCGA Study",           "TCGA",   "tag",
        # "Gender",          "TCGA",   "sample",
        # "Race",            "TCGA",   "sample",
        # "Ethnicity",       "TCGA",   "sample",
        # "Immune Subtype",  "PCAWG",  "tag",
        # "PCAWG Study",     "PCAWG",  "tag",
        # "Gender",          "PCAWG",  "sample",
        # "Race",            "PCAWG",  "sample"
    )
    
    group_named_list <- shiny::reactive({
        shiny::req(selected_dataset())
        dataset_to_group_tbl %>% 
            dplyr::filter(dataset == selected_dataset()) %>% 
            dplyr::pull(group)
        "SELECT display, id FROM tags" %>% 
            .GlobalEnv$perform_query("Get tags") %>% 
            dplyr::inner_join(
                dataset_to_group_tbl, 
                by = c("display" = "group")
            ) %>% 
            dplyr::select(display, id) %>% 
            tibble::deframe()
    })
    
    group_element_module_server <- shiny::reactive({
        shiny::req(group_named_list())
        purrr::partial(
            group_filter_element_server,
            group_named_list = group_named_list
        )
    })

    group_element_module_ui <- shiny::reactive(group_filter_element_ui)

    group_filter_output <- shiny::callModule(
        insert_remove_element_server,
        "group_filter",
        element_module = group_element_module_server,
        element_module_ui = group_element_module_ui,
        remove_ui_event = shiny::reactive(selected_dataset())
    )
    
    numeric_element_module_server <- shiny::reactive({
        shiny::req(feature_named_list())
        
        purrr::partial(
            numeric_filter_element_server,
            feature_named_list = feature_named_list
        )
        
    })
    
    numeric_element_module_ui <- shiny::reactive(numeric_filter_element_ui)
    
    numeric_filter_output <- shiny::callModule(
        insert_remove_element_server,
        "numeric_filter",
        element_module = numeric_element_module_server,
        element_module_ui = numeric_element_module_ui,
        remove_ui_event = shiny::reactive(selected_dataset())
    )
    
    numeric_filter_samples <- shiny::reactive({
        shiny::req(sample_ids(), numeric_filter_output())
        samples <- sample_ids()
        numeric_filters <- numeric_filter_output() %>% 
            shiny::reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null)) 
        for(item in numeric_filters){
            shiny::req(
                item$feature_choice,
                item$feature_range[[1]],
                item$feature_range[[2]]
            )
            sample_ids <- get_numeric_filter_samples(
                item$feature_choice,
                item$feature_range[[1]],
                item$feature_range[[2]]
            )
            samples <- intersect(samples, sample_ids)
        }
        return(samples)
    })
    
    group_filter_samples <- shiny::reactive({
        shiny::req(sample_ids(), group_filter_output())
        group_filters <- group_filter_output() %>% 
            shiny::reactiveValuesToList() %>% 
            purrr::discard(purrr::map_lgl(., is.null))
        samples <- sample_ids()
        for(item in group_filters){
            shiny::req(item$group_choice_ids)
            sample_ids <- get_group_filter_samples(item$group_choice_ids)
            samples <- intersect(samples, sample_ids)
        }
        return(samples)
    })
    
    selected_samples <- shiny::reactive({
        shiny::req(numeric_filter_samples(), group_filter_samples())
        intersect(numeric_filter_samples(), group_filter_samples())
    })
    
    output$samples_text <- shiny::renderText({
        c("Number of current samples:", length(selected_samples()))
    })
    
    return(selected_samples)
}