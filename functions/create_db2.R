create_db2 <- function(){
    
    con  <- DBI::dbConnect(RSQLite::SQLite(), dbname = ":memory:")
    
    files <- list.files("data2", full.names = T)
    
    features <- feather::read_feather("data2/features.feather")
    dplyr::copy_to(con, features, "features", temporary = FALSE)
    
    feature_values_long <- feather::read_feather("data2/feature_values_long.feather")
    dplyr::copy_to(con, feature_values_long, "feature_values_long", temporary = FALSE)
    
    groups <- feather::read_feather("data2/groups.feather")
    dplyr::copy_to(con, groups, "groups", temporary = FALSE)
    
    # til_image_links <- feather::read_feather("data2/til_image_links.feather")
    # dplyr::copy_to(con, til_image_links, "til_image_links", temporary = FALSE)
    # 
    # immunomodulator_expr <- feather::read_feather("data2/immunomodulator_expr.feather")
    # dplyr::copy_to(con, immunomodulator_expr, "immunomodulator_expr", temporary = FALSE)
    # 
    # immunomodulators <- feather::read_feather("data2/immunomodulators.feather")
    # dplyr::copy_to(con, immunomodulators, "immunomodulators", temporary = FALSE)
    # 
    # 
    # io_targets <- feather::read_feather("data2/io_targets.feather")
    # dplyr::copy_to(con, io_targets, "io_targets", temporary = FALSE)
    # 
    # files %>%
    #     purrr::keep(stringr::str_detect(., "io_target_expr")) %>%
    #     purrr::map(feather::read_feather) %>%
    #     dplyr::bind_rows() %>%
    #     dplyr::copy_to(con, ., "io_target_expr", temporary = FALSE)
    # 
    # files %>%
    #     purrr::keep(stringr::str_detect(., "driver_mutations")) %>%
    #     purrr::map(feather::read_feather) %>%
    #     dplyr::bind_rows() %>%
    #     dplyr::copy_to(con, ., "driver_mutations", temporary = FALSE)
    # 
    # files %>%
    #     purrr::keep(stringr::str_detect(., "driver_results")) %>%
    #     purrr::map(feather::read_feather) %>%
    #     dplyr::bind_rows() %>%
    #     dplyr::copy_to(con, ., "driver_results", temporary = FALSE)

    
    return(con)

}

