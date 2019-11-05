library(testthat)
source("../functions/transform_new.R")
source("../functions/utils_new.R")
source("../functions/create_db2.R")

setwd("../")
PANIMMUNE_DB   <- create_db2()
setwd("tests")

# cell fractions module -------------------------------------------------------

test_that("build_cell_fractions_barplot_tbl",{
    feature_con <-  PANIMMUNE_DB %>% 
        dplyr::tbl("features") %>% 
        dplyr::filter(class == "Immune Cell Proportion - Original")
    
    value_con <- PANIMMUNE_DB %>% 
        dplyr::tbl("feature_values_long") %>% 
        dplyr::select(sample, group = Study, feature, value) 
    
    result_tbl <- build_cell_fractions_barplot_tbl(feature_con, value_con)
    expect_that(
        colnames(result_tbl),
        is_identical_to(c("x", "y", "color", "label", "error"))
    )
})

# cell proportions module -----------------------------------------------------

test_that("build_cell_proportion_scatterplot_tbl",{
    feature_con <-  PANIMMUNE_DB %>% 
        dplyr::tbl("features") %>% 
        dplyr::filter(class == "Overall Proportion") %>% 
        dplyr::filter(feature != "til_percentage")
    
    value_con <- PANIMMUNE_DB %>% 
        dplyr::tbl("feature_values_long") %>% 
        dplyr::select(sample, group = Study, feature, value) %>% 
        dplyr::inner_join(feature_con) 
    
    result_tbl <- build_cell_proportion_scatterplot_tbl(value_con)
    expect_that(
        colnames(result_tbl),
        is_identical_to(c("x", "y", "label"))
    )
})

# test_that("build_cell_proportion_con",{
#     feature_con <- PANIMMUNE_DB %>% 
#         dplyr::tbl("features") %>% 
#         dplyr::filter(class == "Overall Proportion") %>% 
#         dplyr::filter(feature != "til_percentage")
#     
#     value_con <-  PANIMMUNE_DB %>% 
#         dplyr::tbl("feature_values_long") %>% 
#         dplyr::select(sample, feature, group = Study, value)
#     
#     result_tbl <- build_cell_proportion_tbl(feature_con, value_con)
#     expect_that(
#         colnames(result_tbl),
#         is_identical_to(c("label", "color", "x", "y", "error"))
#     )
#     
# })


# volcano plot module ---------------------------------------------------------

test_that("create_volcano_drilldown_plot_title",{
    volcano_con <- PANIMMUNE_DB %>%
        dplyr::tbl("driver_results") %>%
        dplyr::filter(feature == "leukocyte_fraction", parent_group == "Study")

    expect_that(
        create_volcano_drilldown_plot_title(volcano_con, "RQCD1 P131L;SKCM"),
        is_identical_to("Cohort: RQCD1 P131L;SKCM ; P =  0.0321 ; Mut/Wt : 1.9414")
    )
    expect_that(
        create_volcano_drilldown_plot_title(volcano_con, "TP53 Q136*;HNSC"),
        is_identical_to("Cohort: TP53 Q136*;HNSC ; P =  0.7733 ; Wt/Mut : 1.1024")
    )
})







# distribution plot functions -------------------------------------------------

test_that("build_distribution_plot_df",{
    df1 <- tibble::tribble(
        ~x,  ~y,   ~y2,  ~y3, ~label,
        "A", 1000, 1,    999, "L1",
        "B", 100,  0.1,  99,  "L2",
        "C", 10,   0.01, 9,   "L3",
        "D", NA,   0,    0,   "L4"
    )
    res_df1 <- tibble::tribble(
        ~x,  ~y,   ~label,  
        "A", 1000, "L1",
        "B", 100,  "L2",
        "C", 10,   "L3"
    )
    res_df2 <- tibble::tribble(
        ~x,  ~y, ~label,  
        "A", 3,  "L1",
        "B", 2,  "L2",
        "C", 1,  "L3"
    )
    res_df3 <- tibble::tribble(
        ~x,  ~y,  ~label,  
        "A",  0,  "L1",
        "B", -1,  "L2",
        "C", -2,  "L3"
    )
    res_df4 <- tibble::tribble(
        ~x,  ~y, ~label,  
        "A", 3,  "L1",
        "B", 2,  "L2",
        "C", 1,  "L3",
        "D", 0,  "L4"
    )
    expect_that(
        build_distribution_plot_df(df1),
        is_identical_to(res_df1)
    )
    expect_that(
        build_distribution_plot_df(df1, scale_func_choice = "Log10"),
        is_identical_to(res_df2)
    )
    expect_that(
        build_distribution_plot_df(df1, "y2", scale_func_choice = "Log10"),
        is_identical_to(res_df3)
    )
    expect_that(
        build_distribution_plot_df(df1, "y3", scale_func_choice = "Log10 + 1"),
        is_identical_to(res_df4)
    )

})
