# library(testthat)
# library(dplyr)
# source("../functions/utils.R")
# context("utils.R")
# 
# 
# testthat::test_that("assert_df_has_columns", {
#     test_df1 <- tibble("col1" = c("value1", "value2"),
#                        "col2" = c("A", "B"),
#                        "col3" = c("C", "C"))
#     testthat::expect_that(
#         assert_df_has_columns(test_df1, c("col1", "col2")), 
#         testthat::is_identical_to(NULL))
#     testthat::expect_that(
#         assert_df_has_columns(test_df1, c("cola", "col2")),
#         testthat::throws_error("df has missing columns: cola" ))
#     testthat::expect_that(
#         assert_df_has_columns(test_df1, c("cola", "colb")),
#         testthat::throws_error("df has missing columns: cola, colb" ))
# })
# 
# testthat::test_that("assert_df_has_rows", {
#     test_df1 <- tibble("col1" = c("value1", "value2"),
#                        "col2" = c("A", "B"),
#                        "col3" = c("C", "C"))
#     test_df2 <- test_df1 %>% 
#         filter(col1 == "value3")
#     testthat::expect_that(
#         assert_df_has_rows(test_df1),
#         testthat::is_identical_to(NULL))
#     testthat::expect_that(
#         assert_df_has_rows(test_df2),
#         testthat::throws_error("result df is empty"))
# })
# 
# testthat::test_that("convert_values", {
#     test_df1 <- tibble("col1" = c("value1", "value2"),
#                        "col2" = c("A", "B"),
#                        "col3" = c("C", "C"))
#     testthat::expect_that(
#         convert_values("value1", test_df1, "col1", "col2"), 
#         testthat::is_identical_to("A"))
#     testthat::expect_that(
#         convert_values("value2", test_df1, "col1", "col2"), 
#         testthat::is_identical_to("B"))
#     testthat::expect_that(
#         convert_values("value1", test_df1, "col1", "col3"), 
#         testthat::is_identical_to("C"))
#     testthat::expect_that(
#         convert_values("C", test_df1, "col3", "col1"), 
#         testthat::is_identical_to(c("value1", "value2")))
#     testthat::expect_that(
#         convert_values("value3", test_df1, "col1", "col2"), 
#         testthat::is_identical_to(vector(mode = "character", length = 0)))
#     testthat::expect_that(
#         convert_values("value3", test_df1, "cola", "col2"), 
#         testthat::throws_error("df has missing columns: cola" ))
#     testthat::expect_that(
#         convert_values("value3", test_df1, "col1", "cola"), 
#         testthat::throws_error("df has missing columns: cola" ))
# })
# 
# testthat::test_that("convert_value_between_columns", {
#     test_df1 <- tibble("col1" = c("value1", "value2"),
#                        "col2" = c("A", "B"),
#                        "col3" = c("C", "C"))
#     testthat::expect_that(
#         convert_value_between_columns("value1", test_df1, "col1", "col2"), 
#         testthat::is_identical_to("A"))
#     
#     testthat::expect_that(
#         convert_value_between_columns("C", test_df1, "col3", "col1"),
#         testthat::throws_error("input value: C, has multiple matches: value1, value2"))
#     testthat::expect_that(
#         convert_value_between_columns("C", test_df1, "col3", "col1", many_matches = "return_result"),
#         testthat::is_identical_to(c("value1", "value2")))
#     
#     testthat::expect_that(
#         convert_value_between_columns("value3", test_df1, "col1", "col2"),
#         testthat::throws_error("input value has no matches: value3"))
#     testthat::expect_that(
#         convert_value_between_columns("value3", test_df1, "col1", "col2", no_matches = "return_input"),
#         testthat::is_identical_to("value3"))
#     testthat::expect_that(
#         convert_value_between_columns("value3", test_df1, "col1", "col2", no_matches = "return_na"),
#         testthat::is_identical_to(NA))
# })
# 
# testthat::test_that("convert_values_between_columns", {
#     test_df1 <- tibble("col1" = c("value1", "value2"),
#                        "col2" = c("A", "B"),
#                        "col3" = c("C", NA),
#                        "col4" = c(NA, NA))
#     testthat::expect_that(
#         convert_values_between_columns("value1", test_df1, "col1", "col2"), 
#         testthat::is_identical_to("A"))
#     testthat::expect_that(
#         convert_values_between_columns(c("value1", "value2"), test_df1, "col1", "col2"), 
#         testthat::is_identical_to(c("A", "B")))
#     testthat::expect_that(
#         convert_values_between_columns(c("value1", "value2"), test_df1, "col1", "col3"), 
#         testthat::is_identical_to(c("C")))
#     testthat::expect_that(
#         convert_values_between_columns(c("value1", "value2"), test_df1, "col1", "col4"), 
#         testthat::is_identical_to(vector(mode = "logical", length = 0)))
# })
# 
# testthat::test_that("get_complete_df_by_columns",{
#     test_df <- tibble(
#         "col1" = c("val1", "val2", "val3"),
#         "col2" = c(NA, 1, 2),
#         "col3" = c(NA, NA, NA))
#     testthat::expect_that(
#         get_complete_df_by_columns(test_df, c("col1")),
#         testthat::is_identical_to(tibble(
#             "col1" = c("val1", "val2", "val3"))))
#     testthat::expect_that(
#         get_complete_df_by_columns(test_df, c("col1", "col2")),
#         testthat::is_identical_to(tibble(
#             "col1" = c("val2", "val3"),
#             "col2" = c(1, 2))))
#     testthat::expect_that(
#         get_complete_df_by_columns(test_df, c("col1", "col4")),
#         testthat::throws_error("df has missing columns: col4"))
#     testthat::expect_that(
#         get_complete_df_by_columns(test_df, c("col1", "col3")),
#         testthat::throws_error("result df is empty"))
# })
# 
# testthat::test_that("get_complete_class_df", {
#     test_df <- tibble(
#         "class col" = c("class1", "class1", "class1", "class2", "class2", "class2"),
#         "variable col" = c("var1", "var2", "var3", "var4", "var5", "var6"),
#         "order col" = c(1,2,3,3,2,1))
#     result_df1 <- tibble(
#         "variable col" = c("var1", "var2", "var3"),
#         "order col" = c(1,2,3))
#     result_df2 <- tibble(
#         "variable col" = c("var4", "var5", "var6"),
#         "order col" = c(3,2,1))
#     testthat::expect_that(
#         get_complete_class_df("class1", test_df, "class col", "variable col", "order col"),
#         testthat::is_identical_to(result_df1))
#     testthat::expect_that(
#         get_complete_class_df("class2", test_df, "class col", "variable col", "order col"),
#         testthat::is_identical_to(result_df2))
#     testthat::expect_that(
#         get_complete_class_df("class2", test_df, "class col2", "variable col", "order col"),
#         testthat::throws_error("df has missing columns: class col2"))
#     testthat::expect_that(
#         get_complete_class_df("class4", test_df, "class col", "variable col", "order col"),
#         testthat::throws_error("result df is empty"))
# })
# 
# testthat::test_that("factor_variables_with_df", {
#     test_df1 <- tibble(
#         "variable_col" = c("var1", "var2", "var3"),
#         "order_col" = c(1,2,3))
#     test_df2 <- tibble(
#         "variable_col" = c("var4", "var5", "var6"),
#         "order_col" = c(3,2,1))
#     testthat::expect_that(
#         factor_variables_with_df(test_df1, "variable_col", "order_col"),
#         testthat::is_identical_to(factor(c("var1", "var2", "var3"),
#                                          levels = c("var1", "var2", "var3"))))
#     testthat::expect_that(
#         factor_variables_with_df(test_df2, "variable_col", "order_col"),
#         testthat::is_identical_to(factor(c("var6", "var5", "var4"),
#                                          levels = c("var6", "var5", "var4"))))
# })
# 
# testthat::test_that("get_factored_variables_by_class", {
#     test_df <- tibble(
#         "class col" = c("class1", "class1", "class1", "class2", "class2", "class2"),
#         "variable col" = c("var1", "var2", "var3", "var4", "var5", "var6"),
#         "order col" = c(1,2,3,3,2,1))
#     testthat::expect_that(
#         get_factored_variables_by_class(
#             "class1", test_df, "class col", "variable col", "order col"),
#         testthat::is_identical_to(
#             factor(c("var1", "var2", "var3"), levels = c("var1", "var2", "var3"))))
#     testthat::expect_that(
#         get_factored_variables_by_class(
#             "class2", test_df, "class col", "variable col", "order col"),
#         testthat::is_identical_to(
#             factor(c("var6", "var5", "var4"),  levels = c("var6", "var5", "var4"))))
#     testthat::expect_that(
#         get_factored_variables_by_class(
#             "class3", test_df, "class col", "variable col", "order col"),
#         testthat::throws_error("result df is empty"))
# })
# 
# testthat::test_that("df_to_nested_list", {
#     test_df1 <- tibble(
#         "class_col" = c("class1", "class1", "class1", "class2", "class2", "class3"),
#         "internal_col" = c("name1", "name2", "name3", "name4", "name5", "name6"),
#         "display_col" = c("value1", "value2", "value3", "value4", "value5", "value6"))
#     testthat::expect_that(
#         df_to_nested_list(test_df1, "class_col", "internal_col", "display_col"),
#         testthat::is_identical_to(list(
#             "class1" = c("value1" = "name1",
#                          "value2" = "name2",
#                          "value3" = "name3"),
#             "class2" = c("value4" = "name4",
#                          "value5" = "name5"),
#             "class3" = c("value6" = "name6"))))
# })
# 
# testthat::test_that("get_column_names_of_type", {
#     test_df <- tibble(
#         "char_col" = c("class1"),
#         "int_col" = c(1L),
#         "double_col" = c(1.1),
#         "log_col" = c(T))
#     testthat::expect_that(
#         get_column_names_of_type(test_df, purrr::is_integer),
#         testthat::is_identical_to(c("int_col")))
#     testthat::expect_that(
#         get_column_names_of_type(test_df, purrr::is_double),
#         testthat::is_identical_to(c("double_col")))
#     testthat::expect_that(
#         get_column_names_of_type(test_df, purrr::is_numeric),
#         testthat::is_identical_to(c("int_col", "double_col")))
#     testthat::expect_that(
#         get_column_names_of_type(test_df, is.factor),
#         testthat::throws_error("df has no columns from selection function"))
# })
# 
# testthat::test_that("create_nested_list_by_class", {
#     test_df <- tibble(
#         "class_col" = c(
#             "class1", "class1", "class1", "class2", "class2", "class3",
#             "class4", "class4", "class4", NA),
#         "internal_col" = c(
#             "name1", "name2", "name3", "name4", "name5", "name6",
#             "name7", "name8", "name9", "name10"),
#         "display_col" = c(
#             "value1", "value2", "value3", "value4", "value5", "value6",
#             "value7", "value8", "value9", "value10"))
#     testthat::expect_that(
#         create_nested_list_by_class(
#             test_df, 
#             class_column = "class_col",
#             display_column = "display_col",
#             internal_column = "internal_col"
#         ),
#         testthat::is_identical_to(list(
#             "class1" = c("value1" = "name1",
#                          "value2" = "name2",
#                          "value3" = "name3"),
#             "class2" = c("value4" = "name4",
#                          "value5" = "name5"),
#             "class3" = c("value6" = "name6"),
#             "class4" = c("value7" = "name7",
#                          "value8" = "name8",
#                          "value9" = "name9"),
#             "Other" = c("value10" = "name10"))))
#     testthat::expect_that(
#         create_nested_list_by_class(
#             test_df, 
#             class_col != "class1",
#             class_column = "class_col",
#             display_column = "display_col",
#             internal_column = "internal_col"
#         ),
#         testthat::is_identical_to(list(
#             "class2" = c("value4" = "name4",
#                          "value5" = "name5"),
#             "class3" = c("value6" = "name6"),
#             "class4" = c("value7" = "name7",
#                          "value8" = "name8",
#                          "value9" = "name9"))))
# })
# 
# testthat::test_that("create_filtered_nested_list_by_class", {
#     test_df <- tibble(
#         "CLASS" = c(
#             "class1", "class1", "class1", "class2", "class2", "class3",
#             "class4", "class4", "class4", NA),
#         "INTERNAL" = c(
#             "name1", "name2", "name3", "name4", "name5", "name6",
#             "name7", "name8", "name9", "name10"),
#         "DISPLAY" = c(
#             "value1", "value2", "value3", "value4", "value5", "value6",
#             "value7", "value8", "value9", "value10"),
#         "FILTER" = c(rep("numeric", 5), rep("categorical", 5))
#     )
#     testthat::expect_that(
#         create_filtered_nested_list_by_class(test_df, FILTER == "numeric"),
#         testthat::is_identical_to(list(
#             "class1" = c("value1" = "name1",
#                          "value2" = "name2",
#                          "value3" = "name3"),
#             "class2" = c("value4" = "name4",
#                          "value5" = "name5"))))
# })
# 
# testthat::test_that("get_unique_column_values", {
#     test_df1 <- tibble("col" = c("value1", "value2"))
#     test_df2 <- tibble("col" = c("value1", "value1"))
#     test_df3 <- tibble("col" = c("value1", NA))
#     test_df4 <- tibble("col" = c(5, 6))
#     test_df5 <- tibble("col" = c("value2", "value1"))
#     testthat::expect_that(
#         get_unique_column_values("col", test_df1),
#         testthat::is_identical_to(c("value1", "value2")))
#     testthat::expect_that(
#         get_unique_column_values("col", test_df2),
#         testthat::is_identical_to(c("value1")))
#     testthat::expect_that(
#         get_unique_column_values("col", test_df3),
#         testthat::is_identical_to(c("value1")))
#     testthat::expect_that(
#         get_unique_column_values("col", test_df4),
#         testthat::is_identical_to(c("5", "6")))
#     testthat::expect_that(
#         get_unique_column_values("col", test_df5),
#         testthat::is_identical_to(c("value1", "value2")))
# })
# 
# testthat::test_that("get_variable_classes", {
#     test_df <- tibble(
#         "class" = c("class1", "class1", "class1", "class2", "class2", "class3",
#                     "class4", "class4", "class4"),
#         "type" = c("Numeric", "Numeric", "Numeric", "Factor", "Factor", 
#                    "Numeric", "Logical", "Logical", "Logical"))
#     testthat::expect_that(
#         get_variable_classes(
#             test_df, 
#             type == "Numeric",
#             class_column = "class" 
#         ),
#         testthat::is_identical_to(c("class1", "class3")))
# })


# # cell fractions module -------------------------------------------------------
# 
# test_that("build_cell_fractions_barplot_tbl",{
#     feature_con <-  PANIMMUNE_DB %>% 
#         dplyr::tbl("features") %>% 
#         dplyr::filter(class == "Immune Cell Proportion - Original")
#     
#     value_con <- PANIMMUNE_DB %>% 
#         dplyr::tbl("feature_values_long") %>% 
#         dplyr::select(sample, group = Study, feature, value) 
#     
#     result_tbl <- build_cell_fractions_barplot_tbl(feature_con, value_con)
#     expect_that(
#         colnames(result_tbl),
#         is_identical_to(c("x", "y", "color", "label", "error"))
#     )
# })
# 
# # cell proportions module -----------------------------------------------------
# 
# test_that("build_cell_proportion_scatterplot_tbl",{
#     feature_con <-  PANIMMUNE_DB %>% 
#         dplyr::tbl("features") %>% 
#         dplyr::filter(class == "Overall Proportion") %>% 
#         dplyr::filter(feature != "til_percentage")
#     
#     value_con <- PANIMMUNE_DB %>% 
#         dplyr::tbl("feature_values_long") %>% 
#         dplyr::select(sample, group = Study, feature, value) %>% 
#         dplyr::inner_join(feature_con) 
#     
#     result_tbl <- build_cell_proportion_scatterplot_tbl(value_con)
#     expect_that(
#         colnames(result_tbl),
#         is_identical_to(c("x", "y", "label"))
#     )
# })
# 
# # test_that("build_cell_proportion_con",{
# #     feature_con <- PANIMMUNE_DB %>% 
# #         dplyr::tbl("features") %>% 
# #         dplyr::filter(class == "Overall Proportion") %>% 
# #         dplyr::filter(feature != "til_percentage")
# #     
# #     value_con <-  PANIMMUNE_DB %>% 
# #         dplyr::tbl("feature_values_long") %>% 
# #         dplyr::select(sample, feature, group = Study, value)
# #     
# #     result_tbl <- build_cell_proportion_tbl(feature_con, value_con)
# #     expect_that(
# #         colnames(result_tbl),
# #         is_identical_to(c("label", "color", "x", "y", "error"))
# #     )
# #     
# # })
# 
# 
# # volcano plot module ---------------------------------------------------------
# 
# test_that("create_volcano_drilldown_plot_title",{
#     volcano_con <- PANIMMUNE_DB %>%
#         dplyr::tbl("driver_results") %>%
#         dplyr::filter(feature == "leukocyte_fraction", parent_group == "Study")
# 
#     expect_that(
#         create_volcano_drilldown_plot_title(volcano_con, "RQCD1 P131L;SKCM"),
#         is_identical_to("Cohort: RQCD1 P131L;SKCM ; P =  0.0321 ; Mut/Wt : 1.9414")
#     )
#     expect_that(
#         create_volcano_drilldown_plot_title(volcano_con, "TP53 Q136*;HNSC"),
#         is_identical_to("Cohort: TP53 Q136*;HNSC ; P =  0.7733 ; Wt/Mut : 1.1024")
#     )
# })
# 
# 
# 
# 
# 
# 
# 
# # distribution plot functions -------------------------------------------------
# 
# test_that("build_distribution_plot_df",{
#     df1 <- tibble::tribble(
#         ~x,  ~y,   ~y2,  ~y3, ~label,
#         "A", 1000, 1,    999, "L1",
#         "B", 100,  0.1,  99,  "L2",
#         "C", 10,   0.01, 9,   "L3",
#         "D", NA,   0,    0,   "L4"
#     )
#     res_df1 <- tibble::tribble(
#         ~x,  ~y,   ~label,  
#         "A", 1000, "L1",
#         "B", 100,  "L2",
#         "C", 10,   "L3"
#     )
#     res_df2 <- tibble::tribble(
#         ~x,  ~y, ~label,  
#         "A", 3,  "L1",
#         "B", 2,  "L2",
#         "C", 1,  "L3"
#     )
#     res_df3 <- tibble::tribble(
#         ~x,  ~y,  ~label,  
#         "A",  0,  "L1",
#         "B", -1,  "L2",
#         "C", -2,  "L3"
#     )
#     res_df4 <- tibble::tribble(
#         ~x,  ~y, ~label,  
#         "A", 3,  "L1",
#         "B", 2,  "L2",
#         "C", 1,  "L3",
#         "D", 0,  "L4"
#     )
#     expect_that(
#         build_distribution_plot_df(df1),
#         is_identical_to(res_df1)
#     )
#     expect_that(
#         build_distribution_plot_df(df1, scale_func_choice = "Log10"),
#         is_identical_to(res_df2)
#     )
#     expect_that(
#         build_distribution_plot_df(df1, "y2", scale_func_choice = "Log10"),
#         is_identical_to(res_df3)
#     )
#     expect_that(
#         build_distribution_plot_df(df1, "y3", scale_func_choice = "Log10 + 1"),
#         is_identical_to(res_df4)
#     )
# 
# })


# 
# # distribution plot functions -------------------------------------------------
# 
# test_that("build_distribution_plot_df",{
#     df1 <- tibble::tribble(
#         ~x,  ~y,   ~y2,  ~y3, ~label,
#         "A", 1000, 1,    999, "L1",
#         "B", 100,  0.1,  99,  "L2",
#         "C", 10,   0.01, 9,   "L3",
#         "D", NA,   0,    0,   "L4"
#     )
#     res_df1 <- tibble::tribble(
#         ~x,  ~y,   ~label,  
#         "A", 1000, "L1",
#         "B", 100,  "L2",
#         "C", 10,   "L3"
#     )
#     res_df2 <- tibble::tribble(
#         ~x,  ~y, ~label,  
#         "A", 3,  "L1",
#         "B", 2,  "L2",
#         "C", 1,  "L3"
#     )
#     res_df3 <- tibble::tribble(
#         ~x,  ~y,  ~label,  
#         "A",  0,  "L1",
#         "B", -1,  "L2",
#         "C", -2,  "L3"
#     )
#     res_df4 <- tibble::tribble(
#         ~x,  ~y, ~label,  
#         "A", 3,  "L1",
#         "B", 2,  "L2",
#         "C", 1,  "L3",
#         "D", 0,  "L4"
#     )
#     expect_that(
#         build_distribution_plot_df(df1),
#         is_identical_to(res_df1)
#     )
#     expect_that(
#         build_distribution_plot_df(df1, scale_func_choice = "Log10"),
#         is_identical_to(res_df2)
#     )
#     expect_that(
#         build_distribution_plot_df(df1, "y2", scale_func_choice = "Log10"),
#         is_identical_to(res_df3)
#     )
#     expect_that(
#         build_distribution_plot_df(df1, "y3", scale_func_choice = "Log10 + 1"),
#         is_identical_to(res_df4)
#     )
# 
# })
#                     
# 
# # samplegroup functions -------------------------------------------------------
# 
# testthat::test_that("build_sample_group_key_df",{
#     group_df <- tibble(
#         "group_col1" = c(rep("group1", 5), rep("group2", 3)),
#         "group_col2" = c(rep("group3", 3), rep("group4", 4), NA),
#         "group_col4" = c(rep("group5", 5), rep("group6", 3)),
#         "group_col5" = c(rep("group7", 5), rep("group8", 3))
#     )
#     feature_df <- tibble(
#         "FeatureValue" = c("group1", "group2", "group3", "group4", "group5"),
#         "FeatureName" = c("n1", "n2", "n3", "n4", "n5"),
#         "Characteristics" = c("c1", "c2", "c3", "c4", "c5"),
#         "FeatureHex" = c("red","blue","green","yellow", "orange")
#     )
#     result_df1 <- tibble(
#         "Sample Group" = c("group1", "group2"),
#         "Group Name" = c("n1", "n2"),
#         "Group Size" = c(5L, 3L),
#         "Characteristics" = c("c1", "c2"),
#         "Plot Color" = c("red", "blue")
#     )
#     result_df2 <- tibble(
#         "Sample Group" = c("group3", "group4"),
#         "Group Name" = c("n3", "n4"),
#         "Group Size" = c(3L, 4L),
#         "Characteristics" = c("c3", "c4"),
#         "Plot Color" = c("green", "yellow")
#     )
#     testthat::expect_that(
#         build_sample_group_key_df(group_df, "group_col1", feature_df),
#         testthat::is_identical_to(result_df1))
#     testthat::expect_that(
#         build_sample_group_key_df(group_df, "group_col2", feature_df),
#         testthat::is_identical_to(result_df2))
#     testthat::expect_that(
#         build_sample_group_key_df(group_df, "group_col3", feature_df),
#         testthat::throws_error("df has missing columns: group_col3"))
# })
# 
# testthat::test_that("build_group_size_df",{
#     test_subset_df <- tibble(
#         "group_col1" = c(rep("group1", 5), rep("group2", 3)),
#         "group_col2" = c(rep("group3", 3), rep("group4", 4), NA))
#     result_size_df1 <- tibble(
#         "Group" = c("group1", "group2"),
#         "Group_Size" = c(5L, 3L))
#     result_size_df2 <- tibble(
#         "Group" = c("group3", "group4"),
#         "Group_Size" = c(3L, 4L))
#     testthat::expect_that(
#         build_group_size_df(test_subset_df, "group_col1"),
#         testthat::is_identical_to(result_size_df1))
#     testthat::expect_that(
#         build_group_size_df(test_subset_df, "group_col2"),
#         testthat::is_identical_to(result_size_df2))
# })
# 
# # functions for making plot df label column -----------------------------------
# 
# 
# testthat::test_that("create_label", {
#     input_df <- tibble(
#         "name" = c("id1", "id2", "id3", "id4"),
#         "group" = c("group1", "group1", "group2", "group2"),
#         "value_col1" = c(1, 2, 3, 4),
#         "value_col2" = c(8, 9, 10, 11),
#         "value_col3" = c(12, 14, 100, 110))
#     result_df1 <- tibble(
#         "name" = c("id1", "id2", "id3", "id4"),
#         "group" = c("group1", "group1", "group2", "group2"),
#         "value_col3" = c(12, 14, 100, 110),
#         "label" = c(
#             "<b>ParticipantBarcode:</b> id1 (group1)</br></br>VALUE_COL1: 1.000</br>VALUE_COL2: 8.000",
#             "<b>ParticipantBarcode:</b> id2 (group1)</br></br>VALUE_COL1: 2.000</br>VALUE_COL2: 9.000",
#             "<b>ParticipantBarcode:</b> id3 (group2)</br></br>VALUE_COL1: 3.000</br>VALUE_COL2: 10.000",
#             "<b>ParticipantBarcode:</b> id4 (group2)</br></br>VALUE_COL1: 4.000</br>VALUE_COL2: 11.000"),
#         "value_col1" = c(1, 2, 3, 4),
#         "value_col2" = c(8, 9, 10, 11))
#     result_df2 <- tibble(
#         "name" = c("id1", "id2", "id3", "id4"),
#         "group" = c("group1", "group1", "group2", "group2"),
#         "label" = c(
#             "<b>ParticipantBarcode:</b> id1 (group1)</br></br>VALUE_COL1: 1.000</br>VALUE_COL2: 8.000</br>VALUE_COL3: 12.000",
#             "<b>ParticipantBarcode:</b> id2 (group1)</br></br>VALUE_COL1: 2.000</br>VALUE_COL2: 9.000</br>VALUE_COL3: 14.000",
#             "<b>ParticipantBarcode:</b> id3 (group2)</br></br>VALUE_COL1: 3.000</br>VALUE_COL2: 10.000</br>VALUE_COL3: 100.000",
#             "<b>ParticipantBarcode:</b> id4 (group2)</br></br>VALUE_COL1: 4.000</br>VALUE_COL2: 11.000</br>VALUE_COL3: 110.000"),
#         "value_col1" = c(1, 2, 3, 4),
#         "value_col2" = c(8, 9, 10, 11),
#         "value_col3" = c(12, 14, 100, 110))
#     testthat::expect_that(
#         create_label(input_df, c("value_col1", "value_col2")),
#         testthat::is_identical_to(result_df1))
#     testthat::expect_that(
#         create_label(input_df, c("value_col1", "value_col2", "value_col3")),
#         testthat::is_identical_to(result_df2))
# })
# 
# # other functions -------------------------------------------------------------
# 
# testthat::test_that("summarise_df_at_column",{
#     test_df1 <- tibble(
#         "group_col1" = c(rep("group1", 5)),
#         "group_col2" = c(rep("group1", 3), rep("group2", 2)),
#         "sum_col1" = c(rep(5, 5)))
#     result_df1 <- tibble(
#         "group_col1" = "group1",
#         "mean" = 5)
#     result_df2 <- tibble(
#         "group_col1" = "group1", 
#         "mean" = 5,
#         "sd" = 0)
#     result_df3 <- tibble(
#         "group_col2" = c("group1", "group2"),
#         "mean" = c(5, 5))
#     result_df4 <- tibble(
#         "group_col1" = c("group1", "group1"),
#         "group_col2" = c("group1", "group2"),
#         "mean" = c(5, 5))
#     # one group one function
#     testthat::expect_that(
#         summarise_df_at_column(test_df1, "sum_col1", "group_col1", "mean"),
#         testthat::is_identical_to(result_df1))
#     # two functions
#     testthat::expect_that(
#         summarise_df_at_column(test_df1, "sum_col1", "group_col1", c("mean", "sd")),
#         testthat::is_identical_to(result_df2))
#     # two groups
#     testthat::expect_that(
#         summarise_df_at_column(test_df1, "sum_col1", "group_col2", "mean"),
#         testthat::is_identical_to(result_df3))
#     # two grouping columns
#     testthat::expect_that(
#         summarise_df_at_column(test_df1, "sum_col1", c("group_col1", "group_col2"), "mean"),
#         testthat::is_identical_to(result_df4))
# })



##### deprecated tests --------------------------------------------------------


# immunomodulator functions ---------------------------------------------------

# testthat::test_that("build_immunomodulator_expression_df", {
#     expr_df <- tibble(
#         "ID" = c("a", "a", "b","b"),
#         "COUNT" = c(1, 10, 100, 100),
#         "GENE" = c("genea", "geneb", "genea", "geneb")
#     )
#     group_df <- tibble(
#         "ID" = c("a", "b", "c", "d"),
#         "GROUP" = c("group1", "group2", "group1", "group2")
#     )
#     testthat::expect_that(
#         build_immunomodulator_expression_df(
#             group_df, 
#             filter_value = "genea",
#             group_col = "GROUP",
#             expression_df = expr_df,
#             expression_filter_col = "GENE",
#             expression_col = "COUNT",
#             id_col = "ID"
#         ),
#         testthat::is_identical_to(tibble(
#             "GROUP" = c("group1", "group2"),
#             "LOG_COUNT" = c(log10(1+1), log10(100+1))
#         ))
#     )
# })

# testthat::test_that("filter_immunomodulator_expression_df", {
#     expr_df <- tibble(
#         "ID" = c("a", "a", "b","b"),
#         "COUNT" = c(1, 10, 100, 100),
#         "GENE" = c("genea", "geneb", "genea", "geneb")
#     )
#     testthat::expect_that(
#         filter_immunomodulator_expression_df(
#             expr_df, 
#             id_col = "ID",
#             filter_col = "GENE",
#             expression_col = "COUNT",
#             filter_value = "genea"
#         ),
#         testthat::is_identical_to(tibble(
#             "LOG_COUNT" = c(log10(1+1), log10(100+1)),
#             "ID" = c("a", "b")
#         ))
#     )
# })


# other ----

# testthat::test_that("build_plot_color_df",{
#     subset_df <- tibble(
#         "group_col1" = c(rep("group1", 5), rep("group2", 3)),
#         "group_col2" = c(rep("group2", 3), rep("group3", 4), NA))
#     color_vector = c(
#         "group1" = "red",
#         "group2" = "blue",
#         "group3" = "green")
#     result_color_df1 <- tibble(
#         "Group" = c("group1", "group2"),
#         "Plot_Color" = c("red", "blue"))
#     result_color_df2<- tibble(
#         "Group" = c("group2", "group3"),
#         "Plot_Color" = c("blue", "green"))
#     testthat::expect_that(
#         build_plot_color_df(color_vector, subset_df, "group_col1"),
#         testthat::is_identical_to(result_color_df1))
#     testthat::expect_that(
#         build_plot_color_df(color_vector, subset_df, "group_col2"),
#         testthat::is_identical_to(result_color_df2))
# })