library(yaml)
library(tidyverse)

config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$function_files, source)

testthat::test_that("assert_df_has_columns", {
    test_df1 <- data_frame("col1" = c("value1", "value2"),
                           "col2" = c("A", "B"),
                           "col3" = c("C", "C"))
    testthat::expect_that(
        assert_df_has_columns(test_df1, c("col1", "col2")), 
        testthat::is_identical_to(NULL))
    testthat::expect_that(
        assert_df_has_columns(test_df1, c("cola", "col2")),
        testthat::throws_error("df has missing columns: cola" ))
    testthat::expect_that(
        assert_df_has_columns(test_df1, c("cola", "colb")),
        testthat::throws_error("df has missing columns: cola, colb" ))
})

testthat::test_that("assert_df_has_rows", {
    test_df1 <- data_frame("col1" = c("value1", "value2"),
                           "col2" = c("A", "B"),
                           "col3" = c("C", "C"))
    test_df2 <- test_df1 %>% 
        filter(col1 == "value3")
    testthat::expect_that(
        assert_df_has_rows(test_df1),
        testthat::is_identical_to(NULL))
    testthat::expect_that(
        assert_df_has_rows(test_df2),
        testthat::throws_error("result df is empty"))
})

testthat::test_that("convert_values", {
    test_df1 <- data_frame("col1" = c("value1", "value2"),
                           "col2" = c("A", "B"),
                           "col3" = c("C", "C"))
    testthat::expect_that(
        convert_values("value1", test_df1, "col1", "col2"), 
        testthat::is_identical_to("A"))
    testthat::expect_that(
        convert_values("value2", test_df1, "col1", "col2"), 
        testthat::is_identical_to("B"))
    testthat::expect_that(
        convert_values("value1", test_df1, "col1", "col3"), 
        testthat::is_identical_to("C"))
    testthat::expect_that(
        convert_values("C", test_df1, "col3", "col1"), 
        testthat::is_identical_to(c("value1", "value2")))
    testthat::expect_that(
        convert_values("value3", test_df1, "col1", "col2"), 
        testthat::is_identical_to(vector(mode = "character", length = 0)))
    testthat::expect_that(
        convert_values("value3", test_df1, "cola", "col2"), 
        testthat::throws_error("df has missing columns: cola" ))
    testthat::expect_that(
        convert_values("value3", test_df1, "col1", "cola"), 
        testthat::throws_error("df has missing columns: cola" ))
})

testthat::test_that("convert_value_between_columns", {
    test_df1 <- data_frame("col1" = c("value1", "value2"),
                           "col2" = c("A", "B"),
                           "col3" = c("C", "C"))
    testthat::expect_that(
        convert_value_between_columns("value1", test_df1, "col1", "col2"), 
        testthat::is_identical_to("A"))
    
    testthat::expect_that(
        convert_value_between_columns("C", test_df1, "col3", "col1"),
        testthat::throws_error("input value: C, has multiple matches: value1, value2"))
    testthat::expect_that(
        convert_value_between_columns("C", test_df1, "col3", "col1", many_matches = "return_result"),
        testthat::is_identical_to(c("value1", "value2")))
    
    testthat::expect_that(
        convert_value_between_columns("value3", test_df1, "col1", "col2"),
        testthat::throws_error("input value has no matches: value3"))
    testthat::expect_that(
        convert_value_between_columns("value3", test_df1, "col1", "col2", no_matches = "return_input"),
        testthat::is_identical_to("value3"))
    testthat::expect_that(
        convert_value_between_columns("value3", test_df1, "col1", "col2", no_matches = "return_na"),
        testthat::is_identical_to(NA))
})

testthat::test_that("convert_values_between_columns", {
    test_df1 <- data_frame("col1" = c("value1", "value2"),
                           "col2" = c("A", "B"),
                           "col3" = c("C", NA),
                           "col4" = c(NA, NA))
    testthat::expect_that(
        convert_values_between_columns("value1", test_df1, "col1", "col2"), 
        testthat::is_identical_to("A"))
    testthat::expect_that(
        convert_values_between_columns(c("value1", "value2"), test_df1, "col1", "col2"), 
        testthat::is_identical_to(c("A", "B")))
    testthat::expect_that(
        convert_values_between_columns(c("value1", "value2"), test_df1, "col1", "col3"), 
        testthat::is_identical_to(c("C")))
    testthat::expect_that(
        convert_values_between_columns(c("value1", "value2"), test_df1, "col1", "col4"), 
        testthat::is_identical_to(vector(mode = "logical", length = 0)))
})

testthat::test_that("get_complete_df_by_columns",{
    test_df <- data_frame(
        "col1" = c("val1", "val2", "val3"),
        "col2" = c(NA, 1, 2),
        "col3" = c(NA, NA, NA))
    testthat::expect_that(
        get_complete_df_by_columns(test_df, c("col1")),
        testthat::is_identical_to(data_frame(
            "col1" = c("val1", "val2", "val3"))))
    testthat::expect_that(
        get_complete_df_by_columns(test_df, c("col1", "col2")),
        testthat::is_identical_to(data_frame(
            "col1" = c("val2", "val3"),
            "col2" = c(1, 2))))
    testthat::expect_that(
        get_complete_df_by_columns(test_df, c("col1", "col4")),
        testthat::throws_error("df has missing columns: col4"))
    testthat::expect_that(
        get_complete_df_by_columns(test_df, c("col1", "col3")),
        testthat::throws_error("result df is empty"))
})

testthat::test_that("get_complete_class_df", {
    test_df <- data_frame(
        "class col" = c("class1", "class1", "class1", "class2", "class2", "class2"),
        "variable col" = c("var1", "var2", "var3", "var4", "var5", "var6"),
        "order col" = c(1,2,3,3,2,1))
    result_df1 <- data_frame(
        "variable col" = c("var1", "var2", "var3"),
        "order col" = c(1,2,3))
    result_df2 <- data_frame(
        "variable col" = c("var4", "var5", "var6"),
        "order col" = c(3,2,1))
    testthat::expect_that(
        get_complete_class_df("class1", test_df, "class col", "variable col", "order col"),
        testthat::is_identical_to(result_df1))
    testthat::expect_that(
        get_complete_class_df("class2", test_df, "class col", "variable col", "order col"),
        testthat::is_identical_to(result_df2))
    testthat::expect_that(
        get_complete_class_df("class2", test_df, "class col2", "variable col", "order col"),
        testthat::throws_error("df has missing columns: class col2"))
    testthat::expect_that(
        get_complete_class_df("class4", test_df, "class col", "variable col", "order col"),
        testthat::throws_error("result df is empty"))
})

testthat::test_that("factor_variables_with_df", {
    test_df1 <- data_frame(
        "variable_col" = c("var1", "var2", "var3"),
        "order_col" = c(1,2,3))
    test_df2 <- data_frame(
        "variable_col" = c("var4", "var5", "var6"),
        "order_col" = c(3,2,1))
    testthat::expect_that(
        factor_variables_with_df(test_df1, "variable_col", "order_col"),
        testthat::is_identical_to(factor(c("var1", "var2", "var3"),
                               levels = c("var1", "var2", "var3"))))
    testthat::expect_that(
        factor_variables_with_df(test_df2, "variable_col", "order_col"),
        testthat::is_identical_to(factor(c("var6", "var5", "var4"),
                               levels = c("var6", "var5", "var4"))))
})

testthat::test_that("get_factored_variables_by_class", {
    test_df <- data_frame(
        "class col" = c("class1", "class1", "class1", "class2", "class2", "class2"),
        "variable col" = c("var1", "var2", "var3", "var4", "var5", "var6"),
        "order col" = c(1,2,3,3,2,1))
    testthat::expect_that(
        get_factored_variables_by_class(
            "class1", test_df, "class col", "variable col", "order col"),
        testthat::is_identical_to(
            factor(c("var1", "var2", "var3"), levels = c("var1", "var2", "var3"))))
    testthat::expect_that(
        get_factored_variables_by_class(
            "class2", test_df, "class col", "variable col", "order col"),
        testthat::is_identical_to(
            factor(c("var6", "var5", "var4"),  levels = c("var6", "var5", "var4"))))
    testthat::expect_that(
        get_factored_variables_by_class(
            "class3", test_df, "class col", "variable col", "order col"),
        testthat::throws_error("result df is empty"))
})

testthat::test_that("df_to_nested_list", {
    test_df1 <- data_frame(
        "class_col" = c("class1", "class1", "class1", "class2", "class2", "class3"),
        "internal_col" = c("name1", "name2", "name3", "name4", "name5", "name6"),
        "display_col" = c("value1", "value2", "value3", "value4", "value5", "value6"))
    testthat::expect_that(
        df_to_nested_list(test_df1, "class_col", "internal_col", "display_col"),
        testthat::is_identical_to(list(
            "class1" = c("value1" = "name1",
                         "value2" = "name2",
                         "value3" = "name3"),
            "class2" = c("value4" = "name4",
                         "value5" = "name5"),
            "class3" = c("value6" = "name6"))))
})

testthat::test_that("get_column_names_of_type", {
    test_df <- data_frame(
        "char_col" = c("class1"),
        "int_col" = c(1L),
        "double_col" = c(1.1),
        "log_col" = c(T))
    testthat::expect_that(
        get_column_names_of_type(test_df, purrr::is_integer),
        testthat::is_identical_to(c("int_col")))
    testthat::expect_that(
        get_column_names_of_type(test_df, purrr::is_double),
        testthat::is_identical_to(c("double_col")))
    testthat::expect_that(
        get_column_names_of_type(test_df, purrr::is_numeric),
        testthat::is_identical_to(c("int_col", "double_col")))
    testthat::expect_that(
        get_column_names_of_type(test_df, is.factor),
        testthat::throws_error("df has no columns from selection function"))
})

testthat::test_that("create_nested_list_by_class", {
    test_df <- data_frame(
        "class_col" = c(
            "class1", "class1", "class1", "class2", "class2", "class3",
            "class4", "class4", "class4", NA),
        "internal_col" = c(
            "name1", "name2", "name3", "name4", "name5", "name6",
            "name7", "name8", "name9", "name10"),
        "display_col" = c(
            "value1", "value2", "value3", "value4", "value5", "value6",
            "value7", "value8", "value9", "value10"))
    testthat::expect_that(
        create_nested_list_by_class(
            test_df, "class_col", "display_col", "internal_col"),
        testthat::is_identical_to(list(
            "class1" = c("value1" = "name1",
                         "value2" = "name2",
                         "value3" = "name3"),
            "class2" = c("value4" = "name4",
                         "value5" = "name5"),
            "class3" = c("value6" = "name6"),
            "class4" = c("value7" = "name7",
                         "value8" = "name8",
                         "value9" = "name9"),
            "Other" = c("value10" = "name10"))))
})

testthat::test_that("create_filtered_nested_list_by_class", {
    test_df <- data_frame(
        "CLASS" = c(
            "class1", "class1", "class1", "class2", "class2", "class3",
            "class4", "class4", "class4", NA),
        "INTERNAL" = c(
            "name1", "name2", "name3", "name4", "name5", "name6",
            "name7", "name8", "name9", "name10"),
        "DISPLAY" = c(
            "value1", "value2", "value3", "value4", "value5", "value6",
            "value7", "value8", "value9", "value10"),
        "FILTER" = c(rep("numeric", 5), rep("categorical", 5))
    )
    testthat::expect_that(
        create_filtered_nested_list_by_class(test_df, "numeric"),
        testthat::is_identical_to(list(
            "class1" = c("value1" = "name1",
                         "value2" = "name2",
                         "value3" = "name3"),
            "class2" = c("value4" = "name4",
                         "value5" = "name5"))))
})

testthat::test_that("get_unique_column_values", {
    test_df1 <- data_frame("col" = c("value1", "value2"))
    test_df2 <- data_frame("col" = c("value1", "value1"))
    test_df3 <- data_frame("col" = c("value1", NA))
    test_df4 <- data_frame("col" = c(5, 6))
    test_df5 <- data_frame("col" = c("value2", "value1"))
    testthat::expect_that(
        get_unique_column_values("col", test_df1),
        testthat::is_identical_to(c("value1", "value2")))
    testthat::expect_that(
        get_unique_column_values("col", test_df2),
        testthat::is_identical_to(c("value1")))
    testthat::expect_that(
        get_unique_column_values("col", test_df3),
        testthat::is_identical_to(c("value1")))
    testthat::expect_that(
        get_unique_column_values("col", test_df4),
        testthat::is_identical_to(c("5", "6")))
    testthat::expect_that(
        get_unique_column_values("col", test_df5),
        testthat::is_identical_to(c("value1", "value2")))
})

testthat::test_that("get_variable_classes", {
    test_df <- data_frame(
        "class" = c("class1", "class1", "class1", "class2", "class2", "class3",
                    "class4", "class4", "class4"),
        "type" = c("Numeric", "Numeric", "Numeric", "Factor", "Factor", 
                   "Numeric", "Logical", "Logical", "Logical"))
    testthat::expect_that(
        get_variable_classes(
            test_df, "class", "type", "Numeric"),
        testthat::is_identical_to(c("class1", "class3")))
})

