library(yaml)
library(tidyverse)

config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)

source("../functions/utils.R")

test_that("get_factored_variables_by_class", {
    test_df <- data_frame(
        "class col" = c("class1", "class1", "class1", "class2", "class2", "class2"),
        "variable col" = c("var1", "var2", "var3", "var4", "var5", "var6"),
        "order col" = c(1,2,3,3,2,1))
    expect_that(
        get_factored_variables_by_class(
            "class1", test_df, "class_col", "variable_col", "order_col"),
        is_identical_to(
            factor(c("var1", "var2", "var3"), levels = c("var1", "var2", "var3"))))
    expect_that(
        get_factored_variables_by_class(
            "class2", test_df, "class_col", "variable_col", "order_col"),
        is_identical_to(
            factor(c("var6", "var5", "var4"),  levels = c("var6", "var5", "var4"))))
    expect_that(
        get_factored_variables_by_class(
            "class3", test_df, "class_col", "variable_col", "order_col"),
        throws_error("empty class: class3"))
})


test_that("factor_variables_with_df", {
    test_df1 <- data_frame(
        "variable_col" = c("var1", "var2", "var3"),
        "order_col" = c(1,2,3))
    test_df2 <- data_frame(
        "variable_col" = c("var4", "var5", "var6"),
        "order_col" = c(3,2,1))
    expect_that(
        factor_variables_with_df(test_df1, "variable_col", "order_col"),
        is_identical_to(factor(c("var1", "var2", "var3"),
                               levels = c("var1", "var2", "var3"))))
    expect_that(
        factor_variables_with_df(test_df2, "variable_col", "order_col"),
        is_identical_to(factor(c("var6", "var5", "var4"),
                               levels = c("var6", "var5", "var4"))))
})

test_that("get_complete_class_df", {
    test_df <- data_frame(
        "class col" = c("class1", "class1", "class1", "class2", "class2", "class2"),
        "variable col" = c("var1", "var2", "var3", "var4", "var5", "var6"),
        "order col" = c(1,2,3,3,2,1))
    result_df1 <- data_frame(
        "variable_col" = c("var1", "var2", "var3"),
        "order_col" = c(1,2,3))
    result_df2 <- data_frame(
        "variable_col" = c("var4", "var5", "var6"),
        "order_col" = c(3,2,1))
    expect_that(
        get_complete_class_df("class1", test_df, "class_col", "variable_col", "order_col"),
        is_identical_to(result_df1))
    expect_that(
        get_complete_class_df("class2", test_df, "class_col", "variable_col", "order_col"),
        is_identical_to(result_df2))
})

test_that("get_complete_df_by_columns",{
    test_df <- data_frame(
        "col1" = c("val1", "val2", "val3"),
        "col2" = c(NA, 1, 2))
    expect_that(
        get_complete_df_by_columns(test_df, c("col1")),
        is_identical_to(data_frame(
            "col1" = c("val1", "val2", "val3"))))
    expect_that(
        get_complete_df_by_columns(test_df, c("col1", "col2")),
        is_identical_to(data_frame(
            "col1" = c("val2", "val3"),
            "col2" = c(1, 2))))
})

test_that("get_group_internal_name", {
    test_df1 <- data_frame("FriendlyLabel" = c("value1", "value3", "value3"),
                           "FeatureMatrixLabelTSV" = c("A", "B", "C"))
    expect_that(
        get_group_internal_name("value1", test_df1), 
        is_identical_to("A"))
    expect_that(
        get_group_internal_name("value2", test_df1), 
        is_identical_to("value2"))
    expect_that(
        get_group_internal_name("value3", test_df1),
        throws_error("group name has multiple matches: value3 matches: B, C"))
})

test_that("convert_value_between_columns", {
    test_df1 <- data_frame("col1" = c("value1", "value2"),
                           "col2" = c("A", "B"),
                           "col3" = c("C", "C"))
    expect_that(
        convert_value_between_columns(test_df1, "value1", "col1", "col2"), 
        is_identical_to("A"))
    expect_that(
        convert_value_between_columns(test_df1, "value2", "col1", "col2"), 
        is_identical_to("B"))
    expect_that(
        convert_value_between_columns(test_df1, "value1", "col1", "col3"), 
        is_identical_to("C"))
    expect_that(
        convert_value_between_columns(test_df1, "C", "col3", "col1"), 
        is_identical_to(c("value1", "value2")))
    expect_that(
        convert_value_between_columns(test_df1, "value3", "col1", "col2"), 
        is_identical_to(vector(mode = "character", length = 0)))
})


test_that("get_unique_column_values", {
    test_df1 <- data_frame("col" = c("value1", "value2"))
    test_df2 <- data_frame("col" = c("value1", "value1"))
    test_df3 <- data_frame("col" = c("value1", NA))
    test_df4 <- data_frame("col" = c(5, 6))
    test_df5 <- data_frame("col" = c("value2", "value1"))
    expect_that(
        get_unique_column_values("col", test_df1),
        is_identical_to(c("value1", "value2")))
    expect_that(
        get_unique_column_values("col", test_df2),
        is_identical_to(c("value1")))
    expect_that(
        get_unique_column_values("col", test_df3),
        is_identical_to(c("value1")))
    expect_that(
        get_unique_column_values("col", test_df4),
        is_identical_to(c("5", "6")))
    expect_that(
        get_unique_column_values("col", test_df5),
        is_identical_to(c("value1", "value2")))
})