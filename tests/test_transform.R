library(yaml)
library(tidyverse)

config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$function_files, source)

# immunomodulator functions ---------------------------------------------------

testthat::test_that("build_immunomodulator_expression_df", {
    expr_df <- data_frame(
        "ID" = c("a", "a", "b","b"),
        "COUNT" = c(1, 10, 100, 100),
        "GENE" = c("genea", "geneb", "genea", "geneb")
    )
    group_df <- data_frame(
        "ID" = c("a", "b", "c", "d"),
        "GROUP" = c("group1", "group2", "group1", "group2")
    )
    testthat::expect_that(
        build_immunomodulator_expression_df(
            group_df, 
            filter_value = "genea",
            group_col = "GROUP",
            expression_df = expr_df,
            expression_filter_col = "GENE",
            expression_col = "COUNT",
            id_col = "ID"
        ),
        testthat::is_identical_to(data_frame(
            "GROUP" = c("group1", "group2"),
            "LOG_COUNT" = c(log10(1+1), log10(100+1))
        ))
    )
})

testthat::test_that("filter_immunomodulator_expression_df", {
    expr_df <- data_frame(
        "ID" = c("a", "a", "b","b"),
        "COUNT" = c(1, 10, 100, 100),
        "GENE" = c("genea", "geneb", "genea", "geneb")
    )
    testthat::expect_that(
        filter_immunomodulator_expression_df(
            expr_df, 
            id_col = "ID",
            filter_col = "GENE",
            expression_col = "COUNT",
            filter_value = "genea"
        ),
        testthat::is_identical_to(data_frame(
            "LOG_COUNT" = c(log10(1+1), log10(100+1)),
            "ID" = c("a", "b")
        ))
    )
})
                    

# samplegroup functions -------------------------------------------------------
testthat::test_that("build_sample_group_key_df",{
    group_df <- data_frame(
        "group_col1" = c(rep("group1", 5), rep("group2", 3)),
        "group_col2" = c(rep("group3", 3), rep("group4", 4), NA),
        "group_col4" = c(rep("group5", 5), rep("group6", 3)),
        "group_col5" = c(rep("group7", 5), rep("group8", 3)))
    color_vector = c(
        "group1" = "red",
        "group2" = "blue",
        "group3" = "green",
        "group4" = "yellow",
        "group7" = "orange",
        "group8" = "purple")
    feature_df <- data_frame(
        "FeatureValue" = c("group1", "group2", "group3", "group4", "group5"),
        "FeatureName" = c("n1", "n2", "n3", "n4", "n5"),
        "Characteristics" = c("c1", "c2", "c3", "c4", "c5"))
    result_df1 <- data_frame(
        "Sample Group" = c("group1", "group2"),
        "Group Name" = c("n1", "n2"),
        "Group Size" = c(5L, 3L),
        "Characteristics" = c("c1", "c2"),
        "Plot Color" = c("red", "blue"))
    result_df2 <- data_frame(
        "Sample Group" = c("group3", "group4"),
        "Group Name" = c("n3", "n4"),
        "Group Size" = c(3L, 4L),
        "Characteristics" = c("c3", "c4"),
        "Plot Color" = c("green", "yellow"))
    testthat::expect_that(
        build_sample_group_key_df(group_df, "group_col1", color_vector, feature_df),
        testthat::is_identical_to(result_df1))
    testthat::expect_that(
        build_sample_group_key_df(group_df, "group_col2", color_vector, feature_df),
        testthat::is_identical_to(result_df2))
    testthat::expect_that(
        build_sample_group_key_df(group_df, "group_col3", color_vector, feature_df),
        testthat::throws_error("df has missing columns: group_col3"))
    testthat::expect_that(
        build_sample_group_key_df(group_df, "group_col4", color_vector, feature_df),
        testthat::throws_error("result df is empty"))
})

testthat::test_that("build_plot_color_df",{
    subset_df <- data_frame(
        "group_col1" = c(rep("group1", 5), rep("group2", 3)),
        "group_col2" = c(rep("group2", 3), rep("group3", 4), NA))
    color_vector = c(
        "group1" = "red",
        "group2" = "blue",
        "group3" = "green")
    result_color_df1 <- data_frame(
        "Group" = c("group1", "group2"),
        "Plot_Color" = c("red", "blue"))
    result_color_df2<- data_frame(
        "Group" = c("group2", "group3"),
        "Plot_Color" = c("blue", "green"))
    testthat::expect_that(
        build_plot_color_df(color_vector, subset_df, "group_col1"),
        testthat::is_identical_to(result_color_df1))
    testthat::expect_that(
        build_plot_color_df(color_vector, subset_df, "group_col2"),
        testthat::is_identical_to(result_color_df2))
})

testthat::test_that("build_group_size_df",{
    test_subset_df <- data_frame(
        "group_col1" = c(rep("group1", 5), rep("group2", 3)),
        "group_col2" = c(rep("group3", 3), rep("group4", 4), NA))
    result_size_df1 <- data_frame(
        "Group" = c("group1", "group2"),
        "Group_Size" = c(5L, 3L))
    result_size_df2 <- data_frame(
        "Group" = c("group3", "group4"),
        "Group_Size" = c(3L, 4L))
    testthat::expect_that(
        build_group_size_df(test_subset_df, "group_col1"),
        testthat::is_identical_to(result_size_df1))
    testthat::expect_that(
        build_group_size_df(test_subset_df, "group_col2"),
        testthat::is_identical_to(result_size_df2))
})

# functions for making plot df label column -----------------------------------


testthat::test_that("create_label", {
    input_df <- data_frame(
        "name" = c("id1", "id2", "id3", "id4"),
        "group" = c("group1", "group1", "group2", "group2"),
        "value_col1" = c(1, 2, 3, 4),
        "value_col2" = c(8, 9, 10, 11),
        "value_col3" = c(12, 14, 100, 110))
    result_df1 <- data_frame(
        "name" = c("id1", "id2", "id3", "id4"),
        "group" = c("group1", "group1", "group2", "group2"),
        "value_col3" = c(12, 14, 100, 110),
        "label" = c(
            "<b>ParticipantBarcode:</b> id1 (group1)</br></br>VALUE_COL1: 1.000</br>VALUE_COL2: 8.000",
            "<b>ParticipantBarcode:</b> id2 (group1)</br></br>VALUE_COL1: 2.000</br>VALUE_COL2: 9.000",
            "<b>ParticipantBarcode:</b> id3 (group2)</br></br>VALUE_COL1: 3.000</br>VALUE_COL2: 10.000",
            "<b>ParticipantBarcode:</b> id4 (group2)</br></br>VALUE_COL1: 4.000</br>VALUE_COL2: 11.000"),
        "value_col1" = c(1, 2, 3, 4),
        "value_col2" = c(8, 9, 10, 11))
    result_df2 <- data_frame(
        "name" = c("id1", "id2", "id3", "id4"),
        "group" = c("group1", "group1", "group2", "group2"),
        "label" = c(
            "<b>ParticipantBarcode:</b> id1 (group1)</br></br>VALUE_COL1: 1.000</br>VALUE_COL2: 8.000</br>VALUE_COL3: 12.000",
            "<b>ParticipantBarcode:</b> id2 (group1)</br></br>VALUE_COL1: 2.000</br>VALUE_COL2: 9.000</br>VALUE_COL3: 14.000",
            "<b>ParticipantBarcode:</b> id3 (group2)</br></br>VALUE_COL1: 3.000</br>VALUE_COL2: 10.000</br>VALUE_COL3: 100.000",
            "<b>ParticipantBarcode:</b> id4 (group2)</br></br>VALUE_COL1: 4.000</br>VALUE_COL2: 11.000</br>VALUE_COL3: 110.000"),
        "value_col1" = c(1, 2, 3, 4),
        "value_col2" = c(8, 9, 10, 11),
        "value_col3" = c(12, 14, 100, 110))
    testthat::expect_that(
        create_label(input_df, c("value_col1", "value_col2")),
        testthat::is_identical_to(result_df1))
    testthat::expect_that(
        create_label(input_df, c("value_col1", "value_col2", "value_col3")),
        testthat::is_identical_to(result_df2))
})

# other functions -------------------------------------------------------------

testthat::test_that("summarise_df_at_column",{
    test_df1 <- data_frame(
        "group_col1" = c(rep("group1", 5)),
        "group_col2" = c(rep("group1", 3), rep("group2", 2)),
        "sum_col1" = c(rep(5, 5)))
    result_df1 <- data_frame(
        "group_col1" = "group1",
        "mean" = 5)
    result_df2 <- data_frame(
        "group_col1" = "group1", 
        "mean" = 5,
        "sd" = 0)
    result_df3 <- data_frame(
        "group_col2" = c("group1", "group2"),
        "mean" = c(5, 5))
    result_df4 <- data_frame(
        "group_col1" = c("group1", "group1"),
        "group_col2" = c("group1", "group2"),
        "mean" = c(5, 5))
    # one group one function
    testthat::expect_that(
        summarise_df_at_column(test_df1, "sum_col1", "group_col1", "mean"),
        testthat::is_identical_to(result_df1))
    # two functions
    testthat::expect_that(
        summarise_df_at_column(test_df1, "sum_col1", "group_col1", c("mean", "sd")),
        testthat::is_identical_to(result_df2))
    # two groups
    testthat::expect_that(
        summarise_df_at_column(test_df1, "sum_col1", "group_col2", "mean"),
        testthat::is_identical_to(result_df3))
    # two grouping columns
    testthat::expect_that(
        summarise_df_at_column(test_df1, "sum_col1", c("group_col1", "group_col2"), "mean"),
        testthat::is_identical_to(result_df4))
})

