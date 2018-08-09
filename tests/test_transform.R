library(yaml)
library(tidyverse)
library(testthat)

config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)

source("../functions/transform.R")

test_that("build_mosaicplot_df", {
    group_df <- data_frame(
        "group_col1" = c(rep("group1", 5), rep("group2", 3)),
        "group_col2" = c(rep("group3", 3), rep(NA, 5)),
        "group_col4" = c(rep("group5", 2), rep("group6", 6)),
        "group_col5" = c(rep("group7", 4), rep("group8", 4)))
    result_df1 <- data_frame(
        "group_col1" = as.factor(c(
            rep("group1", 5), rep("group2", 3))),
        "group_col4" = forcats::fct_rev(as.factor(c(
            rep("group5", 2), rep("group6", 6)))))
    result_df2 <- data_frame(
        "group_col1" = as.factor(c(
            rep("group1", 5), rep("group2", 3))),
        "group_col5" = forcats::fct_rev(as.factor(c(
            rep("group7", 4), rep("group8", 4)))))
    result_df3 <- data_frame(
        "group_col1" = as.factor(c(
            rep("group1", 3))),
        "group_col2" = forcats::fct_rev(as.factor(c(
            rep("group3", 3)))))
    expect_that(
        build_mosaicplot_df(group_df, "group_col1", "group_col4"),
        is_identical_to(result_df1))
    expect_that(
        build_mosaicplot_df(group_df, "group_col1", "group_col5"),
        is_identical_to(result_df2))
    expect_that(
        build_mosaicplot_df(group_df, "group_col1", "group_col2"),
        is_identical_to(result_df3))
    expect_that(
        build_mosaicplot_df(group_df, "group_col1", "group_col6"),
        throws_error("Group df has no Y column: group_col6"))
    expect_that(
        build_mosaicplot_df(group_df, "group_col6", "group_col1"),
        throws_error("Group df has no X column: group_col6"))
})

test_that("build_sample_group_key_df",{
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
    expect_that(
        build_sample_group_key_df(group_df, "group_col1", color_vector, feature_df),
        is_identical_to(result_df1))
    expect_that(
        build_sample_group_key_df(group_df, "group_col2", color_vector, feature_df),
        is_identical_to(result_df2))
    expect_that(
        build_sample_group_key_df(group_df, "group_col3", color_vector, feature_df),
        throws_error("Group df has no column: group_col3"))
    expect_that(
        build_sample_group_key_df(group_df, "group_col4", color_vector, feature_df),
        throws_error("No matching members in groups between color vector, group df column: group_col4"))
    expect_that(
        build_sample_group_key_df(group_df, "group_col5", color_vector, feature_df),
        throws_error("Result df is empty"))
})

test_that("build_plot_color_df",{
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
    expect_that(
        build_plot_color_df(color_vector, subset_df, "group_col1"),
        is_identical_to(result_color_df1))
    expect_that(
        build_plot_color_df(color_vector, subset_df, "group_col2"),
        is_identical_to(result_color_df2))
})

test_that("build_group_size_df",{
    test_subset_df <- data_frame(
        "group_col1" = c(rep("group1", 5), rep("group2", 3)),
        "group_col2" = c(rep("group3", 3), rep("group4", 4), NA))
    result_size_df1 <- data_frame(
        "Group" = c("group1", "group2"),
        "Group_Size" = c(5L, 3L))
    result_size_df2 <- data_frame(
        "Group" = c("group3", "group4"),
        "Group_Size" = c(3L, 4L))
    expect_that(
        build_group_size_df(test_subset_df, "group_col1"),
        is_identical_to(result_size_df1))
    expect_that(
        build_group_size_df(test_subset_df, "group_col2"),
        is_identical_to(result_size_df2))
})
