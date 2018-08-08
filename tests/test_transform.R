library(yaml)
library(tidyverse)
library(testthat)

config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)

source("../functions/transform.R")

test_that("build_plot_color_df",{
    plot_colors = c(
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
        build_plot_color_df(plot_colors, c("group1", "group2")),
        is_identical_to(result_color_df1))
    expect_that(
        build_plot_color_df(plot_colors, c("group3", "group2")),
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
