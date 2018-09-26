library(yaml)
library(tidyverse)
library(testthat)

config_yaml <- yaml::read_yaml("../configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)

source("functions/utils.R")
source("tests/test_utils.R")

source("functions/transform.R")
source("tests/test_transform.R")