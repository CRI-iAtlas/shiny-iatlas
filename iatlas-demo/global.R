library(yaml)
library(tidyverse)

config_yaml <- yaml::read_yaml("configuration.yaml")
purrr::walk(config_yaml$libraries, library, character.only = T)
purrr::walk(config_yaml$source_files, source)


# common plot theme
theme_1012 <- theme(
    axis.text = element_text(face = "bold", size = 10, color = "black"),
    axis.title = element_text(face = "bold", size = 12, color = "black"),
    panel.border = element_rect(colour = "black", size = 1),
    strip.text =  element_text(face = "bold", size = 10, color = "black"),
    strip.background = element_rect(colour = "black", size = 1),
    title = element_text(face = "bold", size = 14, color = "black"),
    legend.text = element_text(face = "bold", size = 8, color = "black")
)

# general data loading & prep
USE_REMOTE_BQ = F ## set data as remote (BigQuery) or local(Feature Matrix on disk)
USE_REMOTE_GS = T

feature_table         <- load_manifest()
panimmune_data        <- load_data()
modulators_data       <- load_modulators()
# class_membership_list <- create_membership_list() not used currently
