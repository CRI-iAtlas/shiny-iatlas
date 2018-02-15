library(shinythemes)
library(bigrquery)
library(plotly)
library(heatmaply)
library(survival)
library(survminer)
library(tidyverse)

source("modules/cellcontentmodule.R")
source("modules/immuneinterfacemodule.R")
source("modules/featurecorrelationmodule.R")
source("modules/survivalmodule.R")
source("functions/load_data.R")
source("functions/utils.R")
source("functions/boxplot.R")
#source("functions/heatmap.R")
#source("functions/kmplot.R")

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
USE_REMOTE = FALSE ## set data as remote (BigQuery) or local(Feature Matrix on disk)


cellcontent_data <- load_cellcontent_data(USE_REMOTE)
clonaldiversity_data <- load_clonaldiversity_data(USE_REMOTE)
corrheatmap_data <- load_corrheatmap_data(USE_REMOTE)
survivalcurve_data <- corrheatmap_data