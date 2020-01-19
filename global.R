library(magrittr)

source("database/connect_to_db.R")
pool <- connect_to_db()
rm(connect_to_db)

source("functions/transform.R")
source("functions/utils.R")
source("functions/format.R")
source("functions/plot_functions/scatterplot.R")
source("functions/plot_functions/violinplot.R")
source("functions/plot_functions/mosaicplot.R")
source("functions/plot_functions/barplot.R")
source("functions/plot_functions/boxplot.R")
source("functions/plot_functions/heatmap.R")
source("functions/plot_functions/kmplot.R")
source("functions/plot_functions/histogram.R")





