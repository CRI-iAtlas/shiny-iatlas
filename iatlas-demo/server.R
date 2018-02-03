################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100*1024^2)

################################################################################
# Begin Shiny Server definition.
################################################################################
shinyServer(function(input, output) {
    # Cell content
    source("panels/panel-server-cellcontent.R", local = TRUE)
    # Clonal diversity
    source("panels/panel-server-clonaldiversity.R", local = TRUE)
    # Correlation heatmaps
    source("panels/panel-server-corrheatmap.R", local = TRUE)
    # Survival curves
    source("panels/panel-server-survivalcurve.R", local = TRUE)
})
################################################################################
