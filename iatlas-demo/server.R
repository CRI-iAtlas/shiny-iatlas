################################################################################
# Options, default settings, and load packages
################################################################################
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 100 * 1024^2)

################################################################################
# Begin Shiny Server definition.
################################################################################
shinyServer(function(input, output) {
  # # Cell content
  callModule(cellcontent, "module1")
  # Clonal diversity
  callModule(immuneinterface, "module2")
  # Correlation heatmaps
  callModule(featurecorrelation, "module3")
  # Survival curves
  callModule(survival, "module4")
  # immunomodulators
  callModule(immunomodulator, "module5")
})
################################################################################
