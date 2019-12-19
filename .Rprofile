inRStudio <- Sys.getenv("RSTUDIO") == "1"
if (inRStudio) {
  try(source("renv/activate.R"))
  try(install.packages("startup"))
}

# Attempt to run startup
try(startup::startup())
