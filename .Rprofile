inRStudio <- Sys.getenv("RSTUDIO") == "1"
if (inRStudio) {
  tryCatch(source("renv/activate.R"), error = function(e) {print(e)})
  tryCatch(install.packages("startup"), error = function(e) {print(e)})
}

# Attempt to run startup; log errors
tryCatch(startup::startup(), error = function(e) {print(e)})

# Attempt init renv if inRStudio, log errors
if (inRStudio) {
  cat("NOTE: install dependencies with renv::restore()\n")
}
