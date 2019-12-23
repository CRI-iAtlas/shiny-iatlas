if (Sys.getenv("RSTUDIO") == "1") {
  try(source("renv/activate.R"))
  try(install.packages("startup"))
}

# Attempt to run startup
try(startup::startup())
