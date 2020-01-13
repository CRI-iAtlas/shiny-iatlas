cat("In .Rprofile",fill = TRUE)
if (Sys.getenv("RSTUDIO") == "1" | Sys.getenv("DOCKERBUILD") == "1") {
cat("In if block",fill = TRUE)
  try(source("renv/activate.R"))
  try(install.packages("startup"))
}


# Attempt to run startup
try(startup::startup())

if (Sys.getenv("DOCKERBUILD") == "1") {
  try(renv::restore(confirm = FALSE))
  local({
   options(shiny.port = 3838, shiny.host = "0.0.0.0")
  })
}
