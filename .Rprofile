try(source("renv/activate.R"))
try(if (!"startup" %in% installed.packages()) {renv::install("startup")})
try(if (!"crayon" %in% installed.packages()) {renv::install("crayon")})
try(startup::startup())

cat(
  crayon::green("================================================================="),
  crayon::green("NOTE: run 'renv::restore()' to install packages"),
  crayon::green("      run 'shiny::runApp()' to run the app locally"),
  crayon::green("      run 'rsconnect::deployApp()' to deploy to shinyapps.io"),
  crayon::green("See README.md for more.\n"),
  fill = TRUE
)
