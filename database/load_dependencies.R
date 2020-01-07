load_dependencies <- function() {
  # Ensure crayon is installed. This is used to display messages in the console with color.
  if (!'crayon' %in% installed.packages()) {
    install.packages("crayon")
  }

  # Ensure stringi is installed. This is loaded to do some string manipulation with Regex.
  if (!'stringi' %in% installed.packages()) {
    install.packages("stringi")
  }

  # Ensure magrittr is installed. This is loaded to ensure the %>% pipe is available.
  if (!'magrittr' %in% installed.packages()) {
    install.packages("magrittr")
  }

  # Ensure feather is installed. This is used to read feather files.
  if (!'feather' %in% installed.packages()) {
    install.packages("feather")
  }

  # Ensure shiny is installed. This is used to connect to a PostgreSQL db.
  if (!'RPostgres' %in% installed.packages()) {
    install.packages("RPostgres")
  }

  # Ensure pool is installed. This is used to connect to a db using pooling. Also provides transactions.
  if (!'pool' %in% installed.packages()) {
    install.packages("pool")
  }

  ### Only need to load packages that have functionality that is NOT called like pkg::function() ###

  # Load magrittr so %>% is available.
  library("magrittr")
}
