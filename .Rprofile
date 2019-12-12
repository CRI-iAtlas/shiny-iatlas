# Initialize renv (this will bootstrap itself).
source("renv/activate.R")

tryCatch(
  startup::startup(),
  error = function(e) {
    if (!'startup' %in% installed.packages()) {
      install.packages("startup")
    }
    try(startup::startup())
  }
)

try(renv::restore(confirm = FALSE))
