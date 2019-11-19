# Initialize renv (this will bootstrap itself).
source("renv/activate.R")

tryCatch(startup::startup(), error = function(e) {
    install.packages("startup")
    try(startup::startup())
})

try(renv::restore(confirm = FALSE))
