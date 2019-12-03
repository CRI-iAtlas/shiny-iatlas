# Create a database connection.
connect_to_db <- function() {
  return(DBI::dbConnect(
    RPostgres::Postgres(),
    host = .GlobalEnv$DB_HOST,
    port = .GlobalEnv$DB_PORT,
    dbname = .GlobalEnv$DB_NAME,
    user = .GlobalEnv$DB_USER,
    password = .GlobalEnv$DB_PW,
    bigint = "numeric"
  ))
}
