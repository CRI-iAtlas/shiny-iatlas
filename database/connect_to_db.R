# Create a database connection.
connect_to_db <- function() {
  return(pool::dbPool(
    # Connect to a PostgreSQL database.
    drv = RPostgres::Postgres(),
    # The database name, host name, port, username,
    # and password all are set in environment variables.
    dbname = .GlobalEnv$DB_NAME,
    host = .GlobalEnv$DB_HOST,
    port = .GlobalEnv$DB_PORT,
    user = .GlobalEnv$DB_USER,
    password = .GlobalEnv$DB_PW,
    # The R type that 64-bit integer types should be mapped to,
    # default is bit64::integer64, which allows the full range of 64 bit integers
    # See: http://www.win-vector.com/blog/2018/03/take-care-if-trying-the-rpostgres-package/
    bigint = "numeric",
    # Limit the max pool size to 10.
    maxSize = 10
  ))
}
