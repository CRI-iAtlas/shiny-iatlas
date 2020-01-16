# Database helper functions.

delete_rows <- function(table_name) {
  current_pool <- pool::poolCheckout(.GlobalEnv$pool)
  result <- pool::dbSendQuery(current_pool, paste("DELETE FROM", table_name, sep = " "))
  pool::poolReturn(current_pool)
  return(result)
}

read_table <- function(table_name) {
  tictoc::tic(paste0("Time taken to read from the `", table_name, "`` table in the DB"))
  current_pool <- pool::poolCheckout(.GlobalEnv$pool)
  result <- pool::dbReadTable(current_pool, table_name)
  pool::poolReturn(current_pool)
  tictoc::toc()
  return(result)
}

# update_table <- function(df, table_name) {
#   return(pool::poolWithTransaction(.GlobalEnv$pool, function(conn) {
#     pool::dbGetQuery(
#       conn,
#       paste0("INSERT INTO", table_name, "(id, column_1, column_2) 
#       VALUES (1, 'A', 'X'), (2, 'B', 'Y'), (3, 'C', 'Z')
#       ON CONFLICT (id) DO UPDATE
#         SET column_1 = excluded.column_1,
#           column_2 = excluded.column_2;")
#     )
#   }))
# }

write_table_ts <- function(df, table_name) {
  tictoc::tic(paste0("Time taken to write to the `", table_name, "`` table in the DB"))
  result <- pool::poolWithTransaction(.GlobalEnv$pool, function(conn) {
    # Disable all table indexes.
    conn %>% pool::dbGetQuery(paste0(
      "UPDATE pg_index ",
      "SET indisready=false ",
      "WHERE indrelid = (",
      "SELECT oid ",
      "FROM pg_class ",
      "WHERE relname='", table_name, "'",
      ");"
    ))

    conn %>% pool::dbWriteTable(table_name, df, append = TRUE, copy = TRUE)

    # Re-enable all table indexes.
    conn %>% pool::dbGetQuery(paste0(
      "UPDATE pg_index ",
      "SET indisready=true ",
      "WHERE indrelid = (",
      "SELECT oid ",
      "FROM pg_class ",
      "WHERE relname='", table_name, "'",
      ");"
    ))

    # Reindex the table
    conn %>% pool::dbExecute(paste0("REINDEX TABLE ", table_name, ";"))
  })
  tictoc::toc()

  return(result)
}

create_conection <- function(table_name) {
    current_pool <- pool::poolCheckout(.GlobalEnv$pool)
    result <- dplyr::tbl(current_pool, table_name)
    pool::poolReturn(current_pool)
    return(result)
}


# INSERT INTO features (person_id, group_id)
# SELECT p.person_id, g.group_id
# FROM  (
#   VALUES
#   ('alice'::varchar, 'girls'::varchar)
#   , ('bob','boys',
#      , ('alice','coolkids')
#      , ('bob','coolkids')
#   ) x (username, group_name)
#   JOIN   person p  USING (username)
#   JOIN   "group" g USING (group_name);

# saveData <- function(data) {
#   # Connect to the database
#   pcon <- dbConnect(psql, dbname = "XXX", host = "XXXXX", port = XXXX, user 
#                     = "UserX", password = "PaswordX")
#   # Construct the update query by looping over the data fields
#   query <- paste0("INSERT INTO table_name.schema_name (message) VALUES ( $1 
# )")
#   # Submit the update query and disconnect
#   dbSendQuery(pcon, query, params=data[["message"]])
#   dbDisconnect(pcon)
# }


# sends the command and creates the table
# dbGetQuery(con, sql_command)
