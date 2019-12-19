# Database helper functions.

delete_rows <- function(table_name) {
  current_pool <- pool::poolCheckout(.GlobalEnv$pool)
  result <- pool::dbSendQuery(current_pool, paste("DELETE FROM", table_name, sep = " "))
  pool::poolReturn(current_pool)
  return(result)
}

read_table <- function(table_name) {
  current_pool <- pool::poolCheckout(.GlobalEnv$pool)
  result <- pool::dbReadTable(current_pool, table_name)
  pool::poolReturn(current_pool)
  return(result)
}

write_table_ts <- function(df, table_name) {
  return(pool::poolWithTransaction(.GlobalEnv$pool, function(conn) {
    pool::dbWriteTable(conn, table_name, df, append = TRUE, copy = TRUE)
  }))
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
