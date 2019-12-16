# Database helper functions.
delete_rows <- function(table_name) {
  RPostgres::dbSendQuery(.GlobalEnv$pool, paste("DELETE FROM", table_name, sep = " "))
}

read_table <- function(table_name) {
  return(RPostgres::dbReadTable(.GlobalEnv$pool, table_name))
}

write_table_ts <- function(df, table_name) {
  rs <- FALSE
  # Begin transaction.
  RPostgres::dbBegin(.GlobalEnv$pool)
  tryCatch(
    rs <- RPostgres::dbWriteTable(.GlobalEnv$pool, table_name, df, append = TRUE, copy = TRUE),
    error = function(e) {
      warning(e)
      warning("Rolling back transaction")
      RPostgres::dbRollback(.GlobalEnv$pool)
    }
  )
  if (isTRUE(rs)) {
    return(RPostgres::dbCommit(.GlobalEnv$pool))
  }
  return(FALSE)
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
