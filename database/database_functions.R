# Database helper functions.
value_to_id <- function(value, int_value, id_value, name_value) {
  if (identical(value, name_value)) {
    return(id_value)
  } else if (!is.na(int_value)) {
    return(int_value)
  } else {
    return(NA)
  }
}

rebuild_features <- function(features, classes, method_tags) {
  # Ensure data.frames.
  classes <- classes %>% as.data.frame
  method_tags <- method_tags %>% as.data.frame
  for (row in 1:nrow(classes)) {
    features <- features %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(class_int = value_to_id(class, class_int, classes[row, "id"], classes[row, "name"]))
  }
  for (row in 1:nrow(method_tags)) {
    features <- features %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(mt_int = value_to_id(methods_tag, mt_int, method_tags[row, "id"], method_tags[row, "name"]))
  }
  return(features)
}

rebuild_groups <- function(groups, all_groups) {
  # Ensure data.frames.
  all_groups <- all_groups %>% as.data.frame
  for (row in 1:nrow(all_groups)) {
    groups <- groups %>%
      dplyr::select(dplyr::everything()) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(parent = value_to_id(parent_group, parent, all_groups[row, "id"], all_groups[row, "name"])) %>%
      dplyr::mutate(subgroup = value_to_id(subtype_group, subgroup, all_groups[row, "id"], all_groups[row, "name"]))
  }
  return(groups)
}

# rebuild_samples <- function(samples, til_image_links) {
#   for (row in 1:nrow(all_groups)) {
#     # cat("rebuild_groups:: row:", row, fill = TRUE, sep = " ")
#     # cat("rebuild_groups:: name:", fill = TRUE)
#     # print(all_groups[row, "name"])
#     # cat("\n", fill = TRUE)
#     groups <- groups %>%
#       dplyr::select(dplyr::everything()) %>%
#       dplyr::rowwise() %>%
#       dplyr::mutate(parent = value_to_id(parent_group, parent, all_groups[row, "id"], all_groups[row, "name"])) %>%
#       dplyr::mutate(subgroup = value_to_id(subtype_group, subgroup, all_groups[row, "id"], all_groups[row, "name"]))
#   }
#   return(groups)
# }

delete_rows <- function(con, tn) {
  RPostgres::dbSendQuery(con, paste("DELETE FROM", tn, sep = " "))
}

write_table_ts <- function(con, tn, df) {
  rs <- FALSE
  # Begin transaction.
  RPostgres::dbBegin(con)
  tryCatch(
    rs <- RPostgres::dbWriteTable(con, tn, df, append = TRUE, copy = TRUE),
    error = function(e) {
      warning(e)
      warning("Rolling back transaction")
      RPostgres::dbRollback(con)
    }
  )
  if (isTRUE(rs)) {
    return(RPostgres::dbCommit(con))
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
