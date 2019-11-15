# Create the database connection.
create_db_connection <- function() {
    con = con <- dbConnect(
        RPostgres::Postgres(),
        dbname = DB_NAME, # 'shiny_iatlas_dev',
        host = DB_HOST, # 'localhost',
        port = DB_PORT, # 5432,
        user = DB_USER, # 'postgres',
        password = DB_PW, # 'docker'
    )
}