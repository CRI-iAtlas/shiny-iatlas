# Global function that may be used to spin-up, create, or reset the Postgres DB.
# env may be "dev", "test", or NULL. If NULL is passed, it will default to dev.
# reset may be "create", "reset", or NULL. If NULL is passed, it won't rebuild the DB and tables.
# NOTE: If "create" or "reset" are passed, the DB and tables will be built, wiping out any existing DB and tables.
create_db <- function(env = "dev", reset = NULL) {
  system(paste("bash scripts/create_db.sh", env, reset, sep = " "))
}
