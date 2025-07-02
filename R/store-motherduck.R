is_motherduck_location <- function(location) {
  grepl("^md:", location)
}

motherduck_connection <- function(location, create = FALSE, overwrite = FALSE) {
  con <- DBI::dbConnect(duckdb::duckdb(), array = "matrix")
  DBI::dbExecute(con, "INSTALL 'motherduck'")
  DBI::dbExecute(con, "LOAD 'motherduck'")
  DBI::dbExecute(con, "ATTACH 'md:'")

  dbName <- strsplit(location, ":", fixed = TRUE)[[1]][2]
  if (create) {
    if (dbName %in% DBI::dbGetQuery(con, "SHOW DATABASES")$database_name) {
      if (overwrite) {
        DBI::dbExecute(
          con,
          glue::glue_sql(.con = con, "DROP DATABASE {`dbName`}")
        )
      } else {
        stop("Database already exists: ", dbName)
      }
    }
    DBI::dbExecute(
      con,
      glue::glue_sql(.con = con, "CREATE DATABASE {`dbName`}")
    )
  }

  DBI::dbExecute(con, glue::glue_sql(.con = con, "USE {`dbName`}"))
  con
}
