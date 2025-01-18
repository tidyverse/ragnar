

#' @export
ragnar_store_connect <- function(loc = ":memory:", read_only = FALSE) {
  if (is(loc, "DBIConnection")) {
    return(loc)
  }

  con <- dbConnect(duckdb::duckdb(), dbdir = loc, read_only = read_only)

  if (!dbExistsTable(con, "chunks")) {

    # Create a sequence for auto-incrementing IDs
    dbExecute(con, "CREATE SEQUENCE IF NOT EXISTS chunks_id_seq START 1;")

    # Create the chunks table with an auto-incrementing ID
    dbExecute(con, glue("CREATE TABLE chunks (
      id INTEGER DEFAULT nextval('chunks_id_seq') PRIMARY KEY,
      embedding FLOAT[{EMBEDDING_SIZE}],
      text VARCHAR
    )"))
  }

  con
}

# TODO: attach this to store based on model on creation, hardcoded for now
# EMBEDDING_SIZE <- 3072L # llama3.2 via ollama
EMBEDDING_SIZE <- 384L # 'all-minilm' via ollama

#' @export
ragnar_store_insert <- function(store, df) {
  store <- ragnar_store_connect(store)

  stopifnot(
    is.data.frame(df),
    is.matrix(df$embedding) && ncol(df$embedding) == EMBEDDING_SIZE,
    is.character(df$text)
  )

  # duckdb-r does not support array columns yet.
  # hand-write the SQL for now
  # hopefully replace all this with a DBI::dbAppendTable() once
  # https://github.com/duckdb/duckdb-r/issues/102 is resolved.
  # TODO: insert in batches?
  rows <- sprintf(
    "(DEFAULT, array_value(%s), %s)",
    df$embedding |> asplit(1) |> map_chr(stri_flatten, ", "),
    DBI::dbQuoteString(store, df$text)
  ) |> paste0(collapse = ",\n")

  stmt <- sprintf("INSERT INTO chunks VALUES \n%s;", rows)
  dbExecute(store, stmt)
  invisible(store)
}


#' @export
ragnar_store_build_index <- function(store) {
  store <- ragnar_store_connect(store)
  dbExecute(store, "INSTALL vss")
  dbExecute(store, "LOAD vss")
  dbExecute(store, paste(
    "DROP INDEX IF EXISTS my_hnsw_index;",
    "CREATE INDEX my_hnsw_index ON chunks USING HNSW (embedding);"
  ))
  invisible(store)
}

