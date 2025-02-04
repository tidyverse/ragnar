
#' Create and connect to a vector store
#'
#' @param location filepath, or `:memory:`
#' @param embed A function that is called with a character vector and returns a matrix of embeddings
#' @param embedding_size integer
#' @param overwrite logical, what to do if `location` already exists
#'
#' @returns a `DuckDBRagnarStore` object, invisibly
#' @export
ragnar_store_create <- function(
    location = ":memory:",
    embed = embed_ollama(),
    embedding_size = ncol(embed("foo")),
    overwrite = FALSE
) {

  if (file.exists(location)) {
    if (overwrite) {
      unlink(locations)
    } else {
      stop("File already exists: ", location)
    }
  }
  con <- dbConnect(duckdb::duckdb(), dbdir = location)

  check_number_whole(embedding_size, min = 1)
  embedding_size <- as.integer(embedding_size)

  metadata <- tibble::tibble(
    embedding_size,
    embed_func = blob::blob(serialize(embed, NULL)),
  )
  dbWriteTable(con, "metadata", metadata)

  # duckdb R interface does not support array columns yet,
  # so we hand-write the sql.

  # dbExecute(con, )
  dbExecute(con, glue("
    CREATE SEQUENCE id_sequence START 1;
    CREATE TABLE chunks (
      id INTEGER DEFAULT nextval('id_sequence'),
      embedding FLOAT[{embedding_size}],
      text VARCHAR
    )"))

  invisible(DuckDBRagnarStore(
    .con = con,
    embed = embed,
    embedding_size = embedding_size
  ))
}

#' @export
ragnar_store_connect <- function(location = ":memory:",
                                 ...,
                                 read_only = FALSE,
                                 build_index = read_only) {
  check_dots_empty()
  # mode = c("retreive", "insert")
  # mode <- match.arg(mode)
  # read_only <- mode == "retreive"

  con <- dbConnect(duckdb::duckdb(), dbdir = location, read_only = read_only)

  if (!dbExistsTable(con, "chunks") || !dbExistsTable("metadata")) {
    stop("Store must be created with ragnar_store_create()")
  }

  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  embedding_size <- metadata$embedding_size

  if (build_index)
    ragnar_store_build_index(con)

  DuckDBRagnarStore(embed = embed, embedding_size = embedding_size, .con = con)
}


#' @export
ragnar_store_insert <- function(store, df) {
  if (!S7_inherits(store, RagnarStore)) {
    stop("store must be a RagnarStore")
  }

  stopifnot(
    is.data.frame(df),
    is.character(df$text)
  )

  if(is.null(df$embedding)) {
    df$embedding <- store@embed(df$text)
  }

  stopifnot(
    is.matrix(df$embedding),
    ncol(df$embedding) == store@embedding_size
  )

  # duckdb-r does not support array columns yet.
  # hand-write the SQL for now
  # hopefully replace all this with a DBI::dbAppendTable() once
  # https://github.com/duckdb/duckdb-r/issues/102 is resolved.
  # TODO: insert in batches?
  rows <- sprintf(
    # "(DEFAULT, array_value(%s), %s)",
    "(array_value(%s), %s)",
    df$embedding |> asplit(1) |> map_chr(stri_flatten, ", "),
    DBI::dbQuoteString(store@.con, df$text)
  ) |> paste0(collapse = ",\n")

  # INSERT INTO tbl (s) VALUES ('hello'), ('world');
  stmt <- sprintf("INSERT INTO chunks (embedding, text) VALUES \n%s;", rows)
  dbExecute(store@.con, stmt)
  invisible(store)
}


#' @export
ragnar_store_build_index <- function(store) {
  stopifnot(S7_inherits(store, DuckDBRagnarStore))
  con <- store@.con

  dbExecute(con, "INSTALL vss")
  dbExecute(con, "LOAD vss")
  dbExecute(con, paste(
    "DROP INDEX IF EXISTS my_hnsw_index;",
    "CREATE INDEX my_hnsw_index ON chunks USING HNSW (embedding);"
  ))
  invisible(con)
}

#' @export
RagnarStore <- new_class("RagnarStore",
  properties = list(
    embed = class_function,
    embedding_size = class_integer,
    .con = methods::getClass("DBIConnection")
  ),
  abstract = TRUE
)


DuckDBRagnarStore <- new_class("DuckDBRagnarStore", RagnarStore)
