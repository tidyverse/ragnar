ragnar_store_create_v2 <- function(
  location = ":memory:",
  embed = embed_ollama(model = "snowflake-arctic-embed2:568m"),
  embedding_size = ncol(embed("foo")),
  overwrite = FALSE,
  ...,
  extra_cols = NULL,
  name = NULL,
  title = NULL
) {
  rlang::check_dots_empty()

  check_string(location)
  check_store_overwrite(location, overwrite)
  name <- name %||% unique_store_name()
  check_string(name)
  stopifnot(grepl("^[a-zA-Z0-9_-]+$", name))
  check_string(title, allow_null = TRUE)

  conn <- dbConnect(
    duckdb::duckdb(),
    dbdir = location,
    array = "matrix",
    overwrite = overwrite
  )

  if (is.null(embed)) {
    embedding_size <- NULL
  } else {
    check_number_whole(embedding_size, min = 0)
    embedding_size <- as.integer(embedding_size)

    if (!inherits(embed, "crate")) {
      environment(embed) <- baseenv()
      embed <- rlang::zap_srcref(embed)
    }
  }

  metadata <- data_frame(
    embedding_size = embedding_size,
    embed_func = blob::blob(serialize(embed, NULL)),
    name = name,
    title = title
  )
  dbWriteTable(conn, "metadata", metadata, overwrite = TRUE)

  # read back in embed, so any problems with an R function that doesn't
  # serialize correctly flush out early.
  metadata <- dbReadTable(conn, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  name <- metadata$name

  # attach function to externalptr, so we can retrieve it from just the connection.
  ptr <- conn@conn_ref
  attr(ptr, "embed_function") <- embed

  dbExecute(
    conn,
    glue(
      r"--(
      DROP VIEW IF EXISTS chunks CASCADE;
      DROP TABLE IF EXISTS embeddings CASCADE;
      DROP TABLE IF EXISTS documents CASCADE;
      DROP SEQUENCE IF EXISTS chunk_id_seq CASCADE;
      DROP SEQUENCE IF EXISTS doc_id_seq CASCADE;
      )--"
    )
  )

  if (length(extra_cols)) {
    extra_col_types <- DBI::dbDataType(conn, extra_cols)
    extra_col_names <- dbQuoteIdentifier(conn, names(extra_col_types))
    extra_cols <- paste0(
      extra_col_names,
      " ",
      extra_col_types,
      ",",
      collapse = "\n"
    )
  } else {
    extra_cols <- ""
  }

  dbExecute(
    conn,
    glue(
      "
      CREATE SEQUENCE chunk_id_seq START 1; -- need a unique id for fts

      CREATE OR REPLACE TABLE documents (
        origin VARCHAR PRIMARY KEY,
        text VARCHAR,
      );

      CREATE OR REPLACE TABLE embeddings (
        origin VARCHAR,
        FOREIGN KEY (origin) REFERENCES documents (origin),
        chunk_id INTEGER DEFAULT nextval('chunk_id_seq'),
        start INTEGER,
        end_ INTEGER,
        PRIMARY KEY (origin, start, end_),
        headings VARCHAR,
        {extra_cols}
        embedding FLOAT[{embedding_size}]
      );

      CREATE OR REPLACE VIEW chunks AS (
        SELECT
          d.origin,
          e.*,
          d.text[ e.start : e.end_ ] as text
        FROM
          documents d
        JOIN
          embeddings e
        USING
          (origin)
      );
      "
    )
  )

  DuckDBRagnarStore(
    embed = embed,
    # schema = schema,
    conn = conn,
    name = name,
    title = title,
    version = 2L
  )
}


#
# ragnar_store_connect_v2 <- function(location, read_only = TRUE) {
#   conn <- dbConnect(
#     duckdb::duckdb(),
#     dbdir = location,
#     read_only = read_only,
#     array = "matrix"
#   )
#   dbExecute(conn, "INSTALL fts; INSTALL vss;")
#   dbExecute(conn, "LOAD fts; LOAD vss;")
#   metadata <- dbReadTable(conn, "metadata")
#   embed <- unserialize(metadata$embed_func[[1L]])
#
#   ptr <- conn@conn_ref
#   attr(ptr, "embed_function") <- embed
#
#   DuckDBRagnarStore(
#     embed = embed,
#     conn = conn,
#     name = metadata$name,
#     title = metadata$title,
#     version = 2L
#   )
# }

ragnar_store_build_index_v2 <- function(store, type = c("vss", "fts")) {
  if (S7_inherits(store, DuckDBRagnarStore)) {
    conn <- store@conn
  } else if (methods::is(store, "DBIConnection")) {
    conn <- store
  } else {
    stop("`store` must be a RagnarStore")
  }

  if ("vss" %in% type && !is.null(store@embed)) {
    # TODO: duckdb has support for three different distance metrics that can be
    # selected when building the index: l2sq, cosine, and ip. Expose these as options
    # in the R interface. https://duckdb.org/docs/stable/core_extensions/vss#usage

    # TODO: expose way to select vss index metric types in api
    dbExecute(conn, "INSTALL vss; LOAD vss;")
    dbExecute(
      conn,
      r"--(
      SET hnsw_enable_experimental_persistence = true;

      DROP INDEX IF EXISTS store_hnsw_cosine_index;
      DROP INDEX IF EXISTS store_hnsw_l2sq_index;
      DROP INDEX IF EXISTS store_hnsw_ip_index;

      CREATE INDEX store_hnsw_cosine_index ON embeddings USING HNSW (embedding) WITH (metric = 'cosine');
      CREATE INDEX store_hnsw_l2sq_index   ON embeddings USING HNSW (embedding) WITH (metric = 'l2sq');
      CREATE INDEX store_hnsw_ip_index     ON embeddings USING HNSW (embedding) WITH (metric = 'ip');
      )--"
    )
  }

  if ("fts" %in% type) {
    dbExecute(conn, "INSTALL fts; LOAD fts;")
    # fts index builder takes many options, e.g., stemmer, stopwords, etc.
    # Expose a way to pass along args. https://duckdb.org/docs/stable/core_extensions/full_text_search
    dbWithTransaction(conn, {
      dbExecute(
        conn,
        r"--(
        ALTER VIEW chunks RENAME TO chunks_view;

        CREATE TABLE chunks AS
        SELECT chunk_id, headings, text
        FROM chunks_view;
        )--"
      )
      dbExecute(
        conn,
        r"--(
        PRAGMA create_fts_index(
          'chunks',            -- input_table
          'chunk_id',          -- input_id
          'headings', 'text',  -- *input_values
          overwrite = 1
        );
        )--"
      )
      dbExecute(
        conn,
        r"--(
        DROP TABLE chunks;
        ALTER VIEW chunks_view RENAME TO chunks
        )--"
      )
    })
  }

  invisible(store)
}


ragnar_store_update_v2 <- function(store, chunks) {
  stopifnot(
    store@version == 2,
    is.data.frame(chunks),
    all(c("origin", "document", "start", "end", "headings") %in% names(chunks))
  )

  if ("text" %in% names(chunks)) {
    if (with(chunks, !identical(text, stri_sub(document, start, end)))) {
      stop("modifying chunks$text is not supported with store@version == 2")
    }
  } else {
    chunks <- chunks |> mutate(text = stri_sub(document, start, end))
  }

  conn <- store@conn

  existing <- dbGetQuery(
    conn,
    "SELECT origin, start, end_, headings, text FROM chunks WHERE origin = ?",
    params = list(unique(chunks$origin))
  )

  origins_to_update <- anti_join(
    chunks,
    existing,
    by = join_by(origin, start, end == end_, headings, text)
  ) |>
    _$origin |>
    unique()

  if (!length(origins_to_update)) {
    return(invisible(store))
  }

  chunks <- chunks[chunks$origin %in% origins_to_update, ]

  documents <- distinct(select(chunks, origin, text = document))
  embeddings <- chunks |>
    mutate(embedding = store@embed(stri_c(headings, "\n", text))) |>
    select(origin, start, end_ = end, headings, embedding)

  local_duckdb_register(conn, "documents_to_upsert", documents)

  dbWithTransaction(conn, {
    dbExecute(
      conn,
      "
      INSERT OR REPLACE INTO documents BY NAME
      SELECT origin, text FROM documents_to_upsert;
      "
    )
    dbExecute(
      conn,
      "DELETE FROM embeddings WHERE origin = ?;",
      params = list(origins_to_update)
    )
    dbAppendTable(conn, "embeddings", embeddings)
  })
}


ragnar_store_insert_v2 <- function(store, chunks, replace_existing = FALSE) {
  stopifnot(
    store@version == 2,
    is.data.frame(chunks),
    all(c("origin", "document", "start", "end", "headings") %in% names(chunks))
  )

  if ("text" %in% names(chunks)) {
    if (!identical(text, stri_sub(chunks$document, chunks$start, chunks$end))) {
      stop("modifying chunks$text is not supported with store@version == 2")
    }
  } else {
    chunks <- chunks |> mutate(text = stri_sub(document, start, end))
  }

  if (!"embedding" %in% names(chunks)) {
    chunks <- chunks |>
      mutate(embedding = store@embed(stri_c(headings, "\n", text)))
  }

  conn <- store@conn
  documents <- distinct(select(chunks, origin, text = document))
  embeddings <- chunks |>
    mutate(embedding = store@embed(stri_c(headings, "\n", text))) |>
    select(origin, start, end_ = end, headings, embedding)

  dbWithTransaction(conn, {
    dbAppendTable(conn, "documents", documents)
    dbAppendTable(conn, "embeddings", embeddings)
  })
}
