ragnar_store_create_v1 <- function(
  location = ":memory:",
  embed = embed_ollama(),
  embedding_size = ncol(embed("foo")),
  overwrite = FALSE,
  ...,
  extra_cols = NULL,
  name = NULL,
  title = NULL,
  version = 2
) {
  rlang::check_dots_empty()

  if (is.null(name)) {
    name <- unique_store_name()
  }
  stopifnot(grepl("^[a-zA-Z0-9_-]+$", name))
  check_string(title, allow_null = TRUE)

  if (is_motherduck_location(location)) {
    con <- motherduck_connection(location, create = TRUE, overwrite)
  } else {
    if (
      any(file.exists(c(location, location.wal <- paste0(location, ".wal"))))
    ) {
      if (overwrite) {
        unlink(c(location, location.wal), force = TRUE)
      } else {
        stop("File already exists: ", location)
      }
    }

    con <- dbConnect(duckdb::duckdb(), dbdir = location, array = "matrix")
  }

  default_schema <- vctrs::vec_ptype(data_frame(
    origin = character(0),
    hash = character(0),
    text = character(0)
  ))

  if (is.null(embed)) {
    embedding_size <- NULL
  } else {
    embed <- process_embed_func(embed)
    check_number_whole(embedding_size, min = 0)
    embedding_size <- as.integer(embedding_size)

    default_schema$embedding <- matrix(
      numeric(0),
      nrow = 0,
      ncol = embedding_size
    )
  }

  if (is.null(extra_cols)) {
    schema <- default_schema
  } else {
    stopifnot(
      is.data.frame(extra_cols)
    )

    schema <- vctrs::vec_ptype(extra_cols)

    # schema can't contain the default schema with different types.
    # It's fine if it doesn't contain all the columns from the default schema,
    # in this case we just add them.
    cols <- names(schema)

    if ("chunk_id" %in% cols) {
      cli::cli_abort(
        "{.arg schema} must not contain a column called {.arg chunk_id}"
      )
    }

    for (nm in names(default_schema)) {
      if (nm %in% cols) {
        stopifnot(
          identical(schema[[nm]], default_schema[[nm]])
        )
      } else {
        schema[[nm]] <- default_schema[[nm]]
      }
    }
  }

  metadata <- tibble::tibble(
    embedding_size,
    embed_func = blob::blob(serialize(embed, NULL)),
    schema = blob::blob(serialize(schema, NULL)),
    name = name,
    title = title
  )

  if (overwrite) {
    dbExecute(
      con,
      glue::trim(
        "
      DROP TABLE IF EXISTS metadata;
      DROP TABLE IF EXISTS chunks;
      DROP SEQUENCE IF EXISTS chunk_id_sequence;
      "
      )
    )
  }

  dbWriteTable(con, "metadata", metadata)

  # read back in embed, so any problems with an R function that doesn't serialize
  # correctly flush out early.
  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  schema <- unserialize(metadata$schema[[1]])
  name <- metadata$name

  # attach function to externalptr, so we can retrieve it from just the connection.
  ptr <- con@conn_ref
  attr(ptr, "embed_function") <- embed

  columns <- stri_c(
    paste(DBI::dbQuoteIdentifier(con, names(schema)), dbDataType2(con, schema)),
    collapse = ",\n  "
  )

  dbExecute(
    con,
    glue(
      "
    CREATE SEQUENCE chunk_id_sequence START 1;
    CREATE TABLE chunks (
      chunk_id INTEGER DEFAULT nextval('chunk_id_sequence'),
      {columns}
    )"
    )
  )

  DuckDBRagnarStore(
    embed = embed,
    schema = schema,
    con = con,
    name = name,
    title = title,
    version = 1L
  )
}


ragnar_store_update_v1 <- function(store, chunks) {
  # ?? swap arg order? piping in df will be more common...
  # -- can do df |> ragnar_store_insert(store = store)
  if (!S7_inherits(store, RagnarStore)) {
    stop("store must be a RagnarStore")
  }

  if (!"origin" %in% names(chunks) || !"hash" %in% names(chunks)) {
    cli::cli_abort(c(
      "{.arg chunks} must have {.code origin} and {.code hash} column, got {.val {names(chunks)}}.",
      i = "Use {.code ragnar_store_insert()} to insert chunks without origin and hash."
    ))
  }

  # Before computing the embeddings, and inserting we want make sure we check
  # if the the document is already in the store. If it's, we want to make sure
  # it really changed.
  # If the embedding is already computed this will be handled by the INSERT INTO
  # statement that handles conflicts.

  tryCatch(
    {
      # Insert the new chunks into a temporary table
      DBI::dbWriteTable(
        store@con,
        "tmp_chunks",
        chunks |> dplyr::select("origin", "hash") |> dplyr::distinct(),
        temporary = TRUE,
        overwrite = TRUE
      )

      # We want to insert into the chunks table all chunks whose origin and hash
      # are not already in the chunks table.
      chunks_to_insert <- DBI::dbGetQuery(
        store@con,
        "SELECT * FROM tmp_chunks
      EXCEPT
      SELECT DISTINCT origin, hash FROM chunks"
      )

      # Only leave the chunks that will be inserted.
      chunks <- dplyr::left_join(
        chunks_to_insert,
        chunks,
        by = c("origin", "hash")
      )
      # We've already done everything we needed, we can simply throw out the transaction.
      dbExecute(store@con, "DROP TABLE tmp_chunks;")
    },
    error = function(e) {
      cli::cli_abort("Failed to filter chunks to insert", parent = e)
    }
  )

  if (!nrow(chunks)) {
    return(invisible(store))
  }

  dbExecute(store@con, "BEGIN TRANSACTION;")
  tryCatch(
    {
      # Remove rows that have the same origin as those that will be included
      origins <- DBI::dbQuoteString(store@con, unique(chunks$origin)) |>
        stri_c(collapse = ", ")
      dbExecute(
        store@con,
        glue("DELETE FROM chunks WHERE origin IN ({origins})")
      )

      # Insert the new chunks into the store
      ragnar_store_insert(store, chunks)

      # Finally commit
      dbExecute(store@con, "COMMIT;")
    },
    error = function(e) {
      dbExecute(store@con, "ROLLBACK;")
      cli::cli_abort("Failed to update the store", parent = e)
    }
  )
  invisible(store)
}


ragnar_store_insert_v1 <- function(store, chunks) {
  if (store@version == 2L) {
    return(ragnar_store_insert_v2(store, chunks))
  }

  # ?? swap arg order? piping in df will be more common...
  # -- can do df |> ragnar_store_insert(store = store)
  if (!S7_inherits(store, RagnarStore)) {
    stop("store must be a RagnarStore")
  }

  if (is.character(chunks)) {
    chunks <- data_frame(text = chunks)
  }

  if (!nrow(chunks)) {
    # No chunks, just return them
    return(invisible(store))
  }

  if (S7::S7_inherits(chunks, MarkdownDocumentChunks)) {
    chunks <- chunks |>
      dplyr::select(dplyr::any_of(names(store@schema)))
    if (is.null(chunks[["origin"]])) {
      chunks$origin <- chunks@document@origin
      chunks$hash <- rlang::hash(as.character(chunks@document))
    }
    # unclass to avoid possible unwanted interactions between
    # the S7 class and DBI
    chunks <- as_bare_df(chunks)
  }

  if (is.null(chunks[["origin"]])) {
    chunks$origin <- NA_character_
  }

  if (is.null(chunks[["hash"]])) {
    chunks$hash <- vapply(chunks$text, rlang::hash, character(1))
  }

  stopifnot(
    is.data.frame(chunks),
    is.character(chunks$text),
    is.character(chunks$origin),
    is.character(chunks$hash)
  )

  if (!is.null(store@embed) && !"embedding" %in% names(chunks)) {
    chunks$embedding <- store@embed(chunks$text)
    stopifnot(
      is.matrix(chunks$embedding)
      # ncol(df$embedding) == store@embedding_size
    )
  }

  # Validate that chunks share ptype with schema
  # Its NOT OK for chunks to miss columns that don't match the schema
  schema <- store@schema
  if (!all(names(schema) %in% names(chunks))) {
    cli::cli_abort(c(
      "Columns in chunks do not match schema",
      x = "Missing columns: {.val {setdiff(names(schema), names(chunks))}}"
    ))
  }

  chunks <- chunks |>
    dplyr::select(dplyr::any_of(names(schema))) |>
    vctrs::vec_cast(store@schema)

  dbAppendTable(
    store@con,
    "chunks",
    chunks
  )

  invisible(store)
}


ragnar_store_build_index_v1 <- function(store, type = c("vss", "fts")) {
  if (S7_inherits(store, DuckDBRagnarStore)) {
    con <- store@con
  } else if (methods::is(store, "DBIConnection")) {
    con <- store
  } else {
    stop("`store` must be a RagnarStore")
  }

  if ("vss" %in% type && !is.null(store@embed)) {
    # MotherDuck currently does not support VSS embedding, so we'll skip building
    # the VSS index with a warning.
    if (is_motherduck_con(store@con)) {
      warning("MotherDuck does not support building VSS index. Skipping.")
    } else {
      # TODO: duckdb has support for three different distance metrics that can be
      # selected when building the index: l2sq, cosine, and ip. Expose these as options
      # in the R interface. https://duckdb.org/docs/stable/core_extensions/vss#usage
      dbExecute(con, "INSTALL vss;")
      dbExecute(con, "LOAD vss;")
      dbExecute(
        con,
        paste(
          "SET hnsw_enable_experimental_persistence = true;",
          "DROP INDEX IF EXISTS my_hnsw_index;",
          "CREATE INDEX my_hnsw_index ON chunks USING HNSW (embedding);"
        )
      )
    }
  }

  if ("fts" %in% type) {
    dbExecute(con, "INSTALL fts;")
    dbExecute(con, "LOAD fts;")
    # fts index builder takes many options, e.g., stemmer, stopwords, etc.
    # Expose a way to pass along args. https://duckdb.org/docs/stable/core_extensions/full_text_search
    dbExecute(
      con,
      "PRAGMA create_fts_index('chunks', 'chunk_id', 'text', overwrite = 1);"
    )
  }

  invisible(store)
}

is_motherduck_con <- function(con) {
  loaded <- DBI::dbGetQuery(
    con,
    "SELECT extension_name, loaded FROM duckdb_extensions() WHERE extension_name='motherduck' and loaded=TRUE"
  )
  nrow(loaded) > 0
}

dbDataType2 <- function(con, x) {
  dataTypes <- DBI::dbDataType(con, x)
  for (nm in names(x)) {
    if (nm == "embedding") {
      # The embedding column must be FLOAT not DOUBLE as inferred
      # by default.
      dataTypes[[nm]] <- paste0("FLOAT[", ncol(x[[nm]]), "]")
    } else if (is.matrix(x[[nm]])) {
      dataTypes[[nm]] <- paste0(dataTypes[[nm]], "[", ncol(x[[nm]]), "]")
    }
  }
  dataTypes
}
