#' Create and connect to a vector store
#'
#' @param location filepath, or `:memory:`
#' @param embed A function that is called with a character vector and returns a
#'   matrix of embeddings. Note this function will be serialized and then
#'   deserialized in new R sessions, so it cannot reference to any objects in
#'   the global or parent environments. Make sure to namespace all function
#'   calls with `::`. If additional R objects must be available in the function,
#'   you can optionally supply a `carrier::crate()` with packaged data.
#'   It can also be `NULL` for stores that don't need to embed their texts, for example,
#'   if only using FTS algorithms such as [ragnar_retrieve_bm25()].
#' @param embedding_size integer
#' @param overwrite logical, what to do if `location` already exists
#' @param ... Unused. Must be empty.
#' @param extra_cols A zero row data frame used to specify additional columns that
#'  should be added to the store. Such columns can be used for adding additional
#'  context when retrieving. See the examples for more information.
#'  [vctrs::vec_cast()] is used to consistently perform type checks and casts
#'  when inserting with [ragnar_store_insert()].
#' @param name A unique name for the store. Must match the `^[a-zA-Z0-9_-]+$`
#'  regex. Used by [ragnar_register_tool_retrieve()] for registering tools.
#'
#' @examples
#' # A store with a dummy embedding
#' store <- ragnar_store_create(
#'   embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
#' )
#' ragnar_store_insert(store, data.frame(text = "hello"))
#'
#' # A store with a schema. When inserting into this store, users need to
#' # provide a `area` column.
#' store <- ragnar_store_create(
#'   embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
#'   extra_cols = data.frame(area = character()),
#' )
#' ragnar_store_insert(store, data.frame(text = "hello", area = "rag"))
#'
#' # If you already have a data.frame with chunks that will be inserted into
#' # the store, you can quickly create a suitable store with:
#' chunks <- data.frame(text = letters, area = "rag")
#' store <- ragnar_store_create(
#'   embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
#'   extra_cols = vctrs::vec_ptype(chunks),
#' )
#' ragnar_store_insert(store, chunks)
#'
#' @returns a `DuckDBRagnarStore` object
#' @export
ragnar_store_create <- function(
  location = ":memory:",
  embed = embed_ollama(),
  embedding_size = ncol(embed("foo")),
  overwrite = FALSE,
  ...,
  extra_cols = NULL,
  name = NULL
) {
  rlang::check_dots_empty()

  if (is.null(name)) {
    name <- unique_store_name()
  }
  stopifnot(grepl("^[a-zA-Z0-9_-]+$", name))

  if (any(file.exists(c(location, location.wal <- paste0(location, ".wal"))))) {
    if (overwrite) {
      unlink(c(location, location.wal), force = TRUE)
    } else {
      stop("File already exists: ", location)
    }
  }
  con <- dbConnect(duckdb::duckdb(), dbdir = location, array = "matrix")

  default_schema <- vctrs::vec_ptype(data_frame(
    origin = character(0),
    hash = character(0),
    text = character(0)
  ))

  if (is.null(embed)) {
    embedding_size <- NULL
  } else {
    check_number_whole(embedding_size, min = 0)
    embedding_size <- as.integer(embedding_size)

    if (!inherits(embed, "crate")) {
      environment(embed) <- baseenv()
      embed <- rlang::zap_srcref(embed)
    }

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

    if ("id" %in% cols) {
      cli::cli_abort("{.arg schema} must not contain a column called {.arg id}")
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
    name = name
  )

  if (overwrite)
    dbExecute(
      con,
      glue::trim(
        "
      DROP TABLE IF EXISTS metadata;
      DROP TABLE IF EXISTS chunks;
      DROP SEQUENCE IF EXISTS id_sequence;
      "
      )
    )

  dbWriteTable(con, "metadata", metadata)

  # read back in embed, so any problems with an R function that doesn't serialize
  # correctly flush out early.
  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  schema <- unserialize(metadata$schema[[1]])
  name <- metadata$name

  # attach function to externalptr, so we can retreive it from just the connection.
  ptr <- con@conn_ref
  attr(ptr, "embed_function") <- embed

  # duckdb R interface does not support array columns yet,
  # so we hand-write the sql.
  columns <- map2(names(schema), schema, function(nm, type) {
    # TODO add support for more data types!
    dbtype <- if (is.character(type)) {
      "VARCHAR"
    } else if (is.matrix(type) && is.integer(type)) {
      glue::glue("INTEGER[{ncol(type)}]")
    } else if (is.matrix(type) && is.double(type)) {
      glue::glue("FLOAT[{ncol(type)}]")
    } else if (is.integer(type)) {
      "INTEGER"
    } else if (is.double(type)) {
      "FLOAT"
    } else {
      cli::cli_abort(
        "Unexpected type for column {.val {nm}}: {.cls {class(type)}} / {.cls {typeof(type)}}"
      )
    }

    glue("{nm} {dbtype}")
  })

  dbExecute(
    con,
    glue(
      "
    CREATE SEQUENCE id_sequence START 1;
    CREATE TABLE chunks (
      id INTEGER DEFAULT nextval('id_sequence'),
      {stri_c(columns, collapse = ',')}
    )"
    )
  )

  DuckDBRagnarStore(embed = embed, schema = schema, .con = con, name = name)
}

unique_store_name <- function() {
  the$current_store_id <- (the$current_store_id %||% 0) + 1
  sprintf("store_%03d", the$current_store_id)
}

#' Connect to `RagnarStore`
#'
#' @param location string, a filepath location.
#' @param ... unused; must be empty.
#' @param read_only logical, whether the returned connection can be used to
#'   modify the store.
#' @param build_index logical, whether to call `ragnar_store_build_index()` when
#'   creating the connection
#'
#' @returns a `RagnarStore` object.
#' @export
#'
#' @rdname rangar_store_create
ragnar_store_connect <- function(
  location = ":memory:",
  ...,
  read_only = FALSE,
  build_index = FALSE
) {
  check_dots_empty()
  # mode = c("retrieve", "insert")
  # mode <- match.arg(mode)
  # read_only <- mode == "retrieve"

  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = location,
    read_only = read_only,
    array = "matrix"
  )

  # can't use dbExistsTable() because internally it runs:
  # > dbGetQuery(conn, sqlInterpolate(conn, "SELECT * FROM ? WHERE FALSE", dbQuoteIdentifier(conn, name)))
  # which fails with:
  # > Error in dbSendQuery(conn, statement, ...) :
  # >  rapi_prepare: Unknown column type for prepare: FLOAT[384]
  if (!all(c("chunks", "metadata") %in% dbListTables(con))) {
    stop("Store must be created with ragnar_store_create()")
  }
  dbExecute(con, "INSTALL fts; INSTALL vss;")
  dbExecute(con, "LOAD fts; LOAD vss;")

  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  schema <- unserialize(metadata$schema[[1L]])
  name <- metadata$name %||% unique_store_name()

  # attach function to externalptr, so we can retreive it from just the connection.
  ptr <- con@conn_ref
  attr(ptr, "embed_function") <- embed

  if (build_index) ragnar_store_build_index(con)

  DuckDBRagnarStore(embed = embed, schema = schema, .con = con, name = name)
}

#' Inserts or updates chunks in a `RagnarStore`
#'
#' @inheritParams ragnar_store_insert
#' @details
#' `chunks` must be a data frame containing `origin` and `hash` columns.
#' We first filter out chunks for which `origin` and `hash` are already in the store.
#' If an `origin` is in the store, but with a different `hash`, we all of its chunks
#' with the new chunks. Otherwise, a regular insert is performed.
#'
#' This can help spending less time computing embeddings for chunks that are already in the store.
#'
#' @returns `store`, invisibly.
#' @export
ragnar_store_update <- function(store, chunks) {
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
        store@.con,
        "tmp_chunks",
        chunks |> dplyr::select("origin", "hash") |> dplyr::distinct(),
        temporary = TRUE,
        overwrite = TRUE
      )

      # We want to insert into the chunks table all chunks whose origin and hash
      # are not already in the chunks table.
      chunks_to_insert <- DBI::dbGetQuery(
        store@.con,
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
      dbExecute(store@.con, "DROP TABLE tmp_chunks;")
    },
    error = function(e) {
      cli::cli_abort("Failed to filter chunks to insert", parent = e)
    }
  )

  if (!nrow(chunks)) {
    return(invisible(store))
  }

  dbExecute(store@.con, "BEGIN TRANSACTION;")
  tryCatch(
    {
      # Remove rows that have the same origin as those that will be included
      origins <- DBI::dbQuoteString(store@.con, unique(chunks$origin)) |>
        stri_c(collapse = ", ")
      dbExecute(
        store@.con,
        glue("DELETE FROM chunks WHERE origin IN ({origins})")
      )

      # Insert the new chunks into the store
      ragnar_store_insert(store, chunks)

      # Finally commit
      dbExecute(store@.con, "COMMIT;")
    },
    error = function(e) {
      dbExecute(store@.con, "ROLLBACK;")
      cli::cli_abort("Failed to update the store", parent = e)
    }
  )
  invisible(store)
}

#' Insert chunks into a `RagnarStore`
#'
#' @param store a `RagnarStore` object
#' @param chunks a character vector or a dataframe with a `text` column, and
#'   optionally, a pre-computed `embedding` matrix column. If `embedding` is not
#'   present, then `store@embed()` is used. `chunks` can also be a character
#'   vector.
#' @returns `store`, invisibly.
#' @export
ragnar_store_insert <- function(store, chunks) {
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

  if (is.null(chunks$origin)) {
    chunks$origin <- NA_character_
  }

  if (is.null(chunks$hash)) {
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

  # Ideally this would use dbWriteTable, but we can't really because it currently
  # doesn't support array columns.
  cols <- imap(schema, function(ptype, name) {
    # Ensures that the column in chunks has the expected ptype. (or at least
    # something that can be cast to the correct ptype with no loss)
    col <- vctrs::vec_cast(
      chunks[[name]],
      ptype,
      x_arg = glue::glue("chunks${name}")
    )

    if (is.matrix(col) && is.numeric(col)) {
      stri_c(
        "array_value(",
        col |> asplit(1) |> map_chr(stri_flatten, ", "),
        ")"
      )
    } else if (is.character(col)) {
      DBI::dbQuoteString(store@.con, col)
    } else if (is.numeric(col)) {
      DBI::dbQuoteLiteral(store@.con, col)
    } else {
      cli::cli_abort("Unsupported type {.cls {class(col)}}")
    }
  })

  rows <- stri_c("(", do.call(\(...) stri_c(..., sep = ","), cols), ")")
  rows <- stri_c(rows, collapse = ",\n")

  insert_statement <- sprintf(
    "INSERT INTO chunks (%s) VALUES \n%s;",
    stri_c(names(schema), collapse = ", "),
    rows
  )

  dbExecute(store@.con, insert_statement)

  invisible(store)
}

#' Build a Ragnar Store index
#'
#' A search index must be built before calling `ragnar_retrieve()`. If
#' additional entries are added to the store with `ragnar_store_insert()`,
#' `ragnar_store_build_index()` must be called again to rebuild the index.
#'
#' @param store a `RagnarStore` object
#' @param type The retrieval search type to build an index for.
#'
#' @returns `store`, invisibly.
#' @export
ragnar_store_build_index <- function(store, type = c("vss", "fts")) {
  if (S7_inherits(store, DuckDBRagnarStore)) con <- store@.con else if (
    methods::is(store, "DBIConnection")
  )
    con <- store else stop("`store` must be a RagnarStore")

  if ("vss" %in% type && !is.null(store@embed)) {
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

  if ("fts" %in% type) {
    dbExecute(con, "INSTALL fts;")
    dbExecute(con, "LOAD fts;")
    # fts index builder takes many options, e.g., stemmer, stopwords, etc.
    # Expose a way to pass along args. https://duckdb.org/docs/stable/core_extensions/full_text_search
    dbExecute(
      con,
      "PRAGMA create_fts_index('chunks', 'id', 'text', overwrite = 1);"
    )
  }

  invisible(store)
}

# @export
RagnarStore <- new_class(
  "RagnarStore",
  properties = list(
    embed = S7::new_union(class_function, NULL),
    schema = class_data.frame,
    name = class_character
  ),
  abstract = TRUE
)

DuckDBRagnarStore <- new_class(
  "DuckDBRagnarStore",
  RagnarStore,
  properties = list(
    .con = methods::getClass("DBIConnection")
  )
)

#' Launches the Ragnar Inspector Tool
#'
#' @param store A `RagnarStore` object that you want to inspect with the tool.
#' @param ... Passed to [shiny::runApp()].
#'
#' @returns `NULL` invisibly
#'
#' @export
ragnar_store_inspect <- function(store, ...) {
  rlang::check_installed("shiny")
  app_dir <- system.file("store-inspector", package = "ragnar")
  withr::with_options(list(ragnar_inspector_store = store), {
    shiny::runApp(app_dir, ...)
  })
  invisible(NULL)
}

#' @importFrom dplyr tbl sql arrange collect
method(tbl, ragnar:::DuckDBRagnarStore) <- function(src, from = "chunks", ...) {
  tbl(src@.con, from)
}
rm(tbl)
