check_store_overwrite <- function(location, overwrite) {
  check_bool(overwrite)
  if (location == ":memory:") {
    return()
  }

  paths <- c(location, location.wal <- paste0(location, ".wal"))
  if (any(file.exists(paths))) {
    if (overwrite) {
      unlink(c(location, location.wal), force = TRUE)
    } else {
      stop("File already exists: ", location)
    }
  }
}



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

  conn <- dbConnect(duckdb::duckdb(), dbdir = location, array = "matrix")

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
  dbWriteTable(conn, "metadata", metadata)

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
      DROP TABLE IF EXISTS documents CASCADE;
      DROP TABLE IF EXISTS embeddings CASCADE;
      DROP SEQUENCE IF EXISTS chunk_id_seq CASCADE;
      DROP SEQUENCE IF EXISTS doc_id_seq CASCADE;
      )--"
    )
  )

  if (length(extra_cols)) {
    extra_cols <- DBI::dbDataType(conn, extra_cols)
    extra_cols <- paste0(
      dbQuoteIdentifier(conn, names(extra_cols)),
      " ",
      extra_cols,
      ",",
      collapse = "\n"
    )
  } else {
    extra_cols <- ""
  }

  dbExecute(
    conn,
    glue(
      r"--(

      CREATE SEQUENCE doc_id_seq START 1;
      CREATE SEQUENCE chunk_id_seq START 1; -- need a unique id for fts

      CREATE OR REPLACE TABLE documents (
        doc_id INTEGER DEFAULT nextval('doc_id_seq'), -- PRIMARY KEY
        -- hash AS (hash(text)),
        origin VARCHAR,                               -- UNIQUE
        text VARCHAR,
      );

      CREATE OR REPLACE TABLE embeddings (
        doc_id INTEGER,
        chunk_id INTEGER DEFAULT nextval('chunk_id_seq'),
        doc_chunk_idx INTEGER,
        doc_char_start_idx INTEGER,
        doc_char_end_idx INTEGER,
        headings VARCHAR,
        {extra_cols}
        embedding FLOAT[{embedding_size}]
      );

      CREATE OR REPLACE VIEW chunks AS (
        SELECT
          d.origin,
          e.*,
          d.text[ e.doc_char_start_idx : e.doc_char_end_idx ] as text
        FROM
          documents d
        JOIN
          embeddings e
        USING
          (doc_id)
      );
      )--"
    )
  )

  DuckDBRagnarStore(
    embed = embed,
    # schema = schema,
    .con = conn,
    name = name
    # title = title
  )
}

ragnar_store_build_index_v2 <- function(store, type = c("vss", "fts")) {
  if (S7_inherits(store, DuckDBRagnarStore)) {
    conn <- store@.con
  } else if (methods::is(store, "DBIConnection")) {
    conn <- store
  } else {
    stop("`store` must be a RagnarStore")
  }

  if ("vss" %in% type && !is.null(store@embed)) {
    # TODO: duckdb has support for three different distance metrics that can be
    # selected when building the index: l2sq, cosine, and ip. Expose these as options
    # in the R interface. https://duckdb.org/docs/stable/core_extensions/vss#usage
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
  # )--"
  # )
  # dbExecute(
  #   conn,
  #   r"--(

  invisible(store)
}

#' Workhorse that inserts a document and its chunks into the store.
#' Relies on lazy evaluation to not evaluate `chunks` unless needed. 
ragnar_store_update_document_and_chunks <- function(store, document, chunks) {
  stopifnot(is.data.frame(document) && nrow(document) == 1L)
  # we don't typecheck chunks until later because we rely on lazy evaluation to not
  # evaluate chunks if it's not needed.
  
  on.exit({
    duckdb::duckdb_unregister(conn, "_ragnar_insert_temp_view")
  })
  
  conn <- store@.con

  already_present <- dbGetQuery(
    conn,
    "SELECT hash(text) as hash, doc_id FROM documents WHERE origin = ?",
    list(document$origin)
  )

  if (nrow(already_present)) {
    duckdb::duckdb_register(
      conn,
      "_ragnar_insert_temp_view",
      document,
      overwrite = TRUE
    )
    to_insert <- dbGetQuery(
      conn,
      "SELECT hash(text) AS hash FROM _ragnar_insert_temp_view"
    )
    if (identical(to_insert$hash, already_present$hash)) {
      return(invisible(store))
    }
    document$doc_id <- already_present$doc_id
  }

  # Now we are sure we're inserting this document, so we force chunks
  stopifnot(is.data.frame(chunks) && 
    all(c("origin", "doc_text", "chunk_start", "chunk_end", "chunk_headings", 
"chunk_text") %in% names(chunks)))
  
  # Get embeddings
  embeddings <- chunks |>
      dplyr::mutate(text = chunk_text) |> 
      store@embed() |>
      mutate(
        doc_chunk_idx = dplyr::row_number(),
        headings = map_chr(chunk_headings, \(x) paste0(x, collapse = "\n")),
      ) |>
      select(
        doc_chunk_idx,
        doc_char_start_idx = chunk_start,
        doc_char_end_idx = chunk_end,
        headings,
        embedding
      )
  
  # Update the database in a single transaction
  DBI::dbWithTransaction(conn, {
    
    # Delete doc ids if we are replacing
    if (nrow(already_present)) {
      doc_id <- already_present$doc_id
      dbExecute(
        conn,
        glue(
          # TODO: use UPDATE statement instead of DELETE ???
          "
          DELETE FROM documents WHERE doc_id = {doc_id};
          DELETE FROM embeddings WHERE doc_id = {doc_id};
          "
        )
      )
    }

    DBI::dbAppendTable(conn, "documents", document)
    # can optionally hand-write INSERT ... RETURNING doc_id
    embeddings$doc_id <- document[["doc_id"]] %||%
      as.integer(dbGetQuery(
        conn,
        "SELECT currval('doc_id_seq') AS doc_id"
      ))
    DBI::dbAppendTable(conn, "embeddings", embeddings)

  })

  invisible(store)
}

ragnar_store_update_documents <- function(store, documents) {
  stopifnot(is.data.frame(documents) && all(
    c("origin", "text") %in% names(documents)
  ))

  for (i in seq_len(nrow(documents))) {
    document <- documents[i,,drop=FALSE]
    ragnar_store_update_document_and_chunks(store, document, ragnar_chunk(document))
  }

  invisible(store)
}

ragnar_store_update_chunks <- function(store, chunks) {
  stopifnot(is.data.frame(chunks) && all(
    c("origin", "doc_text", "chunk_start", "chunk_end", "chunk_headings", 
      "chunk_text") %in% names(chunks)
  ))

  chunks <- dplyr::distinct(chunks)

  documents <- chunks |> 
    dplyr::select(origin, text = doc_text) |> 
    dplyr::distinct()

  for (i in seq_len(nrow(documents))) {
    document <- documents[i,,drop=FALSE]
    ragnar_store_update_document_and_chunks(
      store, 
      document, 
      # keeping this here is slightly more efficient because we don't
      # evaluate if the document already exists in the DB.
      chunks[chunks$origin == document$origin,,drop=FALSE]
    )
  }

  invisible(store)
}

# ragnar_store_from <- function(paths)
ragnar_store_ingest <- function(store, paths, ...) {
  # cli::cli_progress_bar("Cleaning data", total = length(paths))
  # for (origin in paths) {
  for (i in seq_along(paths)) {
    origin <- paths[i]
    message(sprintf("[% 3i/%i] ingesting: %s", i, length(paths), origin))
    document <- tibble::tibble(origin = origin, text = read_as_markdown(origin, ...))
    ragnar_store_update_document_and_chunks(store, document, ragnar_chunk(document))
    # cli::cli_progress_update()
  }
  # cli::cli_progress_done()
  invisible(store)
}

ragnar_store_connect_v2 <- function(location, read_only = TRUE) {
  conn <- dbConnect(
    duckdb::duckdb(),
    dbdir = location,
    read_only = read_only,
    array = "matrix"
  )
  dbExecute(conn, "INSTALL fts; INSTALL vss;")
  dbExecute(conn, "LOAD fts; LOAD vss;")
  metadata <- dbReadTable(conn, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  # schema <- unserialize(metadata$schema[[1L]])
  name <- metadata$name
  title <- metadata$title

  ptr <- conn@conn_ref
  attr(ptr, "embed_function") <- embed

  DuckDBRagnarStore(
    embed = embed,
    # schema = schema,
    .con = conn,
    name = name,
    title = metadata$title
  )
}

ragnar_retrieve_vss_v2 <- function(
  store,
  query,
  top_k = 3L,
  method = "cosine_distance"
) {
  conn <- store@.con
  retrieve_df <- data_frame(
    query = query,
    embedding = store@embed(query)
  )
  tmp_name <- "_ragnar_retrieve_query"
  on.exit(duckdb::duckdb_unregister(conn, tmp_name))
  duckdb::duckdb_register(conn, tmp_name, retrieve_df, overwrite = TRUE)

  top_k <- as.integer(top_k)
  method <- "cosine_distance"
  .[method_func, order_key_direction] <- method_to_info(method)
  dbGetQuery(
    conn,
    glue(
      "
      SELECT
        *,
        '{method}' as metric_name,
        {method_func}(
           embedding,
           (SELECT embedding FROM _ragnar_retrieve_query)
        ) as metric_value
       FROM chunks
       ORDER BY metric_value {order_key_direction}
       LIMIT {top_k};
       "
    )
  )
}


ragnar_retrieve_bm25_v2 <- function(store, text, top_k = 3L) {
  conn <- store@.con
  dbGetQuery(
    conn,
    glue(
      "
      SELECT
        *,
        'bm25' as metric_name,
        fts_main_chunks.match_bm25( chunk_id, ?) as metric_value
      FROM chunks
      WHERE metric_value IS NOT NULL
      ORDER BY metric_value
      LIMIT {top_k};
      "
    ),
    params = list(text)
  )
}

ragnar_store_from <- function(
  sources,
  embed = embed_ollama(model = "snowflake-arctic-embed2:568m"),
  location = ":memory:",
  ...
) {
  sources <- unique(sources)
  store <- ragnar_store_create_v2(embed = embed, location = location)
  ragnar_store_ingest(store, sources, ...)
  ragnar_store_build_index_v2(store)
  if (location != ":memory:") {
    dbDisconnect(store@.con)
    store <- ragnar_store_connect_v2(location, read_only = TRUE)
  }
  store
}


ragnar_deoverlap <- chunks_deoverlap <- function(store, chunks) {
  deoverlapped <- chunks |>
    group_by(doc_id) |>
    arrange(doc_chunk_idx, .by_group = TRUE) |>
    mutate(
      overlap_grp = cumsum(
        doc_char_start_idx > lag(doc_char_end_idx, default = -1L)
      )
    ) |>
    group_by(doc_id, overlap_grp) |>
    summarise(
      origin = first(origin),
      chunk_ids = list(chunk_id),
      doc_chunk_idxs = list(doc_chunk_idx),
      doc_char_start_idx = min(doc_char_start_idx),
      doc_char_end_idx = max(doc_char_end_idx),
      headings = first(headings),
      .groups = "drop"
    ) |>
    mutate(tmp_chunk_id = row_number())

  tmp_ <- deoverlapped |>
    select(doc_id, doc_char_start_idx, doc_char_end_idx, tmp_chunk_id)

  conn <- store@.con
  tmp_name <- "_ragnar_tmp_rechunk"
  on.exit(duckdb::duckdb_unregister(conn, tmp_name))
  duckdb::duckdb_register(conn, tmp_name, tmp_, overwrite = TRUE)

  new_text <- dbGetQuery(
    conn,
    glue(
      "
      SELECT
        c.tmp_chunk_id,
        d.text[ c.doc_char_start_idx : c.doc_char_end_idx ] AS text
      FROM {tmp_name} c
      JOIN documents d
      USING (doc_id)
      "
    )
  )

  deoverlapped <- deoverlapped |>
    full_join(
      new_text,
      by = join_by(tmp_chunk_id)
    )
  deoverlapped |>
    select(-overlap_grp, -tmp_chunk_id)
}

pluck_augmented_text <- function(chunks) {
  # c()
  # origin <- chunks$origin
  # headings <- chunks$headings |>
  #   stri_split_lines() |>
  #   lapply(\(x) {
  #     if (nzchar(x)) {
  #       paste0("> ", x, collapse = "\n")
  #     }
  #   })
  origin <- glue("excerpt from: {chunks$origin}")
  headings <- chunks$headings
  about <- as.character(map2(origin, headings, \(o, h) {
    lines <- unlist(stri_split_lines(c(o, h)))
    paste0("> ", lines[nzchar(lines)], collapse = "\n")
  }))
  text <- chunks$text
  glue(
    "
    {about}
    ---
    {text}
    "
  )
}

ragnar_augment <- function(chunks) {
  # TODO:
  # context <- format(chunks$headings... other cols) # as yaml?
  # paste(context, "\n" headings) # fences? quote metadata? or just dump it in context as json?
}


if (FALSE) {
  library(DBI)
  library(stringi)
  library(dplyr, warn.conflicts = FALSE)
  devtools::load_all()

  if (!file.exists("duckdb_docs.ragnar3.duckdb")) {
    paths <- ragnar_find_links("https://duckdb.org/sitemap.html")
    store <- ragnar_store_create_v2(
      location = "duckdb_docs.ragnar3.duckdb",
      embed = embed_ollama(model = "snowflake-arctic-embed2:568m"),
      overwrite = TRUE,
      extra_cols = data.frame(date_accessed = Sys.time())
    )
    ragnar_store_ingest(
      store,
      paths,
      html_extract_selectors = c(
        "main",
        "#main_content_wrap"
      ),
      html_zap_selectors = c(
        "header",
        "footer",
        ".sidenavigation",
        ".searchoverlay",
        "#sidebar"
      )
    )
    ragnar_store_build_index_v2(store)
    dbDisconnect(store@.con)
  }
  store <- ragnar_store_connect_v2(
    "duckdb_docs.ragnar2.duckdb",
    read_only = TRUE
  )

  show <- \(...) cat(..., sep = paste("\n\n*", strrep("~", 80), "*\n"))

  ragnar_retrieve_vss_v2(store, "create a table from json files", top_k = 10) |>
    ragnar_deoverlap(store = store)
  pull(text) |>
    show()
  ragnar_retrieve_bm25_v2(store, "json") |> pull(text) |> show()
  ragnar_retrieve_bm25_v2(store, "json", top_k = 10) |>
    ragnar_deoverlap(store = store)

  chunks <- bind_rows(
    ragnar_retrieve_vss_v2(store, "create a table from json files", top_k = 15),
    ragnar_retrieve_bm25_v2(store, "json", top_k = 15)
  ) |>
    ragnar_deoverlap(store = store)

  rag

  # |>

  origin = "https://quarto.org/docs/computations/r.html"
  md <- read_as_markdown(origin)
  ragnar_chunk_v2(md)
  ragnar_chunk_v2(md)$headings |> head()
  ragnar_chunk_v2(md)$headings |> tail()
}
