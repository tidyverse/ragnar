#' Create and connect to a vector store
#'
#' @details
#'
#' ## Store versions
#'
#' **Version 2 – documents with chunk ranges** (default)
#'
#' With `version = 2`, ragnar stores each document once and records the start
#' and end positions of its chunks. This provides strong support for overlapping
#' chunk ranges with de-overlapping at retrieval, and generally allows
#' retrieving arbitrary ranges from source documents, but does not support
#' modifying chunks directly before insertion. Chunks can be augmented via the
#' `context` field and with additional fields passed to `extra_cols`. The
#' easiest way to prepare `chunks` for `version = 2` is with
#' `read_as_markdown()` and `markdown_chunk()`.
#'
#' Incremental ingestion and store synchronization require the current v2
#' format. Recreate older v2 stores with `ragnar_store_create()`; existing
#' stores are not migrated automatically.
#'
#' **Version 1 – flat chunks**
#'
#' With `version = 1`, ragnar keeps all chunks in a single table. This lets you
#' easily modify chunk text before insertion. However, dynamic rechunking
#' (de-overlapping) or extracting arbitrary ranges from source documents is not
#' supported, since the original full documents are no longer available. Chunks
#' can be augmented by modifying the chunk text directly (e.g., with `glue()`).
#' Additionally, if you intend to call `ragnar_store_update()`, it is your
#' responsibility to provide `rlang::hash(original_full_document)` with each
#' chunk. The easiest way to prepare `chunks` for `version = 1` is with
#' `ragnar_read()` and `ragnar_chunk()`.
#'
#' ## Memory use
#'
#' Store connections disable DuckDB's insertion-order preservation to reduce
#' memory use. Use an explicit `ORDER BY` when querying the connection directly
#' if row order matters.
#'
#' DuckDB sets a default `memory_limit` based on available system memory. For
#' deployments with a smaller process memory budget, lower this limit after
#' creating or connecting to a store, for example:
#'
#' ```r
#' DBI::dbExecute(store@con, "SET memory_limit = '4GB'")
#' ```
#'
#' Choose a limit that leaves room for R and other allocations. This setting
#' limits DuckDB's buffer manager, not total process memory; HNSW indexes and
#' some other allocations are outside this limit.
#'
#' @param location filepath, or `:memory:`. Location can also be a database name
#'   specified with `md:dbname`, in this case the database will be created in
#'   MotherDuck after a connection is established.
#' @param embed A function that is called with a character vector and returns a
#'   matrix of embeddings. Note this function will be serialized and then
#'   deserialized in new R sessions, so it cannot reference to any objects in
#'   the global or parent environments. Make sure to namespace all function
#'   calls with `::`. If additional R objects must be available in the function,
#'   you can optionally supply a `carrier::crate()` with packaged data. It can
#'   also be `NULL` for stores that don't need to embed their texts, for
#'   example, if only using FTS algorithms such as [ragnar_retrieve_bm25()].
#' @param embedding_size integer
#' @param overwrite logical, what to do if `location` already exists
#' @param ... Unused. Must be empty.
#' @param extra_cols A zero row data frame used to specify additional columns
#'   that should be added to the store. Such columns can be used for adding
#'   additional context when retrieving. See the examples for more information.
#'   [vctrs::vec_cast()] is used to consistently perform type checks and casts
#'   when inserting with [ragnar_store_insert()].
#' @param name A unique name for the store. Must match the `^[a-zA-Z0-9_-]+$`
#'   regex. Used by [ragnar_register_tool_retrieve()] for registering tools.
#' @param title A title for the store, used by [ragnar_register_tool_retrieve()]
#'   when the store is registered with an [ellmer::Chat] object.
#'
#' @param version integer. The version of the store to create. See details.
#'
#' @returns a `RagnarStore` object
#' @export
#' @examplesIf ragnar:::can_load_duckdb_extensions()
#' # A store with a dummy embedding
#' store <- ragnar_store_create(
#'   embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
#'   version = 1
#' )
#' ragnar_store_insert(store, data.frame(text = "hello"))
#'
#' # A store with a schema. When inserting into this store, users need to
#' # provide an `area` column.
#' store <- ragnar_store_create(
#'   embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
#'   extra_cols = data.frame(area = character()),
#'   version = 1
#' )
#' ragnar_store_insert(store, data.frame(text = "hello", area = "rag"))
#'
#' # If you already have a data.frame with chunks that will be inserted into
#' # the store, you can quickly create a suitable store with `vec_ptype()`:
#' chunks <- data.frame(text = letters, area = "rag")
#' store <- ragnar_store_create(
#'   embed = \(x) matrix(stats::runif(10), nrow = length(x), ncol = 10),
#'   extra_cols = vctrs::vec_ptype(chunks),
#'   version = 1
#' )
#' ragnar_store_insert(store, chunks)
#'
#' # version = 2 (the default) has support for deoverlapping
#' store <- ragnar_store_create(
#'   # if embed = NULL, then only bm25 search is used (not vss)
#'   embed = NULL
#' )
#' doc <- MarkdownDocument(
#'   paste0(letters, collapse = ""),
#'   origin = "/some/where"
#' )
#' chunks <- markdown_chunk(doc, target_size = 3, target_overlap = 2 / 3)
#' chunks$context <- substring(chunks$text, 1, 1)
#' chunks
#' ragnar_store_insert(store, chunks)
#' ragnar_store_build_index(store)
#'
#' ragnar_retrieve(store, "abc bcd xyz", deoverlap = FALSE)
#' ragnar_retrieve(store, "abc bcd xyz", deoverlap = TRUE)
ragnar_store_create <- function(
  location = ":memory:",
  embed = embed_ollama(),
  ...,
  embedding_size = ncol(embed("foo")),
  overwrite = FALSE,
  extra_cols = NULL,
  name = NULL,
  title = NULL,
  version = 2
) {
  check_number_whole(version)
  create <- switch(
    as.integer(version),
    ragnar_store_create_v1,
    ragnar_store_create_v2
  )
  create(
    location = location,
    embed = embed,
    embedding_size = embedding_size,
    overwrite = overwrite,
    ...,
    extra_cols = extra_cols,
    name = name,
    title = title
  )
}

unique_store_name <- function() {
  the$current_store_id <- (the$current_store_id %||% 0) + 1
  sprintf("store_%03d", the$current_store_id)
}

process_embed_func <- function(embed) {
  if (is.null(embed) || inherits(embed, "crate")) {
    return(embed)
  }
  og_embed_env <- environment(embed)
  environment(embed) <- baseenv()
  embed <- rlang::zap_srcref(embed)

  embed_func_names <- grep(
    "^embed_",
    getNamespaceExports("ragnar"),
    value = TRUE
  )

  walker <- function(x) {
    switch(
      typeof(x),
      list = {
        x <- lapply(x, walker)
      },
      language = {
        if (rlang::is_call(x, embed_func_names, ns = c("", "ragnar"))) {
          name <- rlang::call_name(x)
          fn <- get(name)
          ox <- x
          x <- rlang::call_match(x, fn, defaults = FALSE, dots_expand = FALSE)
          x <- as.list(x)

          # ensure 'model' is explicit arg embedded in call
          if (!"model" %in% names(x)) {
            x["model"] <- formals(fn)["model"]
          }

          if (!is.character(x[["model"]])) {
            tryCatch(
              x[["model"]] <- eval(x[["model"]], og_embed_env),
              error = function(e) NULL
            )
          }

          # preserve `...` if they were present in the call (call_match() removes them)
          if (any(map_lgl(as.list(ox), identical, quote(...)))) {
            x <- c(x, quote(...))
          }
          x <- as.call(x)

          # ensure the call is namespaced
          if (is.null(rlang::call_ns(x))) {
            x[[1L]] <- call("::", quote(ragnar), as.symbol(name))
          }
        } else {
          x <- as.call(lapply(x, walker))
        }
      },
      x
    )
    x
  }

  body(embed) <- walker(body(embed))
  embed
}


check_store_overwrite <- function(location, overwrite) {
  check_bool(overwrite)
  if (location == ":memory:" || is_motherduck_location(location)) {
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


#' @param ... unused; must be empty.
#' @param read_only logical, whether the returned connection can be used to
#'   modify the store.
#'
#' @export
#' @rdname ragnar_store_create
ragnar_store_connect <- function(
  location,
  ...,
  read_only = TRUE
) {
  check_dots_empty()

  if (is_motherduck_location(location)) {
    con <- motherduck_connection(location, create = FALSE, overwrite = FALSE)
  } else {
    con <- dbConnect(
      duckdb::duckdb(),
      dbdir = location,
      read_only = read_only,
      array = "matrix"
    )
  }
  dbExecute(con, "SET preserve_insertion_order = false")

  tables <- dbListTables(con)
  if (all(c("documents", "embeddings", "metadata") %in% tables)) {
    version <- 2L
  } else if (all(c("chunks", "metadata") %in% tables)) {
    version <- 1L
  } else {
    stop("Store must be created with ragnar_store_create()")
  }

  if (
    version == 2L &&
      !all(c("last_modified", "content_hash", "ingested_at") %in%
        dbListFields(con, "documents"))
  ) {
    dbDisconnect(con, shutdown = TRUE)
    rlang::abort(
      "This store uses an older v2 format. Recreate it with `ragnar_store_create()`."
    )
  }

  dbExecute(con, "INSTALL fts; INSTALL vss;")
  dbExecute(con, "LOAD fts; LOAD vss;")

  metadata <- dbReadTable(con, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  schema <- switch(version, unserialize(metadata$schema[[1L]]), NULL)
  name <- metadata$name %||% unique_store_name()

  # attach function to externalptr, so we can retrieve it from just the connection.
  ptr <- con@conn_ref
  attr(ptr, "embed_function") <- embed

  DuckDBRagnarStore(
    location = normalizePath(location, winslash = "/", mustWork = FALSE),
    embed = embed,
    schema = schema,
    con = con,
    name = name,
    title = metadata$title,
    version = version
  )
}

#' Inserts or updates chunks in a `RagnarStore`
#'
#' @param store a `RagnarStore` object
#' @param chunks Content to insert or update. The precise input structure
#'   depends on `store@version`. See Details.
#' @details
#'
#' **Store Version 2**
#'
#' `chunks` must be `MarkdownDocumentChunks` object.
#'
#' **Store Version 1**
#'
#' `chunks` must be a data frame containing `origin`, `hash`, and `text`
#' columns. We first filter out chunks for which `origin` and `hash` are already
#' in the store. If an `origin` is in the store, but with a different `hash`, we
#' replace all of its chunks with the new chunks. Otherwise, a regular insert is
#' performed.
#'
#' This can help avoid needing to compute embeddings for chunks that are already
#' in the store.
#'
#' @returns `store`, invisibly.
#' @export
ragnar_store_insert <- function(store, chunks) {
  switch(
    store@version,
    ragnar_store_insert_v1(store, chunks),
    ragnar_store_insert_v2(store, chunks)
  )
  invisible(store)
}

#' @rdname ragnar_store_insert
#' @export
ragnar_store_update <- function(store, chunks) {
  switch(
    store@version,
    ragnar_store_update_v1(store, chunks),
    ragnar_store_update_v2(store, chunks)
  )
  invisible(store)
}

#' Remove stale documents from a Ragnar store
#'
#' `ragnar_store_sync()` deletes any documents whose `origin` is not in `keep`,
#' along with their embeddings. Use `dry_run = TRUE` to
#' preview what would be deleted without modifying the store.
#'
#' @param store A `DuckDBRagnarStore` (version 2 only).
#' @param keep Character vector of origin URLs or file paths to retain.
#'   All other origins currently in the store are deleted.
#' @param dry_run Logical. If `TRUE`, return a summary tibble of what would
#'   be deleted without making any changes. Default `FALSE`.
#' @param rebuild_index Logical. If `TRUE` (default), rebuild search indexes
#'   after deletion via [ragnar_store_build_index()].
#'
#' @returns When `dry_run = FALSE`: `store` invisibly, with a `sync_summary`
#'   attribute -- a tibble with columns `origin` (character) and `action`
#'   (character: `"kept"` or `"deleted"`).
#'   When `dry_run = TRUE`: the summary tibble directly.
#'
#' @export
ragnar_store_sync <- function(
  store,
  keep,
  dry_run       = FALSE,
  rebuild_index = TRUE
) {
  stopifnot(S7::S7_inherits(store, DuckDBRagnarStore))
  if (store@version != 2L) {
    rlang::abort(
      "`ragnar_store_sync()` only supports version 2 stores.",
      call = NULL
    )
  }
  check_character(keep, allow_na = FALSE)
  check_bool(dry_run)
  check_bool(rebuild_index)

  con <- store@con

  # Identify all current origins and classify as kept or deleted
  keep_df <- tibble::tibble(origin = unique(as.character(keep)))
  local_duckdb_register(con, "origins_to_keep", keep_df)

  summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.origin,
      CASE WHEN k.origin IS NOT NULL THEN 'kept' ELSE 'deleted' END AS action
    FROM documents d
    LEFT JOIN origins_to_keep k USING(origin)
    ORDER BY action, d.origin
    "
  )
  summary <- tibble::as_tibble(summary)

  if (dry_run) {
    return(summary)
  }

  to_delete <- summary$origin[summary$action == "deleted"]

  if (length(to_delete)) {
    to_delete_df <- tibble::tibble(origin = to_delete)
    local_duckdb_register(con, "origins_to_delete", to_delete_df)

    # Both deletes are wrapped in a single transaction for atomicity:
    # either all stale origins are removed or none are. Embeddings must be
    # deleted before documents because the embeddings table references doc_id
    # (enforced by application code; DuckDB 1.x lacks deferred FK checking).
    dbWithTransaction2(con, {
      DBI::dbExecute(
        con,
        "
        DELETE FROM embeddings
        WHERE doc_id IN (
          SELECT doc_id FROM documents
          WHERE origin IN (SELECT origin FROM origins_to_delete)
        )
        "
      )
      DBI::dbExecute(
        con,
        "DELETE FROM documents
         WHERE origin IN (SELECT origin FROM origins_to_delete)"
      )
    })
  }

  if (rebuild_index && length(to_delete)) {
    ragnar_store_build_index(store)
  }

  attr(store, "sync_summary") <- summary
  invisible(store)
}

#' Audit a Ragnar store against an expected set of URLs
#'
#' `ragnar_store_audit()` checks which URLs from an expected set are present in
#' the store's `documents` table. Use it to verify that all pages were
#' successfully ingested after a [ragnar_store_ingest()] run.
#'
#' @param store A `DuckDBRagnarStore` (version 2 only).
#' @param expected A character vector or [`ragnar_links`][ragnar_find_links()]
#'   object of URLs or file paths to check, or a data frame with a character
#'   column named `source` containing those origins.
#' @param group_by Optional name of a column in `expected` to group by. It must
#'   differ from the summary column names below. The default, `NULL`, reports
#'   totals for the full expected set.
#'
#' @returns A tibble with one row, or one row per group when `group_by` is set,
#'   with columns:
#'   - The grouping column, when requested, in order of first appearance.
#'   - `expected` (integer): number of distinct expected origins.
#'   - `ingested` (integer): number of expected origins present in the store.
#'   - `missing` (integer): number of expected origins absent from the store.
#'   - `chunks` (integer): total stored chunks for the expected origins.
#'   - `missing_urls` (list): a character vector of missing origins for each row,
#'     or `character()` when all expected origins are present.
#'
#'   Duplicate origins are counted once per group. Origins stored outside the
#'   expected set are excluded. Empty ungrouped input returns one row of zero
#'   counts; empty grouped input returns zero rows. Read-only connections are
#'   supported.
#'
#' @export
ragnar_store_audit <- function(store, expected, group_by = NULL) {
  stopifnot(S7::S7_inherits(store, DuckDBRagnarStore))
  if (store@version != 2L) {
    rlang::abort(
      "`ragnar_store_audit()` only supports version 2 stores.",
      call = NULL
    )
  }
  if (!is.data.frame(expected)) {
    check_character(expected, allow_na = FALSE)
    expected <- tibble::tibble(source = as.character(expected))
  }
  stopifnot("source" %in% names(expected))
  check_character(expected$source, allow_na = FALSE)
  check_string(group_by, allow_null = TRUE)
  if (!is.null(group_by)) {
    if (!group_by %in% names(expected)) {
      cli::cli_abort(
        "Grouping column {.val {group_by}} is absent from {.arg expected}."
      )
    }
    if (
      group_by %in% c("expected", "ingested", "missing", "chunks", "missing_urls")
    ) {
      cli::cli_abort("{.arg group_by} must differ from the summary column names.")
    }
  }
  origins <- tibble::tibble(.origin = as.character(expected$source))
  if (!is.null(group_by)) {
    origins$.group <- expected[[group_by]]
  }
  origins <- distinct(origins)

  con <- store@con

  expected_df <- tibble::tibble(origin = unique(origins$.origin))
  local_duckdb_register(con, "audit_expected", expected_df)

  result <- DBI::dbGetQuery(
    con,
    "
    SELECT
      e.origin,
      d.origin IS NOT NULL AS present,
      CAST(COUNT(em.doc_id) AS INTEGER) AS chunks
    FROM audit_expected e
    LEFT JOIN documents d USING(origin)
    LEFT JOIN embeddings em ON em.doc_id = d.doc_id
    GROUP BY e.origin, d.origin
    "
  )

  i <- match(origins$.origin, result$origin)
  origins$.present <- result$present[i]
  origins$.chunks <- result$chunks[i]
  result <- origins |>
    dplyr::summarise(
      expected = dplyr::n(),
      ingested = sum(.data$.present),
      missing = sum(!.data$.present),
      chunks = sum(.data$.chunks),
      missing_urls = list(.data$.origin[!.data$.present]),
      .by = any_of(".group")
    )
  if (!is.null(group_by)) {
    names(result)[[1L]] <- group_by
  }
  result
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
  switch(
    store@version,
    ragnar_store_build_index_v1(store, type),
    ragnar_store_build_index_v2(store, type)
  )
}

# @export
#
# The plan is to export RagnarStore when we're ready for users to subclass
# to implement other store types/backends. Note: before doing this, we are
# also considering transitioning to R6 for the store class object, since a
# store is inherently stateful and S7 is better suited to stateless objects.
RagnarStore := new_class(
  properties = list(
    location = prop_string(),
    embed = S7::new_union(class_function, NULL),
    schema = NULL | class_data.frame,
    name = prop_string(),
    title = prop_string(allow_null = TRUE)
  ),
  abstract = TRUE
)

DuckDBRagnarStore := new_class(
  parent = RagnarStore,
  properties = list(
    con = methods::getClass("DBIConnection"),
    version = class_integer,
    .con = new_property(
      getter = function(self) {
        warning("@.con renamed to @con, please update your code")
        self@con
      }
    )
  )
)

local({
  method(print, DuckDBRagnarStore) <- function(x, ...) {
    embed <- deparse1(x@embed)
    embed <- sub("function ?\\(", "\\\\(", embed)
    embed <- sub(") +", ") ", embed)

    # TODO: restore @schema:
    cat(glue(
      '
    <ragnar::DuckDBRagnarStore>
     @ location: {x@location}
     @ embed   : {embed}
     @ name    : {x@name}
     @ title   : {x@title %||% "NULL"}
     @ con     : <DBI::DBIConnection>
     @ version : {x@version}

    '
    ))
  }
})

#' @importFrom dplyr tbl sql arrange collect
local({
  method(tbl, ragnar:::DuckDBRagnarStore) <-
    function(src, from = "chunks", ...) {
      tbl(src@con, from)
    }
})


#' Launch the Ragnar Store Inspector
#'
#' Launches a Shiny app for interactively browsing a Ragnar store, previewing
#' document chunks, and testing search behavior.
#'
#' @param store A `RagnarStore` object to inspect.
#' @param ... Passed to [shiny::runApp()].
#'
#' @returns `NULL` (invisibly).
#'
#' @details
#'
#' The Store Inspector is a Shiny app for exploring a `RagnarStore`. Use it to
#' quickly see what was ingested and preview search results for different
#' queries. Type a query in the search bar and choose BM25 or VSS. The list of
#' documents on the left updates, and clicking a row shows its text and metadata
#' on the right. You can drag the divider to resize the document list and
#' preview area.
#'
#' The preview area shows the chunk content. You can view it as rendered
#' Markdown or switch to “Raw Text” to see the stored text (long lines are
#' wrapped). Metadata is shown above the text in YAML format, including any
#' extra fields stored with the chunk.
#'
#' @section Keyboard Shortcuts:
#'
#' | *Context*        | *Shortcut*                                      | *Action*                |
#' |------------------|-------------------------------------------------|-------------------------|
#' | Global           | `/`, `Esc`                                      | Focus search; clear it  |
#' | Documents list   | `ArrowUp`/`ArrowDown`, `j`/`k`                  | Move selection          |
#' | Vertical Divider | `ArrowLeft`/`ArrowRight` (+`Shift`), `g`/`Home` | Resize; reset           |
#'
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

#' Visualize a store using Embedding Atlas
#'
#' @inheritParams ragnar_store_inspect
#' @param port Port to run the Embedding Atlas server on.
#' @param host Host to run the Embedding Atlas server on.
#' @param launch.browser Whether to launch the browser automatically.
#'
#' @note This function requires the `embedding-atlas` Python package (>= 0.20.0)
#' in your reticulate Python environment, the `duckdb` R package (>= 1.5.0),
#' and `nanoarrow` (>= 0.8.0) to transfer data from the DuckDB store to Python.
#'
#' @examples
#' \dontrun{
#' # Start Ollama, then run this in a terminal:
#' # ollama pull embeddinggemma:300m-qat-q4_0
#'
#' # Create an in-memory store using local embeddings
#' store <- ragnar_store_create(
#'   embed = embed_ollama(model = "embeddinggemma:300m-qat-q4_0")
#' )
#'
#' # Read and embed a chapter from R for Data Science
#' chunks <- "https://r4ds.hadley.nz/data-transform.html" |>
#'   read_as_markdown() |>
#'   markdown_chunk()
#' ragnar_store_insert(store, chunks)
#'
#' # Launch the Embedding Atlas app
#' # Interrupt R (Esc or Ctrl+C) to stop the server
#' ragnar_store_atlas(store)
#' }
#'
#'
#' @export
ragnar_store_atlas <- function(
  store,
  ...,
  host = "localhost",
  port = 3030,
  launch.browser = interactive()
) {
  rlang::check_installed("nanoarrow", version = "0.8.0")

  # duckdb R version need to match the Python version at least in major.minor
  duckdb_version <- numeric_version(utils::packageVersion("duckdb"))

  reticulate::py_require(c(
    "embedding-atlas>=0.20.0",
    "nanoarrow",
    sprintf("duckdb==%s.%s.*", duckdb_version$major, duckdb_version$minor),
    "numba>=0.62.0rc2",
    "llvmlite>=0.45.0rc2"
  ))
  atlas <- reticulate::import_from_path(
    "_ragnartools.atlas",
    system.file("python", package = "ragnar")
  )
  df <- DBI::dbGetQueryArrow(
    store@con,
    "SELECT * FROM chunks;"
  )
  join_thread <- atlas$run_embedding_atlas(df, host, as.integer(port))
  if (launch.browser) {
    utils::browseURL(sprintf("http://%s:%d", host, port))
  }
  join_thread()
  invisible(NULL)
}
