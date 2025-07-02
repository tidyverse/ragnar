#' Create and connect to a vector store
#'
#' @param location filepath, or `:memory:`. Location can also be a database name
#'   specified with `md:dbname`, in this case the database will be created in
#'   MotherDuck after a connection is etablished.
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
#' @param version integer. The version of the store to create. Version 1 stores
#'   a single flat table consisting of individual chunks. This provides
#'   opportunities to modify (augment) chunks in-place for embedding and
#'   retrieval. Version 2 stores whole documents and separately embeddings for chunk
#'   ranges. This provides opportunities for chunk de-overlapping or extracting
#'   arbitrary ranges from a source document, and works well with overlapping
#'   and automatic context generation. Version 2 is the default.
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
#'
ragnar_store_create <- function(
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
  if (inherits(embed, "crate")) {
    return(embed)
  }
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
  location,
  ...,
  read_only = TRUE
) {
  check_dots_empty()

  if (is_motherduck_location(location)) {
    con <- motherduck_connection(location, create = FALSE, overwrite = FALSE)
  } else {
    conn <- dbConnect(
      duckdb::duckdb(),
      dbdir = location,
      read_only = read_only,
      array = "matrix"
    )
  }

  tables <- dbListTables(conn)
  if (all(c("documents", "embeddings", "metadata") %in% tables)) {
    version <- 2L
  } else if (all(c("chunks", "metadata") %in% tables)) {
    version <- 1L
  } else {
    stop("Store must be created with ragnar_store_create()")
  }

  dbExecute(conn, "INSTALL fts; INSTALL vss;")
  dbExecute(conn, "LOAD fts; LOAD vss;")

  metadata <- dbReadTable(conn, "metadata")
  embed <- unserialize(metadata$embed_func[[1L]])
  schema <- switch(version, unserialize(metadata$schema[[1L]]), NULL)
  name <- metadata$name %||% unique_store_name()

  # attach function to externalptr, so we can retrieve it from just the connection.
  ptr <- conn@conn_ref
  attr(ptr, "embed_function") <- embed

  DuckDBRagnarStore(
    embed = embed,
    schema = schema,
    conn = conn,
    name = name,
    title = metadata$title,
    version = version
  )
}

#' Inserts or updates chunks in a `RagnarStore`
#'
#' @inheritParams ragnar_store_insert
#' @details
#'
#' ### Store Version 2:
#' chunks must be a data frame containing columns:
#' - origin: character, the source of the document (e.g., a url or file path). This is used as a unique key for documents in the store
#' - document: character, the full document
#' - start: integer, the character index of the chunk
#' - end: integer, the character index of the chunk
#' - headings: character, the markdown headings in scope at `start`. This is used to assemble the context.
#'
#' ### Store Version 1:
#'
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
  switch(
    store@version,
    ragnar_store_update_v1(store, chunks),
    ragnar_store_update_v2(store, chunks)
  )
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
  switch(
    store@version,
    ragnar_store_insert_v1(store, chunks),
    ragnar_store_insert_v2(store, chunks)
  )
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
    ragnar_store_build_index_v2(store, type),
  )
}


# @export
RagnarStore <- new_class(
  "RagnarStore",
  properties = list(
    embed = S7::new_union(class_function, NULL),
    schema = NULL | class_data.frame,
    name = class_character,
    title = S7::new_union(NULL, class_character)
  ),
  abstract = TRUE
)

DuckDBRagnarStore <- new_class(
  "DuckDBRagnarStore",
  RagnarStore,
  properties = list(
    conn = methods::getClass("DBIConnection"),
    version = class_integer,
    .con = new_property(
      # methods::getClass("DBIConnection"),
      getter = function(self) {
        warning("@.con renamed to @conn, please update your code")
        self@conn
      },
      setter = function(self, value) {
        if (!is.null(value)) {
          warning("@.con renamed to @conn, please update your code")
          self@conn <- value
        }
        self
      }
    )
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
local({
  method(tbl, ragnar:::DuckDBRagnarStore) <-
    function(src, from = "chunks", ...) {
      tbl(src@conn, from)
    }
})
