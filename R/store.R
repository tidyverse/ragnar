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
#' @examples
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

  tables <- dbListTables(con)
  if (all(c("documents", "embeddings", "metadata") %in% tables)) {
    version <- 2L
  } else if (all(c("chunks", "metadata") %in% tables)) {
    version <- 1L
  } else {
    stop("Store must be created with ragnar_store_create()")
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
#' @inheritParams ragnar_store_insert
#' @param chunks Content to update. The precise input structure depends on
#'   `store@version`. See Details.
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
ragnar_store_update <- function(store, chunks) {
  switch(
    store@version,
    ragnar_store_update_v1(store, chunks),
    ragnar_store_update_v2(store, chunks)
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
    embed = S7::new_union(class_function, NULL),
    schema = NULL | class_data.frame,
    name = class_character,
    title = S7::new_union(NULL, class_character)
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
