RagnarStoreV2 <- R6::R6Class(
  "RagnarStore",
  public = list(
    embed = NULL,
    initialize = function(
      location = ":memory:",
      read_only = !file.exists(location)
    ) {
      if (!file.exists(location)) {
        ragnar_store_create_v2(location, )
      }
      # if exists... populate, otherwise, get embed.
    }
  ),
  private = list(
    conn_ = NULL,
    finalize = function() {
      DBI::dbDisconnect(self$conn)
    }
  ),
  active = list(
    conn = function(x) {
      if (!missing(x)) {
        stop(
          "RagnarStore$conn cannot be changed after the store is created",
          call. = FALSE
        )
      }
      private$conn_
    }
  ),
  cloneable = FALSE
)


# ragnar_store_from <- function(paths)
ragnar_store_ingest <- function(store, paths, ...) {
  # cli::cli_progress_bar("Cleaning data", total = length(paths))
  # for (origin in paths) {
  for (i in seq_along(paths)) {
    origin <- paths[i]
    message(sprintf("[% 3i/%i] ingesting: %s", i, length(paths), origin))
    document <- tibble::tibble(
      origin = origin,
      text = read_as_markdown(origin, ...)
    )
    ragnar_store_update_document_and_chunks(
      store,
      document,
      ragnar_chunk(document)
    )
    # cli::cli_progress_update()
  }
  # cli::cli_progress_done()
  invisible(store)
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
    dbDisconnect(store@conn)
    store <- ragnar_store_connect_v2(location, read_only = TRUE)
  }
  store
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
    dbDisconnect(store@conn)
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
