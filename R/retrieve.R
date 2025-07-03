#' Uses vector similarity search
#'
#' Computes a similarity measure between the query and the documents embeddings
#' and uses this similarity to rank the documents.
#'
#' @inherit ragnar_retrieve
#' @param top_k Integer, maximum amount of document chunks to retrieve
#' @param method A string specifying the method used to compute the similarity
#'   between the query and the document chunks embeddings store in the database.
#'
#' @details
#' The supported methods are:
#' - **cosine_distance**: Measures the similarity between two vectors based on
#'   the cosine of the angle between them. Ranges from -1 (opposite) to 1 (identical),
#'   with 0 indicating orthogonality.
#' - **euclidean_distance**: Computes the straight-line (L2) distance between
#'   two points in a multidimensional space. Defined as \eqn{\sqrt{\sum(x_i - y_i)^2}}.
#' - **negative_inner_product**: Computes the sum of the element-wise products of two vectors.
#'
#' @family ragnar_retrieve
#' @export
ragnar_retrieve_vss <- function(
  store,
  query,
  top_k = 3L,
  ...,
  method = "cosine_distance",
  filter
) {
  check_string(query)
  check_number_whole(top_k)
  if (inherits(store, "tbl_sql")) {
    warning(
      "Passing a `tbl()` to ragnar_retrieve_vss() is no longer supported ",
      "and will be removed in a future replease. Instead, pass a `filter` expression directly."
    )
    tbl <- store
    return(ragnar_retrieve_vss_tbl(tbl, query, top_k, method))
  }

  # The VSS extension is strict about using the hnsw index. To make sure the
  # index scan is used:
  # - `ORDER BY ... LIMIT N` must be applied directly to the result from one of:
  #     array_distance(), array_cosine_distance(), array_negative_inner_product().
  #   Even a simple transform (e.g., -dist, 1-dist) or alias call (e.g., array_negative_dot_product())
  #   forces a sequential scan.
  # - The second argument to the distance function must is a constant vector.
  #   Using a parameterized queries or temporary tables to pass the query
  #   embedding vector forces a sequental scan; The `query` string embedding must
  #   must be serialized in the SQL string.

  ## to support user supplied `filter` args we use dbplyr to generate the sql.
  ## It's pretty similar to the sql I'd write by hand, and optimizes to
  ## an essentially identical physical plan.

  .[method_func, order_direction] <- method_to_info(method)
  embedded_query <- store@embed(query)
  metric_value <- sql(sprintf(
    "%s(embedding, %s)",
    method_func,
    sql_float_array_value(embedded_query)
  ))

  tbl <- tbl(store@conn, switch(store@version, "chunks", "embeddings"))
  if (!missing(filter)) {
    tbl <- dplyr::filter(tbl, {{ filter }})
  }

  tbl <- tbl |>
    mutate(
      metric_name = .env$method,
      metric_value = .env$metric_value
    ) |>
    arrange(metric_value) |>
    head(top_k)

  if (store@version == 2L) {
    tbl <- tbl |>
      left_join(dplyr::tbl(store@conn, "documents"), by = "origin") |>
      mutate(text = array_slice(text, start, end))
  }

  # # to confirm HNSW_INDEX_SCAN is being used:
  # {
  #   sql_query_string <- dbplyr::remote_query(tbl)
  #   dbGetQuery(store@conn, paste("EXPLAIN", sql_query_string))
  # }
  collect(tbl)
}


sql_float_array_value <- function(...) {
  x <- unlist(c(...))
  n <- length(x)
  x <- paste0(as.character(x), collapse = ",")
  sql(sprintf("[%s]::FLOAT[%i]", x, n))
  # x <- paste0("(", as.character(x), ")::FLOAT", collapse = ", ")
  # sql(sprintf("array_value(%s)", x))
}


method_to_info <- function(method) {
  # see possible distances:
  # https://duckdb.org/docs/stable/sql/functions/array.html#array-native-functions
  switch(
    method,

    ## the vss extension only currently supports three array distance functions
    ## these must exact matches to the array funciton name, using an alias or
    ## trivial transform means that the index won't be used. index scan is only performed for
    ## ORDER BY array_distance(), array_cosine_distance(), or array_negative_inner_product()
    cosine_distance = c("array_cosine_distance", "ASC"),
    euclidean_distance = c("array_distance", "ASC"), # sqrt((x-y)^2)
    negative_inner_product = c("array_negative_inner_product", "ASC"),

    ## These other methods are commented out because they cannot be used by the
    ## vss hnsw index (In an `ORDER BY ... LIMIT n` expression, they always
    ## perform a sequential scan). Also, they each have equivalent counterparts
    ## that _do_ support an index, so no meaningful capability is lost.

    ## Most embedding model providers normalize the embedding vector, which
    ## means that: inner_product == cosine_similarity

    # # 1 - cosine_distance
    # cosine_similarity = c("array_cosine_similarity", "DESC"),

    # # -negative_inner_product
    # inner_product = c("array_inner_product", "DESC")

    # # alias for inner_product
    # dot_product = c("array_dot_product", "DESC"),

    # # alias for negative_inner_product
    # negative_dot_product = c("array_negative_dot_product", "ASC"),

    stop("Unknown method")
  )
}


get_store_embed <- function(x) {
  if (S7_inherits(x, RagnarStore)) {
    return(x@embed)
  }

  if (inherits(x, "tbl_sql")) {
    con <- dbplyr::remote_con(x)
    ptr <- con@conn_ref
    embed <- attr(ptr, "embed_function", exact = TRUE)
    if (!is.null(embed)) {
      return(embed)
    }

    # Attribute missing: reread from db and cache on ptr
    embed_blob <- DBI::dbGetQuery(
      con,
      "SELECT embed_func FROM metadata LIMIT 1"
    )$embed_func[[1]]
    embed <- unserialize(embed_blob)
    attr(ptr, "embed_function") <- embed
    return(embed)
  }

  cli::cli_abort("`store` must be a RagnarStore or a dplyr::tbl()")
}


ragnar_retrieve_vss_tbl <- function(tbl, text, top_k, method) {
  rlang::check_installed("dbplyr")
  .[.., order_direction] <- method_to_info(method)
  tbl |>
    mutate(
      metric_value = sql(calculate_vss(tbl, text, method)),
      metric_name = method
    ) |>
    select(-"embedding") |>
    arrange(sql(glue("metric_value {order_direction}"))) |>
    head(n = top_k) |>
    collect() |>
    as_tibble()
}

ragnar_retrieve_bm25_tbl <- function(tbl, text, top_k) {
  rlang::check_installed("dbplyr")
  con <- dbplyr::remote_con(tbl)
  text_quoted <- DBI::dbQuoteString(con, text)

  tbl |>
    mutate(
      metric_value = sql(glue::glue(
        "fts_main_chunks.match_bm25(id, {text_quoted})"
      )),
      metric_name = "bm25"
    ) |>
    filter(sql('metric_value IS NOT NULL')) |>
    arrange(.data$metric_value) |>
    select(-"embedding") |>
    head(n = top_k) |>
    collect()
}

# if we ever export it, this can be used as
# store |> dplyr::mutate(score = calculate_vss(store, text))
# using dbplyr
calculate_vss <- function(store, text, method) {
  embed <- get_store_embed(store)
  if (is.null(embed)) {
    cli::cli_abort("Store must have an embed function but got {.code NULL}")
  }

  embedded_text <- embed(text)
  embedding_size <- ncol(embedded_text)

  .[method_function, ..] <- method_to_info(method)
  glue(
    r"---(
    {method_function}(
      embedding,
      [{stri_flatten(embedded_text, ", ")}]::FLOAT[{embedding_size}]
    )
    )---"
  )
}


#' Retrieves chunks using the BM25 score
#'
#' BM25 refers to Okapi Best Matching 25. See \doi{10.1561/1500000019} for more
#' information.
#'
#' @param conjunctive	Whether to make the query conjunctive i.e., all terms in
#'   the query string must be present in order for a chunk to be retrieved
#' @param k,b \eqn{k_1} and \eqn{b} parameters in the Okapi BM25 retrieval model.
#' @inherit ragnar_retrieve_vss
#' @family ragnar_retrieve
#' @export
ragnar_retrieve_bm25 <- function(
  store,
  text,
  top_k = 3L,
  ...,
  k = 1.2,
  b = 0.75,
  conjunctive = FALSE,
  filter
) {
  check_string(text)
  check_number_whole(top_k)
  check_bool(conjunctive)
  check_number_decimal(k)
  check_number_decimal(b)
  conjunctive <- as.integer(conjunctive)

  if (inherits(store, "tbl_sql")) {
    warning(
      "passing a tbl() to ragnar_retrieve() is no longer supported and will ",
      "be removed in a future release. Please pass a filter expression directly."
    )
    return(ragnar_retrieve_bm25_tbl(store, text, top_k))
  }

  tbl <- tbl(store@conn, "chunks") |>
    mutate(
      metric_name = "bm25",
      metric_value = sql(glue(
        "fts_main_chunks.match_bm25(id, ?, k := {k}, b := {b}, conjunctive := {conjunctive})"
      ))
    )

  filters <- c(
    if (!missing(filter)) list(enquo(filter)),
    list(sql('metric_value IS NOT NULL'))
  )
  tbl <- dplyr::filter(tbl, !!!filters)

  sql_query <- tbl |>
    arrange(metric_value) |>
    select(-any_of("embedding")) |>
    head(n = top_k) |>
    dbplyr::remote_query()

  as_tibble(dbGetQuery(store@conn, sql_query, params = list(text)))
}

calculate_bm25 <- function(store, text) {
  text <- dbQuoteString(store@conn, text)
  glue("fts_main_chunks.match_bm25(id, {text})")
}

#' Retrieve VSS and BM25
#'
#' Runs [ragnar_retrieve_vss()] and [ragnar_retrieve_bm25()] and get the distinct
#' documents.
#'
#' @note The results are not re-ranked after identifying the unique values.
#'
#' @inherit ragnar_retrieve
#' @param ... Forwarded to [ragnar_retrieve_vss()]
#' @param top_k Integer, the number of entries to retrieve using **per method**.
#' @family ragnar_retrieve
#' @export
ragnar_retrieve_vss_and_bm25 <- function(store, text, top_k = 3, ...) {
  check_string(text)
  check_number_whole(top_k)

  vss <- ragnar_retrieve_vss(store, text, top_k, ...)
  vss[["embedding"]] <- NULL

  bm25 <- ragnar_retrieve_bm25(store, text, top_k, ...)

  out <- vctrs::vec_rbind(vss, bm25)

  # maybe reorder cols, id first, text last
  out <- out[reorder_names("id", names(out), last = "text")]

  # pivot to wide format
  out <- tidyr::pivot_wider(
    out,
    names_from = "metric_name",
    values_from = "metric_value"
  )

  # TODO: come up with a nice reordering that doesn't involve too much compute.
  as_tibble(out)
}


#' Retrieve chunks from a `RagnarStore`
#'
#' [ragnar_retrieve()] is a thin wrapper around [ragnar_retrieve_vss_and_bm25()]
#' using the recommended best practices.
#'
#' @param store A `RagnarStore` object or a `dplyr::tbl()` derived from
#'   it. When you pass a `tbl`, you may use usual dplyr verbs (e.g.
#'   `filter()`, `slice()`) to restrict the rows examined before
#'   similarity scoring. Avoid dropping essential columns such as
#'   `text`, `embedding`, `origin`, and `hash`.
#' @param text A string to find the nearest match too
#' @param top_k Integer, the number of nearest entries to find *per method*.
#'
#' @returns A dataframe of retrieved chunks. Each row corresponds to an
#'   individual chunk in the store. It always contains a column named `text`
#'   that contains the chunks.
#'
#' @section Pre-filtering with dplyr:
#' The store behaves like a lazy table backed by DuckDB, so row‑wise
#' filtering is executed directly in the database. This lets you narrow the
#' search space efficiently without pulling data into R.
#'
#' @family ragnar_retrieve
#' @export
#' @examplesIf (rlang::is_installed("dbplyr") && nzchar(Sys.getenv("OPENAI_API_KEY")))
#' # Basic usage
#' store <- ragnar_store_create(
#'   embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small")
#' )
#' ragnar_store_insert(store, data.frame(text = c("foo", "bar")))
#' ragnar_store_build_index(store)
#' ragnar_retrieve(store, "foo")
#'
#' # More Advanced: store metadata, retrieve with pre-filtering
#' store <- ragnar_store_create(
#'   embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
#'   extra_cols = data.frame(category = character())
#' )
#'
#' ragnar_store_insert(
#'   store,
#'   data.frame(
#'     category = "desert",
#'     text = c("ice cream", "cake", "cookies")
#'   )
#' )
#'
#' ragnar_store_insert(
#'   store,
#'   data.frame(
#'     category = "meal",
#'     text = c("steak", "potatoes", "salad")
#'   )
#' )
#'
#' ragnar_store_build_index(store)
#'
#' # simple retrieve
#' ragnar_retrieve(store, "carbs")
#'
#' # retrieve with pre-filtering
#' dplyr::tbl(store) |>
#'   dplyr::filter(category == "meal") |>
#'   ragnar_retrieve("carbs")
ragnar_retrieve <- function(store, text, top_k = 3L, ..., deoverlap = TRUE) {
  chunks <- ragnar_retrieve_vss_and_bm25(store, text, top_k, ...)
  if (deoverlap && store@version == 2) {
    chunks <- chunks_deoverlap(store, chunks)
  }
  chunks
}

#' Merge Overlapping Chunks in Retrieved Results
#'
#' Groups and merges overlapping text chunks from the same origin in the
#' retrieval results.
#'
#' @param store A `RagnarStore` object. Must have `@version == 2`.
#' @param chunks A dataframe of retrieved chunks, such as the output of
#'   [ragnar_retrieve()].
#'
#' @details When multiple retrieved chunks from the same origin have overlapping
#' character ranges, this function combines them into a single chunk per overlap
#' group.
#'
#' @return A dataframe of de-overlapped chunks, where each row represents a
#'   non-overlapping region.
#'
#' @export
chunks_deoverlap <- function(store, chunks) {
  if (store@version < 2) {
    stop("chunks_deoverlap() only supported with store@verion == 2")
  }
  deoverlapped <- chunks |>
    arrange(origin, start) |>
    mutate(
      .by = origin,
      overlap_grp = cumsum(start > lag(end, default = -1L))
    ) |>
    summarize(
      .by = c(origin, overlap_grp),
      origin = first(origin),
      id = list(unlist(id)),
      start = first(start),
      end = last(end),
      headings = first(headings)
    ) |>
    select(-overlap_grp)

  local_duckdb_register(
    store@conn,
    "_ragnar_tmp_rechunk",
    deoverlapped |> mutate('deoverlapped_id' = row_number())
  )

  deoverlapped$text <- dbGetQuery(
    store@conn,
    "
    SELECT
    rechunked.deoverlapped_id,
      doc.text[ rechunked.start: rechunked.end ] AS text
    FROM _ragnar_tmp_rechunk rechunked
    JOIN documents doc
    USING (origin)
    ORDER BY rechunked.deoverlapped_id
    "
  )$text

  deoverlapped
}


utils::globalVariables(c(
  # retrieve and helpers
  "origin",
  "overlap_grp",
  "headings",
  "id",
  "metric_value",
  "array_slice",
  "embedding"
))
