#' Vector Similarity Search Retrieval
#'
#' Computes a similarity measure between the query and the document embeddings
#' and uses this similarity to rank and retrieve document chunks.
#'
#' @inherit ragnar_retrieve
#' @param query Character. The query string to embed and use for similarity
#'   search.
#' @param top_k Integer. Maximum number of document chunks to retrieve.
#'   Defaults to 3.
#' @param ... Additional arguments passed to methods.
#' @param method Character. Similarity method to use: `"cosine_distance"`,
#'   `"euclidean_distance"`, or `"negative_inner_product"`. Defaults to
#'   `"cosine_distance"`.
#' @param query_vector Numeric vector. The embedding for `query`.
#'   Defaults to `store@embed(query)`.
#' @param filter Optional. A filter expression evaluated with
#'   `dplyr::filter()`.
#'
#' @details
#' Supported methods:
#' * **cosine_distance** – cosine of the angle between two vectors.
#' * **euclidean_distance** – L2 distance between vectors.
#' * **negative_inner_product** – negative sum of element-wise products.
#'
#' If `filter` is supplied, the function first performs the similarity
#' search, then applies the filter in an outer SQL query. It uses the HNSW
#' index when possible and falls back to a sequential scan for large result
#' sets or filtered queries.
#'
#' @return A `tibble` with the top_k retrieved chunks,
#'   ordered by `metric_value`.
#'
#' @family ragnar_retrieve
#' @export
ragnar_retrieve_vss <- function(
  store,
  query,
  top_k = 3L,
  ...,
  method = "cosine_distance",
  query_vector = store@embed(query),
  filter
) {
  check_string(query)
  check_number_whole(top_k)
  top_k <- as.integer(top_k)
  if (inherits(store, "tbl_sql")) {
    warning(
      "Passing a `tbl()` to ragnar_retrieve_vss() is no longer supported ",
      "and will be removed in a future release. Instead, pass a `filter` expression directly."
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
  query_vector
  metric_value <- sql(sprintf(
    "%s(embedding, %s)",
    method_func,
    sql_float_array_value(query_vector)
  ))
  con <- store@con

  # TODO: if "text" %in% all.names(filter_expr), use v1 query w/ warning.
  if (missing(filter)) {
    ## simplest case
    sql_query <- glue(switch(
      store@version,
      # store@version 1
      "
      SELECT
        *,
        '{method}' AS metric_name,
        {metric_value} AS metric_value
      FROM chunks
      ORDER BY metric_value
      LIMIT {top_k}
      ",
      # store@version 2
      "
      SELECT
        doc.* EXCLUDE (text, doc_id),
        e.*,
        doc.text[ e.start: e.end ] AS text
      FROM (
        SELECT
          *,
          '{method}' AS metric_name,
          {metric_value} AS metric_value
        FROM embeddings
        ORDER BY metric_value
        LIMIT {top_k}
      ) AS e
      JOIN documents doc USING (doc_id)
      ORDER BY metric_value
      "
    ))
    # check_hnsw_index_scan_used(con, sql_query)
    res <- dbGetQuery(con, sql_query)
    return(as_tibble(res))
  }

  ## - The DuckDB VSS extension will segfault if a query that uses the hnsw index has a WHERE clause.
  ## https://github.com/duckdb/duckdb-vss/issues/62
  ## - If LIMIT is greater than 5000, then the index is not used
  ## - If we force `metric_value` in the WHERE clause like metrics_value > -99999,
  ##   we avoid a segfault but then the HNSW index is not used.
  ## Simply nesting the inner query, an outer `WHERE` is automatically pushed down, causes a segfault
  ## However, nesting *and* forcing metric_value in the outer query is optimized
  ## into a streaming plan.
  ## This is an ugly but practical workarounds to make sure we use the hnsw index,
  ## don't segfault, and work well (fast) in the common case, and correctly in the uncommon case.

  # inner query
  sql_query <- glue(switch(
    store@version,
    # store@version 1
    "
    SELECT
      *,
      '{method}' AS metric_name,
      {metric_value} AS metric_value
    FROM chunks
    ORDER BY metric_value
    LIMIT 5000
    ",
    # store@version 2
    "
    SELECT
      doc.* EXCLUDE (text, doc_id),
      e.*,
      doc.text[ e.start: e.end ] AS text
    FROM (
      SELECT
        *,
        '{method}' AS metric_name,
        {metric_value} AS metric_value
      FROM embeddings
      ORDER BY metric_value
      LIMIT 5000
    ) AS e
    JOIN documents doc USING (doc_id)
    ORDER BY metric_value
    "
  ))

  # use dbplyr to interpret the supplied filter R expression
  # and build the outer sql query around the inner query
  tbl <- tbl(
    src = con,
    from = sql(sql_query),
    vars = dbListFields(con, switch(store@version, "chunks", "embeddings"))
  )
  tbl <- dplyr::filter(tbl, {{ filter }})
  tbl <- tbl |> head(top_k)
  sql_query <- dbplyr::remote_query(tbl)

  if (top_k <= 5000L) {
    # check_hnsw_index_scan_used(con, sql_query)
    res <- dbGetQuery(con, sql_query)
    if (nrow(res) == top_k) {
      # if we have enough rows, return it
      res <- res |> as_tibble() |> arrange(metric_value)
      return(res)
    }
    ## earlier we set LIMIT 5000 on the inner query because anything higher than
    ## that forces a sequential scan. This is a slow fall back - it will force a
    ## sequential scan (not use the hnsw index) but will return the correct result.
    sql_query <- sql_query |>
      stri_replace_last_fixed("LIMIT 5000\n", "OFFSET 5000\n")
  } else {
    # hnsw index cannot be used with top_k > 5000,
    # remove the inner query limit, this will cause the inner
    # query to use a sequential scan.
    sql_query <- sql_query |>
      stri_replace_last_fixed("LIMIT 5000\n", "")
    res <- NULL
  }

  # if we got here, it's because either top_k > 5000, or the combination
  # of the inner query with LIMIT 5000 and streaming filter on the outer query
  # did not produce enough (top_k) rows, so we have to do a sequential scan of the
  # entire db.
  res2 <- dbGetQuery(con, sql_query)
  out <- vec_rbind(res, res2) |> as_tibble() |> arrange(metric_value)
  out
}


check_hnsw_index_scan_used <- function(con, sql, show_query = FALSE) {
  plan <- dbGetQuery(con, paste("EXPLAIN", sql))
  # print(plan)
  used <- any(grepl("HNSW_INDEX_SCAN", unlist(plan)))
  if (isTRUE(show_query)) {
    cat("HNSW_INDEX_SCAN ", if (!used) "not ", "used by query:\n", sql)
  } else if (isFALSE(show_query)) {
    cat("HNSW_INDEX_SCAN ", if (!used) "not ", "used\n")
  } else {
    if (used) {
      cat("HNSW_INDEX_SCAN used\n")
    } else {
      cat("HNSW_INDEX_SCAN not used by query:\n", sql)
    }
  }
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
        "fts_main_chunks.match_bm25(chunk_id, {text_quoted})"
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
#'   the query string must be present in order for a chunk to be retrieved.
#' @param k,b \eqn{k_1} and \eqn{b} parameters in the Okapi BM25 retrieval method.
#' @param filter Optional. A filter expression evaluated with `dplyr::filter()`.
#' @param text String, the text to search for.
#' @inheritParams ragnar_retrieve
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

  tbl <- tbl(store@con, "chunks") |>
    mutate(
      metric_name = "bm25",
      metric_value = sql(glue(
        "fts_main_chunks.match_bm25(chunk_id, ?, k := {k}, b := {b}, conjunctive := {conjunctive})"
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

  as_tibble(dbGetQuery(store@con, sql_query, params = list(text)))
}

calculate_bm25 <- function(store, text) {
  text <- dbQuoteString(store@con, text)
  glue("fts_main_chunks.match_bm25(chunk_id, {text})")
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
#' @keywords internal
ragnar_retrieve_vss_and_bm25 <- function(store, text, top_k = 3, ...) {
  check_string(text)
  check_number_whole(top_k)

  if (is.null(store@embed)) {
    vss <- NULL
  } else {
    vss <- ragnar_retrieve_vss(store, text, top_k, ...)
    vss[["embedding"]] <- NULL
  }

  bm25 <- ragnar_retrieve_bm25(store, text, top_k, ...)

  out <- vctrs::vec_rbind(vss, bm25)

  # maybe reorder cols, origin first, context and text last

  # pivot to wide format
  out <- tidyr::pivot_wider(
    out,
    names_from = "metric_name",
    values_from = "metric_value"
  )

  out <- out |>
    reorder_by_names(to_front = "origin", to_back = c("context", "text"))

  # TODO: come up with a nice reordering that doesn't involve too much compute.
  as_tibble(out)
}


#' Retrieve chunks from a `RagnarStore`
#'
#' Combines both `vss` and `bm25` search and returns the
#' union of chunks retrieved by both methods.
#'
#' @note The results are not re-ranked after identifying the unique values.
#'
#' @param store A `RagnarStore` object returned by `ragnar_store_connect()` or `ragnar_store_create()`.
#' @param text Character. Query string to match.
#' @param top_k Integer. Number of nearest entries to find per method.
#' @param ... Additional arguments passed to the lower-level retrieval functions.
#' @param deoverlap Logical. If `TRUE` (default) and `store@version == 2`,
#'   overlapping chunks are merged with [chunks_deoverlap()].
#'
#' @return A `tibble` of retrieved chunks. Each row
#'   represents a chunk and always contains a `text` column.
#'
#' @family ragnar_retrieve
#' @examplesIf (rlang::is_installed("dbplyr") && nzchar(Sys.getenv("OPENAI_API_KEY")))
#' ## Build a small store with categories
#' store <- ragnar_store_create(
#'   embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
#'   extra_cols = data.frame(category = character()),
#'   version = 1 # store text chunks directly
#' )
#'
#' ragnar_store_insert(
#'   store,
#'   data.frame(
#'     category = c(rep("pets", 3), rep("dessert", 3)),
#'     text     = c("playful puppy", "sleepy kitten", "curious hamster",
#'                  "chocolate cake", "strawberry tart", "vanilla ice cream")
#'   )
#' )
#' ragnar_store_build_index(store)
#'
#' # Top 3 chunks without filtering
#' ragnar_retrieve(store, "sweet")
#'
#' # Combine filter with similarity search
#' ragnar_retrieve(store, "sweet", filter = category == "dessert")
#' @export
ragnar_retrieve <- function(store, text, top_k = 3L, ..., deoverlap = TRUE) {
  chunks <- ragnar_retrieve_vss_and_bm25(store, text, top_k, ...)
  if (!S7_inherits(store, RagnarStore)) {
    # back-compat with tbl() supplied for store
    return(chunks)
  }
  switch(
    store@version,
    {
      # @version == 1
      chunks[["hash"]] <- NULL
    },
    {
      # @version == 2
      if (deoverlap) {
        chunks <- chunks_deoverlap(store, chunks)
      }
    }
  )

  chunks
}

#' Merge overlapping chunks in retrieved results
#'
#' Groups and merges overlapping text chunks from the same origin in the
#' retrieval results.
#'
#' @param store A `RagnarStore` object. Must have `@version == 2`.
#' @param chunks A [tibble][tibble::tibble] of retrieved chunks, such as the
#'   output of [ragnar_retrieve()].
#'
#' @details When multiple retrieved chunks from the same origin have overlapping
#' character ranges, this function combines them into a single non-overlapping
#' region.
#'
#' @return A [tibble][tibble::tibble] of de-overlapped chunks.
#'
#' @export
chunks_deoverlap <- function(store, chunks) {
  if (store@version < 2L) {
    stop("chunks_deoverlap() only supported with store@verion == 2")
  }
  deoverlapped <- chunks |>
    mutate(embedding = NULL) |>
    arrange(origin, start) |>
    mutate(
      .by = origin,
      overlap_grp = cumsum(start > lag(end, default = -1L))
    ) |>
    summarize(
      .by = c(origin, overlap_grp),
      origin = first(origin),
      start = first(start),
      end = last(end),
      context = first(context),
      across(
        -all_of(c("start", "end", "context", "text")),
        \(x) list(unlist(x))
      )
    ) |>
    select(-overlap_grp)

  local_duckdb_register(
    store@con,
    "_ragnar_tmp_rechunk",
    deoverlapped |>
      mutate(
        origin,
        start,
        end,
        'deoverlapped_id' = row_number(),
        .keep = "none"
      )
  )

  deoverlapped$text <- dbGetQuery(
    store@con,
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

  deoverlapped |> reorder_by_names(names(chunks))
}


utils::globalVariables(c(
  # retrieve and helpers
  "origin",
  "overlap_grp",
  "id",
  "chunk_id",
  "doc_id",
  "context",
  "metric_value",
  "array_slice",
  "embedding"
))
