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
#' - **cosine_distance**: Measures the dissimilarity between two vectors based on
#'   the cosine of the angle between them. Defined as \eqn{1 - cos(\theta)},
#'   where \eqn{cos(\theta)} is the cosine similarity.
#' - **cosine_similarity**: Measures the similarity between two vectors based on
#'   the cosine of the angle between them. Ranges from -1 (opposite) to 1 (identical),
#'   with 0 indicating orthogonality.
#' - **euclidean_distance**: Computes the straight-line (L2) distance between
#'   two points in a multidimensional space. Defined as \eqn{\sqrt{\sum(x_i - y_i)^2}}.
#' - **dot_product**: Computes the sum of the element-wise products of two vectors.
#' - **negative_dot_product**: The negation of the dot product.
#'
#' @family ragnar_retrieve
#' @export
ragnar_retrieve_vss <- function(
    store, text, top_k = 3L,
    method = c("cosine_distance", "cosine_similarity", "euclidean_distance",
               "dot_product", "negative_dot_product")
) {
  check_string(text)
  check_number_whole(top_k)
  method <- rlang::arg_match(method)

  cols <- names(store@schema) |>
    stringi::stri_subset_regex("^embedding$", negate = TRUE) |>
    stringi::stri_c(collapse=",")

  .[.., order_key] <- method_to_info(method)

  # TODO: support specifying a minimum distance threshold too, in addition to `top_k`.
  query <- glue(r"---(
    SELECT
      id,
      '{method}' as metric_name,
      {calculate_vss(store, text, method)} as metric_value,
      {cols}
    FROM chunks
    ORDER BY metric_value {order_key}
    LIMIT {top_k};
    )---")

  as_tibble(dbGetQuery(store@.con, query))
}

# if we ever export it, this can be used as
# store |> dplyr::mutate(score = calculate_vss(store, text))
# using dbplyr
calculate_vss <- function(store, text, method) {
  if (is.null(store@embed)) {
    cli::cli_abort("Store must have an embed function but got {.code NULL}")
  }

  embedded_text <- store@embed(text)
  embedding_size <- ncol(embedded_text)

  .[method_function, ..] <- method_to_info(method)

  glue::glue(r"---(
    {method_function}(
      embedding,
      [{stri_flatten(embedded_text, ", ")}]::FLOAT[{embedding_size}]
    )
  )---")
}

method_to_info <- function(method) {
  # see possible distances:
  # https://duckdb.org/docs/stable/sql/functions/array.html#array-native-functions
  switch(
    method,
    cosine_distance = c("array_cosine_distance", "ASC"),
    euclidean_distance = c("array_distance", "ASC"),
    negative_dot_product = c("array_negative_dot_product", "ASC"),
    cosine_similarity = c("array_cosine_similarity", "DESC"),
    dot_product = c("array_dot_product", "DESC")
  )
}

#' Retrieves chunks using the BM25 score
#'
#' BM25 refers to Okapi Best Matching 25. See \doi{10.1561/1500000019} for more information.
#'
#' @inherit ragnar_retrieve_vss
#' @family ragnar_retrieve
#' @export
ragnar_retrieve_bm25 <- function(store, text, top_k = 3L) {
  check_string(text)
  check_number_whole(top_k)

  cols <- names(store@schema) |>
    stringi::stri_subset_regex("^embedding$", negate = TRUE) |>
    stringi::stri_c(collapse=",")

  text <- dbQuoteString(store@.con, text)
  sql_query <- glue(r"---(
    SELECT
      id,
      'bm25' as metric_name,
      {calculate_bm25(store, text)} as metric_value,
      {cols}
    FROM chunks
    WHERE metric_value IS NOT NULL
    ORDER BY metric_value
    LIMIT {top_k};
    )---")

  as_tibble(dbGetQuery(store@.con, sql_query))
}

calculate_bm25 <- function(store, text) {
  text <- dbQuoteString(store@.con, text)
  glue::glue("fts_main_chunks.match_bm25(id, {text})")
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

  out <- vctrs::vec_rbind(
    ragnar_retrieve_vss(store, text, top_k, ...),
    ragnar_retrieve_bm25(store, text, top_k)
  )

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
#' @param store A `RagnarStore` object.
#' @param text A string to find the nearest match too
#' @param top_k Integer, the number of nearest entries to find *per method*.
#'
#' @returns A dataframe of retrieved chunks. Each row corresponds to an
#'   individual chunk in the store. It always contains a column named `text`
#'   that contains the chunks.
#'
#' @family ragnar_retrieve
#' @export
ragnar_retrieve <- function(store, text, top_k = 3L) {
  ragnar_retrieve_vss_and_bm25(store, text, top_k)
}

# TODO: re-ranking.

