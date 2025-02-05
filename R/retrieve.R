
#' @export
ragnar_retrieve_vss <- function(store, text, top_k = 3L) {
  check_string(text)
  check_number_whole(top_k)

  # TODO: retrieve model and embedding_size from store. Hardcoded for now.
  embedded_text <- store@embed(text)
  embedding_size <- ncol(embedded_text)

  # TODO: support specifying a minimum distance threshold too, in addition to `top_k`.
  query <- glue(r"---(
    SELECT
      id,
      array_distance(
        embedding,
        [{stri_flatten(embedded_text, ", ")}]::FLOAT[{embedding_size}]
      ) as l2sq_distance,
      text
    FROM chunks
    ORDER BY l2sq_distance
    LIMIT {top_k};
    )---")

  as_tibble(dbGetQuery(store@.con, query))
}

#' @export
ragnar_retrieve_bm25 <- function(store, text, top_k = 3L) {
  check_string(text)
  check_number_whole(top_k)

  text <- dbQuoteString(store@.con, text)
  sql_query <- glue(r"---(
    SELECT
      id,
      fts_main_chunks.match_bm25(id, {text}) as bm25_score,
      text
    FROM chunks
    WHERE bm25_score IS NOT NULL
    ORDER BY bm25_score DESC
    LIMIT {top_k};
    )---")

  as_tibble(dbGetQuery(store@.con, sql_query))
}


#' @export
ragnar_retrieve <- function(store, text, top_k = 3L, methods = c("vss", "bm25")) {
  check_string(text)
  check_number_whole(top_k)

  out <- NULL
  if ("vss" %in% methods)
    out <- vctrs::vec_rbind(out, ragnar_retrieve_vss(store, text, top_k))

  if ("bm25" %in% methods)
    out <- vctrs::vec_rbind(out, ragnar_retrieve_bm25(store, text, top_k))

  # maybe reorder cols, id first, text last
  out <- out[reorder_names("id", names(out), last = "text")]

  # Fill missing bm25 distances
  if (any(na_bm25_score <- is.na(out$bm25_score))) {
    ids <- out$id[na_bm25_score]
    text_quoted <- dbQuoteString(store@.con, text)
    query <- glue::glue("
      SELECT
        id,
        fts_main_chunks.match_bm25(id, {text_quoted}) AS bm25_score
      FROM chunks
      WHERE id IN ({paste(ids, collapse = ', ')})
      ")
    df <- DBI::dbGetQuery(store@.con, query)
    out$bm25_score[na_bm25_score] <- df$bm25_score[match(ids, df$id)]
  }

  # Fill missing vss distances
  if (any(na_l2sq_dist <- is.na(out$l2sq_distance))) {
    ids <- out$id[na_l2sq_dist]
    embedded_text <- store@embed(text)
    embedding_size <- ncol(embedded_text)
    query <- glue::glue("
      SELECT
        id,
        array_distance(
          embedding,
          [{paste(embedded_text, collapse = ', ')}]::FLOAT[{embedding_size}]
        ) as l2sq_distance
      FROM chunks
      WHERE id IN ({paste(ids, collapse = ', ')})
    ")
    df <- DBI::dbGetQuery(store@.con, query)
    out$l2sq_distance[na_l2sq_dist] <- df$l2sq_distance[match(ids, df$id)]
  }

  as_tibble(out)
}



# TODO: re-ranking.
