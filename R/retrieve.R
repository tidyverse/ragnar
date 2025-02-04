
#' @export
ragnar_retrieve_vss <- function(store, prompt, top_k = 3L) {
  check_string(prompt)
  check_number_whole(top_k)

  # TODO: retrieve model and embedding_size from store. Hardcoded for now.
  prompt_embedding <- store@embed(prompt)
  embedding_size <- ncol(prompt_embedding)

  # TODO: support specifying a minimum distance threshold too, in addition to `top_k`.
  query <- glue(r"---(
    SELECT
      id,
      array_distance(
        embedding,
        [{stri_flatten(prompt_embedding, ", ")}]::FLOAT[{embedding_size}]
      ) as distance,
      text
    FROM chunks
    ORDER BY distance
    LIMIT {top_k};
    )---")

  as_tibble(dbGetQuery(store@.con, query))
}

# TODO:
# ragnar_retrieve_search <- function(...) {}
# ragnar_retrieve_bm25 <- function(...) {}
# Consider consolidating into:
#   ragnar_retrieve(..., method = c("vss", "text_search", "bm25"))
# once we have a way to do re-ranking.
