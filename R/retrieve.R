
#' @export
ragnar_retreive_vss <- function(store, prompt, top_k = 3L) {
  check_string(prompt)
  check_number_whole(top_k)
  store <- ragnar_store_connect(store)

  # TODO: retrieve model and embedding_size from store. Hardcoded for now.
  model <- "all-minilm"
  prompt_embedding <- ragnar_embed_ollama(prompt, model = model)
  embedding_size <- ncol(prompt_embedding)

  query <- glue(r"---(
    SELECT id, text
    FROM chunks
    ORDER BY array_distance(
      embedding,
      [{stri_flatten(prompt_embedding, ", ")}]::FLOAT[{embedding_size}])
    LIMIT {top_k};
    )---")

  as_tibble(dbGetQuery(store, query))
}

# TODO:
# ragnar_retreive_search <- function(...) {}
# ragnar_retreive_bm25 <- function(...) {}
# Consider consolidating into:
#   ragnar_retreive(..., method = c("vss", "text_search", "bm25"))
# once we have a way to do re-ranking.

