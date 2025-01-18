



#' Embedd Text
#'
#' @param x A character vector, or a data frame with a column named `"text"`
#' @param base_url url where the ollama service is available
#' @param model string; model name
#' @param batch_size split `x` into batches when embedding.
#'
#' @returns If `x` is a character vector, then a numeric matrix is returned,
#' where `nrow = length(x)` and `ncol = <model-embedding-size>`.
#' If `x` is a data.frame, then a new `embedding` matrix "column" is added,
#' containing the matrix described in the previous sentence.
#' @export
#'
#' @examples
#' text <- c("a chunk of text", "another chunk of text", "one more chunk of text")
#' text |>
#'   ragnar_embed_ollama() |>
#'   str()
ragnar_embed_ollama <- function(x,
                                base_url = "http://localhost:11434",
                                model = "all-minilm",
                                batch_size = 10L) {
  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(x[["text"]],
                               base_url = base_url,
                               model = model,
                               batch_size = batch_size)
    return(x)
  }
  check_character(x)

  starts <- seq.int(from = 1L, to = length(x), by = batch_size)
  ends <- c(starts[-1L] - 1L, length(x))

  embeddings <- map2(starts, ends, function(start, end) {
    req <- request(base_url) |>
      req_url_path_append("/api/embed") |>
      req_body_json(list(model = model, input = x[start:end]))

    resp <- req_perform(req)
    resp_body_json(resp, simplifyVector = TRUE)$embeddings
  })

  # do.call(rbind, embeddings)
  vctrs::vec_c(!!!embeddings)
}

