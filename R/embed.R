



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

  vctrs::vec_c(!!!embeddings)
}


#' Embed text via OpenAI API
#'
#' @param x character vector, or a dataframe with a column named `text`.
#' @param model string
#' @param base_url api url
#' @param api_key resolved using env var `OPENAI_API_KEY`
#' @param dims An integer, can be used to truncate the embedding to a specific size.
#' @param user User name passed via the API.
#' @param batch_size Integer, limit of strings to include in a single request.
#'
#' @returns A matrix of embeddings with 1 row per input string, or a dataframe with an 'embedding' column.
#' @export
ragnar_embed_openai <- function(x,
                                model = "text-embedding-3-small",
                                base_url = "https://api.openai.com/v1",
                                api_key = openai_key(),
                                dims = NULL,
                                user = get_ragnar_username(),
                                batch_size = 20) {

  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      model = model,
      base_url = base_url,
      api_key = api_key,
      dims = dims,
      user = user,
      batch_size = batch_size
    )
    return(x)
  }

  text <- x
  check_character(text)
  check_string(model, allow_empty = FALSE)

  ## open ai models have max token length of 8191... what happens if too long?
  data <- list(model = model, input = NULL)
  data$user <- user
  if (!is.null(dims)) {
    check_number_whole(dims, min = 1)
    data$dimensions <- as.integer(dims)
  }

  starts <- seq.int(from = 1L, to = length(text), by = batch_size)
  ends <- c(starts[-1L] - 1L, length(text))

  embeddings <- map2(starts, ends, function(start, end) {
    ## max input is 8191 tokens per chunk... what happens if too long?
    data$input <- as.list(text[start:end])

    req <- request(base_url) |>
      req_url_path_append("/embeddings") |>
      req_auth_bearer_token(api_key) |>
      req_retry(max_tries = 2) |>
      req_body_json(data)

    resp <- req_perform(req)
    resp_body_json(resp, simplifyVector = TRUE)$data$embedding

  })

  matrix(unlist(embeddings), nrow = length(text), byrow = TRUE)
}

openai_key <- function() {
  get_envvar("OPENAI_API_KEY")
}


# ---- utils ----

get_envvar <- function(name, error_call = caller_env()) {
  val <- Sys.getenv(name, NA)
  if (is.na(val)) {
    if (is_testing()) {
      testthat::skip(sprintf("%s env var is not configured", name))
    } else {
      cli::cli_abort("Can't find env var {.code {name}}.", call = error_call)
    }
  }
  val
}

get_ragnar_username <- function() {
  paste0(Sys.info()[["user"]], " via ragnar")
}
