



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
  if (missing(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_ollama), alist(x = ), args)
    return(fn)
  }

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


#' @export
embed_ollama <- ragnar_embed_ollama


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

  if (missing(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::ragnar_embed_openai), alist(x = ), args)
    return(fn)
  }

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

    # embeddings is a list of length(text), of double vectors

    # > resp_body_json(resp, simplifyVector = TRUE) |> str()
    # List of 4
    #  $ object: chr "list"
    #  $ data  :'data.frame':	89 obs. of  3 variables:
    #   ..$ object   : chr [1:89] "embedding" "embedding" "embedding" "embedding" ...
    #   ..$ index    : int [1:89] 0 1 2 3 4 5 6 7 8 9 ...
    #   ..$ embedding:List of 89
    #   .. ..$ : num [1:1536] -0.01258 0.03318 0.00534 -0.04137 0.00282 ...
    #   .. ..$ : num [1:1536] -0.0191 0.0215 0.0508 -0.0391 0.0168 ...
    #   .. ..$ : num [1:1536] -0.0235 0.0288 0.0298 -0.0365 0.0191 ...
    #   .. ..$ : num [1:1536] -0.000126 -0.005694 0.021306 -0.018764 -0.012051 ...
    #   .. ..$ : num [1:1536] 0.02475 -0.00438 0.01781 -0.00192 0.01195 ...
    #  $ model : chr "text-embedding-3-small"
    #  $ usage :List of 2
    #   ..$ prompt_tokens: int 12436
    #   ..$ total_tokens : int 12436
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
  sprintf("'%s' via ragnar", Sys.info()[["user"]])
}


capture_args <- function(omit_default_values = TRUE) {
  ## modified copy of keras3:::capture_args()
  envir <- parent.frame(1L)
  fn <- sys.function(-1L)
  call <- sys.call(-1L)
  call <- match.call(fn, call, expand.dots = TRUE, envir = parent.frame(2L))

  fn_arg_nms <- names(formals(fn))
  known_args <- intersect(names(call), fn_arg_nms)
  names(known_args) <- known_args

  call <- as.call(c(
    list,
    lapply(known_args, as.symbol),
    if ("..." %in% fn_arg_nms) quote(...)
  ))
  args <- eval(call, envir)

  if (omit_default_values) {
    default_args <- as.list(formals(fn))
    for (nm in names(args)) {
      if (identical(default_args[[nm]], args[[nm]]))
        args[[nm]] <- NULL
    }
  }

  args

}

partial <- function(.fn, .sig, ...) {
  body <- as.call(c(.fn, lapply(names(.sig), as.symbol),  ...))
  as.function.default(c(.sig, body), envir = environment(.fn) %||% globalenv())
}
