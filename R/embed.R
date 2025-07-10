#' Embed Text
#'
#' @param x x can be:
#'  - A character vector, in which case a matrix of embeddings is returned.
#'  - A data frame with a column named `text`, in which case the dataframe is
#'    returned with an additional column named `embedding`.
#'  - Missing or `NULL`, in which case a function is returned that can be called
#'    to get embeddings. This is a convenient way to partial in additional arguments like `model`,
#'    and is the most convenient way to produce a function that can be passed to the `embed` argument of `ragnar_store_create()`.
#' @param base_url string, url where the service is available.
#' @param model string; model name
#' @param batch_size split `x` into batches when embedding. Integer, limit of
#'   strings to include in a single request.
#'
#' @returns If `x` is a character vector, then a numeric matrix is returned,
#'   where `nrow = length(x)` and `ncol = <model-embedding-size>`. If `x` is a
#'   data.frame, then a new `embedding` matrix "column" is added, containing the
#'   matrix described in the previous sentence.
#' @name embed_ollama
#' @examples
#' text <- c("a chunk of text", "another chunk of text", "one more chunk of text")
#' \dontrun{
#' text |>
#'   embed_ollama() |>
#'   str()
#'
#' text |>
#'   embed_openai() |>
#'   str()
#' }
NULL

#' @export
#' @rdname embed_ollama
embed_ollama <- function(
  x,
  base_url = "http://localhost:11434",
  model = "snowflake-arctic-embed2:568m",
  batch_size = 10L
) {
  if (missing(x) || is.null(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_ollama), alist(x = ), args)
    return(fn)
  }

  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      base_url = base_url,
      model = model,
      batch_size = batch_size
    )
    return(x)
  }

  check_character(x)
  if (!length(x)) {
    # ideally we'd return a 0-row matrix, but currently the correct
    # embedding_size is not convenient to access in this context
    return(NULL)
  }

  starts <- seq.int(from = 1L, to = length(x), by = batch_size)
  ends <- c(starts[-1L] - 1L, length(x))

  embeddings <- map2(starts, ends, function(start, end) {
    req <- request(base_url) |>
      req_user_agent(ragnar_user_agent()) |>
      req_url_path_append("/api/embed") |>
      req_body_json(list(model = model, input = x[start:end])) |>
      req_error(body = \(resp) {
        resp_body_json(resp)$error
      })

    resp <- req_perform(req)
    resp_body_json(resp, simplifyVector = TRUE)$embeddings
  })

  list_unchop(embeddings)
}


#' @param api_key resolved using env var `OPENAI_API_KEY`
#' @param dims An integer, can be used to truncate the embedding to a specific size.
#' @param user User name passed via the API.
#'
#' @returns A matrix of embeddings with 1 row per input string, or a dataframe with an 'embedding' column.
#' @export
#' @rdname embed_ollama
embed_openai <- function(
  x,
  model = "text-embedding-3-small",
  base_url = "https://api.openai.com/v1",
  api_key = get_envvar("OPENAI_API_KEY"),
  dims = NULL,
  user = get_user(),
  batch_size = 20L
) {
  if (missing(x) || is.null(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_openai), alist(x = ), args)
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

  if (!length(text)) {
    # ideally we'd return a 0-row matrix, but currently the correct
    # embedding_size is not convenient to access in this context
    return(NULL)
  }

  ## open ai models have max token length of 8191... what happens if too long?
  data <- list(model = model, input = NULL)
  data$user <- user
  if (!is.null(dims)) {
    check_number_whole(dims, min = 1L)
    data$dimensions <- as.integer(dims)
  }

  starts <- seq.int(from = 1L, to = length(text), by = batch_size)
  ends <- c(starts[-1L] - 1L, length(text))

  embeddings <- map2(starts, ends, function(start, end) {
    ## max input is 8191 tokens per chunk... what happens if too long?
    data$input <- as.list(text[start:end])

    req <- request(base_url) |>
      req_user_agent(ragnar_user_agent()) |>
      req_url_path_append("/embeddings") |>
      req_auth_bearer_token(api_key) |>
      req_retry(max_tries = 2L) |>
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


# ---- utils ----

get_envvar <- function(name, error_call = caller_env()) {
  val <- Sys.getenv(name, NA_character_)
  if (is.na(val)) {
    if (is_testing()) {
      testthat::skip(sprintf("%s env var is not configured", name))
    } else {
      cli::cli_abort("Can't find env var {.code {name}}.", call = error_call)
    }
  }
  val
}

get_user <- function() {
  sys_info <- Sys.info()
  user <- sys_info[["effective_user"]]
  if (user != "unknown") {
    return(user)
  }
  user <- sys_info[["user"]]
  if (user != "unknown") {
    return(user)
  }
  NULL
}

ragnar_user_agent <- function() {
  paste0("r-ragnar/", .package_version)
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

.package_version <- c(read.dcf('DESCRIPTION', 'Version'))
