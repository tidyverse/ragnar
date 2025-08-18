#' @inheritParams embed_openai
#' @param api_key resolved using env var `GEMINI_API_KEY`
#' @describeIn embed_google_vertex Use the Gemini API to create embeddings.
#' @export
#' @examplesIf Sys.get("GEMINI_API_KEY") != ""
#' embed_google_gemini(
#'   "hello world",
#'   model = model
#' )
embed_google_gemini <- function(
  x,
  model = "gemini-embedding-001",
  base_url = "https://generativelanguage.googleapis.com/v1beta",
  api_key = get_envvar("GEMINI_API_KEY"),
  dims = NULL,
  task_type = "RETRIEVAL_QUERY",
  batch_size = 20L
) {
  if (missing(x) || is.null(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_google_gemini), alist(x = ), args)
    return(fn)
  }

  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      model = model,
      base_url = base_url,
      api_key = api_key,
      dims = dims,
      task_type = task_type,
      batch_size = batch_size
    )
    return(x)
  }

  text <- x
  check_character(text)
  check_string(model, allow_empty = FALSE)

  base_req <- base_url |>
    httr2::request() |>
    req_user_agent(ragnar_user_agent()) |>
    httr2::req_headers_redacted("x-goog-api-key" = api_key) |>
    httr2::req_url_path_append(
      "models",
      paste0(model, ":batchEmbedContents")
    ) |>
    httr2::req_error(body = function(resp) {
      json <- resp_body_json(resp, check_type = FALSE)
      json$error$message
    })

  out <- list()
  
  base_body <- body <- list(
    taskType = task_type,
    model = paste0("models/", model)
  )
  if (!is.null(dims)) {
      base_body$outputDimensionality <- dims
  }

  for (indices in chunk_list(seq_along(x), batch_size)) {
    requests <- lapply(indices, function(i) {
      body <- base_body
      body$content <- list(parts = list(list(text = x[[i]])))
      body
    })
  
    resp <- base_req |>
      httr2::req_body_json(list(requests = requests)) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    out[indices] <- lapply(resp$embeddings, \(x) x$values)
  }

  matrix(
    unlist(out),
    nrow = length(x),
    ncol = length(out[[1]]),
    byrow = TRUE
  )
}
