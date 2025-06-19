#' Embed using Google Vertex API platform
#' @inheritParams embed_openai
#' @inheritParams ellmer::chat_google_vertex
#' @param model Character specifying the embedding model.
#' See supported models in [Text embeddings API](https://cloud.google.com/vertex-ai/generative-ai/docs/model-reference/text-embeddings-api)
#' @param task_type Used to convey intended downstream application to help the
#' model produce better embeddings. If left blank, the default used is `"RETRIEVAL_QUERY"`.
#' - `"RETRIEVAL_QUERY"`
#' - `"RETRIEVAL_DOCUMENT"`
#' - `"SEMANTIC_SIMILARITY"`
#' - `"CLASSIFICATION"`
#' - `"CLUSTERING"`
#' - `"QUESTION_ANSWERING"`
#' - `"FACT_VERIFICATION"`
#' - `"CODE_RETRIEVAL_QUERY"`
#' For more information about task types, see [Choose an embeddings task type](https://cloud.google.com/vertex-ai/generative-ai/docs/embeddings/task-types).
#' @examples
#' \dontrun{
#' embed_google_vertex(
#'  "hello world",
#'  model="gemini-embedding-001",
#'  project = "<your-project-id>",
#'  location = "us-central1"
#' )
#' }
#'@export
embed_google_vertex <- function(
  x,
  model,
  location,
  project_id,
  task_type = "RETRIEVAL_QUERY"
) {
  if (missing(x) || is.null(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_google_vertex), alist(x = ), args)
    return(fn)
  }

  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      model = model,
      location = location,
      project_id = project_id,
      task_type = task_type
    )
    return(x)
  }

  check_character(x)
  if (!length(x)) {
    # ideally we'd return a 0-row matrix, but currently the correct
    # embedding_size is not convenient to access in this context
    return(NULL)
  }

  check_string(model, allow_empty = FALSE)
  credentials <- google_credentials()
  credentials <- credentials()

  base_req <- vertex_url(location, project_id) |>
    httr2::request() |>
    req_user_agent(ragnar_user_agent()) |>
    httr2::req_headers(!!!credentials, .redact = names(credentials)) |>
    httr2::req_url_path_append(
      "models",
      paste0(model, ":predict")
    ) |>
    httr2::req_error(body = function(resp) {
      json <- resp_body_json(resp, check_type = FALSE)
      json$error$message
    })

  out <- list()
  # The gemini model does not support batches
  chunk_size <- if (grepl("gemini", model)) 1 else 20

  for (indices in chunk_list(seq_along(x), chunk_size)) {
    instances <- lapply(indices, \(i) {
      list(task_type = task_type, content = x[[i]])
    })

    resp <- base_req |>
      httr2::req_body_json(list(
        parameters = list(
          autoTruncate = FALSE
        ),
        instances = instances
      )) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    out[indices] <- lapply(resp$predictions, \(x) x$embeddings$values)
  }

  matrix(
    unlist(out),
    nrow = length(x),
    ncol = length(out[[1]]),
    byrow = TRUE
  )
}

vertex_url <- function(location, project_id) {
  paste(
    unlist(list(
      c("https://", location, "-aiplatform.googleapis.com/v1"),
      c("/projects/", project_id),
      c("/locations/", location),
      "/publishers/google/"
    )),
    collapse = ""
  )
}


google_credentials <- function(error_call = caller_env()) {
  scope <- "https://www.googleapis.com/auth/cloud-platform"
  if (has_connect_viewer_token(scope = scope)) {
    return(function() {
      token <- connectcreds::connect_viewer_token(scope = scope)
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  if (is_testing()) {
    testthat::skip_if_not_installed("gargle")
  }

  check_installed("gargle", "for Google authentication")

  gargle::with_cred_funs(
    funs = list(credentials_app_default = gargle::credentials_app_default),
    {
      token <- gargle::token_fetch(scopes = scope)
    },
    action = "replace"
  )

  if (is.null(token) && is_testing()) {
    testthat::skip("no Google credentials available")
  }

  if (is.null(token)) {
    cli::cli_abort(
      c(
        "No Google credentials are available.",
        i = "Try suppling an API key or configuring Google's application default credentials."
      ),
      call = error_call
    )
  }

  if (!token$can_refresh()) {
    return(function() {
      list(Authorization = paste("Bearer", token$credentials$access_token))
    })
  }

  expiry <- Sys.time() + token$credentials$expires_in - 5

  return(function() {
    if (expiry < Sys.time()) {
      token$refresh()
    }
    list(Authorization = paste("Bearer", token$credentials$access_token))
  })
}

has_connect_viewer_token <- function(...) {
  if (!is_installed("connectcreds")) {
    return(FALSE)
  }
  connectcreds::has_viewer_token(...)
}
