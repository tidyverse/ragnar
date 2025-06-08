#' Embed text using a Databricks model
#'
#' [`embed_databricks()`] gets embeddings for text using a model hosted in a
#' Databricks workspace. It relies on the \pkg{ellmer} package for managing
#' Databricks credentials. See [`ellmer::chat_databricks`] for more on
#' supported modes of authentication.
#'
#' @inheritParams embed_ollama
#' @inheritParams ellmer::chat_databricks
#' @param model The name of a text embedding model.
#'
#' @export
embed_databricks <- function(
  x,
  workspace = databricks_workspace(),
  model = "databricks-bge-large-en",
  batch_size = 512L
) {
  if (missing(x) || is.null(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_databricks), alist(x = ), args)
    return(fn)
  }

  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      workspace = workspace,
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
  check_string(model, allow_empty = FALSE)

  credentials <- default_databricks_credentials(workspace)

  # See: https://docs.databricks.com/aws/en/machine-learning/foundation-model-apis/api-reference#embedding-task
  req <- request(workspace) |>
    req_url_path_append(
      glue::glue("serving-endpoints/{model}/invocations")
    ) |>
    httr2::req_headers_redacted(!!!credentials()) |>
    httr2::req_user_agent(databricks_user_agent()) |>
    httr2::req_error(body = function(resp) {
      if (httr2::resp_content_type(resp) == "application/json") {
        resp_body_json(resp)$message
      }
    })

  starts <- seq.int(from = 1L, to = length(x), by = batch_size)
  ends <- c(starts[-1L] - 1L, length(x))

  embeddings <- map2(starts, ends, function(start, end) {
    resp <- req_body_json(req, list(input = x[start:end])) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE)
    resp$data$embedding
  })

  matrix(unlist(embeddings), nrow = length(x), byrow = TRUE)
}

databricks_workspace <- function() {
  if (!is.null(the$databricks_workspace)) {
    return(the$databricks_workspace())
  }
  check_installed(
    "ellmer",
    "for Databricks credential management",
    call = quote(embed_databricks())
  )
  the$databricks_workspace <- get(
    "databricks_workspace",
    envir = asNamespace("ellmer")
  )
  the$databricks_workspace()
}

default_databricks_credentials <- function(workspace) {
  if (!is.null(the$default_databricks_credentials)) {
    return(the$default_databricks_credentials())
  }
  check_installed(
    "ellmer",
    "for Databricks credential management",
    call = quote(embed_databricks())
  )
  the$default_databricks_credentials <- get(
    "default_databricks_credentials",
    envir = asNamespace("ellmer")
  )
  the$default_databricks_credentials(workspace)
}

databricks_user_agent <- function() {
  user_agent <- paste0("r-ragnar/", utils::packageVersion("ragnar"))
  if (nchar(Sys.getenv("SPARK_CONNECT_USER_AGENT")) != 0) {
    user_agent <- paste(Sys.getenv("SPARK_CONNECT_USER_AGENT"), user_agent)
  }
  user_agent
}
