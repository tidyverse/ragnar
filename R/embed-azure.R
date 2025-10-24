
# setup: https://learn.microsoft.com/en-us/azure/ai-foundry/how-to/develop/sdk-overview?pivots=programming-language-python#prerequisites
#model_id <- "azureml://registries/azure-openai/models/text-embedding-3-large/versions/1"
#https://ragnar.cognitiveservices.azure.com/openai/deployments/text-embedding-3-large/embeddings?api-version=2023-05-15


#' Uses Azure AI Foundry to create embeddings
#' 
#' @inheritParams embed_openai
#' @param endpoint The Azure AI Foundry endpoint URL. A URI in the form of
#'   `https://<project>.cognitiveservices.azure.com/`. Defaults to the value
#'   of the `AZURE_OPENAI_ENDPOINT` environment variable.
#'   This URL is appended with `/openai/deployments/{model}/embeddings`. 
#'   Where `model` is the deployment name of the model.
#' @param api_version The API version to use. Defaults to `2023-05-15`.
#' @param model The deployment name of the model to use for generating embeddings.
#' @param api_args A list of additional arguments to pass to the API request body.
#' 
#' @export
embed_azure_openai <- function(
  x, 
  endpoint = get_envvar("AZURE_OPENAI_ENDPOINT"),
  api_key = get_envvar("AZURE_OPENAI_API_KEY"),
  api_version = "2023-05-15", 
  model,
  batch_size = 20L,
  api_args = list()
) {
  if (missing(x) || is.null(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_azure_openai), alist(x = ), args)
    return(fn)
  }

  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      endpoint = endpoint,
      api_key = api_key,
      api_version = api_version,
      model = model,
      batch_size = batch_size,
      api_args = api_args
    )
    return(x)
  }

  text <- x
  check_character(text)
  if (!length(text)) {
    # ideally we'd return a 0-row matrix, but currently the correct
    # embedding_size is not convenient to access in this context
    return(NULL)
  }
  check_string(model, allow_empty = FALSE)
  if (!is.list(api_args)) {
    cli::cli_abort("`api_args` must be a list.")
  }

  # API reference:
  # https://learn.microsoft.com/en-us/rest/api/aifoundry/model-inference/get-embeddings/get-embeddings
  base_req <- httr2::request(endpoint) |>
    embed_req_retry() |>
    httr2::req_url_path_append("openai", "deployments", model, "embeddings") |>
    httr2::req_url_query("api-version" = api_version) |>
    httr2::req_headers_redacted("api-key" = api_key) |>
    httr2::req_headers("Content-Type" = "application/json") |>
    httr2::req_user_agent(ragnar_user_agent()) |>
    httr2::req_error(body = function(resp) {
      json <- httr2::resp_body_json(resp, check_type = FALSE)
      json$error$message
    })

  out <- vector("list", length(text))
  base_body <- rlang::list2(model = model, !!!api_args)

  for (indices in chunk_list(seq_along(text), batch_size)) {
    body <- base_body
    body$input <- as.list(text[indices])

    resp <- base_req |>
      httr2::req_body_json(body) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)

    out[indices] <- resp$data$embedding
  }

  matrix(unlist(out), nrow = length(text), byrow = TRUE)
}
