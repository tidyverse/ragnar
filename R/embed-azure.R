
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
#' @param api_version The API version to use. Defaults to `2024-02-01`.
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
  api_args = list()
) {
  # https://learn.microsoft.com/en-us/rest/api/aifoundry/model-inference/get-embeddings/get-embeddings?view=rest-aifoundry-model-inference-2024-05-01-preview&tabs=HTTP#request-body
  base_req <- httr2::request(endpoint) |> 
    httr2::req_url_path_append("openai", "deployments", model, "embeddings") |> 
    httr2::req_url_query("api-version" = api_version) |> 
    httr2::req_headers_redacted("api-key" = api_key, "Content-Type" = "application/json")

  body <- rlang::list2(
    input = x,
    model = model,
    !!!api_args
  )

  req <- base_req |> 
    httr2::req_body_json(body) |> 
    httr2::req_user_agent(ragnar_user_agent()) |> 
    httr2::req_error(body = function(resp) {
      json <- httr2::resp_body_json(resp, check_type = FALSE)
      json$error$message
    })
  
  resp <- httr2::req_perform(req)

}