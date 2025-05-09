#' Embed text using a Bedrock model
#'
#' @inheritParams embed_ollama
#' @inheritParams ellmer::chat_bedrock
#' @param model Currently only Cohere.ai and Amazon Titan models are supported.
#'   There are no guardarails for the kind of model that is used, but the model
#'   must be available in the AWS region specified by the profile.
#'   You may look for available models in the Bedrock Model Catalog
#' @param profile AWS profile to use.
#' @param api_args Additional arguments to pass to the Bedrock API. Dependending
#'   on the `model`, you might be able to provide different parameters. Check
#'   the documentation for the model you are using in the
#'   [Bedrock user guide](https://docs.aws.amazon.com/bedrock/latest/userguide/model-parameters.html).
#'
#' @seealso [embed_ollama()]
#'
#' @returns
#' If `x` is missing returns a function that can be called to get embeddings.
#' If `x` is not missing, a matrix of embeddings with 1 row per input string, or a dataframe with an 'embedding' column.
#'
#' @export
embed_bedrock <- function(x, model, profile, api_args = list()) {
  if (missing(x) || is.null(x)) {
    args <- capture_args()
    fn <- partial(quote(ragnar::embed_bedrock), alist(x = ), args)
    return(fn)
  }

  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      model = model,
      profile = profile,
      api_args = api_args
    )
    return(x)
  }

  rlang::check_installed("paws.common", "AWS authentication")
  cache <- aws_creds_cache(profile)
  credentials <- paws_credentials(profile, cache = cache)

  check_character(x)
  check_string(model, allow_empty = FALSE)

  if (!length(x)) {
    return(NULL) # Return early if there are no inputs
  }

  req <- httr2::request(paste0(
    "https://bedrock-runtime.",
    credentials$region,
    ".amazonaws.com"
  ))

  req <- httr2::req_url_path_append(
    req,
    "model",
    model,
    "invoke"
  )

  req <- httr2::req_error(req, body = function(resp) {
    body <- httr2::resp_body_json(resp)
    body$Message %||% body$message
  })

  # Closure that adds credentials to a request after refreshing them.
  req_auth_bedrock <- function(req) {
    credentials <- paws_credentials(profile, cache = cache)
    httr2::req_auth_aws_v4(
      req,
      aws_access_key_id = credentials$access_key_id,
      aws_secret_access_key = credentials$secret_access_key,
      aws_session_token = credentials$session_token
    )
  }

  if (grepl("cohere", model)) {
    # Cohere.ai models support a batch of texts in each request
    return(embed_bedrock_cohere(req, x, api_args, req_auth_bedrock))
  }

  if (grepl("titan", model)) {
    # Amazon Titan models support a single text in each request
    return(embed_bedrock_titan(req, x, api_args, req_auth_bedrock))
  }

  cli::cli_abort("Unsupported model: ", model)
}

embed_bedrock_cohere <- function(base_req, inputs, api_args, req_auth_bedrock) {
  # https://docs.aws.amazon.com/bedrock/latest/userguide/model-parameters-embed.html
  # {
  #     "texts":[string],
  #     "input_type": "search_document|search_query|classification|clustering",
  #     "truncate": "NONE|START|END",
  #     "embedding_types": embedding_types
  # }

  out <- list()
  for (indices in chunk_list(seq_along(inputs), 20)) {
    body <- rlang::list2(
      texts = as.list(inputs[indices]),
      !!!api_args
    )

    resp <- base_req |>
      httr2::req_body_json(body) |>
      req_auth_bedrock() |>
      httr2::req_perform()

    out[indices] <- httr2::resp_body_json(resp)$embeddings
  }

  matrix(
    unlist(out),
    nrow = length(inputs),
    ncol = length(out[[1]]),
    byrow = TRUE
  )
}


embed_bedrock_titan <- function(base_req, inputs, api_args, req_auth_bedrock) {
  # https://docs.aws.amazon.com/bedrock/latest/userguide/model-parameters-titan-embed-text.html
  # {
  #   "inputText": string,
  #   "dimensions": int,
  #   "normalize": boolean,
  #   "embeddingTypes": list
  # }
  # only works with a single input at a time
  out <- lapply(inputs, function(input) {
    body <- rlang::list2(
      inputText = input,
      !!!api_args
    )

    resp <- base_req |>
      httr2::req_body_json(body) |>
      req_auth_bedrock() |>
      httr2::req_perform()

    httr2::resp_body_json(resp)$embedding
  })

  matrix(
    unlist(out),
    nrow = length(inputs),
    ncol = length(out[[1]]),
    byrow = TRUE
  )
}

chunk_list <- function(lst, n) {
  split(lst, ceiling(seq_along(lst) / n))
}

# Helpers ---------------------------------------------------------------------

paws_credentials <- function(
  profile,
  cache = aws_creds_cache(profile),
  reauth = FALSE
) {
  creds <- cache$get()
  if (reauth || is.null(creds) || creds$expiration < Sys.time()) {
    cache$clear()
    rlang::try_fetch(
      creds <- locate_aws_credentials(profile),
      error = function(cnd) {
        if (is_testing()) {
          testthat::skip("Failed to locate AWS credentials")
        }
        cli::cli_abort("No IAM credentials found.", parent = cnd)
      }
    )
    cache$set(creds)
  }
  creds
}

# Wrapper for paws.common::locate_credentials() so we can mock it in tests.
locate_aws_credentials <- function(profile) {
  paws.common::locate_credentials(profile)
}

aws_creds_cache <- function(profile) {
  credentials_cache(key = hash(c("aws", profile)))
}


the <- rlang::new_environment()
the$credentials_cache <- rlang::new_environment()
the$current_store_id <- NULL

credentials_cache <- function(key) {
  list(
    get = function() env_get(the$credentials_cache, key, default = NULL),
    set = function(creds) env_poke(the$credentials_cache, key, creds),
    clear = function() env_unbind(the$credentials_cache, key)
  )
}
