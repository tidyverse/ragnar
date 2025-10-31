#' Generate embeddings using Snowflake
#'
#' Uses the [Cortex API `EMBED`](https://docs.snowflake.com/en/release-notes/2025/other/2025-04-14-cortex-offers-embed-rest-api)
#' functions to generate embeddings.
#'
#' See [complete documentation](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-rest-api#label-cortex-llm-embed-function).
#'
#' @section Authentication:
#'
#' - a *Programmatic Access Token* (PAT) defined via the SNOWFLAKE_PAT environment variable.
#' - A static OAuth token defined via the SNOWFLAKE_TOKEN environment variable.
#' - Key-pair authentication credentials defined via the SNOWFLAKE_USER and SNOWFLAKE_PRIVATE_KEY (which can be a PEM-encoded private key or a path to one) environment variables.
#' - Posit Workbench-managed Snowflake credentials for the corresponding account.
#' - Viewer-based credentials on Posit Connect. Requires the connectcreds package.
#'
#' @inheritParams embed_ollama
#' @inheritParams ellmer::chat_snowflake
#' @export
embed_snowflake <- function(
  x,
  account = snowflake_account(),
  credentials = NULL,
  model = "snowflake-arctic-embed-m-v1.5",
  api_args = list(),
  batch_size = 512L
) {
  if (is.data.frame(x)) {
    x[["embedding"]] <- Recall(
      x[["text"]],
      account = account,
      credentials = credentials,
      model = model,
      api_args = api_args,
      batch_size = batch_size
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

  auth_headers <- function() {
    if (is.null(credentials)) {
      default_snowflake_credentials(account)()
    } else if (is.function(credentials)) {
      credentials()
    } else {
      credentials
    }
  }

  headers <- auth_headers()

  base_req <- account |>
    snowflake_url() |>
    httr2::request() |>
    embed_req_retry() |>
    httr2::req_url_path_append("api/v2/cortex/inference:embed") |>
    httr2::req_headers_redacted(!!!headers) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json"
    ) |>
    httr2::req_user_agent(ragnar_user_agent()) |>
    httr2::req_error(body = function(resp) {
      tryCatch({
        json <- httr2::resp_body_json(resp, check_type = FALSE)
        json$message
      }, error = function(e) {
        "Unknown error"
      })
    })

  out <- vector("list", length(text))
  base_body <- rlang::list2(model = model, !!!api_args)

  for (indices in chunk_list(seq_along(text), batch_size)) {
    body <- base_body
    body$text <- as.list(text[indices])

    resp <- base_req |>
      httr2::req_body_json(body) |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    out[indices] <- lapply(resp$data, \(x) x$embedding)
  }

  matrix(unlist(out), nrow = length(text), byrow = TRUE)
}

# Snowflake utilities copied from ellmer ------------
# Handling Snowflake credentials and authentication in workbench + multiple other scnearios.

snowflake_account <- function() {
  val <- Sys.getenv("SNOWFLAKE_ACCOUNT")
  if (!identical(val, "")) {
    val
  } else {
    cli::cli_abort("SNOWFLAKE_ACCOUNT environment variable is not set.")
  }
}

snowflake_url <- function(account) {
  paste0("https://", account, ".snowflakecomputing.com")
}

default_snowflake_credentials <- function(account = snowflake_account()) {
  # Detect viewer-based credentials from Posit Connect.
  url <- snowflake_url(account)
  if (is_installed("connectcreds") && connectcreds::has_viewer_token(url)) {
    return(function() {
      token <- connectcreds::connect_viewer_token(url)
      list(
        Authorization = paste("Bearer", token$access_token),
        `X-Snowflake-Authorization-Token-Type` = "OAUTH"
      )
    })
  }

  token <- Sys.getenv("SNOWFLAKE_TOKEN")
  if (nchar(token) != 0) {
    return(function() {
      list(
        Authorization = paste("Bearer", token),
        # See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/authentication#using-oauth
        `X-Snowflake-Authorization-Token-Type` = "OAUTH"
      )
    })
  }

  token <- Sys.getenv("SNOWFLAKE_PAT")
  if (nchar(token) != 0) {
    return(function() {
      list(
        Authorization = paste("Bearer", token),
        # See https://docs.snowflake.com/en/user-guide/programmatic-access-tokens
        `X-Snowflake-Authorization-Token-Type` = "PROGRAMMATIC_ACCESS_TOKEN"
      )
    })
  }

  # Support for Snowflake key-pair authentication.
  # See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/authentication#generate-a-jwt-token
  user <- Sys.getenv("SNOWFLAKE_USER")
  private_key <- Sys.getenv("SNOWFLAKE_PRIVATE_KEY")
  if (nchar(user) != 0 && nchar(private_key) != 0) {
    check_installed(c("jose", "openssl"), "for key-pair authentication")
    key <- openssl::read_key(private_key)
    return(function() {
      token <- snowflake_keypair_token(account, user, key)
      list(
        Authorization = paste("Bearer", token),
        `X-Snowflake-Authorization-Token-Type` = "KEYPAIR_JWT"
      )
    })
  }

  # Check for Workbench-managed credentials.
  sf_home <- Sys.getenv("SNOWFLAKE_HOME")
  if (grepl("posit-workbench", sf_home, fixed = TRUE)) {
    token <- workbench_snowflake_token(account, sf_home)
    if (!is.null(token)) {
      return(function() {
        # Ensure we get an up-to-date token.
        token <- workbench_snowflake_token(account, sf_home)
        list(
          Authorization = paste("Bearer", token),
          `X-Snowflake-Authorization-Token-Type` = "OAUTH"
        )
      })
    }
  }

  cli::cli_abort("No Snowflake credentials are available.")
}

snowflake_keypair_token <- function(
  account,
  user,
  key,
  cache = snowflake_keypair_cache(account, key),
  lifetime = 600L,
  reauth = FALSE
) {
  # Producing a signed JWT is a fairly expensive operation (in the order of
  # ~10ms), but adding a cache speeds this up approximately 500x.
  creds <- cache$get()
  if (reauth || is.null(creds) || creds$expiry < Sys.time()) {
    cache$clear()
    expiry <- Sys.time() + lifetime
    # We can't use openssl::fingerprint() here because it uses a different
    # algorithm.
    fp <- openssl::base64_encode(
      openssl::sha256(openssl::write_der(key$pubkey))
    )
    if (grepl(".+\\.privatelink$", account)) {
      # account identifier is everything up to the first period
      account <- gsub("^([^.]*).+", "\\1", account)
    }
    sub <- toupper(paste0(account, ".", user))
    iss <- paste0(sub, ".SHA256:", fp)
    # Note: Snowflake employs a malformed issuer claim, so we have to inject it
    # manually after jose's validation phase.
    claim <- jose::jwt_claim("dummy", sub, exp = as.integer(expiry))
    claim$iss <- iss
    creds <- list(expiry = expiry, token = jose::jwt_encode_sig(claim, key))
    cache$set(creds)
  }
  creds$token
}

snowflake_keypair_cache <- function(account, key) {
  credentials_cache(key = hash(c("sf", account, openssl::fingerprint(key))))
}

snowflake_credentials_exist <- function(...) {
  tryCatch(
    is_list(default_snowflake_credentials(...)),
    error = function(e) FALSE
  )
}

# Reads Posit Workbench-managed Snowflake credentials from a
# $SNOWFLAKE_HOME/connections.toml file, as used by the Snowflake Connector for
# Python implementation. The file will look as follows:
#
# [workbench]
# account = "account-id"
# token = "token"
# authenticator = "oauth"
workbench_snowflake_token <- function(account, sf_home) {
  cfg <- readLines(file.path(sf_home, "connections.toml"))
  # We don't attempt a full parse of the TOML syntax, instead relying on the
  # fact that this file will always contain only one section.
  if (!any(grepl(account, cfg, fixed = TRUE))) {
    # The configuration doesn't actually apply to this account.
    return(NULL)
  }
  line <- grepl("token = ", cfg, fixed = TRUE)
  token <- gsub("token = ", "", cfg[line])
  if (nchar(token) == 0) {
    return(NULL)
  }
  # Drop enclosing quotes.
  gsub("\"", "", token)
}
